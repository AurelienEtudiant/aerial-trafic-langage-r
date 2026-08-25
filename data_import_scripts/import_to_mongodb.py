#!/usr/bin/env python3
import argparse, json, os, re
from datetime import datetime, timezone
from pathlib import Path
import pandas as pd
try:
    from dotenv import load_dotenv
except ImportError:
    def load_dotenv(): return False
from pypdf import PdfReader
from pymongo import MongoClient, ASCENDING, GEOSPHERE
from pymongo.errors import BulkWriteError

COLS_FLIGHTS = "year month day dep_time sched_dep_time dep_delay arr_time sched_arr_time arr_delay carrier flight tailnum origin dest air_time distance hour minute time_hour".split()
COLS_WEATHER = "origin year month day hour temp dewp humid wind_dir wind_speed wind_gust precip pressure visib time_hour".split()

def blank(v): return pd.isna(v) or str(v).strip() in {"", "NA", "nan", "None"}
def integer(v): return None if blank(v) else int(float(v))
def number(v): return None if blank(v) else float(v)
def iso(v):
    if blank(v): return None
    return datetime.fromisoformat(str(v).replace("Z", "+00:00")).astimezone(timezone.utc)

def weather_rows(path):
    for page in PdfReader(path).pages:
        for line in (page.extract_text() or "").splitlines():
            line=line.strip().replace("\x0c", "")
            if not line or line.startswith("origin,"): continue
            values=line.split(',')
            if len(values) == len(COLS_WEATHER): yield dict(zip(COLS_WEATHER, values))

def chunks(rows, size=5000):
    buf=[]
    for row in rows:
        buf.append(row)
        if len(buf) >= size: yield buf; buf=[]
    if buf: yield buf

def reset_collection(db, name, validator):
    db.drop_collection(name)
    db.create_collection(name, validator={"$jsonSchema": validator}, validationLevel="strict", validationAction="error")

def main():
    ap=argparse.ArgumentParser(); ap.add_argument('--source',default='data'); ap.add_argument('--uri'); ap.add_argument('--drop',action='store_true')
    args=ap.parse_args(); load_dotenv()
    uri=args.uri or os.getenv('MONGODB_URI')
    if not uri: raise SystemExit('MONGODB_URI absent (utilisez .env, un secret Docker ou un gestionnaire de secrets).')
    src=Path(args.source)
    options={'serverSelectionTimeoutMS':10000}
    if uri.startswith('mongodb+srv'): options['tls']=True
    client=MongoClient(uri, **options)
    db=client.get_default_database()

    schemas={
      'airlines': {'bsonType':'object','required':['_id','name'],'properties':{'_id':{'bsonType':'string','pattern':'^[A-Z0-9]{2}$'},'name':{'bsonType':'string'}}},
      'airports': {'bsonType':'object','required':['_id','name','location'],'properties':{'_id':{'bsonType':'string','pattern':'^[A-Z0-9]{3,4}$'},'name':{'bsonType':'string'},'location':{'bsonType':'object'},'alt':{'bsonType':['int','null']},'tz':{'bsonType':['int','null']},'dst':{'enum':['A','U','N',None]},'tzone':{'bsonType':['string','null']}}},
      'planes': {'bsonType':'object','required':['_id'],'properties':{'_id':{'bsonType':'string','pattern':'^N[0-9A-Z]{1,5}$'},'year':{'bsonType':['int','null']},'engines':{'bsonType':['int','null']},'seats':{'bsonType':['int','null']},'speed':{'bsonType':['double','int','null']}}},
      'weather': {'bsonType':'object','required':['_id','origin','recorded_at'],'properties':{'_id':{'bsonType':'string'},'origin':{'bsonType':'string','pattern':'^[A-Z0-9]{3,4}$'},'recorded_at':{'bsonType':'date'}}},
      'flights': {'bsonType':'object','required':['_id','date','carrier','flight','origin','dest','scheduled_departure'],'properties':{'_id':{'bsonType':'string'},'carrier':{'bsonType':'string','pattern':'^[A-Z0-9]{2}$'},'tailnum':{'bsonType':['string','null'],'pattern':'^N[0-9A-Z]{1,5}$'},'tailnum_raw':{'bsonType':['string','null']},'origin':{'bsonType':'string','pattern':'^[A-Z0-9]{3,4}$'},'dest':{'bsonType':'string','pattern':'^[A-Z0-9]{3,4}$'}}}
    }
    for name,schema in schemas.items(): reset_collection(db,name,schema)

    airlines=json.load(open(src/'airlines.json',encoding='utf-8'))
    db.airlines.insert_many([{'_id':x['carrier'],'name':x['name']} for x in airlines])
    airports=pd.read_excel(src/'airports.xlsx').drop(columns=['Unnamed: 0'],errors='ignore')
    supplemental=json.load(open(src/'supplemental_airports.json',encoding='utf-8'))
    airport_docs=[]
    for x in airports.to_dict('records')+supplemental:
        airport_docs.append({'_id':str(x['faa']).strip(),'name':str(x['name']).strip(),'location':{'type':'Point','coordinates':[number(x['lon']),number(x['lat'])]},'alt':integer(x.get('alt')),'tz':integer(x.get('tz')),'dst':None if blank(x.get('dst')) else str(x['dst']),'tzone':None if blank(x.get('tzone')) else str(x['tzone']),'source':x.get('source','openflights')})
    db.airports.insert_many(airport_docs)
    planes=pd.read_html(src/'planes.html')[0].drop(columns=['Unnamed: 0'],errors='ignore')
    db.planes.insert_many([{'_id':str(x['tailnum']).strip(),'year':integer(x['year']),'type':str(x['type']),'manufacturer':str(x['manufacturer']),'model':str(x['model']),'engines':integer(x['engines']),'seats':integer(x['seats']),'speed':number(x['speed']),'engine':str(x['engine'])} for x in planes.to_dict('records')])

    wdocs=[]
    for x in weather_rows(src/'weather.pdf'):
        stamp=iso(x['time_hour']); key=f"{x['origin']}:{stamp.isoformat()}"
        wdocs.append({'_id':key,'origin':x['origin'],'year':integer(x['year']),'month':integer(x['month']),'day':integer(x['day']),'hour':integer(x['hour']),'recorded_at':stamp,'temp_f':number(x['temp']),'dewpoint_f':number(x['dewp']),'humidity_pct':number(x['humid']),'wind_dir_deg':number(x['wind_dir']),'wind_speed_mph':number(x['wind_speed']),'wind_gust_mph':number(x['wind_gust']),'precip_in':number(x['precip']),'pressure_mb':number(x['pressure']),'visibility_mi':number(x['visib'])})
    db.weather.insert_many(wdocs)

    raw=pd.read_excel(src/'flights.xlsx',dtype=str).iloc[:,0]
    plane_ids=set(db.planes.distinct('_id'))
    airport_ids=set(db.airports.distinct('_id'))
    def flight_docs():
      for line in raw:
        x=dict(zip(COLS_FLIGHTS,str(line).split(',')))
        y,m,d,h=map(integer,(x['year'],x['month'],x['day'],x['hour']))
        key=f"{y:04d}-{m:02d}-{d:02d}:{h:02d}:{x['carrier']}:{int(x['flight'])}"
        raw_tailnum=None if blank(x['tailnum']) else x['tailnum'].strip().upper()
        valid_tailnum=raw_tailnum if raw_tailnum and re.fullmatch(r'^N[0-9A-Z]{1,5}$',raw_tailnum) else None
        yield {'_id':key,'date':datetime(y,m,d,tzinfo=timezone.utc),'year':y,'month':m,'day':d,'hour':h,'minute':integer(x['minute']),'dep_time':integer(x['dep_time']),'arr_time':integer(x['arr_time']),'sched_dep_time':integer(x['sched_dep_time']),'sched_arr_time':integer(x['sched_arr_time']),'dep_delay_min':integer(x['dep_delay']),'arr_delay_min':integer(x['arr_delay']),'carrier':x['carrier'],'flight':integer(x['flight']),'tailnum':valid_tailnum,'tailnum_raw':raw_tailnum if raw_tailnum != valid_tailnum else None,'plane_matched':valid_tailnum in plane_ids if valid_tailnum else False,'origin':x['origin'],'dest':x['dest'],'airports_matched':x['origin'] in airport_ids and x['dest'] in airport_ids,'air_time_min':integer(x['air_time']),'distance_mi':integer(x['distance']),'scheduled_departure':iso(x['time_hour'])}
    for batch in chunks(flight_docs()): db.flights.insert_many(batch,ordered=False)

    db.airports.create_index([('location',GEOSPHERE)])
    db.flights.create_index([('year',ASCENDING),('month',ASCENDING),('day',ASCENDING),('hour',ASCENDING),('carrier',ASCENDING),('flight',ASCENDING)],unique=True,name='uq_flight_business_key')
    db.flights.create_index([('carrier',ASCENDING)]); db.flights.create_index([('tailnum',ASCENDING)]); db.flights.create_index([('origin',ASCENDING),('dest',ASCENDING)])
    db.weather.create_index([('origin',ASCENDING),('recorded_at',ASCENDING)],unique=True)
    db.create_collection('import_metadata') if 'import_metadata' not in db.list_collection_names() else None
    db.import_metadata.insert_one({'imported_at':datetime.now(timezone.utc),'counts':{n:db[n].count_documents({}) for n in schemas},'source':'OneDrive_1_24-08-2026.zip'})
    print(json.dumps({n:db[n].count_documents({}) for n in schemas},indent=2))

if __name__=='__main__': main()