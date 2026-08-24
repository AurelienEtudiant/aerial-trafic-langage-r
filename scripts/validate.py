import json, os
try:
    from dotenv import load_dotenv
except ImportError:
    def load_dotenv(): return False
from pymongo import MongoClient
load_dotenv(); c=MongoClient(os.environ['MONGODB_URI']); db=c.get_default_database()
checks={
 'counts':{n:db[n].count_documents({}) for n in ['flights','airports','airlines','planes','weather']},
 'unmatched_plane_flights':db.flights.count_documents({'plane_matched':False}),
 'distinct_unmatched_tailnums':len(db.flights.distinct('tailnum',{'plane_matched':False})),
 'unmatched_airport_flights':db.flights.count_documents({'airports_matched':False}),
 'unknown_carriers':list(db.flights.aggregate([{'$lookup':{'from':'airlines','localField':'carrier','foreignField':'_id','as':'a'}},{'$match':{'a':{'$size':0}}},{'$group':{'_id':'$carrier'}}]))
}
print(json.dumps(checks,indent=2,default=str))
