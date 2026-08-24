db.flights.aggregate([
  {$limit: 10},
  {$lookup:{from:"airlines",localField:"carrier",foreignField:"_id",as:"airline"}},
  {$lookup:{from:"planes",localField:"tailnum",foreignField:"_id",as:"plane"}},
  {$lookup:{from:"airports",localField:"origin",foreignField:"_id",as:"origin_airport"}},
  {$lookup:{from:"airports",localField:"dest",foreignField:"_id",as:"dest_airport"}}
]).forEach(printjson);

db.flights.aggregate([
  {$group:{_id:"$carrier",flights:{$sum:1},avg_dep_delay:{$avg:"$dep_delay_min"},avg_arr_delay:{$avg:"$arr_delay_min"}}},
  {$sort:{avg_arr_delay:-1}}
]).forEach(printjson);

db.flights.aggregate([
  {$match:{origin:"JFK"}},{$limit:100},
  {$lookup:{from:"weather",let:{o:"$origin",t:"$scheduled_departure"},pipeline:[{$match:{$expr:{$and:[{$eq:["$origin","$$o"]},{$eq:["$recorded_at","$$t"]}]}}}],as:"weather"}},
  {$unwind:{path:"$weather",preserveNullAndEmptyArrays:true}}
]).forEach(printjson);

db.flights.aggregate([{$match:{plane_matched:false}},{$group:{_id:"$tailnum",flights:{$sum:1}}},{$sort:{flights:-1}}]).forEach(printjson);

