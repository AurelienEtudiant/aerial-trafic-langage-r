const appDb = db.getSiblingDB(process.env.MONGO_DB || "nyc_flights");
appDb.createUser({
  user: process.env.MONGO_APP_USERNAME,
  pwd: process.env.MONGO_APP_PASSWORD,
  roles: [{ role: "readWrite", db: appDb.getName() }]
});

