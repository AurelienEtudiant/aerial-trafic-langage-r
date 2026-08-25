const sourceDatabase =
    process.env.MONGO_SOURCE_DB || "nyc_flights";

const cleanedDatabase =
    process.env.MONGO_CLEAN_DB || "nyc_flights_cleaned";

const sourceDb = db.getSiblingDB(sourceDatabase);
const cleanedDb = db.getSiblingDB(cleanedDatabase);

sourceDb.createUser({
    user: process.env.MONGO_APP_USERNAME,
    pwd: process.env.MONGO_APP_PASSWORD,
    roles: [
        {
            role: "readWrite",
            db: sourceDatabase
        },
        {
            role: "readWrite",
            db: cleanedDatabase
        }
    ]
});

cleanedDb.createCollection("data_quality_reports");