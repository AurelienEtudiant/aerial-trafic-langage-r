# aerial-trafic-langage-r

## Initialisation de la base de données

Pour initialiser la base de données MongoDB et charger les données, exécutez les commandes suivantes :

### 1. Démarrer MongoDB en arrière-plan
```bash
docker compose up -d mongodb
```

### 2. Importer les données
```bash
docker compose --profile import run --rm importer
```

Ces commandes vont:
- Démarrer le service MongoDB en conteneur (option `-d` pour détaché)
- Attendre que MongoDB soit prêt (healthcheck)
- Exécuter le conteneur importer avec le profil "import" pour charger les données initiales
- Nettoyer le conteneur importer après l'exécution (`--rm`)

### 3.Nettoyer les données et les contrôler
```bash
 docker compose --env-file .env --profile analysis run --rm \
  r-analysis \
  Rscript /workspace/script_r/clean_data.R 
```

Ce script va créer une nouvelle base de données `nyc_flights_cleaned` avec les collections nettoyées et prêtes à l'analyse.
Une collection `data_quality_reports` sera également créée pour stocker les rapports de qualité des données.

## Connexion via MongoDB Compass

MongoDB Compass est une interface graphique pour gérer et explorer votre base de données MongoDB.

### Prérequis
- [MongoDB Compass](https://www.mongodb.com/products/compass) doit être installé sur votre machine

### Paramètres de connexion

Copier la variable MONOGODB_URI depuis le fichier `.env` et remplacer le nom du service docker par "localhost".

### Étapes de connexion

1. Ouvrez MongoDB Compass
2. Cliquez sur "New Connection"
3. Entrez l'URI de connexion
4. Cliquez sur "Connect"
5. Vous devriez maintenant voir la base de données `nyc_flights` avec ses collections
