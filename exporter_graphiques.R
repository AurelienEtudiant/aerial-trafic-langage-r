

library(ggplot2)



if (!exists("graph_annulations_carrier") || !exists("graph_carte_usa") ||
    !exists("graph_carte_top100")) {
  source("analyse_reporting.R")
}

# --- Créer le dossier de sortie ---

dir.create("graphiques", showWarnings = FALSE)

# --- Export de chaque graphique ---

ggsave("graphiques/01_trafic_mensuel_facette.png",
       graph_trafic_mensuel, width = 8, height = 8, dpi = 150)

ggsave("graphiques/02_trafic_journalier_annote.png",
       graph_trafic_journalier, width = 10, height = 6, dpi = 150)

ggsave("graphiques/03_retard_moyen_journalier.png",
       graph_retard_journalier, width = 10, height = 6, dpi = 150)

ggsave("graphiques/04_distribution_retards.png",
       graph_distribution_retards, width = 8, height = 6, dpi = 150)

ggsave("graphiques/05_retard_par_heure.png",
       graph_retard_par_heure, width = 9, height = 6, dpi = 150)

ggsave("graphiques/06_retard_vs_distance.png",
       graph_retard_distance, width = 9, height = 6, dpi = 150)

ggsave("graphiques/07_annulations_par_compagnie.png",
       graph_annulations_carrier, width = 9, height = 6, dpi = 150)

ggsave("graphiques/08_carte_trajectoires_usa.png",
       graph_carte_usa, width = 10, height = 7, dpi = 150)

ggsave("graphiques/09_carte_top100_routes.png",
       graph_carte_top100, width = 10, height = 7, dpi = 150)

message("Tous les graphiques ont été exportés dans le dossier graphiques/")
message("Fichiers créés :")
print(list.files("graphiques"))