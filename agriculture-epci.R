# Auteur
# Jules Rostand

# Description
# Ce script R propose une classification des EPCI selon l'artificialisation des sols et ses déterminants.

# Bibliothèques
library(tidyverse)

###########################################
## Préparation des données : agriculture ##
###########################################

# SAA 2010
# https://stats.agriculture.gouv.fr/cartostat/

readxl::read_xlsx(path = "data/agreste-saa-2010.xlsx", skip = 3, na = "N/A - division par 0") -> agreste2010

# Séparation des codes et des libellés
agreste2010 %<>%
  rename("canton_code" = Code, 
         "canton_libelle" = Libellé,
         "nb_exploitation_2010" = `Nombre total d'exploitations, 2010`,
         "sau_moyenne_2010" = `SAU moyenne par exploitation, 2010`,
         "sau_moyenne_2010_estimation" = `SAU moyenne par exploitation, 2010_x000d_\nestimation (*=oui)`,
         "pbs_moyenne_2010" = `PBS moyenne, 2010`,
         "pbs_moyenne_2010_exploitation" = `PBS moyenne, 2010_x000d_\nestimation (*=oui)`,
         "part_under_40_2010" = `Moins de 40 ans parmi les chefs d'exploitation et coexploitants : part en 2010`,
         "part_under_40_2010_estimation" = `Moins de 40 ans parmi les chefs d'exploitation et coexploitants : part en 2010_x000d_\nestimation (*=oui)`) %>%
  select(-nb_exploitation_2010)

# Du canton à l'EPCI
readxl::read_xlsx(path = "data/table-appartenance-geo-communes-2020.xlsx", skip = 5) -> codegeo2020

codegeo2020 %<>%
  select(CODGEO, CV, EPCI) %>%
  rename("canton_code" = CV) %>%
  left_join(agreste2010) %>%
  group_by(EPCI) %>%
  summarise(sau_moyenne_2010_mean = mean(sau_moyenne_2010),
            pbs_moyenne_2010_mean = mean(pbs_moyenne_2010),
            part_under_40_mean = mean(part_under_40_2010))

# SAA 2020

readxl::read_xlsx(path = "data/agreste-saa-2020.xlsx", skip = 3, na = "N/A - division par 0") -> agreste2020

# Séparation des codes et des libellés
agreste2020 %<>%
  rename("commune_code" = Code, "commune_libelle" = Libellé) %>%
  separate(col = "Département 2020", into = c("departement_code","departement_libelle"), sep = " - ", extra = "merge", fill = "left") %>%
  separate(col = "Intercommunalité 2020", into = c("intercommunalite_code","intercommunalite_libelle"), sep = " - ", extra = "merge", fill = "left") %>%
  separate(col = "Région 2020", into = c("region_code","region_libelle"), sep = " - ", extra = "merge", fill = "left") %>%
  separate(col = "Spécialisation de la production agricole en 2020 (12 postes)", into = c("specialisation12_code","specialisation12_libelle"), sep = " - ", extra = "merge", fill = "left") %>%
  separate(col = "Spécialisation de la production agricole en 2020 (17 postes)", into = c("specialisation17_code","specialisation17_libelle"), sep = " - ", extra = "merge", fill = "left")

# Renommer les variables
agreste2020 %<>%
  rename("sau_evolution_2020_2010" = `SAU : évolution 2020/2010`,
         "sau_moyenne_2020" = `SAU moyenne en 2020`,
         "sau_moyenne_variation_absolue_2020_2010" = `SAU moyenne : variation absolue 2020-2010`,
         "pbs_moyenne_2020" = `PBS moyenne en 2020`,
         "pbs_evolution_2020_2010" = `PBS : évolution 2020/2010`,
         "pbs_moyenne_evolution_2020_2010" = `PBS moyenne : évolution 2020/2010`,
         "sau_2020" = `SAU en 2020`,
         "sau_variation_absolue_2020_2010" = `SAU : variation absolue 2020-2010`,
         "pbs_2020" = `PBS en 2020`, 
         "nb_exploitation_2020" = `Nombre d'exploitations en 2020`)

# Réparation de l'indicateur d'estimation
agreste2020 %<>%
  rename("specialisation12_estimation" = `Spécialisation de la production agricole en 2020 (12 postes)_x000d_\nestimation (*=oui)`,
         "specialisation17_estimation" = `Spécialisation de la production agricole en 2020 (17 postes)_x000d_\nestimation (*=oui)`,
         "sau_evolution_2020_2010_estimation" = `SAU : évolution 2020/2010_x000d_\nestimation (*=oui)`,
         "sau_moyenne_2020_estimation" = `SAU moyenne en 2020_x000d_\nestimation (*=oui)`,
         "sau_moyenne_variation_absolue_2020_2010_estimation" = `SAU moyenne : variation absolue 2020-2010_x000d_\nestimation (*=oui)`,
         "pbs_moyenne_2020_estimation" = `PBS moyenne en 2020_x000d_\nestimation (*=oui)`,
         "pbs_evolution_2020_2010_estimation" = `PBS : évolution 2020/2010_x000d_\nestimation (*=oui)`,
         "pbs_moyenne_evolution_2020_2010_estimation" = `PBS moyenne : évolution 2020/2010_x000d_\nestimation (*=oui)`,
         "sau_2020_estimation" = `SAU en 2020_x000d_\nestimation (*=oui)`,
         "sau_variation_absolue_2020_2010_estimation" = `SAU : variation absolue 2020-2010_x000d_\nestimation (*=oui)`,
         "pbs_2020_estimation" = `PBS en 2020_x000d_\nestimation (*=oui)`) 

# Calculs valeurs de 2010

agreste2020 %<>%
  mutate(sau_2010 = sau_2020 / (1 + 0.01*sau_evolution_2020_2010),
         pbs_2010 = pbs_2020 / (1 + 0.01*pbs_evolution_2020_2010),
         sau_moyenne_2010 = sau_moyenne_2020 - sau_moyenne_variation_absolue_2020_2010,
         pbs_moyenne_2010 = pbs_moyenne_2020 / (1 + 0.01*pbs_moyenne_evolution_2020_2010),
         nb_exploitation_2010 = sau_2010 / sau_moyenne_2010) %>%
  mutate(across(34:35, round, 0)) %>%
  mutate(across(36:37, round, 1)) %>%
  mutate(across(38:38, round, 0))

# Agrégation des variables au niveau de l'EPCI
agreste2020 %>%
  group_by(intercommunalite_code, intercommunalite_libelle) %>%
  summarise(specialisation12_code_mode = modeest::mfv1(specialisation12_code), 
            specialisation12_libelle_mode = modeest::mfv1(specialisation12_libelle), 
            sau_2010_somme = sum(sau_2010),
            sau_2020_somme = sum(sau_2020),
            pbs_2010_somme = sum(pbs_2010),
            pbs_2020_somme = sum(pbs_2020),
            nb_exploitation_2010_somme = sum(nb_exploitation_2010),
            nb_exploitation_2020_somme = sum(nb_exploitation_2020),
            sau_variation_absolue_2020_2010_somme = sum(sau_variation_absolue_2020_2010)) -> agreste2020.epci

# Calculs des variables moyennes et d'évolutions au niveau de l'EPCI
agreste2020.epci %<>%
  mutate(sau_moyenne_2010_somme = sau_2010_somme / nb_exploitation_2010_somme,
         sau_moyenne_2020_somme = sau_2020_somme / nb_exploitation_2020_somme,
         pbs_moyenne_2010_somme = sau_2010_somme / nb_exploitation_2010_somme,
         pbs_moyenne_2020_somme = pbs_2020_somme / nb_exploitation_2020_somme,
         sau_evolution_2020_2010_somme = (sau_2020_somme - sau_2010_somme) / sau_2010_somme,
         pbs_evolution_2020_2010_somme = (pbs_2020_somme - pbs_2010_somme) / pbs_2010_somme,
         sau_moyenne_evolution_2020_2010_somme = (sau_moyenne_2020_somme - sau_moyenne_2010_somme) / sau_2010_somme,
         pbs_moyenne_evolution_2020_2010_somme = (pbs_moyenne_2020_somme - pbs_moyenne_2010_somme) / pbs_moyenne_2010_somme,
         nb_exploitation_evolution_2020_2010_somme = (nb_exploitation_2020_somme - nb_exploitation_2010_somme) / nb_exploitation_2010_somme)

# Jointure

agreste2020.epci %<>%
  left_join(codegeo2020, by = c("intercommunalite_code" = "EPCI"))

# Calculs complémentaires

agreste2020.epci$sau_moyenne_evolution_2020_2010_mean = agreste2020.epci$sau_moyenne_2020_somme - 
  agreste2020.epci$sau_moyenne_2010_mean

agreste2020.epci$pbs_moyenne_evolution_2020_2010_mean = agreste2020.epci$pbs_moyenne_2020_somme - 
  agreste2020.epci$pbs_moyenne_2010_mean

# Arrondis et export des données
agreste2020.epci %<>%
  mutate(across(12:25, round, 2)) %>%
  ungroup() %>%
  write_excel_csv("res/agriculture-epci-2010-2020.csv", quote = "all")
