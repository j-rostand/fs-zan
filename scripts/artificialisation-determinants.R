# Auteur
# Jules Rostand

# Description
# Ce script R propose une analyse en composantes principales d'un ensemble de
# déterminants de l'artificialisation des sols au niveau de la commune

# Bibliothèques
library(tidyverse)
library(explor)
library(FactoMineR)

###############
### SOURCES ###
###############

# Nom et identifiant des communes françaises
# https://www.insee.fr/fr/information/6051727
read_csv(file = "data/commune_2022.csv") %>%
  select(COM, DEP, LIBELLE) %>%
  rename("CODGEO" = COM, "LIBGEO" = LIBELLE) -> insee.communes22

# Nombre de lits touristiques par commune
# https://www.insee.fr/fr/statistiques/2021703#consulter
readxl::read_xlsx(path = "data/base-cc-tourisme-2023-geo2022.xlsx", skip = 5) %>%
  select(CODGEO, RTLIT23) %>%
  rename(litstour23 = "RTLIT23") -> insee.tourisme

# Population communale
# https://www.observatoire-des-territoires.gouv.fr/population-au-dernier-recensement
readxl::read_xlsx(path = "data/insee_rp_hist_1968.xlsx", skip = 4) %>%
  filter(an == "2019") %>%
  select(codgeo, p_pop) %>%
  rename("CODGEO" = codgeo, 
         "pop2019" = p_pop) -> insee.pop2019

# Production brute standard (PBS) moyenne, évolution 2010 à 2020 
# JR d'après Agreste
# Les territoires non agricoles (NA) sont considérés comme ayant une contribution nulle
readxl::read_xlsx(path = "data/agreste-saa-2020.xlsx", skip = 3, na = "N/A - division par 0") %>%
  select(Code, `PBS moyenne : évolution 2020/2010`) %>%
  rename("CODGEO" = Code, 
         "pbsmoy1020" = `PBS moyenne : évolution 2020/2010`) %>%
  mutate(pbsmoy1020 = replace_na(pbsmoy1020, 0)) -> agreste2020

# Médiane du revenu par unité de consommation
# https://www.observatoire-des-territoires.gouv.fr/mediane-du-revenu-disponible-par-uc
readxl::read_xlsx(path = "data/filosofi.xlsx", skip = 4) %>%
  select(codgeo, med_disp) %>%
  rename("CODGEO" = codgeo, 
         "revmeduc20" = med_disp) -> filosofi.revenu2020

# Evolution du nombre de ménages et artificialisation liée à l'habitat
# LN d'après l'ONAS
read_delim(file = "data/efficacitehab.txt",
           delim = " ", col_names = TRUE, col_types = c("ccdd")) %>%
  select(codeinsee, men0919, artifhab) %>%
  rename("CODGEO" = codeinsee) -> insee.menages0919

# Evolution du nombre d'emplois et artificialisation liée à l'activité
# LN
read_delim(file = "data/empsal.txt",
           delim = " ", col_names = TRUE, col_types = c("ccdd")) %>%
  select(codeinsee, emp1121, act1121) %>%
  rename("CODGEO" = codeinsee) -> insee.emploi1121

# Part des Cadres et professions intellectuelles superieures dans la population
# https://www.observatoire-des-territoires.gouv.fr/part-des-cadres-et-professions-intellectuelles-superieures-dans-la-population
# redondant avec le revenu
# readxl::read_xlsx(path = "data/indic_sex_rp.xlsx", skip = 4) %>%
#   filter(sexe == "T" & an == "2019") %>%
#   select(codgeo, p_csp_cadpis) %>%
#   rename("CODGEO" = codgeo, 
#          "partcspsup19" = p_csp_cadpis) -> insee.csp2019

# Ensoleillement
# https://www.linternaute.com/voyage/climat/classement/departements/soleil
# On applique la valeur du Haut-Rhin au Territoire de Belfort
readxl::read_xlsx(path = "data/ensoleillement.xlsx", skip = 1) %>%
  select(iddep, heures_soleil_2022) %>%
  rename("DEP" = iddep) -> internaute.soleil

# Evolution du nombre de logements en résidences principales et secondaires, 
# avec la part des logements vacants
# LN
# part des proprios redondante avec le revenue
read_csv(file = "data/logement1.csv", col_types = c("ccdddd")) %>%
  select(codeinsee, evollog0819, evolsec0819, partlogvacant) %>%
  rename("CODGEO" = codeinsee) -> insee.logement1

# Prix de l'immobilier
# LN
# On a 4276 données manquantes, source retiré de l'analyse à défaut d'une meilleure qualité
read_delim(file = "data/valeurimmo.txt",
           delim = " ", col_names = TRUE, col_types = c("ccd")) %>%
  select(codeinsee, valeursurf) %>%
  rename("CODGEO" = codeinsee) -> insee.immobilier

# Flux et part d'artificialisation par commune
# JR, d'après l'ONAS
read_csv(file = "res/artificialisation-communes-2011-2021.csv", 
         col_types = "cc") %>%
  select(idcom, artcom2020, nafart1121) %>%
  rename("CODGEO" = idcom) -> onas.communes

# Grille de densité des communes de l'Insee
# https://www.insee.fr/fr/statistiques/fichier/6439600/grille_densite_7_niveaux_detaille_2023.xlsx
readxl::read_xlsx(path = "data/grille_densite_7_niveaux_detaille_2023.xlsx", 
                  skip = 1) %>%
  select(depcom, libdens) %>%
  rename("CODGEO" = depcom) -> insee.densite2023

##################
### TRAITEMENT ###
##################

insee.communes22 %>%
  left_join(insee.tourisme, by = "CODGEO") %>%
  left_join(insee.pop2019, by = "CODGEO") %>%
  left_join(agreste2020, by = "CODGEO") %>%
  left_join(filosofi.revenu2020, by = "CODGEO") %>%
  left_join(insee.menages0919, by = "CODGEO") %>%
  left_join(insee.emploi1121, by = "CODGEO") %>% 
  left_join(insee.logement1, by = "CODGEO") %>% 
  left_join(insee.immobilier, by = "CODGEO") %>%
  left_join(insee.densite2023, by = "CODGEO") %>%
  right_join(internaute.soleil, by = "DEP") %>%
  left_join(onas.communes, by = "CODGEO") %>%
  mutate(partlitstour23 = litstour23 / pop2019) %>% # part du nombre de lits de tourisme dans la population
  select(-pop2019, -litstour23, -act1121, -artifhab, -valeursurf) %>% # NA élevés valeursurf
  write_excel_csv("res/determinants-artificialisation.csv", 
                  quote = "all") -> determinants.artificialisation

###############
### ANALYSE ###
###############

determinants.artificialisation %>%
  select(-CODGEO, -LIBGEO, -DEP) %>% 
  filter_all(all_vars(!is.na(.))) %>%
  print() %>%
  PCA(ncp = 5, graph = FALSE, 
      quanti.sup = c("artcom2020", "nafart1121"), 
      quali.sup = c("libdens"),
      scale.unit = TRUE) -> res.pca

explor(res.pca)

summary(res.pca)

###############
### ANNEXES ###
###############

# Code pour une PCA au niveau de l'EPCI
# Sans secret statistique et données manquantes
# 
# determinants.artificialisation.epci %>%
#  filter_at(vars(starts_with("artifh")), any_vars(. != 0)) %>% # division par 0
#  filter_at(vars(starts_with("act11")), any_vars(. != 0)) %>% # division par 0
#  mutate(part_lits_tourisme_2023 = lits_tourisme_2023 / pop2019,
#         artif_efficacite_habitat = men0919 / artifhab,
#         artif_efficacite_activite = emp1121 / act1121) %>%
#  select(-pop2019, -lits_tourisme_2023, 
#         -artifhab, -act1121) -> determinants.artificialisation.epci
# 
# determinants.artificialisation.communes %>%
#  select(-CODGEO, -LIBGEO, -DEP) %>% # -artif_efficacite_habitat, -artif_efficacite_activite
#  print() %>%
#  filter_all(all_vars(!is.na(.))) %>%
#  print() %>%
#  PCA(ncp = 5, graph = FALSE, 
#      quanti.sup = c("artcom2020", "nafart1121", "artif_efficacite_habitat", "artif_efficacite_activite"), 
#      quali.sup = c("libdens"),
#      scale.unit = TRUE) -> res.pca
