# Auteur
# Jules Rostand

# Description
# Ce script R propose une ACP des déterminants de l'artificialisation des sols au niveau de la commune

# Bibliothèques
library(tidyverse)
library(explor)
library(FactoMineR)

# https://www.insee.fr/fr/information/6051727
read_csv(file = "data/commune_2022.csv") %>%
  select(COM, DEP, LIBELLE) %>%
  rename("CODGEO" = COM, "LIBGEO" = LIBELLE) -> insee.communes22

# https://www.insee.fr/fr/statistiques/2021703#consulter
readxl::read_xlsx(path = "data/base-cc-tourisme-2023-geo2022.xlsx", skip = 5) %>%
  select(CODGEO, RTLIT23) %>%
  rename(lits_tourisme_2023 = "RTLIT23") -> insee.tourisme

# https://www.observatoire-des-territoires.gouv.fr/population-au-dernier-recensement
readxl::read_xlsx(path = "data/insee_rp_hist_1968.xlsx", skip = 4) %>%
  filter(an == "2019") %>%
  select(codgeo, p_pop) %>%
  rename("CODGEO" = codgeo, 
         "pop2019" = p_pop) -> insee.pop2019

# JR
readxl::read_xlsx(path = "data/agreste-saa-2020.xlsx", skip = 3, na = "N/A - division par 0") %>%
  select(Code, `PBS moyenne : évolution 2020/2010`) %>%
  rename("CODGEO" = Code, 
         "pbs_moyenne_evolution_2020_2010" = `PBS moyenne : évolution 2020/2010`) -> agreste2020

# https://www.observatoire-des-territoires.gouv.fr/mediane-du-revenu-disponible-par-uc
readxl::read_xlsx(path = "data/filosofi.xlsx", skip = 4) %>%
  select(codgeo, med_disp) %>%
  rename("CODGEO" = codgeo, 
         "revenu_disponible_menages_2020_mediane" = med_disp) -> filosofi.revenu2020

# LN
read_delim(file = "data/efficacitehab.txt",
           delim = " ", col_names = TRUE, col_types = c("ccdd")) %>%
  select(codeinsee, men0919, artifhab) %>%
  rename("CODGEO" = codeinsee) -> insee.menages0919

# LN
read_delim(file = "data/empsal.txt",
           delim = " ", col_names = TRUE, col_types = c("ccdd")) %>%
  select(codeinsee, emp1121, act1121) %>%
  rename("CODGEO" = codeinsee) -> insee.emploi1121

# https://www.observatoire-des-territoires.gouv.fr/part-des-cadres-et-professions-intellectuelles-superieures-dans-la-population
# redondant avec le revenu
# readxl::read_xlsx(path = "data/indic_sex_rp.xlsx", skip = 4) %>%
#   filter(sexe == "T" & an == "2019") %>%
#   select(codgeo, p_csp_cadpis) %>%
#   rename("CODGEO" = codgeo, 
#          "part_csp_cadres_intellectuelles_2019" = p_csp_cadpis) -> insee.csp2019

# Ensoleillement
# https://www.linternaute.com/voyage/climat/classement/departements/soleil
readxl::read_xlsx(path = "data/ensoleillement.xlsx", skip = 1) %>%
  select(iddep, heures_soleil_2022) %>%
  rename("DEP" = iddep) -> internaute.soleil

# LN
# part des proprios redondante avec le revenue
read_delim(file = "data/logement.txt", col_types = c("ccdddd")) %>%
  select(codeinsee, evollog0819, evolsec0819, partlogvacant) %>%
  rename("CODGEO" = codeinsee) -> insee.logement

# evollog019 est restrient aux seumes résidences principales
read_csv(file = "data/logement1.csv", col_types = c("ccdddd")) %>%
  select(codeinsee, evollog0819, evolsec0819, partlogvacant) %>%
  rename("CODGEO" = codeinsee) -> insee.logement1

# LN
read_delim(file = "data/valeurimmo.txt",
           delim = " ", col_names = TRUE, col_types = c("ccd")) %>%
  select(codeinsee, valeursurf) %>%
  rename("CODGEO" = codeinsee) -> insee.immobilier

# JR, d'après l'ONAS
read_csv(file = "res/artificialisation-communes-2011-2021.csv", 
         col_types = "cc") %>%
  select(idcom, artcom2020, nafart1121) %>%
  rename("CODGEO" = idcom)-> onas.communes

# https://www.insee.fr/fr/statistiques/fichier/6439600/grille_densite_7_niveaux_detaille_2023.xlsx
readxl::read_xlsx(path = "data/grille_densite_7_niveaux_detaille_2023.xlsx", 
                  skip = 1) %>%
  select(depcom, libdens) %>%
  rename("CODGEO" = depcom) -> insee.densite2023

insee.communes22 %>%
  left_join(insee.tourisme, by = "CODGEO") %>%
  left_join(insee.pop2019, by = "CODGEO") %>%
  left_join(agreste2020, by = "CODGEO") %>%
  left_join(filosofi.revenu2020, by = "CODGEO") %>%
  left_join(insee.menages0919, by = "CODGEO") %>%
  left_join(insee.emploi1121, by = "CODGEO") %>% 
  left_join(insee.logement, by = "CODGEO") %>% 
  left_join(insee.immobilier, by = "CODGEO") %>%
  left_join(insee.densite2023, by = "CODGEO") %>%
  right_join(internaute.soleil, by = "DEP") %>%
  left_join(onas.communes, by = "CODGEO") -> determinants.artificialisation

determinants.artificialisation %<>%
  filter_at(vars(starts_with("artifh")), any_vars(. != 0)) %>%
  mutate(part_lits_tourisme_2023 = lits_tourisme_2023 / pop2019,
         artif_efficacite_habitat = men0919 / artifhab,
         artif_efficacite_activite = emp1121 / act1121) %>%
  select(-pop2019, -lits_tourisme_2023, -artifhab, -act1121) %>%
  write_excel_csv("res/determinants-artificialisation.csv", quote = "all")

determinants.artificialisation %>%
  select(-CODGEO, -LIBGEO, -artif_efficacite_habitat, -artif_efficacite_activite, -DEP) %>%
  filter_all(all_vars(!is.na(.))) %>%
  PCA(ncp = 5, graph = FALSE, 
      quanti.sup = c("artcom2020", "nafart1121"), 
      quali.sup = c("libdens"),
      scale.unit = TRUE) -> res.pca

explor(res.pca)

summary(res.pca)

