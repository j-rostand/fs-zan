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
  select(COM, LIBELLE) %>%
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
readxl::read_xlsx(path = "data/indic_sex_rp.xlsx", skip = 4) %>%
  filter(sexe == "T" & an == "2019") %>%
  select(codgeo, p_csp_cadpis) %>%
  rename("CODGEO" = codgeo, 
         "part_csp_cadres_intellectuelles_2019" = p_csp_cadpis) -> insee.csp2019

# LN
read_delim(file = "data/logement.txt",
           delim = " ", col_names = TRUE, col_types = c("ccdddd")) %>%
  select(codeinsee, evollog0819, evolsec0819, partlogvacant, partproprios) %>%
  rename("CODGEO" = codeinsee) -> insee.logement

read_csv(file = "res/artificialisation-communes-2011-2021.csv", 
         col_types = "cc") %>%
  select(idcom, artcom2020, nafart1121) %>%
  rename("CODGEO" = idcom)-> onas.communes

insee.communes22 %>%
  left_join(insee.tourisme, by = "CODGEO") %>%
  left_join(insee.pop2019, by = "CODGEO") %>%
  left_join(agreste2020, by = "CODGEO") %>%
  left_join(filosofi.revenu2020, by = "CODGEO") %>%
  left_join(insee.menages0919, by = "CODGEO") %>%
  left_join(insee.emploi1121, by = "CODGEO") %>% 
  left_join(insee.logement, by = "CODGEO") %>% 
  left_join(onas.communes, by = "CODGEO") %>% 
  left_join(insee.csp2019, by = "CODGEO") -> determinants.artificialisation

determinants.artificialisation %>%
  write_excel_csv("res/determinants-artificialisation.csv", quote = "all")

determinants.artificialisation %>%
  mutate(part_lits_tourisme_2023 = lits_tourisme_2023 / pop2019) %>%
  select(-CODGEO, -LIBGEO, -pop2019, -lits_tourisme_2023) %>%
  filter_all(all_vars(!is.na(.))) %>%
  PCA(ncp = 5, graph = FALSE, quanti.sup = c("artifhab", "act1121", "artcom2020", "nafart1121")) -> res.pca

explor(res.pca)

summary(res.pca)

