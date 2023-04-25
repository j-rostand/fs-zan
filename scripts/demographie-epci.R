# Auteur
# Jules Rostand

# Description
# Ce script R propose une classificiation simple de la tendance démographique 
# au niveau de l'EPCI sur la période 2008-2018, à partir des données de l'Insee

# Bibliothèques
library(tidyverse)

###############
### SOURCES ###
###############

# Import des données démographiques de l'Insee (Observatoire des Territoires)
# https://www.observatoire-des-territoires.gouv.fr/visiotheque/2017-dynpop-typologie-de-levolution-de-la-population-entre-1999-et-2013

readxl::read_xlsx(path = "./../data/insee_rp_evol_1968_var_pop.xlsx", skip = 4) -> insee.varpop
readxl::read_xlsx(path = "./../data/insee_rp_evol_1968_sn_brut.xlsx", skip = 4) -> insee.soldenat
readxl::read_xlsx(path = "./../data/insee_rp_evol_1968_sm_brut.xlsx", skip = 4) -> insee.soldemig

###################
### TRAITEMENTS ###
###################

insee.varpop %>%
  left_join(insee.soldenat) %>%
  left_join(insee.soldemig) -> insee.poptot

# Filtre sur la période 2008-2018 pour les données démographiques
insee.poptot %>%
  filter(an == "2008-2013" | an == "2013-2018") %>%
  group_by(codgeo, libgeo) %>%
  summarise(varpop0818 = sum(var_pop), 
            sn_brut0818 = sum(sn_brut), 
            sm_brut0818 = sum(sm_brut)) %>%
  rename("epci21" = codgeo, "epci21txt" = libgeo) %>%
  na.omit() -> insee.poptot0818

# Classification manuelle pour la démographie
# https://www.observatoire-des-territoires.gouv.fr/typologie-des-soldes-naturel-et-migratoire-apparent

insee.poptot0818 %>%
  mutate(varpop0818_rec = case_when(varpop0818 < 0 ~ 0, varpop0818 >= 0 ~ 1)) %>%
  mutate(sn_brut0818_rec = case_when(sn_brut0818 < 0 ~ 0, sn_brut0818 >= 0 ~ 1)) %>%
  mutate(sm_brut0818_rec= case_when(sm_brut0818 < 0 ~ 0, sm_brut0818 >= 0 ~ 1)) %>%
  mutate(clust = case_when(varpop0818_rec == 1 & sn_brut0818_rec == 1 & sm_brut0818_rec == 1 ~ "Croissance totale",
                           varpop0818_rec == 1 & sn_brut0818_rec == 1 & sm_brut0818_rec == 0 ~ "Croissance liée à un solde naturel positif",
                           varpop0818_rec == 1 & sn_brut0818_rec == 0 & sm_brut0818_rec == 1 ~ "Croissance liée à un solde migratoire apparent positif",
                           varpop0818_rec == 0 & sn_brut0818_rec == 1 & sm_brut0818_rec == 0 ~ "Décroissance liée à un solde migratoire apparent négatif",
                           varpop0818_rec == 0 & sn_brut0818_rec == 0 & sm_brut0818_rec == 1 ~ "Décroissance liée à un solde naturel négatif",
                           varpop0818_rec == 0 & sn_brut0818_rec == 0 & sm_brut0818_rec == 0 ~ "Décroissance totale")) %>%
  select(epci21, epci21txt, clust, varpop0818, sn_brut0818, sm_brut0818) -> insee.poptot0818.rec

##############
### EXPORT ###
##############

insee.poptot0818.rec %>%
  write_excel_csv("./../res/demographie-epci-2008-2018.csv", quote = "all")
