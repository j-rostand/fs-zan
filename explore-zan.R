# Auteur
# Jules Rostand

# Description
# Ce script R propose une classification des EPCI selon l'artificialisation des sols et ses déterminants.

# Bibliothèques
library(tidyverse)
library(textshape)
library(pastecs)
library(FactoMineR)
library(factoextra)
library(questionr)
library(explor)

#################################################
## Préparation des données : artificialisation ##
#################################################

# Import des données de l'observatoire de l'artificialisation des sols
read_delim(file = "data/obs_artif_conso_com_2009_2021.csv",
           delim = ";", 
           escape_double = FALSE,
           trim_ws = TRUE) -> artif

stat.desc(artif[73:77]) # 5 variables de flux d'artificialisation 2009-2021

# Calculs des flux de référence 2011-2021

# Flux entre NAF et artificialisé sur la période 2011-2021
artif$nafart1121 = artif$naf11art12 + artif$naf12art13 + artif$naf13art14 + 
  artif$naf14art15 + artif$naf15art16 + artif$naf16art17 + artif$naf17art18 + 
  artif$naf18art19 + artif$naf19art20 + artif$naf19art20 + artif$naf20art21

# Flux NAF vers artificialisé destiné à l'activité sur la période 2011-2021
artif$artact1121 = artif$art11act12 + artif$art12act13 + artif$art13act14 + 
  artif$art14act15 + artif$art15act16 + artif$art16act17 + artif$art17act18 + 
  artif$art18act19 + artif$art19act20 + artif$art19act20 + artif$art20act21

# Flux NAF vers artificialisé destiné à l'habitat sur la période 2011-2021
artif$arthab1121 = artif$art11hab12 + artif$art12hab13 + artif$art13hab14 + 
  artif$art14hab15 + artif$art15hab16 + artif$art16hab17 + artif$art17hab18 + 
  artif$art18hab19 + artif$art19hab20 + artif$art19hab20 + artif$art20hab21

# Flux NAF vers artificialisé destiné au mixte sur la période 2011-2021
artif$artmix1121 = artif$art11mix12 + artif$art12mix13 + artif$art13mix14 + 
  artif$art14mix15 + artif$art15mix16 + artif$art16mix17 + artif$art17mix18 + 
  artif$art18mix19 + artif$art19mix20 + artif$art19mix20 + artif$art20mix21

# Flux NAF vers artificialisé dont la destination est inconnue sur la période 2011-2021
artif$artinc1121 = artif$art11inc12 + artif$art12inc13 + artif$art13inc14 + 
  artif$art14inc15 + artif$art15inc16 + artif$art16inc17 + artif$art17inc18 + 
  artif$art18inc19 + artif$art19inc20 + artif$art19inc20 + artif$art20inc21

# Regroupement par EPCI
artif %>%
  group_by(epci21, epci21txt) %>%
  summarise(nafart1121_tot = sum(nafart1121), 
            artact1121_tot = sum(artact1121), 
            arthab1121_tot = sum(arthab1121), 
            artmix1121_tot = sum(artmix1121),
            artinc1121_tot = sum(artinc1121), 
            artcom2020_tot = sum(artcom2020 * surfcom2021) / sum(surfcom2021),
            surfcom2021_tot = sum(surfcom2021)) -> artif.epci

###########################################
## Préparation des données : démographie ##
###########################################

# Import des données démographiques de l'Insee, mises à disposition par l'Observatoire des Territoires de l'ANCT
# https://www.observatoire-des-territoires.gouv.fr/visiotheque/2017-dynpop-typologie-de-levolution-de-la-population-entre-1999-et-2013
readxl::read_xlsx(path = "data/insee_rp_evol_1968_var_pop.xlsx", skip = 4) -> insee.varpop
readxl::read_xlsx(path = "data/insee_rp_evol_1968_sn_brut.xlsx", skip = 4) -> insee.soldenat
readxl::read_xlsx(path = "data/insee_rp_evol_1968_sm_brut.xlsx", skip = 4) -> insee.soldemig

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

# Tableau de données complet (démographie et artificialisation)
artif.epci %>%
  left_join(insee.poptot0818, by = "epci21") %>%
  relocate(epci21, epci21txt.x, epci21txt.y, varpop0818, sn_brut0818, sm_brut0818, nafart1121_tot, 
           artact1121_tot, arthab1121_tot, artmix1121_tot, artinc1121_tot, artcom2020_tot, surfcom2021_tot) %>%
  mutate(across(surfcom2021_tot, as.numeric)) %>%
  ungroup() %>%
  filter_all(all_vars(!is.na(.))) -> artpop.epci

##########################
## Analyse des données ###
##########################

# Analyses en composantes principales sur l'artificialisation

artpop.epci %>%
  column_to_rownames("epci21") %>%
  select(nafart1121_tot, artcom2020_tot) %>%
  filter_all(all_vars(!is.na(.))) %>%
  PCA(ncp = 5, graph = FALSE) -> res.pca

# explor(res.pca)

res.hcpc = HCPC(res.pca)
res.hcpc$data.clust -> artif.map
artif.map %>%
  left_join(artif.epci) %>%
  select(epci21, epci21txt, clust, nafart1121_tot, artcom2020_tot) %>%
  write_excel_csv("res/artif-epci-map.csv", quote = "all")
  
# Analyses en composantes principales sur la démographie

artpop.epci %>%  
  filter(epci21 != "200054781") %>%
  column_to_rownames("epci21") %>%
  select(varpop0818, sn_brut0818, sm_brut0818) %>%
  filter_all(all_vars(!is.na(.))) %>%
  PCA(ncp = 5, graph = FALSE) -> res2.pca

explor(res2.pca)

res2.hcpc = HCPC(res2.pca)
res2.hcpc$data.clust -> demographie.map
demographie.map %>%
  rownames_to_column("epci21") %>%
  left_join(artif.epci, by = "epci21") %>%
  select(epci21, epci21txt, clust, varpop0818, sn_brut0818, sm_brut0818) %>%
  write_excel_csv("res/demographie-epci-map.csv", quote = "all")

# Classification manuelle pour la démographie
# https://www.observatoire-des-territoires.gouv.fr/typologie-des-soldes-naturel-et-migratoire-apparent

insee.poptot0818 %>%
  mutate(varpop0818_rec = case_when(varpop0818 < 0 ~ 0, varpop0818 >= 0 ~ 1)) %>%
  mutate(sn_brut0818_rec = case_when(sn_brut0818 < 0 ~ 0, sn_brut0818 >= 0 ~ 1)) %>%
  mutate(sm_brut0818_rec= case_when(sm_brut0818 < 0 ~ 0, sm_brut0818 >= 0 ~ 1)) %>%
  select(epci21, epci21txt, varpop0818_rec, sn_brut0818_rec, sm_brut0818_rec) -> insee.poptot0818.rec

insee.poptot0818.rec %>%
  mutate(clust = case_when(varpop0818_rec == 1 & sn_brut0818_rec == 1 & sm_brut0818_rec == 1 ~ "Croissance totale",
                           varpop0818_rec == 1 & sn_brut0818_rec == 1 & sm_brut0818_rec == 0 ~ "Croissance liée à un solde naturel positif",
                           varpop0818_rec == 1 & sn_brut0818_rec == 0 & sm_brut0818_rec == 1 ~ "Croissance liée à un solde migratoire apparent positif",
                           varpop0818_rec == 0 & sn_brut0818_rec == 1 & sm_brut0818_rec == 0 ~ "Décroissance liée à un solde migratoire apparent négatif",
                           varpop0818_rec == 0 & sn_brut0818_rec == 0 & sm_brut0818_rec == 1 ~ "Décroissance liée à un solde naturel négatif",
                           varpop0818_rec == 0 & sn_brut0818_rec == 0 & sm_brut0818_rec == 0 ~ "Décroissance totale")) %>%
  select(epci21, epci21txt, clust) -> insee.poptot0818.rec.clust

insee.poptot0818.rec.clust %>%
  write_excel_csv("res/demographie-epci-binaire-map.csv", quote = "all")

