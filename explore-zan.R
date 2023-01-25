# Auteur
# Jules Rostand

# Description
# Ce script R propose une classification des EPCI selon l'artificialisation des sols et ses déterminants.

# Bibliothèques
library(tidyverse)
library(magrittr)
library(textshape)
library(pastecs)
library(FactoMineR)
library(factoextra)
library(questionr)
library(explor)
library(missMDA)

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
#artif.map %>%
#  left_join(artif.epci) %>%
#  select(epci21, epci21txt, clust, nafart1121_tot, artcom2020_tot) %>%
#  write_excel_csv("res/artif-epci-map.csv", quote = "all")
  
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
#demographie.map %>%
#  rownames_to_column("epci21") %>%
#  left_join(artif.epci, by = "epci21") %>%
#  select(epci21, epci21txt, clust, varpop0818, sn_brut0818, sm_brut0818) %>%
#  write_excel_csv("res/demographie-epci-map.csv", quote = "all")

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

#insee.poptot0818.rec.clust %>%
#  write_excel_csv("res/demographie-epci-binaire-map.csv", quote = "all")

## Agriculture
# https://stats.agriculture.gouv.fr/cartostat/
# Statistique agricole annuelle

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

# Fonction de calcul du mode statistique
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

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

# Arrondis et exprt des données
agreste2020.epci %<>%
  mutate(across(12:19, round, 2)) %>%
  ungroup() %>%
  write_excel_csv("res/agriculture-epci-data.csv", quote = "all")

# Essais d'analyses factorielles
agreste2020.epci %>%
  column_to_rownames("intercommunalite_code") %>%
  select(sau_evolution_2020_2010_somme, 
         sau_moyenne_evolution_2020_2010_somme, 
         nb_exploitation_evolution_2020_2010_somme, 
         specialisation12_libelle_mode) %>%
  filter_all(all_vars(!is.na(.))) %>%
  FAMD(ncp = 5, graph = FALSE) -> agreste.famd

# Questions des données manquantes
#imputePCA(agreste2020.epci.pre, ncp = 2) -> agreste2020.epci.sansna
#PCA(agreste2020.epci.sansna, ncp = 2, graph = FALSE) -> agreste.pca
#) -> 
#missMDA::imputeFAMD(agreste2020.epci.test)$completeObs -> agreste2020.epci.sansna
#agreFAMD(ncp = 5, graph = FALSE) -> agreste.famd
#explor(agreste.pca)

# Calculs
agreste.hcpc = HCPC(agreste.pca)
agreste.hcpc$data.clust -> agreste.map

# Exports
agreste2020.epci %>%
  select(intercommunalite_code, intercommunalite_libelle) -> passage

agreste.map %>%
  rownames_to_column("intercommunalite_code") %>%
  left_join(passage) %>%
  select(intercommunalite_code, intercommunalite_libelle, 
       sau_evolution_2020_2010_somme, sau_moyenne_evolution_2020_2010_somme,
       nb_exploitation_evolution_2020_2010_somme, clust) %>%
  write_excel_csv("res/agriculture-epci-map.csv", quote = "all")
