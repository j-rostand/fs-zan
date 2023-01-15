# Auteur
# Jules Rostand

# Description
# Ce script R propose une exploration dans les données de l'artificialisation des sols.

# Librairies génériques
library(readr)
library(dplyr)
library(ggplot2)

# Statistiques descriptives
library(pastecs)

# Statistiques avancées
library(FactoMineR)
library(factoextra)
library(questionr)
library(explor)
library(forcats)

########################
## Import des données ##
########################

# Import des données de l'observatoire de l'artificialisation des sols

read_delim(file = "data/obs_artif_conso_com_2009_2021.csv",
           delim = ";", 
           escape_double = FALSE,
           trim_ws = TRUE) -> artif

## Calculs des flux de référence 2011-2021

### Flux entre NAF et artificialisé sur la période 2011-2021

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

## Regroupement par EPCI

artif %>%
  group_by(epci21, epci21txt) %>%
  summarise(nafart1121_tot = sum(nafart1121), 
            artact1121_tot = sum(artact1121), 
            arthab1121_tot = sum(arthab1121), 
            artmix1121_tot = sum(artmix1121),
            artinc1121_tot = sum(artinc1121), 
            artcom2020_tot = sum(artcom2020 * surfcom2021) / sum(surfcom2021),
            surfcom2021_tot = sum(surfcom2021)) -> artif.epci

# Import des données démographiques de l'Insee
# TODO : à refaire avec les données de l'Insee reprises par l'ANCT, qui l'a réalisée à l'échelle bassin de vie
# https://www.observatoire-des-territoires.gouv.fr/visiotheque/2017-dynpop-typologie-de-levolution-de-la-population-entre-1999-et-2013

#read_delim(file = "data/data_insee_2013-2019.csv",
#           delim = ";", 
#           escape_double = FALSE,
#           skip = 2,
#           trim_ws = TRUE,
#           col_types = "cccc") -> pop1319

readxl::read_xlsx(path = "data/insee_rp_evol_1968_var_pop.xlsx", skip = 4) -> insee.varpop
readxl::read_xlsx(path = "data/insee_rp_evol_1968_sn_brut.xlsx", skip = 4) -> insee.soldenat
readxl::read_xlsx(path = "data/insee_rp_evol_1968_sm_brut.xlsx", skip = 4) -> insee.soldemig

insee.varpop %>%
  left_join(insee.soldenat) %>%
  left_join(insee.soldemig) -> insee.poptot

insee.poptot %>%
  filter(an == "2008-2013" | an == "2013-2018") %>%
  group_by(codgeo, libgeo) %>%
  summarise(varpop0818 = sum(var_pop), 
            sn_brut0818 = sum(sn_brut), 
            sm_brut0818 = sum(sm_brut)) %>%
  rename("epci21" = codgeo, "epci21txt" = libgeo) %>%
  na.omit()-> insee.poptot0818

#pop1319 %>%
#  rename("epci21" = Code, "epci21insee" = Libellé, 
#         "soldeapp1319" = `Évol. annuelle moy. de la pop. due au solde apparent entrées/sorties 2013-2019`,
#         "soldenat1319" = `Évol. annuelle moy. de la pop. due au solde naturel 2013-2019`) %>%
#  right_join(artif.epci, by = "epci21") %>%
#  relocate(epci21, epci21insee, epci21txt) %>%
#  mutate(across(soldeapp1319, as.numeric)) %>%
#  mutate(across(soldenat1319, as.numeric)) %>%
#  na.omit() -> artpop.epci

artif.epci %>%
  left_join(insee.poptot0818, by = "epci21") %>%
  relocate(epci21, epci21txt.x, epci21txt.y, varpop0818, sn_brut0818, sm_brut0818, nafart1121_tot, 
           artact1121_tot, arthab1121_tot, artmix1121_tot, artinc1121_tot, artcom2020_tot, surfcom2021_tot) %>%
  mutate(across(surfcom2021_tot, as.numeric)) %>%
  ungroup() %>%
  filter_all(all_vars(!is.na(.)))-> artpop.epci
  
# Transformaition en classes (icut)

#artpop.epci$varpop0818 = cut(artpop.epci$varpop0818, breaks=quantile(artpop.epci$varpop0818), labels = FALSE)
#artpop.epci$sn_brut0818 = cut(artpop.epci$sn_brut0818, breaks=quantile(artpop.epci$sn_brut0818), labels = FALSE)
#artpop.epci$sm_brut0818 = cut(artpop.epci$sm_brut0818, breaks=quantile(artpop.epci$sm_brut0818), labels = FALSE)
#artpop.epci$nafart1121_tot = cut(artpop.epci$nafart1121_tot, breaks=quantile(artpop.epci$nafart1121_tot), labels = FALSE)
#artpop.epci$artact1121_tot = cut(artpop.epci$artact1121_tot, breaks=quantile(artpop.epci$artact1121_tot), labels = FALSE)
#artpop.epci$arthab1121_tot = cut(artpop.epci$arthab1121_tot, breaks=quantile(artpop.epci$arthab1121_tot), labels = FALSE)
#artpop.epci$artmix1121_tot = cut(artpop.epci$artmix1121_tot, breaks=quantile(artpop.epci$artmix1121_tot), labels = FALSE)
#artpop.epci$artinc1121_tot = cut(artpop.epci$artinc1121_tot, breaks=quantile(artpop.epci$artinc1121_tot), labels = FALSE)
#artpop.epci$surfcom2021_tot = cut(artpop.epci$surfcom2021_tot, breaks=quantile(artpop.epci$surfcom2021_tot), labels = FALSE)

# ACM

#artpop.epci %>%
#  select(-epci21, -epci21txt.x, -epci21txt.y) %>%
#  filter_all(all_vars(!is.na(.))) %>%
#  MCA(ncp = 5, quanti.sup = 5:9, graph = FALSE, na.method = ) -> res.mca

artpop.epci %>%
  select(nafart1121_tot, surfcom2021_tot, artcom2020_tot) %>%
  filter_all(all_vars(!is.na(.))) %>%
  PCA(ncp = 5, graph = FALSE) -> res.pca

res.hcpc = HCPC(res.pca)

print(res.pca)
barplot(res.pca$eig[, 2], main="Histogramme des valeurs propres", names.arg=rownames(res.pca$eig), xlab="Axes", ylab="Pourcentage d’inertie", cex.axis=0.8, font.lab=3, col="orange")

explor(res.pca)

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8      # Augment l'espace pour le texte
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map"
)

