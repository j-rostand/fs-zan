# Auteur
# Jules Rostand

# Description
# Ce script R propose un calcul des flux de référence de l'artificialisation sur la période 2011-2021
# au niveau de l'EPCI à partir des données de l'Observatoire national d'artificialisation des sols

# Bibliothèques
library(tidyverse)
library(pastecs)

###############
### SOURCES ###
###############

# Import des données de l'Observatoire de l'artificialisation des sols
# https://cerema.app.box.com/v/pnb-action7-indicateurs-ff
read_delim(file = "data/obs_artif_conso_com_2009_2021.csv",
           delim = ";", 
           escape_double = FALSE,
           trim_ws = TRUE) -> artif

###################
### TRAITEMENTS ###
###################

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

# Regroupement par SCOT
artif %>%
  group_by(scot) %>%
  summarise(nafart1121_tot = sum(nafart1121), 
            artact1121_tot = sum(artact1121), 
            arthab1121_tot = sum(arthab1121), 
            artmix1121_tot = sum(artmix1121),
            artinc1121_tot = sum(artinc1121), 
            artcom2020_tot = sum(artcom2020 * surfcom2021) / sum(surfcom2021),
            surfcom2021_tot = sum(surfcom2021)) -> artif.scot

##############
### EXPORT ###
##############

# Export par communes
artif %>% 
  write_excel_csv("res/artificialisation-communes-2011-2021.csv", quote = "all")
# Export par EPCI
artif.epci %>% 
  write_excel_csv("res/artificialisation-epci-2011-2021.csv", quote = "all")
# Export par SCOT
artif.scot %>% 
  write_excel_csv("res/artificialisation-scot-2011-2021.csv", quote = "all")
