# Auteur
# Jules Rostand

# Description
# Ce script R propose une analyse de la répartition de l'artificialisation 
# selon grille de densité communale de l'Insee

# Bibliothèques
library(tidyverse)
library(ggplot2)

###############
### SOURCES ###
###############

# https://www.insee.fr/fr/statistiques/5395965
readxl::read_xlsx(path = "./../data/base-cc-serie-historique-2018.xlsx", 
                  skip = 5, na = "N/A - résultat non disponible") -> insee_recensement_2018
# https://www.insee.fr/fr/statistiques/fichier/6439600/grille_densite_7_niveaux_detaille_2023.xlsx
readxl::read_xlsx(path = "./../data/grille_densite_7_niveaux_detaille_2023.xlsx", 
                  skip = 1) -> insee_grille_densite_2023
# Fichier de l'Observatoire national, tel que transformé par le script artificialisation-flux.R
read_csv(file = "./../res/artificialisation-communes-2011-2021.csv", 
         col_types = "cc") -> artificialisation_communes_2021

###################
### TRAITEMENTS ###
###################

# Jointures
insee_grille_densite_2023 %>%
  left_join(artificialisation_communes_2021, by = c("depcom" = "idcom")) %>%
  left_join(insee_recensement_2018, by = c("depcom" = "CODGEO")) %>%
  mutate(menhab0818 = case_when(arthab1121 == 0 ~ 0, TRUE ~ (P18_RP - P08_RP)/(arthab1121/10000)),
         menages_tcam = ((P18_RP/P08_RP)^(1/10) - 1),
         parthab = arthab1121 / nafart1121) %>%
  select(depcom, libcom, libdens, artcom2020, nafart1121, 
         arthab1121, parthab, menhab1318, menhab0818, menages_tcam) -> territoires1

# Factorisation
territoires1$libdens <- as.factor(territoires1$libdens)
territoires1$libdens <- factor(territoires1$libdens, levels = c("Rural à habitat très dispersé",
                                                         "Rural à habitat dispersé",
                                                         "Bourgs ruraux", 
                                                         "Ceintures urbaines", 
                                                         "Petites villes", 
                                                         "Centres urbains intermédiaires", 
                                                         "Grands centres urbains"))

# Statistiques descriptive sur les variables, utiles pour vérifier la déformation liée à la
# représentation graphique
tapply(territoires1$artcom2020, territoires1$libdens, FUN = summary) 

##################
### GRAPHIQUES ###
##################

# Surface communale artificialisé sur la période
territoires1 %>% 
  ggplot(aes(x = libdens, y=nafart1121/10000)) + 
  geom_boxplot(notch = FALSE, outlier.shape = NA) +
  coord_cartesian(ylim=c(0, 100)) + 
  stat_summary(fun.y=mean, geom="point", shape=23) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  labs(title="Surface artificialisée sur la période 2011-2021 en fonction de la densité communale",
       x ="", y = "en ha") 

# Part de la surface communale artificialisée (stock)
territoires1 %>% 
  ggplot(aes(x = libdens, y=artcom2020)) + 
  geom_boxplot(notch = FALSE, outlier.shape = NA) +
  coord_cartesian(ylim=c(0, 5)) + 
  stat_summary(fun = "mean", geom="point", shape=23) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  labs(title="Part de la surface artificialisée en 2020 en fonction de la densité communale",
       x ="", y = "en %") 

# Nombre moyen de ménages par hectares artificialisé 
territoires1 %>% 
  ggplot(aes(x = libdens, y=log10(menhab0818))) + 
    geom_boxplot(notch = FALSE, outlier.shape = NA) +
    stat_summary(fun.y=mean, geom="point", shape=23) +
    coord_cartesian(ylim=c(0, 3)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
    labs(title="Nombre de ménages par hectare artificialisée (log10) sur la période 2008-2018 en fonction de la densité communale",
         x ="", y = "en log10(ménages / ha)")

# Taux de croissance annuel moyen du nombre de ménages
territoires1 %>% 
  ggplot(aes(x = libdens, y=menages_tcam)) + 
  geom_boxplot(notch = FALSE, outlier.shape = NA) +
  coord_cartesian(ylim=c(-0.02, 0.04)) + 
  stat_summary(fun.y=mean, geom="point", shape=23) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  labs(title="Taux de croissance annuel moyen du nombre de ménages sur la période 2008_2018, en fonction de la densité communale",
       x ="", y = "en %")

##############
### EXPORT ###
##############

# Tableau des moyennes des variables de l'artificialisation en fonction de la densité communale
territoires1 %>%
  droplevels() %>%
  drop_na() %>%
  group_by(libdens) %>%
  summarise(nombre = n(),
            nafart1121_mean = mean(nafart1121),
            arthab1121_mean = mean(arthab1121),
            artcom2020_mean = mean(artcom2020),
            parthab_mean = mean(parthab),
            menages_tcam_mean = mean(menages_tcam),
            menhab0818_mean = mean(menhab0818)) %>%
  mutate(across(2:4, round, 1)) %>%
  mutate(across(5:7, round, 4)) %>%
  mutate(across(8:8, round, 1)) %>%
  write.csv(file = "./../res/artificialisation-moyennes-densite-communale.csv")

  