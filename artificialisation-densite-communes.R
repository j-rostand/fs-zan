# Auteur
# Jules Rostand

# Description
# Ce script R propose une analyse de la répartition de l'artificialisation en fonction de la 
# grille de densité communale de l'Insee

# Bibliothèques
library(tidyverse)
library(ggplot2)

# Sources

# https://www.insee.fr/fr/statistiques/5395965
readxl::read_xlsx(path = "data/base-cc-serie-historique-2018.xlsx", 
                  skip = 5, na = "N/A - résultat non disponible") -> insee_recensement_2018
# https://www.insee.fr/fr/statistiques/fichier/6439600/grille_densite_7_niveaux_detaille_2023.xlsx
readxl::read_xlsx(path = "data/grille_densite_7_niveaux_detaille_2023.xlsx", 
                  skip = 1) -> insee_grille_densite_2023
# Fichier de l'Observatoire, transformé par le script artificialisation 
read_csv(file = "res/artificialisation-communes-2011-2021.csv", 
         col_types = "cc") -> artificialisation_communes_2021

# Jointures
insee_grille_densite_2023 %>%
  left_join(artificialisation_communes_2021, by = c("depcom" = "idcom")) %>%
  left_join(insee_recensement_2018, by = c("depcom" = "CODGEO")) %>%
  mutate(menhab0818 = case_when(art09hab21 == 0 ~ 0, TRUE ~ (P18_RP - P08_RP)/(art09hab21/10000)),
         menages_tcam = ((P18_RP/P08_RP)^(1/10) - 1)) %>%
  select(depcom, libcom, libdens, artcom2020, nafart1121, 
         arthab1121, artact1121, menhab1318, menhab0818, menages_tcam) -> territoires1

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
tapply(territoires1$menhab0818, territoires1$libdens, FUN = summary) 

# Surface communale artificialisé sur la période
territoires1 %>% 
  ggplot(aes(x = libdens, y=nafart1121/10000)) + 
  geom_boxplot(notch = FALSE, outlier.shape = NA) +
  scale_y_continuous(limits=c(0, 100)) +
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
  scale_y_continuous(limits=c(0, 10)) +
  stat_summary(fun.y=mean, geom="point", shape=23) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  labs(title="Part de la surface artificialisée en 2020 en fonction de la densité communale",
       x ="", y = "en %") 

# Surface communale artificialisé pour l'habitat
territoires1 %>% 
  ggplot(aes(x = libdens, y=arthab1121/10000)) + 
  geom_boxplot(notch = FALSE, outlier.shape = NA) +
  scale_y_continuous(limits=c(0, 100)) +
  stat_summary(fun.y=mean, geom="point", shape=23) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Nombre moyen de ménages par hectares artificialisé 
territoires1 %>% 
  ggplot(aes(x = libdens, y=log10(menhab0818))) + 
    geom_boxplot(notch = FALSE, outlier.shape = NA) +
    stat_summary(fun.y=mean, geom="point", shape=23) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.title.y = element_text(size = 8),
          plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
    labs(title="Nombre de ménages par hectare artificialisée (log10) sur la période 2008-2018 en fonction de la densité communale",
         x ="", y = "en log10(ménages / ha)")

# Taux de croissance annuel moyen du nombre de ménages
territoires1 %>% 
  ggplot(aes(x = libdens, y=menages_tcam)) + 
  geom_boxplot(notch = FALSE, outlier.shape = NA) +
  scale_y_continuous(limits=c(-0.025, 0.05)) +
  stat_summary(fun.y=mean, geom="point", shape=23) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
  labs(title="Taux de croissance annuel moyen du nombre de ménages sur la période 2008_2018, en fonction de la densité communale",
       x ="", y = "en %") 

# Comparaison entre la surface consommée et son efficacité

territoires1 %>%
  droplevels() %>%
  drop_na() %>%
  group_by(libdens) %>%
    summarise(hab = log10(mean(arthab1121)),
              men = log10(mean(menhab0818))) %>%
  ggplot(aes(x = libdens)) +
    geom_point(aes(y = hab), shape = 23) +
    geom_point(aes(y = men), shape = 2) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y = element_text(size = 8),
        plot.title = element_text(size=10, hjust = 0.5, face="bold")) +
    labs(title="Artificialisation pour l'habitat, en surface et en surface par ménages, sur la période 2008-2018, en fonction de la densité communale",
       x ="", y = "log10") 
