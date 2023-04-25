# Mission sur le Zéro Artificialisation Nette (ZAN)

Dans le cadre de la mission sur l'objectif Zéro Artificialisation Nette (ZAN), réalisée à France Stratégie à la suite du rapport [Soutenabilités!](https://www.strategie.gouv.fr/publications/soutenabilites-orchestrer-planifier-laction-publique), ce dossier contient les programmes d'analyse et de traitements de données sur les niveaux, la répartition et les déterminants de la consommation d'espaces naturels, agricoles et forestiers (ENAF) au sens des fichiers fonciers.

## Utilisation

Au sein du dossier `script`, les programmes suivants permettent : 

- S'agissant des scripts `agriculture-EPCI.R` et `demographie-EPCI.R`, d'obtenir un tableau de données contenant les évolutions des variables d'intérêt sur une période de référence de dix ans au niveau de l'EPCI ;
- S'agissant du fichier `artificialisaiton-flux.R`, de calculer les différents flux d'artificialisation sur une période de dix ans, au niveau de la commune, de l'EPCI et du SCOT ;
- S'agissant du programme `artificialisation-repartition.R`, de réaliser des graphiques représentant la répartition des flux d'artificialisation selon le niveau de densité des communes ;
- Enfin, s'agissant du programme `artificialisation-determinants`, de mener une analyse en composantes principales sur les déterminants le niveau d'artificialisation.

## Sources et données

Avant de lancer l'exécution des programmes, il convient de disposer des données, selon l’arborescence présentée ci-dessous :

```
├── agreste-saa-2010.xlsx
├── agreste-saa-2020.xlsx
├── base-cc-serie-historique-2018.xlsx
├── base-cc-tourisme-2023-geo2022.xlsx
├── commune_2022.csv
├── efficacitehab.txt
├── empsal.txt
├── ensoleillement.xlsx
├── filosofi.xlsx
├── grille_densite_7_niveaux_detaille_2023.xlsx
├── indic_sex_rp.xlsx
├── insee_rp_evol_1968_sm_brut.xlsx
├── insee_rp_evol_1968_sn_brut.xlsx
├── insee_rp_evol_1968_var_pop.xlsx
├── insee_rp_hist_1968.xlsx
├── logement1.csv
├── obs_artif_conso_com_2009_2021.csv
├── table-appartenance-geo-communes-2020.xlsx
└── valeurimmo.txt
```

Ces données sont issues de sources publiques accessibles en ligne :

- [Le suivi de la consommation d'espaces naturels, agricoles et forestiers (NAF)](https://cerema.app.box.com/v/pnb-action7-indicateurs-ff), publié par l'Observatoire de l'artificialisation des sols ; 
- [Indicateurs issus de la typologie de l'évolution de la population entre 1999 et 2013](https://www.observatoire-des-territoires.gouv.fr/visiotheque/2017-dynpop-typologie-de-levolution-de-la-population-entre-1999-et-2013), publiée par l'Observatoire des territoires.
- [Le recensement agricole](https://stats.agriculture.gouv.fr/cartostat/#c=indicator), réalisé par le Ministère de l'Agriculture ;
- [Table d’appartenance géographique des communes et tables de passage](https://www.insee.fr/fr/information/2028028), réalisée par l'Insee ;
- [Le recensement de la population (base des principaux indicateurs) issues des séries historiques en 2018](https://www.insee.fr/fr/statistiques/5395965), publié par l'Insee ;
- [La grille de densité des communes en 2023](https://www.insee.fr/fr/statistiques/fichier/6439600/grille_densite_7_niveaux_detaille_2023.xlsx
), réalisée par l'Insee.

## Crédits

Ce travail a été réalisé en collaboration étroite avec Hélène Arambourou, au sein de l'équipe-projet ZAN, 
