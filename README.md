# Soutenabilités x Territoires : le Zéro Artificialisation Nette (ZAN)

Préparation et exploration des données d'artificialisation des sols, de démographie et de situation agraire au niveau des EPCI dans le cadre de la mission de France Stratégie sur l'objectif Zéro Artificialisation Nette (ZAN).

## Utilisation

Ce projet contient deux programmes distincts : 

1. Le fichier `explore-zan.R` propose une exploration des données d'artificialisation des sols, de démographie et de situation agraire au niveau des EPCI, qui peut être utilisé à des fins d'essais d'analyses croisées ;
2. Les fichiers `artificialisation-epci.R`, `demographie-EPCI.R` et `artificialisation-EPCI.R` contiennent les instructions nécessaires à l'obtention de fichiers de données normalisés au niveau de l'EPCI, sur des périodes de dix années, qui peuvent être servir de référence pour l'analyse de chacune de ces thématiques.

## Sources et données

Avant de lancer l'exécution des programmes, il convient de disposer des données, selon l’arborescence présentée ci-dessous :

```
├── data
│   ├── agreste-saa-2020.xlsx
│   ├── insee_rp_evol_1968_sm_brut.xlsx
│   ├── insee_rp_evol_1968_sn_brut.xlsx
│   ├── insee_rp_evol_1968_var_pop.xlsx
│   ├── obs_artif_conso_com_2009_2021.csv
│   └── table-appartenance-geo-communes-2020.xlsx
```

Ces données publiques sont accessibles en ligne :

- [Le suivi de la consommation d'espaces NAF](https://cerema.app.box.com/v/pnb-action7-indicateurs-ff), publié par l'Observatoire de l'artificialisation des sols ; 
- [Indicateurs issus de la typologie de l'évolution de la population entre 1999 et 2013](https://www.observatoire-des-territoires.gouv.fr/visiotheque/2017-dynpop-typologie-de-levolution-de-la-population-entre-1999-et-2013), publiée par l'Observatoire des territoires.
- [Le recensement agricole](https://stats.agriculture.gouv.fr/cartostat/#c=indicator), réalisé par le Ministère de l'Agriculture ;
- [Table d’appartenance géographique des communes et tables de passage](https://www.insee.fr/fr/information/2028028), réalisée par l'Insee.
