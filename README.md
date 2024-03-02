# BuildingsEmissionsCalculator

Le package R `BuildingsEmissionsCalculator` permet de calculer les émissions de CO2 d'une liste de batiments. A partir d'un tableau comptenant une liste d'EGID ou d'adresses de batiments, il ira chercher les informations manquantes sur des bases de données, comme celle de RegBl, puis utilisera le calculateur [`co2calculatorPACTA2022`](https://maximecharriere.github.io/co2calculatorPACTA2022/), suivant la norme SIA 380/1:2009 "Thermische Energie im Hochbau", afin de calculer les émissions de chaque batiment. 

## Installation & Utilisation

Pour des explication sur l'installation et l'utilisation de `BuildingsEmissionsCalculator`, allez voir la page [`Getting Started`](articles/BuildingsEmissionsCalculator.html).


## Ressources

- Doc about the `co2calculatorPACTA2022` package :  
  https://maximecharriere.github.io/co2calculatorPACTA2022/
- To see what is included in the RegBl database, and understand the RegBl abbreviations, refer to the document `Catalogue des caractères - Version 4.2` :  
  https://www.bfs.admin.ch/asset/fr/22905271
- To understand the codes written in the Excel file, refer to the document `Codes du RegBL` :  
  https://www.housing-stat.ch/files/GWRCodes.xlsx
- For information on the data access, using the RegBl MADD API, refer to the `eCH-0206` norm :  
  https://www.housing-stat.ch/fr/madd/restricted/ech-0206.html
- To found the XPath corresponding to a given RegBl abbreviation, take a look at `Annexe H` of the `eCH-0206 – Données RegBL à des tiers` document :  
  https://www.housing-stat.ch/files/STAN_f_DEF_2022-06-18_eCH-0206_V2.0.0_Donnees_RegBL_aux_tiers.pdf

## Coding style

The Tidyverse style is used  -> [style.tidyverse.org](https://style.tidyverse.org/).

