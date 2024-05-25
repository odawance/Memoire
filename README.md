# Dépôt du mémoire

__Auteur__ : DAWANCE Océane (UNamur)

__Sujet du mémoire__ : Étude de la vitesse de convergence de la méthode de prochaine génération

## Structure du dépôt

### Documents_PDF_annexes

Ce dossier contient les résultats graphiques obtenus au cours des analyses.
Chaque fichier PDF est nommé de manière descriptive pour indiquer le contenu des graphiques qu'il contient :
* *3* signifie que les graphiques concernent les classes d'âge [0, 20[, [20, 60[ et 60+,
* *10* signifie que les graphiques concernent les classes d'âge [0, 6[, [6, 12[, [12, 18[, [18, 30[, [30, 40[, [40, 50[, [50, 60[,
[60, 70[, [70, 80[ et 80+,
* *AH* signifie que nous tenons compte de l'hétérogénéité pour la susceptibilité et l’infectiosité.

### Fichiers_RDS

Dans ce dossier, vous trouverez les sorties de certaines fonctions sous forme de fichiers RDS.
Chaque fichier est nommé de manière à refléter son contenu (excepté out_select01 qui représente les résultats sélectionnés en sortie du MCMC).

### R

Ce dossier contient plusieurs éléments essentiels pour la reproduction des résultats :
* __Données sur les hospitalisations__ : le fichier *covid-hospitalizations.csv* contient les données brutes concernant les hospitalisations.
* __Fonctions R personnelles__ :
    * *contact_matrices.R* : générer et représenter graphiquement les matrices des contacts sociaux.
    * *convergence.R* (chapitres 3 et 4 du mémoire) :
        * calculer et représenter graphiquement les taux d'amortissement,
        * représenter graphiquement la convergence de l'incidence relative et calculer quand les seuils sont atteints,
        * représenter graphiquement les équilibres et calculer la distance qui les sépare.
    * *modele.R* (chapitre 5 du mémoire) :
        * modèle SIHR à trois compartiments, calibré sur des données COVID-19 belges de 2020,
        * représenter graphiquement la convergence de la prévalence (relative) et calculer quand les seuils sont atteints.
* __SOCRATES__ : le dossier *socrates_rshiny-master* contient l'outil SOCRATES auxquels les nouveaux programmes font appel.