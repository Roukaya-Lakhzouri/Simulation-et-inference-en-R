# Projet Statistique I – Simulation, Tests et Analyse de Données en R

Projet réalisé dans le cadre du module de Statistiques I (Période 1) en 2e année du parcours MINDS (ENIT). Ce projet met en œuvre des simulations probabilistes, des tests statistiques et des analyses comparatives en **R** à partir de plusieurs situations réelles ou modélisées.

## 📁 Contenu

Le dépôt contient :

- `rapport_projet.pdf` : rapport complet avec code, graphiques et analyses
- Scripts R  : `exercice 1.R`, `exercice 2.R`, `exercice 3.R`

---

## 📌 Exercice 1 – Simulation autour d’un dé équilibré

Ce premier exercice modélise des expériences aléatoires basées sur le lancer d’un dé :

- Simulation d’échantillons aléatoires suivant la loi d’un dé à 6 faces
- Calculs de moyennes empiriques et visualisation des lois issues de ces moyennes (`Dₙ`)
- Observation de la convergence vers une loi normale (théorème central limite)
- Implémentation d’un jeu de dés récursif avec relance sur 6, simulation de scores, et estimation par fréquence et espérance

**Compétences mises en œuvre** : simulation Monte Carlo, histogrammes, fonctions R, espérance empirique, approximation gaussienne

---

## 📌 Exercice 2 – Test d’hypothèse sur des scores cognitifs

Un test paramétrique est appliqué pour évaluer l’impact d’un programme d’enrichissement cognitif :

- Comparaison des moyennes avant et après traitement
- Application d’un test t de Student pour données appariées
- Décision au seuil de 5% quant à l'amélioration significative des scores

**Compétences mises en œuvre** : test de Student pour données appariées, hypothèses nulles/alternatives, prise de décision statistique

---

## 📌 Exercice 3 – Analyse de la longueur des œufs selon la taille des nids

Données biologiques issues d’un article scientifique :

- Vérification de la normalité des distributions (tests de Shapiro-Wilk)
- Test de l’égalité des variances entre deux groupes (test de Fisher)
- Comparaison des moyennes pour valider une hypothèse biologique

**Objectif** : déterminer si la taille des œufs varie selon la taille des nids.

**Compétences mises en œuvre** : tests de normalité, test F d’égalité des variances, test t pour échantillons indépendants

---

## 🔧 Outils et langages

- **R** : langage principal utilisé
- Packages : `ggplot2`, `stats`, `dplyr`, etc.
- Analyse interactive via scripts ou rapport compilé en PDF

---

## 🎯 Objectifs pédagogiques

- Maîtriser la simulation de lois de probabilité
- Appliquer des tests paramétriques en contexte réel
- Interpréter des résultats statistiques et valider des hypothèses

---

## 📄 Rapport

Le rapport complet se trouve dans le fichier :  
📄 `rapport_projet.pdf`



