
# Comparaison : Filtre de Bloom vs Recherche Dichotomique

## Objectif
Comparer deux méthodes de recherche de mots :
- **Filtre de Bloom** : rapide mais approximatif (peut donner de faux positifs)
- **Recherche dichotomique** : plus lente mais toujours exacte


## Méthodologie

1. Chargement du dictionnaire (`dictionary.txt`, ~99 912 mots)
2. Construction :
   - d’un **filtre de Bloom** (1% de faux positifs)
   - d’un **tableau trié** pour la recherche binaire
3. Test de 6 mots (présents & absents)
4. Mesure :
   - de la **détection de présence**
   - du **temps d’exécution** pour chaque méthode


##  Résultats expérimentaux

| Mot       | Bloom  | Temps Bloom    | Dichotomique  | Temps Dichotomique   |
|-----------|--------|----------------|---------------|----------------------|
| tiger     | true   | 0.000006s      | true          | 0.000007s            |
| banana    | true   | 0.000004s      | true          | 0.000005s            |
| fantôme   | false  | 0.000002s      | false         | 0.000005s            |
| asdfgh    | false  | 0.000002s      | false         | 0.000005s            |
| zebre     | false  | 0.000002s      | false         | 0.000005s            |
| dragon    | true   | 0.000004s      | true          | 0.000003s            |

- **Remarque** : `dragon` est un **faux positif** du filtre de Bloom (le mot n’est pas dans le dictionnaire).


## Faux positifs mesurés

- Nombre de mots aléatoires testés : **500**
- Faux positifs détectés : **~2**
- **Taux réel mesuré ≈ 0.40%**


## Analyse comparative

| Critère                  | Filtre de Bloom                 | Recherche Dichotomique       |
|--------------------------|---------------------------------|------------------------------|
| **Temps de recherche**   |  Ultra rapide                   |  Un peu plus lent            |
| **Exactitude**           |  Faux positifs possibles        |  Toujours exact              |
| **Faux négatifs**        |  Aucun                          |  Aucun                       |
| **Espace mémoire**       |  Compact (utilise des bits)     |  Tableau complet en mémoire  |
| **Utilisation idéale**   | Vérification rapide de présence |  Recherches fiables et sûres |


## Conclusion

Le **filtre de Bloom** est idéal pour des cas où :
- La **rapidité** prime sur la précision absolue
- Un **faux positif** est tolérable (ex : cache, dédoublonnage)

La **recherche dichotomique** reste le choix :
- Pour des **résultats toujours exacts**
- Quand les **faux positifs sont critiques**