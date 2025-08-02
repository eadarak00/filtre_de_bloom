# Comparaison : Filtre de Bloom vs Recherche Dichotomique

## Objectif
Comparer les performances et la précision entre deux méthodes de recherche de mots :
- **Filtre de Bloom** (probabiliste, rapide, mais possibilité de faux positifs)
- **Recherche dichotomique** (précise mais plus lente pour certains cas)

## Méthodologie
1. Chargement de `dictionary.txt` (~99 912 mots)
2. Insertion de tous les mots dans :
   - un **filtre de Bloom**
   - un **tableau trié** pour la recherche dichotomique
3. Tests sur 6 mots mixtes (présents et absents)
4. Mesure du **temps d’exécution** et de la **présence** détectée
5. Génération d’un tableau de comparaison

## Résultats des tests

| Mot        | Bloom | Temps Bloom | Binaire | Temps Binaire |
|------------|--------|--------------|---------|----------------|
| tiger      | true   | 0.000010s     | true    | 0.000007s      |
| banana     | true   | 0.000006s     | true    | 0.000007s      |
| fantôme    | false  | 0.000002s     | false   | 0.000007s      |
| asdfgh     | false  | 0.000003s     | false   | 0.000006s      |
| zebre      | false  | 0.000002s     | false   | 0.000006s      |
| dragon     | true   | 0.000006s     | true    | 0.000005s      |


## Analyse

-  **Filtre de Bloom** : Très rapide (temps < 10 µs), mais 1 faux positif (`dragon`)
-  **Recherche binaire** : Très fiable, mais légèrement plus lente
-  Le **taux de faux positifs mesuré** sur 500 mots aléatoires inexistants :  
  - Faux positifs : 2  
  - **Taux réel ≈ 0.40%**


## Conclusion

| Critère                | Filtre de Bloom               | Recherche Dichotomique  |
|------------------------|-------------------------------|-------------------------|
| Temps de recherche     | Très rapide                   |  Plus lent              |
| Exactitude             | Faux positifs possibles       |  100% fiable            |
| Espace mémoire         | Optimisé (bits)               |  Tableau complet        |
| Idéal pour             | Présence approximative rapide |  Recherche exacte       |

