# Filtre de Bloom en OCaml

Ce projet implémente un **filtre de Bloom** en OCaml — une structure de données probabiliste utilisée pour tester l’appartenance à un ensemble, avec une faible probabilité de faux positifs.


## Structure du projet

- `bloom.ml` : Code source du filtre de Bloom
- `dictionary.txt` : Fichier contenant un dictionnaire de mots
- `comparaison.md` : Comparaison détaillée entre filtre de Bloom et recherche binaire
- `README.md` : Ce fichier


## Fonctionnalités

- Création d’un filtre de Bloom optimal (`m`, `k` calculés automatiquement)
- Ajout d’éléments au filtre
- Vérification de présence (avec une certaine probabilité de faux positifs)
- Comparaison de performances avec une recherche dichotomique classique
- Mesure du taux réel de faux positifs

## Tests réalisés

1. **Éléments ajoutés** : vérifie que les mots ajoutés sont détectés
2. **Éléments absents** : vérifie que les mots non ajoutés sont généralement rejetés
3. **Chargement du dictionnaire** (`dictionary.txt`)
4. **Comparaison avec la recherche binaire**
5. **Mesure du taux de faux positifs sur mots aléatoires**

## Exemple de sortie

\=== TEST 1 : Éléments ajoutés ===
"chat" présent ? true (attendu: true)
...

\=== TEST 4 : Comparaison Bloom vs Binaire ===

| Mot   | Bloom | Temps Bloom | Binaire | Temps Binaire |
| ----- | ----- | ----------- | ------- | ------------- |
| tiger | true  | 0.000010s   | true    | 0.000007s     |
| ...   |       |             |         |               |

\=== TEST 5 : Faux positifs mesurés ===

* Nombre de mots testés : 500
* Faux positifs détectés : 2
* Taux mesuré : 0.40%


## Compilation & Exécution

### Avec OCaml et dune :
```sh
ocamlopt -o bloom bloom.ml
./bloom
```

## Auteurs

* Projet réalisé dans le cadre du module d’algorithmique avancée
* Encadrant : *\[Mouhamadou GAYE]*
* Étudiant : *\[El Hadji Abdou DRAME]*


## Références

* [Wikipedia - Bloom Filter](https://en.wikipedia.org/wiki/Bloom_filter)
* [OCaml Documentation](https://ocaml.org/)