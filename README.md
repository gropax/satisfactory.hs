# Questions
- À quoi sert la quantification `forall` ?
- Comment le _builder pattern_ se traduit-il en Haskell ? Via la monade `State` ?
- Quel est le pattern utilisé qui décompose la compilation en deux parties : `compileWith` et `compileClosed` ?
- 

# Notes
- Kinds : descripteurs/types de types
    + `Symbol` : chaînes au niveau des types
    + `Nat` : entiers au niveau des types
    + `Char` : caractères au niveau des types
    * Vivent pendant la compilation, et ne font pas partie du programme compilé
    * L'introspection via `KnownSymbol` et `Proxy` à lieu une fois pendant la compilation
- _Type reflection classes_ : permettent de refléter un littéral de type en valeur runtime
    + `KnownSymbol`
    + `KnownNat`
    + `KnownChar`
- Réification d'un type
    + Création d'une valeur vide du type donné via `Proxy` pour le manipuler en runtime.
- Promotion d'un type
    + Conversion d'un constructeur de données en constructeur de types

# Haskell
## Concepts
- Différence entre `type`, `newtype` et `data`
- _Type-level literal reflection_
- `DataKinds` : tout type algébrique est automatiquement promu en _kind_, et ses constructeurs
deviennent des constructeurs de types.
- _Type family_ : fonction au niveau des types. Permet de calculer/réécrire des types en d'autres types pendant le _typechecking_.

## Compétences
- Construire des algèbres de types
    + Maîtriser l'extension `DataKinds` et la promotion des types.
    + Construire des fonctions sur les types avec `type family`.
    + Maîtriser la réflexion et la réification des types (`KnownSymbol`)
    + Maîtriser l'existentialisation des valeurs (`SomeSymbol`)
- Paradigme fonctionnel
    + L'ID ne doit pas être dans les modèles (e.g. `Node` ne connais pas `NodeId`), contrairement au paradigme DB ou Web.
        * Dans un CRUD, l'identité est **primaire** (métier), l'objet est une entité.
        * Dans mes graphes, l'ID est un artéfact d'indexation
        - -> Construire une note **Faut-il mettre l'Id dans la structure de données ?**

# Pistes
- https://chatgpt.com/s/t_69a0b30dcb5c81918a2c653f097e4396
    + Monoïde vs comonoïde
    + bialgèbre
    + algèbre de Hopf
    + algèbre de flux (linéaire)
    + structure/loi de Frobenius
    + 

# Algorithmes
- _Union-find_
- _Hash consing_ / _Merkle hashing_

# TODO
- [ ] Typer les noeuds par un AST (`Recipe`, `Junction`, `Source`, `Sink`)
- [ ] Utiliser des `Text` au lieu des `Symbol` dans les graphes
- [ ] Ajouter l'opérateur de `Trace`
