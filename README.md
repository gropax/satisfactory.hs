# Questions
- Quel rapport entre `Symbol` et `KnownSymbol` ? Peut-on créer soi-même ce genre de « couple » ?
- Qu'est-ce que `GHC.TypeLits` ?
- Comment le _builder pattern_ se traduit-il en Haskell ? Via la monade `State` ?
- Qu'est-ce que `Type` ? Qu'est-ce qu'un `Kind` ?
- À quoi sert la quantification `forall` ?

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
