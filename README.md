# TIPE : détection d'erreurs grammaticales en français  
  
## Structure 
   
syntactic_analyzer/  
├── data/  
│   ├── le_dm.csv                # Fichier de données : dictionnaire morphosyntaxique (Le DM de François Trouilleux)  
│   ├── le_dm_extrait.csv        # Et version abrégée (pour les tests)  
│   └── index.msh                # Fichier Marshal généré par index_builder.ml  
├── bin/  
│   └── main.ml                  # Programme principal (exécution du projet)  
├── lib/  
│   ├── correcteur.ml            # Module principal  
│   ├── types.ml                 # Déclaration de types  
│   ├── index_builder.ml         # Création, export et chargement du DM en Marshal (vers index.msh)  
│   ├── lexer.ml                 # Analyse lexicale  
│   └── parser.ml                # Analyse syntaxique (parsing)  
├── _build/                      # Répertoire de build généré par Dune  
└── dune-project                 # Fichier principal dune  