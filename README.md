# rcoleo

`rcoleo` un paquet R permettant le retrait des données collectées dans le cadre du programme de suivi de la biodiversité. Ce programme a été mis sur pied par le Ministère des Forêts, de la Faune et des Parcs (2015).

Ce paquet R expose les services `RESTFull` de l'API de Coléo. Colée est un système d'information sur la biodiversité du Québec développé en partenariat avec le laboratoire d'Écologie Intégrative de l'Université de Sherbrooke.

[Pour en savoir davantage...](https://synapse.vhost33.genap.ca/docs/)


## Installer le paquet rcoleo

```r
devtools::install_github("TheoreticalEcosystemEcology/rcoleo")
```

## Mettre en cache votre jeton d'accès

Pour obtenir votre jeton d'accès, veuillez vous authentifier sur le portail coleo, votre jeton d'accès sera affiché dans la section profile [http://www.quebec-biodiversité.ca/coleo/portal](http://www.quebec-biodiversité.ca/coleo/portail)

Il est fortement recommandé de mettre en cache se jeton d'accès afin de s'assurer qu'il ne soit pas visible ou transmis avec votre code à un autre utilisateur. Ce jeton d'accès est unique et révocable.

```r
bearer <- "7f8df438e1be96a18436e9dab5d97d68ed0e0441d9b68f59e0ce631b2919f3aa"
```

TODO: find the best stratey to manage access token

*Le jeton d'accès est un exemple ici et n'est aucunement valide.*
