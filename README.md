# Documentation du paquet `rcoleo` <img src="man/figures/rcoleo.svg" width="130" height="150" align="right"/>

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

`rcoleo` un paquet R permettant le retrait et l'analyse des données collectées dans le cadre du programme de suivi de la biodiversité (BdQc).

Ce paquet R expose les services `RESTFull` de l'API de Coléo. Colée est un système d'information sur la biodiversité du Québec développé par le laboratoire d'Écologie Intégrative de l'Université de Sherbrooke.

[Pour en savoir davantage...](https://coleo.biodiversite-quebec.ca/docs/)


## Installer le paquet `rcoleo`

```r
devtools::install_github("TheoreticalEcosystemEcology/rcoleo")
```

## Mettre en cache votre jeton d'accès

Pour obtenir votre jeton d'accès, veuillez vous authentifier sur le portail coleo, votre jeton d'accès sera affiché dans la section profile [http://coleo.quebec-biodiversite.ca/portail](http://coleo.quebec-biodiversite.ca/portail)

Il est fortement recommandé de mettre en cache se jeton d'accès afin de s'assurer qu'il ne soit pas visible ou transmis avec votre code à un autre utilisateur. Ce jeton d'accès est unique et révocable.

```r
bearer <- "7f8df438e1be96a18436e9dab5d97d68ed0e0441d9b68f59e0ce631b2919f3aa"
```

### TODO: Enhancements

- [ ] find the best stratey to manage access token
- [ ] Setup appVeyor (windows) et travis (linux)
- [ ] Codecov? sur serveur test?

### Features requested - roadmap

- [ ] List campaigns from site
- [ ] Map sites (filter date and campaigns)
- [ ] Get obs from a campaigns
- [ ] List of obs (with and w/o external datasets)
- [ ] Get medias from a campaigns
- [ ] Extract clim for sites
- [ ] List of species (add all ids)
- [ ] Compute indicators (start with alpha-com)
- [ ] Export shapefiles
- [ ] Export DwC-A

External dataset: second layer/service with elastic search?

*Le jeton d'accès est un exemple ici et n'est aucunement valide.*
