# Geospatial Analytics Lab 

The present repository is my personal geospatial and geostatistical analytics lab, the idea is to use open source data, public sources and standard mapping tools and apply machine learning algorithms and analytics to uncover new information from human activity, nature and market dynamics around the globe, while learning the complex spatial data management and improving machine learning coding and design. 

The documentation site ("digital book") is built with Quarto and
published via GitHub Pages.

 **Documentation:** 
`https://edgar-daniel-codes.github.io/geospatial-analytics-lab/`

The most important functions and/or algorithms could be duplicated both in global `lib` directory and the project `<project-name>/src`, this in order to make each project self-contain while reusing the geenrated code in each project. 

## Relvant projects

Some of the most useful and insighfult (completetly personal biased) projects inside the repo:

### Isochrone Calculation 

R repository with functions 

### 


## Structure

[PENDING]

- `docs/` — Quarto website (the main "book" for this lab).
- `projects/` — Individual projects (isochrones, population density, etc.).
- `src/` — Shared Python utilities for geospatial work.
- `.github/workflows/` — CI pipeline that builds and publishes the site.



## Local Usage

For a local implementation of this book, first install Quarto from its (oficial site)[https://quarto.org]


```bash
# Starting at repo root
cd docs
quarto preview # live preview of the site
quarto render # build the static site into docs/_site
```


