# Geospatial Analytics Lab 

The current repository is my personal geospatial and geostatistical analytics laboratory. The idea is to use open-source data and public sources, as well as standard mapping tools, to apply machine learning algorithms and analytics. This will allow us to uncover new information about human activity, natural phenomena, and market dynamics around the world. At the same time, we will learn about complex spatial data management and improve our machine learning coding and design skills.


 **Documentation:** 
`https://edgar-daniel-codes.github.io/geospatial-analytics-lab/`

The most important functions and/or algorithms could be duplicated in both the global `lib` directory and the project `<project-name>/src`, directory. This would make each project self-contained while enabling the reuse of generated code. 

## Relevant projects

Some of the most useful and insightful (completely personal and biased) projects in the repository are listed below.

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


