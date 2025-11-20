# Geospatial Analytics Lab 

The current repository is my personal geospatial and geo-statistical analytics laboratory. The idea is to use open-source data and public sources, as well as standard mapping tools, to apply machine learning algorithms and analytics. This will allow us to uncover new information about human activity, natural phenomena, and market dynamics around the world. At the same time, we will learn about complex spatial data management and improve our machine learning coding and design skills.


 **Documentation:** 
`https://edgar-daniel-codes.github.io/geospatial-analytics-lab/`

The most important functions and/or algorithms could be duplicated in both the global `lib` directory and the project `<project-name>/src`, directory. This would make each project self-contained while enabling the reuse of generated code. 

## Relevant projects

Some of the most useful and insightful (completely personal and biased) projects in the repository are listed below.

### Isochrone Calculation 
Compute travel-time catchment areas from an origin over a street network.

### Population Density Analysis
An analysis of how the population is distributed across urban communities in Mexico.

## Structure

├── `data` - Folder with all data inputs and outputs.
│   ├── `clean` - Cleaned databases. 
│   ├── `inference` - Data generated as a result of a statistical model or algorithm. 
│   ├── `processed` - Processed data, data engineered variables.
│   ├── `raw` - original data sources. 
│   └── `sample` - Data samples for making the public repo functions work and showcase. 
├── `docs` — Quarto website (the main "book" for this lab).
├── `examples` - Code extracts and figures for very particular examples.
├── `lib` — Source code for all individual projects in the same folder. 
├── `projects` — Individual projects (isochrones, population density, etc.).
├── `README.md` — README file for the whole project.
├── `requirements.txt` - All libraries and required dependencies.
└── `tests` - Non concluded experiments, functions and notebooks.


## Local Usage

For a local implementation of this book, first install Quarto from its (official site)[https://quarto.org]


```bash
# Starting at repo root
cd docs
quarto preview # live preview of the site
quarto render # build the static site into docs/_site
```


