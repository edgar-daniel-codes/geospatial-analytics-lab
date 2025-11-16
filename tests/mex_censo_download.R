# mex_censo_download.R

## Geospatial Analytics Lab
## Isochrone Calculator
## By Edgar Daniel

# This scripts allows you to download entity level data for Mexico's INEGI 2020 
# CENSUS at a MZA (Manzana Urbana) level by entity. 
# 

# Parameters 
OUT_DIR <- './data/raw/mex_censo_2020/'

# Create the out directory 
dir.create(OUT_DIR)

# Create the custom PATHS for entity level census data
paths <- sprintf("https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/ageb_manzana/RESAGEBURB_%02s_2020_csv.zip"
                 , 1:32)

# Download entity zip files
for(p in paths){
  filename <- gsub("https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/ageb_manzana/","",p)
  download.file(url = p, 
                destfile = paste0(OUT_DIR,filename),
                mode = "wb",
                quiet = FALSE)
}

# Unzip entity level files 
for(f in list.files(OUT_DIR)){
  dir_out <- gsub(".zip","",f)
  dir.create(paste0(OUT_DIR, dir_out), showWarnings=FALSE, recursive=TRUE)
  unzip(paste0(OUT_DIR, f), exdir=paste0(OUT_DIR, dir_out), overwrite=TRUE)
  file.remove(paste0(OUT_DIR, f))
}


