# A script to list all the packages used in this project
# and install them as needed in a dedicated environment

# Run this script from your working directory

# For historical reference only:
# made obsolete by the renv framework

if(!"packrat" %in% rownames(installed.packages())) {
  install.packages("packrat")
}

library(packrat)

# Init project-specific package library
packrat::init()
library(stringr)

libs = lapply(list.files(pattern = "\\.Rmd$", recursive=T),
               function(x){
                 d = readLines(x)
                 d = d[grepl("^ *library\\(.*\\)", d)]
                 d = gsub("library\\((.*)\\)", "\\1", d)
                 gsub("^\\s+|\\s+$", "", d)
               })
libs = unique(unlist(libs))

list.of.packages <- list()

for (l in libs) {
  is.available <- require(l, character.only = TRUE)
  if(!is.available) {
    install.packages(l)
    list.of.packages <- append(list.of.packages, l)
  }
}

# The list of newly installed packages
lapply(list.of.packages, write, "installed_packages.txt",
       append=TRUE)

# The list of required packages
lapply(libs, write, "requirements.txt", append=TRUE)
