# Lecture du fichier requirements.txt
requirements <- readLines("requirements_R.txt")

# Installation des packages
for (package in requirements) {
  install.packages(package)
}

library(aws.s3)
library(dplyr)
library(readr)

bucket <- "projet-funathon"
path_data <- "2023/sujet3/diffusion"

description_indiv <- s3read_using(read_delim, object = paste(path_data, "description-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
habitudes_indiv <- s3read_using(read_delim, object = paste(path_data, "habitudes-indiv.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
actphys_sedent <- s3read_using(read_delim, object = paste(path_data, "actphys-sedent.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)
fpq <- s3read_using(read_delim, object = paste(path_data, "fpq.csv", sep="/"), bucket = bucket, opts = list('region'=''), show_col_types = FALSE)

library(ggplot2)
library(ggcorrplot)
library(sf)

options(dplyr.width = Inf)
options(repr.plot.width=20, repr.plot.height=10)

# Histogramme des IMC

ggplot(data=description_indiv,aes(x=imc))+
  geom_histogram(binwidth=1,color="grey",fill="lightblue")

