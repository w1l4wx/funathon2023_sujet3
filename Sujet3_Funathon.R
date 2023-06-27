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

#Cartographie

url <- "https://minio.lab.sspcloud.fr/projet-cartiflette/diffusion/shapefiles-test1/year%3D2022/administrative_level%3DREGION/crs%3D4326/FRANCE_ENTIERE%3Dmetropole/vectorfile_format%3D%27geojson%27/provider%3D%27IGN%27/source%3D%27EXPRESS-COG-CARTO-TERRITOIRE%27/raw.geojson"

region <- sf::st_read(url)

# Passons le fonds de carte dans le système de coordonnées de référence utilisé pour la FRance, Lambert 93 (code : 2154) au lieu de WGS 84
region <- region %>% st_transform(2154)

# Représentons les contours de notre fond de carte
plot(st_geometry(region))



#fusion des deux tables sur les individus
ponderations <- description_indiv %>%
                select (NOIND,pond_indiv_adu_pop2, pond_indiv_enf_pop2)
desc_ind <- description_indiv %>%
            select(NOIND,sex_PS,tage_PS, region_adm_12cl, agglo_5cl)

habitudes <- habitudes_indiv %>%
  left_join(ponderations,by="NOIND") %>%
  left_join(desc_ind,by="NOIND")


#recodage de la région
habitudes <- habitudes %>% mutate(region_recode=case_when(region_adm_12cl==1 ~ "ILE-DE-FRANCE",
                                                          region_adm_12cl==2 ~ "NORMANDIE",
                                                          region_adm_12cl==3 ~ "CENTRE-VAL DE LOIRE",
                                                          region_adm_12cl==4 ~ "PAYS DE LA LOIRE",
                                                          region_adm_12cl==5 ~ "BRETAGNE",
                                                          region_adm_12cl==6 ~ "HAUTS-DE-FRANCE",
                                                          region_adm_12cl==7 ~ "GRAND EST",
                                                          region_adm_12cl==8 ~ "BOURGOGNE-FRANCHE-COMTE",
                                                          region_adm_12cl==9 ~ "AUVERGNE-RHONE-ALPES",
                                                          region_adm_12cl==10 ~ "PROVENCE-ALPES-COTE D'AZUR",
                                                          region_adm_12cl==11 ~ "OCCITANIE",
                                                          region_adm_12cl==12 ~ "NOUVELLE-AQUITAINE",))

# Variable à représenter géographiquement : biere par région
#biere_par_region <- description_x_fpq %>% group_by(region_recode) %>% summarise(freq_conso_biere_moyenne=mean(BA_biere_freq_M,na.rm=TRUE))
#biere_par_region

#on veut voir dans quelle région le beurre est plus présent sur la table
part_beurre <- habitudes %>% select(NOIND, region_recode, table_beurre) 

tot_beurre <-part_beurre %>% summarise(tot_beurre = sum(table_beurre, na.rm = TRUE))

reg_beurre <-   part_beurre %>% 
                group_by(region_recode) %>% 
                summarise(sommebeurre = sum(table_beurre, na.rm = TRUE)) %>% mutate(part=sommebeurre/772)

region_inca <- left_join(region,reg_beurre,by=c("NOM_M"="region_recode"))

ggplot(data=region_inca) + geom_sf(aes())


