###########################################################################################
#                                                                                         #
#  Ce script sert à préparer les jeux de données qui seront utilisés pour les analyses    #
#                                                                                         #
###########################################################################################


#Charger les packages R utiles pour la suite ----------------------

library(dplyr) #permet d'utiliser %>%, filter, summarize et bien d'autres.
library(sf) #pour manipuler des objets spatiaux
library(ggplot2)  #pour les graphiques


#importer les données STOC
mydat = read.csv2("data/0041794-241126133413365.csv", sep = '\t') # note pour Océane: pense à mettre le fichier de données dans le dossier STOC_Bret/data !
mydat = read.csv2("D:/Home/ocbegassat/Documents/Oceane_BEGASSAT/Thesis_guifette/ENS/CPES 2 projet/0041794-241126133413365.csv", sep = '\t') # ligne à enlever


#stocker dans une variable les données concernant uniquement les espèces spécialistes : alouette des champs et bruant jaune
alouette = mydat %>%  filter(species == "Alauda arvensis")
bruantj = mydat %>% filter(species == "Emberiza citrinella")

#idem pour les espèces généralistes tourterelle des bois et mésange charbonnière
tourt = mydat %>% filter(species == "Streptopelia turtur")
charbo = mydat %>% filter(species == "Parus major")

#importer une carte de l'Ille et Vilaine (polygone au format shapefile, donc un objet spatial)
#pour les objets spatiaux utiliser st_read
dep35 <- st_read("data/departement-35/admin-departement.shp")

#convertir les donées STOC qui contiennent déjà des colonnes Longitude et Latitude
#en objet spatial aussi (projeter les données)
alouette = st_as_sf(alouette,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
#le crs est est le "Coordinate Reference System", ici WGS84, système géodésique mondial
plot(st_geometry(alouette)) #représente toutes les observations de l'alouette en France, selon leurs coordonnées


#représenter les observations d'alouettes en Bretagne
ggplot() + 
  geom_sf(data = dep35) + #trace le contour du département 35
  geom_sf(data = alouette) + #ajoute les données d'observation d'alouette 
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) #restreint ce qu'on voit de ces données à une zone en Bretagne

#représenter uniquement les observations en Ille-et-Vilaine
#faire une intersection spatiale : extraire uniquement les points qui se trouvent dans le polygone d'intérêt (ici dpt 35)
alouette_in_dep35 <- st_intersection(alouette, dep35)

ggplot() + 
  geom_sf(data = dep35, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = alouette_in_dep35, color = "blue", size = 1) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOP-eps) d'Alouettes des champs en Ille-et-Vilaine") #ajout d'un titre


# Même chose avec les autres espèces --------------------------------------

#conversion en spatial
bruantj = st_as_sf(bruantj,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
tourt = st_as_sf(tourt,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
charbo = st_as_sf(charbo,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)

ggplot() + 
  geom_sf(data = dep35) + #trace le contour du département 35
  geom_sf(data= charbo, color = "grey")+
  geom_sf(data = alouette, color = "blue") + #ajoute les données d'observation d'alouette 
  geom_sf(data= bruantj, color = "yellow")+
  geom_sf(data= tourt, color = "brown")+
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) #restreint ce qu'on voit de ces données à une zone en Bretagne

bruantj_in_dep35 <- st_intersection(bruantj, dep35)
tourt_in_dep35 <- st_intersection(tourt, dep35)
charbo_in_dep35 <- st_intersection(charbo, dep35)

ggplot() + 
  geom_sf(data = dep35, fill = "white", color = "black") + # Contour du département
  geom_sf(data = charbo_in_dep35, aes(color = "Mésange charbonnière"), size = 1) + # Points pour mésange charbonnière
  geom_sf(data = alouette_in_dep35, aes(color = "Alouette des champs"), size = 1) + # Points pour alouette
  geom_sf(data = bruantj_in_dep35, aes(color = "Bruant jaune"), size = 1) + # Points pour bruant jaune
  geom_sf(data = tourt_in_dep35, aes(color = "Tourterelle des bois"), size = 1) + # Points pour tourterelle
  scale_color_manual(values = c("Mésange charbonnière" = "grey", #pour ajouter une légende manuellement
                                "Alouette des champs" = "blue", 
                                "Bruant jaune" = "yellow", 
                                "Tourterelle des bois" = "brown")) + # Couleurs spécifiques pour chaque espèce
  theme_minimal() + #pour l'esthétique
  ggtitle("Observations des espèces (STOC-EPS) en Ille-et-Vilaine")+
  theme(legend.title = element_blank()) # Enlever le titre de la légende



# Créer un polygone Bretagne --------------------------------------
dep35 <- st_read("data/departement-35/admin-departement.shp")
dep22 <- st_read("data/departement-22/admin-departement.shp")
dep56 <- st_read("data/departement-56/admin-departement.shp")
dep29 <- st_read("data/departement-29/admin-departement.shp")

dep35_22 = st_union(dep35,dep22)
plot(st_geometry(dep35_22))

dep56_29 = st_union(dep56,dep29)
plot(st_geometry(dep56_29))

Bret = st_union(dep35_22,dep56_29)
plot(st_geometry(Bret))


# Extraire observations bretonnes  --------------------------------------


alouette.bret = st_intersection(alouette,Bret)
ggplot() + 
  geom_sf(data = Bret, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = bruantj.bret, color = "blue", size = 1) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOP-eps) d'alouettes des champs en Bretagne") #ajout d'un titre


bruantj.bret = st_intersection(bruantj,Bret)
ggplot() + 
  geom_sf(data = Bret, fill = "white", color = "black") + # Contour du département35
  geom_sf(data = bruantj.bret, color = "blue", size = 1) + # Observations d'alouettes extraites
  theme_minimal() + #juste pour l'esthétique
  ggtitle("Observations (STOP-eps) de Bruants jaunes en Bretagne") #ajout d'un titre






# toutes les espèces en Bretagne

data.bret = mydat %>% 
  st_as_sf(coords = c("decimalLongitude","decimalLatitude"), crs = 4326) %>% 
  st_intersection(Bret)





saveRDS(data.bret, "data/data.bret.RDS")
write.csv2(data.bret, "data/data.bret.csv")

saveRDS(alouette.bret, "data/alouette.bret.RDS")
write.csv2(alouette.bret, "data/alouette.bret.csv")

saveRDS(bruantj.bret, "data/bruantj.bret.RDS")
write.csv2(bruantj.bret, "data/bruantj.bret.csv")

# annoter les nouvelles lignes
# rajouter la carte interactive



