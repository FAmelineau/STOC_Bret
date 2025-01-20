###########################################################################################
#                                                                                         #
#  Ce script sert à faire des figures pour explorer les données                           #
#                                                                                         #
###########################################################################################



library(dplyr) #permet d'utiliser %>%, filter, summarize et bien d'autres.
library(sf) #pour manipuler des objets spatiaux
library(ggplot2)  #pour les graphiques

# ici mettre votre chemin d'accès jusqu'au dossier où se trouvent les fichiers .RDS 
# (penser à changer les \ en / )

mypath = "C:/Users/fameline/OneDrive - ENS RENNES/Documents/CPES/L2 24-25/suivi oiseaux communs/STOC_Bret/data"

alouette.bret = readRDS(paste0(mypath,"/alouette.bret.RDS"))

bruantj.bret = readRDS( paste0(mypath,"/bruantj.bret.RDS"))

sum.alouette = alouette.bret %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(n.observations = n(), n.observateurs = n_distinct(recordedBy) ) %>% 
  mutate(ratio = n.observations/n.observateurs)

ggplot(sum.alouette) +
  geom_point(aes(x=year, y = ratio))




sum.bruantj = bruantj.bret %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(n.observations = n(), n.observateurs = n_distinct(recordedBy) ) %>% 
  mutate(ratio = n.observations/n.observateurs)



ggplot(sum.bruantj) +
  geom_point(aes(x=year, y = ratio))
