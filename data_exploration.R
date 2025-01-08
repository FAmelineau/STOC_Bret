###########################################################################################
#                                                                                         #
#  Ce script sert à faire des figures pour explorer les données                           #
#                                                                                         #
###########################################################################################



library(dplyr) #permet d'utiliser %>%, filter, summarize et bien d'autres.
library(sf) #pour manipuler des objets spatiaux
library(ggplot2)  #pour les graphiques

#ré-importer les jeux de données enregistrés
alouette.bret = readRDS("data/alouette.bret.RDS")
bruantj.bret = readRDS( "data/bruantj.bret.RDS")



# Nombre d'observations et d'observateurs d'Alouettes ---------------------

#Somme des observateurs et observations d'alouettes par an et ratio de l'un sur l'autre
sum.alouette = alouette.bret %>% 
  st_drop_geometry() %>% #retire la partie spatial epour faciliter le traitement des données numériques
  group_by(year) %>%  #groupe par année (première colone = année)
  summarize(n.observations = n(), n.observateurs = n_distinct(recordedBy) ) %>% #crée deux nouvelles colonnes
#nombre d'observations = nombre de ligne alouette par an
#nombre d'observateurs = nombre de noms différents ayant fait ces observations d'alouettes par an
  mutate(ratio = n.observations/n.observateurs) #nouvelle colonne qui résume le ratio des deux

# représenter ce ratio selon les années
ggplot(sum.alouette) +
  geom_point(aes(x=year, y = ratio))



# Même chose pour le bruant jaune -----------------------------------------

sum.bruantj = bruantj.bret %>% 
  st_drop_geometry() %>% 
  group_by(year) %>% 
  summarize(n.observations = n(), n.observateurs = n_distinct(recordedBy) ) %>% 
  mutate(ratio = n.observations/n.observateurs)

ggplot(sum.bruantj) +
  geom_point(aes(x=year, y = ratio))
