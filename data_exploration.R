###########################################################################################
#                                                                                         #
#  Ce script sert à faire des figures pour explorer les données                           #
#                                                                                         #
###########################################################################################



library(dplyr) #permet d'utiliser %>%, filter, summarize et bien d'autres.
library(sf) #pour manipuler des objets spatiaux
library(ggplot2)  #pour les graphiques

alouette.bret = readRDS("data/alouette.bret.RDS")

bruantj.bret = readRDS( "data/bruantj.bret.RDS")

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
