library(dplyr)
library(sf)
library(ggplot2)



mydat = read.csv2("data/0041794-241126133413365.csv", sep = '\t')


alouette = mydat %>%  filter(species == "Alauda arvensis")
bruantj = mydat %>% filter(species == "Emberiza citrinella")


tourt = mydat %>% filter(species == "Streptopelia turtur")
charbo = mydat %>% filter(species == "Parus major")


dep35 <- st_read("data/departement-35/admin-departement.shp")


alouette = st_as_sf(alouette,coords = c("decimalLongitude","decimalLatitude"), crs = 4326)
plot(alouette)



ggplot() + 
  geom_sf(data = dep35) +
  geom_sf(data = alouette) +
  coord_sf(xlim = c(-6,0), ylim = c(46,49.5), expand = FALSE) 
