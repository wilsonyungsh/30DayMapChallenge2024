pacman::p_load(strayr,sf,tidyverse,patchwork)

# load aus sf and simplfy a bit
aus<-read_absmap(name = "aus2021") %>% filter(aus_code_2021 == "AUS") %>% st_simplify(dTolerance = 0.1)


comparison<-
list("EPSG:3112,Geoscience Australia Lambert" = 3112,
     "EPSG4326, World Geodetic System 1984" = 4326 ) %>% 
  map(~ st_transform(aus,.x)) 

p1 <- ggplot() +
  geom_sf(data = comparison$`EPSG:3112,Geoscience Australia Lambert`, col = "blue", alpha = 0.1, lwd = 0.5) +
  labs(title = names(comparison)[1])

p2<-ggplot() +
  geom_sf(data = comparison$`EPSG4326, World Geodetic System 1984`, col = "red", alpha = 0.1, lwd = 0.5) +
  labs(title = names(comparison)[2])

p1/p2

