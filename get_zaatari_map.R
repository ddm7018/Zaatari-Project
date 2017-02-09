library(OpenStreetMap)
library(rgdal)

lat     <- c(32.3143,32.2630)
lon     <- c(36.2989,36.3597)
zaatari <- openmap(c(lat[1],lon[1]),c(lat[2],lon[2]),zoom=15,'osm')

plot(zaatari)