library(sp)
library(ggplot2)
library(ggmap)
library(sf)
library(tmap)
library(sp)
library(spdep)
library(dplyr)
library(spData)
library(hexbin)
library(readr)
library(ggspatial)
library(leaflet)

for (x in libs){                                                #cycle through each item in libs object
  if(x %in% rownames(installed.packages()) == FALSE) {          #if the package is not installed
    print(paste0("installing ", x, "..."))                      #print a message to tell me
    install.packages(x)                                         #and then install the packages
  }
  else{                                                         #otherwise (if it is installed)
    print (paste0(x, " is already installed "))                 #print a message to tell me
  }
  library(x, character.only = TRUE)                             #and then load the packages
}


city_connectivity <- read_csv("city connectivity.csv")

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

city_spatial <- st_as_sf(city_connectivity, coords = c("longitude", "latitude"), 
                       crs = 4326, agr = "constant")

ma_spatial <- sfc_as_cols(city_spatial, c("lng", "lat"))


ggplot(ma_spatial, aes(lng, lat)) +                                       #define data and variables for x and y axes 
  stat_binhex() +                                                         #add binhex layer (hexbin)
  scale_fill_gradientn(colours = c("white","red"), name = "Frequency")    #add shading based on number of ASB incidents

tmap_mode("view")
tm_shape(ma_spatial)

ggplot(ma_spatial, aes(x = lng, y = lat)) +
  annotation_map_tile("cartolight",zoom=6) + 
  stat_binhex(alpha=0.8) +                                                #add binhex layer (hexbin)
  scale_fill_gradientn(colours = c("white","red"), name = "Frequency")
