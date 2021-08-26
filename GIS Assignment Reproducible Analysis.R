####################### Packages we need ###########################

library(sp)
library(ggplot2)
library(ggmap)
library(sf)
library(tmap)
library(sp)
library(spdep)
library(plyr)
library(dplyr)
library(spData)
library(hexbin)
library(readr)
library(ggspatial)
library(leaflet)
library(igraph)
library(ggraph)
library(dplyr)
library(tidyr)
####################### Raw data loading ###########################

df_trends <- read_csv("data/Basic Analysis Data/patent numbers (foreign and china).csv")

df_scale <- read_csv("data/Basic Analysis Data/region_domestic.csv")

df_knowledge_combination_edges <- read_csv("data/Network Data/Patent Knowledge network edges.csv")
df_knowledge_combination_nodes <- read_csv("data/Network Data/Patent Knowledge network nodes.csv")

df_city_network <- read_csv("data/Network Data/Top 25% cities potential connection.csv")

df_connectivity <- read_csv("data/Map Data/city connectivity.csv")

df_regression_knowledge <- read_csv("data/Regression Data/Patent knowledge network.csv")
df_regression_city <- read_csv("data/Regression Data/city network.csv")

######################## Analysis Part #############################

# 1.Basic Statistics Analysis

## Trends of number of patents ##

ggplot(data=df_trends, aes(x=Year, y=`Number of Patents`, group=Region, color=Region)) +
  geom_line(size=1.2) + geom_point(size=3)+
  scale_color_brewer(palette="Paired")+
  theme_minimal()

## Number of patents in each province(cities) ##

#aggregate the count column based on two columns.
df2_agg <- aggregate(count~region+status, data=df_scale, sum)

#sort the count number based on region and status column
df2_sorted <- arrange(df2_agg, region, status) 
head(df2_sorted)

# Calculate the cumulative sum of count for each city
df2_cumsum <- ddply(df2_sorted, "region",
                   transform, label_count=cumsum(count))

df2_all <- aggregate(count~region, data=df2_cumsum, sum)

df2_cumsum <- left_join(df2_cumsum, df2_all, by = c("region" = "region"))

df2_cumsum_sorted <- arrange(df2_cumsum,count.y)
head(df2_cumsum_sorted)

# Create the bar plot
ggplot(data=df2_cumsum_sorted, aes(x = reorder(region, -count.x), y=count.x, fill=status)) +
  geom_bar(stat="identity")+
  #geom_text(aes(y=label_count, label=count.x), vjust=1.6, 
  #          color="white", size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_minimal()

# 2.EPP patents Knowledge Network

#constructing knowledge network
knowledge_network <- graph_from_data_frame(d=df_knowledge_combination_edges, vertices=df_knowledge_combination_nodes, directed=F)
summary(knowledge_network)

#Plot the knowledge network by using gggraph
got_palette <- c("#a6bddb", "#fdae61", "#e0f3f8","#6baed6" , "#4575b4", "#abd9e9","#fee090","#f46d43","#878787")
ggraph(knowledge_network,"stress",bbox = 15)+
  geom_edge_link0(edge_colour = "#eaeaea",edge_width =0.2)+
  geom_node_point(aes(fill = type),shape = 21,size = sqrt(sqrt(igraph::degree(knowledge_network))))+
  geom_node_text(aes(label =  ifelse(igraph::degree(knowledge_network) > 100, V(knowledge_network)$name, NA),size =10* sqrt(sqrt(igraph::degree(knowledge_network)))),
                 family = "serif",repel = TRUE,colour='#252525')+
  scale_fill_manual(values=got_palette)+
  scale_size(range=c(2,5),guide = FALSE)+
  theme_graph()+
  theme(legend.position = "bottom")

# 3.EPP Patents Spatial Network (at the city level) 

#construct the input data structure for spatial network
df_city_network_2 <- df_city_network %>% pivot_longer(cols = -c('X1'), names_to = "to", values_to = "weight")

#drop the rows that contain zero number
df_city_network_edges <-  df_city_network_2[df_city_network_2$weight != 0, ]

#rename column name
colnames(df_city_network_edges)[1] <- "from"

#acquire the latest nodes list
df_city_network_nodes <- subset(df_city_network_edges, select = -c(weight) )

#combine the two columns' nodes together
df_city_network_nodes <- data.frame(name = unlist(df_city_network_nodes, use.names = FALSE)) 

#drop the duplicated nodes
df_city_network_nodes <- data.frame(name = df_city_network_nodes[!duplicated(df_city_network_nodes), ]) 

city_network <- graph_from_data_frame(d=df_city_network_edges, vertices=df_city_network_nodes, directed=F)

#Plot the knowledge network by using gggraph
V(city_network)$color <- ifelse(strength(city_network, vids = V(city_network), mode = c("all"),
                                         loops = TRUE) > 95000, "#fee090", ifelse(strength(city_network, vids = V(city_network), mode = c("all"),
                                                                                           loops = TRUE) > 10000 & strength(city_network, vids = V(city_network), mode = c("all"),
                                                                                                                            loops = TRUE) < 95000,"#4575b4",'#74add1' ))
#plot the graph
ggraph(city_network,"stress",bbox = 15)+
  geom_edge_link0(edge_colour = "#eaeaea",edge_width =0.2)+
  geom_node_point(fill = V(city_network)$color,shape = 21,size = 1.2*sqrt(sqrt(strength(city_network, vids = V(city_network), mode = c("all"),
                                                                             loops = TRUE))))+
  geom_node_text(aes(label = name,size = 1),
                 repel = TRUE)+
  scale_fill_manual(values=V(city_network)$color)+
  scale_size(range=c(2,5),guide = FALSE)+
  theme_graph()+
  theme(legend.position = "bottom")

# 4.Frequency of potential connectivity in each city

sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

city_spatial <- st_as_sf(df_connectivity, coords = c("longitude", "latitude"), 
                       crs = 4326, agr = "constant")

ma_spatial <- sfc_as_cols(city_spatial, c("lng", "lat"))

#define data and variables for x and y axes, add binhex layer (hexbin), and add shading based on number of ASB incidents
ggplot(ma_spatial, aes(lng, lat)) +                                       
  stat_binhex() +                                                         
  scale_fill_gradientn(colours = c("white","red"), name = "Frequency")  

tmap_mode("view")

#Plot the map
ggplot(ma_spatial, aes(x = lng, y = lat)) +
  annotation_map_tile("cartolight",zoom=6) + 
  stat_binhex(alpha=0.8) +                                                
  scale_fill_gradientn(colours = c("white","red"), name = "Frequency")

# 5.regression

#scatter plot of knowledge degree and its rank
reg1 <- lm(knowledge_degree~knowledge_rank,data=df_regression_knowledge) 
summary(reg1)

#fit with regression line
with(df_regression_knowledge,plot(knowledge_rank, knowledge_degree))
abline(reg1)

#scatter plot of city degree and its rank
reg2 <- lm(city_degree~city_rank,data=df_regression_city) 
summary(reg2)

#fit with regression line
with(df_regression_city,plot(city_rank, city_degree))
abline(reg2)

