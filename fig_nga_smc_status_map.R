# Creating Nigeria map and mapping data on it. 



# create a map to describe the SMC status in each state in Nigeria
# load the create-sf-object and north symbol package
library(sf)
library(ggsn)
?st_as_sf
# load map (shp.file accessed from https://datacatalog.worldbank.org/search/dataset/0039368)
ng_map <- st_read("~/Desktop/door_to_door/nga_admbnda_adm1_osgof/nga_admbnda_adm1_osgof_20161215.shp")

# Load the tidyr package
library(tidyr)

# browse the structure of shp.data
str(ng_map)

# rename variable in map data for consistency
colnames(ng_map)[1] = "state"

# rename speicifc states with long strings for mapping labels
ng_map[3,1] <- "AI" # Akwa Ibom

ng_map[9,1] <- "CR" # Cross River

ng_map[15,1] <- "FCT" # Federal Capital Territory




# read in SMC status data
nigeria_data <- read_excel("~/Desktop/Door_to_door/data/Supplemental file.xlsx", sheet = "SMC status")

str(nigeria_data)

# renmae speicifc states with long strings for mapping labels
nigeria_data[c(3,40),1] <- "AI"

nigeria_data[c(9,46),1] <- "CR"

nigeria_data[c(15, 52),1] <- "FCT"

# browse the updated data
head(nigeria_data, 10)


table(mapdata$index)


# merge data by state
mapdata <- merge(ng_map, nigeria_data, by = "state", all = TRUE)

# create points referring to labels of map
points <- cbind(mapdata, st_coordinates(st_centroid(mapdata$geometry)))

str(points)

# change the position of labels by replacing their coordinates
points[c(3:4), "X"] <- 12.3
points[c(43:44), "X"] <- 4.0
points[c(9:10), "X"] <- 9.7


# use ggplot to map the data of SMC status
map <- ggplot(mapdata) +
  
  geom_sf(aes(fill = index), color = "grey50", size = 0.2) +
  
  scale_fill_manual(values = c( "#089f8f", "#08737f", "#2a4858"))+ # alternatives palette ""#D2E8E8"",#089f8f",,"#4EA0B0","#306E66","#1C464F"
  
  geom_text(data = points, aes(x = X, y = Y, label = paste(state)),
            
            color ="white", size = 1.8, angle = 0) + #
  
  theme_void() + # or type blank() =
  
  theme(legend.title = element_blank(), legend.position = "bottom", legend.text = element_text(size = 8), strip.text = element_text(size = 10)) +
  
  facet_wrap(vars(year))

# save a high resolution pdf pic
pdf(file="Nigeria_status_map.pdf")

map

north2(map, x = .88, y = 0.93, scale = 0.1, symbol = 2) # add 

# optional (sclare bar): scalebardata <- st_transform(mapdata2, 4326)

dev.off()




