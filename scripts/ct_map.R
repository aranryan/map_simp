
# this approach is based on the following article
#http://www.r-bloggers.com/creating-styled-google-maps-in-ggmap/

library("RJSONIO")
library("ggmap")
library("magrittr")
library("readxl")
library("dplyr")
require(RColorBrewer)
library("directlabels")

# read in addresses of facilities
facility_info <- read_excel("input_data/facility_info.xlsx", sheet = 1, col_names = TRUE, col_types = NULL)
colnames(facility_info) <- colnames(facility_info) %>%
  tolower() %>%
  gsub("\\.", "\\_", .)
facility_info <- facility_info %>%
  filter(!is.na(facility)) %>%
  data.frame()
  
# add lat lon with geocode function in ggmap
facility_info <- facility_info %>%
  mutate_geocode(location = address) 

####
# 
# set up color palette
display.brewer.pal(10, "RdBu")
mypallete <- brewer.pal( 10 , "RdBu" )


######
#
# I had initially followed the approach outlined here:
#http://www.r-bloggers.com/creating-styled-google-maps-in-ggmap/
# this gives a few functions for creating a style string that can be used to 
# pull styled maps from the Google API. What I found was that the functions were maybe affecting
# how the style was getting described (I think order matters, so maybe that's the issue) and I 
# wasn't getting the right maps back. So I ended up adding a step where I just get to a map style 
# that I like at the Google site, and then copy the link and use it to create a string describing that
# style, and then I can use that in pulling the map. 

# set up a style using Styled Maps Wizard website:
#http://gmaps-samples-v3.googlecode.com/svn/trunk/styledmaps/wizard/index.html

style <- '[
  {
"featureType": "road.highway",
"elementType": "geometry.fill",
"stylers": [
{ "visibility": "on" },
{ "color": "#808080" },
{ "weight": 0.4 }
]
},{
"featureType": "administrative.province",
"elementType": "geometry",
"stylers": [
{ "visibility": "on" },
{ "color": "#808080" },
{ "weight": 2.9 }
]
},{
"featureType": "road",
"elementType": "geometry.stroke",
"stylers": [
{ "color": "#808080" }
]
}
]'
style_list <- fromJSON(style, asText=TRUE)

create_style_string<- function(style_list){
  style_string <- ""
  for(i in 1:length(style_list)){
    if("featureType" %in% names(style_list[[i]])){
      style_string <- paste0(style_string, "feature:", 
                             style_list[[i]]$featureType, "|")      
    }
    elements <- style_list[[i]]$stylers
    a <- lapply(elements, function(x)paste0(names(x), ":", x)) %>%
      unlist() %>%
      paste0(collapse="|")
    style_string <- paste0(style_string, a)
    if(i < length(style_list)){
      style_string <- paste0(style_string, "&style=")       
    }
  }  
  # google wants 0xff0000 not #ff0000
  style_string <- gsub("#", "0x", style_string)
  return(style_string)
}

style_string <- create_style_string(style_list)
style_string

########
#
# created my own style string by creating and copying from
# http://gmaps-samples-v3.googlecode.com/svn/trunk/styledmaps/wizard/index.html

style_string_2 <- c("feature:road.highway|element:geometry.fill|visibility:on|color:0x808080|weight:0.4&style=feature:administrative.province|element:geometry|visibility:on|color:0x808080|weight:2.9&style=feature:landscape.man_made|element:geometry.fill|color:0xf4a583&style=feature:road.highway|element:labels|visibility:off")
style_string_3 <- c("feature:road.highway|element:geometry.fill|visibility:on|color:0x808080|weight:0.1&style=feature:road.highway|element:geometry.stroke|visibility:on|color:0x808080&style=feature:road.highway|element:labels.icon|visibility:off&style=feature:poi.park|visibility:off&style=feature:landscape.man_made|element:geometry.fill|visibility:on|weight:0.1|color:0xa5a5a5|lightness:50")
style_string_4 <- c("feature:road.highway|element:geometry.fill|visibility:on|color:0x808080|weight:0.1&style=feature:road.highway|element:geometry.stroke|visibility:on|color:0x808080&style=feature:road.highway|element:labels.icon|visibility:off&style=feature:poi.park|visibility:off&style=feature:landscape.man_made|element:geometry.fill|visibility:on|weight:0.1|color:0xa5a5a5|lightness:50&style=feature:administrative.province|element:geometry|visibility:on|color:0x808080|weight:2.7")
############
#
# create map
temp <- facility_info %>%
  filter(include_on_map == "Yes")

# this approach of using the visible argument didn't seem to work
# vis <- temp %>%
#   select(lon, lat) 
# mymap <- ggmap(get_googlemap(visible=vis, size=c(640,640), scale = 2, zoom = 8, style=style_string_2, maptype="roadmap", key="AIzaSyBOybTC9pXmOwVo0itauCJ_PnLziPekN3E"), extent="device")
# mymap

# instead used this approach of specifying a center
cent <- c(-72.724548, 41.576468)
mymap <- ggmap(get_googlemap(center=cent, size=c(640,640), scale = 2, zoom = 8, style=style_string_4, maptype="roadmap", key="AIzaSyBOybTC9pXmOwVo0itauCJ_PnLziPekN3E"), extent="device")
mymap

# takes the map and adds the legend
p <- mymap + geom_point(data =temp,aes(x=lon,y=lat, color=status, group=map_name), size=5)
p <- p + scale_colour_manual(
  values = c("Existing" = "grey30","Recently opened" = mypallete[3],"Proposed" = mypallete[2]),
  guide = guide_legend(title = "Casinos", title.theme = element_text(size=8, face="bold", angle = 0))) + 
  # format legend text
  theme(legend.text = element_text(size = 8, face = 'plain')) +
  # put white background
  theme(legend.key = element_rect(colour = 'white', linetype=NULL)) +
  # put legend in the lower right corner
  theme(legend.justification=c(1,0), legend.position=c(1,0.02))
p
ggsave(filename="output_data/mymap_nolabels.png", width=5, height=5)

# add simple labels
p <- p + geom_text(data=temp,aes(x=lon, y=lat, label=map_name),size=3, colour="black", hjust=0, vjust=1.2)
p
ggsave(filename="output_data/mymap_labels.png", width=6.5, height=6.5)

# use a direct labels approach based on the following great post
# http://stackoverflow.com/questions/30178954/dynamic-data-point-label-positioning-in-ggmap

draw.rects.modified <- function(d,...){
  if(is.null(d$box.color))d$box.color <- NA
  if(is.null(d$fill))d$fill <- "grey95"
  for(i in 1:nrow(d)){
    with(d[i,],{
      grid.rect(gp = gpar(col = box.color, fill = fill),
                vp = viewport(x, y, w, h, "cm", c(hjust, vjust=0.25), angle=rot))
    })
  }
  d
}

enlarge.box.modified <- function(d,...){
  if(!"h"%in%names(d))stop("need to have already calculated height and width.")
  calc.borders(within(d,{
    w <- 0.9*w
    h <- 1.1*h
  }))
}

boxes <-
  list("top.bumptwice", "calc.boxes",  "enlarge.box.modified", "draw.rects.modified")

mymap + 
  geom_point(data = temp,aes(x = lon, y = lat), 
             alpha = 1, fill = "red", pch = 21, size = 6) + 
  geom_dl(data = temp, aes(label = map_name), method = list(dl.trans(y = y + 0.4), 
                                                        "boxes", cex = .8, fontface = "bold"))
ggsave(filename="output_data/mymap_dllabels.png", width=6.5, height=6.5)