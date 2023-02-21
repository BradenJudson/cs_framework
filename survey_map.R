# Braden
# Community Science Framework survey distribution.

# Libraries.
library(googlesheets4); library(ggplot2);
library(dplyr); library(rgdal); library(cowplot)
library(rmapshaper); library(sf); library(raster)

setwd("C:/Users/Brade/OneDrive/Desktop/iNat/report/survey")

data <- read_sheet("https://docs.google.com/spreadsheets/d/15t4CES49zGuai4IqrKX2LUTIN5NqzvFZkqKzlzG6hz0/edit#gid=96158631")

loc_frq <- data[,ncol(data)] %>% 
  `colnames<-`(., c("CFSAUID")) %>% 
  mutate(CFSAUID = as.factor(CFSAUID)) %>% 
  group_by(CFSAUID) %>% 
  summarise(n = length(CFSAUID))
  
unzip("gfsa000a11a_e.zip"); list.files()

# Shape file from: https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm
bc <- st_read("gfsa000b11a_e.shp")  %>% 
  filter(PRNAME == "British Columbia / Colombie-Britannique") %>%
  st_transform(.,
               crs = "+proj=longlat +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") %>% 
  merge(., loc_frq, by = "CFSAUID", all = TRUE)

scales::show_col(c("#9DBF9E", "#A84268", "#FCB97D", "#C0BCB5", "#4A6C6F", "#FF5E5B"))

(bcplot <- ggplot() +
    geom_sf(data = bc, color = "white", 
            lwd = 0, aes(fill = n)) +
    scale_fill_gradientn(
      colors = c("#9DBF9E", "#FCB97D", "#A84268"),
      na.value = "gray80"
    ) +
    theme_void() +
    theme(legend.position = c(0.95, 0.8)))

(bc_rect <- bcplot +
    geom_rect(aes(xmin = -123.8, xmax = -122,
              ymin = 48.22, ymax = 49.4),
              fill = NA, colour = "black", size = 0.5) +
    theme(legend.title = element_blank()))

(final <- bc_rect %>% 
    ggdraw() +
    draw_plot(
      {
        bcplot +
          coord_sf(
            xlim = c(-122, -123.8),
            ylim = c(48.22, 49.4),
            expand = FALSE) +
          theme_void() +
          theme(legend.position = "none",
                panel.border = element_rect(color = "black",
                                            fill = NA,
                                            size = 1) )
      },
      x = 0.03, y = 0.02,
      width = 0.33, height = 0.33
    ))


ggsave("survey.png", units = "px",
       width = 1000, height = 1000)
