
#Set working directory

library(raster)
elevation_world <- getData('worldclim', var='alt', res=2.5)
plot(elevation_world$alt)

#coerce class 'structure("RasterLayer", package = "raster")' to a data.frame
df <- data.frame(rasterToPoints(elevation_world$alt))

library(ggplot2)

#world elevation map
ggplot(df, aes(x=x, y=y)) +
  geom_raster(aes(fill=alt)) +
  #geom_tile(aes(fill=alt)) +
  scale_fill_gradientn(colours = c("gray", "blue", "green", "yellow", "red"),
                       values = scales::rescale(c(0, 0.133, 0.266, 0.4, 1))) +
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black'))


#select Asia region
df_asia <- subset(df, x>60 & x<145 & y>15 & y<60) #focus on Asia

ggplot(df_asia, aes(x=x, y=y)) +
  geom_raster(aes(fill=alt)) +
  #geom_tile(aes(fill=alt)) +
  scale_fill_gradientn(colours = c("white", "blue", "green", "yellow", "red"),
                       values = scales::rescale(c(0, 0.137, 0.138, 0.276, 1))) +
  theme(legend.text=element_text(size=18),
        legend.title=element_text(size=14,face="bold"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black'))


ggsave("elevation map_asia.eps", device=cairo_ps)



