
#Set working directory

mydata <- read.csv("pcoa_bray-curtis_4ggplot.csv", check.names = FALSE, sep = ",")
row.names(mydata) <- mydata$SampleID
mydata <- mydata[,-1]
mydata_df <- as.data.frame(mydata)
library(ggplot2)


fill_color_custom <- c("red1", "blue", "darkgreen", "black",  "blueviolet", "brown", "burlywood", "cadetblue1",
                       "chartreuse", "chocolate",  "darkslateblue", "deeppink1",
                       "dimgray", "gold1", "lightslateblue", "mediumvioletred",  
                       "seagreen2", "slategrey")


ggplot(mydata_df, aes(x=PCo1, y=PCo2, shape = as.factor(day), color = DMM_L6)) +
  #scale_colour_manual(values=fill_color_custom) + 
  geom_point(size=5, alpha = 1) +
  geom_line(aes(group = subject), color = "black", size = 1, alpha = 1,
            #arrow = arrow(length=unit(0.3,"cm"), ends="last", type = "closed")
            ) +
  labs(x = "PCo1 (13.60%)", y = "PCo2 (7.37%)",
       title = "Beta-diversity with mOTUs v2.6") +
  theme(title=element_text(size=14, colour = 'black')) +
  theme(axis.text=element_text(size=14, colour = 'black'),
        axis.title=element_text(size=14, colour = 'black')) +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        legend.key=element_blank()) + #remove grey background
  theme(panel.background = element_rect(fill = 'white', colour = 'lightgray'),
        panel.grid.major = element_line(size = 1, linetype = 'solid', colour = "lightgray"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 4, stroke = 2))) +
  #facet_wrap( ~ host_diet, ncol = 2) +
  #facet_grid(host_subject_id ~ host_diet) +
  theme(strip.text = element_text(size = 14, color="black"))

ggsave("pcoa_bray-curtis.eps", device=cairo_ps)



