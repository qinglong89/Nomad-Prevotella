
#set working directory

charts.data <- read.csv("Combined_mOTU_v2.6_profiles_L5_4ggplot.csv")

library(ggplot2)
library(reshape2)

#melt the dataframe to fit the input requirement by ggplot
charts.data.melt <- melt(charts.data, id=c("SampleID", "Ethnicity", "Enterotype"))

fill_color_custom <- c("red1", "blue", "darkgreen", "black",  
                       "blueviolet", "cyan", "burlywood", "deeppink1",
                       "chartreuse", "chocolate",  "darkslateblue", "brown",
                       "seagreen2", "gold1", "lightslateblue", "gray",
                       "mediumvioletred", "dimgray", "slategrey")

charts.data.melt$Ethnicity <- factor (charts.data.melt$Ethnicity, 
                               levels = c("Tibetan", "Mongolian", "Kazakh", "Denmark",
                                          "Han Chinese", "USA (HMP)", "Germany"))

#remove background
ggplot(data = charts.data.melt, aes(y = value, x = Ethnicity, fill = variable)) +
  geom_bar(stat = "identity", width=0.9, position = position_stack(reverse = F)) +
  labs(y = "Average relative abundance", fill = "Family") +
  scale_fill_manual(values=fill_color_custom) +
  facet_wrap( ~ factor(Enterotype), ncol = 3) +
  theme(strip.text = element_text(size = 12, color="black")) +
  theme(title=element_text(size=12, color="black"),
        axis.title=element_text(size=14, color="black", face="bold"),
        axis.text=element_text(size=12, color="black"), 
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size=6, color="black"),
        legend.text=element_text(size=10,  color="black"),
        legend.title=element_text(size=10,  color="black"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  guides(fill=guide_legend(ncol=1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) + #Increase distance between text and title on the y-axis
  scale_y_continuous(expand = expand_scale(mult = c(0.01, 0.01))) # this is to control the space between axis & area-plot in ggplot2

ggsave("Per-enterotype L5_stacked abundance plot.eps", device=cairo_ps)

