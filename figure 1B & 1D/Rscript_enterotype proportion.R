
#set working directory

charts.data <- read.csv("Enterotype Proportion_4ggplot.csv")

library(ggplot2)
library(reshape2)

#melt the dataframe to fit the input requirement by ggplot
charts.data.melt <- melt(charts.data, id=c("Ethnicity"))

charts.data.melt$Ethnicity <-  factor(charts.data.melt$Ethnicity, 
                                  levels = c("Mongolian", "Kazakh", "Tibetan_Tibet", "Tibetan_Yunnan",
                                             "Denmark", "Germany", "Han Chinese", "USA (HMP)"))

ggplot(data = charts.data.melt, aes(x = factor(Ethnicity), y = value, fill = variable)) +
  geom_bar(stat = "identity", width=0.9, alpha = 1) +
  labs(y = "Proportion", x="", fill = "Enterotype") +
  #scale_fill_manual(values=fill_color_custom) +
  #facet_wrap( ~ Ethnicity, ncol = 4) +
  theme(strip.text = element_text(size = 12, color="black")) +
  theme(title=element_text(size=12, color="black"),
        axis.title=element_text(size=14, color="black", face="bold"),
        axis.text=element_text(size=12, color="black"), 
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=8, color="black"),
        legend.text=element_text(size=8,  color="black"),
        legend.title=element_text(size=12,  color="black"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  guides(fill=guide_legend(ncol=1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) + #Increase distance between text and title on the y-axis
  scale_y_continuous(expand = expand_scale(mult = c(0.01, 0.01))) # this is to control the space between axis & area-plot in ggplot2


ggsave("Enterptype proportion per ethnicity.eps", device=cairo_ps)



