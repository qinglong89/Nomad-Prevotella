
setwd("C:/Users/qinglonw/Desktop/New folder/Figure 1B & 1D")
getwd()

#generate PCo1 vs PCo2 plots
mydata <- read.csv("pcoa_bray-curtis_4ggplot.csv", check.names = FALSE, sep = ",")
row.names(mydata) <- mydata$SampleID
mydata <- mydata[,-1]
mydata_df <- as.data.frame(mydata)
library(ggplot2)


fill_color_custom <- c("red1", "blue", "darkgreen", "black",  "blueviolet", "cyan", "burlywood", "cadetblue1",
                       "chartreuse", "chocolate",  "darkslateblue", "deeppink1",
                       "dimgray", "gold1", "lightslateblue", "mediumvioletred",  
                       "seagreen2", "slategrey")

mydata_df$Ethnicity <- factor (mydata_df$Ethnicity, 
                           levels = c("Tibetan", "Mongolian", "Kazakh", "Denmark",
                                      "Han Chinese", "USA (HMP)", "Germany"))


#PCo1 vs ethnicity
ggplot(mydata_df, aes(x=Ethnicity, y=PCo1, fill = Ethnicity)) +
  labs(title = "", x = "", y = "PCo1") +
  scale_fill_manual(values=fill_color_custom) +
  #geom_violin(trim=FALSE, alpha = 0.75) +
  #geom_jitter(shape = 19, size = 2, width = 0.2,  alpha = 0.25) +
  geom_boxplot(width = 0.6, outlier.size = -1, size = 1, alpha = 1, 
               position = position_dodge(preserve = "single")) +
  theme(title=element_text(size=12, color="black"),
        axis.title=element_text(size=14, color="black"),
        axis.text=element_text(size=12, color="black"), 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, size=12, color="black"),
        legend.text=element_text(size=12,  color="black"),
        legend.title=element_text(size=14,  color="black"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  guides(fill=guide_legend(ncol=1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))   #Increase distance between text and title on the y-axis

ggsave("PCo1 vs ethnicity_boxplot.eps", device=cairo_ps)

#add stats
Pairwise_Wilcox_BH <- pairwise.wilcox.test(mydata_df$PCo1, mydata_df$Ethnicity, alternative = c("two.sided"), paired = FALSE, p.adj = "BH")
write.table(Pairwise_Wilcox_BH$p.value, file = "Pairwise-Wilcox-BH_PCo1.txt", sep = "\t", row.names = TRUE, col.names = TRUE)


#PCo2 vs ethnicity
ggplot(mydata_df, aes(x=Ethnicity, y=PCo2, fill = Ethnicity)) +
  labs(title = "", x = "", y = "PCo2") +
  scale_fill_manual(values=fill_color_custom) +
  #geom_violin(trim=FALSE, alpha = 0.75) +
  #geom_jitter(shape = 19, size = 2, width = 0.2,  alpha = 0.25) +
  geom_boxplot(width = 0.6, outlier.size = -1, size = 1, alpha = 1, 
               position = position_dodge(preserve = "single")) +
  theme(title=element_text(size=12, color="black"),
        axis.title=element_text(size=14, color="black"),
        axis.text=element_text(size=12, color="black"), 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, size=12, color="black"),
        legend.text=element_text(size=12,  color="black"),
        legend.title=element_text(size=14,  color="black"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  guides(fill=guide_legend(ncol=1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))   #Increase distance between text and title on the y-axis

ggsave("PCo2 vs ethnicity_boxplot.eps", device=cairo_ps)

#add stats
Pairwise_Wilcox_BH <- pairwise.wilcox.test(mydata_df$PCo2, mydata_df$Ethnicity, alternative = c("two.sided"), paired = FALSE, p.adj = "BH")
write.table(Pairwise_Wilcox_BH$p.value, file = "Pairwise-Wilcox-BH_PCo2.txt", sep = "\t", row.names = TRUE, col.names = TRUE)


#PCo1 vs PCo2
ggplot(mydata_df, aes(x=PCo1, y=PCo2, colour = Ethnicity)) +
  scale_colour_manual(values=fill_color_custom) + 
  geom_point(size=2, alpha = 0.5) +
  labs(colour = "Group", 
       x = "PCo1 (9.52%)", y = "PCo2 (5.84%)",
       title = "Beta-diversity with mOTUs v2.6") +
  theme(title=element_text(size=14, colour = 'black')) +
  theme(axis.text=element_text(size=14, colour = 'black'),
        axis.title=element_text(size=14, colour = 'black')) +
  theme(legend.text=element_text(size=14),
        legend.title=element_text(size=14),
        legend.position = "right",
        legend.key=element_blank()) + #remove grey background
  theme(#panel.background = element_rect(fill = 'white', colour = 'lightgray'),
        panel.grid.major = element_line(size = 1, linetype = 'solid', colour = "lightgray"),
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 4, stroke = 2))) +
  #facet_wrap( ~ Dataset, ncol = 3) +
  #facet_wrap( ~ Ethnicity, ncol = 4) +
  theme(strip.text = element_text(size = 14, color="black"))

ggsave("pcoa_bray-curtis_ethnicity.eps", device=cairo_ps)


#highlight DMM clusters
ggplot(mydata_df, aes(x=PCo1, y=PCo2, colour = DMM_L6)) +
  #scale_colour_manual(values=fill_color_custom) + 
  geom_point(size=3, alpha = 1) +
  labs(colour = "DMM_L6", 
       x = "PCo1 (9.52%)", y = "PCo2 (5.84%)",
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
  #facet_wrap( ~ Dataset, ncol = 3) +
  #facet_wrap( ~ Ethnicity, ncol = 4) +
  theme(strip.text = element_text(size = 14, color="black"))

ggsave("pcoa_bray-curtis_DMM-L6.eps", device=cairo_ps)



#plot P. copri abundance
mydata_df$Group <-  factor(mydata_df$Group, 
                           levels = c("Mongolian", "Kazakh", "Tibetan_Tibet", "Tibetan_Yunnan",
                                      "Denmark", "Germany", "Han Chinese", "USA (HMP)"))

ggplot(mydata_df, aes(x=Group, y=Prevotella_copri, color = DMM_L6)) +
  labs(title = "Meta-analysis", x = "", y = "Prevotella copri") +
  scale_fill_manual(values=fill_color_custom) +
  #geom_jitter(shape = 19, size = 2, width = 0.2,  alpha = 1) +
  geom_boxplot(width = 0.6, outlier.size = -1, size = 0.5, alpha = 0.1,
               position = position_dodge(preserve = "single")) +
  theme(title=element_text(size=12, color="black"),
        axis.title=element_text(size=14, color="black"),
        axis.text=element_text(size=12, color="black"), 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, size=12, color="black"),
        legend.text=element_text(size=12,  color="black"),
        legend.title=element_text(size=14,  color="black"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  guides(fill=guide_legend(ncol=1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))   #Increase distance between text and title on the y-axis
  #annotate("text", x = 1.5, y = 1, label = paste("Wilcox test: p = 0.0083")) 

ggsave("Prevotella_copri_boxplot.eps", device=cairo_ps)

#add stats
subset <- subset(mydata_df, DMM_L6 == "Cluster 3")
Pairwise_Wilcox_BH <- pairwise.wilcox.test(subset$Prevotella_copri, subset$SuperGroup, alternative = c("two.sided"), paired = FALSE, p.adj = "BH")
write.table(Pairwise_Wilcox_BH$p.value, file = "Pairwise-Wilcox-BH.txt", sep = "\t", row.names = TRUE, col.names = TRUE)




#plot Shannon
ggplot(mydata_df, aes(x=Group, y=Shannon, color = DMM_L6)) +
  labs(title = "Meta-analysis", x = "", y = "Shannon index") +
  scale_fill_manual(values=fill_color_custom) +
  #geom_jitter(shape = 19, size = 2, width = 0.2,  alpha = 1) +
  geom_boxplot(width = 0.6, outlier.size = -1, size = 0.5, alpha = 0.1,
               position = position_dodge(preserve = "single")) +
  theme(title=element_text(size=12, color="black"),
        axis.title=element_text(size=14, color="black"),
        axis.text=element_text(size=12, color="black"), 
        axis.text.x = element_text(angle = 30, vjust = 1, hjust=1, size=12, color="black"),
        legend.text=element_text(size=12,  color="black"),
        legend.title=element_text(size=14,  color="black"),
        axis.line = element_line(colour = "black"), 
        panel.background = element_rect(fill = 'white', colour = 'black')) +
  guides(fill=guide_legend(ncol=1)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))   #Increase distance between text and title on the y-axis

ggsave("Shannon_boxplot.eps", device=cairo_ps)

#add stats
subset <- subset(mydata_df, DMM_L6 == "Cluster 3")
Pairwise_Wilcox_BH <- pairwise.wilcox.test(mydata_df$Shannon, mydata_df$SuperGroup, alternative = c("two.sided"), paired = FALSE, p.adj = "BH")
write.table(Pairwise_Wilcox_BH$p.value, file = "Pairwise-Wilcox-BH.txt", sep = "\t", row.names = TRUE, col.names = TRUE)







