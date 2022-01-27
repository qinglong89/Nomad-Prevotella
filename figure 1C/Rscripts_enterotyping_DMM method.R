
#Set working directory

library(microbiome)
library(DirichletMultinomial)
library(reshape2)
library(magrittr)
library(dplyr)


mydata <- read.csv("Combined_mOTU_v2.6_profiles_L6_Normalized10K_4DMM.csv", check.names = FALSE, sep = ",")
row.names(mydata) <- mydata$SampleID
mydata <- mydata[,-1]

count <- as.matrix(mydata)


#Fit the DMM model. Let us set the maximum allowed number of community types to 3 to speed up the example.
fit <- lapply(1:3, dmn, count = count, verbose=TRUE)

#Check model fit with different number of mixture components using standard information criteria
lplc <- sapply(fit, laplace) # AIC / BIC / Laplace
aic  <- sapply(fit, AIC) # AIC / BIC / Laplace
bic  <- sapply(fit, BIC) # AIC / BIC / Laplace

plot(lplc, type="b", xlab="Number of Dirichlet Components", ylab="Model Fit")
lines(aic, type="b", lty = 2)
lines(bic, type="b", lty = 3)

#Pick the optimal model
best <- fit[[which.min(unlist(lplc))]]

#Mixture parameters pi and theta
mixturewt(best)

#Sample-component assignments
ass <- apply(mixture(best), 1, which.max)

write.csv(ass, file="DMM_3clusters_L6.csv")

#Contribution of each taxonomic group to each component

for (k in seq(ncol(fitted(best)))) {
  d <- melt(fitted(best))
  colnames(d) <- c("Feature", "cluster", "strength")

  d <- subset(d, cluster == k) %>%
    # Arrange OTUs by assignment strength
    arrange(strength) %>%
    mutate(Feature = factor(Feature, levels = unique(Feature))) %>%
    # Only show the most important drivers
    filter(abs(strength) > quantile(abs(strength), 0.97))     
  
  p <- ggplot(d, aes(x = Feature, y = strength)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = paste("Top drivers: community type", k))
  
  print(p)
}










