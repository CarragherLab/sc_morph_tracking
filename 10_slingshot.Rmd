---
title: "Untitled"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, cache = TRUE, message = FALSE, warning = FALSE, fig.align = 'centre')
```


```{r}
library(tidyverse)
library(data.table)
#library(ggridges)
library(viridis)
library(slingshot)
library(mclust)
library(ggpubr)
#library(RColorBrewer)


#########################################Gaussian G=5 clusters current best fit####################################################


df <- fread("../data/08_cleaned_umap.csv")






timecourse <- df %>%
  filter(Metadata_library=="timecourse")

timecourse$Metadata_plate <- as.factor(timecourse$Metadata_plate)

timecourse <- timecourse %>%
  mutate_at("Metadata_plate", as.factor)
  
timecourse <- timecourse[order(timecourse$Metadata_plate),]

timecourse_sub <- timecourse %>%
  group_by(Metadata_plate)%>%
  slice_sample(n=2000) %>%
  ungroup()

rm(df)


#get drugs

drugs_df <- df %>%
  filter(Metadata_library != "timecourse") %>%
  select(X1, X2, Metadata_compound, Metadata_concentration)

drugs_rd <- df %>%
  filter(Metadata_library != "timecourse") %>%
  select(X1, X2)






#for kmeans clusters

#plot clusters on umap for kmeans 1:12 clusters
for (i in 1:12){
  cl <- kmeans(rd, centers = i)$cluster
  timecourse_sub$cl <- as.factor(cl)
  p <- ggplot(timecourse_sub, aes(X1, X2))+ 
  geom_point(aes(colour = cl), alpha = 0.5)+
  #scale_colour_viridis_d(option="C")+
  labs(colour="cluster")
  print(p)

}

#plot clusters on umap for gaussian 5:12 clusters
for (i in 5:12){
  cl <- Mclust(rd, G = i)$classification
  timecourse_sub$cl <- as.factor(cl)
  p <- ggplot(timecourse_sub, aes(X1, X2))+ 
  geom_point(aes(colour = cl), alpha = 0.5)+
  #scale_colour_viridis_d(option="C")+
  labs(colour="cluster")
  print(p)

}

#create cluster labels
cl <- Mclust(rd, G = 5)$classification

#add cluster labels to dataframe
timecourse_sub$cl <- as.factor(cl)

ggplot(timecourse_sub, aes(X1, X2))+ 
  geom_point(aes(colour = cl), alpha = 0.5)+
  #scale_colour_viridis_d(option="C")+
  labs(colour="cluster")

fwrite(timecourse_sub, "../data/10_slingshot_clusters.csv")








