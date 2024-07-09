#load packages and set ggplot theme
library(tidyverse)
library(data.table)
library(datawizard)
library(umap)

theme_set(theme_classic())

###load data
standardised <- fread("../data/01_combined_standardised.csv") 

#split meta and feature data
f_dat <- standardised %>%
  select(-starts_with("Metadata")) 
m_dat <- standardised %>%
  select(starts_with("Metadata"))

rm(standardised)

### PCA on scaled dataset
data_pca <-  prcomp(f_dat)


p1 <- ggplot(as.data.frame(data_pca$x)) +
  geom_point(aes(PC1, PC2, color=m_dat$Metadata_plate), alpha=0.3) +
  labs(color="plate") 
#scale_color_viridis_d(option="C")

ggsave("../outputs/02_combined_standardised_pca_by_plate.png", p1, height=6, width = 7)

rm(p1)

umap_df <- data.frame(m_dat, umap(data_pca$x[,1:10])$layout)

rm(list=c("data_pca", "f_dat", "m_dat"))

fwrite(umap_df, "../data/02_combined_standardised_umap.csv")

p2 <- ggplot(umap_df, aes(X1, X2))+ 
  geom_point(aes(colour = Metadata_plate), alpha = 0.3)+
  scale_color_viridis_d()+
  labs(colour="")

ggsave("../outputs/02_combined_standardised_umap_by_plate.png",p2)

