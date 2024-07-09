#load packages and set ggplot theme
library(tidyverse)
library(data.table)
library(datawizard)
library(umap)

theme_set(theme_classic())

###load data
predictions <- fread("../data/04_standardised_predictions.csv")
df <- fread("../data/01_combined_standardised.csv")

dim(df)

df <- left_join(df, predictions)

rm(predictions)

print("read in data")

#create batch label so we can see how the different screens combine

df <- df %>%
  mutate(Metadata_batch = as.factor(case_when(grepl("pseudo", Metadata_plate) ~ "timecourse",
                                              grepl("dif", Metadata_plate) ~ "dif",
                                              grepl("val", Metadata_plate) ~ "val")))

print("dimensions when joined and batch added:") 
dim(df)

#split meta and feature data
f_dat <- df %>%
  select(-starts_with("Metadata")) 
m_dat <- df %>%
  select(starts_with("Metadata"))

rm(df)

dim(f_dat)

### PCA on scaled dataset
data_pca <-  prcomp(f_dat)


p1 <- ggplot(as.data.frame(data_pca$x)) +
  geom_point(aes(PC1, PC2, color=m_dat$Metadata_doublet), alpha=0.3) +
  labs(color="plate") 
#scale_color_viridis_d(option="C")

ggsave("../outputs/05_standardised_pca_by_prediction.png", p1, height=6, width = 7)

rm(p1)

umap_df <- data.frame(m_dat, umap(data_pca$x[,1:10])$layout)

rm(list=c("data_pca", "f_dat", "m_dat"))

fwrite(umap_df, "../data/05_umap_predictions.csv")

p2 <- ggplot(umap_df, aes(X1, X2))+ 
  geom_point(aes(colour = Metadata_doublet), alpha = 0.3)+
  scale_color_viridis_d()+
  labs(colour="")

ggsave("../outputs/05_standardised_umap_by_prediction.png",p2)

