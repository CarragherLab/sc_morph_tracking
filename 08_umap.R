#load packages and set ggplot theme
library(tidyverse)
library(data.table)
library(datawizard)
library(umap)

theme_set(theme_classic())

###load data
df <- fread("data/07_cleaned.csv") 

#split into timecourse and validation for normalisation
timecourse <- df %>%
  filter(Metadata_library =="timecourse")

val <- df %>%
  filter(Metadata_library == "VAL")

rm(df)

#Median normalise the entire dataset using "standardise" from datawizard
timecourse_scaled <- timecourse%>%
  mutate_at(vars(-starts_with("Metadata")), standardise) %>%
  na.omit()


val_scaled <- val%>%
  mutate_at(vars(-starts_with("Metadata")), standardise) %>%
  na.omit()

standardised_df <- rbind(timecourse_scaled, val_scaled)

fwrite(standardised_df, "../data/08_cleaned_standardised.csv")

rm(timecourse)
rm(val)

#split meta and feature data
f_dat <- standardised_df %>%
  select(-starts_with("Metadata")) 
m_dat <- standardised_df %>%
  select(starts_with("Metadata"))


### PCA on scaled dataset
data_pca <-  prcomp(f_dat)


p1 <- ggplot(as.data.frame(data_pca$x)) +
  geom_point(aes(PC1, PC2, color=m_dat$Metadata_plate), alpha=0.3) +
  labs(color="plate") 
#scale_color_viridis_d(option="C")

ggsave("../outputs/08_cleaned_pca_by_plate.png", p1, height=6, width = 7)

rm(p1)

umap_df <- data.frame(m_dat, umap(data_pca$x[,1:10])$layout)

rm(list=c("data_pca", "f_dat", "m_dat"))

umap_df$Metadata_mito_intensity <- standardised_df$Cytoplasm_Intensity_MaxIntensity_W4
umap_df$Metadata_nuclear_area <- standardised_df$FilteredNuclei_AreaShape_Area
umap_df$Metadata_cell_area <- standardised_df$Cells_AreaShape_Area
umap_df$Metadata_dapi_intensity <- standardised_df$FilteredNuclei_Intensity_MeanIntensity_W1

fwrite(umap_df, "../data/08_cleaned_umap.csv")

p2 <- ggplot(umap_df, aes(X1, X2))+ 
  geom_point(aes(colour = Metadata_plate), alpha = 0.3)+
  scale_color_viridis_d()+
  labs(colour="")

ggsave("../outputs/08_cleaned_umap_by_plate.png",p2)

