library(tidyverse)
library(data.table)
library(datawizard)
library(umap)


theme_set(theme_classic())


predictions <- fread("../data/04_standardised_predictions.csv")

df <- fread("../data/01_combined_cleaned.csv")

df <- left_join(df, predictions)

rm(predictions)

print("read in data")

#create batch label so we can see how the different screens combine

df <- df %>%
  mutate(Metadata_batch = as.factor(case_when(grepl("pseudo", Metadata_plate) ~ "timecourse",
                                              grepl("val", Metadata_plate) ~ "val")))


print("formatted data")

print("Number of doublets in df:")

table(df$Metadata_doublet)


doublet_rm <- df %>%
  filter(Metadata_doublet != "doublet")

table(doublet_rm$Metadata_doublet)

print("removed doublets")

fwrite(doublet_rm, "../data/06_doublets_rm.csv")



#split into timecourse and validation for normalisation
timecourse <- doublet_rm %>%
  filter(Metadata_library =="timecourse")

val <- doublet_rm %>%
  filter(Metadata_library == "VAL")

rm(doublet_rm)

#Median normalise the entire dataset using "standardise" from datawizard
timecourse_scaled <- timecourse%>%
  mutate_at(vars(-starts_with("Metadata")), standardise) %>%
  na.omit()


val_scaled <- val%>%
  mutate_at(vars(-starts_with("Metadata")), standardise) %>%
  na.omit()

doublet_rm <- rbind(timecourse_scaled, val_scaled)

print("standardised data")

fwrite(doublet_rm, "../data/06_doublets_rm_standardised.csv")



f_dat <- doublet_rm %>%
  select(-starts_with("Metadata")) 
m_dat <- doublet_rm %>%
  select(starts_with("Metadata"))



### PCA on scaled and thresholded dataset
data_pca <-  prcomp(f_dat)

fwrite(data.frame(m_dat, data_pca$x[,1:20]), "../data/06_doublets_rm_pca.csv")

p1 <- ggplot(as.data.frame(data_pca$x)) +
  geom_point(aes(PC1, PC2, color=m_dat$Metadata_plate), alpha=0.3) +
  labs(color="plate") 
#scale_color_viridis_d(option="C")

ggsave("../outputs/06_doublets_rm_pca_by_plate.png", p1, height=6, width = 7)

rm(p1)

print("PCA complete")


### UMAP on first 20 PC's
umap_doublet_rm <- data.frame(m_dat, umap(data_pca$x[,1:10])$layout)

rm(list=c("data_pca", "f_dat", "m_dat"))

plot_features <- c("Cytoplasm_Intensity_MaxIntensity_W4", "FilteredNuclei_AreaShape_Area", "FilteredNuclei_Intensity_MeanIntensity_W1")

umap_doublet_rm$Metadata_mito_intensity <- doublet_rm$Cytoplasm_Intensity_MaxIntensity_W4
umap_doublet_rm$Metadata_nuclear_area <- doublet_rm$FilteredNuclei_AreaShape_Area
umap_doublet_rm$Metadata_cell_area <- doublet_rm$Cells_AreaShape_Area
umap_doublet_rm$Metadata_dapi_intensity <- doublet_rm$FilteredNuclei_Intensity_MeanIntensity_W1


fwrite(umap_doublet_rm, "../data/06_doublet_rm_umap.csv")

p4 <- ggplot(umap_doublet_rm, aes(X1, X2))+ 
  geom_point(aes(colour = Metadata_plate), alpha = 0.3)+
  scale_color_viridis_d()+
  labs(colour="plate")

ggsave("../outputs/06_doublets_rm_umap_by_plate.png",p4)

rm(p4)

plot_features <- c("Metadata_mito_intensity", "Metadata_nuclear_area", "Metadata_cell_area", "Metadata_dapi_intensity")

for(i in plot_features){
  p1 <- ggplot(umap_doublet_rm, aes(X1, X2))+ 
    geom_point(aes_string(colour = i), alpha = 0.7, size=2)+
    scale_colour_viridis_c()+
    labs(colour="")+
    ggtitle(i)
  
  ggsave(paste0("../outputs/06_doublets_rm_", i, "_umap.png"), p1, width =10)
}

print("UMAP complete")