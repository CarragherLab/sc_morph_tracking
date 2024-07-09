#load packages and set ggplot theme
library(tidyverse)
library(data.table)
library(datawizard)
library(umap)

theme_set(theme_classic())

###load data
timecourse <- fread("rawdata/00_pseudo_sc_sub_E.csv") 
val <- fread("rawdata/01_val_subset.csv")

#remaining IDs after image level QC (stratominer) and single cells removed based on biological thresholds (nuclear and total cell area)
cleaned_ids <- fread("data/03_cleaned_ids.csv")


#format timecourse metadata
timecourse$Metadata_image <- timecourse$Image_ImageNumber
timecourse$Image_ImageNumber <- NULL
timecourse$Metadata_plate <- timecourse$Image_Metadata_platename
timecourse$Image_Metadata_platename <- NULL 
timecourse$Metadata_well <- timecourse$Image_Metadata_well
timecourse$Image_Metadata_well <- NULL
timecourse$Metadata_site <- timecourse$Image_Metadata_site
timecourse$Image_Metadata_site <- NULL
timecourse$Image_Metadata_platenum <- NULL


#create image level ID column from plate name, well and site
timecourse$Metadata_id <- paste0(timecourse$Metadata_plate, "_", timecourse$Metadata_well, "_", timecourse$Metadata_site)
timecourse$Metadata_unique <- paste0(timecourse$Metadata_plate, "_", timecourse$Metadata_well, "_", timecourse$Metadata_site, "_", timecourse$FilteredNuclei_ObjectNumber)


#filter datasets for cleaned and downsampled IDs
timecourse_cleaned <- timecourse %>%
  filter(Metadata_unique %in% cleaned_ids$Metadata_unique)

val_cleaned <- val %>%
  filter(Metadata_unique %in% cleaned_ids$Metadata_unique)


rm(list=c("val", "timecourse"))


timecourse_cleaned$Metadata_library <- "timecourse"
timecourse_cleaned$Metadata_compound <- "timecourse"
timecourse_cleaned$Metadata_treatment <- "timecourse"
timecourse_cleaned$Metadata_compound <- "timecourse"
timecourse_cleaned$Metadata_row <- "timecourse"
timecourse_cleaned$Metadata_col <- "timecourse"
timecourse_cleaned$Metadata_concentration <- "timecourse"
timecourse_cleaned$Metadata_nuclei_location_x <- timecourse_cleaned$FilteredNuclei_Location_Center_X
timecourse_cleaned$Metadata_nuclei_location_y <- timecourse_cleaned$FilteredNuclei_Location_Center_Y



# get col names that are common to both df's
common_cols <- intersect(names(timecourse_cleaned), names(val_cleaned))

#select only those columns
timecourse_cleaned <- timecourse_cleaned %>%
  select(all_of(common_cols))

val_cleaned <- val_cleaned %>%
  select(all_of(common_cols))

filter = "^Image_|FileName|Object.?Number|Image.?Number|^URL|Location|[XY]$|Phase|NumberOfNeighbors|Orientation|Extent|BoundingBox|Moment|EulerNumber"
removed_features = grep(filter, colnames(timecourse_cleaned), value = TRUE)

timecourse_cleaned<- timecourse_cleaned %>%
  select(-all_of(removed_features))


val_cleaned<- val_cleaned %>%
  select(-all_of(removed_features))


#Median normalise the entire dataset using "standardise" from datawizard
timecourse_scaled <- timecourse_cleaned %>%
  mutate_at(vars(-starts_with("Metadata")), standardise) %>%
  na.omit()

fwrite(timecourse_scaled, "data/01_timecourse_standardised.csv")

val_scaled <- val_cleaned %>%
  mutate_at(vars(-starts_with("Metadata")), standardise) %>%
  na.omit()


cleaned <- rbind(timecourse_cleaned, val_cleaned)
fwrite(cleaned, "data/01_combined_cleaned.csv")

rm(cleaned)
rm(timecourse_cleaned)
rm(val_cleaned)

standardised <- rbind(timecourse_scaled, val_scaled)
fwrite(standardised, "data/01_combined_standardised.csv")

rm(list=c("timecourse_scaled", "val_scaled", "common_cols", "filter", "removed_features", "cleaned_ids"))


#split meta and feature data
f_dat <- standardised %>%
  select(-starts_with("Metadata")) 
m_dat <- standardised %>%
  select(starts_with("Metadata"))


### PCA on scaled dataset
data_pca <-  prcomp(f_dat)


p1 <- ggplot(as.data.frame(data_pca$x)) +
  geom_point(aes(PC1, PC2, color=m_dat$Metadata_plate), alpha=0.3) +
  labs(color="plate") 
#scale_color_viridis_d(option="C")

ggsave("../outputs/01_pca_by_plate.png", p1, height=6, width = 7)

rm(p1)

umap_df <- data.frame(m_dat, umap(data_pca$x[,1:10])$layout)

rm(list=c("data_pca", "f_dat", "m_dat"))

fwrite(umap_df, "../data/01_batchcorrected_subset_umap.csv")

p4 <- ggplot(umap_df, aes(X1, X2))+ 
  geom_point(aes(colour = Metadata_plate), alpha = 0.3)+
  scale_color_viridis_d()+
  labs(colour="")

ggsave("../outputs/01_umap_by_plate.png",p4)

