library(tidyverse)
library(data.table)


theme_set(theme_classic())

df <- fread("../data/06_doublets_rm.csv")
umap_df <- fread("../data/06_doublet_rm_umap.csv")

ggplot(umap_df, aes(X1, X2))+ 
  geom_point()+
  geom_vline(xintercept=-13)


outliers <- umap_df %>%
  filter(X1 > 15) %>%
  select(Metadata_unique)

fwrite(outliers, "../data/07_outliers.csv")


cleaned <- df %>%
  filter(!Metadata_unique %in% outliers$Metadata_unique)

fwrite(cleaned, "../data/07_cleaned.csv")

dim(cleaned) == norw(df) - nrow(outliers)


