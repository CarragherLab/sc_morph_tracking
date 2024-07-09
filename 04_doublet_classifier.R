#nrow##load packages and set ggplot theme

library(tidyverse)
library(data.table)
library(randomForest)
library(caret)


###load data

progenitor_labels <- fread("../data/progenitors.csv") 
doublet_labels <- fread("../data/doublets.csv") 
inlier_labels <- fread("../data/inliers.csv") 
df <- fread("../data/01_combined_standardised.csv") 

print("successfully read in data")

print(dim(df))


#Add classifier labels to training data and separate out training and test data

#add Metadata_label, 3 classes = progenitor, doublet, inlier. based on presence in df or not
#ensure metadata_label is of class factor so that randomforest is classifier not regression
df <- df %>%
  mutate(Metadata_label = as.factor(case_when(Metadata_unique %in% progenitor_labels$Metadata_unique ~ "progenitor",
                                    Metadata_unique %in% doublet_labels$Metadata_unique ~ "doublet",
                                    Metadata_unique %in% inlier_labels$Metadata_unique ~ "inlier",
                                    TRUE ~ "test")))


prop.table(table(df$Metadata_label))

rm(list=c("inlier_labels", "doublet_labels", "progenitor_labels"))


m_dat <- df %>%
  select(starts_with("Metadata"))

#split off the known training set for training but keep in original file for predictions (since we want to predict further doublets from the inlier that arent in our ground truth)
train <- df %>%
  filter(Metadata_label != "test")



#remove all unnecessary metadata
train <- train %>%
  select(-starts_with("Metadata"), Metadata_label, Metadata_unique)
df <- df %>%
  select(-starts_with("Metadata"), Metadata_label, Metadata_unique)

#drop levels since factor levels still contains "test" even though this has been separated
train <- droplevels(train)

 #number of samples in smallest class
  nmin <- as.numeric(min(table(train$Metadata_label)))
  nclass <- length(names(table(train$Metadata_label)))
  
  #train random forest
  #classes are down sampled to match lowest class
  rf <- randomForest(Metadata_label ~ ., data=train[,-"Metadata_unique"], 
                   ntree=500,
                   mtry= 28,
                   #sample by strata i.e. by class
                   strata = train$Metadata_label, 
                   #sample size for each class (smallest class). rep for number of classes
                   sampsize = rep(nmin, nclass))

print("trained classifier")
  
  

# split dataset into chunks to avoid the RF long vector error
n <- 10
df <- split(df, factor(sort(rank(row.names(df))%%n)))


# apply trained rf to final dataset and get predictions
for(i in 1:n){
  df[[i]]$Metadata_prediction <- predict(rf, select(df[[i]], c(-Metadata_unique, -Metadata_label)))
}

# bind rows back together
df <- bind_rows(df)
 
 # mutate Metadata_prediction to label when label == "doublets" (so that incorrectly missed doublets are still removed)
 df <- df %>%
   mutate(Metadata_doublet= case_when(Metadata_label == "doublet" ~ "doublet", TRUE ~ as.character(Metadata_prediction)))

print("predicted for dataset")

# remove feature columns from the dataset
df <- df %>%
  select(starts_with("Metadata"))

df <- merge(df, m_dat)

rm(list=c("rf", "train", "m_dat"))
  
fwrite(df, "../data/04_standardised_predictions.csv")



