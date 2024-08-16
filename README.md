# sc_morph_tracking
The script for article "Single-cell morphological tracking of cell states to identify small-molecule modulators of liver differentiation".

This was broken down into small chunks given the size of the single cell data at the start and was run on a supercomputer

**01_subset.R**
- format and merge timecourse and drug screening data

**02_umap_subset.R**
- Run PCA and UMAP on the combined dataframes to inspect single cell data for outliers

**04_doublet_classifier.R**
- Inspection of UMAP shows cluster of single cells that upon identifying in the images using X,Y coordinates appear to be "doublets" (where two nuclei have been segmented as one)
- Aggregation removes this issue but to use the single cell data these doublets ideally need to be removed from the dataset
- No single feature appears to delineate them well enough so a classifier could be trained to label them based on all features
- Train classifier to label doublets and write out a list of cells to be removed from the dataset

**05_umap_predictions.R**
- inspection of cell labels in UMAP space

**06_remove_doublets_umap.R**
- go back to the unnormalised data and remove the cells predicted to be doublets
- repeat standardisation and UMAP visualisation

**07_outlier_rm.R**
- one cluster of a handful of extreme outlier cells removed

**08_umap.R**
- finally repeat UMAP on cleaned dataset

**09_explore.R**
- create all the plots for publication

**10_slingshot.R**
- generate clusters for slingshot trajectories

**11_slingshot_analysis.R**
- run slingshot on timecourse data
- then add in drug data to get pseudotime values along the timecourse trajectories


