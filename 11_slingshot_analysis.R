

library(tidyverse)
library(data.table)
#library(ggridges)
library(viridis)
library(slingshot)
library(ggpubr)
#library(RColorBrewer)






df <- fread("../data/08_cleaned_umap.csv")
cl <- fread("../data/10_slingshot_clusters.csv")

df2 <- df %>%
  mutate(Metadata_compound=case_when(Metadata_compound == "ECF506" ~ "NXP900",
                                     TRUE ~ Metadata_compound ))



theme_set(theme_classic()+ theme(panel.border = element_blank(),
                                 plot.title = element_text(hjust = 0.5, size =20)))



timecourse_sub <- df2 %>%
  filter(Metadata_unique %in% cl$Metadata_unique)

#create cluster labels
cl <- cl %>%
  dplyr::select(Metadata_unique, cl)

#add cluster labels to dataframe
timecourse_sub <- merge(timecourse_sub, cl)


rd <- timecourse_sub %>%
  dplyr::select(X1,X2)

cl <- timecourse_sub%>%
  dplyr::select(cl)%>%
  unlist()


  
ggplot(timecourse_sub, aes(X1, X2))+ 
  geom_point(aes(colour = cl), alpha = 0.5)+
  #scale_colour_viridis_d(option="C")+
  labs(colour="cluster")


#get drugs

#did not include diff-media in pseudotime density plots

NXP900_df2 <- df2 %>%
  dplyr::filter(!Metadata_compound %in% c("timecourse") & Metadata_concentration %in% c("DMSO" , 10, "Progenitor", "Diff_Media")) %>%
  dplyr::select(X1, X2, Metadata_compound, Metadata_concentration, Metadata_plate)

NXP900_rd <- df2 %>%
  dplyr::filter(!Metadata_compound %in% c("timecourse") & Metadata_concentration %in% c("DMSO" , 10, "Progenitor", "Diff_Media")) %>%
  dplyr::select(X1, X2)

#rm(df2)

sce <- slingshot(as.matrix(rd), clusterLabels = cl, start.clus = "5", end.clus = c("2", "3"))

curves <- slingCurves(sce, as.df = TRUE)



p <- ggplot(timecourse_sub, aes(X1, X2))+ 
  geom_point(aes(colour = Metadata_plate), alpha = 0.5)+
  scale_colour_viridis_d(option="d")+
  theme(legend.position = "none")+
geom_path(data = curves %>% arrange(Order),
              aes(x=X1, y=X2,group = Lineage), size=1.5) +
  theme(axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.title = element_blank(),
                                 axis.line = element_blank())


ggsave("../outputs/11_slingshot_curves.png", p)


plate <- as.integer(as.factor(timecourse_sub$Metadata_plate))

plot(rd, col = viridis(20)[plate], asp = 1, pch = 16)+
lines(SlingshotDataSet(sce), lwd = 3, col = 'black')


plot(rd, col = viridis(5)[cl], asp = 1, pch = 16)+
lines(SlingshotDataSet(sce), type = "lineages", lwd = 3, col = 'black', show.constraints = TRUE)

pseudo1 <- as.data.frame(slingPseudotime(sce))

#get vector of pseudotime values, one per data point/cell 
#(average of pseudotime across all lineages for cells belonging to multiple lineages)
pseudo_avg <- pseudo1 %>%
  rowMeans(na.rm=TRUE) 

#create vector of colours baesd on pseudotime
colors <- viridis(100)
plotcol <- colors[cut(pseudo_avg, breaks=100)]

#png("../outputs/plots/05/slingshot_curves.png", type = "cairo")



plot(rd, col = plotcol, asp = 1, pch = 16)+
  lines(SlingshotDataSet(sce), lwd = 3, col = 'black')



pto <- SlingshotDataSet(sce) 

newPTO <- slingshot::predict(pto, NXP900_rd)

NXP900_sling <- slingPseudotime(newPTO)

#ecf_pseudo_max <- as.matrix(slingPseudotime(newPTO)) %>%
#  rowMaxs(na.rm = TRUE)

#newplotcol <- colors[cut(ecf_pseudo_max, breaks=100)]

lin_weights <- as.data.frame(slingCurveWeights(newPTO)) %>%
  dplyr::rename(lin1_weight=Lineage1, lin2_weight=Lineage2)

NXP900_pseudo <- cbind(NXP900_df2, slingPseudotime(newPTO), lin_weights)


NXP900_pseudo <- NXP900_pseudo %>%
  mutate(lineage = case_when(lin1_weight== 1 & lin2_weight < 1 & Lineage1>3  ~ "Hepatocyte",
                             lin2_weight== 1 & lin1_weight < 1 & Lineage1>3 ~ "Biliary",
                             TRUE ~ "Progenitor"))

#get proportion of cells belonging to each lineage (1, 1and2, 2)
NXP900_pseudo <- NXP900_pseudo %>%
  mutate(Lineage1_filtered = case_when(lin1_weight< 1  ~ NA,
                             TRUE ~ Lineage1))

NXP900_pseudo <- NXP900_pseudo %>%
  mutate(Lineage2_filtered = case_when(lin2_weight< 1  ~ NA,
                             TRUE ~ Lineage2))

ecf_only <- NXP900_pseudo %>%
  filter(Metadata_compound %in% c("Diff_Media", "DMSO", "NXP900", "Progenitor"))

p2 <- ggplot(ecf_only, aes(Lineage1_filtered, fill=Metadata_compound, colour = Metadata_compound))+
  geom_density(alpha=0.3)+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  xlab("Hepatocyte Lineage Pseudotime Value")+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(size =14))

ggsave("../outputs/11_hepatocyte_lineage_pseudotime_density.png",p2, width = 5, height = 3.5)



p3 <- ggplot(NXP900_pseudo, aes(Lineage2_filtered, fill=Metadata_compound, colour=Metadata_compound))+
  geom_density(alpha=0.3)+
  xlab("Biliary Lineage Pseudotime Value")+
  theme(legend.title = element_blank())

ggsave("../outputs/11_biliary_lineage_pseudotime_density.png",p3, width = 6, height = 4)



NXP900_pseudo <- NXP900_pseudo %>% 
  rowwise() %>%
  mutate(avg_pseudo =mean(c_across(c("Lineage1_filtered", "Lineage2_filtered")), na.rm=TRUE))


ggplot()+
  geom_point(data=timecourse_sub, aes(x=X1, y=X2), colour= "azure4")+
  geom_point(data=NXP900_pseudo %>% filter(Metadata_compound=="NXP900", Metadata_concentration ==10, lin1_weight==1),aes(x=X1, y=X2, colour=avg_pseudo))+
  scale_colour_viridis_c()+
  geom_path(data = curves %>% arrange(Order),
              aes(x=X1, y=X2, group = Lineage), size=1.5) 



table(NXP900_pseudo$lineage, NXP900_pseudo$Metadata_compound)

level_order <- c("Diff_Media", "Progenitor" , "Dasatinib", "NXP900", "PP1", "PP2", "M-344", "Oxamflatin", "Trichostatin-A", "PKC-412", "LY294002")

test <- NXP900_pseudo %>%
  filter(Metadata_compound != "DMSO") %>%
  group_by(Metadata_compound, lineage, Metadata_plate)%>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Metadata_compound, Metadata_plate) %>%
  mutate(perc = (count/sum(count)*100), Metadata_compound= factor(Metadata_compound, level=level_order)) 
  

compare_means(perc ~ Metadata_compound, data= test, group.by = "lineage", ref.group = "Diff_Media", method = "t.test")

lineage_summary <- test %>% group_by(Metadata_compound, lineage) %>%
  summarise(mean=mean(count), sd=sd(count)) %>%
  mutate(perc = mean/sum(mean)) 

drugs <- c("Progenitor", "Dasatinib", "NXP900", "PP1", "PP2", "M-344", "Oxamflatin", "Trichostatin-A", "PKC-412", "LY294002")

x <- NULL

for(i in drugs) {
  drug <- lineage_summary %>%
  filter(Metadata_compound == i) %>%
  select(mean)
  Diff_Media <- lineage_summary %>%
  filter(Metadata_compound == "Diff_Media") %>%
  select(perc)
  res <- chisq.test(drug$mean, p=Diff_Media$perc)
  x <- rbind(x, c(i, "Diff_Media", as.numeric(res$p.value)))
}

x <- as.data.frame(x)
x$V3 <- as.numeric(x$V3)

x <- dplyr::rename(x, group1 = V1, group2 = V2)

x$p.sym <- symnum(x$V3, corr = FALSE, na = FALSE, 
                 cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                 symbols = c("***", "**", "*", ".", " "))





p4 <- ggbarplot(test, x="Metadata_compound", y = "perc", add = "mean_sd", color= "lineage", fill="lineage", position = position_dodge(0.8), alpha = 0.6) +
  stat_pvalue_manual(x, y.position = 90, x = "group1",  label = "p.sym")+
  ylab("Percentage of Cells")+
  xlab("")+
  scale_colour_viridis_d()+
  scale_fill_viridis_d()+
  theme(axis.text.x = element_text(angle = 45, vjust=0.5))



ggsave("../outputs/11_lineage_proportions_barplot.png", p4, width =6, height=4)





ggplot(NXP900_pseudo, aes(Metadata_compound, fill = lineage)) +
geom_bar()
  #geom_jitter()+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 30)

ggplot(NXP900_pseudo, aes(Metadata_compound, Lineage2, colour = lineage)) +
  geom_boxplot()+
  #geom_jitter()+
  stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 30)





#plot NXP900 hepatocyte lineage only colour by pseudotime for lineage 1

plot(rd, col = 'grey', bg = 'grey', pch=21, asp = 1)+
points(slingReducedDim(newPTO), col = newplotcol, pch = 16)+
lines(SlingshotDataSet(sce), lwd=2, col = 'black')














