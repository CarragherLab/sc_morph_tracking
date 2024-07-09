#load packages and set ggplot theme
library(tidyverse)
library(data.table)
library(ggpointdensity)
library(cowplot)
library(plotly)


theme_set(theme_classic()+ theme(panel.border = element_blank(),
                                 axis.text = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.title = element_blank(),
                                 axis.line = element_blank(),
                                 plot.title = element_text(hjust = 0.5, size =20)))

###load data
df <- fread("data/08_cleaned_umap.csv") 

df<- df %>%
  mutate(Metadata_day=case_when(Metadata_plate == "pseudo-01" ~ 1,
                                Metadata_plate == "pseudo-02" ~ 2,
                                Metadata_plate == "pseudo-03" ~ 3,
                                Metadata_plate == "pseudo-04" ~ 4,
                                Metadata_plate == "pseudo-05" ~ 7,
                                Metadata_plate == "pseudo-06" ~ 8,
                                Metadata_plate == "pseudo-07" ~ 9,
                                Metadata_plate == "pseudo-08" ~ 10,
                                Metadata_plate == "pseudo-09" ~ 11,
                                Metadata_plate == "pseudo-10" ~ 14,
                                Metadata_plate == "pseudo-11" ~ 14,
                                Metadata_plate == "pseudo-12" ~ 14,
                                Metadata_plate == "pseudo-13" ~ 15,
                                Metadata_plate == "pseudo-14" ~ 16,
                                Metadata_plate == "pseudo-15" ~ 17,
                                Metadata_plate == "pseudo-16" ~ 18,
                                Metadata_plate == "pseudo-17" ~ 21,
                                Metadata_plate == "pseudo-18" ~ 23,
                                Metadata_plate == "pseudo-19" ~ 25,
                                Metadata_plate == "pseudo-20" ~ 28))

table(df$Metadata_day,df$Metadata_plate)

timecourse <- df %>%
  filter(Metadata_library=="timecourse")

#timecourse$Metadata_plate <- as.factor(timecourse$Metadata_plate)

timecourse <- timecourse %>%
  mutate_at("Metadata_plate", as.factor)
  
timecourse <- timecourse[order(timecourse$Metadata_plate),]

timecourse_sub <- timecourse %>%
  group_by(Metadata_plate)%>%
  slice_sample(n=2000)



#plot timecourse
p1 <- ggplot(timecourse_sub, aes(X1, X2))+ 
  geom_point(aes(colour = Metadata_day))+
  scale_color_viridis_c(labels=NULL)+
  labs(colour="")

ggsave("outputs/09_timecourse_umap.png", p1)


p2 <- ggplot()+ 
  geom_point(data=timecourse_sub, aes(x=X1, y=X2, colour = Metadata_mito_intensity))+
  scale_color_gradient(low = "#e0e0e0", high = "#b2182b", labels= NULL)+
  ggtitle("Mitochondrial Intensity")+
  labs(colour="")

ggsave("outputs/09_timecourse_umap_by_mito.png", p2, height=5, width=5)


p4 <- ggplot()+ 
  geom_point(data=timecourse_sub, aes(x=X1, y=X2, colour = Metadata_nuclear_area))+
  scale_color_gradient(low = "#e0e0e0", high = "#b2182b", label = NULL)+
  ggtitle("Nuclear Area")+
  labs(colour="")

ggsave("outputs/09_timecourse_umap_by_nuclear_area.png", p4, height=5, width=5)

p5 <- ggplot()+ 
  geom_point(data=timecourse_sub, aes(x=X1, y=X2, colour = Metadata_cell_area))+
  scale_color_gradient(low = "#e0e0e0", high = "#b2182b", label = NULL)+
  ggtitle("Cell Area")+
  labs(colour="")
ggsave("outputs/09_timecourse_umap_by_cell_area.png", p5, height=5, width=5)



#plot progenitors from timecourse and drug screen together

progenitors <- df %>%
  filter(Metadata_compound == "Progenitor"| Metadata_plate == "pseudo-01") %>%
  mutate(Metadata_dataset= case_when(Metadata_library == "timecourse" ~ "Timecourse progenitors",
                                     TRUE ~ "Screen progenitors"))

p6 <- ggplot(data=progenitors, aes(x=X1, y=X2, colour= Metadata_dataset))+ 
  geom_point(alpha=0.6)+
  scale_color_manual(values = c("brown", "azure4"))+
  theme(legend.position = "top", legend.title = element_text(""))

ggsave("outputs/09_progenitor_overlap.png", p6, height=5, width=5)



#list of drugs (minus timecourse)
drugs <- unique(df$Metadata_compound)[c(-1,-3, -4, -12)]
controls <- c("Progenitor", "DMSO", "Diff_Media")

for(i in drugs){
  p5 <- df %>%
    filter(Metadata_compound == i)%>%
    mutate(Metadata_concentration= factor(Metadata_concentration,levels = c("1","3","10")))%>%
    group_split(Metadata_concentration) %>%
    map(~ ggplot(.,aes(x=X1, y=X2))+ 
    geom_point(data=timecourse_sub[,-11], aes(x=X1, y=X2), colour="azure4", alpha=0.6)+
    geom_pointdensity()+
    geom_density_2d( colour = "black")+
    scale_color_viridis_c()+
    labs(colour=paste0(i," Density"))+
    theme(legend.position="none")+
    theme(strip.background = element_blank())+
    theme(strip.text.x = element_text(size = 20))) %>%
    plot_grid(plotlist = ., ncol=3)
    ggsave(paste0("outputs/09_", i, "_umap.png"), p5, height = 4, width =11)
}

for(i in controls){
  drug_df <- df %>%
    filter(Metadata_compound == i)
  p5 <- ggplot()+ 
    geom_point(data=timecourse_sub[,-11], aes(x=X1, y=X2), colour="azure4", alpha=0.6)+
    geom_pointdensity(data=drug_df, aes(x=X1, y=X2))+
    geom_density_2d(data=drug_df, aes(x=X1, y=X2), colour = "black")+
    scale_color_viridis_c()+
    labs(colour=paste0(i," Density"))+
    ggtitle(i)+
    theme(legend.position="none")
  ggsave(paste0("outputs/09_", i, "_umap.png"), p5, height=5, width = 5)
}










timecourse_trellis <- timecourse %>%
  filter(Metadata_plate %in% 
           c("pseudo-01", "pseudo-04", "pseudo-08", "pseudo-13", "pseudo-17", "pseudo-20"))


timecourse_trellis <- timecourse_trellis %>% mutate(Metadata_day_label=factor(case_when(Metadata_plate == "pseudo-01" ~ "Day 1",
                                                      Metadata_plate == "pseudo-04" ~ "Day 4",
                                                      Metadata_plate == "pseudo-08" ~ "Day 10",
                                                      Metadata_plate == "pseudo-13" ~ "Day 15",
                                                      Metadata_plate == "pseudo-17" ~ "Day 21",
                                                      Metadata_plate == "pseudo-20" ~ "Day 28"),
                                                      levels = c("Day 1", "Day 4", "Day 10", "Day 15", "Day 21", "Day 28")))

p6 <- ggplot(timecourse_trellis, aes(X1, X2))+ 
  geom_pointdensity( size =0.75)+
  geom_density_2d(aes(x=X1, y=X2), colour = "black")+
  facet_wrap("Metadata_day_label")+
  scale_color_viridis_c(labels=NULL)+
  labs(colour="")+
  theme(strip.background = element_blank())+
  theme(strip.text.x = element_text(size = 20))

p6
ggsave("outputs/09_timecourse_umap_facet_wrap.png", p6, height=8, width=11)


