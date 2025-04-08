rm(list=ls())
#library("MetBrewer")
library(tidyverse)
library(readxl)
library(rstan)
library(brms)
library(patchwork)
library(ghibli)
library(ggplot2)
library(dplyr)
library(viridisLite) #palette for colour blindness

Data_Biomass <- read.csv("~/Desktop/Honours/Data_Biomass.csv",header=T)
Data_Biomass_Tank <- read.csv("~/Desktop/Honours/Data_Biomass_Tank.csv",header=T)

#Models
load("~/Desktop/Honours/Tank_Ratio_Mod_i.Rdata") 
load("~/Desktop/Honours/Tank_Ratio_Mod_i.Rdata")

#Data Sets
Field_Ratio_Mod_i_CE_df <- read.csv("~/Desktop/Honours/Field_Ratio_Mod_i_CE_df.csv")
Field_Ratio_Mod_g_CE_df <- read.csv("~/Desktop/Honours/Field_Ratio_Mod_g_CE_df.csv")
Tank_Ratio_Mod_i_CE_df <- read.csv("~/Desktop/Honours/Tank_Ratio_Mod_i_CE_df.csv")
Tank_Ratio_Mod_g_CE_df <- read.csv("~/Desktop/Honours/Tank_Ratio_Mod_g_CE_df.csv")


#Making a new column for ratio 
Data_Biomass_Ratio <- Data_Biomass %>% mutate(Field_Ratio = (Aboveground_Dry/Belowground_Dry))

Data_Biomass_Tank_Ratio <- Data_Biomass_Tank %>% mutate(Tank_Ratio = (Aboveground_Dry/Belowground_Dry))


#Field ----------------

#Separate the data
Data_Biomass_Ratio_i <- Data_Biomass_Ratio %>% filter(`Planting_Method` == "Individual") #separating the indv. and group data 
Data_Biomass_Ratio_g <- Data_Biomass_Ratio %>% filter(`Planting_Method` == "Group")


#Making the model 
Field_Ratio_Mod <- brm(Field_Ratio ~ Source_Marsh*Planting_Method, data = Data_Biomass_Ratio, family = lognormal())
# pp_check(Field_Ratio_Mod)
# conditional_effects(Field_Ratio_Mod)


Field_Ratio_Mod_i <- brm(Field_Ratio ~ Source_Marsh, data = Data_Biomass_Ratio_i, family= lognormal())
pp_check(Field_Ratio_Mod_i)
conditional_effects(Field_Ratio_Mod_i)


#Field_Ratio_Mod_g <- brm(Field_Ratio ~ Source_Marsh, data = Data_Biomass_Ratio_g, family= lognormal()) 
pp_check(Field_Ratio_Mod_g)
conditional_effects(Field_Ratio_Mod_g)

#Making the plot - individual
summary(Field_Ratio_Mod_i)
Field_Ratio_Mod_i_CE <- conditional_effects(Field_Ratio_Mod_i,effects="Source_Marsh",re_formula=NA,method="fitted") 
Field_Ratio_Mod_i_CE_df <- as.data.frame(Field_Ratio_Mod_i_CE$`Source_Marsh`)

#head(Field_Ratio_Mod_i_CE_df)
#write.csv(Field_Ratio_Mod_i_CE_df, "~/Desktop/Honours/Field_Ratio_Mod_i_CE_df.csv") #will make an excel file 

Field_Ratio_i <- ggplot()+
  geom_hline (yintercept = 1, linetype = "longdash")+
  geom_point(data=Data_Biomass_Ratio_i, aes(x=Source_Marsh,
                                      y=Field_Ratio,
                                      group = Source_Marsh,
                                      colour = Source_Marsh),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=Field_Ratio_Mod_i_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                     y=estimate__,
                                                     group = Source_Marsh,
                                                     colour = Source_Marsh),size=4)+
  geom_errorbar(data=Field_Ratio_Mod_i_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                        ymin=lower__,
                                                        ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw(base_size=12)+
  labs(y="Biomass Ratio",
       x="Field Site",
       title="Field Biomass Ratio - Individual", 
       subtitle = "(a)",
       colour = "Field Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  ylim(0,4)+
  theme(axis.text.x=element_text(angle=45,vjust=0.5), 
        legend.position="bottom")

Field_Ratio_i

#Making the plot - group
summary(Field_Ratio_Mod_g)
Field_Ratio_Mod_g_CE <- conditional_effects(Field_Ratio_Mod_g,effects="Source_Marsh",re_formula=NA,method="fitted") 
Field_Ratio_Mod_g_CE_df <- as.data.frame(Field_Ratio_Mod_g_CE$`Source_Marsh`)
head(Field_Ratio_Mod_g_CE_df)

#write.csv(Field_Ratio_Mod_g_CE_df, "~/Desktop/Honours/Field_Ratio_Mod_g_CE_df.csv") #will make an excel file 

Field_Ratio_g <- ggplot()+
  geom_hline (yintercept = 1, linetype = "longdash")+
  geom_point(data=Data_Biomass_Ratio_g, aes(x=Source_Marsh,
                                            y=Field_Ratio,
                                            group = Source_Marsh,
                                            colour = Source_Marsh),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=Field_Ratio_Mod_g_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                           y=estimate__,
                                                           group = Source_Marsh,
                                                           colour = Source_Marsh),size=4)+
  geom_errorbar(data=Field_Ratio_Mod_g_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                              ymin=lower__,
                                                              ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw(base_size=12)+
  labs(y="Biomass Ratio",
       x="Field Site",
       title="Field Biomass Ratio - Group", 
       subtitle = "(b)",
       colour = "Field Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  ylim(0,4)+
  theme(axis.text.x=element_text(angle=45,vjust=0.5), 
        legend.position="bottom")

Field_Ratio_g


#Tank ----------------

#Separate the data
Data_Biomass_Tank_i <- Data_Biomass_Tank_Ratio %>% filter(`Planting_Method` == "Individual") %>% 
  mutate(Tank = fct_recode(Treatment, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber"))  
head(Data_Biomass_Tank_i)

Data_Biomass_Tank_g <- Data_Biomass_Tank_Ratio %>% filter(`Planting_Method` == "Group") %>% 
  mutate(Tank = fct_recode(Treatment, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber")) 

Data_Tank_Ratio_i <- Data_Biomass_Tank_i %>% filter(`Planting_Method` == "Individual") #separating the indv. and group data 
Data_Tank_Ratio_g <- Data_Biomass_Tank_g %>% filter(`Planting_Method` == "Group")


#Making the model 

Tank_Ratio_Mod_i <- brm(Tank_Ratio ~ Tank, data = Data_Tank_Ratio_i, family= lognormal())
pp_check(Tank_Ratio_Mod_i)
conditional_effects(Tank_Ratio_Mod_i)
#save(Tank_Ratio_Mod_i, file="~/Desktop/Honours/Tank_Ratio_Mod_i.Rdata")
load("~/Desktop/Honours/Tank_Ratio_Mod_i.Rdata") 

Tank_Ratio_Mod_g <- brm(Tank_Ratio ~ Tank, data = Data_Tank_Ratio_g, family= lognormal()) 
pp_check(Tank_Ratio_Mod_g)
conditional_effects(Tank_Ratio_Mod_g)
#save(Tank_Ratio_Mod_g, file="~/Desktop/Honours/Tank_Ratio_Mod_g.Rdata")
load("~/Desktop/Honours/Tank_Ratio_Mod_i.Rdata")



#Making the plot - individual
summary(Tank_Ratio_Mod_i)
Tank_Ratio_Mod_i_CE <- conditional_effects(Tank_Ratio_Mod_i,effects="Tank",re_formula=NA,method="fitted") 
Tank_Ratio_Mod_i_CE_df <- as.data.frame(Tank_Ratio_Mod_i_CE$`Tank`)

#head(Tank_Ratio_Mod_i_CE_df)
#write.csv(Tank_Ratio_Mod_i_CE_df, "~/Desktop/Honours/Tank_Ratio_Mod_i_CE_df.csv") #will make an excel file 

Tank_Ratio_i <- ggplot()+
  geom_hline (yintercept = 1, linetype = "longdash")+
  geom_point(data=Data_Tank_Ratio_i, aes(x=Tank,
                                            y=Tank_Ratio,
                                            group = Tank,
                                            colour = Tank),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=Tank_Ratio_Mod_i_CE$`Tank`, aes(x=Tank,
                                                           y=estimate__,
                                                           group = Tank,
                                                           colour = Tank),size=4)+
  geom_errorbar(data=Tank_Ratio_Mod_i_CE$`Tank`, aes(x=Tank,
                                                              ymin=lower__,
                                                              ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  theme_bw(base_size=12)+
  labs(y="Biomass Ratio",
       x="Experimental Substrate",
       title="Tank Biomass Ratio - Individual", 
       subtitle = "(a)",
       colour = "Experimental Substrate") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  ylim(0,4)+
  theme(axis.text.x=element_text(angle=45,vjust=0.5), 
        legend.position="bottom")

Tank_Ratio_i


#Making the plot - group
summary(Tank_Ratio_Mod_g)
Tank_Ratio_Mod_g_CE <- conditional_effects(Tank_Ratio_Mod_g,effects="Tank",re_formula=NA,method="fitted") 
Tank_Ratio_Mod_g_CE_df <- as.data.frame(Tank_Ratio_Mod_g_CE$`Tank`)

#head(Tank_Ratio_Mod_g_CE_df)
#write.csv(Tank_Ratio_Mod_g_CE_df, "~/Desktop/Honours/Tank_Ratio_Mod_g_CE_df.csv") #will make an excel file 

Tank_Ratio_g <- ggplot()+
  geom_hline (yintercept = 1, linetype = "longdash")+
  geom_point(data=Data_Tank_Ratio_g, aes(x=Tank,
                                         y=Tank_Ratio,
                                         group = Tank,
                                         colour = Tank),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=Tank_Ratio_Mod_g_CE$`Tank`, aes(x=Tank,
                                                       y=estimate__,
                                                       group = Tank,
                                                       colour = Tank),size=4)+
  geom_errorbar(data=Tank_Ratio_Mod_g_CE$`Tank`, aes(x=Tank,
                                                          ymin=lower__,
                                                          ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  theme_bw(base_size=12)+
  labs(y="Biomass Ratio",
       x="Experimental Substrate",
       title="Tank Biomass Ratio - Group", 
       subtitle = "(b)",
       colour = "Experimental Substrate") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  ylim(0,4)+
  theme(axis.text.x=element_text(angle=45,vjust=0.5), 
        legend.position= "bottom")

Tank_Ratio_g

#Figure Loading _______________

(Field_Ratio_i + (theme(legend.position = "none") ) + Field_Ratio_g + (theme(legend.position = "none")) ) / legend_frb

(Tank_Ratio_i + (theme(legend.position = "none") ) + Tank_Ratio_g + (theme(legend.position = "none")) ) / legend_trb


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_frb <- g_legend(Field_Ratio_i)
legend_trb <- g_legend(Tank_Ratio_i)





