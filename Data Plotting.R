rm(list=ls())
library(tidyverse)
library(readxl)
library(rstan)
library(brms)
library(patchwork)
library(ggplot2)
library(dplyr)
library(viridisLite) #palette for colour blindness


#Data sets
Tank_Individuals <- read.csv("~/Desktop/Honours/Tank_Individuals.csv",header=T) 
Tank_Groups <- read.csv("~/Desktop/Honours/Tank_Groups.csv",header=T)
Data_Biomass <- read.csv("~/Desktop/Honours/Data_Biomass.csv",header=T)
Data_Quadrat_Height <- read.csv("~/Desktop/Honours/Data_Quadrat_Height.csv",header=T) 
Data_Biomass_Tank <- read.csv("~/Desktop/Honours/Data_Biomass_Tank.csv",header=T)
Data_Tank_Output <- read.csv("~/Desktop/Honours/Data_Tank_Output.csv",header=T)

#Models
load("~/Desktop/Honours/Tank_Height_Mod_Individuals.Rdata")
load("~/Desktop/Honours/Tank_Height_Mod_Groups.Rdata")
load("~/Desktop/Honours/Quadrat_Mod_Height.Rdata")


load("~/Desktop/Honours/A_Biomass_Mod_i.Rdata") 
load("~/Desktop/Honours/A_Biomass_Mod_g.Rdata") 

load("~/Desktop/Honours/B_Biomass_Mod_i.Rdata") 
load("~/Desktop/Honours/B_Biomass_Mod_g.Rdata")

load("~/Desktop/Honours/A_Biomass_Tank_Mod_i.Rdata")
load("~/Desktop/Honours/A_Biomass_Tank_Mod_g.Rdata")

load("~/Desktop/Honours/B_Biomass_Tank_Mod_i.Rdata") 
load("~/Desktop/Honours/B_Biomass_Tank_Mod_g.Rdata") 

load("~/Desktop/Honours/Tank_Tiller_Output_Mod.Rdata")
load("~/Desktop/Honours/Tank_Inflo_Output_Mod.Rdata")

#Print plots quickly:

Quadrat_Height_Fig

Tank_Individual_Fig

Tank_Group_Fig


(A_Quadrat_Biomass_Fig_i + theme(legend.position = "none") + A_Quadrat_Biomass_Fig_g)/ 
  (B_Quadrat_Biomass_Fig_i + B_Quadrat_Biomass_Fig_g) / legend_fb

(A_Tank_Biomass_Fig_i + A_Tank_Biomass_Fig_g)/ (B_Tank_Biomass_Fig_i + B_Tank_Biomass_Fig_g) / legend_tb


Tiller_Output_Fig + Inflo_Output_Fig 

#_______________________
#Legends 

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_fb <- g_legend(A_Quadrat_Biomass_Fig_i)
legend_tb <- g_legend(A_Tank_Biomass_Fig_i)



#Colour Info:
#4 different palettes:
  #A = viridis
  #B = magma
  #C = plasma 
  #D = inferno 
#For the line:
  #scale_colour_viridis_d( "data", option = "A", "B", "C", or "D")
#For the error margin:
  #scale_fill_viridis_d( "data", option = "A", "B", "C", or "D")
#-------------------------------------------------------------------------------
#Quadrat Tiller Heights (linear)

#summary(Quadrat_Mod_Height)
Quadrat_Height_Fitted <- cbind(Quadrat_Mod_Height$data,
                                        fitted(Quadrat_Mod_Height,
                                               re_formula=NA)) %>%
  as_tibble() %>%
  inner_join(Data_Quadrat_Height %>% distinct(Day,Tiller_Height,Survey_Site))
#head(Quadrat_Height_Fitted)

Quadrat_Height_Fig <- ggplot()+
  # geom_point(data=Quadrat_Height_Fitted,
  # aes(x=Day,y=Tiller_Height,colour=Survey_Site),
  # alpha=0.5,
  # position=position_jitter(width=0.25,height=0.25)) +
  geom_line(data=Quadrat_Height_Fitted,
            aes(x=Day,y=Estimate,colour=Survey_Site,group=Survey_Site)) +
  geom_ribbon(data=Quadrat_Height_Fitted,
              aes(x=Day,ymin=Q2.5,ymax=Q97.5,fill=Survey_Site,group=Survey_Site),
              alpha=0.3) +
  theme_bw(base_size=12) +
  theme(legend.position = "right",
        legend.justification = "center",
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text (size = 12)) +
  labs(y = "Tiller Height (cm)",
       title = "Field Tiller Height",
       colour = "Survey Site",
       fill = "Survey Site",
       subtitle = "(a)") +
  scale_color_viridis_d(labels = c("DeSable River",
                                   "North River",
                                   "Tryon River",
                                   "Westmoreland River")) +
  scale_fill_viridis_d(labels = c("DeSable River",
                                  "North River",
                                  "Tryon River",
                                  "Westmoreland River"))

Quadrat_Height_Fig

#-------------------------------------------------------------------------------
#Tank Individual Tiller Heights (linear) --> this is the example 

#Plotting tank individual height
summary(Tank_Height_Mod_Individuals)
Tank_Height_Individuals_Fitted <- cbind(Tank_Height_Mod_Individuals$data,
                                        fitted(Tank_Height_Mod_Individuals,
                                               re_formula=NA)) %>% 
  as_tibble() %>%
  inner_join(Tank_Individuals %>% distinct(Day,Tiller_Height,Tank)) %>% 
  mutate(Tank = fct_recode(Tank, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber")) 

#head(Tank_Height_Individuals_Fitted)

Tank_Height_Individuals_Fitted %>% select(Tank) %>% distinct()


Tank_Individual_Fig <- ggplot()+ #+ in ggplot is the pipe for this version
#geom_point(data=Tank_Height_Individuals_Fitted,
  #aes(x=Day,y=Tiller_Height,colour=Tank),
  #alpha=0.5,
  #position=position_jitter(width=0.25,height=0.25))+ #puts in the points - I don't want them here since they're messy
  geom_line(data=Tank_Height_Individuals_Fitted,
          aes(x=Day,
              y=Estimate,
              colour=Tank,
              group=Tank)) + #making the lines
  geom_ribbon(data=Tank_Height_Individuals_Fitted,
              aes(x=Day,
                  ymin=Q2.5,
                  ymax=Q97.5,
                  fill=Tank,
                  group=Tank), alpha=0.3) + #making the error thing
  theme_bw(base_size=12)+ #changes background aesthetic and outline, size changes text size
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) + #removes legend for side-by-side plot
  labs(y="Tiller Height (cm)", 
       title="Tank Tiller Height - Individual",
       subtitle = "(a)")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_viridis_d(option = "C")+
  scale_fill_viridis_d(option = "C")

Tank_Individual_Fig


#Tank Group Tiller Heights 
Tank_Height_Group_Fitted <- cbind(Tank_Height_Mod_Groups$data,
                                        fitted(Tank_Height_Mod_Groups,
                                               re_formula=NA)) %>% 
as_tibble() %>%
  inner_join(Tank_Groups %>% distinct(Day,Tiller_Height,Tank))  %>% 
  mutate(Tank = fct_recode(Tank,
                           "Coir Fiber" = "Coir Log",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush Mat"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber")) 

head(Tank_Height_Group_Fitted)  

Tank_Height_Group_Fitted %>% select(Tank) %>% distinct()

#making the plot
Tank_Group_Fig <- ggplot()+
  #geom_point(data=Tank_Height_Group_Fitted,
  #aes(x=Day,y=Tiller_Height,colour=Tank),
  #alpha=0.5,
  #position=position_jitter(width=0.25,height=0.25)) +   #puts in the points
  geom_line(data=Tank_Height_Group_Fitted,
            aes(x=Day,
                y=Estimate,
                colour=Tank,
                group=Tank))+ #making the lines
  geom_ribbon(data=Tank_Height_Group_Fitted,
              aes(x=Day,
                  ymin=Q2.5,
                  ymax=Q97.5,
                  fill=Tank,
                  group=Tank), alpha=0.3)+ #making the error thing
  theme_bw(base_size=12)+ #changes background aesthetic and outline, size changes text size
  labs(y="Tiller Height (cm)",
       title="Tank Tiller Height - Group",
       colour = "Experimental Substrate",
       fill = "Experimental Substrate",
       subtitle = "(a)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        legend.title = element_text(size = 12),
        legend.position = "bottom",
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.subtitle = element_text(size = 12))+ #moving title to the middle
  scale_colour_viridis_d(option = "C")+
  scale_fill_viridis_d(option = "C")

Tank_Group_Fig


#-------------------------------------------------------------------------------
#Individual Quadrat Biomass (categorical)

#summary(A_Biomass_Mod_i)
A_Biomass_i_CE <- conditional_effects(A_Biomass_Mod_i,effects="Source_Marsh",re_formula=NA,method="fitted") 
A_Biomass_i_CE_df <- as.data.frame(A_Biomass_i_CE$`Source_Marsh`)

#head(A_Biomass_i_CE_df)
#View(A_Biomass_i_CE)
#write.csv(A_Biomass_i_CE_df, "~/Desktop/Honours/A_Biomass_i_CE_df.csv") #will make an excel file 

A_Quadrat_Biomass_Fig_i <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Biomass_i, aes(x=Source_Marsh,
                                    y=Aboveground_Dry,
                                    group = Source_Marsh,
                                    colour = Source_Marsh),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
    
  geom_point(data=A_Biomass_i_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                   y=estimate__,
                                                   group = Source_Marsh,
                                                   colour = Source_Marsh),size=4)+
  geom_errorbar(data=A_Biomass_i_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                      ymin=lower__,
                                                      ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw(base_size=12)+
  labs(y="Aboveground Dry \n Biomass (g)",
       x="",
       title="Field Aboveground \n Biomass - Individual", 
       subtitle = "(a)",
       colour = "Field Site") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  ylim(0,15)+
  scale_x_discrete(labels = c("DeSable" = "",
                              "North River" = "",
                              "Tryon" = "",
                              "Westmoreland" = "")) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5), 
        legend.position="bottom")

A_Quadrat_Biomass_Fig_i


#Plotting individual quadrat below biomass#Plotting inNULLdividual quadrat below biomass

#summary(B_Biomass_Mod_i)
B_Biomass_i_CE <- conditional_effects(B_Biomass_Mod_i,effects="Source_Marsh",re_formula=NA,method="fitted")
B_Biomass_i_CE_df <- as.data.frame(B_Biomass_i_CE$`Source_Marsh`)

#head(B_Biomass_i_CE_df)
#View(B_Biomass_i_CE)
#write.csv(B_Biomass_i_CE_df, "~/Desktop/Honours/B_Biomass_i_CE_df.csv") #will make an excel file 

B_Quadrat_Biomass_Fig_i <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Biomass_i, aes(x=Source_Marsh,
                                    y=Belowground_Dry,
                                    group = Source_Marsh,
                                    colour = Source_Marsh),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=B_Biomass_i_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                   y=estimate__,
                                                   group = Source_Marsh,
                                                   colour = Source_Marsh),size=4)+
  geom_errorbar(data=B_Biomass_i_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                      ymin=lower__,
                                                      ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  ylim(0,7.5)+
  theme_bw(base_size=12)+
  labs(y="Belowground Dry \n Biomass (g)",
       x="Source Marsh",
       title="Field Belowground \n Biomass - Individual",
       subtitle = "(c)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position="none")
B_Quadrat_Biomass_Fig_i

#-------------------------------------------------------------------------------
#Group Quadrat Biomass (categorical)

#summary(A_Biomass_Mod_g)
A_Biomass_g_CE <- conditional_effects(A_Biomass_Mod_g,effects="Source_Marsh",re_formula=NA,method="fitted")
A_Biomass_g_CE_df <- as.data.frame(A_Biomass_g_CE$`Source_Marsh`)

head(A_Biomass_g_CE_df)
#View(A_Biomass_g_CE)
#write.csv(A_Biomass_g_CE_df, "~/Desktop/Honours/A_Biomass_g_CE_df.csv") #will make an excel file 

A_Quadrat_Biomass_Fig_g <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Biomass_g, aes(x=Source_Marsh,
                                    y=Aboveground_Dry,
                                    group = Source_Marsh,
                                    colour = Source_Marsh),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=A_Biomass_g_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                     y=estimate__,
                                                     group = Source_Marsh,
                                                     colour = Source_Marsh),size=4)+
  geom_errorbar(data=A_Biomass_g_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                        ymin=lower__,
                                                        ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  theme_bw(base_size=12)+
  ylim(0,17.5) +
  labs(y="",
       x="",title="Field Aboveground \n Biomass - Group",
       subtitle = "(b)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  scale_x_discrete(labels = c("DeSable" = "", 
                              "North River" = "", 
                              "Tryon" = "", 
                              "Westmoreland" = "")) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position="none")
A_Quadrat_Biomass_Fig_g

#Belowground

#summary(B_Biomass_Mod_g)
B_Biomass_g_CE <- conditional_effects(B_Biomass_Mod_g,effects="Source_Marsh",re_formula=NA,method="fitted")
B_Biomass_g_CE_df <- as.data.frame(B_Biomass_g_CE$`Source_Marsh`)

#head(B_Biomass_g_CE_df)
#View(B_Biomass_g_CE)
#write.csv(B_Biomass_g_CE_df, "~/Desktop/Honours/B_Biomass_g_CE_df.csv") #will make an excel file 

B_Quadrat_Biomass_Fig_g <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data = Data_Biomass_g, aes(x=Source_Marsh,
                                    y=Belowground_Dry,
                                    group = Source_Marsh,
                                    colour = Source_Marsh),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0))+
  
  geom_point(data=B_Biomass_g_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                     y=estimate__,
                                                     group = Source_Marsh,
                                                     colour = Source_Marsh),size=4)+
  geom_errorbar(data=B_Biomass_g_CE$`Source_Marsh`, aes(x=Source_Marsh,
                                                        ymin=lower__,
                                                        ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  ylim(0,350)+
  theme_bw(base_size=12)+
  labs(y="",
       x="Source Marsh",
       title="Field Belowground \n Biomass - Group",
       subtitle = "(d)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position="none")
B_Quadrat_Biomass_Fig_g

#-------------------------------------------------------------------------------
#Individual Tank Biomass (categorical)

summary(A_Biomass_Tank_Mod_i)
head(A_Biomass_Tank_Mod_i)
A_Biomass_Tank_i_CE <- conditional_effects(A_Biomass_Tank_Mod_i,effects="Treatment",re_formula=NA,method="fitted")
A_Biomass_Tank_i_CE_df <- as.data.frame(A_Biomass_Tank_i_CE$`Treatment`) %>% 
  mutate(Tank = fct_recode(Treatment, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber")) 
head(A_Biomass_Tank_i_CE_df)
#View(A_Biomass_Tank_i_CE_df)
#write.csv(A_Biomass_Tank_i_CE_df, "~/Desktop/Honours/A_Biomass_Tank_i_CE_df.csv") #will make an excel file 

A_Tank_Biomass_Fig_i <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Biomass_Tank_i, aes(x = Tank,
                                           y=Aboveground_Dry,
                                           group = Tank,
                                           colour = Tank),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=A_Biomass_Tank_i_CE_df, aes(x = Tank,
                                                       y=estimate__,
                                                       group = Tank,
                                                       colour = Tank),size=4)+
  geom_errorbar(data=A_Biomass_Tank_i_CE_df, aes(x = Tank,
                                                          ymin=lower__,
                                                          ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  ylim(0,15)+
  theme_bw(base_size=12)+
  labs(y="Aboveground Dry \n Biomass (g)",
       x="",
       title="Tank Aboveground \n Biomass - Individual",
       subtitle = "(a)",
       colour = "Experimental Substrate") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position="none")
A_Tank_Biomass_Fig_i


summary(B_Biomass_Tank_Mod_i)
head(B_Biomass_Tank_Mod_i)
B_Biomass_Tank_i_CE <- conditional_effects(B_Biomass_Tank_Mod_i,effects="Treatment",re_formula=NA,method="fitted")
B_Biomass_Tank_i_CE_df <- as.data.frame(B_Biomass_Tank_i_CE$`Treatment`) %>%
  mutate(Tank = fct_recode(Treatment, 
                         "Coir Fiber" = "Coir",
                         "Control" = "Control",
                         "Mixed" = "Mixed",
                         "Spruce Branch" = "Brush"),
       Tank = fct_relevel(Tank,
                          "Control", "Spruce Branch", "Mixed", "Coir Fiber"))

#head(B_Biomass_Tank_i_CE_df)
#View(B_Biomass_Tank_i_CE_df)
#write.csv(B_Biomass_Tank_i_CE_df, "~/Desktop/Honours/B_Biomass_Tank_i_CE_df.csv") #will make an excel file 

B_Tank_Biomass_Fig_i <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Biomass_Tank_i, aes(x = Tank,
                                         y= Belowground_Dry,
                                         group = Tank,
                                         colour = Tank),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=B_Biomass_Tank_i_CE_df, aes(x = Tank,
                                               y=estimate__,
                                               group = Tank,
                                               colour = Tank), size=4)+
  geom_errorbar(data=B_Biomass_Tank_i_CE_df, aes(x=Tank,
                                                  ymin=lower__,
                                                  ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  ylim(0,5)+
  theme_bw(base_size=12)+
  labs(y="Belowground Dry \n Biomass (g)",
       x="Treatment",
       title="Tank Belowground \n Biomass - Individual",
       subtitle = "(c)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position="none")
B_Tank_Biomass_Fig_i

#-------------------------------------------------------------------------------
#Group Tank Biomass

summary(A_Biomass_Tank_Mod_g)
head(A_Biomass_Tank_Mod_g)
A_Biomass_Tank_g_CE <- conditional_effects(A_Biomass_Tank_Mod_g,effects="Treatment",re_formula=NA,method="fitted")
A_Biomass_Tank_g_CE_df <- as.data.frame(A_Biomass_Tank_g_CE$`Treatment`) %>%
  mutate(Tank = fct_recode(Treatment, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber"))
#head(A_Biomass_Tank_g_CE_df)
#View(A_Biomass_Tank_g_CE_df)
#write.csv(A_Biomass_Tank_g_CE_df, "~/Desktop/Honours/A_Biomass_Tank_g_CE_df.csv") #will make an excel file 

A_Tank_Biomass_Fig_g <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Biomass_Tank_g, aes(x=Tank,
                                         y=Aboveground_Dry,
                                         group = Tank,
                                         colour = Tank),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=A_Biomass_Tank_g_CE_df, aes(x = Tank,
                                                       y=estimate__,
                                                       group = Tank,
                                                       colour = Tank),size=4)+
  geom_errorbar(data=A_Biomass_Tank_g_CE_df, aes(x = Tank,
                                                          ymin=lower__,
                                                          ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  ylim(0,15)+
  theme_bw(base_size=12)+
  labs(y="",
       x="",title="Tank Aboveground \n Biomass - Group",
       subtitle = "(b)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position="none")
A_Tank_Biomass_Fig_g


summary(B_Biomass_Tank_Mod_g)
head(B_Biomass_Tank_Mod_g)
B_Biomass_Tank_g_CE <- conditional_effects(B_Biomass_Tank_Mod_g,effects="Treatment",re_formula=NA,method="fitted")
B_Biomass_Tank_g_CE_df <- as.data.frame(B_Biomass_Tank_g_CE$`Treatment`) %>%
  mutate(Tank = fct_recode(Treatment, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber"))

#head(B_Biomass_Tank_g_CE_df)
#View(B_Biomass_Tank_g_CE_df)
#write.csv(B_Biomass_Tank_g_CE_df, "~/Desktop/Honours/B_Biomass_Tank_g_CE_df.csv") #will make an excel file 

B_Tank_Biomass_Fig_g <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Biomass_Tank_g, aes(x = Tank,
                                         y= Belowground_Dry,
                                         group = Tank,
                                         colour = Tank),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+
  
  geom_point(data=B_Biomass_Tank_g_CE_df, aes(x = Tank,
                                              y=estimate__,
                                              group = Tank,
                                              colour = Tank),size=4)+
  geom_errorbar(data=B_Biomass_Tank_g_CE_df, aes(x= Tank,
                                                 ymin=lower__,
                                                 ymax=upper__),width=0.3)+ #width changes the width of the top and bottom lines
  scale_color_viridis_d(option = "C") +
  scale_fill_viridis_d(option = "C") +
  #ylim(0,10)+
  theme_bw(base_size=12)+
  labs(y="",
       x="Treatment",
       title="Tank Belowground \nBiomass - Group",
       subtitle = "(d)") +
  theme(plot.title = element_text(hjust = 0.5, size = 12),  # Change plot title font size
        axis.title = element_text(size = 12),                # Change axis title font size
        axis.text = element_text(size = 12),                  # Change axis text font size
        legend.title = element_text(size = 12),               # Change legend title font size
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5),legend.position="none")
B_Tank_Biomass_Fig_g



#-------------------------------------------------------------------------------
#Tank Individual Output (categorical)

#Tank Tiller Output
summary(Data_Tank_Output)
head(Data_Tank_Output)
Data_Tank_Output_Fixed <- Data_Tank_Output %>% 
  mutate(Replicate = as.character(Replicate),
         Tank = fct_recode(Treatments, 
                                  "Coir Fiber" = "Coir",
                                  "Control" = "Control",
                                  "Mixed" = "Mixed",
                                  "Spruce Branch" = "Brush"),
                Tank = fct_relevel(Tank,
                                   "Control", "Spruce Branch", "Mixed", "Coir Fiber"))
head(Data_Tank_Output_Fixed)

Output_Tiller_CE <- conditional_effects(Tank_Tiller_Output_Mod,effects="Treatments:Planting_Method",re_formula=NA,method="fitted")
Output_Tiller_CE_df <- as.data.frame(Output_Tiller_CE$`Treatments:Planting_Method`) %>%
  mutate(Tank = fct_recode(Treatments, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber"))
head(Output_Tiller_CE_df)
#write.csv(Output_Tiller_CE_df, "~/Desktop/Honours/Output_Tiller_CE_df.csv")



Tiller_Output_Fig <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Tank_Output_Fixed, aes(x=Tank,
                                              y=Tillers,
                                              group=Planting_Method,
                                              colour=Planting_Method),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.25))+

  geom_point(data=Output_Tiller_CE_df, aes(x=Tank,
                                                                        y=estimate__,
                                                                        colour=Planting_Method),size=4)+
  geom_errorbar(data=Output_Tiller_CE_df, aes(x=Tank,
                                                                           ymin=lower__,
                                                                           ymax=upper__,
                                                                           group=Planting_Method,
                                                                           colour=Planting_Method),width=0.3)+
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  theme_bw(base_size=12) +
  labs(y= "Tillers",
       x="Experimental Substrate",
       title="Tank Tiller Output",
       colour = "Planting Method",
       fill = "Planting Method",
       subtitle = ("(a)")) +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) +
  theme(axis.text.x=element_text(angle=45,vjust=0.5), legend.position="none")

Tiller_Output_Fig


#Tank Inflorescence Output
#summary(Data_Tank_Output_Fixed)
Output_Inflo_CE <- conditional_effects(Tank_Inflo_Output_Mod,effects="Treatments:Planting_Method",re_formula=NA,method="fitted")
Output_Inflo_CE_df <- as.data.frame(Output_Inflo_CE$`Treatments:Planting_Method`) %>%
  mutate(Tank = fct_recode(Treatments, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber"))
  
head(Output_Inflo_CE_df)
#write.csv(Output_Inflo_CE_df, "~/Desktop/Honours/Output_Inflo_CE_df.csv")


Inflo_Output_Fig <- ggplot()+
  geom_hline (yintercept = 0, linetype = "longdash")+
  geom_point(data=Data_Tank_Output_Fixed, aes(x=Tank,
                                              y=Inflorescences,
                                              group=Planting_Method,
                                              colour=Planting_Method),
             size=2,
             alpha = 0.7,
             position=position_jitter(width=0.25,height=0.05))+
  
  geom_point(data=Output_Inflo_CE_df, aes(x= Tank,
                                          y=estimate__,
                                          colour=Planting_Method), size=4)+
  geom_errorbar(data=Output_Inflo_CE_df, aes(x= Tank,
                                             ymin=lower__,
                                             ymax=upper__,
                                             group=Planting_Method,
                                             colour=Planting_Method),width=0.3)+ 
  scale_colour_viridis_d(option = "D") +
  scale_fill_viridis_d(option = "D") +
  theme_bw(base_size=12) +
  ylim(0,10)+
  labs(y= "Inflorescences",
       x="Experimental Substrate",
       title="Tank Inflorescence Output",
       colour = "Planting Method",
       fill = "Planting Method",
       subtitle = ("(b)"))+
  theme(plot.title = element_text(hjust = 0.5, size = 12),  
        axis.title = element_text(size = 12),                
        axis.text = element_text(size = 12),                 
        legend.title = element_text(size = 12),             
        legend.text = element_text(size = 12),
        legend.position = "right") +
  theme(axis.text.x=element_text(angle=45,vjust=0.5))

Inflo_Output_Fig

