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
load("~/Desktop/Honours/A_Biomass_Mod.Rdata") 
load("~/Desktop/Honours/A_Biomass_Mod_i.Rdata") 
load("~/Desktop/Honours/A_Biomass_Mod_g.Rdata") 
load("~/Desktop/Honours/B_Biomass_Mod.Rdata") 
load("~/Desktop/Honours/B_Biomass_Mod_i.Rdata") 
load("~/Desktop/Honours/B_Biomass_Mod_g.Rdata")
load("~/Desktop/Honours/A_Biomass_Tank_Mod.Rdata")
load("~/Desktop/Honours/B_Biomass_Tank_Mod.Rdata")
load("~/Desktop/Honours/Tank_Tiller_Output_Mod.Rdata")
load("~/Desktop/Honours/Tank_Inflo_Output_Mod.Rdata")

#Print plots quickly:
print(Quadrat_Height_Fig)
print(Tank_Individual_Fig)
print(Tank_Group_Fig)


#Figures for paper:

(Quadrat_Height_Fig + theme(legend.position = "none"))/(Quadrat_Height_Slopes_Fig + theme(legend.position ="bottom")) #quadrat tiller height 

(Tank_Individual_Fig + theme(legend.position = "none"))/(Tank_Individual_Slopes_Fig + theme(legend.position ="bottom"))

(Tank_Group_Fig + theme(legend.position = "none"))/(Tank_Group_Slopes_Fig + theme(legend.position ="bottom")) 


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

#Quadrat Tiller Heights Slope (example) 
# Summarize the Quadrat_Height_Fig object to get an overview of the model
summary(Quadrat_Mod_Height)

# Convert model results into a data frame and randomly select 1000 draws (simulated values from the posterior distribution) from a total of 2000 possible draws
Quadrat_Height.p <- as_draws_df(Quadrat_Mod_Height,  subset = floor(runif(n = 1000, 1, max = 2000)))

# Check the first few rows of the sampled data
head(Quadrat_Height.p)
# View column names to understand what parameters are included
colnames(Quadrat_Height.p)

# Extract specific columns related to slopes at different survey sites and create new variables
Quadrat_Height.p_df <- Quadrat_Height.p %>%
  select(b_Day, `b_Day:Survey_SiteNorthRiver`, `b_Day:Survey_SiteTryon`, `b_Day:Survey_SiteWestmoreland`) %>%
  mutate( DeSable.p = `b_Day`,  # DeSable River slope is the base b_Day
          NorthRiver.p = (`b_Day` + `b_Day:Survey_SiteNorthRiver`),  # Add North River interaction
          Tryon.p = (`b_Day` + `b_Day:Survey_SiteTryon`),  # Add Tryon River interaction
          Westmoreland.p =(`b_Day` + `b_Day:Survey_SiteWestmoreland`)  # Add Westmoreland River interaction
  ) %>%
  # Remove original columns after creating new ones
  select(-c(b_Day, `b_Day:Survey_SiteNorthRiver`, `b_Day:Survey_SiteTryon`, `b_Day:Survey_SiteWestmoreland`))

# Check the new data frame with calculated slopes
head(Quadrat_Height.p_df)

# Calculate mean slope, 95% credible intervals, and add labels for each survey site
# DeSable River calculations
ds.p <-  Quadrat_Height.p_df %>% 
  mutate( `Survey Site`="DeSable", 
          slope = mean(DeSable.p),  # Mean slope
          slope_lower = quantile(DeSable.p, probs=0.025),  # Lower 2.5% quantile
          slope_upper = quantile(DeSable.p, probs=0.975))  %>%  # Upper 97.5% quantile
  dplyr::select(c(slope,slope_upper,slope_lower,`Survey Site`)) %>% distinct()  

# Repeat calculations for North River
nr.p <-  Quadrat_Height.p_df %>% 
  mutate(  `Survey Site`="North River", 
           slope = mean(NorthRiver.p),
           slope_lower = quantile(NorthRiver.p, probs=0.025),
           slope_upper = quantile(NorthRiver.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Survey Site`)) %>% distinct()  

# Repeat calculations for Tryon River
t.p <-  Quadrat_Height.p_df %>% 
  mutate( `Survey Site`="Tryon", 
          slope = mean(Tryon.p),
          slope_lower = quantile(Tryon.p, probs=0.025),
          slope_upper = quantile(Tryon.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Survey Site`)) %>% distinct()  

# Repeat calculations for Westmoreland River
wm.p <-  Quadrat_Height.p_df %>% 
  mutate(  `Survey Site`="Westmoreland", 
           slope = mean(Westmoreland.p),
           slope_lower = quantile(Westmoreland.p, probs=0.025),
           slope_upper = quantile(Westmoreland.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Survey Site`)) %>% distinct()  

# Combine all calculated data frames into one for plotting
slope_quadrat.p <- ds.p %>% bind_rows(nr.p, t.p, wm.p)

# View the combined results
slope_quadrat.p #this is the result for the paper 

#Plot the results

Quadrat_Height_Slopes_Fig <- ggplot(slope_quadrat.p, aes(x = `Survey Site`, y = slope, color=`Survey Site`)) +
  geom_point(size = 3) +  # Plot mean slope as points
  geom_errorbar(aes(ymin = slope_lower, ymax = slope_upper), width = 0.2) +  # Add credible intervals
  geom_hline(yintercept = 0, linetype = "dashed") +  # Add a horizontal line at zero for reference
  labs(title = "Field Posterior Estimates of Slopes",
       x = "Field Site",
       y = "Slope Estimate (cm/day)",
       subtitle = "(b)") +
  scale_color_viridis_d(name = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 12, hjust = 0.5),
        plot.subtitle = element_text (size = 12),
        legend.text = element_text(size = 12)) +
  guides(colour = guide_legend(nrow = 2))
  #theme_minimal()  # Clean plot appearance
# coord_flip()  # Optional: flip axes for better readability

Quadrat_Height_Slopes_Fig

#use patchwork to plot with corresponding regression (example) - or however you want to do it
(Quadrat_Height_Fig + theme(legend.position = "none"))/(Quadrat_Height_Slopes_Fig + theme(legend.position ="bottom"))


#-------------------------------------------------------------------------------

#Tiller Individual Slope Height

summary(Tank_Height_Mod_Individuals)
Tiller_Individual_Height.p <- as_draws_df(Tank_Height_Mod_Individuals,  subset = floor(runif(n = 1000, 1, max = 2000)))

head(Tiller_Individual_Height.p)
colnames(Tiller_Individual_Height.p)

# Extract specific columns related to slopes at different survey sites and create new variables
Tiller_Individual_Height.p_df <- Tiller_Individual_Height.p %>%
  select(b_Day, `b_Day:TankControl`, `b_Day:TankCoir`, `b_Day:TankMixed`) %>%
  mutate( Individual_Brush.p = `b_Day`,  # Mixed slope is the base 
          Individual_Control.p = (`b_Day` + `b_Day:TankControl`), 
          Individual_Mixed.p = (`b_Day` + `b_Day:TankMixed`),  
          Individual_Coir.p =(`b_Day` + `b_Day:TankCoir`) 
  ) %>%
  select(-c(b_Day, `b_Day:TankControl`, `b_Day:TankCoir`, `b_Day:TankMixed`)) #removing the original columns

# Check the new data frame with calculated slopes
head(Tiller_Individual_Height.p_df)

# Calculate mean slope, 95% credible intervals, and add labels for each tank
# Individual Control
ic.p <-  Tiller_Individual_Height.p_df %>% 
  mutate( `Tank`="Control", 
          slope = mean(Individual_Control.p),  # Mean slope
          slope_lower = quantile(Individual_Control.p, probs=0.025),  # Lower 2.5% quantile
          slope_upper = quantile(Individual_Control.p, probs=0.975))  %>%  # Upper 97.5% quantile
  dplyr::select(c(slope,slope_upper,slope_lower,`Tank`)) %>% distinct()  

# Individual Brush
ib.p <-  Tiller_Individual_Height.p_df %>% 
  mutate(  `Tank`="Spruce Branch", 
           slope = mean(Individual_Brush.p),
           slope_lower = quantile(Individual_Brush.p, probs=0.025),
           slope_upper = quantile(Individual_Brush.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Tank`)) %>% distinct()  

# Individual Mixed
im.p <-  Tiller_Individual_Height.p_df %>% 
  mutate( `Tank`="Mixed", 
          slope = mean(Individual_Mixed.p),
          slope_lower = quantile(Individual_Mixed.p, probs=0.025),
          slope_upper = quantile(Individual_Mixed.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Tank`)) %>% distinct()  

# Individual Coir
ico.p <-  Tiller_Individual_Height.p_df %>% 
  mutate(  `Tank`="Coir Fiber", 
           slope = mean(Individual_Coir.p),
           slope_lower = quantile(Individual_Coir.p, probs=0.025),
           slope_upper = quantile(Individual_Coir.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Tank`)) %>% distinct()  

# Combine all calculated data frames into one for plotting
slope_tank_i.p <- ic.p %>% bind_rows(ib.p, im.p, ico.p) %>% 
  mutate(Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber")) 

# View the combined results
slope_tank_i.p

#Plot the results

Tank_Individual_Slopes_Fig <- ggplot(slope_tank_i.p, aes(x = `Tank`, y = slope, color=`Tank`)) +
  geom_point(size = 3) +  # Plot mean slope as points
  geom_errorbar(aes(ymin = slope_lower, ymax = slope_upper), 
                width = 0.2) +  # Add credible intervals
  geom_hline(yintercept = 0, 
             linetype = "dashed") + # Add a horizontal line at zero for reference 
  labs(title = "Tank Posterior Estimates \n of Slopes - Individual",
       x = "Experimental Substrate",
       y = "Slope Estimate (cm/day)",
       subtitle = "(b)") +
  scale_color_viridis_d(option = "C",
                        name = NULL) +
  guides(colour = guide_legend(nrow = 2)) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), #tilt axis labels
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.subtitle = element_text(size = 12)) # move title to middle 
  #theme_classic(base_size=18)
  #theme_minimal()  # Clean plot appearance
#coord_flip()  # Optional: flip axes for better readability

Tank_Individual_Slopes_Fig

# extract legends
# Source: https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

legend_l <- g_legend(Tank_Individual_Slopes_Fig)

#use patchwork to plot with corresponding regression (example) - or however you want to do it
(Tank_Individual_Fig + theme(legend.position = "none"))/(Tank_Individual_Slopes_Fig + theme(legend.position ="bottom")) #/(legend_l) + plot_layout(heights = c(10, 10, 1))
#there are different colours for each, aaahhhh, I will have to fix it to be sure it's consistent

#tip: use line (can turn it on and off with commenting)
#   facet_wrap(~Survey_Site)+ 
#on line 68 of Quadrat_Height_Fig- see what it does
# we can also plot the 'end of season average height ' for each site
#ill look into it - not suuuper sure what this means, but we'll chat on it :) 

#-------------------------------------------------------------------------------

#Tiller Group Slope Height

summary(Tank_Height_Mod_Groups)
Tiller_Group_Height.p <- as_draws_df(Tank_Height_Mod_Groups,  subset = floor(runif(n = 1000, 1, max = 2000)))

head(Tiller_Group_Height.p)
colnames(Tiller_Group_Height.p)

# Extract specific columns related to slopes at different survey sites and create new variables
Tiller_Group_Height.p_df <- Tiller_Group_Height.p %>%
  select(b_Day, `b_Day:TankControl`, `b_Day:TankCoirLog`, `b_Day:TankMixed`) %>%
  mutate( Group_Brush.p = `b_Day`,  # Brush slope is the base 
          Group_Control.p = (`b_Day` + `b_Day:TankControl`), 
          Group_Mixed.p = (`b_Day` + `b_Day:TankMixed`),  
          Group_Coir.p =(`b_Day` + `b_Day:TankCoirLog`) 
  ) %>%
  select(-c(b_Day, `b_Day:TankControl`, `b_Day:TankCoirLog`, `b_Day:TankMixed`)) #removing the original columns

# Check the new data frame with calculated slopes
head(Tiller_Group_Height.p_df)

# Calculate mean slope, 95% credible intervals, and add labels for each tank
# Individual Control
gc.p <-  Tiller_Group_Height.p_df %>% 
  mutate( `Tank`="Control", 
          slope = mean(Group_Control.p),  # Mean slope
          slope_lower = quantile(Group_Control.p, probs=0.025),  # Lower 2.5% quantile
          slope_upper = quantile(Group_Control.p, probs=0.975))  %>%  # Upper 97.5% quantile
  dplyr::select(c(slope,slope_upper,slope_lower,`Tank`)) %>% distinct()  

# Individual Brush
gb.p <-  Tiller_Group_Height.p_df %>% 
  mutate(  `Tank`="Spruce Branch", 
           slope = mean(Group_Brush.p),
           slope_lower = quantile(Group_Brush.p, probs=0.025),
           slope_upper = quantile(Group_Brush.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Tank`)) %>% distinct()  

# Individual Mixed
gm.p <-  Tiller_Group_Height.p_df %>% 
  mutate( `Tank`="Mixed", 
          slope = mean(Group_Mixed.p),
          slope_lower = quantile(Group_Mixed.p, probs=0.025),
          slope_upper = quantile(Group_Mixed.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Tank`)) %>% distinct()  

# Individual Coir
gco.p <-  Tiller_Group_Height.p_df %>% 
  mutate(  `Tank`="Coir Fiber", 
           slope = mean(Group_Coir.p),
           slope_lower = quantile(Group_Coir.p, probs=0.025),
           slope_upper = quantile(Group_Coir.p, probs=0.975))  %>%
  dplyr::select(c(slope,slope_upper,slope_lower,`Tank`)) %>% distinct()  

# Combine all calculated data frames into one for plotting
slope_tank_g.p <- gc.p %>% bind_rows(gb.p, gm.p, gco.p) %>%
  mutate(Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber")) 

# View the combined results
slope_tank_g.p

#Plot the results

Tank_Group_Slopes_Fig <- ggplot(slope_tank_g.p, aes(x = `Tank`, y = slope, color=`Tank`)) +
  geom_point(size = 3) +  # Plot mean slope as points
  geom_errorbar(aes(ymin = slope_lower, ymax = slope_upper,
                    x = Tank), 
                width = 0.2) +  # Add credible intervals
  geom_hline(yintercept = 0, 
             linetype = "dashed", # Add a horizontal line at zero for reference
             aes (x = Tank)) +  
  labs(title = "Tank Posterior Estimates \n of Slopes - Group",
       x = "Experimental Substrate",
       y = "Slope Estimate (cm/day)",
       subtitle = "(b)") +
  scale_color_viridis_d(option = "C", 
                        name = NULL) +
  guides(colour = guide_legend(nrow = 2)) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), # Tilt x-axis labels
        plot.title = element_text(hjust = 0.5, size = 12),
        axis.text = element_text(size = 12),
        plot.subtitle = element_text(size = 12),
        legend.text = element_text(size = 12))  
#theme_minimal()  # Clean plot appearance
# coord_flip()  # Optional: flip axes for better readability
Tank_Group_Slopes_Fig

legend_g <- g_legend(Tank_Group_Fig)

#use patchwork to plot with corresponding regression (example) - or however you want to do it
(Tank_Group_Fig + theme(legend.position = "none"))/(Tank_Group_Slopes_Fig + theme(legend.position ="bottom")) 

#)/(legend_g) + plot_layout(heights = c(10, 10, 5))
#there are different colours for each, aaahhhh, I will have to fix it to be sure it's consistent

#tip: use line (can turn it on and off with commenting)
#   facet_wrap(~Survey_Site)+ 
#on line 68 of Quadrat_Height_Fig- see what it does
# we can also plot the 'end of season average height ' for each site
#ill look into it - not suuuper sure what this means, but we'll chat on it :) 
