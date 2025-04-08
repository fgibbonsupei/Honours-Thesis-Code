rm(list=ls())
library(tidyverse)
library(readxl)
library(rstan)
library(brms)

#Getting the sheets in
TidyData_Tank<-read_excel("~/Desktop/Honours/TidyData.xlsx",sheet=1)
TidyData_QuadratTHeight<-read_excel("~/Desktop/Honours/TidyData.xlsx",sheet=2)
TidyData_Biomass_Quadrat<-read_excel("~/Desktop/Honours/TidyData.xlsx",sheet=3)
TidyData_Biomass_Tank<-read_excel("~/Desktop/Honours/TidyData.xlsx",sheet=4)
TidyData_Tank_Output<- read_excel("~/Desktop/Honours/TidyData.xlsx",sheet=5)


#-------------------------------------------------------------------------------
#Elongated summary of how to make a model:
#Shows the head of the data
#head(TidyData_Tank)
#print(TidyData_Tank,n=20) #n=20 shows 20 rows
#View(TidyData_Tank)

#Making it tidy
Tank_Long <- TidyData_Tank %>% group_by(Day,Tank, `Planting Method`) %>%
  gather(key= "Rep",value="Tiller_Height",`Rep 1`:`Rep 6`) %>%
  filter(Tiller_Height != "-") #removing the blank data (!=) everything but 
head(Tank_Long)
print(Tank_Long,n=20) 

#Making the model
Tank_Individuals <- Tank_Long %>% filter(`Planting Method` == "Individuals") %>% mutate(Tiller_Height = as.numeric(Tiller_Height))
#write.csv(Tank_Individuals, "~/Desktop/Honours/Tank_Individuals.csv",row.names=FALSE)
Tank_Groups <- Tank_Long %>% filter(`Planting Method` == "Groups") %>% mutate(Tiller_Height = as.numeric(Tiller_Height))
#write.csv(Tank_Groups, "~/Desktop/Honours/Tank_Groups.csv",row.names=FALSE)

summary(Tank_Individuals)

#Model for tank individuals
#Tank_Height_Mod_Individuals <- brm(Tiller_Height ~ Day *Tank,data=Tank_Individuals)
#save(Tank_Height_Mod_Individuals, file="~/Desktop/Honours/Tank_Height_Mod_Individuals.Rdata")
load("~/Desktop/Honours/Tank_Height_Mod_Individuals.Rdata") #load here to load the model 
summary(Tank_Height_Mod_Individuals)
pp_check(Tank_Height_Mod_Individuals)
plot(Tank_Height_Mod_Individuals)
conditional_effects(Tank_Height_Mod_Individuals)

#Model for tank groups
#Tank_Height_Mod_Groups <- brm(Tiller_Height ~ Day *Tank,data=Tank_Groups)
#save(Tank_Height_Mod_Groups, file="~/Desktop/Honours/Tank_Height_Mod_Groups.Rdata")
load("~/Desktop/Honours/Tank_Height_Mod_Groups.Rdata") #load here to load the model 
summary(Tank_Height_Mod_Groups)
pp_check(Tank_Height_Mod_Groups)
plot(Tank_Height_Mod_Groups)
conditional_effects(Tank_Height_Mod_Groups)

#-------------------------------------------------------------------------------
#Data for quadrat and tiller heights
head(TidyData_QuadratTHeight)
summary(TidyData_QuadratTHeight)
Data_Quadrat_Height <- TidyData_QuadratTHeight %>% mutate(`Tiller Height (cm)` = as.numeric(`Tiller Height (cm)`)) %>% 
  filter(!is.na(`Tiller Height (cm)`)) %>% mutate(Survey_Site = `Survey Site`) %>% mutate(Tiller_Height = `Tiller Height (cm)`)
write.csv(Data_Quadrat_Height, "~/Desktop/Honours/Data_Quadrat_Height.csv",row.names=FALSE)
head(Data_Quadrat_Height)
#Quadrat_Mod_Height <- brm(Tiller_Height~ Day * Survey_Site, data= Data_Quadrat_Height)
#save(Quadrat_Mod_Height, file="~/Desktop/Honours/Quadrat_Mod_Height.Rdata")
load("~/Desktop/Honours/Quadrat_Mod_Height.Rdata") #load here to load the model 
summary(Quadrat_Mod_Height)
pp_check(Quadrat_Mod_Height)
plot(Quadrat_Mod_Height)
conditional_effects(Quadrat_Mod_Height)

#-------------------------------------------------------------------------------
#Model for QUADRAT biomass above
head(TidyData_Biomass)
summary(TidyData_Biomass)
Data_Biomass <- TidyData_Biomass_Quadrat %>% mutate(Replicate = as.character(Replicate)) %>% #Changing from character 
  mutate(Aboveground_Dry = `Aboveground Dry Mass`) %>% mutate(Belowground_Dry = `Belowground Dry Mass`) %>% 
  mutate(Source_Marsh = `Source Marsh`) %>% mutate(Planting_Method = `Planting Method`)
#write.csv(Data_Biomass, "~/Desktop/Honours/Data_Biomass.csv",row.names=FALSE)
summary(Data_Biomass) 
head(Data_Biomass)
Data_Biomass %>% select(`Planting_Method`) %>% distinct()

Data_Biomass_i <- Data_Biomass %>% filter(`Planting_Method` == "Individual") #separating the indv. and group data 
Data_Biomass_g <- Data_Biomass %>% filter(`Planting_Method` == "Group")
head(Data_Biomass_i)
head(Data_Biomass_g)

#A_Biomass_Mod <- brm(Aboveground_Dry ~ Source_Marsh * Planting_Method, data = Data_Biomass) 
A_Biomass_Mod_i <- brm(Aboveground_Dry ~ Source_Marsh, data = Data_Biomass_i, family = lognormal())
A_Biomass_Mod_g <- brm(Aboveground_Dry ~ Source_Marsh, data = Data_Biomass_g, family = lognormal())

#save(A_Biomass_Mod, file="~/Desktop/Honours/A_Biomass_Mod.Rdata")
#save(A_Biomass_Mod_i, file="~/Desktop/Honours/A_Biomass_Mod_i.Rdata")
#save(A_Biomass_Mod_g, file="~/Desktop/Honours/A_Biomass_Mod_g.Rdata")
load("~/Desktop/Honours/A_Biomass_Mod.Rdata") 
load("~/Desktop/Honours/A_Biomass_Mod_i.Rdata") 
load("~/Desktop/Honours/A_Biomass_Mod_g.Rdata") 
summary(A_Biomass_Mod)
pp_check(A_Biomass_Mod)
plot(A_Biomass_Mod)
conditional_effects(A_Biomass_Mod)
conditional_effects(A_Biomass_Mod_i)
conditional_effects(A_Biomass_Mod_g)

#-------------------------------------------------------------------------------
#Model for QUADRAT biomass below 
#B_Biomass_Mod <- brm(Belowground_Dry ~ Source_Marsh * Planting_Method, data = Data_Biomass, family= lognormal()) #seeing if this will be above 0 
B_Biomass_Mod_i <- brm(Belowground_Dry ~ Source_Marsh, data = Data_Biomass_i, family= lognormal())
B_Biomass_Mod_g <- brm(Belowground_Dry ~ Source_Marsh, data = Data_Biomass_g, family= lognormal()) 

#save(B_Biomass_Mod, file="~/Desktop/Honours/B_Biomass_Mod.Rdata")
#save(B_Biomass_Mod_i, file="~/Desktop/Honours/B_Biomass_Mod_i.Rdata")
#save(B_Biomass_Mod_g, file="~/Desktop/Honours/B_Biomass_Mod_g.Rdata")
load("~/Desktop/Honours/B_Biomass_Mod.Rdata") 
load("~/Desktop/Honours/B_Biomass_Mod_i.Rdata") 
load("~/Desktop/Honours/B_Biomass_Mod_g.Rdata") 

summary(B_Biomass_Mod)
pp_check(B_Biomass_Mod)
plot(B_Biomass_Mod)
conditional_effects(B_Biomass_Mod)
conditional_effects(B_Biomass_Mod_i)
conditional_effects(B_Biomass_Mod_g)

#-------------------------------------------------------------------------------
#Model for TANK biomass above 
head(Data_Biomass_Tank)

Data_Biomass_Tank_i <- Data_Biomass_Tank %>% filter(`Planting_Method` == "Individual") %>% 
  mutate(Tank = fct_recode(Treatment, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber"))  #separating the indv. and group data 

Data_Biomass_Tank_g <- Data_Biomass_Tank %>% filter(`Planting_Method` == "Group") %>% 
  mutate(Tank = fct_recode(Treatment, 
                           "Coir Fiber" = "Coir",
                           "Control" = "Control",
                           "Mixed" = "Mixed",
                           "Spruce Branch" = "Brush"),
         Tank = fct_relevel(Tank,
                            "Control", "Spruce Branch", "Mixed", "Coir Fiber")) 
head(Data_Biomass_Tank_i)
head(Data_Biomass_Tank_g)


head(TidyData_Biomass_Tank)
summary(TidyData_Biomass_Tank)
Data_Biomass_Tank <- TidyData_Biomass_Tank %>% mutate(Replicate = as.character(Replicate)) %>% #Changing from character 
  mutate(Aboveground_Dry = `Aboveground Dry Mass`) %>% mutate(Belowground_Dry = `Belowground Dry Mass`) %>% 
  mutate(Planting_Method = `Planting Method`)
#write.csv(Data_Biomass_Tank, "~/Desktop/Honours/Data_Biomass_Tank.csv",row.names=FALSE)
summary(Data_Biomass_Tank)

#A_Biomass_Tank_Mod <- brm(Aboveground_Dry ~ Treatment * Planting_Method, data = Data_Biomass_Tank) #this is the model 
A_Biomass_Tank_Mod_i <- brm(Aboveground_Dry ~ Treatment, data = Data_Biomass_Tank_i, family = lognormal())
A_Biomass_Tank_Mod_g <- brm(Aboveground_Dry ~ Treatment, data = Data_Biomass_Tank_g, family = lognormal())

#save(A_Biomass_Tank_Mod, file="~/Desktop/Honours/A_Biomass_Mod.Rdata")
#save(A_Biomass_Tank_Mod_i, file="~/Desktop/Honours/A_Biomass_Tank_Mod_i.Rdata")
#save(A_Biomass_Tank_Mod_g, file="~/Desktop/Honours/A_Biomass_Tank_Mod_g.Rdata")
load("~/Desktop/Honours/A_Biomass_Tank_Mod.Rdata") 
load("~/Desktop/Honours/A_Biomass_Tank_Mod_i.Rdata") 
load("~/Desktop/Honours/A_Biomass_Tank_Mod_g.Rdata") 

summary(A_Biomass_Tank_Mod)
pp_check(A_Biomass_Tank_Mod)
plot(A_Biomass_Tank_Mod)
conditional_effects(A_Biomass_Tank_Mod)

head(Data_Biomass_Tank_i)

#-------------------------------------------------------------------------------
#Model for TANK biomass below 
#B_Biomass_Tank_Mod <- brm(Belowground_Dry ~ Treatment * Planting_Method, data = Data_Biomass_Tank) #this is the model 
B_Biomass_Tank_Mod_i <- brm(Belowground_Dry ~ Treatment, data = Data_Biomass_Tank_i, family = lognormal())
B_Biomass_Tank_Mod_g <- brm(Belowground_Dry ~ Treatment, data = Data_Biomass_Tank_g, family = lognormal())

#save(B_Biomass_Tank_Mod, file="~/Desktop/Honours/B_Biomass_Mod.Rdata")
#save(B_Biomass_Tank_Mod_i, file="~/Desktop/Honours/B_Biomass_Tank_Mod_i.Rdata")
#save(B_Biomass_Tank_Mod_g, file="~/Desktop/Honours/B_Biomass_Tank_Mod_g.Rdata")
load("~/Desktop/Honours/B_Biomass_Tank_Mod.Rdata") 
load("~/Desktop/Honours/B_Biomass_Tank_Mod_i.Rdata") 
load("~/Desktop/Honours/B_Biomass_Tank_Mod_g.Rdata") 

summary(B_Biomass_Tank_Mod)
pp_check(B_Biomass_Tank_Mod)
plot(B_Biomass_Tank_Mod)
conditional_effects(B_Biomass_Tank_Mod)

#-------------------------------------------------------------------------------
#Model for tank tiller output 
head(TidyData_Tank_Output)
summary(TidyData_Tank_Output)
Data_Tank_Output <- TidyData_Tank_Output %>% mutate(Replicate = as.character(Replicate)) %>% #Changing from character 
  mutate(Planting_Method = `Planting Method`)
#write.csv(Data_Tank_Output, "~/Desktop/Honours/Data_Tank_Output.csv",row.names=FALSE)
#summary(Data_Tank_Output) 
Tank_Tiller_Output_Mod <- brm(Tillers ~ Treatments * Planting_Method, data = Data_Tank_Output, family = poisson()) #this is the model 
#save(Tank_Tiller_Output_Mod, file="~/Desktop/Honours/Tank_Tiller_Output_Mod.Rdata")
load("~/Desktop/Honours/Tank_Tiller_Output_Mod.Rdata") #load here to load the model 
summary(Tank_Tiller_Output_Mod)
pp_check(Tank_Tiller_Output_Mod)
plot(Tank_Tiller_Output_Mod)
conditional_effects(Tank_Tiller_Output_Mod)


# Data_Tank_Output_i <- Data_Tank_Output %>% filter(`Planting_Method` == "Individual") #separating the indv. and group data 
# Data_Tank_Output_g <- Data_Tank_Output %>% filter(`Planting_Method` == "Group")
# head(Data_Tank_Output_i)
# head(Data_Tank_Output_g)

# Tank_Tiller_Output_Mod_i <- brm(Tillers ~ Treatments, data = Data_Tank_Output_i, family= lognormal())
# Tank_Tiller_Output_Mod_g <- brm(Tillers ~ Treatments, data = Data_Tank_Output_g, family= lognormal()) 
#save(Tank_Tiller_Output_Mod_i, file="~/Desktop/Honours/Tank_Tiller_Output_Mod_i.Rdata")
#save(Tank_Tiller_Output_Mod_g, file="~/Desktop/Honours/Tank_Tiller_Output_Mod_g.Rdata")
load("~/Desktop/Honours/Tank_Tiller_Output_Mod_i.Rdata")
load("~/Desktop/Honours/Tank_Tiller_Output_Mod_g.Rdata") 

#-------------------------------------------------------------------------------
#Model for tank inflorescence output
Tank_Inflo_Output_Mod <- brm(Inflorescences ~ Treatments * Planting_Method, data = Data_Tank_Output, family = poisson()) #this is the model 
#save(Tank_Inflo_Output_Mod, file="~/Desktop/Honours/Tank_Inflo_Output_Mod.Rdata")
load("~/Desktop/Honours/Tank_Inflo_Output_Mod.Rdata") #load here to load the model 
summary(Tank_Inflo_Output_Mod)
pp_check(Tank_Inflo_Output_Mod)
plot(Tank_Inflo_Output_Mod)
conditional_effects(Tank_Inflo_Output_Mod)

# Data_Tank_Output_i
# Data_Tank_Output_g
# 
# Tank_Inflo_Output_Mod_i <- brm(Inflorescences ~ Treatments, data = Data_Tank_Output_i, family= poisson())
# conditional_effects(Tank_Inflo_Output_Mod_i)
# 
# Tank_Inflo_Output_Mod_g <- brm(Inflorescences ~ Treatments, data = Data_Tank_Output_g, family= poisson())
# conditional_effects(Tank_Inflo_Output_Mod_g)

#save(Tank_Tiller_Output_Mod_i, file="~/Desktop/Honours/Tank_Tiller_Output_Mod_i.Rdata")
#save(Tank_Tiller_Output_Mod_g, file="~/Desktop/Honours/Tank_Tiller_Output_Mod_g.Rdata")
# load("~/Desktop/Honours/Tank_Tiller_Output_Mod_i.Rdata")
# load("~/Desktop/Honours/Tank_Tiller_Output_Mod_g.Rdata") 

