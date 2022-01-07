## RESEARCH PROPOSAL
## Contracting people in trust position in public sector: Evidence from Brazil


# clean up workspace (all existing objects)
rm(list = ls())

#working directory
setwd('C:/Users/Mateus/Downloads/Comissionados')

#package
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)


#import data
#file
my_data <- 'C:/Users/Mateus/Downloads/Comissionados/Base_MUNIC_2020.xlsx'
#kind of contracts
#clean lines with 'Recusa'
my_rh <- read_excel(my_data, sheet = 'Recursos humanos') 

#habitation info
my_hab <- read_excel(my_data, sheet = 'Habitação')

#transportation info
my_trans <- read_excel(my_data, sheet = 'Transporte')

#farming info
my_farm <- read_excel(my_data, sheet = 'Agropecuário')

#environment info
my_enviro <- read_excel(my_data, sheet = 'Meio ambiente')

#risk managment info
#my_risk <- read_excel(my_data, sheet = 'Gestão de riscos')


#cleaning my_rh
#clean lines with 'Recusa'
my_rh <- my_rh %>%
  dplyr::filter(my_rh$Mreh0113 != 'Recusa')

#clean lines with '(*) Não soube informar'
my_rh <- my_rh %>%
  dplyr::filter(my_rh$Mreh0113 != '(*) Não soube informar')

#clean lines with '(*) Não informou'
my_rh <- my_rh %>%
  dplyr::filter(my_rh$Mreh0113 != 'Não informou')

#change chr for numeric
my_rh$Mreh0113 <- as.numeric(my_rh$Mreh0113)
my_rh$Mreh0116 <- as.numeric(my_rh$Mreh0116)

#remove NAs 
my_rh <- my_rh %>%
  na.omit(my_rh$Mreh0116)



#create variable: comissionados/Total
my_rh$perc_comis <- my_rh$Mreh0113 / my_rh$Mreh0116


#create variable for policies
# relevant questions for habitation
quest_hab <- dplyr::tibble(my_hab$Mhab201,
                           my_hab$Mhab202,
                           my_hab$Mhab203,
                           my_hab$Mhab204,
                           my_hab$Mhab205,
                           my_hab$Mhab206,
                           my_hab$Mhab207,
                           my_hab$Mhab21)

#relevant questions for transport services
quest_trans <- dplyr::tibble(my_trans$Mtra23,
                             my_trans$Mtra24)


#relevant questions for farming
quest_farm <- dplyr::tibble(my_farm$Magr131,
                            my_farm$Magr132,
                            my_farm$Magr133,
                            my_farm$Magr134,
                            my_farm$Magr135,
                            my_farm$Magr136,
                            my_farm$Magr141,
                            my_farm$Magr142,
                            my_farm$Magr143,
                            my_farm$Magr151,
                            my_farm$Magr152,
                            my_farm$Magr153,
                            my_farm$Magr154,
                            my_farm$Magr155)

#relevant questions for envirnment
quest_enviro <- dplyr::tibble(my_enviro$Mmam08,
                              my_enviro$Mmam241,
                              my_enviro$Mmam242,
                              my_enviro$Mmam243,
                              my_enviro$Mmam244,
                              my_enviro$Mmam245,
                              my_enviro$Mmam246,
                              my_enviro$Mmam247)

#relevant questions for risk managment
#quest_risk <- dplyr::tibble(my_risk$)


#create a function to calculate index
function.index <- function(questions) {
  
  sumation <- matrix(0, nrow =  nrow(questions), ncol = 1)
  #if answer is "yes" = 1
  for (j in 1:ncol(questions)) {
    for (i in 1:nrow(questions)) {
      if(questions[i,j] == 'Sim') {
        sumation[i] <- sumation[i] + 1
      }
    }
  }
  #creating the index
  index <- sumation / ncol(questions)
  #return index
  return(index)
}

#put index in the own dataframe
#my_hab
my_hab$index <- function.index(quest_hab)
#summary(my_hab$index)

#my_trans
my_trans$index <- function.index(quest_trans)
#summary(my_trans$index)

#my_farm
my_farm$index <- function.index(quest_farm)
#summary(my_farm$index)

#my_enviro
my_enviro$index <- function.index(quest_enviro)
#summary(my_enviro$index)





#create function to put each index in my_rh by CodMun: OK
function.matching <- function(rh, area) {
  rh$index <- matrix(0, nrow =  nrow(rh), ncol = 1)
  for(i in 1:nrow(rh)){
    for(j in 1:nrow(area)){
      #search for CodMun
      if (rh$CodMun[i] == area$CodMun[j]){
        rh$index[i] <- area$index[j]
        break
      }
    }
  }
  return(rh$index)
}

#put indexes in the my_rh dataframe
#habitation
my_rh$index_hab <- function.matching(my_rh, my_hab)
#summary(my_rh$index_hab)

#transport
my_rh$index_trans <- function.matching(my_rh, my_trans)
#summary(my_rh$index_trans)

#farming
my_rh$index_farm <- function.matching(my_rh, my_farm)
#summary(my_rh$index_farm)

#environment
my_rh$index_enviro <- function.matching(my_rh, my_enviro)
#summary(my_rh$index_enviro)




#descriptive statistics
#indexes
summary(my_rh$perc_comis)
summary(my_rh$index_hab)
summary(my_rh$index_trans)
summary(my_rh$index_farm)
summary(my_rh$index_enviro)



#regression: habitation ~ comissionados
regres_hab <- lm(my_rh$index_hab ~ my_rh$perc_comis)
summary(regres_hab)

#regression: transport ~ comissionados
regres_trans <- lm(my_rh$index_trans ~ my_rh$perc_comis)
summary(regres_trans)

#regression: farming ~ comissionados
regres_farm <- lm(my_rh$index_farm ~ my_rh$perc_comis)
summary(regres_farm)

#regression: environment ~ comissionados
regres_enviro <- lm(my_rh$index_enviro ~ my_rh$perc_comis)
summary(regres_enviro)


#graphics
#habitation
ggplot(my_rh) +
  geom_point(mapping = aes(x = my_rh$perc_comis, y = my_rh$index_hab)) + 
  geom_abline(slope = coef(regres_hab)[["my_rh$perc_comis"]], 
              intercept = coef(regres_hab)[["(Intercept)"]],
              color='red') +
  xlab("Commissioned percentile") +
  ylab("Housing index")

#transport
ggplot(my_rh) +
  geom_point(mapping = aes(x = my_rh$perc_comis, y = my_rh$index_trans)) + 
  geom_abline(slope = coef(regres_trans)[["my_rh$perc_comis"]], 
              intercept = coef(regres_trans)[["(Intercept)"]],
              color='red') +
  xlab("Commissioned percentile") +
  ylab("Transport index")

#farming
ggplot(my_rh) +
  geom_point(mapping = aes(x = my_rh$perc_comis, y = my_rh$index_farm)) + 
  geom_abline(slope = coef(regres_farm)[["my_rh$perc_comis"]], 
              intercept = coef(regres_farm)[["(Intercept)"]],
              color='red') +
  xlab("Commissioned percentile") +
  ylab("Agriculture index")

#environment
ggplot(my_rh) +
  geom_point(mapping = aes(x = my_rh$perc_comis, y = my_rh$index_enviro)) + 
  geom_abline(slope = coef(regres_enviro)[["my_rh$perc_comis"]], 
              intercept = coef(regres_enviro)[["(Intercept)"]],
              color='red') +
  xlab("Commissioned percentile") +
  ylab("Environment index")
  






