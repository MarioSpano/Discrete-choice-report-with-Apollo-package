rm(list = ls())

#### Load libraries and wd####
# [Discrete Choice Note]: 'apollo' is a specialized R package used for Choice Modelling. 
# It allows us to estimate how people make choices between different alternatives.
library(apollo)
library(tidyverse)
library(dplyr)

setwd("C:\\UNIBO\\Biondi\\Project")

#### Load database ####
data = read.csv("C:/UNIBO/Biondi/Project/heating_data.csv")
data <- data
#These cost are conditional on the house having central air-conditioning. (That 
#is why the installation cost of gas central is lower than that for gas room: 
#the central system can use the air-conditioning products that have been installed.)

head(data)
#sum(is.na(database))=0, so we do not have missing values

database<-data
unique(database$region)


####Socio-Demographic variables####
# [Discrete Choice Note]: In choice models, characteristics of the decision-maker (like age or income) 
# do not change across the alternatives. To include them in our equations, we often convert them 
# into binary "dummy" variables (0 or 1). This helps us see if a specific group (e.g., high income) 
# has a stronger or weaker preference for a certain alternative.

#regions
unique(database$region)
reg.dummies = model.matrix(~ region - 1, data = database)
database = cbind(database, reg.dummies)

#income
unique(database$income)
database$high_income = ifelse(database$income>= 5, 1, 0)
database$low_income =  ifelse(database$income< 5, 1, 0)

#age
unique(database$agehed)
database$old = ifelse(database$agehed>= 50, 1, 0)
database$young=  ifelse(database$agehead< 50, 1, 0)

#rooms
unique(database$rooms)

database$many_rooms = ifelse(database$rooms>= 5, 1, 0)
database$few_rooms =  ifelse(database$rooms< 5, 1, 0)



##### Multinominal Logit #####
# [Discrete Choice Note]: Multinomial Logit (MNL) is the most standard choice model. 
# It assumes that decision-makers evaluate the "Utility" (overall value or score) of each option 
# and choose the one with the highest Utility. 
apollo_initialise()


##########Model 1##########


## Set core controls ####
apollo_control = list(
  modelName       = "MNL big",
  modelDescr      = "MNL with everything",
  indivID         = "idcase", 
  outputDirectory = "output"
)


## DEFINE MODEL PARAMETERS ####

### Vector of parameters, including any that are kept fixed in estimation
# [Discrete Choice Note]: 'apollo_beta' are the parameters (weights) the model will estimate. 
# - 'b0_...' are Alternative Specific Constants (ASCs). They capture the baseline preference 
#   for an alternative, ignoring all other attributes.
# - 'b_ic' and 'b_oc' represent how much Installation Cost and Operational Cost impact the choice.
apollo_beta=c( b0_gc   =0, b0_gr=0, b0_ec =0, b0_er =0, b0_hp =0, 
               b_age_gc = 0, b_age_gr = 0, b_age_ec = 0, b_age_er = 0,
               b_rooms_gc = 0, b_rooms_gr = 0, b_rooms_ec = 0, b_rooms_er = 0,
               b_mountn_gc=0, b_mountn_ec=0, b_mountn_gr=0, b_mountn_er=0,
               b_scostl_gc=0, b_scostl_ec=0, b_scostl_er=0, b_scostl_gr=0,
               b_ncostl_gc=0, b_ncostl_ec=0, b_ncostl_gr=0, b_ncostl_er=0,
               b_income_gc= 0,b_income_gr= 0,b_income_ec= 0,b_income_er= 0,
               b_ic =0, b_oc =0 
               ) 

# [Discrete Choice Note]: We must fix one alternative's constant to 0 to act as a "reference point." 
# If we estimated all 5, the math wouldn't work (a problem called perfect collinearity).
# Here, Heat Pump (hp) is our baseline.
apollo_fixed = c("b0_hp")

# ################################################################# #
## GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  # [Discrete Choice Note]: V represents the "Deterministic Utility" (the part of the choice score we can measure).
  # We construct mathematical equations for each choice. For instance, the score for Gas Central (gc) 
  # is its baseline (b0_gc) + the impact of its install cost (b_ic * ic.gc) + its operational cost + user demographics.
  V = list()
  
  #define utilities
  V[["gc"]] = b0_gc + b_ic * ic.gc + b_oc * oc.gc + 
    b_mountn_gc * regionmountn + b_ncostl_gc * regionncostl + b_scostl_gc * regionscostl +
    b_age_gc * old + b_income_gc * high_income + b_rooms_gc * many_rooms
  V[["gr"]] = b0_gr + b_ic * ic.gr + b_oc * oc.gr + 
    b_mountn_gr * regionmountn + b_ncostl_gr * regionncostl + b_scostl_gr * regionscostl +
    b_age_gr * old + b_income_gr * high_income + b_rooms_gr * many_rooms
  V[["ec"]] = b0_ec + b_ic * ic.ec + b_oc * oc.ec + 
    b_mountn_ec * regionmountn + b_ncostl_ec * regionncostl + b_scostl_ec * regionscostl +
    b_age_ec * old + b_income_ec * high_income + b_rooms_ec * many_rooms
  V[["er"]] = b0_er + b_ic * ic.er + b_oc * oc.er +
    b_mountn_er * regionmountn + b_ncostl_er * regionncostl + b_scostl_er * regionscostl +
    b_age_er * old + b_income_er * high_income + b_rooms_er * many_rooms
  
  #Alternative: HP as BASE CASE (Notice how it lacks the b0 and demographic shifts, acting as our zero-point anchor).
  V[["hp"]] = b_ic * ic.hp + b_oc * oc.hp

  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(gc="gc", gr="gr", ec="ec", er="er", hp="hp"), 
    choiceVar     = depvar, # 'depvar' is the column showing what the user actually chose
    utilities     = V)
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

# [Discrete Choice Note]: The software now uses Maximum Likelihood Estimation to find the exact parameter weights 
# that make the choices actually observed in the data as probable as possible.
model1 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

#---- FORMATTED OUTPUT (TO SCREEN and TO FILE                             ----
# ----------------------------------------------------------------- #

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(model1, modelOutput_settings)
apollo_saveOutput(model1, modelOutput_settings)


##########Model2##########
# [Discrete Choice Note]: The analyst realizes Model 1 has too many insignificant variables. 
# Model 2 simplifies things by dropping age, rooms, and regions, keeping only income and costs.

##### Set core controls ####
apollo_control = list(
  modelName       = "MNL income",
  modelDescr      = "MNL only ASC, cost and Income",
  indivID         = "idcase", 
  outputDirectory = "output"
)


## DEFINE MODEL PARAMETERS ####

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c( b0_gc   =0, b0_gr=0, b0_ec =0, b0_er =0, b0_hp =0, 
               b_ic =0,
               b_oc =0,
               b_income_gr= 0, b_income_gc= 0, b_income_ec= 0, b_income_er= 0
) 


apollo_fixed = c("b0_hp")

# ################################################################# #
## GROUP AND VALIDATE INPUTS                                    ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                         ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  
  # ################################################################# #
  #### DEFINE UTILITIES                                            ####
  # ################################################################# #
  
  V[["gc"]] = b0_gc + b_ic* ic.gc + b_oc* oc.gc + b_income_gc*high_income
  V[["gr"]] = b0_gr + b_ic* ic.gr + b_oc * oc.gr + b_income_gr * high_income
  V[["ec"]] = b0_ec + b_ic*ic.ec + b_oc* oc.ec + b_income_ec*high_income
  V[["er"]] = b0_er + b_ic* ic.er + b_oc * oc.er + b_income_er*high_income
  V[["hp"]] = b0_hp + b_ic* ic.hp + b_oc * oc.hp 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(gc="gc", gr="gr", ec="ec", er="er", hp="hp"), 
    choiceVar     = depvar,
    utilities     = V)
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


model2 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#---- FORMATTED OUTPUT (TO SCREEN and TO FILE                             ----

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(model2, modelOutput_settings)
apollo_saveOutput(model2, modelOutput_settings)


##########Model 3##########
# [Discrete Choice Note]: Now we introduce "interactions". Instead of income just adding a flat bonus 
# to a choice, we want to see if being high-income changes how much a person cares about costs.
# We create shift parameters: base cost sensitivity + a bonus sensitivity if the user has high income.

## Set core controls ####
apollo_control = list(
  modelName       = "MNL with 2 gr interactions",
  modelDescr      = "MNL with double income interaction",
  indivID         = "idcase", 
  outputDirectory = "output"
)


## DEFINE MODEL PARAMETERS ####

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c( b0_gc   =0, b0_gr=0, b0_ec =0, b0_er =0, b0_hp =0, 
               b_ic =0, b_oc =0, b_ic_income=0, b_oc_income=0)


apollo_fixed = c("b0_hp")

# ################################################################# #
## GROUP AND VALIDATE INPUTS                                    ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                         ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ##specifico interazione (Specifying the interaction)
  # [Discrete Choice Note]: The new cost weights (b_ic_shift, b_oc_shift) depend on the user's income.
  b_ic_shift = b_ic + b_ic_income * high_income
  b_oc_shift = b_oc + b_oc_income * high_income
  
  ### List of utilities
  V = list()
  
  # ################################################################# #
  #### DEFINE UTILITIES                                            ####
  # ################################################################# #
  
  V[["gc"]] = b0_gc + b_ic * ic.gc + b_oc* oc.gc 
  # [Discrete Choice Note]: We only apply this interaction to Gas Room (gr) to test a specific hypothesis.
  V[["gr"]] = b0_gr + b_ic_shift * ic.gr + b_oc_shift * oc.gr
  V[["ec"]] = b0_ec + b_ic * ic.ec + b_oc * oc.ec 
  V[["er"]] = b0_er + b_ic * ic.er + b_oc * oc.er 
  V[["hp"]] = b0_hp + b_ic * ic.hp + b_oc * oc.hp 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(gc="gc", gr="gr", ec="ec", er="er", hp="hp"), 
    choiceVar     = depvar,
    utilities     = V)
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


model3 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
####---- FORMATTED OUTPUT (TO SCREEN and TO FILE ---####

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(model3, modelOutput_settings)
apollo_saveOutput(model3, modelOutput_settings) 



###Test model3 between odel2####
# [Discrete Choice Note]: apollo_lrTest performs a Likelihood Ratio Test. It statistically checks 
# if the extra complexity (the interactions) in Model 3 provides a significantly better fit than Model 2.
apollo_lrTest(model3, model2)


##########Model 4##########
# [Discrete Choice Note]: Model 4 is refined further. The analyst found that only the interaction between
# operational cost and income is significant, so the installation cost interaction is removed. This becomes the final model.

## Set core controls ####
apollo_control = list(
  modelName       = "MNL single interaction",
  modelDescr      = "MNL single interaction",
  indivID         = "idcase", 
  outputDirectory = "output"
)


## DEFINE MODEL PARAMETERS ####

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c( b0_gc   =0, b0_gr=0, b0_ec =0, b0_er =0, b0_hp =0, 
               b_ic =0, b_oc =0, b_oc_income=0)


apollo_fixed = c("b0_hp")

# ################################################################# #
## GROUP AND VALIDATE INPUTS                                    ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                         ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ##specifico interazione
  b_oc_shift = b_oc + b_oc_income * high_income
  
  ### List of utilities
  V = list()
  
  # ################################################################# #
  #### DEFINE UTILITIES                                            ####
  # ################################################################# #
  
  V[["gc"]] = b0_gc + b_ic * ic.gc + b_oc* oc.gc 
  V[["gr"]] = b0_gr + b_ic * ic.gr + b_oc_shift * oc.gr
  V[["ec"]] = b0_ec + b_ic * ic.ec + b_oc * oc.ec 
  V[["er"]] = b0_er + b_ic * ic.er + b_oc * oc.er 
  V[["hp"]] = b0_hp + b_ic * ic.hp + b_oc * oc.hp 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(gc="gc", gr="gr", ec="ec", er="er", hp="hp"), 
    choiceVar     = depvar,
    utilities     = V)
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


model4 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
####---- FORMATTED OUTPUT (TO SCREEN and TO FILE ---####                        

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(model4, modelOutput_settings)
apollo_saveOutput(model4, modelOutput_settings)  



###Test model4 between model3####
apollo_lrTest(model4, model3)



##########Model 5##########
# [Discrete Choice Note]: Model 5 is the "Micro MNL", the absolute simplest model containing only constants (ASCs) 
# and the two costs. The analyst tests it just to confirm that keeping the single income interaction in Model 4 
# was mathematically justified.

## Set core controls ####
apollo_control = list(
  modelName       = "Micro MNL",
  modelDescr      = "MNL only ASC and costs ",
  indivID         = "idcase", 
  outputDirectory = "output"
)


## DEFINE MODEL PARAMETERS ####

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c( b0_gc=0, b0_gr=0, b0_ec =0, b0_er =0, b0_hp =0, 
               b_ic =0,
               b_oc =0)


apollo_fixed = c("b0_hp")

# ################################################################# #
## GROUP AND VALIDATE INPUTS                                    ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                         ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  
  # ################################################################# #
  #### DEFINE UTILITIES                                            ####
  # ################################################################# #
  
  V[["gc"]] = b0_gc + b_ic* ic.gc + b_oc* oc.gc 
  V[["gr"]] = b0_gr + b_ic* ic.gr + b_oc * oc.gr
  V[["ec"]] = b0_ec + b_ic*ic.ec + b_oc* oc.ec 
  V[["er"]] = b0_er + b_ic* ic.er + b_oc * oc.er 
  V[["hp"]] = b0_hp + b_ic* ic.hp + b_oc * oc.hp 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(gc="gc", gr="gr", ec="ec", er="er", hp="hp"), 
    choiceVar     = depvar,
    utilities     = V)
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


model5 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
####---- FORMATTED OUTPUT (TO SCREEN and TO FILE ---####                        

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(model5, modelOutput_settings)
apollo_saveOutput(model5, modelOutput_settings)  



###Test model4 between model5####
apollo_lrTest(model4, model5)





#####Most interesting marginal rate of substitutions for the model4####
# [Discrete Choice Note]: The Marginal Rate of Substitution (MRS), in this context, is the Willingness To Pay.
# By dividing the operational cost coefficient by the installation cost coefficient, we calculate 
# how many dollars of up-front installation cost a person is willing to trade for a 1 dollar drop in ongoing operational costs.

MRS_oc.ic <- model4[["estimate"]][["b_oc"]]/model4[["estimate"]][["b_ic"]]
#on average the households in California would pay (result=) 4.566 dollar more in 
#Installation Cost to decrease the Operational Cost of 1 euro.



#### 1. Preparation data for merging####
# [Discrete Choice Note]: Now we take our estimated model parameters and feed the original data back into it
# to generate predicted probabilities for each household making each choice.
predictions_base = apollo_prediction(model4, apollo_probabilities, apollo_inputs)

predictions_df <- as.data.frame(predictions_base)

#just to ceck:
predictions_df %>% slice(1) %>% select(-ID, -Observation, -chosen) %>%
rowSums() # the result is 1, so everything is fine (probabilities for a single person must always sum to 100%)

names(predictions_df)[names(predictions_df) == 'ID'] <- 'idcase'

database2 <- merge(database, predictions_df, by = c("idcase"))   


# We can see the market share of all the 5 depvar
GC<-sum(database2$gc) / nrow(database2)
GR<-sum(database2$gr) / nrow(database2)
EC<-sum(database2$ec) / nrow(database2)
ER<-sum(database2$er) / nrow(database2)
HP<-sum(database2$hp) / nrow(database2)

#Let's see the individual elasticity for the operative cost on each dependent variable
# [Discrete Choice Note]: "Own Elasticity" measures how sensitive a choice is to its own price. 
# Here, it asks: "If the operational cost of Gas Central goes up by 1%, by what percentage does 
# the probability of choosing Gas Central go down?"
database2$own.elas_oc.gc <- model4[["estimate"]][["b_oc"]] * database2$oc.gc * (1 - database2$gc) #percentage decrease 
#in the probability to choose a gas central heating system after a 1% increase in the operative cost

#the others own elasticity for an increase in the Operative Cost:
database2$own.elas_oc.gr <- model4[["estimate"]][["b_oc"]] * database2$oc.gr * (1 - database2$gr)
database2$own.elas_oc.ec <- model4[["estimate"]][["b_oc"]] * database2$oc.ec * (1 - database2$ec)
database2$own.elas_oc.er <- model4[["estimate"]][["b_oc"]] * database2$oc.er * (1 - database2$er)
database2$own.elas_oc.hp <- model4[["estimate"]][["b_oc"]] * database2$oc.hp * (1 - database2$hp)

#Mean own elasticities
mean_own.elas_oc.gc<-mean(database2$own.elas_oc.gc) 
mean_own.elas_oc.gr<-mean(database2$own.elas_oc.gr)
mean_own.elas_oc.ec<-mean(database2$own.elas_oc.ec)
mean_own.elas_oc.er<-mean(database2$own.elas_oc.er)
mean_own.elas_oc.hp<-mean(database2$own.elas_oc.hp)

mean_own.elas_oc.gc 
mean_own.elas_oc.gr
mean_own.elas_oc.ec
mean_own.elas_oc.er
mean_own.elas_oc.hp





#Cross elasticity at the indivudal level with respect to the Operational Cost 
#for the Gas Central alternative
# [Discrete Choice Note]: "Cross Elasticity" looks at competitors. 
# If Gas Central's cost goes up, by what percentage does the probability of choosing 
# something ELSE (like Gas Room) go UP?
database2$cross.elas_oc.gc <- -model4[["estimate"]][["b_oc"]] * database2$oc.gc * database2$gc
database2$cross.elas_oc.gc #this represent the increase (in probability) of  
#the probability to choose the other choices for an increase in cost of 1%

#The mean value of the Cross elasticity is:
mean(database2$cross.elas_oc.gc)

#then repeat for every choice
database2$cross.elas_oc.gr <- -model4[["estimate"]][["b_oc"]] * database2$oc.gr * database2$gr
mean(database2$cross.elas_oc.gr)

database2$cross.elas_oc.ec <- -model4[["estimate"]][["b_oc"]] * database2$oc.ec * database2$ec
mean(database2$cross.elas_oc.ec)

database2$cross.elas_oc.er <- -model4[["estimate"]][["b_oc"]] * database2$oc.er * database2$er
mean(database2$cross.elas_oc.er)

database2$cross.elas_oc.hp <- -model4[["estimate"]][["b_oc"]] * database2$oc.hp * database2$hp
mean(database2$cross.elas_oc.hp)

#Let's see the scenario for an increase of the Operational Cost for 1%
# [Discrete Choice Note]: This is a "Scenario Simulation". The company wants to know what physically 
# happens to the market if they raise the operational cost of Gas Central by 1%. 
# We multiply the current gc cost by 1.01 and ask the model to predict choices again.
database$oc.gc<- 1.01 * database$oc.gc

apollo_inputs = apollo_validateInputs()

# Calculate the new probabilities
predictions_new = apollo_prediction(model4, apollo_probabilities, apollo_inputs)

#Undo the process, to keep the original dataset intact
database$oc.gc<- database$oc.gc/1.01
apollo_inputs = apollo_validateInputs()

predictions_new.df <- as.data.frame(predictions_new)

#just to ceck:
predictions_new.df %>% slice(1) %>% select(-ID, -Observation, -chosen) %>%
rowSums() # the result is 1, so everything is fine

names(predictions_new.df)[names(predictions_new.df) == 'ID'] <- 'idcase'

database3<-merge(database, predictions_new.df, by = c("idcase"))   

#New Market share (Aggregating the simulated probabilities to see the new landscape)

New_GC<-sum(database3$gc) / nrow(database3)
New_GR<-sum(database3$gr) / nrow(database3)
New_EC<-sum(database3$ec) / nrow(database3)
New_ER<-sum(database3$er) / nrow(database3)
New_HP<-sum(database3$hp) / nrow(database3)

New_GC
New_GR
New_EC
New_ER
New_HP



#The new market share VS old market share for GC
GC
New_GC
#The share changes slightly less than the 3%

#Own Elasticity of Gas Central with the formula for the new scenario:
log(New_GC / GC) / log(1.01) #=-0.4320221

#As you can see the value is very similar to the one that we have calculate before
#mean_own.elas_oc.gc=-0.4325582

#Cross Elasticity for every other alternative with the new scenario:
log(New_GR/ GR) / log(1.01)
log(New_EC/ EC) / log(1.01)
log(New_ER/ ER) / log(1.01)
log(New_HP/ HP) / log(1.01)



#We can see an example of how the mean elasticities change in some strata of population
# [Discrete Choice Note]: We can filter the data by demographics (young vs old) to see which group 
# is more elastic (price sensitive). Highly elastic users will abandon a product much faster when prices rise.

GC.old<-sum(database2$gc[(database2$old==1)]) / sum(database2$old==1)
GC.young<-sum(database2$gc[(database2$old==0)]) / sum(database2$old==0)
New_GC.old<-sum(database3$gc[(database3$old==1)]) / sum(database3$old==1)
New_GC.young<-sum(database3$gc[(database3$old==0)]) / sum(database3$old==0)

log(New_GC.old / GC.old) / log(1.01)#= -0.4288973
log(New_GC.young / GC.young) / log(1.01) #= -0.4341919