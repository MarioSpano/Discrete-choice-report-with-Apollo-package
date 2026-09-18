rm(list = ls())

#### Load libraries and wd####

library(apollo)
library(tidyverse)
library(dplyr)

setwd("C:\\UNIBO\\Biondi\\Project")

#### Load database ####
data = read.csv("C:/UNIBO/Biondi/Project/heating_data.csv")

#https://rdrr.io/cran/mlogitBMA/man/heating.html

#These cost are conditional on the house having central air-conditioning. (That 
#is why the installation cost of gas central is lower than that for gas room: 
#the central system can use the air-conditioning products that have been installed.)

head(data)
#sum(is.na(database))=0, so we do not have missing values

database<-data
unique(database$region)


####Socio-Demographic variables####

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
database$young=  ifelse(database$agehed< 50, 1, 0)

#rooms
unique(database$rooms)

database$many_rooms = ifelse(database$rooms>= 5, 1, 0)
database$few_rooms =  ifelse(database$rooms< 5, 1, 0)



####Mixed logit#####
# [Discrete Choice Note]: The standard MNL assumes that everyone has the exact same baseline preferences 
# (if they have the same demographics). A "Mixed Logit" relaxes this assumption. It suggests that preference 
# is a distribution (like a bell curve) rather than a single fixed number, capturing hidden (unobserved) heterogeneity.
apollo_initialise()


apollo_control = list(
  modelName      = "Mixed_logit",
  indivID         = "idcase", 
  outputDirectory = "output",
  mixing =TRUE, # This tells the software to expect random distributed variables
  nCores=2 
)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c( mu_b_ic=0,
               sigma_b_ic=0,
               mu_b_oc=0,
               sigma_b_oc=0,
               b0_gc=0, b0_gr=0, b0_ec =0, b0_er =0, b0_hp =0,
               b_oc_income=0)
#as you can see we haved specified 2 parameters for each characteristic j, you know why
# [Discrete Choice Note]: For a standard model we just estimate 'b_ic'. In a Mixed Logit, because the parameter 
# is treated as a normal distribution, we must estimate its mean ('mu') and its standard deviation ('sigma' / variance).

apollo_fixed = c("b0_hp")


# [Discrete Choice Note]: Because Mixed Logit equations are highly complex and don't have straightforward math solutions, 
# they are estimated using simulation. 'Halton draws' are smart, structured random numbers used to simulate these distributions.
apollo_draws = list(
  interDrawsType = "halton", #type of draws, not for 5 or more attributes 
  interNDraws = 200,
  interNormDraws = c("draws_ic","draws_oc")
)


### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  # NORMAL
  # [Discrete Choice Note]: Here we explicitly define that the cost preferences equal the mean + (standard deviation * a random draw).
  randcoeff[["b_ic"]] = mu_b_ic + sigma_b_ic * draws_ic
  randcoeff[["b_oc"]] = mu_b_oc + sigma_b_oc * draws_oc
  return(randcoeff)
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  
  #interaction
  b_oc_shift = b_oc + b_oc_income * high_income
  
  #define utilities
  V[["gc"]] = b0_gc + b_ic* ic.gc + b_oc* oc.gc
  
  V[["gr"]] = b0_gr + b_ic* ic.gr + b_oc_shift * oc.gr 
  
  V[["ec"]] = b0_ec + b_ic* ic.ec + b_oc* oc.ec 
  
  V[["er"]] = b0_er + b_ic* ic.er + b_oc* oc.er
  
  V[["hp"]] = b0_hp + b_ic * ic.hp + b_oc* oc.hp 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(gc="gc", gr="gr", ec="ec", er="er", hp="hp"),
    choiceVar     = depvar,
    utilities     = V
  )
  
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Average over inter-individual draws
  # [Discrete Choice Note]: We simulate the choice process 200 times (our draws) for each person 
  # and average the results to get stable predicted probabilities.
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model6 = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, 
                         apollo_inputs)

# ################################################################# #
####---- FORMATTED OUTPUT (TO SCREEN and TO FILE ---####                       

modelOutput_settings=list(printPVal=2)
apollo_modelOutput(model6, modelOutput_settings)
apollo_saveOutput(model6, modelOutput_settings) 
#As you can see from the result the sigma_Beta of both of the predictors is not 
#significant so there is no inter-individual heterogeneity, use a Mixed Logit Model 
#is meaning less.
# [Discrete Choice Note]: The output showed that 'sigma_b_ic' and 'sigma_b_oc' were not statistically different from zero. 
# This means there is no measurable "spread" or variation in preference hidden in the population; everyone responds 
# to costs similarly (aside from the known income interaction). Therefore, the simpler MNL model is better!


#It's also meaningless try to do the "apollo_conditionals" command, because as 
#we have understand there is no difference in the population.