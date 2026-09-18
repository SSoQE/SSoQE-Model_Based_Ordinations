###################################################
# September 18th 2026
# by Manuel
# Code from github.com/orgs/SSoQE
# Inspired by https://jenniniku.github.io/gllvm/index.html
# Aim:
# Analysing complex community data using model based ordination
#
###################################################

# install.packages("gllvm")
# install.packages("mvabund")
# install.packages("corrplot")
# install.packages("vegan")
library("vegan")
library("gllvm")
library("mvabund")
library("corrplot")

############################################
#Data example: Spiders
data("spider", package = "mvabund")
# Abundances of 12 hunting spider species measured as a count at 28 sites.
spec <- spider$abund
head(spec)

####
# Six predictor variables #
env <- scale(spider$x) #scaled predictors
head(env)
# Six environmental variables measured at each site.
#       soil.dry: Soil dry mass
#        bare.sand: cover of bare sand
#        fallen.leaves: cover of fallen leaves/twigs
#        moss: cover of moss
#        herb.layer: cover of herb layer
#        reflection: reflection of the soil surface with a cloudless sky
############################################

###########################
# Correspondence Analysis #
###########################
ca<-cca(spec)
plot(ca)
vegan::scores(ca)$sites
###########################

######################
# Canonical Correspondence Analysis (CCA) #
cca<-cca(spec ~ soil.dry + reflection ,data= as.data.frame(env))
cca
plot(cca)
######################

############################################
# R package gllvm fits Generalized linear latent variable models (GLLVM) for multivariate data

# gllvm(y = NULL, X = NULL, family, num.lv = 2,  formula = NULL)
# y: species abundances
# X: environmental variables
# family: distribution for responses
# num.lv: number of latent variables
# formula: model formula e.g. = ~ soil.dry + reflection
############################################


############################################
# Task A1: Fit a GLLVM for the spider community without environmental predictors and with two latent variables. Start with a Poisson response distribution. Why might a negative-binomial distribution also be considered?
# Task A2: Use gllvm::ordiplot() to visualise the latent variables
# Task A3: Extract and visualise the residual correlation matrix using getResidualCor(). What do these residual correlations represent? Are they evidence for direct biological interactions between species?
# Task A4: Use corrplot() to visualise the residual correlation matrix
#####
## Put your code here

###
############################################


############################################
# Task B1: Now, fit the spider community with a model that only includes soil.dry and reflection as predictor variables (no latent variables).
# Task B2: Look at the model. Extract the coefficients for the predictors and visualise them with coefplot()
# Task B3: How does this multivariate GLLVM with num.lv = 0 differ from fitting separate univariate GLMs for each species?
# Task B4: Try to extract the residual correlation matrix using getResidualCor(). Why is a residual correlation structure not available for this model?
#####
## Put your code here

###



############################################
# Task C1: Now, fit the spider community with a model that includes soil.dry and reflection as predictor variables and 2 latent variables (num.lv = 2). Make sure that the model has converged successfully.
# Task C2: Compare the ordination with the corresponding model from Task A. What changed after accounting for environmental predictors?
# Task C3: Compare the residual correlation matrices between the models from Tasks A and C. Which residual species associations remain after accounting for the environmental predictors?
# Task C4: Compare the models with and without latent variables using AIC. What does the change in AIC tell you about the additional latent structure?
#####
## Put your code here



###############################################


############################################
# Bonus question 1: How could I include quadratic relationships with the environmental data

############################################
# Bonus question 2: Constrained ordination

# GLLVM can also be used for constrained ordination, where environmental variables inform the ordination axes.

mod_RR <- gllvm(spec, env, family = "poisson", num.RR = 2, lv.formula = ~ bare.sand + fallen.leaves + moss + herb.layer + reflection, formula = ~ soil.dry)
gllvm::ordiplot(mod_RR)

# Question: How does this constrained ordination differ from the unconstrained ordination with latent variables?

############################################
