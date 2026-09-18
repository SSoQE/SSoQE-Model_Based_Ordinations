library(vegan)

#######################
# Data ################
# Species abundances from sites in Finnland
data(varespec)
head(varespec)

# Environmental data 
data(varechem)
head(varechem)
#######################

###########################
# Correspondance Analysis #
###########################
ca<-cca(varespec)
plot(ca)
vegan::scores(ca)$sites
###########################

######################
# Canonical CA (CCA) #
cca<-cca(varespec ~ Ca + Al + Baresoil ,data=varechem)
cca
plot(cca)
######################

######################
# GLLVM no environment
mod_no_env <- gllvm(y = varespec, family = "poisson", num.lv = 2)

# Ordiplot
gllvm::ordiplot(mod1, biplot = TRUE, spp.arrows = FALSE)
abline(h = 0, v = 0, lty=2)

# GLLVM with environment 
varechem_scaled<-scale(varechem)

mod_with_env  <- gllvm(y = varespec, X=varechem_scaled, formula = ~ Ca + Al + Baresoil, family = "poisson", num.lv = 2)

coefplot(mod_with_env, which.Xcoef="Ca", xlim.list=list(c(-10,10)))

# coefplot(mod_with_env, mfrow = c(1,3), cex.ylab = 0.8)

# Compare residual associations
cr_sp <- getResidualCor(mod_no_env)
cr_env <- getResidualCor(mod_with_env)
par(mfrow=c(1,2))
corrplot::corrplot(cr_sp, diag = FALSE, type = "lower", method = "square", tl.srt = 25,main="no environment")
corrplot::corrplot(cr_env, diag = FALSE, type = "lower", method = "square", tl.srt = 25,main="with environment")
