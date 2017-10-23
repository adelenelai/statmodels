#### Based on {pls} 

###FIt model and CHoose ncomp
fich <- function(mydata){
library(pls)

###fit models
## https://stat.ethz.ch/pipermail/r-help/2004-February/045457.html  #centering/scaling
###p12 of {pls}: predictor variables centered as part of fit alg.
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="LOO") ##LOO

#summary(train)
#summary(tenfold) ###confirm that doing tenfold "10 consecutive segments" ##assumes data are random; do so to ensure reproducibility when calculating RMSE and MAE
#summary(loo)    ###confirm that doing LOO "nrow(mydata) random segments"
###CrossVal plots to choose ncomp
par(bg="white",mfrow=c(3,1),ann=F)
plot(RMSEP(train))
title(main="Training",xlab="ncomp",ylab="RMSEP")
plot(RMSEP(tenfold))
title(main="10-fold",xlab="ncomp",ylab="RMSEP")
plot(RMSEP(loo))
title(main="LOO",xlab="ncomp",ylab="RMSEP")


#################################
########script to select the number of components with lowest RMSE
########in practice, results cannot be trusted because local minimum not always selected
######## human must judge what local min is!!##########
#################################

###find (cross-validated) no. of components to use; USE LOCAL MINIMUM
#delta.train <- diff(RMSEP(train)$val)
#ncomp.train <- which(delta.train==min(delta.train))
#ncomp.train <- 6 ###is this the local minimum or is 2?
#print(ncomp.train)

#for tenfold and loo, use adjCV (bias-corrected)
#ind1 <- seq(from=2,to=length(RMSEP(tenfold)$val),by=2)
#print(RMSEP(tenfold)$val)
#delta.tenfold <- diff(RMSEP(tenfold)$val[ind1])
#print(delta.tenfold)
#ncomp.tenfold <- which(delta.tenfold==min(delta.tenfold))
#print(ncomp.tenfold)

#ind2 <- seq(from=2,to=length(RMSEP(loo)$val),by=2)
#delta.loo <- diff(RMSEP(loo)$val[ind2])
#ncomp.loo <- which(delta.loo==min(delta.loo))
#print(ncomp.loo)

#ncomp.train <- #gives index of minimum;-1 to account for intercept
#ncomp.tenfold <- which.min(RMSEP(tenfold,estimate="CV")$val)-1 #gives index of minimum;-1 to account for intercept
#ncomp.loo <- which.min(RMSEP(loo,estimate="CV")$val)-1 #gives index of minimum;-1 to account for intercept
#################################
#################################
}

###PRedict and EXtract, with user-specified ncomps
prex <- function(mydata,ncomp.train,ncomp.tenfold,ncomp.loo){ 

###fit models again
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="LOO") ##LOO

###pred vs. measured plots of tenfold and LOO
par(bg="white",mfrow=c(2,1),ann=F)
predplot(tenfold,which="validation",ncomp=ncomp.tenfold,line=T) 
points(predict(tenfold,ncomp=ncomp.tenfold)~mydata$logDT50,col="red") ##plots the training points
title(main=paste("10-fold CV of logDT50,",ncomp.tenfold,"comps"),xlab="measured",ylab="predicted")
#
predplot(loo,which="validation",ncomp=ncomp.loo,line=T)
points(predict(loo,ncomp=ncomp.loo)~mydata$logDT50,col="red") ##plots the training points
title(main=paste("LOOCV of logDT50,",ncomp.loo,"comps"),xlab="measured",ylab="predicted")

###extract R2,RMSE,MAE of the chosen ncomp
mae_train <- mean(abs(predict(train,comp=ncomp.train)-train$model[,1]))
Training <- c(pls::R2(train)$val[[ncomp.train+1]],
	RMSEP(train)$val[[2*ncomp.train+2]],mae_train)
#print(tenfold$model[,1])
#print(loo$model[,1])
#mae_tenfold <- mean(abs(predict(tenfold,comp=ncomp.tenfold)-tenfold$model[,1])) ###gives training set somehow
mae_tenfold <- mean(abs(tenfold$validation$pred[,,ncomp.tenfold,drop=F]-tenfold$model[,1])) ##see line 405 https://github.com/cran/pls/blob/master/R/plots.R
Tenfold <- c(pls::R2(tenfold)$val[[ncomp.tenfold+1]], 
	RMSEP(tenfold)$val[[2*ncomp.tenfold+2]],mae_tenfold)

#mae_loo <- mean(abs(predict(loo,comp=ncomp.loo)-loo$model[,1])) ###gives training set somehow
mae_loo <- mean(abs(loo$validation$pred[,,ncomp.loo,drop=F]-loo$model[,1]))
LOO <- c(pls::R2(loo)$val[[ncomp.loo+1]],
	RMSEP(loo)$val[[2*ncomp.loo+2]],mae_loo)

table <- rbind(Training,Tenfold,LOO)
colnames(table) <- c("R2", "RMSE", "MAE")
print(signif(as.matrix(table),2))

###Score plot to diagnose outliers, groups, patterns; same for training, tenfold, LOO
par(bg='white')
plot(train,plottype="scores",comps=1:3,main="Training")
#plot(tenfold,plottype="scores",comps=1:3,main="Tenfold")
#plot(loo,plottype="scores",comps=1:3,main="LOO")
}


#################################
#################################


###Find VAriance explained and plot COrrelation loadings plot
vaco<- function(mydata){ 
###fit models again
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="LOO") ##LOO



###Correlation loadings plot - to see correlations between each var and selected components - same for training, tenfold, loo
par(bg="white")
corrplot(train,labels="names",main="Training")
#corrplot(tenfold$validation,labels="names")
#corrplot(loo$validation,labels="names")

####extract the correlations between variables and components
###from line 257 https://github.com/cran/pls/blob/master/R/plots.R
S <- scores(train)[,1:2, drop = FALSE]
cl <- round(cor(model.matrix(train), S),2)
print(cl)

###Extract explained variance
vartable <- rbind(explvar(train),explvar(tenfold),explvar(loo))
vartable <- signif(as.matrix(vartable),2)
rownames(vartable) <- c("Training","Tenfold", "LOO")
print(vartable)
}

###output the reg. coef#################regression coefficients not very important!
#TrCoef <- coef(train,ncomp=ncomp.train,intercept=TRUE)[1:(ncomp.train+1)] ###+1 to add intercept
#TeCoef <- coef(tenfold,ncomp=ncomp.tenfold,intercept=TRUE)[1:(ncomp.tenfold+1)] ###+1 to add intercept
#LoCoef <- coef(loo,ncomp=ncomp.loo,intercept=TRUE)[1:(ncomp.loo+1)] ###+1 to add intercept
#coefftable <- rbind(TrCoef,TeCoef,LoCoef)
#colnames(coefftable) <- c("Intercept","Comp1","Comp2",) ##no way of knowing how many comps I will have!
#print(signif(as.matrix(coefftable),2))

###plot(train,plottype="coef",ncomp=1:3) ###coefficients plot
#print(Training)
################################
###############################
################################
###############################
################################
###############################
################################
###############################
### Same as above, but with no Moisture

fich.nomo <- function(mydata){ ##fit model and choose ncomp
library(pls)

###fit models
## https://stat.ethz.ch/pipermail/r-help/2004-February/045457.html  #centering/scaling
###p12 of {pls}: predictor variables centered as part of fit alg.
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2+ cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="LOO") ##LOO

summary(train)
summary(tenfold) ###confirm that doing tenfold "10 consecutive segments" ##assumes data are random; do so to ensure reproducibility when calculating RMSE and MAE
summary(loo)    ###confirm that doing LOO "nrow(mydata) random segments"
###CrossVal plots to choose ncomp
par(bg="white",mfrow=c(3,1),ann=F)
plot(RMSEP(train))
title(main="Training",xlab="ncomp",ylab="RMSEP")
plot(RMSEP(tenfold))
title(main="10-fold",xlab="ncomp",ylab="RMSEP")
plot(RMSEP(loo))
title(main="LOO",xlab="ncomp",ylab="RMSEP")


#################################
########cannot be trusted, human must judge what local min is!!##########
#################################

###find (cross-validated) no. of components to use; USE LOCAL MINIMUM
#delta.train <- diff(RMSEP(train)$val)
#ncomp.train <- which(delta.train==min(delta.train))
#ncomp.train <- 6 ###is this the local minimum or is 2?
#print(ncomp.train)

#for tenfold and loo, use adjCV (bias-corrected)
#ind1 <- seq(from=2,to=length(RMSEP(tenfold)$val),by=2)
#print(RMSEP(tenfold)$val)
#delta.tenfold <- diff(RMSEP(tenfold)$val[ind1])
#print(delta.tenfold)
#ncomp.tenfold <- which(delta.tenfold==min(delta.tenfold))
#print(ncomp.tenfold)

#ind2 <- seq(from=2,to=length(RMSEP(loo)$val),by=2)
#delta.loo <- diff(RMSEP(loo)$val[ind2])
#ncomp.loo <- which(delta.loo==min(delta.loo))
#print(ncomp.loo)

#ncomp.train <- #gives index of minimum;-1 to account for intercept
#ncomp.tenfold <- which.min(RMSEP(tenfold,estimate="CV")$val)-1 #gives index of minimum;-1 to account for intercept
#ncomp.loo <- which.min(RMSEP(loo,estimate="CV")$val)-1 #gives index of minimum;-1 to account for intercept
#################################
#################################
}


prex.nomo <- function(mydata,ncomp.train,ncomp.tenfold,ncomp.loo){ ##predict and extract, with user-specified ncomps

###fit models again
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="LOO") ##LOO

###pred vs. measured plots of tenfold and LOO
par(bg="white",mfrow=c(2,1),ann=F)
predplot(tenfold,which="validation",ncomp=ncomp.tenfold,line=T) 
points(predict(tenfold,ncomp=ncomp.tenfold)~mydata$logDT50,col="red") ##plots the training points
title(main=paste("10-fold CV of logDT50,",ncomp.tenfold,"comps"),xlab="measured",ylab="predicted")
#
predplot(loo,which="validation",ncomp=ncomp.loo,line=T)
points(predict(loo,ncomp=ncomp.loo)~mydata$logDT50,col="red") ##plots the training points
title(main=paste("LOOCV of logDT50,",ncomp.loo,"comps"),xlab="measured",ylab="predicted")

###extract R2,RMSE,MAE of the chosen ncomp
mae_train <- mean(abs(predict(train,comp=ncomp.train)-train$model[,1]))
Training <- c(pls::R2(train)$val[[ncomp.train+1]],
    RMSEP(train)$val[[2*ncomp.train+2]],mae_train)
#print(tenfold$model[,1])
#print(loo$model[,1])
#mae_tenfold <- mean(abs(predict(tenfold,comp=ncomp.tenfold)-tenfold$model[,1])) ###gives training set somehow
mae_tenfold <- mean(abs(tenfold$validation$pred[,,ncomp.tenfold,drop=F]-tenfold$model[,1])) ##see line 405 https://github.com/cran/pls/blob/master/R/plots.R
Tenfold <- c(pls::R2(tenfold)$val[[ncomp.tenfold+1]], 
    RMSEP(tenfold)$val[[2*ncomp.tenfold+2]],mae_tenfold)

#mae_loo <- mean(abs(predict(loo,comp=ncomp.loo)-loo$model[,1])) ###gives training set somehow
mae_loo <- mean(abs(loo$validation$pred[,,ncomp.loo,drop=F]-loo$model[,1]))
LOO <- c(pls::R2(loo)$val[[ncomp.loo+1]],
    RMSEP(loo)$val[[2*ncomp.loo+2]],mae_loo)

table <- rbind(Training,Tenfold,LOO)
colnames(table) <- c("R2", "RMSE", "MAE")
print(signif(as.matrix(table),2))

###Score plot to diagnose outliers, groups, patterns
###they all look the same??
#par(bg="white",mfrow=c(2,2))
par(bg='white')
plot(train,plottype="scores",comps=1:3,main="Training")
#plot(tenfold,plottype="scores",comps=1:3,main="Tenfold")
#plot(loo,plottype="scores",comps=1:3,main="LOO")
}


#################################
#################################



vaco.nomo<- function(mydata){ ###correlation plot,variance explained
###fit models again
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + cec + OrgCont + Biomass_start,data=mydata,scale=T,validation="LOO") ##LOO



###Correlation loadings plot - to see correlations between each var and selected components
###they all look the same??
par(bg="white")
corrplot(train,labels="names",main="Training")
#corrplot(tenfold,labels="names")
#corrplot(loo,labels="names")

####extract the correlations between variables and components
###from line 257 https://github.com/cran/pls/blob/master/R/plots.R
S <- scores(train)[,1:2, drop = FALSE]
cl <- as.data.frame(round(cor(model.matrix(train), S),2))
print(cl)

###Extract explained variance
vartable <- rbind(explvar(train),explvar(tenfold),explvar(loo))
vartable <- signif(as.matrix(vartable),2)
rownames(vartable) <- c("Training","Tenfold", "LOO")
print(vartable)
}
################################
###############################
################################
###############################
###### Same as above, but with no Biomass_start

fich.nost <- function(mydata){ ##fit model and choose ncomp
library(pls)

###fit models
## https://stat.ethz.ch/pipermail/r-help/2004-February/045457.html  #centering/scaling
###p12 of {pls}: predictor variables centered as part of fit alg.
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="LOO") ##LOO

summary(train)
summary(tenfold) ###confirm that doing tenfold "10 consecutive segments" ##assumes data are random; do so to ensure reproducibility when calculating RMSE and MAE
summary(loo)    ###confirm that doing LOO "nrow(mydata) random segments"
###CrossVal plots to choose ncomp
par(bg="white",mfrow=c(3,1),ann=F)
plot(RMSEP(train))
title(main="Training",xlab="ncomp",ylab="RMSEP")
plot(RMSEP(tenfold))
title(main="10-fold",xlab="ncomp",ylab="RMSEP")
plot(RMSEP(loo))
title(main="LOO",xlab="ncomp",ylab="RMSEP")


#################################
########cannot be trusted, human must judge what local min is!!##########
#################################

###find (cross-validated) no. of components to use; USE LOCAL MINIMUM
#delta.train <- diff(RMSEP(train)$val)
#ncomp.train <- which(delta.train==min(delta.train))
#ncomp.train <- 6 ###is this the local minimum or is 2?
#print(ncomp.train)

#for tenfold and loo, use adjCV (bias-corrected)
#ind1 <- seq(from=2,to=length(RMSEP(tenfold)$val),by=2)
#print(RMSEP(tenfold)$val)
#delta.tenfold <- diff(RMSEP(tenfold)$val[ind1])
#print(delta.tenfold)
#ncomp.tenfold <- which(delta.tenfold==min(delta.tenfold))
#print(ncomp.tenfold)

#ind2 <- seq(from=2,to=length(RMSEP(loo)$val),by=2)
#delta.loo <- diff(RMSEP(loo)$val[ind2])
#ncomp.loo <- which(delta.loo==min(delta.loo))
#print(ncomp.loo)

#ncomp.train <- #gives index of minimum;-1 to account for intercept
#ncomp.tenfold <- which.min(RMSEP(tenfold,estimate="CV")$val)-1 #gives index of minimum;-1 to account for intercept
#ncomp.loo <- which.min(RMSEP(loo,estimate="CV")$val)-1 #gives index of minimum;-1 to account for intercept
#################################
#################################
}


prex.nost <- function(mydata,ncomp.train,ncomp.tenfold,ncomp.loo){ ##predict and extract, with user-specified ncomps

###fit models again
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="LOO") ##LOO

###pred vs. measured plots of tenfold and LOO
par(bg="white",mfrow=c(2,1),ann=F)
predplot(tenfold,which="validation",ncomp=ncomp.tenfold,line=T) 
points(predict(tenfold,ncomp=ncomp.tenfold)~mydata$logDT50,col="red") ##plots the training points
title(main=paste("10-fold CV of logDT50,",ncomp.tenfold,"comps"),xlab="measured",ylab="predicted")
#
predplot(loo,which="validation",ncomp=ncomp.loo,line=T)
points(predict(loo,ncomp=ncomp.loo)~mydata$logDT50,col="red") ##plots the training points
title(main=paste("LOOCV of logDT50,",ncomp.loo,"comps"),xlab="measured",ylab="predicted")

###extract R2,RMSE,MAE of the chosen ncomp
mae_train <- mean(abs(predict(train,comp=ncomp.train)-train$model[,1]))
Training <- c(pls::R2(train)$val[[ncomp.train+1]],
    RMSEP(train)$val[[2*ncomp.train+2]],mae_train)
#print(tenfold$model[,1])
#print(loo$model[,1])
#mae_tenfold <- mean(abs(predict(tenfold,comp=ncomp.tenfold)-tenfold$model[,1])) ###gives training set somehow
mae_tenfold <- mean(abs(tenfold$validation$pred[,,ncomp.tenfold,drop=F]-tenfold$model[,1])) ##see line 405 https://github.com/cran/pls/blob/master/R/plots.R
Tenfold <- c(pls::R2(tenfold)$val[[ncomp.tenfold+1]], 
    RMSEP(tenfold)$val[[2*ncomp.tenfold+2]],mae_tenfold)

#mae_loo <- mean(abs(predict(loo,comp=ncomp.loo)-loo$model[,1])) ###gives training set somehow
mae_loo <- mean(abs(loo$validation$pred[,,ncomp.loo,drop=F]-loo$model[,1]))
LOO <- c(pls::R2(loo)$val[[ncomp.loo+1]],
    RMSEP(loo)$val[[2*ncomp.loo+2]],mae_loo)

table <- rbind(Training,Tenfold,LOO)
colnames(table) <- c("R2", "RMSE", "MAE")
print(signif(as.matrix(table),2))

###Score plot to diagnose outliers, groups, patterns
###they all look the same??
#par(bg="white",mfrow=c(2,2))
par(bg='white')
plot(train,plottype="scores",comps=1:3,main="Training")
#plot(tenfold,plottype="scores",comps=1:3,main="Tenfold")
#plot(loo,plottype="scores",comps=1:3,main="LOO")
}


#################################
#################################



vaco.nost<- function(mydata){ ###correlation plot,variance explained
###fit models again
train <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="none") ##training
tenfold <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="CV",segment.type="consecutive") ##default 10-fold
loo <- plsr(logDT50~Silt + Clay + Temp + pHCaCl2 + Moisture + cec + OrgCont,data=mydata,scale=T,validation="LOO") ##LOO


###Correlation loadings plot - to see correlations between each var and selected components
###they all look the same??
par(bg="white")
corrplot(train,labels="names",main="Training")
#corrplot(tenfold$validation,labels="names")
#corrplot(loo$validation,labels="names")

####extract the correlations between variables and components
###from line 257 https://github.com/cran/pls/blob/master/R/plots.R
S <- scores(train)[,1:2, drop = FALSE]
cl <- round(cor(model.matrix(train), S),2)
print(cl)

###Extract explained variance
vartable <- rbind(explvar(train),explvar(tenfold),explvar(loo))
vartable <- signif(as.matrix(vartable),2)
rownames(vartable) <- c("Training","Tenfold", "LOO")
print(vartable)
}





#################################
#################################
### (*) As described in Chong, Il-Gyo & Jun, Chi-Hyuck, 2005, Performance of
### some variable selection methods when multicollinearity is present,
### Chemometrics and Intelligent Laboratory Systems 78, 103--112.

## VIP returns all VIP values for all variables and all number of components,
## as a ncomp x nvars matrix.
VIP <- function(object) {
    if (object$method != "oscorespls")
        stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
    if (nrow(object$Yloadings) > 1)
        stop("Only implemented for single-response models")

    SS <- c(object$Yloadings)^2 * colSums(object$scores^2)
    Wnorm2 <- colSums(object$loading.weights^2)
    SSW <- sweep(object$loading.weights^2, 2, SS / Wnorm2, "*")
    sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}


## VIPjh returns the VIP of variable j with h components
VIPjh <- function(object, j, h) {
    if (object$method != "oscorespls")
        stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
    if (nrow(object$Yloadings) > 1)
        stop("Only implemented for single-response models")

    b <- c(object$Yloadings)[1:h]
    T <- object$scores[,1:h, drop = FALSE]
    SS <- b^2 * colSums(T^2)
    W <- object$loading.weights[,1:h, drop = FALSE]
    Wnorm2 <- colSums(W^2)
    sqrt(nrow(W) * sum(SS * W[j,]^2 / Wnorm2) / sum(SS))
}
