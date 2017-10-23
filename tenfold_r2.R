### cross-validate the training set
tr <- function(fit){  
        
        dat <- fit$model
        ###RMSE
        pred <- predict(fit, data=dat)
        #print(pred.rmse)
        spe <- (dat$logDT50 - pred)^2
        RMSE <- sqrt(mean(spe))

        ###MAE
        ae <- abs(dat$logDT50 - pred)
        MAE <- mean(ae)

        ###R2 and residual df
        if(class(fit)=="lm"){
        R2<- summary(fit)$r.sq ###if lm.object
        adjR2 <- summary(fit)$adj.r.sq ###if lm.object
        #trres.df <- summary.lm(fit)$df[[2]] ###if lm.object
            
        } else{ ###if gam.object
            R2 <- 1-sum(resid(fit)^2)/sum((dat$logDT50-mean(dat$logDT50))^2)
            adjR2 <- summary(fit)$r.sq ###gives adj r2 by 'default' from summary
            #trres.df <- summary.gam(fit)$residual.df 
            }

    results <- signif(as.matrix(cbind(R2, adjR2, RMSE, MAE)),3)
    model <- toString(fit$call)
    #results2 <- cbind.data.frame(as.string(fit$call),results)
    table <- cbind(model, results)
    table
    
    
} 


##########
##########
##########

### 10-fold cross-validation
tf <- function(fit){  

	###randomly shuffle data
	set.seed(33)
	dat <- fit$model[sample(nrow(fit$model)),]
	tenpred <- c()
    obs.ordered <- c()
	fo <- 10 ###no. of folds
	sb <- round(seq(0,nrow(dat),length.out=(fo+1))) ###how folds will be split e.g. 1 5 10 15 20 25 30 35 40 45 50
    mae <- c()
    rmse <- c()
	for (i in 1:fo) {
        tes <- (sb[((fo+1)-i)]+1): (sb[((fo+2)-i)])
        #print(tes)
    	train <-(1:nrow(dat))[-tes]
        #print(train)
    	fit2 <- update(fit, data=dat[train,])
    	pred <- predict(fit2, newdata=dat[tes,]) ###predicted values of test set using model for each fold


    	tenpred <- c(tenpred,pred) ### create vector of predicted values looping over each fold
        obs.ordered <- rbind(obs.ordered,dat[tes,]) ### reorder dat to match pred vector order

        ###rmse
        rmse <- c(rmse,(dat$logDT50[tes]-pred)^2)     

        ###mae

        mae <- c(mae,abs(dat$logDT50[tes]-pred))
    } 
    
    RMSE <- signif(sqrt(mean(rmse)),2)
    MAE <- signif(mean(mae),2)
    R2_unrounded <- cor(tenpred,obs.ordered[,"logDT50"], method="pearson")^2
    R2 <- signif(R2_unrounded,2)


    num <- (1-R2_unrounded)*(nrow(fit$model)-1)
    denom <- nrow(fit$model)-ncol(fit$model)-1-1
    adjR2 <- signif((1-(num/denom)),2)

    model <- toString(fit$call)
    cbind(model,R2,adjR2,RMSE,MAE)

    
}
##########
##########
##########
### Leave-one-out cross-validation 
loo<- function(fit){  

    ###randomly shuffle data
    #set.seed(33)
    #dat <- fit$model[sample(nrow(fit$model)),]
    dat <- fit$model
    tenpred <- c()
    obs.ordered <- c()
    fo <- nrow(fit$model) ###no. of folds
    sb <- round(seq(0,nrow(dat),length.out=(fo+1))) ###how folds will be split e.g. 1 5 10 15 20 25 30 35 40 45 50
    rmse <- c()
    mae <- c()
    for (i in 1:fo) {
        tes <- (sb[((fo+1)-i)]+1): (sb[((fo+2)-i)])
        #print(tes)
        train <-(1:nrow(dat))[-tes]
        #print(train)
        fit2 <- update(fit, data=dat[train,])
        pred <- predict(fit2, newdata=dat[tes,]) ###predicted values of test set using model for each fold


        tenpred <- c(tenpred,pred) ### create vector of predicted values looping over each fold
        obs.ordered <- rbind(obs.ordered,dat[tes,]) ### reorder dat to match pred vector order
        #print(obs.ordered)
        ###rmse
        rmse <- c(rmse,(dat$logDT50[tes]-pred)^2)     


        ###mae
        mae <- c(mae,abs(dat$logDT50[tes]-pred))
    } 
    

    RMSE <- signif(sqrt(mean(rmse)),2)
    #print(RMSE)
    MAE <- signif(mean(mae),2)
    R2_unrounded <- cor(tenpred,obs.ordered[,"logDT50"], method="pearson")^2
    R2 <- signif(R2_unrounded,2)


    num <- (1-R2_unrounded)*(nrow(fit$model)-1)
    denom <- nrow(fit$model)-ncol(fit$model)-1-1
    adjR2 <- signif((1-(num/denom)),2)

    model <- toString(fit$call)
    cbind(model,R2,adjR2,RMSE,MAE)

    
}

##############################
#############################
#### Same functions as above, but for ZnormDT50
##############################
#############################


trz <- function(fit){  
        
        dat <- fit$model
        ###RMSE
        pred <- predict(fit, data=dat)
        #print(pred.rmse)
        spe <- (dat$znormlogDT50 - pred)^2
        RMSE <- sqrt(mean(spe))

        ###MAE
        ae <- abs(dat$znormlogDT50 - pred)
        MAE <- mean(ae)

        ###R2 and residual df
        if(class(fit)=="lm"){
        R2<- summary(fit)$r.sq ###if lm.object
        adjR2 <- summary(fit)$adj.r.sq ###if lm.object
        #trres.df <- summary.lm(fit)$df[[2]] ###if lm.object
            
        } else{ ###if gam.object
            R2 <- 1-sum(resid(fit)^2)/sum((dat$znormlogDT50-mean(dat$znormlogDT50))^2)
            adjR2 <- summary(fit)$r.sq ###gives adj r2 by 'default' from summary
            #trres.df <- summary.gam(fit)$residual.df 
            }

    results <- signif(as.matrix(cbind(R2, adjR2, RMSE, MAE)),3)
    model <- toString(fit$call)
    #results2 <- cbind.data.frame(as.string(fit$call),results)
    table <- cbind(model, results)
    table
    
    
} 


##########
##########
##########

tfz <- function(fit){  

    ###randomly shuffle data
    set.seed(33)
    dat <- fit$model[sample(nrow(fit$model)),]
    tenpred <- c()
    obs.ordered <- c()
    fo <- 10 ###no. of folds
    sb <- round(seq(0,nrow(dat),length.out=(fo+1))) ###how folds will be split e.g. 1 5 10 15 20 25 30 35 40 45 50
    mae <- c()
    rmse <- c()
    for (i in 1:fo) {
        tes <- (sb[((fo+1)-i)]+1): (sb[((fo+2)-i)])
        #print(tes)
        train <-(1:nrow(dat))[-tes]
        #print(train)
        fit2 <- update(fit, data=dat[train,])
        pred <- predict(fit2, newdata=dat[tes,]) ###predicted values of test set using model for each fold


        tenpred <- c(tenpred,pred) ### create vector of predicted values looping over each fold
        obs.ordered <- rbind(obs.ordered,dat[tes,]) ### reorder dat to match pred vector order

        ###rmse
        rmse <- c(rmse,(dat$znormlogDT50[tes]-pred)^2)     

        ###mae

        mae <- c(mae,abs(dat$znormlogDT50[tes]-pred))
    } 
    
    RMSE <- signif(sqrt(mean(rmse)),2)
    MAE <- signif(mean(mae),2)
    R2_unrounded <- cor(tenpred,obs.ordered[,"znormlogDT50"], method="pearson")^2
    R2 <- signif(R2_unrounded,2)


    num <- (1-R2_unrounded)*(nrow(fit$model)-1)
    denom <- nrow(fit$model)-ncol(fit$model)-1-1
    adjR2 <- signif((1-(num/denom)),2)

    model <- toString(fit$call)
    cbind(model,R2,adjR2,RMSE,MAE)

    
}
##########
##########
##########

looz<- function(fit){  

    ###randomly shuffle data
    #set.seed(33)
    #dat <- fit$model[sample(nrow(fit$model)),]
    dat <- fit$model
    tenpred <- c()
    obs.ordered <- c()
    fo <- nrow(fit$model) ###no. of folds
    sb <- round(seq(0,nrow(dat),length.out=(fo+1))) ###how folds will be split e.g. 1 5 10 15 20 25 30 35 40 45 50
    rmse <- c()
    mae <- c()
    for (i in 1:fo) {
        tes <- (sb[((fo+1)-i)]+1): (sb[((fo+2)-i)])
        #print(tes)
        train <-(1:nrow(dat))[-tes]
        #print(train)
        fit2 <- update(fit, data=dat[train,])
        pred <- predict(fit2, newdata=dat[tes,]) ###predicted values of test set using model for each fold


        tenpred <- c(tenpred,pred) ### create vector of predicted values looping over each fold
        obs.ordered <- rbind(obs.ordered,dat[tes,]) ### reorder dat to match pred vector order
        #print(obs.ordered)
        ###rmse
        rmse <- c(rmse,(dat$znormlogDT50[tes]-pred)^2)     


        ###mae
        mae <- c(mae,abs(dat$znormlogDT50[tes]-pred))
    } 
    

    RMSE <- signif(sqrt(mean(rmse)),2)
    #print(RMSE)
    MAE <- signif(mean(mae),2)
    R2_unrounded <- cor(tenpred,obs.ordered[,"znormlogDT50"], method="pearson")^2
    R2 <- signif(R2_unrounded,2)


    num <- (1-R2_unrounded)*(nrow(fit$model)-1)
    denom <- nrow(fit$model)-ncol(fit$model)-1-1
    adjR2 <- signif((1-(num/denom)),2)

    model <- toString(fit$call)
    cbind(model,R2,adjR2,RMSE,MAE)

    
}