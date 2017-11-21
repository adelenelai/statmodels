## using a gam object obtained from {mgcv}

unadj <- function(gam.object){
	adjR2 <- summary(gam.object)$r.sq ###gam objects give adj R2
	num <- -((adjR2-1)*(nrow(gam.object$model)-ncol(gam.object$model)-1)-nrow(gam.object$model) + 1)#subtract additional 1 because ncol includes response
	denom <- 1-nrow(gam.object$model)
	R2 <- num/denom
	print(R2)

}
