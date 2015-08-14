#'Output module: ResponseCurve
#'
#'Plot a detailed conditional response curve from the model
#' against one covariate.
#'
#'@param model  
#'@param ras  
#'@param cov which of the covariates to plot the response against
#'@name ResponseCurve
ResponseCurve <- function (model, ras, cov = 1) {
  
  rescale <- function (x) {
    # scale to 0/1 for colour schemes
    x <- x - min(x)
    x / max(x)
  }
  
  # extract covariates matrix
  covars <- model$data[, 7:ncol(model$data), drop = FALSE]
  
  # name of key covariate
  name <- colnames(covars)[cov]
  
  # get covariate of interest
  covar <- covars[, cov]
  
  # ~~~~~~~~~~~~~~~~~~~~
  # make predictions for all covariates
  
  
  #select how many pred points to use
  Ntestpoints = 500
  #get the number of coefficients in the model
  Ncoff <- ncol(covars)		#	length(coefficients(model$model))-1 
  # create a dummy data frame
  Epred <- as.data.frame( matrix(0, nrow=Ntestpoints , ncol=Ncoff) )
  colnames(Epred) <- colnames(covars)
  # make a copy where the value is held for all others, and one for predictions
  Eres <- Emeds <- Epred
  
  # for each variable
  for(jj in 1:Ncoff){
    # fill in the median value for all varaibles
    Emeds[jj] <- rep(median(covars[[jj]], na.rm = TRUE), dim(Emeds)[1])
    # create a sequence form low to high for the varaibale of interest
    Emin <- min(covars[[jj]], na.rm = TRUE)
    Emax <- max(covars[[jj]], na.rm = TRUE)
    Epred[jj] <- seq(Emin, Emax, by = (Emax-Emin)/(Ntestpoints -1) )
  }
  
  for(kk in 1: Ncoff){
    # select which predictor variable 
    Etest <- Emeds
    # Add in variation for variable of focus
    Etest[kk] <- Epred[kk]
    # make predictions
    Eres[kk] <- predict(model$model, Etest, type = 'response')
    
  }	
  
  
  
  
  
  
  
  
  # ~~~~~~~~~~~~~~~~~~~~
  # plotting
  
  # formatting
  cexlab  = 1.3	# subtitles
  cexlab2 = 1.8	# main title
  cexlab3 = 1.2		# axis label
  coltits = c("grey10")
  coltits2 = c("grey30")	#
  colaxes = c("grey50")
  colpres = c("grey10")	#colours()[495]
  colpseu = c("grey10")	#colours()[433]
  
  ###___ layout 
  
  lay <- matrix(8, nrow=10, ncol=4)
  #graphs
  lay[2:6, 2:4] <- 2
  lay[7:8, 2:4] <- 4
  lay[9:10, 2:4] <- 6
  #side panels for subtitles
  lay[2:6, 1] <- 1
  lay[7:8, 1] <- 3
  lay[9:10, 1] <- 5
  # top panel for title
  lay[1, 2:4] <- 7	
  layout(lay) 
  
  ###___
  
  # transform var to 0-1 range for colour schemes.
  covar_scale <- rescale(covar)
  # get range of values for var
  covar_min <- min( covar)
  covar_max <- max( covar )
  # get subsets for both presences and background points
  pres_idx <- which(model$data$type=="presence")
  back_idx <- which(model$data$type=="background")
  
  ###___ Plot RESPONSE FUNCTION
  par(mar=c(0,1,1,1))
  plot(-99,-99, xlim=c(1,100), ylim=c(1,100), cex=0, main="", axes = FALSE, xlab="", ylab="")
  #points(50,50,pch=19, cex=10)
  text(1,99, labels="Response", col=coltits2, adj=0, cex= cexlab)
  text(1,95, labels="Function", col=coltits2, adj=0, cex= cexlab)
  text(85, 50, labels="prediction", col=coltits, cex=cexlab3, srt=90, font=3)
  
  #PLOT number 2 for lay
  e1 <- Epred[, cov]
  p1 <- Eres[, cov]
  
  plot(e1,p1, cex=0, axes = FALSE, xlab="", ylab="", ylim = 0:1)
  lines(e1,p1, col="grey10", lwd=4)
  lines(e1,p1, col="white", lwd=2)
  axis(1, at= seq( covar_min, covar_max, by=10), las=3, col=colaxes)
  axis(2, col=colaxes, las=2)
  axis(3, at= seq( covar_min, covar_max, by=10), las=3, col=colaxes)
  box(col=colaxes)
  
  ###___ HIST FOR PRESENCE DATA
  par(mar=c(0.5,1,4,1))
  plot(-99,-99, xlim=c(1,100), ylim=c(1,100), 
       cex=0, main="", axes = FALSE, xlab="", ylab="")
  #points(50,50,pch=19, cex=10)
  text(1,90, labels="Presence", col=coltits2, adj=0, cex= cexlab)
  text(1,75, labels="Data", col=coltits2, adj=0, cex= cexlab)
  text(85, 50, labels="denisty", col=coltits, cex=cexlab3, srt=90, font=3)
  
  # PLOT number 4 for lay
  hist(covar[pres_idx], breaks=50, xlim=c(covar_min, covar_max), axes = FALSE, main="", 
       col=colpres, border = FALSE)
  axis(1, at= seq( covar_min, covar_max, by=10), labels = FALSE, col=colaxes)
  axis(2, col=colaxes, las=2)
  axis(3, at= seq( covar_min, covar_max, by=10), labels = FALSE, tck=-0.25, col=colaxes)
  box(col=colaxes)
  
  ###___ HIST FOR BACKGROUND DATA
  par(mar=c(4,1,0.5,1))
  plot(-99,-99, xlim=c(1,100), ylim=c(1,100), cex=0, main="", axes = FALSE, xlab="", ylab="")
  #points(50,50,pch=19, cex=10)
  text(1,90, labels="Background", col=coltits2, adj=0, cex= cexlab)
  text(1,75, labels="Data", col=coltits2, adj=0, cex= cexlab)
  text(85, 50, labels="density", col=coltits, cex=cexlab3, srt=90, font=3)
  
  # PLOT number 6 for lay
  hist(covar[back_idx], breaks=50, xlim=c(covar_min, covar_max), axes = FALSE, main="", 
       xlab="", ylab="F", las=2, 
       col=colpseu, border = FALSE)
  axis(1, at= seq( covar_min, covar_max, by=10), las=3, col=colaxes)
  axis(2, col=colaxes, las=2)
  axis(3, at= seq( covar_min, covar_max, by=10), labels = FALSE, col=colaxes)
  box(col=colaxes)
  
  ###___ TITLE
  par(mar=c(0,1,0,1))
  plot(-99,-99, xlim=c(1,100), ylim=c(1,100), cex=0, main="", axes = FALSE, xlab="", ylab="")
  text(1,90, labels=c(name), 
       col=coltits, adj=0, cex= cexlab2, font=2)
  
}