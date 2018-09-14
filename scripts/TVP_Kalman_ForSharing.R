# Load libraries
library(dlm)
library(dplR)
library(ggplot2)

# List vector of climate variables
input <- c("ppt6t1","ppt7t1","ppt5t1","ppt3t1","ppt2t1","ppt3t0","ppt6t0","ppt7t0","tmax7t1","tmax8t1","tmax6t1","tmax4t1","tmax5t1","tmax9t2","tmax10t2","tmax9t0","tmax8t0","tmax6t0","tmax5t0","tmin7t0","tmin8t0","tmin9t2","tmin10t2")

# Use read.rwl function from Andy Bunn's dplR package
chronAv <- read.rwl("dir\\filename.rwl",format="auto")

####################################################

######################################
#### Kalman Filter TVP Regression ####
####                              ####
####  Vars of interest:           ####
####    chronAv = mean chronology ####
####    input = vector of climate ####
####            variables         ####
####                              ####
######################################

for(g in input){
  # Function to build Time Varying Parameter model
  buildTVP <- function(parm,x.mat){
    parm <- exp(parm)
    return( dlmModReg(X=x.mat,dV=parm[1],dW=c(parm[2],parm[3])) )
  }

  start.vals <- c(0,0,0)
  var <- with(out,get(g))

  # Max Likelihood estimation of parameters
  TVP.mle <- dlmMLE(y=chronAv,parm=start.vals,x.mat=var,build=buildTVP,hessian=T)

  # Fitted state space model
  TVP.dlm <- buildTVP(TVP.mle$par,var)

  # Apply Kalman Filter (estimate current state given previous observations)
  TVP.f <- dlmFilter(chronAv,TVP.dlm)

  # Kalman smoothing
  TVP.s <- dlmSmooth(TVP.f)

  # Confidence Intervals
  alpha.s <- TVP.s$s[-1,1,drop=FALSE]
  beta.s <- TVP.s$s[-1,2,drop=FALSE]
  colnames(alpha.s) <- "alpha"
  colnames(beta.s) <- "beta"

  # Extract std errors for confidence intervals
  mse.list <- dlmSvd2var(TVP.s$U.S, TVP.s$D.S)
  se.mat <- t(sapply(mse.list, FUN=function(x) sqrt(diag(x))))
  se.mat <- dropFirst(se.mat)
  colnames(se.mat) <- c("alpha", "beta")
  a.u <- alpha.s + 1.96*se.mat[,"alpha"]
  a.l <- alpha.s - 1.96*se.mat[, "alpha"]
  b.u <- beta.s + 1.96*se.mat[,"beta"]
  b.l <- beta.s - 1.96*se.mat[, "beta"]

  # Plot smoothed estimates with 95% CI
  # TVP model is as follows:
  #       y(t) = alpha(t) + beta(t)*X(t) + noise
  
  # Range of years for plotting
  Year <- 1909:2008
  
  # Set data frame for plotting
  A <- data.frame(Year,beta.s, b.l, b.u)
  A$Shade <- "95% CI"
  
  #Define axis labels:
  xlabel <- "Year"
  ylabel <- "Regression Slope"
  
  # Plot
  p <- ggplot(data=A, aes(x=Year, y=beta, ymin=beta.1, ymax=beta.2, fill=Shade)) + 
    geom_line() + 
    geom_ribbon(alpha=0.5) +  
    xlab(xlabel) + 
    ylab(ylabel) +
    ylim(-0.16,0.16)
  
  #ggsave(p, file=paste(c("C:\\Users\\Danny\\Documents\\SugarMaple_Work\\Figures\\TVP_KalmanFilter\\",g,"_mid.jpeg"),collapse=""), width=8, height=4.5)

}

