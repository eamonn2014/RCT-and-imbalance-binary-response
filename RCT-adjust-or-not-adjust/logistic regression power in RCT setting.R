

fun.d<-function(nsample, drug.allocation, 
                alpha,  beta.drug,
                seed=NULL){ 
  
  if (!is.null(seed)) set.seed(seed)
 
  drug<- (rbinom(nsample, 1, prob =drug.allocation ))   

  Xmat <- model.matrix(~ drug )
  beta.vec <- c(alpha,  beta.drug )
  
  lin.pred <- Xmat[,] %*% beta.vec                 # Value of lin.predictor
  exp.p <- exp(lin.pred) / (1 + exp(lin.pred))     # Expected proportion
  y <- rbinom(n = nsample, size = 1, prob = exp.p) # Add binomial noise
  #y<- runif(nsample) <  exp.p                     # alternatively ads noise in this way
  
  d<-as.data.frame(cbind(y, drug))         # create a dataset
  
  return(d)
  
}

# lrtest
simfunc <- function(d) {
  fit1 <- glm(y  ~ drug , d, family = binomial) 
  fit2 <- glm(y  ~ 1, d, family = binomial) 
  c(anova(fit1,fit2, test='Chisq')[2,5] )
}

# wald test

simfunc <- function(d) {
  fit1 <- glm(y  ~ drug , d, family = binomial) 
  c( summary(fit1)$coef["drug","Pr(>|z|)"] )
}


# (foo<-fun.d(nsample=20, drug.allocation=0.5,  
#             alpha=log(.3),  beta.drug=log(1)
#             , seed=NULL))

 
out <- replicate(10000, simfunc(fun.d( nsample=300, drug.allocation=0.5,  
                                       alpha=log(.35),  
                                       beta.drug=log(1.5))))
mean( out < 0.05, na.rm=TRUE )        ##may get NA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# check this also
# https://www.dartmouth.edu/~eugened/
# We emphasize that the Wald test should be used to match a typically used coefficient significance testing. 
# https://www.dartmouth.edu/~eugened/
# frank Harrell
library(Hmisc)
bpower(p1=.35, odds.ratio=c(1.5,2), n=300, alpha=c(.05))
bpower.sim(p1=.35, odds.ratio=c(1.5), n=300, alpha=c(.05))






