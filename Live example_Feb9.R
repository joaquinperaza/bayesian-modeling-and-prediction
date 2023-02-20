# Data
y1 <- 0 # Results from rapid test
y2 <- 0 # Results from PCR test


# Hyperparameters for priors
alpha.p <- 1
beta.p <- 1

# Fixed (assumed known) values of true positive rate
a1 <- 0.80
a2 <- 0.99


# Preliminary MCMC stuff
K <- 50000
samples <- matrix(,K,2)
colnames(samples) <- c("z1","p")
p.initial <- 0.01


# Gibbs sampler
for(k in 1:K){
  
  # Take one draw from the full conditional of z1 (i.e., [z1|p,y1,y2])
  p <- ifelse(k==1,p.initial,p)
  p.tilde <- (p*(1-a1)*(1-a2))/((p*(1-a1)*(1-a2))+1-p)
  z1 <- ifelse(min(c(y1,y2))==0,rbinom(1,1,p.tilde),1)
  
  # Take one draw from the full conditional of p (i.e., [p|z1,y1,y2])
  p <- rbeta(1,alpha.p+z1,beta.p+1-z1)
  
  # Save samples
  samples[k,] <- c(z1,p)
}


# Trace plots
plot(1:K,samples[,1],xlab="k",ylab="z|y",typ="l",main="")
plot(1:K,samples[,2],xlab="k",ylab="p|y",typ="l",main="")

# Histogram representation of the posterior distributions
hist(samples[,1],freq=FALSE,xlab="z|y",ylab="[z|y]",main="")
hist(samples[,2],freq=FALSE,xlab="p|y",ylab="[p|y]",main="")



  