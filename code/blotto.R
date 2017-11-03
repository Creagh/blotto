library(MCMCpack)
library(mgcv)
library(ggplot2)

setwd("~/Google Drive/Documents/blotto/code")

# Dependencies:
source("scoring.R")
source("conduct_tournament.R")
source("tweak_placements.R")

######################################################
# Resource Placement Simulation
######################################################
# IDEA: sample resource placements from a Dirichlet-Multinomial distribution, 
# where we choose the vector of shape parameters to simulate different strategy regimes

set.seed(2023)

# Set parameters
n <- 10 # number of districts
r <- 100 # number of resources
num_strat <- 5 # number of different strategies (e.g. front-loaded, uniform, etc.)
num_place <- 5000 # number of resource placements to simulate for each strategy
total_num <- num_strat * num_place # total number of simulated placements

# Set the vector of shape parameters to choose different strategy regimes
strat <- matrix(nrow=num_strat, ncol=n)
strat[1,] <- rep(1, n) # uniform strategies
strat[2,] <- n:1 # front-loaded strategies
strat[3,] <- 1:n # back-loaded strategies
strat[4,] <- (n:1)^2 # very front-loaded
strat[5,] <- (1:n)^2 # very back-loaded

# Sample the multinomial probability vectors given the strategy
prob <- NA
for(i in 1:num_strat){
	prob <- rbind(prob, rdirichlet(n=num_place, alpha=strat[i,]))	
}
prob <- prob[-1,] # remove first row of NA values

# Sample the placements given the prob. vectors
x <- matrix(nrow=total_num, ncol=n)
for(i in 1:total_num) {
	x[i,] <- rmultinom(n=1, size=r, prob=prob[i,])
}

# Tweak the sampled placements such that no resources are "wasted"
#x_new <- tweak(x, n, total_num, prob)
#x_old <- x
#x <- x_new
#is_opt <- checkOpt(x_new, n, total_num)
#which(!is_opt)

# Tweak half of the sampled placements such that no resources are "wasted."
# This way at least half of these samples will be offensively placed
row_perm <- sample(total_num, replace=FALSE)
x <- x[row_perm,]
prob <- prob[row_perm,]
half <- floor(total_num / 2)
x_new <- tweak(x[1:half,], n, half, prob[1:half,])
x_new <- rbind(x_new, x[(half+1):total_num,])
x_old <- x
x <- x_new

# Add some (deterministically) chosen placements
#x <- rbind(x, c(100, rep(0,9)))
#x <- rbind(x, c(rep(0,7), 9, 0, 91))
#x <- rbind(x, c(rep(0,6), 8, 0, 1, 91))
#x <- rbind(x, c(51, 49, rep(0,8)))
x <- rbind(x, c(37,3,4,5,6,7,8,9,10,11))
#x <- rbind(x, c(91, rep(1,9)))
#x <- rbind(x, c(rep(1,9), 91))
x <- rbind(x, c(36,13,13,5,6,7,8,1,10,1))
x <- rbind(x, c(0,0,4,0,11,13,15,17,19,21))
x <- rbind(x, c(1,1,1,1,11,13,15,17,19,21))
x <- rbind(x, c(4,0,0,0,11,13,15,17,19,21))
x <- rbind(x, c(0,3,0,1,11,13,15,17,19,21))
x <- rbind(x, c(47,7,13,21,2,2,2,2,2,2))

total_num <- total_num + 7
(num_matches <- choose(total_num, 2)) # total number of matches that will be played

# Count the number of unique placements sampled
nrow(uniquecombs(x))
nrow(uniquecombs(x_old))

######################################################
# Tournament Simulation
######################################################

# Simulate the votes won by each placement in each matchup
# Start the clock!
ptm <- proc.time()
votes <- runTournament(x, n, total_num)
# Stop the clock
proc.time() - ptm

# Calculate the average number of votes won for each placement
avg <- rowMeans(votes, na.rm=TRUE)

# Summarize the results
summary(avg)
sort(avg, decreasing=TRUE)
hist(avg)

# Find the best scoring placement
index <- which(avg == max(avg))
x[index,]
hist(votes[index,])
summary(votes[index,])

# Find the 2nd best scoring placement
index2 <- which(avg == sort(avg, decreasing=TRUE)[2])
x[index2,]
hist(votes[index2,])
summary(votes[index2,])

# Find the worst scoring placement
index3 <- which(avg == min(avg))
x[index3,]
hist(votes[index3,])

# Find the top 1% scoring placements
cutoff <- quantile(avg, prob=0.99)
top1pct <- which(avg >= cutoff)
order <- order(avg[top1pct], decreasing=TRUE)
x[top1pct[order],]

# a few more summaries...
ggplot(data.frame(avg=avg), aes(x=avg)) + geom_density()

df <- data.frame(avg=c(avg[1:4000], avg[4001:8000], avg[8001:12000]), strategy=c(rep("uniform", 4000), rep("front-load", 4000),
																																								 rep("back-load", 4000)))
ggplot(df, aes(x=avg, fill=strategy)) + geom_density(alpha=0.25) + geom_vline(xintercept=avg[12001:12012])
avg[12001:12012]
