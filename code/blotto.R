library(MCMCpack)

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

set.seed(2017)

# Set parameters
n <- 10 # number of districts
r <- 100 # number of resources
num_strat <- 3 # number of different strategies (e.g. front-loaded, uniform, etc.)
num_place <- 4000 # number of resource placements to simulate for each strategy
total_num <- num_strat * num_place # total number of simulated placements
(num_matches <- choose(total_num, 2)) # total number of matches that will be played

# Set the vector of shape parameters to choose different strategy regimes
strat <- matrix(nrow=num_strat, ncol=n)
strat[1,] <- rep(1, n) # uniform strategies
strat[2,] <- n:1 # front-loaded strategies
strat[3,] <- 1:n # back-loaded strategies

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
x_new <- tweak(x, n, total_num)
x_old <- x
x <- x_new
#is_opt <- checkOpt(x_new, n, total_num)
#which(!is_opt)

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

# Find the 2nd best scoring placement
index2 <- which(avg == sort(avg, decreasing=TRUE)[2])
x[index2,]
hist(votes[index2,])

# Find the top 1% scoring placements
cutoff <- quantile(avg, prob=0.99)
top1pct <- which(avg >= cutoff)
order <- order(avg[top1pct], decreasing=TRUE)
x[top1pct[order],]


