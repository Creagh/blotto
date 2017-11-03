######################################################
# FUNCTION: Tweak Sampled Placements
# @param x - matrix of simulated resource placements
# @param n - number of districts
# @param total_num - total number of simulated placements
# @param prob - the matrix of multinomial prob vectors for each placement
# @returns matrix of "optimal" simulated resource placements
######################################################

# Sampled placements should be "tweaked" so that no resources are wasted.

# For e.g., in district 10, optimal values of resource placement are
# 1, 11, 21, ... , 91. Placing 2 resources in district 10 will not win a vote in any
# situations where 1 resource won't. This is because if your opponent places k
# resources in district n, then you need at least (kn + 1) to win.

# Thus, placing j resources in district n>1 will result in minimal waste when j mod n = 1, or j = 0.
# Note: when n=1, no allocations are optimally less wasteful

tweak <- function(x, n, total_num, prob) {
	
	# loop through all placements
	for(k in 1:total_num){ 
		
		excess <- 0
		
		# Skim the excess resources
		# loop through districts 2 to n for placement k
		for(i in 2:n) {
			# Find which resource allocations are optimal for district i
			opt <- c(0, which(1:100 %% i == 1))
			
			# "skim" off the excess/wasted resources for district i
			opt_floor <- max(opt[opt <= x[k,i]])
			excess <- excess + ( x[k,i] - opt_floor )
			x[k,i] <- opt_floor
			
		}
		
		# Redistribute any skimmed resources
		if(excess > 0) {
				
			# randomize the order to attempt to redistribute excess &
			# weight the order according to the strategy being used
			sample <- sample(1:n, replace=FALSE, prob=prob[k,])
			
			# loop through redistribution order	
			for(j in sample) {
				#print(paste("x[k,] = ", x[k,]))
				#print(paste("sample = ", sample))
				#print(paste("excess = ", excess))
				if(excess >= j) {
					add <- j * floor(excess/j) # max amount that can be added to district j to remain optimal
					x[k,j] <- x[k,j] + add # update district j
					excess <- excess - add # update amount of excess
				} else if(excess > 1) { # 1 < excess < j, so it cannot be added to jth district
					# do nothing
				} else if(excess == 1) { # excess can only be added to district 1 now
					x[k,1] <- x[k,1] + 1
					excess <- 0
				} else { # if no excess left then break for-loop
					break
				}
			}
		}
	}
	
	return(x)
}

######################################################
# FUNCTION: Check Optimality of Tweaked Placements
# @param x - matrix of tweaked resource placements
# @param n - number of districts
# @param total_num - total number of simulated placements
# @returns matrix of Boolean values indicating if a resource placement
#          is optimal for each district
######################################################

checkOpt <- function(x, n, total_num) {
	
	is_opt <- matrix(nrow=total_num, ncol=n)
	is_opt[,1] <- TRUE
	
	# loop through all placements
	for(k in 1:total_num){ 
		
		# loop through districts 2 to n for placement k
		for(i in 2:n) {
			# Find which resource allocations are optimal for district i
			opt <- c(0, which(1:100 %% i == 1))
			
			is_opt[k,i] <- x[k,i] %in% opt
		}
	}
	
	return(is_opt)
}