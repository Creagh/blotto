# ! INCOMPLETE ! #
# work in progress #

# Can we enumerate all possible "optimal" placements?
# i.e. those which are not wasting resources

# Create a list of viable allocations for each district
opt <- list()
opt[[1]] <- 1:100
for(i in 2:n){
	opt[[i]] <- c(0, which(1:100 %% i == 1))
}

# Count the number of choices for each district
opt_counts <- lapply(opt, length)
# Compute the total number of choices without any constraints
prod(rapply(opt_counts, c)) # too many to enumerate

# Run a top-down approach to selecting placements with the
# constraint that the sum of resources must be = r

top <- opt[[n]]
for(i in top) {
	sum <- i
	for(j in (n-1):1) {
		next_down <- opt[[j]]
		which(sum + next_down <= 100)
	}
}


