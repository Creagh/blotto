######################################################
# FUNCTION: Conduct Tournament
# @param x - matrix of simulated resource placements
# @param n - number of districts
# @param total_num - total number of simulated placements
# @returns matrix of the number votes won by placement i when matched against placement j
######################################################
# Every placement should face-off against every other placement exactly once,
# thus there are total_num choose 2 number of matches and
# each placement's score is an average of (total_num - 1) of those matches
runTournament <- function(x, n, total_num) {
	# Create matrix of the number of votes won by placement i when matched against placement j
	votes <- matrix(nrow=total_num, ncol=total_num)
	#num_matches <- choose(total_num, 2)
	
	iteration <- 1 
	for(i in 1:(total_num - 1)){
		for(j in (i+1):total_num){
			print(paste("iteration: ", iteration))
			scores <- getScore(x[i], x[j], n)
			votes[i,j] <- scores[1]
			votes[j,i] <- scores[2]
			iteration <- iteration + 1
		}
	}
	
	return(votes)
}