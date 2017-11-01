######################################################
# FUNCTION: Scoring Rules
# @param x - a vector of resource placements
# @param y - another vector of resource placements
# @param n - the length of the vectors (# of districts)
# @returns vector of length 2 indicating the number of votes won by x and y
######################################################
getScore <- function(x, y, n) {
	score_x <- sum(x > 1:n * y)
	score_y <- sum(y > 1:n * x)
	
	return(c(score_x, score_y))
}