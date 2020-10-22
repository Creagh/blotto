# Blotto

My submission for the Jane Street Symposium competition (2017)

## Puzzle Description

You and another (equally qualified) candidate are running for office. There are 10 districts, numbered 1, 2, 3, …, 10 and each is worth 1 vote. You have 100 discrete units of resources (e.g. time, campaign workers), which you can allocate between the districts however you wish. Your opponent independently does the same. 

District N  will award its vote to a candidate if and only if that candidate spends strictly more than N times the resources as their opponent (for N = 1, 2, 3, …, 10).

For example, here is one match:
| District | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 |
|----------|----|----|----|----|----|----|----|----|----|-----|
| Alice    | 10 | 10 | 10 | 10 | 10 | 10 | 10 | 10 | 10 | 10  |
| Carol    | 15 | 5  | 30 | 5  | 10 | 5  | 0  | 10 | 0  | 20  |

In this match, Alice wins districts 7 and 9, for a total of 2 districts, and Carol wins only district 1, for a total of 1 district. (Nobody wins a vote among the remaining 7 districts.)

## Objective

We're going to play a tournament. You get one entry and your final score is the average of your number of districts won playing head-to-head against each other entry among applicants to our Jane Street Symposium event (plus a few hundred Jane Streeters too!).

## My Submission

My submission document can be found [here](explanation/blotto_briercliffe.pdf).

My general approach was to simulate a number of tournaments consisting of some randomly generated entries, as well as some cleverly chosen deterministic ones. I chose the strategy that performed the best on average in my simulations.

R code for my simulations are found [here](code).
