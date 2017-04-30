## Q1: Random generation of coins without replacement. Here I assume that all coins will be drawn from
## the bag. If this is not true, the script can be modified by adding another variable to the function
## coin.draw.
rm(list = ls()) ##Clear workspace
options(digits=10)

coin.draw <- function(x) { ## function(x,y)
    set.seed(13) ## Set seed for randomization
    coins <- sample(1:x, x, replace = FALSE)
    ## coins <- sample(1:x, y, replace = FALSE)
    new.payment <- coins[1]
    for (i in 1:(length(coins)-1)){
        new.payment <- c(new.payment, abs(coins[i] - coins[i+1]))
    }
    return(new.payment)
}

payment1 <- coin.draw(10)
mean(payment1) ## 3.7
sd(payment1)   ## 2.710063550

payment2 <- coin.draw(20)
mean(payment2) ## 8
sd(payment2)   ## 4.723959089

## Using Poisson's distribution
ppois(45, lambda=(mean(payment1)), lower=FALSE) ## 6.689049464e-34
ppois(160, lambda=(mean(payment2)), lower=FALSE) ## 1.161012904e-145
