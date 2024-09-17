library(ggplot2)
library(knitr)
#knitr::opts_chunk$set(echo=TRUE, fig.path='part1/', fig.width=10, fig.height=6, cache=TRUE)
set.seed(13411)

lambda <- .2
num_exponential <- 40
num_of_simulations <- 1000


simulation_exp <- replicate(num_of_simulations, rexp(num_exponential, lambda))
mean_exponential <- apply(simulation_exp, 2, mean)

#Plot 1
#Theoritical mean vs Actual mean

mean_sample <- mean(mean_exponential)
mean_sample

mean_theory <- 1 / lambda
mean_theory


hist(mean_exponential, col="light blue", main = "Theoritical Mean vs Actual Mean", breaks=30, xlab = "Simulation means")
abline(v=mean_theory, lwd=2, col="blue")
abline(v=mean(mean_sample), lwd=2, col="red")
legend('topright', c("Sample Mean", "Theoretical Mean"), 
       bty = "n",       
       lty = c(2,2), 
       col = c(col = "blue", col = "red"))


## Compare sample Standard Deviation vs Theoretical Standard Deviation

theoretical_deviation <- round((1/lambda) / sqrt(num_exponential), 4)
theoretical_deviation

sample_standard_deviation <- round(sd(mean_exponential), 4)
sample_standard_deviation

## Compare sample Variance vs Theoretical variance

theoretical_variance = (1/lambda)^2/num_exponential
theoretical_variance

sample_variance = var(mean_exponential)
sample_variance

hist(mean_exponential, prob=TRUE, col="light green", main="Simulated values vs Theoretical values", breaks=30, xlim=c(2,9), xlab = "Simulation Means")
lines(density(mean_exponential), lwd=2, col="blue")

# Normal distribution line creation
x <- seq(min(mean_exponential), max(mean_exponential), length=2*n)
y <- dnorm(x, mean=1/lambda, sd=sqrt(((1/lambda)/sqrt(n))^2))
#lines(x, y, pch=22, col="black", lwd=2, lty = 2)
lines(x, y, col="red", lwd=2, lty = 2)

legend('topright', c("Simulated Values", "Theoretical Values"), 
      bty = "n", lwd = c(2,2), col = c("blue", "red"))


qqnorm(mean_exponential,main ="Q-Q Plot", col = "blue")
qqline(mean_exponential, col = "red", lwd = 2)


