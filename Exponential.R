nosim <- 1000
n <- 40
lambda = 0.2

data <- matrix(rexp(nosim * n, lambda), nosim)
data <- apply(data, 1, mean)

sample_mean <- mean(data)
sample_sd <- sd(data)

library(ggplot2)
ggplot(data = data.frame(data), aes(x = data)) + 
    geom_histogram(binwidth = 0.1, aes(y= ..density..), fill = "firebrick4", 
    color = "#999999") + xlab("Sample Mean where lambda = 0.2") +
    geom_vline(xintercept = sample_mean, size = 2, color = "gray9") + 
    stat_function(fun = dnorm, color = "gray9", size = 2, 
    arg = list(mean = 1/lambda, sd = 1/lambda*(1/sqrt(n)))) +
    ggtitle("Exponential Sample Means of 1000 Simulations where n = 40") 
    

sample_mean + c(-1, 1) * qnorm(.975) * sample_sd/sqrt(n)
sample_mean + c(-1.96, 1.96) * sample_sd/sqrt(n)
