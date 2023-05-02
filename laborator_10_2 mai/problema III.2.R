set.seed(123) 

n = 65 
alpha = 3
lambda = 1.2 
num_samples = 10050

X = matrix(rgamma(n * num_samples, shape = alpha, rate = lambda), nrow = num_samples)

# calcularea deviatiei standard si a medianei
sample_mean <- apply(X, 1, mean)
sample_sd <- apply(X, 1, sd)

true_mean <- alpha / lambda
true_sd <- sqrt(alpha / (lambda^2 * n))

# Calcularea z-scores
z_scores <- abs((sample_mean - true_mean) / true_sd)

# Calcularea probabilitatilor empirice
z_vals <- seq(-3, 3, by = 0.01)
emp_probs <- mean(z_scores >= z_vals[1:length(z_scores)])

plot(z_vals, dnorm(z_vals), type = "l", lwd = 2)
lines(seq(-3, 3, by = 0.01), rep(emp_probs, length(z_vals)), col = "red", lwd = 2)
legend("topright", c("Standard Normal", "Empirical"), lty = c(1,1), col = c("black", "red"), lwd = 2)