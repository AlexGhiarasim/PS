densitate <- function(m, sigma) 
{
  x = seq(m - 5*sigma, m + 5*sigma, 0.1) #definirea intervalului de valori
  y = dnorm(x, m, sigma) # calcularea densitatii pt fiecare x
  plot(x, y, type = "l", lwd = 2, xlab = "x", ylab = "Densitatea de probabilitate", main = "Distributia Normala")
}

# Test
densitate(m = 0.1, sigma = 2)
