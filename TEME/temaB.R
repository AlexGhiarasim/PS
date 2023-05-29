#B1

verifica_LNM_Geom = function(p, n) 
{
  exact_mean = 1 / p
  
  # Generare sir de variabile aleatoare rep geometric
  X = rgeom(n, p)
  mean_X = mean(X) + 1 #calcul medie
  
  # Afisare rezultate
  cat("Valoarea exacta asteptata pentru media variabilelor aleatoare:", exact_mean, "\n")
  cat("Valoarea estimata a mediei sirului de variabile aleatoare:", mean_X, "\n")
}

# Verificare pentru diferite valori de n si p
n_values = c(5000, 10000, 100000, 500000)
p_values = c(0.2, 0.6, 0.6, 0.8)

for (n in n_values) 
{
  for (p in p_values) 
  {
    cat("Verificare pentru n =", n, "si p =", p, "\n")
    verifica_LNM_Geom(p, n)
    cat("\n")
  }
}


#B2

CLT_Student = function(r, n, N, z) {
  expectation = 0
  st_dev = sqrt(r / (r - 2))
  
  upper_bound = z * st_dev / sqrt(n) + expectation
  sum = 0
  
  for (i in 1:N) {
    x_n = mean(rt(n, r))
    
    if (x_n <= upper_bound) {
      sum = sum + 1
    }
  }
  return(abs(pnorm(z) - sum / N))
}

n = 50
N_values = c(5000, 10000, 20000)
z_values = c(-1.5, 0, 1.5)
r = 4  # Valoarea specificata în enuntul exercitiului

for (N in N_values) 
{
  for (z in z_values) 
  {
    result = CLT_Student(r, n, N, z)
    error = abs(result - 0)  
    cat("N =", N, ", z =", z, ", Error =", error, "\n")
  }
}

#B3

# Definirea functiei
approx_binomial_probability = function(n, p, h, k) 
{
  expectation = n * p
  variance = n * p * (1 - p)
  standard_deviation = sqrt(variance)
  left = (h  - expectation) / standard_deviation
  right = (k  + 0.5- expectation) / standard_deviation
  return(pnorm(right) - pnorm(left))
}


n = 100
p = 0.3
h = 20
k = 40

# Calcularea probabilitatii aproximative 
approx_prob = approx_binomial_probability(n, p, h, k)

# Calcularea probabilitatii exacte 
exact_prob = sum(dbinom(h:k-1, n, p))

# Afisarea rezultatelor
cat("Probalitate aproximativa: ", approx_prob, "\n")
cat("Probabilitate exacta: ", exact_prob, "\n")

