set.seed(123) # pentru reproducibilitate

n_valori = c(1000, 10000, 100000, 1000000)
r_valori = c(2, 3, 4, 5)

for (r in r_valori) 
  {
  cat(sprintf("r = %d\n", r))
  for (n in n_valori) 
    {
    cat(sprintf("n = %d\n", n))
    Z = rt(n, df = r-2)
    X = Z * sqrt(r/(r-2))
    
    cat(sprintf("E[Xi] = %f\n", mean(X)))
    m = 0
    sigma2 = r/(r-2)
    var_exact = r/(r-2)^2 * (r/(r-4))
    
    sample_mean = mean(X)
    sample_var = var(X)
    
    # afisare rezultate
    cat(sprintf("Valoare exacta a mediei: %f\n", m))
    cat(sprintf("Valoare estimata a mediei: %f\n", sample_mean))
    cat(sprintf("Valoare exacta a variantei: %f\n", var_exact))
    cat(sprintf("Valoare estimata a variantei: %f\n", sample_var))
    cat("\n")
  }
}
