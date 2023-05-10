
prob_all.infected_MC = function(N) 
{
  count_infectati = 0
  for (i in 1:N) 
  {
    nr_inf = 1
    nrzile = 2
    ultimele_erori = c(18, 22, 28)
    nr_erori = 18
    while (nr_erori > 0) 
    {
      lambda = min(ultimele_erori)
      nr_erori = rpois(1, lambda)
      ultimele_erori = c(nr_erori, ultimele_erori[1:2])
      nr_inf = nr_inf + nr_erori
      nrzile = nrzile + 1
    }
    if (nr_inf == 40) 
    {
      count_infectati = count_infectati + 1
    }
  }
  return(count_infectati / N)
}

prob_last15.infected_MC = function(N) 
{
  count_at_least_15_infected = 0
  for (i in 1:N) 
  {
    nr_inf = 1
    nrzile = 2
    ultimele_erori = c(18, 22, 28)
    nr_erori = 18
    while (nr_erori > 0) 
    {
      lambda = min(ultimele_erori)
      nr_erori = rpois(1, lambda)
      ultimele_erori = c(nr_erori, ultimele_erori[1:2])
      nr_inf = nr_inf + nr_erori
      nrzile = nrzile + 1
    }
    if (nr_inf >= 15) 
    {
      count_at_least_15_infected = count_at_least_15_infected + 1
    }
  }
  return(count_at_least_15_infected / N)
}

prob_last15.infected_MC_error = function(target_error, confidence) 
{
  N = 10000
  p_hat = prob_last15.infected_MC(N)
  error = 1
  while (error > target_error) 
  {
    N = N * 2
    p_hat_prev = p_hat
    p_hat = prob_last15.infected_MC(N)
    error = qnorm(1 - (1 - confidence) / 2) * sqrt(p_hat_prev * (1 - p_hat_prev) / N)
  }
  return(list(probability = p_hat, error = error))
}

#cerinta a 
prob_all_infected <- prob_all.infected_MC(10000)
cat("Probabilitatea ca într-o anumită zi toate computerele să fie infectate:", prob_all_infected, "\n")


#cerinta b
prob_at_least_15_infected <- prob_last15.infected_MC(10000)
cat("Probabilitatea ca într-o anumită zi cel puțin 15 computere să fie infectate:", prob_at_least_15_infected, "\n")


#cerinta c
confidence = 0.95
result = prob_last15.infected_MC_error(target_error, confidence)
cat("Probabilitatea ca într-o anumită zi cel puțin 15 computere să fie infectate:", result$probability, "\n")
cat("Eroarea estimării:", result$error, "\n")
