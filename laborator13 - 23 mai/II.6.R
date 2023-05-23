interval_file = function(file, alfa, sigma) 
{
  data = scan(file)  # Citim date
  n = length(data)  # Calculăm dimensiunea eșantionului
  
  sample_mean = mean(data)  # Calculăm media de selecție
  
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical_z * sigma / sqrt(n)
  b = sample_mean + critical_z * sigma / sqrt(n)
  interval = c(a, b)
  
  return(interval)
}

file = "history.txt"
alfa = 0.05
sigma = 10

interval = interval_file(file, alfa, sigma)

print(interval)
 
##valori afisate 69.51801 74.85236





