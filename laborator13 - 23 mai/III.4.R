conf_interval_file = function(filename, alpha) 
{
  data = scan(filename)  # Citim datele din fișierul specificat
  n = length(data)  # Calculăm dimensiunea eșantionului
  xn = mean(data)  # Calculăm media de selecție
  s = sd(data)  # Calculăm deviația standard
  
  se = s / sqrt(n)
  critical_t = qt(1 - alpha/2, n - 1)
  a = xn - critical_t * se
  b = xn + critical_t * se
  interval = c(a, b)
  
  return(interval)
}

file = "history.txt"
alpha = 0.05;

interval = conf_interval_file(file, alpha)

print(interval)
##valori afisate 67.84059 76.52978