#C1
paraboloid = function(a, N) 
{
  count = 0
  for (i in 1:N) 
  {
    x = runif(1, -sqrt(a), sqrt(a))
    y = runif(1, -sqrt(a), sqrt(a))
    z = runif(1, 0, a)
    if (z >= x^2 + y^2 && z <= a) {
      count = count + 1
    }
  }
  
  volum_total = (2 * sqrt(a))^2 * a  
  estimare_volum = volum_total * count / N
  return(estimare_volum)
}

esantion = c(10000, 20000, 50000)
a_valori = c(2, 4, 10)

for (a in a_valori) 
  {
  cat("Pentru a =", a, ":\n")
  valoare_exacta = (pi * a^2) / 2
  
  for (N in esantion) 
  {
    estimare = paraboloid(a, N)
    eroare_relativa = abs(estimare - valoare_exacta) / valoare_exacta * 100
    
    cat("Esantion:", N, "\tEstimare:", estimare, "\tEroare relativa:", eroare_relativa, "%\tValoare exacta:", valoare_exacta, "\n")
    cat("\n")
  }
}

#C2

patrulater = function(N) 
{
  a = 0
  b = 3
  c = 0
  d = (3 + 6) / 3
  
  count = 0
  for (i in 1:N) 
  {
    x = runif(1, a, b)
    y = runif(1, c, d)
    if (3 * y <= x + 6 && y <= 12 - 3 * x) 
    {
      count = count + 1
    }
  }
  
  aria_rectangulara = (b - a) * (d - c)
  estimare_aria = aria_rectangulara * count / N
  return(estimare_aria)
}

N = 20000
estimare_aria = patrulater(N)
cat("Estimare aria:", estimare_aria, "\n")

# C3

#a
MC_integration = function(N)
{
  a = -1
  b = 1 #intervale de integrare

  valoare_exacta = pi / 3 
  n = 20000 #dim esantionului pt metoda MC

  x = runif(n, a, b) # Generarea esantionului de puncte aleatoare în intervalul a,b
  fx1 = (x + 1) / sqrt(4 - x^2)

  integrala1 = sum(fx1) * (b - a) / n
  eroare = abs(integrala1 - valoare_exacta)*100/valoare_exacta
  cat("Valoarea exacta a integralei:", valoare_exacta, "\n")
  cat("Valoarea estimata a integralei:", integrala1, "\n")
  cat(" Eroarea este de:",eroare,"%\n" )
}
MC_integration(20000)


#b

MC_improved_integration = function(N) 
{
  count = 0
  for (i in 1:N) 
  {
    x = runif(1, 0, 1)
    y = runif(1, 0, 1)
    if (y <= 1/(x^2+1)) 
    {
      count = count + 1
    }
  }
  integral_estimate = count/N
  return(integral_estimate)
}
MC_imprvd_integr_average = function(k, N) 
{
  valoare_exacta = pi/4
  estimates = 0
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N)
  integrala2 = mean(estimates)
  eroare = abs(integrala2 - valoare_exacta)*100/valoare_exacta
  
  # Rezultate
  cat("Valoarea exacta a integralei:", valoare_exacta, "\n")
  cat("Valoarea estimata a integralei:", integrala2, "\n")
  cat(" Eroarea este de:",eroare,"%\n" )
}
MC_imprvd_integr_average(30, 20000)


#c

MC_improved_integration = function(N)
{
  sum = 0
  
  for (i in 1:N) {
    x = -rexp(1, 0.5)
    sum = sum + x * exp(x) / 0.5 * exp(-x * 0.5)
  }
  cat("Valoarea exacta a integralei: -1 \n")
  cat("Valoarea estimata a integralei:", sum/N, "\n")
  cat(" Eroarea este de:",abs(-1 - sum / N),"%\n" )
}
MC_improved_integration(20000)



#C4.a)
#numarul de zile pana nu mai exista conturi false
nr_zile = function(m, n, p, q)
{
  conturi_false = m
  nr_z = 0
  nr_conturi_elim = 0
  while (conturi_false > 0)
  {
    nr_z = nr_z + 1
    nr_conturi_elim = 0
    for (k in 1:conturi_false)
    {
      prob = runif(1, 0, 1)
      if (prob <= q)
        nr_conturi_elim = nr_conturi_elim + 1
    }
    conturi_false = conturi_false - nr_conturi_elim
    if (conturi_false <= 0)
      return (nr_z)
    nr_conturi_adaugate = rbinom(1, n, p)
    conturi_false = conturi_false + nr_conturi_adaugate
    #print(conturi_false)
  }
  
  return (nr_z)
}

#numarul de zile pana cand numarul de conturi ajunge sub 50000
nr_zile_lower_bound = function(m, n, p, q)
{
  conturi_false = m
  nr_z = 0
  nr_conturi_elim = 0
  while (conturi_false > 0)
  {
    nr_z = nr_z + 1
    nr_conturi_elim = 0
    for (k in 1:conturi_false)
    {
      prob = runif(1, 0, 1)
      if (prob <= q)
        nr_conturi_elim = nr_conturi_elim + 1
    }
    conturi_false = conturi_false - nr_conturi_elim
    if (conturi_false <= 50000)
      return (nr_z)
    nr_conturi_adaugate = rbinom(1, n, p)
    conturi_false = conturi_false + nr_conturi_adaugate
  }
  
  return (nr_z)
}

#a)
avg_nr_zile_conturi = function(m, n, p, q)
{
  suma = 0
  for (i in 1:N)
  {
    suma = suma + nr_zile(m, n, p, q)
  }
  return (suma / N)
}
#print(avg_nr_zile_conturi(100000, 500, 0.5, 0.1))
#pentru valorile din cerinta, codul ruleaza extrem de greu, pentru valori mai mici, iar prob de dezactivare crescuta
print(avg_nr_zile_conturi(10, 5, 0.2, 0.6))


#b)
probability = function(N, m, n, p, q) 
{
  s = 0;
  for(i in 1:N) 
  {
    if(nr_zile_lower_bound(m, n, p, q) > 40)
      s = s + 1;
  }
  return(s/N);
}
print(probability(2, 20, 10, 0.1, 0.5))


#c)
alfa = 1 - 0.99
z = qnorm(alfa/2)
epsilon = 0.01
p = probability(N, m, n, p, q)
N_min = ((1/4)*(z/epsilon)) ** 2
N_min
nr_zile(N_min + 1)


