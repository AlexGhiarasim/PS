#E1

interval_incredere_sodiu = function(nr_indiv, sample_mean, alfa, sigma)
{
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical_z * sigma/sqrt(nr_indiv)
  b = sample_mean + critical_z * sigma/sqrt(nr_indiv)
  interval_ab = c(a,b)
  return(interval_ab)
}
alfa = c(0.1, 0.05, 0.01)
for (x in alfa)
{
  #apel functie din curs adaptata la cerinta problemei
   print(interval_incredere_sodiu(10, 138, x, 11))
}


#E2 

zconfidence_interval = function(nr_indiv, sample_mean, alfa, sigma)
{
  critical_z = qnorm(1 - alfa/2, 0, 1)
  a = sample_mean - critical_z * sigma/sqrt(nr_indiv)
  b = sample_mean + critical_z * sigma/sqrt(nr_indiv)
  interval_ab = c(a,b)
   return(interval_ab)
}
#apel functie
#apelul contine nr de indivizi, media de selectie, procent interval de incredere si
# esantionului
print(zconfidence_interval( 256, 18 , 0.05, sqrt(1.44)))


#E3

# nu se poate trage concluzia ca schimbarea a fost inutila
#(nu putem trage concluzia ca probabilitatea clientilor nemultumiti va fi mai mare de 12%)
#la nivelul de semnificatie de 1%, respectiv 5%
ipoteza_statistica = function(alfa)
{
  n = 153
  succese = 17
  p_prim = succese / n
  p0 = 0.12
  z_score = (p_prim - p0) / sqrt(p0*(1 - p0) / n)
  critical_z = qnorm(1 - alfa, 0, 1)
  print(z_score)
  print(critical_z)
  if(z_score<=critical_z)
    print("H0 nu se poate respinge")
  else
    print("H0 se respinge")
}

ipoteza_statistica(0.05)
ipoteza_statistica(0.01)

