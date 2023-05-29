#A1.a

#functie pentru graficul distributiilor
distributie = function(lambda, p, n, k) 
{
  k_valori = k:n
  poisson_prob = dpois(k_valori, lambda)
  geometric_prob = dgeom(k_valori, p)
  binomial_prob = dbinom(k_valori,n, p)
  
  # Crearea graficului
  plot(k_valori, poisson_prob, type = "p", pch = 16, col = "blue",xlim = c(k, n), ylim = c(0, max(poisson_prob, geometric_prob, binomial_prob)),
       xlab = "k", ylab = "Probabilitate",
       main = "Functii de masa de probabilitate")
  
  #adaugarea celorlalte 2 distributii
  points(k_valori, geometric_prob, pch = 16, col = "green")
  points(k_valori, binomial_prob, pch = 16, col = "red")
  
  #unirea punctelor cu linii
  lines(k_valori, poisson_prob, type = "l", lwd = 2, col = "blue")
  lines(k_valori, geometric_prob, type = "l", lwd = 2, col = "green")
  lines(k_valori, binomial_prob, type = "l", lwd = 2, col = "red")
  
  legend("topright", legend = c("Poisson", "Geometric", "Binomial"),
       col = c("blue", "green", "red"), pch = 16, lwd = 2, bty = "n")
}
plot_distributions(2, 0.4, 10, 3)

#A1.b

p = 0.3
p_impar = 1/(2-p) #din formula distributiei geometrice: ((1-p)^(k-1))*p -> k = 2n+1, ajungem la o progresie geometrica
p2 = pgeom(3, prob = p, lower.tail = FALSE)
p3 = pgeom(20, prob = p, lower.tail = TRUE)

cat("Probabilitatea ca X - sa fie impar este de ", p_impar)
cat("Probabilitatea ca X >=4 ", p2)
cat("Probabilitatea ca X <= 20 ", p3)

#A1.c

prob_pois = function(lambda)
{
  k0 = 1
  while ((1 - ppois(k0 - 1, lambda)) > 10**-7)
  {
    k0 = k0 + 1
  }
  return(k0)
}
cat("Valoarea cea mai mica a lui k0 este: ", prob_pois(2))


#A2.a

caracteristici = function(nume_fis)
{
  y = read.csv(file = nume_fis, header = T)
  e1 = y[['P']]
  e2 = y[['S']]
  
  print(paste("Mediile: ", mean(e1), mean(e2)))
  
  print(paste("Medianele: ", median(e1), median(e2)))
  
  print(paste("Deviatiatiile standard: ", sd(e1), sd(e2)))
  
  print(paste(
    "Cvartilele (Q1, Q3) esantionului 1: ",
    as.vector(quantile(e1))[2],
    as.vector(quantile(e1))[4]
  ))
  print(paste(
    "Cvartilele (Q1, Q3) esantionului 2: ",
    as.vector(quantile(e2))[2],
    as.vector(quantile(e2))[4]
  ))
}
caracteristici("note.csv")

#A2.b

fara_val_aberante = function(nume_fis, esantion)
{
  y = read.csv(file = nume_fis, header = T)
  e = y[[esantion]]
  
  m = mean(e)
  s = sd(e)
  outliers = vector()
  j = 0
  # folosim media si deviatia standard pt determinarea val aberante
  for (i in 1:length(e))
    if (e[i] < m - 2 * s || e[i] > m + 2 * s) {
      outliers[j] = e[i]
      j = j + 1
    }
  
  e_nou = e[!e %in% outliers] #intructiune care elimina valorile outliers din e
  return(e_nou)
}
print(fara_val_aberante("note.csv", "P"))

#A2.c)

frecvente = function(nume_fis, esantion)
{
  e = fara_val_aberante(nume_fis, esantion)
  interval = seq(1, 10, 1)
  
  par(mfrow = c(1, 1)) # pentru afisarea unui singur grafic
  hist(e,breaks = interval,right = T,freq = T,main = paste("Histograma esantionului", esantion))
}
frecvente("note.csv", "S")

#functia de la c) foloseste functia de la b)