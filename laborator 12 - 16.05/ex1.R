
variabila_aleat = function(matrice) 
  {
   valori = matrice[, 1]
   prob = matrice[, 2]
  
  if (length(valori) != length(prob)) 
  {
    stop("Dimensiunile matricei X nu sunt compatibile")
  }
  
  n = length(valori)
  prob_sum = sum(prob)
  u = runif(1)  #valoare aleatoare intre 0 și 1
  
  cumulativeprob = cumsum(prob) 
  
  for (i in 1:n) 
  {
    if (u <= cumulativeprob[i]) 
    {
      return(valori[i])
    }
  }
}

matrice = matrix(c(1, 0.1, 2, 0.25, 3, 0.45, 4, 0.15, 5, 0.05), nrow = 5, byrow = TRUE)
val_simulata = variabila_aleat(matrice)
cat("Valoarea este: ", val_simulata)