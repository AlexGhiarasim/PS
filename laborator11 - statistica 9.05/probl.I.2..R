y = function(x) -2*x^2 + 5*x - 2

#se genereaza 10000 de puncte in interiorul dreptunghiului
set.seed(123)  #reproducibilitate
n = 10000
x = runif(n, min = 0, max = 2)
y_coord = runif(n, min = 0, max = 2)

# nr de puncte sub grafic
puncte_sub_grafic = sum(y_coord <= y(x))

# aria aproximativa si eroarea relativa existenta
aria_aproximativa = puncte_sub_grafic/n * 4  #Aria = 4
aria_exacta = integrate(y, 0, 2)$value
eroare_relativa = abs(aria_aproximativa - aria_exacta)/aria_exacta

# Rezultatele problemei
cat("Aria aproximativa este:", aria_aproximativa, "\n")
cat("Aria exacta este:", aria_exacta, "\n")
cat("Eroarea relativa este:", eroare_relativa, "\n")