valMC = function(k, N) 
{
  estimare = numeric(k);
  for (i in 1:k) 
  {
    estimare[i] = MC_integrare(N);
  }
  cat("Media estimărilor:", mean(estimare), "\n");
  cat("Deviatia standard:", sd(estimare), "\n");
}

integrareMC = function(N) 
{
  suma = 0;
  for (i in 1:N) 
  {
    u = rexp(1, rate = 3);
    suma = suma + exp(-2*u^2) / dexp(u, rate = 3);
  }
  return(suma/N);
}

N = 50000; #parametrul din cerinta
val_estimata = integrareMC(N);
val_exacta = sqrt(pi) / 8;

eroare_absoluta = abs(val_estimata - val_exacta);
eroare_relativa = abs((val_estimata - val_exacta) / val_exacta) * 100;

cat("Valoarea estimată:", val_estimata, "\n");
cat("Valoarea exactă:", val_exacta, "\n");
cat("Eroarea absolută:", eroare_absoluta, "\n");
cat("Eroarea relativă:", eroare_relativa, "%", "\n");

k = 30;
valMC(k, N);
