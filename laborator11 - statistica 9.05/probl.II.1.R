integrala1 = function(x) exp(x)

integrala1 = integrate(integrala1, 1, 4)$value
eroare1_absoluta = abs(integrala1 - 51.87987)
eroare1_relativa = eroare1_absoluta / abs(51.87987)

integrala2 = function(x) 1 / (4*x^2 - 1)

integrala2 = integrate(integrala2, 1, Inf)$value
eroare2_absoluta = abs(integrala2 - log(3)/4)
eroare2_relativa = eroare2_absoluta / abs(log(3)/4)

cat("Valoare Integrala1: ", integrala1, "\n");
cat("Eroare absoluta integrala 1:", eroare1_absoluta, "\n");
cat("Eroare relativa integrala 1:", eroare1_relativa, "\n");

cat("Valoare Integrala2: ", integrala2, "\n");
cat("Eroare absoluta integrala 2:", eroare2_absoluta, "\n");
cat("Eroare relativa integrala 2:", eroare2_relativa, "\n");