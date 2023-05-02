binom_k = function(n, p, k) 
  {
  if (k < 0 || k >= n) {
    stop("k trebuie sa fie intre 0 si n-1")
  }
  1 - pbinom(k, n, p)
}
n = 15
p = 0.7
k = 5
prob = binom_k(n, p, k)
cat("Probabilitatea de a avea mai mult de ", k, "succese in", n, "incercari cu probabilitatea ", p, "este aproximativ", prob, "\n")

