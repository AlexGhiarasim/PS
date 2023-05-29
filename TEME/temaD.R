#D1

MC_M_elem = function(x, k)
{
  for (i in 1:k)
  {
    nr_aparitii = 0
    s = sample(x, 1)
    for (j in 1:length(x))
    {
      if (s == x[j])
        nr_aparitii = nr_aparitii + 1
    }
    if (nr_aparitii >= length(x) / 2 + 1)
      return(s)
  }
  print("Vectorul nu are un M-element")
}

MC_M_elem(c(1,1,1,1,1,2,2,2,2,1,3,1,1), 2)

#D2

element = function(i, A) 
{
  n = length(A)
  if (n == 1) 
  {
    return(A)
  }
  
  z = sample(A, 1)
  A_less = A[A < z]
  A_greater = A[A > z]
  
  if (length(A_less) > i) 
  {
    return(element(i, A_less))
  } 
  else if (n > i + length(A_greater)) 
  {
    return(z)
  } 
  else 
  {
    return(element(i - n + length(A_greater), A_greater))
  }
}

A = c(1, 3, 5, 7, 9, 11, 13, 15)
i = 4

element(i, A)


#D3
#a)

random_subset_median = function(S) 
{
  n = length(S)
  a = 1
  m = floor(a * log(n))
  
  S_prime = sample(S, m)
  sorted_S_prime = sort(S_prime)
  median_S_prime = median(sorted_S_prime)
  
  return(median_S_prime)
}

S = rnorm(100)  # Generăm o mulțime de date de lungime 100, folosind distribuția normală

result = random_subset_median(S)
print(result)

#b)
# Probabilitatea de a returna corect >= 1 - 2/n**2, deci probabilitatea de a gresi < 1 - 2/n**2
# deci, 1 - 2/n**2 < 10**-7 -> 1 - 10**-7 < 2/n**2 -> (1 - 10**-7)*n**2 < 2 -> n**2 < 2/(1 - 10**-7)
# n < sqrt(2/(1-10**7)) -> n = 1
