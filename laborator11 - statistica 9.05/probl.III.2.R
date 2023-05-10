lambda1 = 4
lambda2 = 12
p = 0.75 #probabilitate
n = 10000 

prob = rbinom(n, 1, p) #prob repr probabilitatea de a fi servit de primul mecanic
K = numeric(n)


K[prob == 1] = rexp(sum(prob == 1), lambda1)
K[prob == 0] = rexp(sum(prob == 0), lambda2/3)

mean(K) #medie