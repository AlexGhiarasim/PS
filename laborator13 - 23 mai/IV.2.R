test_proportion = function(alfa,n,succese,p0,tip_ip){
  p_prim = succese/n
  z_score = (p_prim - p0)/sqrt(p0*(1 - p0)/n)
  cat("z_score=",z_score)
  if(tip_ip=="r"){
    critical_z = qnorm(1 - alfa, 0, 1)
    cat("critical_z=",critical_z)
    if(z_score<=critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
  if(tip_ip=="l"){
    critical_z = qnorm(alfa, 0, 1)
    cat("critical_z=",critical_z)
    if(z_score>=critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
  if(tip_ip=="s"){
    critical_z = qnorm(1-alfa/2, 0, 1)
    cat("critical_z=",critical_z)
    if(abs(z_score)<=critical_z)
      print("H0 nu se poate respinge")
    else
      print("H0 se respinge")
  }
}

test_proportion(0.05, 150, 20, 0.1, "r")