outliers_mean = function(x)
{
  if (is.numeric(x)) 
    {
        mean_x = mean(x, na.rm = TRUE)
    } 
    else 
    {
      cat("Input is not numeric")
      mean_x = NA
    }
  std_dev = sd(x, na.rm = TRUE) 
  outliers = x[x < mean_x - 3 * std_dev | x > mean_x + 3 * std_dev]
  return(outliers)
}
x = c(1, 2, 3, 4, 5, 6, 1000)
outliers_mean(x) 

outliers_iqr = function(x)
{
   Q1 = quantile(x, 0.25, na.rm = TRUE) 
   Q3 = quantile(x, 0.75, na.rm = TRUE) 
   IQR = Q3 - Q1
   outliers = x[x < Q1 - 1.5 * IQR | x > Q3 + 1.5 * IQR]
   return(outliers)
}
x = c(1, 2, 3, 4, 5, 6, 1000)
outliers_iqr(x) 

data = scan("sample2.txt")
summary(data)

outliers_mean(data)
outliers_iqr(data)
