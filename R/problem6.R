rm(list= ls())
# (a)
gaussian_kernel = function(x, x_prime, rho = 1)
{
  result = exp(-rho * abs(x  - x_prime)^2)
  
  return(result)
}



# (b)
krr = function(X, y, lambda = 0.0001, rho = 1)
{
  n = nrow(X)
  K = matrix(0, n, n)
  
  for (i in 1:n)
  {
    for (j in 1:n)
    {
      K[i, j] = gaussian_kernel(X[i, 1], X[j, 1], rho)
    }
  }
  
  alpha = solve(K + lambda * diag(n), y)
  
  result = list(X = X, y = y, alpha = alpha, rho = rho)
  class(result) = "krr"
  
  return(result)
}



# (c)
predict.krr = function(object, newdata)
{
  X = object$X
  alpha = object$alpha
  rho = object$rho
  
  n = nrow(X)
  m = nrow(newdata)
  
  pred = numeric(m)
  
  for (i in 1:m)
  {
    result = 0
    
    for(j in 1:n)
    {
      result = result + gaussian_kernel(newdata[i, 1], X[j, 1], rho) * alpha[j]
    }
    pred[i] = result
  }
  
  return(pred)
}



# (d)
plot.krr = function(x)
{
  X = x$X
  y = x$y
  
  x_new = matrix(seq(min(X), max(X), length.out = 200), ncol = 1)
  y_pred = predict(x, x_new)
  
  plot(X[, 1], y, pch = 16, col = 'black',
       xlab = "x", ylab = "y")
  
  lines(x_new[, 1], y_pred, col = "red", lwd = 2)
}



# (e)
set.seed(1)
n = 150
X = matrix(runif(n, -1, 1), ncol = 1)
ftrue = function(x) sin(2*pi*x) + 0.5 * cos(4*pi*x)
y = ftrue(X[,1]) + rnorm(n, sd = 0.1)

model = krr(X, y)
x_new = matrix(seq(-1, 1, length.out = 200), ncol = 1)
y_pred = predict(model, x_new)
plot(model)

## 그래프 저장
png("plots/problem6.png", width = 1500, height = 1200)
plot(model)
dev.off()