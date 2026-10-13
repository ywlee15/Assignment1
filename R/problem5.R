# (a)
A = matrix(c(4, 2, 2, 
             2, 5, 1,
             2, 1, 3), nrow = 3)
L = t(chol(A))
print(L)

print(L%*%t(L))   # A와 같음


# (b)
forward = function(L, b)
{
  n = length(b)
  z = numeric(n)
  
  for (i in 1:n)
  {
    result = b[i]
    
    if (i > 1)
    {
      for (j in 1:(i-1))
      {
        result = result - L[i, j] * z[j]
      }
    }
    z[i] = result / L[i, i]
  }
  return(z)
}



# (c)
backward = function(L, z)
{
  n = length(b)
  x = numeric(n)
  
  for (i in n:1)
  {
    result = z[i]
    
    if (i < n)
    {
      for(j in (i+1):n)
      {
        result = result - L[j, i] * x[j]
      }
    }
    x[i] = result / L[i, i]
  }
  return(x)
}



# (d)
b = c(1, -2, 3)

z = forward(L, b)
x = backward(L, z)

print(x)
print(solve(A, b))

all.equal(x, solve(A, b))