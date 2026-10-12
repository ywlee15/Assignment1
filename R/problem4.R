rm(list = ls())
# (a)
left_rectangle = function(f, a, b, n)
{
  h = (b - a) / n
  result = 0
  
  for (i in 0:(n-1))
  {
    x_i = a + i * h
    result = result + f(x_i)
  }
  result = h * result
  
  return (result)
}


# (b)
trapezoid = function(f, a, b, n)
{
  h = (b - a) / n
  result = f(a) + f(b)
  
  for(i in 1:(n-1))
  {
    x_i = a + i * h
    result = result + 2 * f(x_i)
  }
  
  result = (h/2) * result
  
  return(result)
}


# (c)
simpson = function(f, a, b, n)
{
  if (n %% 2 != 0)
  {
    stop("n must be even")
  }
  
  h = (b - a) / n
  result = f(a) + f(b)
  
  for (i in 1:(n-1))
  {
    x_i = a + i * h
    
    if (i %% 2 == 1)
    {
      result = result + 4 * f(x_i)
    }
    else
    {
      result = result + 2 * f(x_i)
    }
  }
  result = (h/3) * result
  
  return(result)
}


# (d)
## 1. Left Rectangle
left_rectangle(sin, a = 0, b = pi, n = 100)

## 2. Trapezoid
trapezoid(sin, a = 0, b = pi, n = 100)

## 3. Simpson
simpson(sin, a= 0, b = pi, n = 100)



# (e)
n_values = c(10, 30, 60, 100, 150, 200)
true_value = -cos(pi) + cos(0)          # 해석적으로 구한 값

error_left = numeric(length(n_values))
error_trapezoid = numeric(length(n_values))
error_simpson = numeric(length(n_values))

for (i in 1:(length(n_values)))
{
  n = n_values[i]
  
  error_left[i] = abs(left_rectangle(sin, 0, pi, n) - true_value)
  error_trapezoid[i] = abs(trapezoid(sin, 0, pi, n) - true_value)
  error_simpson[i] = abs(simpson(sin, 0, pi, n) - true_value)
}

df = data.frame(
  n = n_values,
  Left = error_left,
  Trapezoid = error_trapezoid,
  Simpson = error_simpson
)

library(ggplot2)

p = ggplot(df, aes(x = n, group = 1)) +
  geom_line(aes(y = Left, color = "Left Rectangle")) +
  geom_line(aes(y = Trapezoid, color = "Trapezoid")) +
  geom_line(aes(y = Simpson, color = "Simpson")) +
  labs(x = "n", y = "Absolute Error", color = "Method")

print(p)

## 그래프 저장
ggsave("plots/problem4.png", plot = p)