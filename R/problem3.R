rm(list = ls())
# (a)
numerical_diff = function(f, x, h = 1e-6, method = "central")
{
  if (method == "forward")
  {
    result = (f(x+h) - f(x)) / h
  }
  else if (method == "backward")
  {
    result = (f(x) - f(x-h)) / h
  }
  else if (method == "central")
  {
    result = (f(x+h) - f(x-h)) / (2*h)
  }
  return(result)
}

f = function(x)
{
  return(cos(x) - x)
}

f_prime = function(x)
{
  return(-sin(x) - 1)
}


## [0, 2*pi] 구간에서 100개 점 생성
x = seq(0, 2*pi, length.out = 100)

y_forward = numerical_diff(f, x, method = "forward")
y_backward = numerical_diff(f, x, method = "backward")
y_central = numerical_diff(f, x, method = "central")

## 해석적 도함수
y_true = f_prime(x)


## 시각화
library(ggplot2)

df = data.frame(
  x = x,
  Forward = y_forward,
  Backward = y_backward,
  Central = y_central,
  Analytical = y_true
)

p = ggplot(df, aes(x = x, group = 1)) +
  geom_line(aes(y = Forward, color = "Forward")) +
  geom_line(aes(y = Backward, color = "Backward")) +
  geom_line(aes(y = Central, color = "Central")) +
  geom_line(aes(y = Analytical, color = "Analytical"),
            linetype = "dashed") +
  scale_color_manual(values = c(
    "Forward" = "red",
    "Backward" = "blue",
    "Central" = "yellow",
    "Analytical" = "black"
  )) +
  labs(x = "x", y = "Derivative", color = "Method")

print(p)

## 그래프 저장
ggsave("plots/problem3.png", plot = p)





# (b)
newtown_rapshon = function(f, fprime = NULL, x0, maxiter = 100, h = 1e-6, epsilon = 1e-10)
{
  x_t_1 = x0
  
  for (t in 1:maxiter)
  {
    # 도함수 계산
    if(is.null(fprime))
    {
      derivative = numerical_diff(f, x_t_1, h = h, method = "central")
    }
    else
    {
      derivative = fprime(x_t_1)
    }
    
    # Newton-Rapshon 
    x_t = x_t_1 - f(x_t_1) / derivative
    
    if (abs(x_t - x_t_1) < epsilon)
    {
      return(x_t)
    }
    
    x_t_1 = x_t
  }
  return(x_t_1)
}



# (c)
## 수치미분 버전
root_numerical = newtown_rapshon(f, x0 = 0.5)
root_numerical

## 도함수 제공 버전
root_analytical = newtown_rapshon(f, fprime = f_prime, x0 = 0.5)
root_analytical
