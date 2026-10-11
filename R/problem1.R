## (a)
### 데이터 생성
set.seed(1)
n = 200
x = seq(0, 1, length.out = n)
y = sin(2 * pi * x) + rnorm(n, sd = 0.15)
df = data.frame(x = x, y = y)

write.csv(df, "data/problem1.csv", row.names = FALSE)



## (b)
### 데이터 불러오기 및 시각화
library(ggplot2)
df = read.csv("data/problem1.csv")
head(df)

p = ggplot(df, aes(x = x, y = y)) +
  geom_point() +
  geom_smooth(method = "loess")

print(p)


## (c)
### 그래프 저장
ggsave("plots/problem1.png", plot = p)


## (d)

