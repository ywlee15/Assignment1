# (a)
bubble_sort = function(vec, decreasing = FALSE)
{
  n = length(vec)
  
  if (n < 2) return(vec)     # 예외 처리하기
  
  for (i in 1:(n-1))
  {
    for(j in 1:(n-i))
    {
      if (decreasing == FALSE)
      {
        if (vec[j] > vec[j+1])
        {
          temp = vec[j]
          vec[j] = vec[j+1]
          vec[j+1] = temp
        }
      }
      else
      {
        if (vec[j] < vec[j+1])
        {
          temp = vec[j]
          vec[j] = vec[j+1]
          vec[j+1] = temp
        }
      }
    }
  }
  return(vec)
}

set.seed(1)
x = runif(10)
bubble_sort(x, decreasing = FALSE)    # 오름차순
bubble_sort(x, decreasing = TRUE)     # 내림차순



# (b)
quick_sort = function(vec, decreasing = FALSE)
{
  n = length(vec)
  
  if (n < 2) return (vec)      # 예외 처리하기
  
  pivot = vec[1]    # 첫번째 원소를 pivot
  
  left = c()
  right = c()
  
  for (j in 2:n)
  {
    if (decreasing == FALSE)
    {
      if (vec[j] <= pivot)
      {
        left = c(left, vec[j])
      }
      else
      {
        right = c(right, vec[j])
      }
    }
    else
    {
      if (vec[j] >= pivot)
      {
        left = c(left, vec[j])
      }
      else
      {
        right = c(right, vec[j])
      }
    }
  }
  left = quick_sort(left, decreasing)
  right = quick_sort(right, decreasing)
  
  return(c(left, pivot, right))
}

set.seed(1)
x = runif(10)
quick_sort(x, decreasing = FALSE)    # 오름차순
quick_sort(x, decreasing = TRUE)     # 내림차순