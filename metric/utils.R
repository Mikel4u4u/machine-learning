# Евклидово расстояние
euclideanDistance <- function(u, v) {
  sqrt(sum((u - v)^2))
}

# сортировка объектов Xl относительньно произвольного объекта u
# возвращает перестановку
orderByDist <- function(u, xl, metric = euclideanDistance) {
  distances <- c()
  
  for (i in 1:nrow(xl)) {
    distances[i] <-metric(u, xl[i,])
  }
  
  return (order(distances))
}
