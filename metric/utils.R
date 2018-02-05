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

# возвращает расстояния
getDists <- function(u, xl, metric = euclideanDistance) {
  distances <- c()
  
  for (i in 1:nrow(xl)) {
    distances[i] <-metric(u, xl[i,])
  }
  
  return (distances)
}

#Ядра
ker.R = function (r) 0.5 * (abs(r) <= 1) #прямоугольное
ker.T = function (r)(1 - abs(r)) * (abs(r) <= 1) #треугольное
ker.Q = function (r)(15 / 16) * (1 - r ^ 2) ^ 2 * (abs(r) <= 1) #квартическое
ker.E = function (r)(3 / 4) * (1 - r ^ 2) * (abs(r) <= 1) #епанечниково
ker.G = function (r) dnorm(r) #гауссовское