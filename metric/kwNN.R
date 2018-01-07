source('utils.R')

# функция веса
weight <- function(i, k) {
  return((k+1-i)/k)
}

# kwNN
kwNN <- function(train, test, cl, k = 1, weightFunc) {
  weights <- weightFunc(1:k, k)
  
  res <- c()
  for (i in 1:nrow(test)) {
    order <- orderByDist(test[i,], train)
    
    ## Получаем классы первых k соседей
    classes <- cl[order[1:k]]
    
    names(weights) = classes
    
    classWeights <- sapply(names(table(classes)), function(class) {
        sum(weights[names(weights) == class])
    })
 
    ## Находим класс, который доминирует среди первых k соседей
    class <- names(which.max(classWeights))
    
    res[i] <- class
  }
  
  return (res)
}

# LOO
LOO <- function(xl) {
  n <- nrow(xl)
  maxk <- 20
  loo <- numeric(maxk)
  
  # Рассматриваем число возможных соседей от 1 до n-1
  for (k in 1:maxk) {
    
    for (i in 1:n)   {
      pred <- kwNN(train = xl[-i, 3:4], test = xl[i, 3:4], cl = xl$Species, k = k, weight)
      loo[k] <- loo[k] + (pred != xl$Species[i]) 
    }
    
  }
  
  loo <- loo/n
}

drawLOO <- function(xl) {
  n <- nrow(xl)
  maxk <- 20
  loo <- LOO(xl)
  bestK <- which.min(loo)
  
  plot(1:maxk, loo, type = 'l', col = 'red', lwd = 2,
       xlab = 'k', ylab='LOO')
  
  points(bestK, loo[bestK], col = 'green3', bg = 'green3', asp = 1, pch = 21)
  
  legend( x="topright", 
          legend=c("Скользящий контроль","оптимальное k"), 
          col=c("red","green3"), bg=c(NA, 'green3'), lwd=2, lty=c(1,NA), 
          pch=c(NA,19), merge=FALSE, cex=0.8 )
  
  text(bestK, loo[bestK], paste("k=", bestK), col = 'black', pos=3)
}

# картa	классификации
drawKwNN <- function(train, classes, colors) {
  plot(train, pch = 21, bg = colors[classes], col = colors[classes], asp = 1)
  
  step <- 0.1
  ox <- seq(0, 7, step)
  oy <-seq(0, 3, step)
  
  test <- expand.grid(Petal.Length = ox, Petal.Width = oy)
  
  prediction <- kwNN(train, test, classes, k = 1, weight)
  
  points(test, pch = 21, col = colors[prediction], asp = 1)
}

trainIris <- iris[, 3:4]
classes <- iris[, 5]
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")

drawKwNN(trainIris, classes, colors)
#drawLOO(iris)
