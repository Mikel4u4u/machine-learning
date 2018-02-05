source('utils.R')

# функция ядра
ker <- ker.G

# PW
PW <- function(train, test, cl, h) {
  res <- c()
  for (i in 1:nrow(test)) {
    distances <- getDists(test[i,], train)
    
    weights <- ker(distances / h)
    classes <- cl
    
    names(weights) <- classes
    
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
  hVals <- seq(0.1, 2, 0.005)
  loo <- numeric(length(hVals))
  
  for (j in 1:length(hVals)) {
    h <- hVals[j]
    
    for (i in 1:n)   {
      pred <- PW(train = xl[-i, 3:4], test = xl[i, 3:4], cl = xl$Species[-i], h = h)
      loo[j] <- loo[j] + (pred != xl$Species[i])
    }
    
  }
  
  loo <- loo/n
}

drawLOO <- function(xl) {
  n <- nrow(xl)
  hVals <- seq(0.1, 2, 0.005)
  loo <- LOO(xl)
  bestH <- which.min(loo)
  
  plot(hVals, loo, type = 'l', col = 'red', lwd = 2,
       xlab = 'h', ylab='LOO')
  
  points(bestH, loo[bestH], col = 'green3', bg = 'green3', asp = 1, pch = 21)
  
  legend( x="topright", 
          legend=c("Скользящий контроль","оптимальное k"), 
          col=c("red","green3"), bg=c(NA, 'green3'), lwd=2, lty=c(1,NA), 
          pch=c(NA,19), merge=FALSE, cex=0.8 )
  
  text(bestH, loo[bestH], paste("k=", bestH), col = 'black', pos=3)
}

# картa	классификации
drawPW <- function(train, classes, colors) {
  plot(train, pch = 21, bg = colors[classes], col = colors[classes], asp = 1)
  
  step <- 0.1
  ox <- seq(0, 7, step)
  oy <-seq(0, 3, step)
  
  test <- expand.grid(Petal.Length = ox, Petal.Width = oy)
  
  prediction <- PW(train, test, classes, h=0.1)
  
  points(test, pch = 21, col = colors[prediction], asp = 1)
}

trainIris <- iris[, 3:4]
classes <- iris[, 5]
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")

#drawPW(trainIris, classes, colors)
drawLOO(iris)
