source('utils.R')

# kNN
kNN <- function(train, test, cl, k = 1) {
  res <- c()
  
  for (i in 1:nrow(test)) {
    order <- orderByDist(test[i,], train)
    
    ## Получаем классы первых k соседей
    classes <- cl[order[1:k]]
    
    ## Составляем таблицу встречаемости каждого класса
    counts <- table(classes)
    ## Находим класс, который доминирует среди первых k соседей
    class <- names(which.max(counts))
    
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
      pred <- kNN(train = xl[-i, 3:4], test = xl[i, 3:4], cl = xl$Species, k = k)
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

drawLOO(iris)
