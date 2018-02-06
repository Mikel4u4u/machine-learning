source('utils.R')

kwNN.w <- function(i, k) +(i <= k) * (k + 1 - i) / k

#весовая функция kwNN
W <- function(sortedDistances, idxs, k) {
  if (length(idxs) == 0) return(0)
  
  orderedDistances = 1:length(sortedDistances)
  names(orderedDistances) = names(sortedDistances)
  weights = kwNN.w(orderedDistances, k)
  
  weights = weights[idxs]
  if (length(weights) == 0) return(0)
  
  classWeights <- sapply(unique(names(weights)), function(class) {
    sum(weights[names(weights) == class])
  })
  # print(max(classWeights))
  return(max(classWeights)[1])
}

margin <-function(points, classes, u, class) {
  k = 30
  dist = getDists(u, points)
  names(dist) = classes
  sortedDistances = sort(dist)
  
  #Отсутуп относительно данного класса
  uidxs = which(sapply(names(sortedDistances), function(v) any(v == class)))
  
  m1 = W(sortedDistances, uidxs, k)
  
  #Отступ относительно других классов
  m2 = W(sortedDistances, -uidxs, k)
  
  return(m1 - m2)
}

getNoises <- function(margins) {
  # margins[which(margins < 0)]
  noisesIdxs <- which(margins < 0)
  return(noisesIdxs)
}


STOLP <- function (points, classes, tolerance) {
  n <- length(classes)
  # находим все отступы
  margins = numeric(n)
  for (i in 1:n) {
    margins[i] = margin(points, classes, points[i,], classes[i])
  }
  
  # удаляем шумы
  noises <- getNoises(margins)
  
  points <- points[-noises,]
  classes <- classes[-noises]
  n = n - length(noises)
  
  # выбираем лучшего представителя из каждого класса
  etalone = data.frame()
  etaloneClasses = c()
  for (class in unique(classes)) {
    #индексы данного класса
    idxs = which(classes == class)
    
    margins = sapply(idxs, function(i) margin(points, classes, points[i,], class))
    
    i = idxs[which.max(margins)]
    
    #Добавляем точку к эталонным
    etalone = rbind(etalone, points[i,])
    etaloneClasses = c(etaloneClasses, class)
    
    points = points[-i,]
    classes = classes[-i]
    n = n - 1
  }
  names(etalone) = names(points)
  
  while(n>0) {
    # Выделим мн-во объектов на которых алгоитм ошибается
    E = c()
    E.i = c()
    for (i in 1:n) {
      m = margin(etalone, etaloneClasses, points[i,], classes[i])
      if (m <= 0) {
        E = c(E, m)
        E.i = c(E.i, i)
      }
    }
    #print(E)
    if(length(E) < tolerance) break;
    
    #Добавим к эталонным
    i = E.i[which.min(E)]
  
    etalone <- rbind(etalone, points[i,])
    etaloneClasses <- c(etaloneClasses, classes[i])
    
    print(etalone)
    points <- points[-i,]
    classes <- classes[-i]
    n = n - 1
  }
  
  return(list("etalone" = etalone, "etaloneClasses" = etaloneClasses))
}

#Рисуем
drawSTOLP <- function(points, classes, etalone, etaloneClasses, colors) {

  plot(points, col = colors[classes], pch = 21, asp = 1, main = "STOLP для kwNN")
  points(etalone, bg = colors[etaloneClasses], pch = 21, col.lab = "blue")
}

trainIris <- iris[, 3:4]
classes <- iris[, 5]
colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")

res <- STOLP(trainIris, classes, 3)
print(res)

drawSTOLP(trainIris, classes, res$etalone, res$etaloneClasses, colors)
