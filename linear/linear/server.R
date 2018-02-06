library(shiny)
library(kernlab) # Support Vector Machines 
library(pROC)

shinyServer(function(input, output) {
  library(MASS)  
  library(kernlab)
  
  Normalization <- function(xl) {     
    n <- dim(xl)[2] - 1     
    for(i in 1:n)      {         
      xl[,i] <- (xl[,i]-mean(xl[,i]))/sd(xl[,i])      
    }     
    return(xl) 
  }
  
  Prepare <- function(xl) {     
    l <- dim(xl)[1]     
    n <- dim(xl)[2]-1     
    xl <- cbind(xl[,1:n], seq(from = -1, to = -1, length.out = l), xl[,n+1]) 
  } 
  
  lossQuad <- function(x) {     
    return ((x-1)^2) 
  }
  
  lossPerceptron <- function(x) { 
    return (max(-x, 0)) 
  }  
  
  lossLog <- function(x) {     
    return (log2(1 + exp(-x))) 
  }  
  
  sigmoidFunction <- function(z) {     
    return (1 / (1 + exp(-z))) 
  }  
  
  SGradient <- function(xl, eta = 1, lambda = 1/5, id) {
    l <- dim(xl)[1]     
    n <- dim(xl)[2] - 1        
    w <- c(1/2, 1/2, 1/2)     
    iterCount <- 0        
    Q <- 0     
    for (i in 1:l){       
      wx <- sum(w*xl[i,1:n])            
      margin <- wx*xl[i,n+1] 
      if (id == 1) Q <- Q+lossQuad(margin)  
      else if (id == 2) Q <- Q + lossPerceptron(margin)
      else if (id == 3) Q <- Q + lossLog(margin)
    }        
    repeat {         
      margins <- array(dim=l)                  
      for (i in 1:l) {             
        xi <- xl[i,1:n]             
        yi <- xl[i,n+1]                          
        margins[i] <- crossprod(w,xi)*yi          
      }              
      errorIndexes <- which(margins <= 0)              
      if (length(errorIndexes) > 0) {             
        i <- sample(errorIndexes, 1)             
        iterCount <- iterCount+1                           
        xi <- xl[i,1:n]             
        yi <- xl[i,n+1]     
        wx <- sum(w*xi)                         
        margin <- wx*yi      
        if (id == 1) {
          ex <- lossQuad(margin)     
          eta <- 1/sqrt(sum(xi*xi))  
          w <- w-eta*(wx-yi)*xi  
        }
        else if (id == 2) {
          ex <- lossPerceptron(margin)
          eta <- 1/iterCount   
          w <- w+eta*yi*xi 
        }
        else if (id == 3) {
          ex <- lossLog(margin)          
          eta <- 0.3         
          w <- w-eta*xi*yi*sigmoidFunction(wx*yi)  
        }
        print(iterCount)
        Qprev <- Q    
        Q <- (1-lambda)*Q+lambda*ex  
        if (abs(Qprev-Q)/abs(max(Qprev,Q)) < 1e-5) break 
        if (iterCount > 1000) break
      }         
      else break         
    }
    return (w)
  }
  
  output$plot1 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_adaline,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_adaline,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_adaline,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_adaline,",")))
    ObjectsCountOfEachClass <- input$obj1  
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    ## ADALINE 
    plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "ADALINE") 
    w <- SGradient(xlNorm, id = 1)  
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "blue")  
    
    # count errors
    l <- dim(xlNorm)[1]     
    n <- dim(xlNorm)[2] - 1 
    errs = 0
    for (i in 1:l) {
      if (xlNorm[i,n+1] != sign(sum(w*xlNorm[i,1:n]))) {
        errs = errs + 1
      }
    }
    print("error")
    print(errs)
    
  })
  
  output$plot2 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_hebb,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_hebb,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_hebb,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_hebb,",")))
    ObjectsCountOfEachClass <- input$obj2  
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    ##Hebb
    plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "Hebb's rule") 
    w <- SGradient(xlNorm, id = 2)     
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "green3") 
    
    # count errors
    l <- dim(xlNorm)[1]     
    n <- dim(xlNorm)[2] - 1 
    errs = 0
    for (i in 1:l) {
      if (xlNorm[i,n+1] != sign(sum(w*xlNorm[i,1:n]))) {
        errs = errs + 1
      }
    }
    print("error")
    print(errs)
    
  })
  
  
})
