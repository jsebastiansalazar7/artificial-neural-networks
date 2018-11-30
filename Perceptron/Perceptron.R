learn <- function(x, weight, inputName = NULL, iterations = 100) {
        
        ## Subset weight vectors
        
        w10 <- weight[1]
        w11 <- weight[2]
        w12 <- weight[3]
        
        # Subset input vectors
        
        x1 <- x[, 1]
        x2 <- x[, 2]
        x3 <- x[, 3]
        
        ## Initialize an error vector, an error history vector, 
        ## an mse vector and an incremental variable to fill the
        ## error vector
        
        err <- vector("numeric", length = 4)
        errorVector <- vector("numeric")
        mse <- vector("numeric")
        j <- 1
        
        ## Variable to break function when leaning is over
        
        found = FALSE  
        
        ## Subset (x1i, x2i) coordinates when d = 0 and 
        ## (x1f, x2f) when d = 1.  This will be
        ## used for plotting
        
        x1Zeros <- x1[d == 0]
        x2Zeros <- x2[d == 0]
        xZeros <- cbind(x1Zeros, x2Zeros)
        
        x1Ones <- x1[d == 1] 
        x2Ones <- x2[d == 1]
        xOnes <- cbind(x1Ones, x2Ones)
        
        ## For each iteration do:
        
        for (i in 1:iterations) {
                
                ## Finish execution if leaning is over
                
                if (found == TRUE) {
                        
                        break
                        
                }
                
                ## For each input value do:
                
                for (k in 1:4) {
                        
                        ## Calculate a1 (Aggregation function)
                        
                        a1 <- w10 + (x1[k] * w11) + (x2[k] * w12)
                        
                        ## Calculate y (Output or Axon)
                        
                        y <- if (a1 < 0) {
                                0
                        } else {
                                1
                        }
                        
                        ## Calculate the error and fill the err
                        ## vector and the errorVector vectors
                        
                        error <- -(y - d[k])
                        err[k] <- error
                        errorVector[j] <- error
                        j <- j + 1
                        
                        ## Weight actualization & print their
                        ## actual value
                        
                        w10 <- w10 + (error * 1) ## x0 = 1 para el perceptron
                        w11 <- w11 + (error * x1[k]) 
                        w12 <- w12 + (error * x2[k])
                        
                        cat("i:", i, "     k:", k, "     w10: ", w10, 
                            "     w11:", w11, "     w12:", w12, "\n\n")
                        
                        ## Print the equation
                        
                        m <- -w11/w12
                        b <- -w10/w12
                        
                        cat("Equation:", "\n")
                        cat("x2 = ", m, "x1 + ", b, "\n\n")
                        
                        ## PLOTTING
                        
                        ## Plot the frame and define title, subtitle, 
                        ## axis legends
                        
                        par(col = "black", col.lab = "blue", pch = 1)
                        invisible(plot(x1, x2, main = "PERCEPTRON", type="n", 
                             xlim = 2 * range(x), 
                             ylim = 2 * range(x), xlab="X1",
                             ylab="X2"))
                        
                        ## Add (x1, x2) coordinates that make
                        ## d = 0 as blue diamonds
                        
                        par(pch = 18)
                        points(x1Zeros, x2Zeros, type = "p", 
                               col = "sky blue", xlab = "X1", 
                               ylab = "X2")
                        
                        ## Add (x1, x2) coordinates that make
                        ## d = 1 as red filled in circles
                        
                        par(pch = 19)
                        points(x1Ones, x2Ones, type="p", col="red", 
                               xlab="X1", ylab="X2")
                        
                        ## Add the curve plot
                        
                        par(col = "forestgreen")                        
                        curve(m * x + b, from = 0, to = 2 * max(x), 
                              add = TRUE)
                        
                        ## Add legend
                        
                        par(col = "blueviolet")
                        legend("topright", title = "Trained by:", legend = inputName, 
                               bty = "n")
                        
                }
                
                ## Make k = 1 again to loop over each value 
                ## of the inputs again
                
                k = 1
                
                ## Performance measure: Median square error &
                ## filling the mse vector
                
                ecm <- (mean(err^2))/2
                mse[i] <- ecm
                
                ## Compare the value of ecm, if it's zero, then 
                ## make 'found' true
                
                if (abs(ecm) == 0) {
                        
                        cat("I HAVE FOUND A SOLUTION", "\n\n")
                        cat("It took", i, "iterations", "\n\n")
                        cat("The solution is: ", "\n\n")
                        cat("w10: ", w10, 
                            "     w11:", w11, "     w12:", w12, "\n\n")
                        cat("Equation:", "\n\n")
                        cat("x2 = ", m, "x1 + ", b, "\n\n")
                        
                        found = TRUE
                        
                        break
                        
                }   
                
        }
        
        if (found == FALSE) {
                
                cat("I WAS NOT ABLE TO FIND A SOLUTION", "\n\n")
                cat("I made", i, "iterations", "\n\n")
                cat("The last iteration values are: ", "\n\n")
                cat("w10: ", w10, "     w11:", w11, "     w12:", w12, "\n\n")
                cat("Equation:", "\n\n")
                cat("x2 = ", m, "x1 + ", b, "\n\n")
                
        } 
        
        ## Return mse
        
        cat("mse:", "\n\n")
        mse
        
}

## Set working directory at 'NeuralNetworks' folder

setwd("/Users/usuario/Desktop/NeuralNetworks/")

## Initialization of aleatory weights

weight <- signif(runif(3, min = -1, max = 1), digits = 2)

## AND

print("AND")

x1 <- c(0, 0, 1, 1)
x2 <- c(0, 1, 0, 1)
d <- c(0, 0, 0, 1)

x <- cbind(x1, x2, d)

x
weight

learn(x = x, weight = weight, inputName = "AND", iterations = 15)

## Initialization of aleatory weights

weight <- signif(runif(3, min = -1, max = 1), digits = 2)

## OR

print("OR")

x1 <- c(0, 0, 1, 1)
x2 <- c(0, 1, 0, 1)
d <- c(1, 1, 1, 0)

x <- cbind(x1, x2, d)

x
weight

learn(x = x, weight = weight, inputName = "OR", iterations = 15)

## XOR

print("XOR")

x1 <- c(0, 0, 1, 1)
x2 <- c(0, 1, 0, 1)
d <- c(0, 1, 1, 0)

x <- cbind(x1, x2, d)

x
weight

learn(x = x, weight = weight, inputName = "XOR",iterations = 2)
