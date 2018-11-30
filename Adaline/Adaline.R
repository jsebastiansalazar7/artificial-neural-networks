learn <- function(directory, weight, inputName = NULL, iterations = 50, alpha = 0.005, mseAccepted = 0.01) {
        
        ## Subset weight vectors
        
        w10 <- weight[1]
        w11 <- weight[2]
        
        ## read dataset
        
        data <- read.csv(directory)
        
        ## dataset size
        
        dataSize <- nrow(data)
        
        ## Subset column vectors from dataset
        
        ## (x1, d) are the point's coordinates that are going to
        ## be used for plotting
        
        k <- data[, 1] ## Number of measures (from 1 to 97)
        bias <- data[, 2] ## Bias value (x0 = 1)
        x1 <- data[, 3] ## Input Value x1
        yVect <- data[, 4] ## Output y
        d <- data[, 5] ## Desired output d
        err <- data[, 6] ## Error
        mse <- data[, 7] ## Mean Square Error
        
        ## Make mse length = number of iterations:
        
        length(mse) <- iterations
        
        ## Variable to break the function when leaning is over
        
        found = FALSE
        
        ## For each iteration do:
        
        for (i in 1:iterations) {
                
                ## Finish execution if leaning is over
                
                if (found == TRUE) {
                        
                        break
                        
                }
                
                ## For each input's value do:
                
                for (k in 1:dataSize) {
                        
                        ## Calculate a1 (Aggregation function)
                        
                        a1 <- w10 + (x1[k] * w11)
                        
                        ## Calculate y (Output or Axon)
                        
                        y <- a1
                        
                        ## Calculate the error and fill the err vector
                        
                        error <- -(y - d[k])
                        err[k] <- error
                        
                        ## Weight actualization & print their 
                        ## actual value
                        
                        w10 <- w10 + (alpha * (error * bias[k])) ## x0 = 1 for the perceptron
                        w11 <- w11 + (alpha * (error * x1[k])) 
                        
                        knowledgeBase <- cbind(w10, w11)
                        
                        cat("i:", i, "     k:", k, "     w10: ", w10, 
                            "     w11:", "\n\n")
                        
                        ## Print the equation
                        
                        cat("Equation:", "\n")
                        cat("y = ", w10, " + ", w11, "x1", "\n\n")
                        
                        ## PLOTTING
                        
                        ## Plot the frame and define the title, 
                        ## subtitle and axis legends
                        
                        par(col = "black", col.lab = "blue", pch = 1)
                        invisible(plot(x1, d, main = "ADALINE", 
                                       type = "n", xlim = 2 * range(x1), 
                                       ylim = 2 * range(d), 
                                       xlab = "X1", ylab = "Y"))
                        
                        ## Add (x1, d) coordinates as blue
                        ## diamonds
                        
                        par(pch = 18)
                        points(x1, d, type = "p", col = "sky blue",
                               xlab = "X1", ylab = "Y")
                        
                        ## Add the curve
                        
                        par(col = "red")
                        curve(w10 + w11 * x, from = 0, to = 2 * max(x1), 
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
                
                ## Compare the value of ecm, if it's close to 
                ## zero, then make 'found' true
                
                if (abs(ecm) < mseAccepted) {
                        
                        cat("I HAVE FOUND A SOLUTION", "\n\n")
                        cat("It took", i, "iterations", "\n\n")
                        cat("The solution is: ", "\n\n")
                        cat("w10: ", w10, 
                            "     w11:", w11, "\n\n")
                        cat("Equation:", "\n")
                        cat("y = ", w10, " + ", w11, "x1", "\n\n")
                        
                        found = TRUE
                        
                        break
                        
                }
                
        }
        
        ## Save the trained knowledge base
        
        write.csv(knowledgeBase, 
                  file = "Adaline/Results/KnowledgeBase/KnowlegeBaseAdaline.csv", 
                  row.names = FALSE)
        
        ## Complete with NA's when it's necessary.  The mse vector
        ## is longer than the other ones.
        
        vectorSize <- length(mse)
        
        length(k) <- vectorSize
        length(bias) <- vectorSize
        length(x1) <- vectorSize
        length(d) <- vectorSize
        length(yVect) <- vectorSize
        length(knowledgeBase) <- vectorSize
        length(err) <- vectorSize
        
        ## Save the resultant data
        
        resultantData <- cbind(k, bias, x1, d, yVect, knowledgeBase, 
                               err, mse)
        
        write.csv(resultantData, 
                  file = "Adaline/Results/TrainedOutput/TrainedOutputAdaline.csv", 
                  row.names = FALSE, na = "")
        
        if (found == FALSE) {
                
                cat("I WAS NOT ABLE TO FIND A SOLUTION", "\n\n")
                cat("I made", i, "iterations", "\n\n")
                cat("The last iteration values are: ", "\n\n")
                cat("w10: ", w10, "     w11:", w11, "\n\n")
                cat("Equation:", "\n")
                cat("y = ", w10, " + ", w11, "x1", "\n\n")
                
        } 
        
        ## Return ecm
        cat("mse: ", "\n\n")
        mse
        
}

## Set working directory at 'NeuralNetworks' folder

setwd("/Users/usuario/Desktop/NeuralNetworks/")

## Initialization of aleatory weights

## Using round().  signif() can also be used

weight <- round(runif(2, min = -1, max = 1), digits = 2)

weight

dataDirectory = "/Users/usuario/Desktop/NeuralNetworks/Adaline/dataset/datasetAdaline.csv"

learn(directory = dataDirectory, weight = weight, 
      inputName ="datasetAdaline",iterations = 50, 
      alpha = 0.005, mseAccepted = 0.01)
