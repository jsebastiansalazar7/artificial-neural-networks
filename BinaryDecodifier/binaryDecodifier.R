learn <- function(directory, activation = "adaline", weight, inputName = NULL, iterations = 50, alpha = 1.0, mseAccepted = 0.01) {
        
        ## Subset column vectors for each neuron
        
        w <- weight ## weight vector for the 1st neuron
        
        ## read dataset
        
        data <- read.csv(directory)
        
        ## dataset size
        
        dataSize <- nrow(data)
        
        ## Subset column vectors from dataset
        
        k <- data[, 1]
        bias <- data[, 2]
        x1 <- data[, 3]
        x2 <- data[, 4]
        x3 <- data[, 5]
        x4 <- data[, 6]
        d <- data[, 7]
        y <- data[, 8]
        err <- data[, 9]
        mse <- data[, 10]
        
        ## Vector used to store mse and plot it
        
        meanSquareError <- vector("numeric")
        
        ## Variable to break the function when learning is over
        
        found = FALSE
        
        ## For each iteration make:
        
        for (i in 1:iterations) {
                
                ## Finish execution if learning is over
                
                if (found == TRUE) {
                        
                        break
                        
                }
                
                ## For each input's value do:
                
                for (k in 1:dataSize) {
                        
                        ## Calcule the aggregation function
                        ## for the neuron
                        
                        a <- (w[1] * bias[k]) + (w[2] * x1[k]) +
                                (w[3] * x2[k]) + (w[4] * x3[k]) +
                                (w[5] * x4[k])
                        
                        
                        ## When activation function is adaline do
                        
                        if (activation == "adaline") {
                                
                                ## Calculate Output or Axon
                                
                                y[k] <- a ## Axon 1
                                
                                ## Calculate the errors and fill the
                                ## err vectors
                                
                                error <- -(y[k] - d[k]) ## 1st Neuron
                                err[k] <- error
                                
                                ## Knowledge Base Actualization
                                
                                w[1] <- w[1] + (alpha * (error * bias[k]))
                                w[2] <- w[2] + (alpha * (error * x1[k]))
                                w[3] <- w[3] + (alpha * (error * x2[k]))
                                w[4] <- w[4] + (alpha * (error * x3[k]))
                                w[5] <- w[5] + (alpha * (error * x4[k]))
                                
                                ## Also print weight values
                                
                                weight <- cbind(i, k, w)
                                knowledgeBase <- w
                                
                        } else if (activation == "perceptron") {
                                
                                ## Calculate Output or Axon
                                
                                y[k] <- if (a < 0) {
                                        0
                                } else {
                                        1        
                                }
                                
                                ## Calculate the errors and fill the
                                ## err vector
                                
                                error <- -(y[k] - d[k]) 
                                err[k] <- error
                                
                                ## Knowledge Base Actualization
                                
                                w[1] <- w[1] + (error * bias[k])
                                w[2] <- w[2] + (error * x1[k])
                                w[3] <- w[3] + (error * x2[k])
                                w[4] <- w[4] + (error * x3[k])
                                w[5] <- w[5] + (error * x4[k])
                                
                                ## Also print weight values
                                
                                weight <- cbind(i, k, w)
                                knowledgeBase <- w
                                
                        }
                        
                }
                
                ## Make k = 1 again to loop over each value 
                ## of the inputs again
                
                k = 1
                
                ## Performance measure: Median square error &
                ## filling the mse vector
                
                ecm <- (mean(err^2))/2 
                mse[i] <- ecm
                meanSquareError[i] <- ecm
                
                ## Compare the value of ecm, if it's close to 
                ## zero, then make 'found' true
                
                if (abs(ecm) <= mseAccepted) {
                        
                        cat("I HAVE FOUND A SOLUTION", "\n\n")
                        cat("It took", i, "iterations", "\n\n")
                        cat("The solution is: ", "\n\n")
                        cat("w0 = ", w[1], "w1 = ", w[2], "w2 = ", w[3], 
                            "w3 = ", w[4], "w4 = ", w[5], "\n\n")
                        
                        found = TRUE
                        
                        break
                        
                }
                
        }
        
        if (found == FALSE) {
                
                cat("I WAS NOT ABLE TO FIND A SOLUTION", "\n\n")
                cat("I made", i, "iterations", "\n\n")
                cat("The last iteration values are: ", "\n\n")
                cat("w0 = ", w[1], "w1 = ", w[2], "w2 = ", w[3], 
                    "w3 = ", w[4], "w4 = ", w[5], "\n\n")
                
        }
        
        ## Print the equation
        
        cat("Equation:", "\n\n")
        
        cat("y = ", w[1], " + ", w[2], "x1", 
            " + ", w[3], "x2", " + ", w[4], "x3", 
            " + ", w[5], "x4", "\n\n")
        
        ## Create a csv file with the resultant knowledge base
        ## after the training
        
        if (activation == "perceptron") {
                
                write.csv(knowledgeBase, 
                          file = "BinaryDecodifier/Results/KnowledgeBase/KnowlegeBasePerceptron.csv", 
                          row.names = FALSE)
                
                ## PLOTTING
                
                ## Plot the frame and define the title, 
                ## subtitle and axis legends
                
                index <- 1:i
                
                range <- range(meanSquareError) ## Necessary because the
                                                ## mse do not converge for
                                                ## the perceptron
                
                par(col = "black", col.lab = "blue", pch = 1)
                invisible(plot(index, meanSquareError, col = "red",
                               main = "PERCEPTRON", 
                               type = "l", xlim = 1.2 * range(index), 
                               ylim = 1.4 * c(0, range[2]), 
                               xlab = "iterations", ylab = "MSE"))
                
                ## Add legend
                
                par(col = "blueviolet")
                legend("topright", title = "Trained by:", legend = inputName, 
                       bty = "n")
                
        } else if (activation == "adaline") {
                
                write.csv(knowledgeBase, 
                          file = "BinaryDecodifier/Results/KnowledgeBase/KnowlegeBaseAdaline.csv", 
                          row.names = FALSE)
                
                ## PLOTTING
                
                ## Plot the frame and define the title, 
                ## subtitle and axis legends
                
                index <- 1:i
                
                par(col = "black", col.lab = "blue", lwd = 2,  pch = 1)
                plot(index, meanSquareError, col = "red",
                               main = "ADALINE", 
                               type = "l", xlim = 1.2 * range(index), 
                               ylim = 1.2 * range(meanSquareError), 
                               xlab = "iterations", ylab = "MSE")
                
                
                ## Add legend
                
                par(col = "blueviolet")
                legend("topright", title = "Trained by:", legend = inputName, 
                       bty = "n")
                
        }
        
        ## Complete with NA's when it's necessary.  The weight vectors
        ## have length = 16 since there are 16 inputs, but the other
        ## vectors have length = 10 since there are only 10 digits.
        
        vectorSize <- length(mse)
        
        length(x1) = vectorSize
        length(x2) = vectorSize
        length(x3) = vectorSize
        length(x4) = vectorSize
        length(knowledgeBase) = vectorSize
        length(d) = vectorSize
        length(y) = vectorSize
        length(err) = vectorSize
        length(bias) = vectorSize
        
        ## Create a csv file with all the resultant information
        
        resultantData <- cbind(bias, x1, x2, x3, x4, knowledgeBase, 
                               d, y, err, mse)
        
        if (activation == "perceptron") {
                
                write.csv(resultantData, 
                          file = "BinaryDecodifier/Results/TrainedOutput/TrainedOutputPerceptron.csv", 
                          row.names = FALSE, na = "")
                
        } else if (activation == "adaline") {
                
                write.csv(resultantData, 
                          file = "BinaryDecodifier/Results/TrainedOutput/TrainedOutputAdaline.csv", 
                          row.names = FALSE, na = "")
                
        }
        
        ## Return value
        
        invisible(w)
        
}

## Set working directory at 'NeuralNetworks' folder

setwd("/Users/usuario/Desktop/NeuralNetworks/")

## Initialization of aleatory weights between -1 and 1

weight <- round(runif(5, min = -1, max = 1), digits = 2)
weight

dataDirectory = "/Users/usuario/Desktop/NeuralNetworks/BinaryDecodifier/dataset/datasetBinaryDecodifier.csv"

learn(directory = dataDirectory, activation = "adaline", 
      weight = weight, inputName = "datasetBinaryDecodifier", 
      iterations = 500, alpha = 0.05, mseAccepted = 1e-20)

learn(directory = dataDirectory, activation = "perceptron", 
      weight = weight, inputName = "datasetBinaryDecodifier", 
      iterations = 200, alpha = 1.0, mseAccepted = 0)
