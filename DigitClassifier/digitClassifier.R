learn <- function(directory, activation = "adaline", weight, inputName = NULL, iterations = 50, alpha = 1.0, mseAccepted = 0.01) {
        
        ## Subset column vectors for each neuron
        
        w1 <- weight[, 1] ## weight vector for the 1st neuron
        w2 <- weight[, 2] ## weight vector for the 2nd neuron
        w3 <- weight[, 3] ## weight vector for the 3rd neuron
        w4 <- weight[, 4] ## weight vector for the 4th neuron
        
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
        x5 <- data[, 7]
        x6 <- data[, 8]
        x7 <- data[, 9]
        x8 <- data[, 10]
        x9 <- data[, 11]
        x10 <- data[, 12]
        x11 <- data[, 13]
        x12 <- data[, 14]
        x13 <- data[, 15]
        x14 <- data[, 16]
        x15 <- data[, 17]
        d0 <- data[, 18]
        d1 <- data[, 19]
        d2 <- data[, 20]
        d3 <- data[, 21]
        y0 <- data[, 22]
        y1 <- data[, 23]
        y2 <- data[, 24]
        y3 <- data[, 25]
        n <- data[, 26]
        err0 <- data[, 27]
        err1 <- data[, 28]
        err2 <- data[, 29]
        err3 <- data[, 30]
        mse0 <- data[, 31]
        mse1 <- data[, 32]
        mse2 <- data[, 33]
        mse3 <- data[, 34]
        mseTotal <- data[, 35]
        
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
                        ## for each neuron
                        
                        ## 1st Neuron
                        
                        a0 <- (w1[1] * bias[k]) + (w1[2] * x1[k]) +
                                (w1[3] * x2[k]) + (w1[4] * x3[k]) +
                                (w1[5] * x4[k]) + (w1[6] * x5[k]) +
                                (w1[7] * x6[k]) + (w1[8] * x7[k]) +
                                (w1[9] * x8[k]) + (w1[10] * x9[k]) +
                                (w1[11] * x10[k]) + (w1[12] * x11[k]) +
                                (w1[13] * x12[k]) + (w1[14] * x13[k]) +
                                (w1[15] * x14[k]) + (w1[16] * x15[k])
                        
                        ## 2nd Neuron
                        
                        a1 <- (w2[1] * bias[k]) + (w2[2] * x1[k]) +
                                (w2[3] * x2[k]) + (w2[4] * x3[k]) +
                                (w2[5] * x4[k]) + (w2[6] * x5[k]) +
                                (w2[7] * x6[k]) + (w2[8] * x7[k]) +
                                (w2[9] * x8[k]) + (w2[10] * x9[k]) +
                                (w2[11] * x10[k]) + (w2[12] * x11[k]) +
                                (w2[13] * x12[k]) + (w2[14] * x13[k]) +
                                (w2[15] * x14[k]) + (w2[16] * x15[k])
                        
                        ## 3rd Neuron
                        
                        a2 <- (w3[1] * bias[k]) + (w3[2] * x1[k]) +
                                (w3[3] * x2[k]) + (w3[4] * x3[k]) +
                                (w3[5] * x4[k]) + (w3[6] * x5[k]) +
                                (w3[7] * x6[k]) + (w3[8] * x7[k]) +
                                (w3[9] * x8[k]) + (w3[10] * x9[k]) +
                                (w3[11] * x10[k]) + (w3[12] * x11[k]) +
                                (w3[13] * x12[k]) + (w3[14] * x13[k]) +
                                (w3[15] * x14[k]) + (w3[16] * x15[k])
                        
                        ## 4th Neuron
                        
                        a3 <- (w4[1] * bias[k]) + (w4[2] * x1[k]) +
                                (w4[3] * x2[k]) + (w4[4] * x3[k]) +
                                (w4[5] * x4[k]) + (w4[6] * x5[k]) +
                                (w4[7] * x6[k]) + (w4[8] * x7[k]) +
                                (w4[9] * x8[k]) + (w4[10] * x9[k]) +
                                (w4[11] * x10[k]) + (w4[12] * x11[k]) +
                                (w4[13] * x12[k]) + (w4[14] * x13[k]) +
                                (w4[15] * x14[k]) + (w4[16] * x15[k])
                        
                        ## When activation function is adaline do
                        
                        if (activation == "adaline") {
                                
                                ## Calculate Output or Axon
                                
                                y0[k] <- a0 ## Axon 1
                                y1[k] <- a1 ## Axon 2
                                y2[k] <- a2 ## Axon 3
                                y3[k] <- a3 ## Axon 4
                                
                                ## Calculate the errors and fill the
                                ## err vectors
                                
                                error0 <- -(y0[k] - d0[k]) ## 1st Neuron
                                err0[k] <- error0
                                
                                error1 <- -(y1[k] - d1[k]) ## 2nd Neuron
                                err1[k] <- error1
                                
                                error2 <- -(y2[k] - d2[k]) ## 3rd Neuron
                                err2[k] <- error2
                                
                                error3 <- -(y3[k] - d3[k]) ## 4th Neuron
                                err3[k] <- error3
                                
                                ## Knowledge Base Actualization
                                
                                ## 1st Neuron
                                
                                w1[1] <- w1[1] + (alpha * (error0 * bias[k]))
                                w1[2] <- w1[2] + (alpha * (error0 * x1[k]))
                                w1[3] <- w1[3] + (alpha * (error0 * x2[k]))
                                w1[4] <- w1[4] + (alpha * (error0 * x3[k]))
                                w1[5] <- w1[5] + (alpha * (error0 * x4[k]))
                                w1[6] <- w1[6] + (alpha * (error0 * x5[k]))
                                w1[7] <- w1[7] + (alpha * (error0 * x6[k]))
                                w1[8] <- w1[8] + (alpha * (error0 * x7[k]))
                                w1[9] <- w1[9] + (alpha * (error0 * x8[k]))
                                w1[10] <- w1[10] + (alpha * (error0 * x9[k]))
                                w1[11] <- w1[11] + (alpha * (error0 * x10[k]))
                                w1[12] <- w1[12] + (alpha * (error0 * x11[k]))
                                w1[13] <- w1[13] + (alpha * (error0 * x12[k]))
                                w1[14] <- w1[14] + (alpha * (error0 * x13[k]))
                                w1[15] <- w1[15] + (alpha * (error0 * x14[k]))
                                w1[16] <- w1[16] + (alpha * (error0 * x15[k]))
                                
                                ## 2nd Neuron
                                
                                w2[1] <- w2[1] + (alpha * (error1 * bias[k]))
                                w2[2] <- w2[2] + (alpha * (error1 * x1[k]))
                                w2[3] <- w2[3] + (alpha * (error1 * x2[k]))
                                w2[4] <- w2[4] + (alpha * (error1 * x3[k]))
                                w2[5] <- w2[5] + (alpha * (error1 * x4[k]))
                                w2[6] <- w2[6] + (alpha * (error1 * x5[k]))
                                w2[7] <- w2[7] + (alpha * (error1 * x6[k]))
                                w2[8] <- w2[8] + (alpha * (error1 * x7[k]))
                                w2[9] <- w2[9] + (alpha * (error1 * x8[k]))
                                w2[10] <- w2[10] + (alpha * (error1 * x9[k]))
                                w2[11] <- w2[11] + (alpha * (error1 * x10[k]))
                                w2[12] <- w2[12] + (alpha * (error1 * x11[k]))
                                w2[13] <- w2[13] + (alpha * (error1 * x12[k]))
                                w2[14] <- w2[14] + (alpha * (error1 * x13[k]))
                                w2[15] <- w2[15] + (alpha * (error1 * x14[k]))
                                w2[16] <- w2[16] + (alpha * (error1 * x15[k]))
                                
                                ## 3rd Neuron
                                
                                w3[1] <- w3[1] + (alpha * (error2 * bias[k]))
                                w3[2] <- w3[2] + (alpha * (error2 * x1[k]))
                                w3[3] <- w3[3] + (alpha * (error2 * x2[k]))
                                w3[4] <- w3[4] + (alpha * (error2 * x3[k]))
                                w3[5] <- w3[5] + (alpha * (error2 * x4[k]))
                                w3[6] <- w3[6] + (alpha * (error2 * x5[k]))
                                w3[7] <- w3[7] + (alpha * (error2 * x6[k]))
                                w3[8] <- w3[8] + (alpha * (error2 * x7[k]))
                                w3[9] <- w3[9] + (alpha * (error2 * x8[k]))
                                w3[10] <- w3[10] + (alpha * (error2 * x9[k]))
                                w3[11] <- w3[11] + (alpha * (error2 * x10[k]))
                                w3[12] <- w3[12] + (alpha * (error2 * x11[k]))
                                w3[13] <- w3[13] + (alpha * (error2 * x12[k]))
                                w3[14] <- w3[14] + (alpha * (error2 * x13[k]))
                                w3[15] <- w3[15] + (alpha * (error2 * x14[k]))
                                w3[16] <- w3[16] + (alpha * (error2 * x15[k]))
                                
                                ## 4th Neuron
                                
                                w4[1] <- w4[1] + (alpha * (error3 * bias[k]))
                                w4[2] <- w4[2] + (alpha * (error3 * x1[k]))
                                w4[3] <- w4[3] + (alpha * (error3 * x2[k]))
                                w4[4] <- w4[4] + (alpha * (error3 * x3[k]))
                                w4[5] <- w4[5] + (alpha * (error3 * x4[k]))
                                w4[6] <- w4[6] + (alpha * (error3 * x5[k]))
                                w4[7] <- w4[7] + (alpha * (error3 * x6[k]))
                                w4[8] <- w4[8] + (alpha * (error3 * x7[k]))
                                w4[9] <- w4[9] + (alpha * (error3 * x8[k]))
                                w4[10] <- w4[10] + (alpha * (error3 * x9[k]))
                                w4[11] <- w4[11] + (alpha * (error3 * x10[k]))
                                w4[12] <- w4[12] + (alpha * (error3 * x11[k]))
                                w4[13] <- w4[13] + (alpha * (error3 * x12[k]))
                                w4[14] <- w4[14] + (alpha * (error3 * x13[k]))
                                w4[15] <- w4[15] + (alpha * (error3 * x14[k]))
                                w4[16] <- w4[16] + (alpha * (error3 * x15[k]))
                                
                                ## Also print weight values
                                
                                w <- cbind(i, k, w1, w2, w3, w4)
                                knowledgeBase <- cbind(w1, w2, w3, w4)
                                
                        } else if (activation == "perceptron") {
                                
                                ## Calculate Output or Axon
                                
                                ## Axon 1
                                
                                y0[k] <- if (a0 < 0) {
                                        0
                                } else {
                                        1        
                                }
                                
                                ## Axon 2
                                
                                y1[k] <- if (a1 < 0) {
                                        0
                                } else {
                                        1        
                                }
                                
                                ## Axon 3
                                
                                y2[k] <- if (a2 < 0) {
                                        0
                                } else {
                                        1        
                                }
                                
                                ## Axon 4
                                
                                y3[k] <- if (a3 < 0) {
                                        0
                                } else {
                                        1        
                                }
                                
                                ## Calculate the errors and fill the
                                ## err vectors
                                
                                error0 <- -(y0[k] - d0[k]) ## 1st Neuron
                                err0[k] <- error0
                                
                                error1 <- -(y1[k] - d1[k]) ## 2nd Neuron
                                err1[k] <- error1
                                
                                error2 <- -(y2[k] - d2[k]) ## 3rd Neuron
                                err2[k] <- error2
                                
                                error3 <- -(y3[k] - d3[k]) ## 4th Neuron
                                err3[k] <- error3
                                
                                ## Knowledge Base Actualization
                                
                                ## 1st Neuron
                                
                                w1[1] <- w1[1] + (error0 * bias[k])
                                w1[2] <- w1[2] + (error0 * x1[k])
                                w1[3] <- w1[3] + (error0 * x2[k])
                                w1[4] <- w1[4] + (error0 * x3[k])
                                w1[5] <- w1[5] + (error0 * x4[k])
                                w1[6] <- w1[6] + (error0 * x5[k])
                                w1[7] <- w1[7] + (error0 * x6[k])
                                w1[8] <- w1[8] + (error0 * x7[k])
                                w1[9] <- w1[9] + (error0 * x8[k])
                                w1[10] <- w1[10] + (error0 * x9[k])
                                w1[11] <- w1[11] + (error0 * x10[k])
                                w1[12] <- w1[12] + (error0 * x11[k])
                                w1[13] <- w1[13] + (error0 * x12[k])
                                w1[14] <- w1[14] + (error0 * x13[k])
                                w1[15] <- w1[15] + (error0 * x14[k])
                                w1[16] <- w1[16] + (error0 * x15[k])
                                
                                ## 2nd Neuron
                                
                                w2[1] <- w2[1] + (error1 * bias[k])
                                w2[2] <- w2[2] + (error1 * x1[k])
                                w2[3] <- w2[3] + (error1 * x2[k])
                                w2[4] <- w2[4] + (error1 * x3[k])
                                w2[5] <- w2[5] + (error1 * x4[k])
                                w2[6] <- w2[6] + (error1 * x5[k])
                                w2[7] <- w2[7] + (error1 * x6[k])
                                w2[8] <- w2[8] + (error1 * x7[k])
                                w2[9] <- w2[9] + (error1 * x8[k])
                                w2[10] <- w2[10] + (error1 * x9[k])
                                w2[11] <- w2[11] + (error1 * x10[k])
                                w2[12] <- w2[12] + (error1 * x11[k])
                                w2[13] <- w2[13] + (error1 * x12[k])
                                w2[14] <- w2[14] + (error1 * x13[k])
                                w2[15] <- w2[15] + (error1 * x14[k])
                                w2[16] <- w2[16] + (error1 * x15[k])
                                
                                ## 3rd Neuron
                                
                                w3[1] <- w3[1] + (error2 * bias[k])
                                w3[2] <- w3[2] + (error2 * x1[k])
                                w3[3] <- w3[3] + (error2 * x2[k])
                                w3[4] <- w3[4] + (error2 * x3[k])
                                w3[5] <- w3[5] + (error2 * x4[k])
                                w3[6] <- w3[6] + (error2 * x5[k])
                                w3[7] <- w3[7] + (error2 * x6[k])
                                w3[8] <- w3[8] + (error2 * x7[k])
                                w3[9] <- w3[9] + (error2 * x8[k])
                                w3[10] <- w3[10] + (error2 * x9[k])
                                w3[11] <- w3[11] + (error2 * x10[k])
                                w3[12] <- w3[12] + (error2 * x11[k])
                                w3[13] <- w3[13] + (error2 * x12[k])
                                w3[14] <- w3[14] + (error2 * x13[k])
                                w3[15] <- w3[15] + (error2 * x14[k])
                                w3[16] <- w3[16] + (error2 * x15[k])
                                
                                ## 4th Neuron
                                
                                w4[1] <- w4[1] + (error3 * bias[k])
                                w4[2] <- w4[2] + (error3 * x1[k])
                                w4[3] <- w4[3] + (error3 * x2[k])
                                w4[4] <- w4[4] + (error3 * x3[k])
                                w4[5] <- w4[5] + (error3 * x4[k])
                                w4[6] <- w4[6] + (error3 * x5[k])
                                w4[7] <- w4[7] + (error3 * x6[k])
                                w4[8] <- w4[8] + (error3 * x7[k])
                                w4[9] <- w4[9] + (error3 * x8[k])
                                w4[10] <- w4[10] + (error3 * x9[k])
                                w4[11] <- w4[11] + (error3 * x10[k])
                                w4[12] <- w4[12] + (error3 * x11[k])
                                w4[13] <- w4[13] + (error3 * x12[k])
                                w4[14] <- w4[14] + (error3 * x13[k])
                                w4[15] <- w4[15] + (error3 * x14[k])
                                w4[16] <- w4[16] + (error3 * x15[k])
                                
                                ## Also print weight values
                                
                                w <- cbind(i, k, w1, w2, w3, w4)
                                knowledgeBase <- cbind(w1, w2, w3, w4)
                                
                        }
                        
                }
                        
                ## Make k = 1 again to loop over each value 
                ## of the inputs again
                
                k = 1
                
                ## Performance measure: Median square error &
                ## filling the mse vector
                
                ecm0 <- (mean(err0^2))/2 ## 1st Neuron
                mse0[i] <- ecm0
                
                ecm1 <- (mean(err1^2))/2 ## 2nd Neuron
                mse1[i] <- ecm1
                
                ecm2 <- (mean(err2^2))/2 ## 3rd Neuron
                mse2[i] <- ecm2
                
                ecm3 <- (mean(err3^2))/2 ## 1st Neuron
                mse3[i] <- ecm3
                
                ## Make an ecm Vector
                
                mseVector <- cbind(ecm0, ecm1, ecm2, ecm3)
                
                ## GLOBAL MSE
                
                mseGlobal <- (mean(mseVector^2))/2
                mseTotal[i] <- mseGlobal
                
                ## Compare the value of ecm, if it's close to 
                ## zero, then make 'found' true
                
                if (abs(mseGlobal) <= mseAccepted) {
                        
                        cat("I HAVE FOUND A SOLUTION", "\n\n")
                        cat("It took", i, "iterations", "\n\n")
                        cat("The solution is: ", "\n\n")
                        print(w)
                        cat("\n\n")
                        
                        found = TRUE
                        
                        break
                
                }
                
        }
        
        if (found == FALSE) {
                
                cat("I WAS NOT ABLE TO FIND A SOLUTION", "\n\n")
                cat("I made", i, "iterations", "\n\n")
                cat("The last iteration values are: ", "\n\n")
                print(w)
                cat("\n\n")
        
        }
        
        ## Print the equations
        
        cat("Equations:", "\n\n")
        
        cat("y0 = ", w1[1], " + ", w1[2], "x1", 
            " + ", w1[3], "x2", " + ", w1[4], "x3", 
            " + ", w1[5], "x4", " + ", w1[6], "x5", 
            " + ", w1[7], "x6", " + ", w1[8], "x7", 
            " + ", w1[9], "x8", " + ", w1[10], "x9",
            " + ", w1[11], "x10", " + ", w1[12], "x11",
            " + ", w1[13], "x12", " + ", w1[14], "x13",
            " + ", w1[15], "x14", " + ", w1[16], "x15",
            "\n\n")
        
        cat("y1 = ", w2[1], " + ", w2[2], "x1", 
            " + ", w2[3], "x2", " + ", w2[4], "x3", 
            " + ", w2[5], "x4", " + ", w2[6], "x5", 
            " + ", w2[7], "x6", " + ", w2[8], "x7", 
            " + ", w2[9], "x8", " + ", w2[10], "x9",
            " + ", w2[11], "x10", " + ", w2[12], "x11",
            " + ", w2[13], "x12", " + ", w2[14], "x13", 
            " + ", w2[15], "x14", " + ", w2[16], "x15",
            "\n\n")
        
        cat("y2 = ", w3[1], " + ", w3[2], "x1", 
            " + ", w3[3], "x2", " + ", w3[4], "x3", 
            " + ", w3[5], "x4", " + ", w3[6], "x5", 
            " + ", w3[7], "x6", " + ", w3[8], "x7", 
            " + ", w3[9], "x8", " + ", w3[10], "x9",
            " + ", w3[11], "x10", " + ", w3[12], "x11",
            " + ", w3[13], "x12", " + ", w3[14], "x13",
            " + ", w3[15], "x14", " + ", w3[16], "x15",
            "\n\n")
        
        cat("y3 = ", w4[1], " + ", w4[2], "x1", 
            " + ", w4[3], "x2", " + ", w4[4], "x3", 
            " + ", w4[5], "x4", " + ", w4[6], "x5", 
            " + ", w4[7], "x6", " + ", w4[8], "x7", 
            " + ", w4[9], "x8", " + ", w4[10], "x9",
            " + ", w4[11], "x10", " + ", w4[12], "x11",
            " + ", w4[13], "x12", " + ", w4[14], "x13",
            " + ", w4[15], "x14", " + ", w4[16], "x15",
            "\n\n")
        
        ## Create a csv file with the resultant knowledge base
        ## after the training
        
        if (activation == "perceptron") {
                
                write.csv(knowledgeBase, 
                          file = "DigitClassifier/Results/KnowledgeBase/KnowledgeBasePerceptron.csv", 
                          row.names = FALSE)
                
        } else if (activation == "adaline") {
                
                write.csv(knowledgeBase, 
                          file = "DigitClassifier/Results/KnowledgeBase/KnowledgeBaseAdaline.csv", 
                          row.names = FALSE)
                
        }
        
        ## Complete with NA's when it's necessary.  The weight vectors
        ## have length = 16 since there are 16 inputs, but the other
        ## vectors have length = 10 since there are only 10 digits.
        
        vectorSize <- nrow(knowledgeBase)
        
        length(x1) = vectorSize
        length(x2) = vectorSize
        length(x3) = vectorSize
        length(x4) = vectorSize
        length(x5) = vectorSize
        length(x6) = vectorSize
        length(x7) = vectorSize
        length(x8) = vectorSize
        length(x9) = vectorSize
        length(x10) = vectorSize
        length(x11) = vectorSize
        length(x12) = vectorSize
        length(x13) = vectorSize
        length(x14) = vectorSize
        length(x15) = vectorSize
        length(d0) = vectorSize
        length(d1) = vectorSize
        length(d2) = vectorSize
        length(d3) = vectorSize
        length(y0) = vectorSize
        length(y1) = vectorSize
        length(y2) = vectorSize
        length(y3) = vectorSize
        length(n) = vectorSize
        length(err0) = vectorSize
        length(err1) = vectorSize
        length(err2) = vectorSize
        length(err3) = vectorSize
        length(mse0) = vectorSize
        length(mse1) = vectorSize
        length(mse2) = vectorSize
        length(mse3) = vectorSize
        
        ## Create a csv file with all the resultant information
        
        resultantData <- cbind(x1, x2, x3, x4, x5, x6, x7, 
                               x8, x9, x10, x11, x12, x13, x14, 
                               x15, knowledgeBase, d0, d1, d2, 
                               d3, y0, y1, y2, y3, n, err0, 
                               err1, err2, err3, mse0, mse1, 
                               mse2, mse3)
        
        if (activation == "perceptron") {
                
                write.csv(resultantData, 
                          file = "DigitClassifier/Results/TrainedOutput/TrainedOutputPerceptron.csv", 
                          row.names = FALSE, na = "")
                
        } else if (activation == "adaline") {
                
                write.csv(resultantData, 
                          file = "DigitClassifier/Results/TrainedOutput/TrainedOutputAdaline.csv", 
                          row.names = FALSE, na = "")
                
        }
        
        
        
        ## Return value
        
        invisible(w)
        
}

## Set working directory at 'NeuralNetworks' folder

setwd("/Users/usuario/Desktop/NeuralNetworks/")

## Initialization of aleatory weights between -1 and 1

weight1 <- round(runif(16, min = -1, max = 1), digits = 2)
weight2 <- round(runif(16, min = -1, max = 1), digits = 2)
weight3 <- round(runif(16, min = -1, max = 1), digits = 2)
weight4 <- round(runif(16, min = -1, max = 1), digits = 2)

weight <- cbind(weight1, weight2, weight3, weight4)
weight

dataDirectory = "/Users/usuario/Desktop/NeuralNetworks/DigitClassifier/dataset/datasetDigitClassifier.csv"

learn(directory = dataDirectory, activation = "adaline", 
      weight = weight, inputName = "datasetDigits", 
      iterations = 300, alpha = 0.05, mseAccepted = 0.0001)

learn(directory = dataDirectory, activation = "perceptron", 
      weight = weight, inputName = "datasetDigits", 
      iterations = 200, alpha = 1.0, mseAccepted = 0)
