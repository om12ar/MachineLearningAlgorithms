#Reading data
train_data <- data.frame(read.csv("TrainData.csv"))
test_data <- data.frame(read.csv("TestData.csv"))
# print(dim (test_data))

# Omitting last column and saving it to 'classess'
train_classes <- train_data[ ,9]
test_classes <- test_data[ , 9]
train_data[[9]] <- NULL
test_data[[9]] <- NULL

x <- NULL
answer <- NULL
euclid_dist_vector <- NULL

#Euclidian Distance Function
eucilid_dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))

#Performing KNN algorithm

get_diff <- function(){
  total_diff = list()

for (i in 1:nrow(test_data)){
  
   euclid_dist_vector <- vector()
   current_test_vector <- as.numeric(test_data[i,])
  
    for (j in 1:nrow(train_data)){
     current_train_vector <- as.numeric(train_data[j,])
  
     euclid_dist_vector <- c(euclid_dist_vector, eucilid_dist(current_test_vector,current_train_vector))
    
   }
   names(euclid_dist_vector) <- train_classes[1:nrow(train_data)]
   
   total_diff<- c(total_diff,list(euclid_dist_vector))
   
   
  }
  total_diff
}

get_k_diff <- function(k ,total_diff){
  answers <- vector()
 for(i in 1:length(total_diff)){
   
   k_nearest_points <- c(sort(total_diff[[i]])[1:k])
   
   x <- names(k_nearest_points)
   answer <- names((sort(table(x))))
   
   
   answers <- c(answers,answer[length(answer)])
   
 }
  answers
}

get_accuracy <- function(test_classes, total_diff,k){
  cat(" K value : " , k ,"\n")
  predicted <- get_k_diff(k, total_diff)
  
  results <- predicted == test_classes
  num_correct <- length(results[results == TRUE])
  accuracy <- ( num_correct/ length(results)) * 100
  
  for(i in 1:length(predicted)){
    
    cat("Predicted class : ",predicted[i] ," Actual class : " , toString(test_classes[i]),"\n")
  }
  cat("Number of correctly classified instances :" , num_correct ,"\n")
  cat("Total number of instances : ", length(predicted), "\n")
  cat("Accuracy : " ,accuracy,"\n")
  accuracy
}

main <- function(){
  t <- get_diff()
  acc_arr <- numeric()
  k_values <- integer()
  
  for(i in 1:9){
    k_values<-c(k_values,i)
    acc_arr<- c(acc_arr , get_accuracy(test_classes,t,i))
  }
  
  plot(k_values,acc_arr)
}
