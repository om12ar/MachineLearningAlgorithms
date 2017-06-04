getMajorityVote <- function(raw_votes, colNum){
  
  no_count <- sum(raw_votes[colNum]=="n")
  yes_count <- sum(raw_votes[colNum]=="y")
  
  ret <- ""
  if(no_count > yes_count)
    ret <- "n"
  else 
    ret <- "y"
  ret
  
}


replaceMissing <- function(votes){
  
  for(i in 1:length(names(votes))){
    
    majority <- getMajorityVote(votes,i)
    votes[i][votes[i] == "?"] <- majority
    
  }
  votes
  
}

votes <- replaceMissing(read.csv("data.csv"))

# 
# 
# decision_num <- vector()
# values <- vector()
node_counter <<- 0

getRules <- function(votes, decision_num, values){
  
  if (ncol(votes) == 1){
    return(list())
  }
  
  num_dem <- sum(votes[1] == "democrat")
  num_rep <- sum(votes[1] == "republican")
  
  total_men <- nrow(votes)
  
  
  if(num_dem == 0 || num_rep == 0){
    
    if(num_dem > num_rep){
      
      names(values) <- decision_num
      
      return(list(values))
    }
    
    return(list())
  }
  
  parent_entropy <- -((num_dem/total_men) * log(num_dem/total_men,2)) - ((num_rep/total_men) * log(num_rep/total_men,2))
  
  gains_arr <- double()
  
  for(i in 2:length(names(votes))){
    
    no_index <- which(votes[i]=="n")
    
    no_count <- length(no_index)
    
    yes_index <- which(votes[i]=="y")
    
    
    yes_count <-  length(yes_index)
    
    
    
    yes_count_republican <- 0 
    yes_count_democrat <- 0 
    
    for(j in yes_index){
      
      if(votes$party[j] == "democrat")
        yes_count_democrat <- yes_count_democrat + 1
      else{
        yes_count_republican <- yes_count_republican + 1 
      }
    }
    
    no_count_republican <- 0 
    no_count_democrat <- 0
    
    for( j in no_index){
      if(votes$party[j] == "democrat")
        no_count_democrat <- no_count_democrat + 1
      else{
        no_count_republican <- no_count_republican+1
      }
    }
    
    
    if(yes_count_democrat == 0 || yes_count_republican == 0){
      yes_e <- 0
    } 
    else{
      yes_e <- -((yes_count_democrat/length(yes_index)) * log(yes_count_democrat/length(yes_index),2)) - ((yes_count_republican/length(yes_index)) * log(yes_count_republican/length(yes_index),2))  
    }
    
    if(no_count_democrat == 0 || no_count_republican == 0 ){
      no_e <- 0
    }
    else{
      no_e <- -((no_count_democrat/length(no_index)) * log(no_count_democrat/length(no_index),2)) - ((no_count_republican/length(no_index)) * log(no_count_republican/length(no_index),2))
    } 
    
    
    gain <- parent_entropy - ((length(yes_index)/total_men) * yes_e) - ((length(no_index)/total_men) * no_e)
    
    gains_arr <- c(gains_arr, gain)
    
  }
  
  max_gain <- max(gains_arr)
  max_indices <- which(gains_arr == max_gain)
  max_index <- max_indices[1] + 1
  
  
  decision_num <- c(decision_num, names(votes[max_index]))
  
  descision_l <- list()
  
  if(length(votes[votes[max_index] == "y"]) != 0){
    
    node_counter <<- node_counter + 1    
    yes_votes <- votes[votes[max_index] == "y",]
    yes_votes[max_index] <- NULL
    
    yes_l <- getRules(yes_votes, decision_num, c(values, "y"))
    descision_l <- c(descision_l, yes_l)
  }
  
  if(length(votes[votes[max_index] == "n"]) != 0){
    
    node_counter <<- node_counter + 1
    no_votes <- votes[votes[max_index] == "n",]
    no_votes[max_index] <- NULL
    
    no_l <- getRules(no_votes, decision_num, c(values, "n"))
    descision_l <- c(descision_l, no_l)
  }
  
  return(descision_l)
}

# rules <- getRules(votes, decision_num, values)


is_democrat <- function(rules, congress_man){
  
  for(i in 1:length(rules)){
    
    if(all(rules[[i]] == congress_man[ ,names(rules[[i]])])){
      return("democrat")
    }
    
  }
  
  return("republican")
}


test <- function(rules, test_data){
  
  results <- character()
  for(i in 1:nrow(test_data)){
    results <- c(results,is_democrat(rules,test_data[i, ]))
  }
  
  results  
}

get_accuracy <- function(rules, test_data){
  
  predicted <- test(rules, test_data)
  
  results <- predicted == test_data[ ,1]
  
  accuracy <- (length(results[results == TRUE]) / length(results)) * 100
  
  accuracy
}

main <- function(){
  votes<- read.csv("data.csv")
  avg_acc <- numeric()
  avg_tree_size<-numeric()
  sample_size <- integer()
  for(i in seq(30,70,by = 10)){
    acc = numeric()
    tree_size<-numeric()
    sample_size<-c(sample_size,i)
    for(j in 1:5){
      size = nrow(votes) *(i/100)
      cols =sample(nrow(votes),size)
      train_data = votes[cols,]
      test_data =votes[-cols,]
      
      node_counter<<-0
      rules <- getRules(train_data,vector() , vector())
      tree_size <- c(tree_size,node_counter)
      acc<-c(acc,get_accuracy(rules,test_data))
    }
    avg_acc <- c(avg_acc,mean(acc))
    
    avg_tree_size <- c(avg_tree_size,mean(tree_size))
    
  }
  
  plot(sample_size,avg_acc)
  plot(sample_size,avg_tree_size)
  
  print(avg_acc)
  print(avg_tree_size)
  
}