brute_force_knapsack <- function(x, W){
  
  # Stop function if elements in x are not more than zero
  stopifnot(all(x >= 0))
  
  # Stop function if class of x is not a data frame
  stopifnot(class(x)=="data.frame") 
  
  # Stop function if W is less than or equal to zero
  stopifnot(W >= 0)
  
  #Create a matrix where items should be saved. 
  object_mat <- matrix(0, nrow=2**nrow(x), ncol=32)
  
  for (i in 1:(2**nrow(x))){
    
    #intToBits gives all possible binary combinations
    object_mat[i,]<- as.numeric(intToBits(i))
    }
  
    #Preliminary values of weights and values
    weights <- NULL
    values <- NULL
  
    weights <- apply(object_mat, 1, "%*%", 
               c(x[[1]],rep(0,ncol(object_mat)-length(x[[1]]))))
    values <- apply(object_mat, 1, "%*%",
                     c(x[[2]],rep(0,ncol(object_mat)-length(x[[2]]))))
    
    # Items with weights less than W gets TRUE
    index <- weights < W
    
    # Setting all weights less than W to zero
    weights <- index*weights
    
    # Setting all values with index = FALSE to zero
    values <- index*values
    
    # Taking the index number of item which has highest value 
    items <- which(values == max(values))[1]
  
    sum_value <- sum(object_mat[items,]*x[2])
    best_items <- which(object_mat[items,] == 1)
  
    list <- list("value"=round(sum_value), "elements"= best_items)
    return(list)
}