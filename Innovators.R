
library(doipkg)
get_top_innovators <- function(final_matrix, p_innovators, method) {
  # Calculate the number of innovators to select
  top_n <- round(nrow(final_matrix) * p_innovators)
  
  # In-degree centrality (number of non-zero connections for each column)
  nan_0_counts <- colSums(final_matrix != 0)
  
  # Betweenness centrality
  total_close <- c()  # Vector to store betweenness centrality
  for (i0 in 1:nrow(final_matrix)) {
    num_close <- 0
    col_first <- final_matrix[, i0]  # Extract column for the current agent
    non_0_index <- which(col_first != 0)  # Get indices of non-zero values in the column
    
    # Loop over each pair of non-zero indices
    for (i1 in non_0_index) {
      except_i <- setdiff(non_0_index, i1)  # Exclude the current index i1
      for (i2 in except_i) {
        value <- final_matrix[i1, i2]  # Get value at (i1, i2) in the matrix
        if (value != 0) {  # Increment if non-zero
          num_close <- num_close + 1
        }
      }
    }
    total_close <- c(total_close, num_close)  # Append the result
  }
  
  # Closeness centrality (sum of similarity values for each column)
  sum_distance <- colSums(final_matrix)
  
  # Determine top innovators based on the selected method
  if (method == "in-degree") {
    top_indices <- order(-nan_0_counts)[1:top_n]
    top_values <- nan_0_counts[top_indices]
  } else if (method == "betweeness") {
    top_indices <- order(-total_close)[1:top_n]
    top_values <- total_close[top_indices]
  } else if (method == "closeness") {
    top_indices <- order(-sum_distance)[1:top_n]
    top_values <- sum_distance[top_indices]
  } else {
    stop("Invalid method. Choose from 'in-degree', 'betweeness', or 'closeness'.")
  }
  
  # Return the results as a list
  return(list(
    indices = top_indices,
    values = top_values,
    method = method
  ))
}


# Function to calculate top innovators based on centrality
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16)

#the reason why we select 300 - 1500 is based on the theory, the increase for the total 60 days is 1500,
#if we split them into 5 parts, that should be 12days, 24days, 36days, 48days, 60days.
#the icnrease is relatively linear, the step increase for each time split should 300, 600, 900, 1200
#people at the final stage may adopt the chatbot the shorted time
#thus the increase of the steps at the final stage is 300 steps
#if we hypothesis is not enirely linear

effectivenss <- c(300, 600, 900, 1200, 1500)

# Replace final_matrix with your matrix
p_innovators <- ps_theory[1]  # Proportion of innovators
method <- "closeness"  # Method: "in-degree", "betweeness", or "closeness"
result <- get_top_innovators(final_matrix, p_innovators, method)


doipkg::get_top_innovators(final_matrix, p_innovators, method)


# Print the results
innovators_indice <- result$indices # Top indices
innovators_values <- result$values   # Corresponding centrality values

#effectiveness, adoption increase 1000 steps, non-adoption increase 250 steps

adoption_efficacy <- effectivenss[1]
original_data[i,'Follow_up_PA'] <- NaN
  
original_data[innovators_indice, 'Follow_up_PA'] <- original_data[innovators_indice, 'Baseline_PA'] + adoption_efficacy



