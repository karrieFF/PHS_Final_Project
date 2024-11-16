

simulate_adoption <- function(final_matrix, result, stages, ps_theory, p_prior, effectiveness, non_adoption_efficacy, original_data) {
  # Initialize variables
  n_total <- nrow(final_matrix)
  innovators <- result$indices
  adoption_index <- c(innovators)
  
  for (stage in stages) {
    n_rest <- n_total - length(adoption_index)
    
    # Initialize total data as an empty data frame
    total_data <- data.frame(Index = integer(), Value = numeric())
    
    for (i in seq_along(adoption_index)) {
      index <- adoption_index[i]  # Select index
      select_column <- final_matrix[, index]  # Select specific column
      nonzero_indices <- which(select_column != 0)  # Find indices of non-zero elements
      filtered_indices <- nonzero_indices[!nonzero_indices %in% adoption_index]  # Exclude already adopted
      nonzero_values <- select_column[filtered_indices]  # Get the corresponding values
      
      # Create a data frame for the current innovator's indices and values
      new_data <- data.frame(Index = filtered_indices, Value = nonzero_values)
      
      if (nrow(new_data) > 0) {
        total_data <- merge(total_data, new_data, by = "Index", all = TRUE)  # Merge with cumulative data
        total_data$Value <- rowSums(total_data[, c("Value.x", "Value.y")], na.rm = TRUE)
        total_data <- total_data[, c("Index", "Value")]  # Retain final structure
      }
    }
    
    # Theoretical number of adoption at this stage
    num_adoption <- round(n_rest * ps_theory[stage] * p_prior)
    
    # Sort and select top indices for adoption
    sorted_data <- total_data[order(-total_data$Value), ]
    top_indices <- head(sorted_data$Index, num_adoption)
    
    # Update adoption index
    adoption_index <- c(adoption_index, top_indices)
    
    # Update effectiveness for adopters
    adoption_efficacy <- effectiveness[stage]
    original_data[top_indices, 'Follow_up_PA'] <- original_data[top_indices, 'Baseline_PA'] + adoption_efficacy
  }
  
  # Identify non-adopters
  non_adoption_index <- setdiff(1:n_total, adoption_index)
  
  # Update effectiveness for non-adopters
  original_data[non_adoption_index, 'Follow_up_PA'] <- original_data[non_adoption_index, 'Baseline_PA'] + non_adoption_efficacy
  
  # Return results
  return(list(
    adoption_index = adoption_index,
    non_adoption_index = non_adoption_index,
    updated_data = original_data
  ))
}


#early adopter, the rest of 13.5% (DOI), adoption rate is 45% (prior knowledge)
select <- c()

#p_theory <- 0.135
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16)
p_prior <- 0.45
stages <- seq(2:5)

effectivenss <- c(300, 600, 900, 1200, 1500)
non_adoption_efficacy <- -250 #finally

result <- simulate_adoption(
  final_matrix = final_matrix,
  result = result,
  stages = stages,
  ps_theory = ps_theory,
  p_prior = p_prior,
  effectiveness = effectivenss,
  non_adoption_efficacy = non_adoption_efficacy,
  original_data = original_data
)

# Access outputs
adoption_index <- result$adoption_index
non_adoption_index <- result$non_adoption_index
updated_data <- result$updated_data

