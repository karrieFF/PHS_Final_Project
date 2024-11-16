
#early adopter, the rest of 13.5% (DOI), adoption rate is 45% (prior knowledge)
select <- c()
#p_theory <- 0.135
ps_theory <- c(0.025, 0.135, 0.34, 0.34, 0.16)
p_prior <- 0.45
stages <- seq(2:5)

effectivenss <- sort(runif(5, min = 300, max = 1500), decreasing  = TRUE) 
non_adoption_efficacy <- -250 #finally

n_total <- nrow(final_matrix)
innovators <- result$indices
adoption_index <- c(innovators)

for (stage in stages){

  n_rest <- n_total - length(adoption_index)
  
  # Initialize total data as an empty data frame
  total_data <- data.frame(Index = integer(), Value = numeric())
  
  for (i in 1:length(adoption_index)) {
    index <- adoption_index[i]  # Select index
    select_column <- final_matrix[, index]  # Select specific column
    nonzero_indices <- which(select_column != 0, arr.ind = TRUE)  # Find indices of non-zero elements
    filtered_indices <- nonzero_indices[!nonzero_indices %in% adoption_index]   # Exclude indices that have already been selected
    nonzero_values <- select_column[filtered_indices]   # Get the corresponding values for filtered indices
    
    # Create a data frame for the current innovator's indices and values
    new_data <- data.frame(Index = filtered_indices, Value = nonzero_values)   
    
    if (nrow(new_data) > 0) { 
      total_data <- merge(total_data, new_data, by = "Index", all = TRUE)  # Proceed only if there is new data # Merge with the cumulative total data
      total_data$Value <- rowSums(total_data[, c("Value.x", "Value.y")], na.rm = TRUE)
      total_data <- total_data[, c("Index", "Value")] # Remove intermediate columns and retain the final structure
    }
  }
  
  #theoretical number of adoption at the second stage based on DOI
  num_adoption <- round(n_rest*ps_theory[stage]*p_prior)
  
  sorted_data <- total_data[order(-total_data$Value), ]
  top_indices <- sorted_data$Index[1:num_adoption]
  
  adoption_index <- c(adoption_index, top_indices)
  
  #effectiveness at the second stage
  adoption_efficacy <- effectivenss[stage]
  original_data[top_indices, 'Follow_up_PA'] <- original_data[top_indices, 'Baseline_PA'] + adoption_efficacy
  
}

length(adoption_index)

non_adoption_index <- setdiff(id, adoption_index)

#calculate the effectiveness of non_adoption
original_data[non_adoption_index, 'Follow_up_PA'] <- original_data[non_adoption_index, 'Baseline_PA'] + non_adoption_efficacy


