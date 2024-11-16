
#early adopter, the rest of 13.5% (DOI), adoption rate is 45% (prior knowledge)
select <- c()
p_theory <- 0.135
p_prior <- 0.45

# Print the results
print(result$indices)  # Top indices
print(result$values)   # Corresponding centrality values

n_total <- nrow(final_matrix)
n_innovators <- length(result$indices)
n_rest <- n_total - n_innovators

#sort the linkage based on the same metric
#if they select the similarity
innovators <- result$indices
select <- c(select, innovators)

# Initialize total data as an empty data frame
total_data <- data.frame(Index = integer(), Value = numeric())

for (i in 1:length(innovators)) {
  index <- innovators[i]  # Select index
  select_column <- final_matrix[, index]  # Select specific column
  nonzero_indices <- which(select_column != 0, arr.ind = TRUE)  # Find indices of non-zero elements
  filtered_indices <- nonzero_indices[!nonzero_indices %in% select]   # Exclude indices that have already been selected
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
num_adoption <- round(n_rest*p_theory*p_prior)

sorted_data <- total_data[order(-total_data$Value), ]
top_indices <- sorted_data$Index[1:num_adoption]

select <- c(select, top_indices)

#effectiveness adoption
top_indices
