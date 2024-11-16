num_agents <- 100

# Generate data
id <- seq(1, num_agents)
gender <- sample(c(0, 1), num_agents, replace = TRUE)  # 0 represents female; 1 represents male
age <- runif(num_agents, min = 18, max = 30)
socio_economic_status <- sample(c(0, 1, 2), num_agents, replace = TRUE)  # 0 = low, 1 = middle, 2 = high
baseline_PA <- runif(num_agents, min = 1000, max = 15000)  # 0 = did not reach PA goal, 1 = reached PA goal

# Create the data frame
original_data <- data.frame(
  ID = id,
  Gender = gender,
  Age = age,
  SES = socio_economic_status,
  Baseline_PA = baseline_PA
)

#Calculate the similarities between two agents
#method 1 Cosine Similarity for numerical demographic # c(25, 0, 2, 1)  # Example: age=25, female, high SES, baseline_PA achieved
#method 2.  Hamming Distance for categorical data #individual1 <- c(0, 2, 1)  # Female, high SES, achieved PA goal
#method 3.  Euclidean Distance for mixed data #individual1 <- c(25, 0, 2)  # Age=25, female, high SES
#method 4.Jaccard Similarity for Binary data #individual1 <- c(1, 0, 1, 1)  # Binary vector for individual 1
# Initialize the similarity matrix
similarity_matrix <- matrix(0, nrow = num_agents, ncol = num_agents)

# Function to calculate similarity between two individuals
calculate_similarity <- function(individual1, individual2, method = "Cosine") {
  if (method == "Cosine") {
    # Calculate cosine similarity
    cosine_similarity <- sum(individual1 * individual2) / 
      (sqrt(sum(individual1^2)) * sqrt(sum(individual2^2)))
    return(cosine_similarity)
    
  } else if (method == "Hamming") {
    # Calculate Hamming distance and convert to similarity
    hamming_distance <- sum(individual1 != individual2)
    return(hamming_distance) 
    
  } else if (method == "Euclidean") {
    # Calculate Euclidean distance and convert to similarity
    euclidean_distance <- sqrt(sum((individual1 - individual2)^2))
    return(euclidean_distance)  # Convert to similarity in range [0,1]
    
  } else if (method == "Jaccard") {
    # Calculate Jaccard similarity
    jaccard_similarity <- sum(individual1 & individual2) / sum(individual1 | individual2)
    return(jaccard_similarity)
    
  } else {
    stop("Invalid method. Choose 'Cosine', 'Hamming', 'Euclidean', or 'Jaccard'.")
  }
}

# Loop through each pair of individuals to populate the similarity matrix
for (i1 in 1:num_agents) {
  for (i2 in 1:num_agents) {
    # Extract individual data as numeric vectors, excluding the ID column
    individual1 <- as.numeric(original_data[i1, -1])  
    individual2 <- as.numeric(original_data[i2, -1])
    
    # Calculate similarity using the specified method (change method as needed)
    similarity_matrix[i1, i2] <- calculate_similarity(individual1, individual2, method = "Euclidean")  
  }
}

#standardize the matrix
min_val <- min(similarity_matrix)
max_val <- max(similarity_matrix)

# Standardize the similarity matrix to range [0, 1]
standardized_similarity_matrix <- (similarity_matrix - min_val) / (max_val - min_val)

#Function 1. Prepare matrix for analysis

#original matrix
agent1 <- sample(1:num_agents, 100, replace = TRUE)
agent2 <- sample(1:num_agents, 100, replace = TRUE)

# Create an adjacency matrix with random connections (0 or 1)
# 0.9 probability for no connection, 0.1 for connection
adj_matrix <- matrix(sample(0:1, num_agents^2, replace = TRUE, prob = c(0.9, 0.1)), 
                     nrow = num_agents, 
                     ncol = num_agents)

adj_matrix[lower.tri(adj_matrix)] <- t(adj_matrix)[lower.tri(adj_matrix)] #Make symmetric
diag(adj_matrix) <- 0 # Remove self-loops

#assign strength to the matrix
final_matrix <- standardized_similarity_matrix * adj_matrix
final_matrix
