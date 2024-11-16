
library(doipkg)
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

# Initialize the similarity matrix
similarity_matrix <- matrix(0, nrow = num_agents, ncol = num_agents)

# Loop through each pair of individuals to populate the similarity matrix
for (i1 in 1:num_agents) {
  for (i2 in 1:num_agents) {
    # Extract individual data as numeric vectors, excluding the ID column
    individual1 <- as.numeric(original_data[i1, -1])  
    individual2 <- as.numeric(original_data[i2, -1])
    
    # Calculate similarity using the specified method (change method as needed)
    similarity_matrix[i1, i2] <- doipkg::calculate_similarity(individual1, individual2, method = "Euclidean") #this used the doipkg
  }
}

#standardize the matrix
min_val <- min(similarity_matrix)
max_val <- max(similarity_matrix)

# Standardize the similarity matrix to range [0, 1]
standardized_similarity_matrix <- (similarity_matrix - min_val) / (max_val - min_val)

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
