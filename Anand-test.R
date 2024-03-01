library(DBI)
library(charlatan)
library(stringi)
library(RSQLite)
library(dplyr)

# Function to generate unique IDs with a specific word and fixed length
generate_unique_ids_with_prefix <- function(n, word,lenOfId) {
  unique_ids <- character(n)
  # Loop through the number of IDs we need to generate
  for (i in 1:n) {
    is_unique <- FALSE  # Initialize a flag to check uniqueness
    while (!is_unique) {
      # Generate a 6-character random alphanumeric string
      random_string <- stri_rand_strings(1, lenOfId-4, pattern = "[a-zA-Z0-9]")
      full_id <- paste0(word, random_string)
      # Check if this ID is already in our list of unique IDs
      if (!full_id %in% unique_ids) {
        unique_ids[i] <- full_id  # If unique, add it to the list
        is_unique <- TRUE  # Set the flag to true to exit the loop
      }
    }
  }
  return(unique_ids)
}


# Assuming n_sellers, n_customers, n_categories, n_products are predefined
n_sellers <- 300
n_customers <- 1000
n_categories <- 25
n_products <- 1000
n_carts <- 300
n_cart_items <- 2000
n_payments <- 300
n_reviews <- 1000


# Example usage for 5 Seller IDs
seller_ids <- generate_unique_ids_with_prefix(n_sellers, "selr_",10)
print(seller_ids)

customer_ids <- generate_unique_ids_with_prefix(n_customers, "cust_",10)
print(customer_ids)

category_ids <- generate_unique_ids_with_prefix(n_categories, "catg_",10)
print(category_ids)

product_ids <- generate_unique_ids_with_prefix(n_products, "prod_",10)
print(product_ids)

cart_ids <- generate_unique_ids_with_prefix(n_carts, "cart_",10)
print(cart_ids)

cart_item_ids <- generate_unique_ids_with_prefix(n_cart_items, "crtI_",10)
print(cart_item_ids)

payment_ids <- generate_unique_ids_with_prefix(n_payments, "pytm_",10)
print(payment_ids)

review_ids <- generate_unique_ids_with_prefix(n_reviews, "revw_",10)
print(review_ids)









# Seller Info
# Initialize an empty data frame for sellers data
sellers_data <- data.frame(
  Seller_id = integer(n_sellers),
  Seller_name = character(n_sellers),
  Seller_email = character(n_sellers),
  Street_name = character(n_sellers),
  City = character(n_sellers),
  State = character(n_sellers),
  Country = character(n_sellers),
  Phone = character(n_sellers),
  stringsAsFactors = FALSE  # Avoid factors to ease data handling
)

# Generate synthetic data for the Seller table using for loops
set.seed(123) # For reproducibility
unique_emails <- character(0)  # Track unique emails

for (i in 1:n_sellers) {
  sellers_data$Seller_id[i] <- seller_ids[i]
  sellers_data$Seller_name[i] <- ch_name()
  # Ensure unique email
  sellers_data$Seller_email<-"xyz"
}

# Convert the data frame to a string format suitable for SQL import
sellers_data_strings <- apply(sellers_data, 1, function(row) {
  sprintf("('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s')",
          row['Seller_id'], row['Seller_name'], row['Seller_email'],
          row['Street_name'], row['City'], row['State'],
          row['Country'], row['Phone'])
})

# Combine into one big string
sellers_data_sql_string <- paste(sellers_data_strings, collapse = ",\n")

# Preview the string
cat(substr(sellers_data_sql_string, 1, 1000))  # Preview the first 1000 characters
















