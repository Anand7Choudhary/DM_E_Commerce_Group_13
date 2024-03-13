library(readr)
library(dplyr)
library(lubridate)
library(stringr)


#Function to Check for No Value/Null in Primary Key
validate_no_null <- function(data, attribute, log_file) {
  # Identify rows with NULL values in the specified attribute
  invalid_rows <- data[is.na(data[[attribute]]) | data[[attribute]] == "", ]
  # Open a connection to the log file in append mode
  con <- file(log_file, open = "a")
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("Rows with Null Value (%s):\n%s\n", attribute, toString(invalid_rows)),con)
    # Remove rows with NULL values in the specified attribute from the data
    data <- data[!is.na(data[[attribute]]), ]
  }
  # Close the connection to the log file
  close(con)
  return(data)
}


#Function to check for Uniqueness in keys
validate_no_duplicate <- function(data, attribute, log_file) {
  duplicated_rows <- data[duplicated(data[[attribute]]) | duplicated(data[[attribute]], fromLast = TRUE), ]
  con <- file(log_file, open = "a")
  if(nrow(duplicated_rows) > 0) {
    writeLines(sprintf("Rows with duplicated primary key (%s):\n%s\n", attribute, toString(duplicated_rows)),con)
    data <- data[!duplicated(data[[attribute]]) & !duplicated(data[[attribute]], fromLast = TRUE), ]
  }
  # Close the connection
  close(con)
  return(data)
}


#Function to check if Attribute Has All Words and Space
validate_words_and_space <- function(data, attribute, log_file) {
  invalid_rows <- data[!grepl("^[A-Za-z '-]+$", data[[attribute]]), ]
  con <- file(log_file, open = "a")
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("Rows with non-word characters in attribute %s:\n%s\n", attribute, toString(invalid_rows)),con)
    data <- data[grepl("^[A-Za-z ]+$", data[[attribute]]), ]
  }
  # Close the connection
  close(con)
  return(data)
}


#function to validate the email
validate_email_format <- function(data, email_attribute, log_file) {
  # Regular expression for basic email validation
  email_regex <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
  
  # Identify rows with invalid email format
  invalid_rows <- data %>% 
    filter(!str_detect(.[[email_attribute]], email_regex))
  
  con <- file(log_file, open = "a")
  # Log invalid rows, if any
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("Rows with invalid email format in attribute %s:\n%s\n", email_attribute, toString(invalid_rows)),con)
    # Remove invalid rows from data
    data <- data %>% 
      filter(str_detect(.[[email_attribute]], email_regex))
  }
  # Close the connection
  close(con)
  return(data)
}

#function to check the phone number
validate_phone_format <- function(data, phone_attribute, log_file) {
  invalid_rows <- data[!grepl("^\\+?[0-9 ()-]*$", data[[phone_attribute]]), ]
  con <- file(log_file, open = "a")
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("Rows with invalid phone format (%s):\n%s\n", phone_attribute, toString(invalid_rows)),con)
    data <- data[grepl("^\\+?[0-9 ()-]*$", data[[phone_attribute]]), ]
  }
  # Close the connection
  close(con)
  return(data)
}

#Valid digit format
validate_numeric_format1 <- function(data, numeric_attribute, log_file) {
  # Open a connection to the log file in append mode
  con <- file(log_file, open = "a")
  # Filter rows where the numeric attribute does not match the pattern
  pattern <- "^[0-9]+(\\.[0-9]+)?$"
  invalid_rows <- data[!grepl(pattern, data[[numeric_attribute]]), ]
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("Rows with incorrectly formatted values in attribute %s:", numeric_attribute), con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste("Row", row, ":", toString(invalid_rows[row, ])), con)
    }
  }
  # Close the connection
  close(con)
  # Remove invalid rows and return the cleaned data
  valid_data <- data[grepl(pattern, data[[numeric_attribute]]), ]
  return(valid_data)
}

validate_numeric_format2 <- function(data, numeric_attribute, log_file) {
  # Open a connection to the log file in append mode
  con <- file(log_file, open = "a")
  # Filter rows where the numeric attribute does not match the pattern
  pattern <- "^[0-9]+$"
  invalid_rows <- data[!grepl(pattern, data[[numeric_attribute]]), ]
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("Rows with incorrectly formatted values in attribute %s:", numeric_attribute), con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste("Row", row, ":", toString(invalid_rows[row, ])), con)
    }
  }
  # Close the connection
  close(con)
  # Remove invalid rows and return the cleaned data
  valid_data <- data[grepl(pattern, data[[numeric_attribute]]), ]
  return(valid_data)
}

validate_positive_values <- function(data, numeric_attribute, log_file) {
  # Open a connection to the log file in append mode
  con <- file(log_file, open = "a")
  # Filter rows where the numeric attribute is not positive
  invalid_rows <- data[data[[numeric_attribute]] < 0, ]
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("Rows with non-positive values in attribute %s:", numeric_attribute), con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste("Row", row, ":", toString(invalid_rows[row, ])), con)
    }
  }
  # Close the connection
  close(con)
  # Remove invalid rows and return the cleaned data
  valid_data <- data[data[[numeric_attribute]] >= 0, ]
  return(valid_data)
}

#check for date
validate_date_format_and_range <- function(data, date_attribute, log_file) {
  # Open a connection to the log file in append mode
  con <- file(log_file, open = "a")
  # Filter rows where dates are not in the past or are improperly formatted
  # Assuming dates are in 'yyyy-mm-dd' format and can be parsed by lubridate::ymd()
  valid_dates <- !is.na(lubridate::mdy(data[[date_attribute]]))
  future_dates <- lubridate::mdy(data[[date_attribute]]) > Sys.Date()
  invalid_rows <- data[!valid_dates | future_dates, ]
  
  if(nrow(invalid_rows) > 0) {
    # Log details of the invalid rows to the file connection
    message <- sprintf("Rows with invalid or future dates in attribute %s:", date_attribute)
    writeLines(message, con)
    # Serialize invalid rows details for logging
    invalid_details <- apply(invalid_rows, 1, function(row) paste(colnames(data), "=", row, collapse = ", "))
    writeLines(invalid_details, con)
    # Remove invalid rows from data
    data <- data[valid_dates & !future_dates, ]
  }
  # Close the file connection
  close(con)
  return(data)
}

#foreign key constraint
library(readr)

validate_fk_availability <- function(source_csv, fk_attribute, referenced_csv, referenced_pk, log_file) {
  # Load the CSV files
  source_data <- read_csv(source_csv)
  referenced_data <- read_csv(referenced_csv)
  
  # Identify rows with foreign keys not present in the referenced table
  invalid_fks <- source_data[!source_data[[fk_attribute]] %in% referenced_data[[referenced_pk]], ]
  
  # Open a connection to the log file in append mode
  con <- file(log_file, open = "a")
  
  if(nrow(invalid_fks) > 0) {
    # Log details of the invalid rows to the file connection
    message <- sprintf("Rows with unavailable foreign keys in attribute %s:", fk_attribute)
    writeLines(message, con)
    # Serialize invalid rows details for logging
    invalid_details <- apply(invalid_fks, 1, function(row) paste(colnames(source_data), "=", row, collapse = ", "))
    writeLines(invalid_details, con)
    # Remove invalid rows from source data
    valid_data <- source_data[source_data[[fk_attribute]] %in% referenced_data[[referenced_pk]], ]
  } else {
    writeLines(sprintf("All foreign keys in attribute %s are available.", fk_attribute), con)
    valid_data <- source_data
  }
  # Close the file connection
  close(con)
  # Optionally, write the cleaned data back to CSV
  write_csv(valid_data, source_csv)
  return(valid_data)
}






# Function to clear a log file
clear_log_file <- function(log_file) {
  file.create(log_file)
  cat("", file = log_file)
}

seller_log_file <- "validation_logs/seller_data_log.txt"
product_log_file<- "validation_logs/product_data_log.txt"
customer_log_file<- "validation_logs/customer_data_log.txt"


checkValidation<- function(seller_data, customer_data, category_data, product_data, payment_data, customer_review_product_relationship, Product_payment_relationship_data){
  
  # Clear each log file
  clear_log_file(seller_log_file)
  clear_log_file(product_log_file)
  clear_log_file(customer_log_file)
  
 #Seller validation
    #seller_id
  seller_data<-validate_no_null(seller_data,"seller_id",seller_log_file)
  seller_data<-validate_no_duplicate(seller_data,"seller_id",seller_log_file)
    #seller_name
  seller_data<-validate_no_null(seller_data,"seller_name",seller_log_file)
    #seller_email
  seller_data<-validate_no_duplicate(seller_data,"seller_email",seller_log_file)
  seller_data<-validate_email_format(seller_data,"seller_email",seller_log_file)
    #seller_phone_no
  seller_data<-validate_phone_format(seller_data,"seller_phone_no",seller_log_file)
  
#Product validation
  #product_id
  product_data<-validate_no_null(product_data,"product_id",product_log_file)
  product_data<-validate_no_duplicate(product_data,"product_id",product_log_file)
  #category_id
  product_data<-validate_no_null(product_data,"category_id",product_log_file)
  #seller_id
  product_data<-validate_no_null(product_data,"seller_id",product_log_file)
  #product_name
  product_data<-validate_no_null(product_data,"product_name",product_log_file)
  #product_price
  product_data<-validate_no_null(product_data,"product_price",product_log_file)
  product_data<-validate_numeric_format1(product_data,"product_price",product_log_file)
  product_data<-validate_positive_values(product_data,"product_price",product_log_file)
  #quantity_available
  product_data<-validate_no_null(product_data,"quantity_available",product_log_file)
  product_data<-validate_numeric_format2(product_data,"quantity_available",product_log_file)
  product_data<-validate_positive_values(product_data,"quantity_available",product_log_file)
  
#Customer Validation
  #customer_id
  customer_data<-validate_no_null(customer_data,"customer_id",customer_log_file)
  customer_data<-validate_no_duplicate(customer_data,"customer_id",customer_log_file)
  #cust_password
  customer_data<-validate_no_null(customer_data,"cust_password",customer_log_file)
  #email
  customer_data<-validate_no_duplicate(customer_data,"email",customer_log_file)
  customer_data<-validate_email_format(customer_data,"email",customer_log_file)
  #first_name
  customer_data<-validate_no_null(customer_data,"first_name",customer_log_file)
  customer_data<-validate_words_and_space(customer_data,"first_name",customer_log_file)
  #last_name
  customer_data<-validate_no_null(customer_data,"last_name",customer_log_file)
  customer_data<-validate_words_and_space(customer_data,"last_name",customer_log_file)
  #date_of_birth
  customer_data<-validate_date_format_and_range(customer_data,"date_of_birth",customer_log_file)
  #phone_number
  customer_data<-validate_phone_format(customer_data,"phone_number",customer_log_file)
  print("hey")
}



