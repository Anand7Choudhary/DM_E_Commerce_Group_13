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
  data_filter<-data
  
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("\nError: Rows with Null Value (%s):", attribute), con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste( row, ":", toString(invalid_rows[row, ])), con)
    }
    
    # Directly modify the data frame in-place to remove invalid rows
    data_filtered <- data[!is.na(data[[attribute]]) & data[[attribute]] != "", ]  # Note the comma
    data <- subset(data, !is.na(data[[attribute]]) & data[[attribute]] != "")
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
    writeLines(sprintf("\nError: Rows with duplicated attribute (%s):", attribute),con)
    for (row in 1:nrow(duplicated_rows)) {
      writeLines(paste( row, ":", toString(duplicated_rows[row, ])), con)
    }
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
    writeLines(sprintf("\nError: Rows with non-word characters in attribute (%s):", attribute),con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste( row, ":", toString(invalid_rows[row, ])), con)
    }
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
    writeLines(sprintf("\nError: Rows with invalid email format in attribute (%s):", email_attribute),con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste( row, ":", toString(invalid_rows[row, ])), con)
    }
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
    writeLines(sprintf("\nError: Rows with invalid phone format (%s):", phone_attribute),con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste( row, ":", toString(invalid_rows[row, ])), con)
    }
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
    writeLines(sprintf("\nError: Rows with incorrectly formatted values in attribute (%s):", numeric_attribute),con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste( row, ":", toString(invalid_rows[row, ])), con)
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
    writeLines(sprintf("\nError: Rows with incorrectly formatted values in attribute (%s):", numeric_attribute),con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste( row, ":", toString(invalid_rows[row, ])), con)
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
    writeLines(sprintf("\nError: Rows with incorrectly formatted values in attribute (%s):", numeric_attribute),con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste( row, ":", toString(invalid_rows[row, ])), con)
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
  # Attempt to parse dates in the 'mm/dd/yyyy' format
  parsed_dates <- mdy(data[[date_attribute]], tz = "UTC")
  
  # Find indices of rows where dates are NA (invalid) or in the future
  invalid_or_future_indices <- which(is.na(parsed_dates) | parsed_dates > Sys.Date())
  
  # Check if there are any rows to log
  if (length(invalid_or_future_indices) > 0) {
    # Open a connection to the log file in append mode
    con <- file(log_file, open = "a")
    # Log a header for this batch of errors
    writeLines(sprintf("\nError: Rows with invalid or future dates in attribute (%s):", date_attribute), con)
    # Iterate over each invalid or future date entry
    for (index in invalid_or_future_indices) {
      # Log details of the invalid row
      row_details <- paste(index, ":", paste(data[index, ], collapse=", "), sep="")
      writeLines(row_details, con)
    }
    # Close the file connection
    close(con)
    # Remove rows with invalid or future dates from the dataset
    data <- data[-invalid_or_future_indices, ]
  }
  return(data)
}




#function to check for the right rating range
validate_rating_logic <- function(data, rating_attribute, log_file) {
  # Open a connection to the log file in append mode
  con <- file(log_file, open = "a")
  invalid_rows <- data[data[[rating_attribute]] < 1 | data[[rating_attribute]] > 5, ]
  if(nrow(invalid_rows) > 0) {
    writeLines(sprintf("\nError: Rows with out-of-range ratings in attribute (%s):", rating_attribute), con)
    for (row in 1:nrow(invalid_rows)) {
      writeLines(paste( row, ":", toString(invalid_rows[row, ])), con)
    }
    data <- data[data[[rating_attribute]] >= 1 & data[[rating_attribute]] <= 5, ]
  }
  close(con)
  return(data)
}



#foreign key constraint
library(dplyr)

validate_fk_availability <- function(source_data, fk_attribute, referenced_data, referenced_pk, log_file) {
  # Identify the rows with foreign keys not present in the referenced table
  invalid_fks <- source_data[!source_data[[fk_attribute]] %in% referenced_data[[referenced_pk]], ]
  
  # Open a connection to the log file in append mode
  con <- file(log_file, open = "a")
  
  if(nrow(invalid_fks) > 0) {
    # Log details of the invalid rows to the file connection
    message <- sprintf("\nError: Rows with unavailable foreign keys in attribute (%s):", fk_attribute)
    writeLines(message, con)
    # Serialize the data of invalid rows for logging
    invalid_rows_data <- apply(invalid_fks, 1, function(row) paste(names(invalid_fks), row, sep=": ", collapse = ", "))
    writeLines(invalid_rows_data, con)
    
    # Remove invalid rows from source data
    source_data <- source_data[!source_data[[fk_attribute]] %in% invalid_fks[[fk_attribute]], ]
  }
  # Close the file connection
  close(con)
  return(source_data)
}

# Function to append a timestamp at the start of each new log entry
append_timestamp_to_log <- function(log_file) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste("\n==================================================================\n------- New Validation Execution at [",timestamp,"]-------\n")
  cat(message, file = log_file, append = TRUE)
}



seller_log_file <- "validation_logs/seller_data_log.txt"
product_log_file<- "validation_logs/product_data_log.txt"
customer_log_file<- "validation_logs/customer_data_log.txt"
category_log_file<- "validation_logs/category_data_log.txt"
payment_log_file<- "validation_logs/payment_data_log.txt"
prod_payment_log_file<- "validation_logs/prod_payment_data_log.txt"
cust_rev_prod_log_file<- "validation_logs/cust_rev_prod_data_log.txt"

seller_data<-


checkValidation<- function(seller_data, customer_data, category_data, product_data, payment_data, customer_review_product_relationship, Product_payment_relationship_data){
  
  # time stamp input
  append_timestamp_to_log(seller_log_file)
  append_timestamp_to_log(product_log_file)
  append_timestamp_to_log(customer_log_file)
  append_timestamp_to_log(category_log_file)
  append_timestamp_to_log(payment_log_file)
  
 #Seller validation
    #seller_id
  seller_data_new<<-validate_no_null(seller_data,"seller_id",seller_log_file)
  seller_data_new<<-validate_no_duplicate(seller_data_new,"seller_id",seller_log_file)
    #seller_name
  seller_data_new<<-validate_no_null(seller_data_new,"seller_name",seller_log_file)
    #seller_email
  seller_data_new<<-validate_no_duplicate(seller_data_new,"seller_email",seller_log_file)
  seller_data_new<<-validate_email_format(seller_data_new,"seller_email",seller_log_file)
    #seller_phone_no
  seller_data_new<<-validate_phone_format(seller_data_new,"seller_phone_no",seller_log_file)
  
#Product validation
  #product_id
  product_data_new<<-validate_no_null(product_data,"product_id",product_log_file)
  product_data_new<<-validate_no_duplicate(product_data_new,"product_id",product_log_file)
  #category_id
  product_data_new<<-validate_no_null(product_data_new,"category_id",product_log_file)
  #seller_id
  product_data_new<<-validate_no_null(product_data_new,"seller_id",product_log_file)
  #product_name
  product_data_new<<-validate_no_null(product_data_new,"product_name",product_log_file)
  #product_price
  product_data_new<<-validate_no_null(product_data_new,"product_price",product_log_file)
  product_data_new<<-validate_numeric_format1(product_data_new,"product_price",product_log_file)
  product_data_new<<-validate_positive_values(product_data_new,"product_price",product_log_file)
  #quantity_available
  product_data_new<<-validate_no_null(product_data_new,"quantity_available",product_log_file)
  product_data_new<<-validate_numeric_format2(product_data_new,"quantity_available",product_log_file)
  product_data_new<<-validate_positive_values(product_data_new,"quantity_available",product_log_file)
  
#Customer Validation
  #customer_id
  customer_data_new<<-validate_no_null(customer_data,"customer_id",customer_log_file)
  customer_data_new<<-validate_no_duplicate(customer_data_new,"customer_id",customer_log_file)
  #cust_password
  customer_data_new<<-validate_no_null(customer_data_new,"cust_password",customer_log_file)
  #email
  customer_data_new<<-validate_no_duplicate(customer_data_new,"email",customer_log_file)
  customer_data_new<<-validate_email_format(customer_data_new,"email",customer_log_file)
  #first_name
  customer_data_new<<-validate_no_null(customer_data_new,"first_name",customer_log_file)
  customer_data_new<<-validate_words_and_space(customer_data_new,"first_name",customer_log_file)
  #last_name
  customer_data_new<<-validate_no_null(customer_data_new,"last_name",customer_log_file)
  customer_data_new<<-validate_words_and_space(customer_data_new,"last_name",customer_log_file)
  #date_of_birth
  customer_data_new<<-validate_date_format_and_range(customer_data_new,"date_of_birth",customer_log_file)
  #phone_number
  customer_data_new<<-validate_phone_format(customer_data_new,"phone_number",customer_log_file)
  
#Categories
  #category_id
  category_data_new<<-validate_no_null(category_data,"category_id",category_log_file)
  category_data_new<<-validate_no_duplicate(category_data_new,"category_id",category_log_file)
  #category_name
  category_data_new<<-validate_no_null(category_data_new,"category_name",category_log_file)
  
#Payment
  #payment_id
  payment_data_new<<-validate_no_null(payment_data,"payment_id",payment_log_file)
  payment_data_new<<-validate_no_duplicate(payment_data_new,"payment_id",payment_log_file)
  #customer_id
  payment_data_new<<-validate_no_null(payment_data_new,"customer_id",payment_log_file)
  #date_time
  payment_data_new<<-validate_date_format_and_range(payment_data_new,"date_time",payment_log_file)
  
  #Product_payment_relationship
  #product_id
  Product_payment_relationship_data_new<<-validate_no_null(Product_payment_relationship_data,"product_id",prod_payment_log_file)
  #payment_id
  Product_payment_relationship_data_new<<-validate_no_null(Product_payment_relationship_data_new,"payment_id",prod_payment_log_file)
  #quantity
  Product_payment_relationship_data_new<<-validate_no_null(Product_payment_relationship_data_new,"quantity",prod_payment_log_file)
  Product_payment_relationship_data_new<<-validate_numeric_format2(Product_payment_relationship_data_new,"quantity",prod_payment_log_file)
  Product_payment_relationship_data_new<<-validate_positive_values(Product_payment_relationship_data_new,"quantity",prod_payment_log_file)
  
  #Customers_review_Products_relationship
  #customer_id
  customer_review_product_relationship_new<<-validate_no_null(customer_review_product_relationship,"customer_id",cust_rev_prod_log_file)
  #product_id
  customer_review_product_relationship_new<<-validate_no_null(customer_review_product_relationship_new,"product_id",cust_rev_prod_log_file)
  #rating
  customer_review_product_relationship_new<<-validate_rating_logic(customer_review_product_relationship_new,"rating",cust_rev_prod_log_file)
  #date_time
  customer_review_product_relationship_new<<-validate_date_format_and_range(customer_review_product_relationship_new,"date_time",cust_rev_prod_log_file)
  
  #foreign key deletion
  #On customer deletion
    payment_data_new<<-validate_fk_availability(payment_data_new,"customer_id",customer_data_new,"customer_id",payment_log_file)
    customer_review_product_relationship_new<<-validate_fk_availability(customer_review_product_relationship_new,"customer_id",customer_data_new,"customer_id",cust_rev_prod_log_file)
  #On payment deletion
    Product_payment_relationship_data_new<<-validate_fk_availability(Product_payment_relationship_data_new,"payment_id",payment_data_new,"payment_id",prod_payment_log_file)
  #On category deletion
    product_data_new<<-validate_fk_availability(product_data_new,"category_id",category_data_new,"category_id",product_log_file)
  #On seller deletion
    product_data_new<<-validate_fk_availability(product_data_new,"seller_id",seller_data_new,"seller_id",product_log_file)
  #On product deletion
    Product_payment_relationship_data_new<<-validate_fk_availability(Product_payment_relationship_data_new,"product_id",product_data_new,"product_id",prod_payment_log_file)
    customer_review_product_relationship_new<<-validate_fk_availability(customer_review_product_relationship_new,"product_id",product_data_new,"product_id",cust_rev_prod_log_file)
  print("hey")
}



