library(RSQLite)

checkValidation<- function(){
conn <- dbConnect(RSQLite::SQLite(), dbname = "DM_assignment.db")

# Import seller_data into Seller table
dbWriteTable(conn, "seller", seller_data, append = TRUE, overwrite = FALSE)

# Import customer_data into Customers table
dbWriteTable(conn, "customers", customer_data, append = TRUE, overwrite = FALSE)

# Import category_data into Categories table
dbWriteTable(conn, "categories", category_data, append = TRUE, overwrite = FALSE)

# Import product_data into Product table
dbWriteTable(conn, "product", product_data, append = TRUE, overwrite = FALSE)

# Import payment_data into Payment table
dbWriteTable(conn, "payment", payment_data, append = TRUE, overwrite = FALSE)

# Import customer_review_product_relationship into Customers_review_Products_relationship table
dbWriteTable(conn, "customers_review_products_relationship", customer_review_product_relationship, append = TRUE, overwrite = FALSE)

# Import Product_payment_relationship_data into Product_payment_relationship table
dbWriteTable(conn, "product_payment_relationship", Product_payment_relationship_data, append = TRUE, overwrite = FALSE)
#  close the database connection 
dbDisconnect(conn)
}

checkValidation()