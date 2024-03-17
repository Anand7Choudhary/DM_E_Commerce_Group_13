library(RSQLite)

databaseUpdation<- function(seller_data_new,payment_data_new,new_product_data,new_customer_data,new_category_data,Product_payment_relationship_data_new,customer_review_product_relationship_new){
  #seller_data_new,payment_data_new,new_product_data,new_customer_data,new_category_data,Product_payment_relationship_data_new,customer_review_product_relationship_new
  print("Pikachu")
  seller_data<-seller_data_new
  payment_data<-payment_data_new
  product_data<-new_product_data
  customer_data<-new_customer_data
  category_data<-new_category_data
  Product_payment_relationship_data<-Product_payment_relationship_data_new
  customers_review_products_relationship<-customer_review_product_relationship_new
  
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
dbWriteTable(conn, "customers_review_products_relationship", customers_review_products_relationship, append = TRUE, overwrite = FALSE)

# Import Product_payment_relationship_data into Product_payment_relationship table
dbWriteTable(conn, "product_payment_relationship", Product_payment_relationship_data, append = TRUE, overwrite = FALSE)
#  close the database connection 
dbDisconnect(conn)
}