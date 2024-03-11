# Define the path to the README file
readme_path <- "../README.md"

# Read the current content of the README
readme_content <- readLines(readme_path, warn = FALSE)

# Get the current timestamp
timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

# Define the message to append, including the name of the script
script_name <- "data_ana" # Replace with your actual script name
update_message <- paste0("- ", script_name, " last run at ", timestamp)

# Append the update message to the README content
readme_content <- c(readme_content, update_message)

# Write the updated content back to the README
writeLines(readme_content, readme_path)