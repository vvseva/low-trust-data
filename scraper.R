# Load the httr2 library
library(httr2)

# --- Configuration ---
target_url <- "https://williamosmanisapredator.com/"
check_interval_seconds <- 300 # 5 minutes (5 * 60 seconds)
user_agent_string <- c("hello Michal i am vvseva Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")

# --- Main Loop ---
while (TRUE) {
  tryCatch({
    # 1. Generate a timestamp for the filename
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
    file_name <- paste0(timestamp, "_example.com.html")
    
    # 2. Create and configure the request
    req <- request(target_url) |>
      req_user_agent(user_agent_string)
    
    # 3. Perform the request
    cat(paste("Fetching", target_url, "at", Sys.time(), "\n"))
    resp <- req_perform(req)
    
    # 4. Check if the request was successful (HTTP status 200-299)
    if (resp_is_success(resp)) {
      # 5. Save the response body to a file
      writeBin(resp_body_raw(resp), file_name)
      cat(paste("Successfully saved content to:", file_name, "\n"))
    } else {
      cat(paste("Failed to fetch website. Status code:", resp_status(resp), "\n"))
    }
    
  }, error = function(e) {
    cat("An error occurred:\n")
    print(e$message)
  })

  cat(paste("Waiting for", check_interval_seconds, "seconds...\n\n"))
  Sys.sleep(check_interval_seconds)
}

