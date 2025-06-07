library(readxl)
library(RSQLite)
library(DBI)
library(dplyr)
library(tidyverse)

#### Getting Study Data

excel_data <- read_excel("C:/Users/Owner/OneDrive/Documents/Excels/adam_study.xlsx") |>
  select(1:10) |>
  select(-days, -hours) |>
  filter(minutes > 0) |>
  mutate(
    date = as.Date(date)
  )
# head(excel_data)  # Check the data


con <- dbConnect(RSQLite::SQLite(), "C:/Users/Owner/OneDrive/Documents/ADAM TRACKER/pushup_data_file.sqlite")

# Loop in data
for (i in 1:nrow(excel_data)) {
  dbExecute(con,
            "INSERT INTO study (date, minutes, notes, anki, github,linkedin, program, status) VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
            params = list(excel_data$date[i], excel_data$minutes[i], excel_data$notes[i],
                            excel_data$anki[i], excel_data$Github[i], excel_data$LinkedIn[i],
                          excel_data$program[i], excel_data$status[i]))
}






### CREATE person TABLE
# CREATE TABLE person1 ( person_id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, relationship TEXT, date_last_connected REAL, contact_after_x_days INTEGER, reminder REAL, phone_number TEXT, email TEXT );


####COPYING DATA TO CSV FOR BACKUP
# sqlite_file <- "C:/Users/Owner/OneDrive/Documents/ADAM TRACKER/pushup_data_file.sqlite"
# csv_file <- "C:/temp/backup.csv"   



# Connect to the SQLite database
# con <- dbConnect(RSQLite::SQLite(), sqlite_file)

# # Query the data from the 'pushups' table
# pushups_data <- dbReadTable(con, "pushups")

# # View the data (optional)
# print(pushups_data)

# # Save the data to a CSV file
# write_csv(pushups_data, csv_file)

# Disconnect from the database
# dbDisconnect(con)

# # Confirm the file is saved
# cat("CSV file saved at:", csv_file)




#### TRANSER DATA CSV TO SQLITE TABLE
# Define file paths and database connection
# csv_file_path <- "C:/temp/backup.csv"  # Replace with your CSV file path
# sqlite_db_path <- "C:/Users/Owner/OneDrive/Documents/ADAM TRACKER/pushup_data_file.sqlite"  # Replace with your SQLite file path

# # Read the CSV file
# csv_data <- read_csv(csv_file_path)
# # print(csv_data)

# # Connect to SQLite database
# conn <- dbConnect(SQLite(), sqlite_db_path)

# # Write CSV data into the SQLite database
# # Loop in data
# for (i in 1:nrow(csv_data)) {
#   dbExecute(conn,
#             "INSERT INTO push1 (date, pushups, type, pushup_pow) VALUES (?, ?, ?, ?)",
#             params = list(csv_data$date[i], csv_data$pushups[i], csv_data$type[i], csv_data$pushup_pow[i]))
# }

# # Close the SQLite connection
# dbDisconnect(conn)