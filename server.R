server <- function(input, output, session) {
  
  # Initialize connection to SQLite database
  con_str <- dbConnect(SQLite(), "C:/Users/Owner/OneDrive/Documents/ADAM TRACKER/pushup_data_file.sqlite")
  
  # Initialize reactive data - faith_weeks -
  react_faith_weeks <- reactiveVal({
  data <- dbReadTable(con_str, "faith_weeks")
  data$start_date = as_date(data$start_date)
  data
  })
  
  # FUNCTION TO ensure faith_weeks table is up to date - VERY IMPORTANT TO RUN FIRST
  ensure_faith_weeks <- function() {

    ### Get today's date and most recent Monday
    today <- Sys.Date()
    last_monday <- today - (wday(today) -2) %% 7
    
    ### Read faith_weeks table
    current_data <- dbReadTable(con_str, "faith_weeks")

    ### Handling for when the current_data is empty or 0
    if (nrow(current_data) == 0) {
      last_table_monday_value <- NA
    } else {
      last_table_monday_value <- as_date(tail(current_data$start_date, 1))
    }

    ## If statement to create new entry if needed.
    ### 1st condition if the table is empty. 2nd condition if too
    if(is.na(last_table_monday_value) || last_table_monday_value < last_monday) {
      ### Insert new value into the SQLite Database
      dbExecute(
        con_str,
        "INSERT INTO faith_weeks (start_date, mon, tue, wed, thu, fri, sat, sun) VALUES(?, 0, 0, 0, 0, 0, 0, 0)",
        params = list(as.numeric(last_monday))
      )
      ### Update the react_faith_weeks
      react_faith_weeks({
        data <- dbReadTable(con_str, "faith_weeks")
        data$start_date <- as_date(data$start_date)
        data
      })
    }
  }

  # Start up check to get old or make new faith_weeks record - VERY IMPORTANT TO RUN FIRST
  ensure_faith_weeks()

  ### Filter the faith data to just get the current months data
  react_faith_this_month <- reactive({

    # Get original data
    data <- react_faith_weeks()

    # Get the current month and year
    current_month <- month(Sys.Date())
    current_year <- year(Sys.Date())
    
    # Make a column for the last day of each row:
    data$end_date <- data$start_date + days(6)

    # Filter data that either has start or end date in the current month
    data <- data[
      (month(data$start_date) == current_month & year(data$start_date) == current_year) | 
      (month(data$end_date) == current_month & year(data$end_date) == current_year), 
    ]
    # Get rid of columns that won't help - week_id and end_date
    data <- subset(data, select = -c(week_id, end_date))
    
    # the marked dates vector that are marked with 1
    marked_dates <- c()

    # Loop through each row(starting with Monday); adds to marked dates
    for (i in 1:nrow(data)){
      if (data$mon[i] == 1) {
        marked_dates <- c(marked_dates, data$start_date[i])
      }
      if (data$tue[i] == 1) {
        marked_dates <- c(marked_dates, data$start_date[i] + 1)
      }
      if (data$wed[i] == 1) {
        marked_dates <- c(marked_dates, data$start_date[i] + 2)
      }
      if (data$thu[i] == 1) {
        marked_dates <- c(marked_dates, data$start_date[i] + 3)
      }
      if (data$fri[i] == 1) {
        marked_dates <- c(marked_dates, data$start_date[i] + 4)
      }
      if (data$sat[i] == 1) {
        marked_dates <- c(marked_dates, data$start_date[i] + 5)
      }
      if (data$sun[i] == 1) {
        marked_dates <- c(marked_dates, data$start_date[i] + 6)
      }
    }

    # Convert vector of dates to date format
    marked_dates <- as_date(marked_dates)

    # Get rid of dates that are not in the current month then get just the day values
    marked_dates <- day(marked_dates[month(marked_dates) == current_month])
    
    # Return the the day values in marked_dates
    marked_dates
  })

  # Get Financial Data
  react_financial <- reactiveVal({
      data <- dbReadTable(con_str, "financial_data")
      
      ### Make changes to data
      data <- data %>%
        mutate(
        ### Convert from SQLite REAL to readable date
        date = as_date(data$date),
        ### Make net column from assets and liabilities
        net = assets - liabilities
        ) 
  })

  # Initialize reactive data - pushups
  react_pushup_data <- reactiveVal({
    data <- dbReadTable(con_str, "push1")
    
    # convert to readable dates
    data$date <- as_date(data$date)
    
    # return all data
    data
  })

  # Initialize reactive data - situps
  react_situp_data <- reactiveVal({
    data1 <- dbReadTable(con_str, "situps")

    # convert to readable dates
    data1$date <- as_date(data1$date)

    # return all data
    data1
  })
  
  # Initialize reactive data - study
  react_study_data <- reactiveVal({
    data2 <- dbReadTable(con_str, "study")
    
    # convert to readable dates
    data2$date <- as_date(data2$date)
    
    # return all data
    data2
  })

  ### CREATING A STUDY BUFFER TABLE HERE
  react_study_buffer <- reactiveVal({
    # Grab data
    data <- dbReadTable(con_str, "study_buffer")
    # Change date to readable
    data$date <- as_date(data$date)
    # Return all data
    data
  })
  
  ### Output study_buffer data for table
  output$react_study_buffer <- DT::renderDataTable({
    react_study_buffer()
  })

  # Observe event to delete study_buffer if it is from a previous day (For study section)
  observe({
    delete_old_buffer <- function(){
      ## Get today's date
      today <- Sys.Date()
    
      ## Get data from react_study_buffer
      buffer_data <- react_study_buffer()
    
      if (nrow(buffer_data) > 0) {
        last_date <- max(buffer_data$date, na.rm = TRUE)  # Get the latest date
        
        ## Check if the last date is before today
        if (last_date < today) {
          # Delete all rows from study_buffer table in SQLite
          dbExecute(con_str, "DELETE FROM study_buffer")
          
          # Clear the react_study_buffer reactive value
          react_study_buffer(data.frame())
          
          print("Old buffer data deleted.")
        }
      }
    }
    delete_old_buffer()
  })

  # Initialize reactive data - person table
  react_person_data <- reactiveVal({
    data3 <- dbReadTable(con_str, "person")
    
    # convert to readable dates
    data3 <- data3 %>%
      mutate(
        ### Column - Date last conntected
        date_last_connected = as_date(data3$date_last_connected),
        ### Calculate the date to contact person
        day_to_contact = date_last_connected + days(contact_after_x_days),
        ### Calculate Binary output Yes 1 or No 0 to contact person
        contact = if_else(day_to_contact <= today(), 1, 0)
      ) %>%
      ### Put the people to contact at the top of the list
      arrange(desc(contact))
       
    # return all data
    data3
  })

  # Initialize ractive data - notes_log table
  react_notes_log <- reactiveVal({
    notes_data <- dbReadTable(con_str, "notes")

    # convert to readable dates
    notes_data$date_added <- as_date(notes_data$date_added)

    # return all data
    notes_data
  })

  #######################################################################################################
  # Intialize reactive variable to hold minutes studied today:
  total_minutes_studied_today <- reactiveVal(0)

  # HOME - FAITH LOG - Observe Event for updating the Faith Log boxes
  observe({

    # Get the latest record from react_faith_weeks
    faith_week_data <- tail(react_faith_weeks(), 1)
    
    # Define the days corresponding to each box
    day_columns <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")

    # Prepare a list of button updates
    button_updates <- lapply(seq_along(day_columns), function(i) {
      day_value <- faith_week_data[[day_columns[i]]]
      color <- ifelse(day_value == 1, "green", "white")
      list(id = paste0("box_", i), color = color)
    })
    
    # Send the updates to JavaScript
    session$sendCustomMessage("updateButtonColors", button_updates)
    
  })

  # HOME - FAITH LOG - Handle button clicks for Boxes
  lapply(1:7, function(i) {
    observeEvent(input[[paste0("box_", i)]], {
      # Get the current reactive dataset
      faith_week_data <- react_faith_weeks()
      
      # Define the day column corresponding to the clicked box
      day_columns <- c("mon", "tue", "wed", "thu", "fri", "sat", "sun")
      day_column <- day_columns[i]
      
      # Get the index of the last record (latest week)
      last_index <- nrow(faith_week_data)
      
      # Toggle the value for the corresponding day in the last record
      new_value <- ifelse(faith_week_data[[day_column]][last_index] == 1, 0, 1)
      
      # Update the SQLite database
      dbExecute(
        con_str,
        sprintf(
          "UPDATE faith_weeks SET %s = %d WHERE week_id = %d",
          day_column, new_value, faith_week_data$week_id[last_index]
        )
      )
      
      # Update the reactive dataset
      faith_week_data[[day_column]][last_index] <- new_value
      react_faith_weeks(faith_week_data)
      
      # Change the box color dynamically
      color <- ifelse(new_value == 1, "green", "grey")
      session$sendCustomMessage("updateButtonColors", list(list(id = paste0("box_", i), color = color)))
      })
  })

  # Observe and update the total_minutes_studied_today based on the last record
  observe({
    # Get the reactive study data
    study_data <- react_study_data()
    
    # Check if there is any data
    if (nrow(study_data) > 0) {
      # Get the last record's date
      last_record_date <- as.Date(study_data$date[nrow(study_data)])
      
      # Check if the last record is for today's date
      if (last_record_date == Sys.Date()) {
        # If yes, set total_minutes_studied_today to the 'minutes' value for today's date
        total_minutes_studied_today(study_data$minutes[nrow(study_data)])
      } else {
        # If no, set total_minutes_studied_today to 0
        total_minutes_studied_today(0)
      }
    } else {
      # If there's no data, set total_minutes_studied_today to 0
      total_minutes_studied_today(0)
    }
  })

  # Creating a goal for minutes each day
  day_goal <- reactiveVal(240)

  observe({
    # Calculate progress percentage
    progress_percent <- (total_minutes_studied_today() / day_goal()) * 100
    progress_percent <- min(progress_percent, 100)  # Ensure it doesn't exceed 100%

    # Dynamically update the progress bar on page load
    shinyWidgets::updateProgressBar(
      session = session, 
      id = "study_progress", 
      value = progress_percent
    )
  })

  # Get 'name' column from person table
  observe({
    # Query person table
    person_names <- dbGetQuery(con_str, "SELECT name FROM person")$name

    # Update the selectizeInput choices
    updateSelectizeInput(session, "person", choices = person_names, server = TRUE)
  })
########################################################################################################
  
  # Initialize reactive values for HOME Financial KPI's
  kpi_cash <- reactiveVal(0)
  kpi_debt <- reactiveVal(0)
  kpi_total <- reactiveVal(0)

  ### Update the Values in the UI
  observe({
    # Example logic (replace with actual financial calculations)
    kpi_cash(tail(react_financial()$assets,1))  # Set cash value
    kpi_debt(tail(react_financial()$liabilities,1))  # Set debt value, 8800 student loan 4870 credit card, 400 parent loan + 400
    kpi_total(tail(react_financial()$net,1))  # Calculate total
  })

  ### LINE CHART FOR Financial Data 
  output$finance_plot <- renderPlot({
    # Ensure react_financial() is returning a valid dataframe
    data <- react_financial()
    
    ggplot(data, aes(x = date, y = net)) +
      geom_line(color = "blue", size = 1) +  # Blue line
      geom_point(color = "red", size = 2) +  # Red dots at points
      labs(title = "", x = "Date", y = "Net Worth") +
      #ylim(min(data$net), 0) +
      theme_minimal() +  # Clean theme
      theme(
        panel.grid = element_blank()
      )
  })

  ### Show Financial Data Table in Finances
  output$financial_table <- DT::renderDataTable({
    data <- react_financial()
    
    DT::datatable(data, 
                  options = list(pageLength = 10, 
                                autoWidth = TRUE,
                                scrollX = TRUE),
                  filter = 'top',
                  rownames = FALSE) |>
      formatDate(columns = 'date', method = 'toLocaleDateString')
  })

  ### ENTRY FOR FINANCIAL DATA
  observeEvent(input$submit_financial_data, {
    
    ### Create a new entry:
    new_financial_entry <- data.frame(
      date = as.Date(input$financial_date),
      assets_ = as.numeric(input$assets),
      liabilities_ = as.numeric(input$liabilities)
    )

    ### Update the SQLite Database with new record and values
    tryCatch({
      dbExecute(con_str, "INSERT INTO financial_data (date, assets, liabilities) VALUES (?, ?, ?)", 
            params = list(new_financial_entry$date, 
                          new_financial_entry$assets_, 
                          new_financial_entry$liabilities_)
                )
      showNotification("Finances updated successfully!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error adding note:", e$message), type = "error", duration = 5)
    })

    ### Update the react_financial
    updated_financial_data <- dbReadTable(con_str, "financial_data")
    
    # Recalculate the net column and format the date
    updated_financial_data <- updated_financial_data %>%
    mutate(
      date = as_date(date),          # Convert SQLite REAL to readable date
      net = assets - liabilities     # Calculate net column
    )
  
    # Update the reactive value
    react_financial(updated_financial_data)
  
    # Set KPI values using the last row
    kpi_cash(tail(updated_financial_data$assets, 1))          # Set cash value
    kpi_debt(tail(updated_financial_data$liabilities, 1))    # Set debt value
    kpi_total(tail(updated_financial_data$net, 1))           # Set total (net worth) value

  })
  
########################################################################################################
  # Variable to store the last time the button was pressed
  last_pressed_time <- reactiveVal(NULL)
  
  # # Variable to store the total paused time
  total_paused_time <- reactiveVal(0)
  
  # # Flag to track whether the timer is paused or running
  is_paused <- reactiveVal(FALSE)
  
  # Timer to update the elapsed time every second
  autoInvalidate <- reactiveTimer(1000)
  
############################################################################################################

  # Update HOME FINANCIAL KPI values:
  output$kpi_cash <- renderText({ paste("Cash:", dollar(kpi_cash())) })
  output$kpi_debt <- renderText({ paste("Debt:", dollar(kpi_debt())) })
  output$kpi_total <- renderText({ paste("Total:", dollar(kpi_total())) })

############################################################################################################

  # Observe the button press and update the current time
  observeEvent(input$get_time, {
    # Get the current time when button is pressed
    current_time <- Sys.time()
    
    # Store the time in last_pressed_time and reset the paused time
    last_pressed_time(current_time)
    total_paused_time(0)  # Reset the total paused time
    is_paused(FALSE)      # Ensure the timer is running when restarted
    
    # Update the small text output with the current time
    output$current_time_display <- renderText({
      paste("Started at:", format(current_time, "%H:%M:%S"))
    })
  })
  
  # Continuously update the elapsed time since the button was last pressed
  output$elapsed_time_display <- renderText({
    autoInvalidate()  # This will trigger every second
    
    # Calculate elapsed time if the button has been pressed
    if (!is.null(last_pressed_time())) {
      if (is_paused()) {
        # When paused, don't increment the elapsed time
        elapsed <- total_paused_time()
      } else {
        # Otherwise, calculate the elapsed time as normal
        elapsed <- Sys.time() - last_pressed_time()
      }
      elapsed_seconds <- as.numeric(elapsed, units = "secs")
      
      # Convert elapsed seconds to hours, minutes, and seconds
      hours <- floor(elapsed_seconds / 3600)
      minutes <- floor((elapsed_seconds %% 3600) / 60)
      seconds <- round(elapsed_seconds %% 60)
      
      # Display the time in a more readable format (HH:MM:SS)
      paste("Elapsed Time:", sprintf("%02d:%02d:%02d", hours, minutes, seconds))
    } else {
      "Press the button to start tracking time."
    }
  })

##########################################################
  
  ### FAITH DATA CALENDAR
  output$faith_calendar <- renderPlot({
    calendR(
      month = as.numeric(format(Sys.Date(), "%m")),
      start="M",
      special.days = react_faith_this_month(),
      special.col = "green",
      low.col = "white"  )
  })

  ### VIEW THE FAITH WEEKS TABLE FULLY
  output$faith_weeks <- DT::renderDataTable({
    data <- react_faith_weeks()

    DT::datatable(data, 
                  options = list(pageLength = 10, 
                                autoWidth = TRUE,
                                scrollX = TRUE),
                  filter = 'top',
                  rownames = FALSE) |>
      formatDate(columns = 'start_date', method = 'toLocaleDateString')
  })

  ### VIEW THE RELATIONSHIP TABLE FULLY
  output$person_table <- DT::renderDataTable({
    data <- react_person_data()
    
    DT::datatable(data, 
                  options = list(pageLength = 10, 
                                autoWidth = TRUE,
                                scrollX = TRUE),
                  filter = 'top',
                  rownames = FALSE) |>
      formatDate(columns = 'date_last_connected', method = 'toLocaleDateString')
  })

  ### FOR HOME RELATIONSHIP TAB
  output$relationship_names <- renderUI({
    data <- react_person_data()
  
    # Filter data where contact is required
    contact_data <- data %>%
    filter(contact == 1)
  
    if (nrow(contact_data) > 0) {
      # Create a list of names
      name_list <- paste(contact_data$name, collapse = ", ")
    
      # Render the names
      div(
        style = "padding: 10px; font-size: 20px; background-color: rgba(255, 255, 255, 0.2); border-radius: 5px; text-align: center;",
        HTML(name_list)
      )
    } else {
      div(
        style = "padding: 10px; font-size: 20px; background-color: rgba(255, 255, 255, 0.2); border-radius: 5px; text-align: center;",
        "No one to contact."
      )
    }
  })

  ### VIEW THE RELATIONSHIP NOTES FULLY
  output$notes_log <- DT::renderDataTable({
    data_for_notes <- react_notes_log()

    DT::datatable(data_for_notes, 
                  options = list(
                    pageLength = 10, 
                    autoWidth = TRUE),
                  filter = 'top',
                  rownames = FALSE) |>
      formatDate(columns = 'date_added', method = 'toLocaleDateString')

  })

##########################################################
  # Submitting a new person to person table
  observeEvent(input$submit_person, {
    # Prepare the SQL statement
    sql <- "INSERT INTO person (name, relationship, date_last_connected, contact_after_x_days, phone_number, email) 
            VALUES (?, ?, ?, ?, ?, ?)"
    
    # Execute the SQL statement
    dbExecute(con_str, sql, params = list(
      input$name,
      input$relationship,
      as.numeric(input$date_last_connected),  # Convert to numeric for SQLite
      input$contact_after_x_days,
      input$phone_number,
      input$email
    ))
    
    # Update the reactive value
    updated_data <- dbReadTable(con_str, "person")
    updated_data$date_last_connected <- as_date(updated_data$date_last_connected)
    react_person_data(updated_data)
    
    # Clear input fields after submission
    updateTextInput(session, "name", value = "")
    updateTextInput(session, "relationship", value = "")
    updateDateInput(session, "date_last_connected", value = Sys.Date())
    updateNumericInput(session, "contact_after_x_days", value = 30)
    updateTextInput(session, "phone_number", value = "")
    updateTextInput(session, "email", value = "")
  })

  # Submitting a new note
  observeEvent(input$submit_relationship_note, {

      # Safely calculate note_number
      if (!is.null(react_notes_log()) && nrow(react_notes_log()) > 0) {
        # Calculate the next note_id if data exists
        note_number <- max(react_notes_log()$note_id, na.rm = TRUE) + 1
      } else {
        # Default to 1 if react_notes_log() is empty
        note_number <- 1
      }
    # Grab the person_id that matches the name
    person_id_ <- react_person_data()$person_id[react_person_data()$name == input$person]

    # Validate inputs
    if (is.null(input$person) || input$person == "") {
      showNotification("Please select a person.", type = "error", duration = 5)
      return()
    }

    ### Maybe add a Validate Date condition here in future
    if (is.null(input$note) || input$note == "") {
      showNotification("Please enter a note.", type = "error", duration = 5)
      return()
    }

    # Create new note entry
    new_note <- data.frame(
      note_id = note_number,
      person_id = person_id_,
      note = input$note,
      date_added = as.Date(input$date),
      stringsAsFactors = FALSE
    )

    # Update the reactive notes log
    current_log <- react_notes_log()
    updated_log <- rbind(current_log, new_note)
    react_notes_log(updated_log)

    #Get last row or react_notes_log
    last_row_ <- tail(react_notes_log(), 1)

    tryCatch({
      dbExecute(con_str, 
                "INSERT INTO notes (note_id, person_id, note, date_added) VALUES (?, ?, ?, ?)", 
                params = list(
                  last_row_$note_id,
                  last_row_$person_id,
                  last_row_$note,
                  last_row_$date_added
                ))
      showNotification("Note added successfully!", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("Error adding note:", e$message), type = "error", duration = 5)
    })

    ### ALSO UPDATE ANOTHER REACTVAL AND SQL TABLE with "new_note$date_added" value
    tryCatch({
      # Update the date_last_connected in the SQLite database
      dbExecute(con_str, 
                "UPDATE person SET date_last_connected = ? WHERE person_id = ?", 
                params = list(
                  as.numeric(last_row_$date_added),  # Ensure date format matches SQLite
                  last_row_$person_id
                ))

      # Refresh the reactive person data
      updated_person_data <- dbReadTable(con_str, "person")
      updated_person_data$date_last_connected <- as_date(updated_person_data$date_last_connected)
      react_person_data(updated_person_data)

      showNotification("Person's last connected date updated!", type = "message", duration = 3)
    }, error = function(e) {
    showNotification(paste("Error updating person:", e$message), type = "error", duration = 5)
    })
  })

  # Reactive calculation for total minutes in study table
  total_minutes <- reactive({
    sum(react_study_data()$minutes, na.rm = TRUE)
  })

  # Get the last program listed in study data
  current_program <- reactive({
    last_program <- react_study_data() %>%
      dplyr::filter(!is.na(program)) %>%
      dplyr::pull(program) %>%
      tail(1)
    
    if (length(last_program) == 0) {
      return(NA) # Return NA if no non-null values are found
    } else {
      return(last_program)
    }
  })

  ### Show table of Study Data (Bording Data)
  output$boring_study <- DT::renderDataTable({
    #Get Data
    data <- react_study_data()
    # Make it descending order
    data <- data %>% arrange(desc(id))
    
    DT::datatable(data, 
                  options = list(pageLength = 10, 
                                autoWidth = TRUE,
                                scrollX = TRUE),
                  filter = 'top',
                  rownames = FALSE)
  })

  ### STUDY - MINUTES - BOX(Total Count) (VIEW)
  output$current_program_box <- renderValueBox({
    valueBox(
      value = formatC(current_program(), format="f", big.mark=",", digits=0),
      subtitle = "Current Program",
      icon = icon("school"),
      color = "blue"
    )
  })
  
  ### STUDY - MINUTES - BOX(Total Count) (VIEW)
  output$total_minutes_box <- renderValueBox({
    valueBox(
      value = formatC(total_minutes(), format="f", big.mark=",", digits=0),
      subtitle = "Total Minutes",
      icon = icon("clock"),
      color = "blue"
    )
  })
  
  ### STUDY - HOURS - BOX(Total Count) (VIEW)
  output$total_hours_box <- renderValueBox({
    valueBox(
      value = formatC(total_minutes()/60, format="f", big.mark=",", digits=0),
      subtitle = "Total Hours",
      icon = icon("clock"),
      color = "blue"
    )
  })

  ### STUDY - SUBMIT GOAL
  observeEvent(input$submit_goal, {
    new_goal <- input$enter_goal
    if (new_goal >= 0) {  # Basic validation
      day_goal(new_goal*60)

      # After updating the goal, recalculate the progress percentage
      progress_percent <- (total_minutes_studied_today() / day_goal()) * 100
      progress_percent <- min(progress_percent, 100)  # Ensure it doesn't exceed 100%
      
      # Dynamically update the progress bar
      shinyWidgets::updateProgressBar(
        session = session, 
        id = "study_progress", 
        value = progress_percent
      )
    }
  })

  ### Update the default value for anki carts
  observe({
    # Extract the 'card_number' column, remove NAs, and get the last non-null value
    last_value <- react_study_data()$anki
    last_value <- last_value[!is.na(last_value)]  # Remove NAs
    
    if (length(last_value) > 0) {
      last_value <- tail(last_value, n = 1)  # Get the last valid value
    } else {
      last_value <- 0  # Default value if no valid values exist
    }

    # Update the numeric input
    updateNumericInput(session, "anki_card_number", value = last_value)
  })

  ### Update the default value for Program ________________________________________________________________________________________________
    observe({
      # Extract the 'program' column, remove NAs, and get the last non-null value
      last_value_p <- react_study_data()$program
      last_value_p <- last_value_p[!is.na(last_value_p)]  # Remove NAs

      if (length(last_value_p) > 0) {
        last_value_p <- tail(last_value_p, n = 1)  # Get the last valid value
      } else {
        last_value_p <- 'null'  # Default value if no valid values exist
      }
      ###print(last_value_p)
      # Update the selectize input
      updateTextInput(session, "program_input", value = last_value_p)
    })


  ### Submit Minutes when clicked
  observeEvent(input$submit_minutes, {
  
    # Get the value from the numeric input
    new_minutes <- as.numeric(input$study_minutes)
    
    ### Create a new entry for today's study session
    new_entry_study <- data.frame(
      date = Sys.Date(),  # Use today's date
      minutes = new_minutes  # Minutes entered by the user
    )

    ### *** STUDY_BUFFER - UPDATE THE SQLITE DATABASE FOR 
    dbExecute(con_str, 
            "INSERT INTO study_buffer (date, min) VALUES (?, ?)", 
            params = list(new_entry_study$date, new_entry_study$min))
    
    ### *** STUDY_BUFFER - GET THE NEW DATA FROM SQLITE
    updated_study_buffer <- dbReadTable(con_str, "study_buffer")

    ### *** STUDY_BUFFER - Update the reactive data
    react_study_buffer(updated_study_buffer)

    ### Get current study data from the reactive data
    current_data <- react_study_data()
    
    ### Check if there is an entry for today
    if (nrow(current_data) > 0 && new_entry_study$date == max(current_data$date)) {
      
      # Update the minutes for today's date
      current_data$minutes[which.max(current_data$date)] <- 
        current_data$minutes[which.max(current_data$date)] + new_entry_study$minutes
      
      # Update the record directly in the SQLite database
      dbExecute(con_str, 
                "UPDATE study SET minutes = ? WHERE date = ?", 
                params = list(current_data$minutes[which.max(current_data$date)], 
                              new_entry_study$date))
      
    } else {
      ### Insert a new record if today's entry does not exist
      dbExecute(con_str, 
                "INSERT INTO study (date, minutes) VALUES (?, ?)", 
                params = list(new_entry_study$date, new_entry_study$minutes))
    }
    
    ### Read the updated data from the database
    updated_study_data <- dbReadTable(con_str, "study")
    
    ### Update the reactive study data with the new data
    react_study_data(updated_study_data)
    
    ### Update the total minutes studied today
    total_minutes_studied_today(total_minutes_studied_today() + new_minutes)
    
    ### Calculate the progress percentage
    progress_percent <- (total_minutes_studied_today() / day_goal()) * 100
    progress_percent <- min(progress_percent, 100)  # Ensure it doesn't exceed 100%
    
    ### Dynamically update the progress bar
    shinyWidgets::updateProgressBar(
      session = session, 
      id = "study_progress", 
      value = progress_percent
    )
  })

  ### Submit FULL ENTRY for Study Data
  observeEvent(input$submit_full_entry, {
    # Create new entry for study data
    new_study_entry <- data.frame(
      date = as.Date(input$study_date),
      minutes = total_minutes_studied_today(),  # assuming you're using this as the minutes studied for the day
      notes = input$study_notes,
      anki_cards = input$anki_card_number,
      github_updated = as.logical(input$github_check),
      linkedin_updated = as.logical(input$linkedin_check),
      program = input$program_input,
      status = input$status_input
    )
    
    # Get current reactive study data
    current_data <- react_study_data()
    
    # Check if there's an entry for today's date in the current data
    if (nrow(current_data) > 0 && new_study_entry$date == max(current_data$date)) {
      # Update the most recent record
      current_data$minutes[which.max(current_data$date)] <- new_study_entry$minutes
      current_data$notes[which.max(current_data$date)] <- new_study_entry$notes
      current_data$anki_cards[which.max(current_data$date)] <- new_study_entry$anki_cards
      current_data$github_updated[which.max(current_data$date)] <- new_study_entry$github_updated
      current_data$linkedin_updated[which.max(current_data$date)] <- new_study_entry$linkedin_updated
      current_data$program[which.max(current_data$date)] <- new_study_entry$program
      current_data$status[which.max(current_data$date)] <- new_study_entry$status

      # Update the record directly in the database
      dbExecute(con_str, 
                "UPDATE study SET 
                  minutes = ?, notes = ?, anki = ?, github = ?, linkedin = ?, 
                  program = ?, status = ? 
                WHERE date = ?", 
                params = list(new_study_entry$minutes, new_study_entry$notes, new_study_entry$anki_cards, 
                              new_study_entry$github_updated, new_study_entry$linkedin_updated, 
                              new_study_entry$program, new_study_entry$status, new_study_entry$date))
    } else {
      # Insert the new entry into the database
      dbExecute(con_str, 
                "INSERT INTO study 
                (date, minutes, notes, anki, github, linkedin, program, status) 
                VALUES (?, ?, ?, ?, ?, ?, ?, ?)", 
                params = list(new_study_entry$date, new_study_entry$minutes, new_study_entry$notes, 
                              new_study_entry$anki_cards, new_study_entry$github_updated, 
                              new_study_entry$linkedin_updated, new_study_entry$program, new_study_entry$status))
    }

    # Read the updated study data from the database
    updated_study_data <- dbReadTable(con_str, "study")
    
    # Update the reactive variable with the new data
    react_study_data(updated_study_data)
  })

  # Show Hours of Day Goal
  output$day_goal_display <- renderText({
      paste(day_goal()/60, "hours")
  })

  # Dynamically update value for numeric input
  output$dynamic_study_duration <- renderUI({
    numericInput("study_duration", "Study Duration (minutes):", 
                value = total_minutes_studied_today(), 
                min = 0)
  })

  ### STRENGTH - PUSHUP LINE CHART
  output$study_minutes_linechart <- renderPlot({
    all_data <- react_study_data()

    # Filter data based on selected date range
    filtered_data <- all_data %>%
      dplyr::filter(as.Date(date) >= input$date_range[1] &
                  as.Date(date) <= input$date_range[2])
    # Plot
    ggplot(
      filtered_data,
      aes(
        x = as.Date(date),
        y = minutes/60
      )
    ) +
      geom_line(color = "red") +
      labs(
        x = "",
        y = "Hours"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  ### UPDATE FORM - STRENGTH (PUSHUPS)
  observeEvent(input$submit_pushups, {
    
    ### Create new entry
    new_entry_pushup <- data.frame(
      date = as.Date(input$exercise_date),
      pushups = as.numeric(input$exercise_count),
      type = ifelse(input$type_input == "Regular", 72.3, 83.7)
    )
    
    ### Get current strength data
    current_data <- react_pushup_data()
    
    ### Check if the new entry date matches the most recent record
    if (new_entry_pushup$date == max(current_data$date)) {
      
      # Updates the most recent record
      current_data$pushups[which.max(current_data$date)] <- current_data$pushups[which.max(current_data$date)] + new_entry_pushup$pushups
      
      ### I will not update type because I will assume that I will keep the type the same throughout the same day

      ### Update the pushup_pow in current data
      current_data$pushup_pow[which.max(current_data$date)] <- current_data$pushup_pow[which.max(current_data$date)] + (new_entry_pushup$pushups * new_entry_pushup$type)

      ### Updated Data
      updated_strg_data <- current_data

      ### Validation - checking if type is the same
      if (new_entry_pushup$date == max(current_data$date) && 
        new_entry_pushup$type != current_data$type[which.max(current_data$date)]) {
      
      # Notify the user about the mismatch and exit the function
      showNotification("Error: Must have same type all day.", type = "error", duration = 5)
      return()
      }

      # Update the record directly in the database NEED TO UPDATE pushups, pushup_pow
      dbExecute(con_str, "UPDATE push1 SET pushups = ?, pushup_pow = ? WHERE id = ?", 
                params = list(
                  current_data$pushups[which.max(current_data$date)],
                  current_data$pushup_pow[which.max(current_data$date)],
                  current_data$id[which.max(current_data$date)]))
      
    } else {
      ### THIS Makes a new Entry into the Table
      ### Append the new entry to the database without overwriting the table ######################### ERRROR IS HERE WHEN TRYING TO GET NEW ENTRY CRASH: CAN'T FIND updated strg_data
      dbExecute(con_str, 
                "INSERT INTO push1 (date, pushups, type, pushup_pow) VALUES (?, ?, ?, ?)", 
                params = list(
                new_entry_pushup$date, 
                new_entry_pushup$pushups,
                new_entry_pushup$type, 
                new_entry_pushup$pushups * new_entry_pushup$type))

      new_id <- dbGetQuery(con_str, "SELECT last_insert_rowid()")[[1]]

      # Update current_data to include the new entry
      updated_strg_data <- rbind(
        current_data, 
        cbind(
          new_entry_pushup, 
          pushup_pow = new_entry_pushup$pushups * new_entry_pushup$type, 
          id = new_id))
    }
    ### Update the reactive strength data
    react_pushup_data(updated_strg_data)
  })
  
  ### UPDATE FORM - SITUPS (SITUPS)
  observeEvent(input$submit_situps, {

    ### Create new entry
    new_entry_situp <- data.frame(
      date = as.Date(input$exercise_date),
      situps = as.numeric(input$exercise_count)
    )
    
    ### Get current strength data
    current_data <- react_situp_data()
    
    ### Check if the new entry date matches the most recent record
    if (nrow(current_data) > 0 &&
        new_entry_situp$date == max(current_data$date)) {

      # Update the most recent record
      current_data$situps[which.max(current_data$date)] <-
        current_data$situps[which.max(current_data$date)] + new_entry_situp$situps
      updated_strg_data <- current_data

      #Update the record directly in the database
      dbExecute(con_str,
                "UPDATE situps SET situps = ? WHERE id = ?",
                params = list(current_data$situps[which.max(current_data$date)],
                              current_data$id[which.max(current_data$date)]))

    } else {
      ### Append the new entry to the database without overwriting the table
      dbExecute(con_str,
                "INSERT INTO situps (date, situps) VALUES (?, ?)",
                params = list(new_entry_situp$date, new_entry_situp$situps))
    }

    ### Read the updated data
    updated_strg_data <- dbReadTable(con_str, "situps")

    ### Update the reactive strength data
    react_situp_data(updated_strg_data)

  })
  
  # Reactive calculation for total pushups
  total_pushups <- reactive({
    sum(react_pushup_data()$pushups, na.rm = TRUE)
  })
  
  # Reactive calculation for total pushups for day
  total_pushups_today <- reactive({
    # Get today's date
    today_date <- as_date(Sys.Date())
    
    # Filter the data for today's date
    today_data <- react_pushup_data()[react_pushup_data()$date == today_date, ]
    
    # Check if there is any data for today, if not return 0
    if (nrow(today_data) == 0) {
      return(0)
    } else {
      return(sum(today_data$pushups, na.rm = TRUE))
    }
  })

  # Reactive calculation for total situps
  total_situps <- reactive({
    sum(react_situp_data()$situps, na.rm = TRUE)
  })
  
  # Reactive calculation for total pushups for day 
  total_situps_today <- reactive({
    # Get today's date
    today_date <- as_date(Sys.Date())
    
    # Filter the data for today's date
    today_data <- react_situp_data()[react_situp_data()$date == today_date, ]
    
    # Check if there is any data for today, if not return 0
    if (nrow(today_data) == 0) {
      return(0)
    } else {
      return(sum(today_data$situps, na.rm = TRUE))
    }
  })
  
  ### STRENGTH - PUSHUPS - BOX(Total Count) (VIEW)
  output$total_pushup_box_view <- renderValueBox({
    valueBox(
      value = formatC(total_pushups(), format="f", big.mark=",", digits=0),
      subtitle = "Total Pushups",
      icon = icon("dumbbell"),
      color = "blue"
    )
  })
  
  ### STRENGTH PUSHUPS - BOX(Today Count) (ENTRY) *************
  output$today_pushup_box <- renderValueBox({
    valueBox(
      value = formatC(total_pushups_today(), format = "f", big.mark = ",", digits = 0),  # total_pushups is a reactive function
      subtitle = "Today's Pushups",
      icon = icon("dumbbell"),
      color = "blue"
    )
  })
  
  ## STRENGTH SITUPS - BOX(Total Count) (VIEW)
  output$total_situp_box_view <- renderValueBox({
    valueBox(
      value = formatC(total_situps(), format = "f", big.mark = ",", digits = 0),
      subtitle = "Total Situps",
      icon = icon("dumbbell"),
      color = "blue"
    )
  })
  
  ## STRENGTH SITUPS - BOX(Today Count) (ENTRY)
  output$today_situp_box <- renderValueBox({
    valueBox(
      value = formatC(total_situps_today(), format = "f", big.mark = ",", digits = 0),
      subtitle = "Total Situps",
      icon = icon("dumbbell"),
      color = "blue"
    )
  })
  
  ### STRENGTH - PUSHUP LINE CHART
  output$pushup_line_chart <- renderPlot({
    line_strength_data <- react_pushup_data()
    
    # Filter data based on date range
    filtered_pushup_data <- line_strength_data %>%
      dplyr::filter(as.Date(date) >= input$strength_date_range[1] &
                    as.Date(date) <= input$strength_date_range[2])
    
    # Plot
    ggplot(
      filtered_pushup_data,
      aes(
        x = as.Date(date),
        y = pushup_pow
      )
    ) +
      geom_line(color = "blue") +
      labs(
        x = "",
        y = "Pushup Power"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  ### STRENGTH - SITUP LINE CHART
  output$situp_line_chart <- renderPlot({

    line_situp_data <- react_situp_data()
  
    # Filter data based on date range
    filtered_situp_data <- line_situp_data %>%
      dplyr::filter(as.Date(date) >= input$strength_date_range[1] &
                    as.Date(date) <= input$strength_date_range[2])
  
    # Plot
    ggplot(
      filtered_situp_data,
      aes(
        x = as.Date(date),
        y = situps
      )
    ) +
    geom_line(color = "blue") +
    labs(
      x = "",
      y = "Sit Ups"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  })
  
  onStop(function() {
    dbDisconnect(con_str)
  })
}