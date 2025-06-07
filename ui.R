### Load Libraries
library(readxl)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(scales)
library(shinyjs)
library(DT)
library(ggplot2)
library(DBI)
library(RSQLite)
library(readxl)
library(shinyWidgets)
library(writexl)
library(calendR)
library(dplyr)
library(lubridate)

### UI - DASHBOARD
ui <- dashboardPage(
  dashboardHeader(title = "Life Tracker"),
  dashboardSidebar(
    sidebarMenu(
       ### HOME DASHBOARD
      menuItem(
        "Home",
        tabName = "Home",
        icon = icon("home")
      ),
      ### Faith Data
      menuItem(
        "Faith",
        tabName = "Faith",
        icon = icon("bible"),
        menuSubItem("View Calendar", tabName = "view_faith_cal"),
        menuSubItem("View Table", tabName = "view_faith")
      ),
      ### Financial Data
      menuItem(
        "Finances",
        tabName = "Finances",
        icon = icon("coins"),
        menuSubItem("View Data", tabName = "chart_finances"),
        menuSubItem("View Table", tabName = "view_finances"),
        menuSubItem("Enter Data", tabName = "financial_form")
      ),
      ### RELATIONSHIP DATA
      menuItem(
        "Relationships",
        tabName = "Relationships",
        icon = icon("users"),
        menuSubItem("People", tabName = "view_Rdata"),
        menuSubItem("Notes", tabName =  "view_log"),
        menuSubItem("Add Person", tabName = "entry_relationship"),
        menuSubItem("Enter Note", tabName = "entry_relationship_note")
      ),      
      ### STUDY DATA
      menuItem(
        "Study",
        tabName = "Study",
        icon = icon("graduation-cap"),
        menuSubItem("View Data", tabName = "view_data"),
        menuSubItem("View Table", tabName = "view_table"),
        menuSubItem("Enter Data", tabName = "study_entry")
      ),
      ### STRENGTH DATA
      menuItem(
        "Strength",
        tabName = "Strength",
        icon = icon("dumbbell"),
        menuSubItem("View Data", tabName = "view_strength"),
        menuSubItem("Enter Data", tabName = "strength_entry")
      )
    )
  ),

  ### DASHBOARD BODY HERE!
  dashboardBody(
    tabItems(
      ### TAB FOR HOME
      tabItem(
        tabName = "Home",
        ### MAIN TITLE - FOCUS POINT
        div(
          HTML("<p style='font-size: 30px; text-align: center; width: 100%;'>
                Seek first his kingdom and his righteousness, and all these things will be given to you as well.
              </p>")
        ),
        br(),
        #### ADDING HTML FOR CUSTOM BUTTON STUFF JAVASCRIPT
        tags$script(
          HTML("
            Shiny.addCustomMessageHandler('updateButtonColors', function(message) {
              message.forEach(function(update) {
                var button = document.getElementById(update.id);
                if (button) {
                  button.style.backgroundColor = update.color;
                }
              });
            });
          ")
        ),
        ### Boxes for Faith Log
        div(
          style = "display: flex; justify-content: space-between; align-items: center; padding: 15px;",
          lapply(1:7, function(i) {
            day_labels <- c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Sabbath")
            actionButton(
              inputId = paste0("box_", i),
              label = day_labels[i],
              style = "flex: 1; margin: 5px; height: 100px; font-size: 18px; text-align: center; background-color: white;"
            )
          })
        ),
        br(),
        ### Section for Relationships
        div(
          style = "display: flex; justify-content: space-between; align-items: flex-start; width: 100%; padding: 15px;",
          
          # Financial Info on Left Half
          div(
            style = "background-color: green; color: white; padding: 20px; width: 50%; border-radius: 10px;",
            HTML("<p style='font-size: 24px; text-align: center;'>Finances</p>"),
            
            # KPIs Section
            div(
              style = "margin-top: 20px; display: flex; flex-direction: column; gap: 10px;",
              lapply(c("Cash", "Debt", "Total"), function(kpi) {
                div(
                  style = "padding: 10px; font-size: 20px; background-color: rgba(255, 255, 255, 0.2); border-radius: 5px; text-align: center;",
                  textOutput(paste0("kpi_", tolower(kpi)))
                )
              })
            )
          ),
          # Relationships Info on Right Half
          div(
            style = "background-color: orange; color: white; padding: 20px; width: 45%; border-radius: 10px;",
            HTML("<p style='font-size: 24px; text-align: center;'>People to Contact</p>"),
  
            #Dynamic Relationship List
            div(
              style = "margin-top: 20px; display: flex; flex-direction: column; gap: 10px;",
              uiOutput("relationship_names")
            )
          )
        )
      ),
      ### TAB FOR FAITH CALENDAR
      tabItem(
        tabName = "view_faith_cal",
        plotOutput("faith_calendar")
      ),
      ### TAB FOR FAITH WEEKS
      tabItem(
        tabName = "view_faith",
        fluidRow(
          title = "View faith Data",
          width = 12,
          DT::dataTableOutput("faith_weeks")
        )
      ),
      ### TAB FOR VEWING FINANCES CHART
      tabItem(
        tabName = "chart_finances",
        ### MAIN TITLE - FOCUS POINT
        div(
          HTML("<p style='font-size: 30px; text-align: center; width: 100%;'>
                Honor the Lord with your wealth
              </p>")
        ),
        br(),
        ### INSERT LINE CHART HERE
        plotOutput("finance_plot")
      ),
      ### TAB FOR VIEW FINANCES TABLE
      tabItem(
        tabName = "view_finances",
        fluidRow(
          title = "View Finance Data",
          width = 12,
          DT::dataTableOutput("financial_table")
        )
      ),
      ### TAB FOR FINANCES ENTRY DATA
      tabItem(
        tabName = "financial_form",
        fluidPage(
          titlePanel("Financial Data Entry"),
          fluidRow(
            column(
              width = 6,  # Adjust width as needed
              div(
                style = "background-color: #f9f9f9; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
                h3("Enter Financial Data", style = "text-align: center;"),
                
                # Date input
                dateInput(
                  inputId = "financial_date",
                  label = "Date:",
                  value = Sys.Date(),  # Default to today's date
                  startview = "month",
                  weekstart = 1
                ),
                
                # Numeric input for assets
                numericInput(
                  inputId = "assets",
                  label = "Assets:",
                  value = 0,  # Default value
                  min = 0,   # Minimum value
                  step = 1   # Step for increment
                ),
                
                # Numeric input for liabilities
                numericInput(
                  inputId = "liabilities",
                  label = "Liabilities:",
                  value = 0,  # Default value
                  min = 0,   # Minimum value
                  step = 1   # Step for increment
                ),
                
                # Submit button
                actionButton(
                  inputId = "submit_financial_data",
                  label = "Submit",
                  icon = icon("check"),
                  style = "margin-top: 20px; width: 100%; background-color: orange; color: white; font-size: 18px;"
                )
              )
            )
          )
        )
      ),
      ### TAB FOR VIEWING STUDY TABLE
      tabItem(
        tabName = "view_table",
        fluidRow(
          title = "View Study Data",
          width = 12,
          DT::dataTableOutput("boring_study")
        )
      ),
      ### TAB FOR VIEW STUDY DATA
      tabItem(
        tabName = "view_data",
        # Top Row to give the Main Highlights
        fluidRow(
          valueBoxOutput("total_minutes_box", 
                         width = 4),
          valueBoxOutput("total_hours_box", 
                         width = 4),
          valueBoxOutput("current_program_box", 
                         width = 4)
        ),
        fluidRow(
          box(
            dateRangeInput(
              inputId = "date_range",
              label = "Select Date Range",
              start = Sys.Date() - 100, # Default start date (last 100 days)
              end = Sys.Date(),       # Default end date (today)
              format = "yyyy-mm-dd"
            ),
            width = 12,
            title = "Filter Date Range"
          )
        ),
        fluidRow(
          box(plotOutput("study_minutes_linechart"), 
              width = 12, 
              title = "Study Sessions") 
        )
      ),
      ### TAB FOR STUDY DATA ENTRY
      tabItem(
        tabName = "study_entry",
        
        # Custom CSS to change progress bar height
        tags$style(HTML("
          .progress {
          height: 40px;  /* Adjust height here */
          }
        ")),
        
        # Progress bar at the top (full width, green, no stripes, starts at 0%)
        div(
          style = "width: 100%;",  
          shinyWidgets::progressBar(
            id = "study_progress", 
            value = 0, 
            status = "success",  # Green color
            display_pct = FALSE
          )
        ),
      
        # Two side-by-side boxes (width 6 each)
        fluidRow(
          # Box 1 (on the left)
          box(
            width = 6,
            title = "Box 1: Study Input",
            height = "300px",  # Set height for box 1 to match box 2
            
            # Part 1: Numeric input and action button
            fluidRow(
              column(6,
                     numericInput(inputId = "study_minutes", label = "Enter minutes studied", value = 0, min = 0),
                     actionButton(inputId = "submit_minutes", label = "Submit"),
                     numericInput(inputId = "enter_goal", label = "Enter Hours Goal for today", value = 4, min = 0),
                     actionButton(inputId = "submit_goal", label = "Submit")
              ),
              
              # Part 2: Total minutes today (moved to the right side)
              column(6,

                # Button and time output
                actionButton(inputId = "get_time", label = "Start/Reset StopWatch"),
                textOutput("current_time_display"),
         
                # Continuously updating elapsed time output
                textOutput("elapsed_time_display"),
                 
                h5("Goal for Today:"),
                textOutput("day_goal_display", inline = TRUE)
              )
            )
          ),
          box(
            title = "Box 2", width = 6,
            
            # Date input
            dateInput("study_date", "Date:", value = Sys.Date()),
            
            # Dynamic study duration input
            uiOutput("dynamic_study_duration"),
            
            # Text input for study notes
            textInput("study_notes", "Notes:"),
            
            # Numeric input for Anki card numbers
            numericInput("anki_card_number", "Anki Card Number:", value = 0, min = 0),
            
            # Checkbox input for GitHub check
            checkboxInput("github_check", "Update Github", value = FALSE),
            
            # Checkbox input for LinkedIn check
            checkboxInput("linkedin_check", "Post on LinkedIn", value = FALSE),
            
            # Dropdown list input for program with option to add new values
            textInput("program_input", "Program:"),
            
            # Dropdown list input for status with option to add new values
            selectizeInput("status_input", "Status:",
                          choices = c("unrelated full time job", "student"), 
                          options = list(create = TRUE)),
            
            # Submit button for Box 2
            actionButton(inputId = "submit_full_entry", label = "Submit Full Entry")
          )
        ),
        fluidRow(
          box(
            title = "Study Buffer Table", width = 12,
            DT::dataTableOutput("react_study_buffer")
          )
        )
      ),
      ### TAB FOR STRENGTH Data view
      tabItem(
        tabName = "view_strength",
        fluidRow(
          column(width = 6,
                valueBoxOutput("total_pushup_box_view", width = NULL)
          ),
          column(width = 6,
                valueBoxOutput("total_situp_box_view", width = NULL)
          )
        ),
        fluidRow(
          box(
            dateRangeInput(
              inputId = "strength_date_range",
              label = "Select Date Range",
              start = Sys.Date() - 100, # Default start date (last 100 days)
              end = Sys.Date(),        # Default end date (today)
              format = "yyyy-mm-dd"
            ),
            width = 12,
            title = "Filter Date Range"
          )
        ),
        fluidRow(
          box(
            plotOutput("pushup_line_chart"),
            width = 6,
            title = "Pushup Chart"
          ),
          box(
            plotOutput("situp_line_chart"),
            width = 6,
            title = "Situp Chart"
          )
        )
      ),
      ### TAB FOR STRENGTH ENTRY
      tabItem(
        tabName = "strength_entry",
        fluidRow(
          valueBoxOutput("today_pushup_box", width = 3),
          valueBoxOutput("today_situp_box", width = 3)
        ),
        fluidRow(
          box(
            title = "Pushups",
            width = 3,
            dateInput("exercise_date", "Date",
              value = Sys.Date()
            ),
            numericInput("exercise_count",
              "Number of Reps",
              value = 10, min = 0
            ),
            selectizeInput("type_input", "Type:", choices = c("Chair_18", "Regular")),
            actionButton("submit_pushups", "Submit")
          ),
          box(
            title = "Situps",
            width = 3,
            dateInput("exercise_date", "Date",
                      value = Sys.Date()
            ),
            numericInput("exercise_count",
                         "Number of Reps",
                         value = 10, min = 0
            ),
            actionButton("submit_situps", "Submit")
          )
        )
      ),
      ### TAB TO VIEW FULL RELATIONSHIP DATA
      tabItem(
        tabName = "view_Rdata",
        fluidRow(
          title = "View Person Data",
          width = 12,
          DT::dataTableOutput("person_table")
        )
      ),
      ### TAB TO VIEW RELATIONSHIP NOTES LOG 
      tabItem(
        tabName = "view_log",
        fluidRow(
          title = "View Notes Log",
          width = 12,
          DT::dataTableOutput("notes_log")
        )
      ),
      ### TAB FOR RELATIONSHIP PERSON ENTRY
      tabItem(
        tabName = "entry_relationship",
        fluidRow(
          box(
            title = "Add New Contact",
            width = 6,
            textInput("name", "Name", placeholder = "Enter name"),
            textInput("relationship", "Relationship", placeholder = "Enter relationship"),
            dateInput("date_last_connected", "Date Last Connected"),
            numericInput("contact_after_x_days", "Contact After (days)", value = 30, min = 1),
            textInput("phone_number", "Phone Number", placeholder = "Enter phone number"),
            textInput("email", "Email", placeholder = "Enter email address"),
            actionButton("submit_person", "Add Contact")
          )
        )
      ),
      ### TAB FOR RELATIONSHIP NOTE ENTRY
      tabItem(
        tabName = "entry_relationship_note",
        fluidRow(
          box(
            title = "Add New Note",
            width = 6,
            selectizeInput("person", "Person:", choices = NULL),
            dateInput("date", "Date"),
            textAreaInput("note", "Note", placeholder = "Note here", height = "80px"),
            actionButton("submit_relationship_note", "Add Note")
          )
        )
      )
    )
  )
)

source("server.R")

shinyApp(ui, server)