library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(ggplot2)
library(plotly)
library(timevis)
library(stringr)
library(readxl)
library(shinyjs)
library(writexl)

Sys.setlocale("LC_ALL", "en_US.UTF-8")
options(encoding = "UTF-8")

# 1) Reading User Excel
excel_path <- "C:/Users/ersan/Desktop/R/companyusers.xlsx" # Write your excel path
df_users <- read_excel(excel_path)

# User Verification Function
login_user <- function(identity, password) {
  matched <- df_users %>%
    filter(
      (tolower(name) == tolower(identity) | tolower(email) == tolower(identity)) &
        password == password
    )
  if (nrow(matched) == 1) {
    return(as.list(matched[1, ]))
  } else {
    return(NULL)
  }
}

# 2) Task Table
tasks <- data.frame(
  id = character(),
  title = character(),
  description = character(),
  assigned_to = character(),
  assigned_by = character(),
  start = as.Date(character()),
  end = as.Date(character()),
  status = character(),
  priority = character(),  # Priority: Low / Medium / High
  archived = logical(),   
  stringsAsFactors = FALSE
)

# 3) Reactive Values
global_logged_in_user <- reactiveVal(NULL)
tasks_data <- reactiveVal(tasks)

# 4) Auxiliary Functions
create_task <- function(title, desc, start_date, end_date, assigned_to, assigned_by, priority) {
  new_id <- paste0("T", format(Sys.time(), "%Y%m%d%H%M%S"))
  data.frame(
    id = new_id,
    title = title,
    description = desc,
    assigned_to = assigned_to,
    assigned_by = assigned_by,
    start = as.Date(start_date),
    end = as.Date(end_date),
    status = "Not launched",  # Use lowercase "l" for consistency
    priority = priority,
    archived = FALSE,
    stringsAsFactors = FALSE
  )
}


update_task_status <- function(task_id, new_status) {
  df <- tasks_data()
  idx <- which(df$id == task_id)
  if (length(idx) == 1) {
    df[idx, "status"] <- new_status
    tasks_data(df)
  }
}

archive_task <- function(task_id, archive_flag = TRUE) {
  df <- tasks_data()
  idx <- which(df$id == task_id)
  if (length(idx) == 1) {
    df[idx, "archived"] <- archive_flag
    tasks_data(df)
  }
}

delete_task <- function(task_id) {
  df <- tasks_data()
  idx <- which(df$id == task_id)
  if (length(idx) == 1) {
    df <- df[-idx, ]
    tasks_data(df)
  }
}

get_all_employees <- function() {
  df_users %>%
    filter(tolower(role) == "employee") %>%
    split(.$ID) %>%
    lapply(as.list)
}

get_manager_employees <- function(manager_dept) {
  df_users %>%
    filter(tolower(role) == "employee", department == manager_dept) %>%
    split(.$ID) %>%
    lapply(as.list)
}

# 5) UI
header <- dashboardHeader(
  title = HTML("Task Management"),
  tags$li(class = "dropdown",
          style = "padding: 8px;",
          uiOutput("user_profile_ui"))
)

sidebar <- dashboardSidebar(
  tags$style(HTML("
    .sidebar-menu > li > a {
      padding: 12px 15px 12px 15px;  /* Space between menu items */
    }
    .sidebar-menu > li.active > a {
      background-color: #2c3b41 !important;  /* Active menu color */
      border-left: 4px solid #3c8dbc;  /* Left edge line */
    }
    .sidebar-menu > li:hover > a {
      background-color: #2c3b41;  /* Hover color */
      border-left: 4px solid #3c8dbc;  /* Left border line in Hover */
    }
    #logout_btn {
      border-radius: 4px;
      transition: all 0.3s;
    }
    #logout_btn:hover {
      background-color: #c9302c !important;  /* Darker red in Hover */
    }
  ")),
  sidebarMenu(id = "sidebar_tabs", 
              uiOutput("sidebar_ui"))
)

body <- dashboardBody(
  useShinyjs(),
  tabItems(
    # -------------------- Login Tab --------------------
    tabItem(
      tabName = "login_tab",
      h2("Log in"),
      textInput("login_identity", "Name or e-mail"),
      passwordInput("login_pass", "Password"),
      actionButton("login_btn", "Log in"),
      textOutput("login_out")
    ),
    # -------------------- Task Assignment Tab --------------------
    tabItem(
      tabName = "task_assign_tab",
      fluidRow(
        box(
          width = 12,  # Full width
          title = "Create New Task",
          status = "primary",
          solidHeader = TRUE,
          
          fluidRow(
            column(
              width = 6,
              textInput("task_title", "Task Title *", 
                        placeholder = "Enter the task title..."),
              textAreaInput("task_desc", "Task Description *", 
                            height = "150px",
                            placeholder = "Enter the task details...")
            ),
            column(
              width = 6,
              dateInput("task_start", "Start Date *", 
                        value = Sys.Date(),
                        min = Sys.Date()),
              dateInput("task_end", "Deadline *", 
                        value = Sys.Date() + 7,
                        min = Sys.Date()),
              selectInput("task_priority", "Priority *", 
                          choices = c("Low", "Medium", "High"), 
                          selected = "Medium"),
              uiOutput("assign_to_ui")
            )
          ),
          
          div(
            style = "text-align: center; margin-top: 20px;",
            actionButton("create_task_btn", "Create Task", 
                         class = "btn-lg btn-success",
                         icon = icon("plus"),
                         style = "width: 200px;")  # Button width
          )
        )
      ),
      
      # Note part
      div(
        class = "alert alert-info",
        style = "margin-top: 20px;",
        tags$b("Not:"), "* marked fields are required."
      )
    ),
    # -------------------- Task List Tab --------------------
    tabItem(
      tabName = "task_list_tab",
      fluidRow(
        box(
          width = 12,
          title = "Task Filtering",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          column(4, textInput("search_task", "Search Task", placeholder = "Title or description...")),
          column(4, selectInput("filter_priority", "Filter Priority",
                                choices = c("All", "Low", "Medium", "High"))),
          column(4, selectInput("filter_status", "Status Filter",
                                choices = c("All", "Not started", "Continues", "Completed")))
        )
      ),
      fluidRow(
        infoBoxOutput("total_tasks_box"),
        infoBoxOutput("completed_tasks_box")
      ),
      h2("Task List"),
      fluidRow(
        box(width = 12,
            style = "overflow-x: auto;",
            DTOutput("task_table"))
      ),
      uiOutput("download_tasks_ui")  # Task Excel for Manager/Admin
    ),
    # -------------------- Archived Tasks Tab --------------------
    tabItem(
      tabName = "archived_tasks_tab",
      h2("Archived Tasks"),
      fluidRow(
        box(width = 12,
            DTOutput("archived_task_table"))
      ),
      p("This tab only lists tasks in the archive.")
    ),
    # -------------------- Report Tab --------------------
    tabItem(
      tabName = "report_tab",
      h2("Task Reports"),
      
      fluidRow(
        box(
          width = 3,
          title = "Summary Statistics",
          solidHeader = TRUE,
          status = "primary",
          uiOutput("task_stats_summary")
        ),
        box(
          width = 9,
          dateRangeInput("report_date_range", "Date Range",
                         start = Sys.Date() - 30,
                         end = Sys.Date() + 30),
          actionButton("filter_report_btn", "Filter", 
                       class = "btn-primary")
        )
      ),
      
      fluidRow(
        box(
          width = 6,
          title = "Distribution of Task Status",
          status = "primary",
          solidHeader = TRUE,
          plotlyOutput("task_status_plot", height = "400px")
        ),
        box(
          width = 6,
          title = "Task Priority Distribution",
          status = "info",
          solidHeader = TRUE,
          plotlyOutput("task_priority_plot", height = "400px")
        )
      ),
      
      fluidRow(
        box(
          width = 6,
          title = "Person Based Distribution",
          status = "warning",
          solidHeader = TRUE,
          plotlyOutput("task_density_plot", height = "400px")
        ),
        box(
          width = 6,
          title = "Department Based Distribution",
          status = "success",
          solidHeader = TRUE,
          plotlyOutput("department_task_plot", height = "400px")
        )
      )
    ),
    # -------------------- Contact Information Tab --------------------
    tabItem(
      tabName = "contacts_tab",
      h2("Contact Information"),
      DTOutput("contacts_table")
    ),
    # -------------------- Admin Tab --------------------
    tabItem(
      tabName = "admin_tab",
      h2("Admin Panel"),
      p("This tab can only be accessed by users in the Admin role."),
      downloadButton("download_excel", "Download User Excel")
    ),
    # -------------------- Settings Tab --------------------
    tabItem(
      tabName = "settings_tab",
      h2("Settings"),
      box(
        width = 6,
        title = "Application Settings",
        status = "primary",
        solidHeader = TRUE,
        radioButtons("app_language", "Dil / Language",
                     choices = c("Türkçe" = "tr", "English" = "en"),
                     selected = "tr"),
        hr(),
        actionButton("save_settings", "Kaydet", 
                     class = "btn-success",
                     icon = icon("save"))
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body)

# 6) SERVER
server <- function(input, output, session) {
  
  user_role <- reactive({
    u <- global_logged_in_user()
    if (is.null(u)) return("Guest")
    return(u$role)
  })
  
  output$user_profile_ui <- renderUI({
    user <- global_logged_in_user()
    if (is.null(user)) {
      HTML("<span style='color:white'>Guest</span>")
    } else {
      HTML(paste0("<span style='color:white'>Welcome, ", user$name, " (", user$role, ")</span>"))
    }
  })
  
  # --- Sidebar Menu: Based By Role ---
  output$sidebar_ui <- renderUI({
    user <- global_logged_in_user()
    if (is.null(user)) {
      list(
        menuItem("Log in", tabName = "login_tab", icon = icon("sign-in-alt"))
      )
    } else {
      common_items <- list(
        menuItem("Task List", tabName = "task_list_tab", icon = icon("tasks")),
        menuItem("Report Tab", tabName = "report_tab", icon = icon("chart-line")),
        menuItem("Contact Information", tabName = "contacts_tab", icon = icon("users")),
        menuItem("Settings", tabName = "settings_tab", icon = icon("cog"))
      )
      if (tolower(user$role) %in% c("admin", "manager")) {
        extended_items <- list(
          menuItem("Task Assignment", tabName = "task_assign_tab", icon = icon("plus-circle")),
          menuItem("Archived Tasks", tabName = "archived_tasks_tab", icon = icon("box-archive"))
        )
        menu_list <- c(extended_items, common_items)
      } else {
        menu_list <- common_items
      }
      admin_item <- NULL
      if (tolower(user$role) == "admin") {
        admin_item <- list(menuItem("Admin Panel", tabName = "admin_tab", icon = icon("user-shield")))
      }
      menu_list <- c(menu_list, admin_item,
                     list(actionButton("logout_btn", "Log out", icon = icon("right-from-bracket"),
                                       style = "margin: 10px; width:80%; background-color: #d9534f; color: white;")))
      do.call(sidebarMenu, menu_list)
    }
  })
  
  # --- LOGIN ---
  observeEvent(input$login_btn, {
    user <- login_user(input$login_identity, input$login_pass)
    if (!is.null(user)) {
      global_logged_in_user(user)
      output$login_out <- renderText(
        paste("Login successful! Welcome,", user$name, user$surname, "Rol:", user$role)
      )
      updateTabItems(session, "sidebar_tabs", "task_list_tab")
    } else {
      output$login_out <- renderText("Entry failed!")
    }
  })
  
  # --- LOGOUT ---
  observeEvent(input$logout_btn, {
    global_logged_in_user(NULL)
    updateTabItems(session, "sidebar_tabs", "login_tab")
  })
  
  # --- To Whom We Assign for Task Assignment ---
  output$assign_to_ui <- renderUI({
    req(tolower(user_role()) %in% c("admin", "manager"))
    current_user <- global_logged_in_user()
    if (tolower(current_user$role) == "admin") {
      emps <- get_all_employees()
    } else {
      manager_dept <- current_user$department
      emps <- get_manager_employees(manager_dept)
    }
    choice_vals <- sapply(emps, function(e) {e$name})
    names(choice_vals) <- choice_vals
    selectInput("assign_to_select", "To Whom is the Task?", choices = choice_vals)
  })
  
  observeEvent(input$create_task_btn, {
    req(tolower(user_role()) %in% c("admin", "manager"))
    
    # Validate required fields
    if (is.null(input$task_title) || input$task_title == "" ||
        is.null(input$task_desc) || input$task_desc == "" ||
        is.null(input$assign_to_select)) {
      showNotification("Please fill in all required fields", type = "error")
      return()
    }
    
    # Validate dates
    if (input$task_end < input$task_start) {
      showNotification("End date cannot be before start date", type = "error")
      return()
    }
    
    assigned_by <- global_logged_in_user()$name
    assigned_to <- input$assign_to_select
    
    new_task <- create_task(
      title = input$task_title,
      desc = input$task_desc,
      start_date = input$task_start,
      end_date = input$task_end,
      assigned_to = assigned_to,
      assigned_by = assigned_by,
      priority = input$task_priority
    )
    
    tasks_data(rbind(tasks_data(), new_task))
    
    # Clear inputs after successful creation
    updateTextInput(session, "task_title", value = "")
    updateTextAreaInput(session, "task_desc", value = "")
    updateDateInput(session, "task_start", value = Sys.Date())
    updateDateInput(session, "task_end", value = Sys.Date() + 7)
    updateSelectInput(session, "task_priority", selected = "Medium")
    
    showNotification(paste("Task created successfully:", new_task$title), type = "success")
    
    # Refresh the page
    shinyjs::runjs("$('.modal-backdrop').remove(); setTimeout(function(){ location.reload(); }, 1000);")
  })
  
  
  # --- InfoBoxes ---
  output$total_tasks_box <- renderInfoBox({
    df <- tasks_data()
    infoBox("Total Task", sum(!df$archived), icon = icon("clipboard-list"), color = "blue")
  })
  
  output$completed_tasks_box <- renderInfoBox({
    df <- tasks_data()
    infoBox("Completed Task", sum(df$status == "Completed" & !df$archived), 
            icon = icon("check-circle"), color = "green")
  })
  
  # --- TASK LIST (Not in the archive) ---
  output$task_table <- renderDT({
    user <- global_logged_in_user()
    if (is.null(user)) {
      return(datatable(data.frame(Uyari = "Please log in!")))
    }
    df <- tasks_data()
    df <- df[!df$archived, ]
    
    if (tolower(user$role) == "employee") {
      df <- df[df$assigned_to == user$name, ]
    } else if (tolower(user$role) == "manager") {
      df <- df[df$assigned_by == user$name | df$assigned_to == user$name, ]
    }
    
    # Apply search filter
    if (!is.null(input$search_task) && input$search_task != "") {
      df <- df %>%
        filter(
          stringr::str_detect(tolower(title), tolower(input$search_task)) |
            stringr::str_detect(tolower(description), tolower(input$search_task))
        )
    }
    
    # Apply status filter
    if (!is.null(input$filter_status) && input$filter_status != "All") {
      df <- df %>%
        filter(status == input$filter_status)
    }
    
    # Apply priority filter
    if (!is.null(input$filter_priority) && input$filter_priority != "All") {
      df <- df %>%
        filter(priority == input$filter_priority)
    }
    
    df$Aksiyonlar <- ifelse(
      tolower(df$status) == "not launched",
      sprintf(paste0(
        '<button class="btn btn-info btn-sm start-task" data-task-id="%s">Start</button> ',
        '<button class="btn btn-success btn-sm complete-task" data-task-id="%s">Complete</button> ',
        '<button class="btn btn-warning btn-sm archive-task" data-task-id="%s">Archive</button>'
      ), df$id, df$id, df$id),
      ifelse(tolower(df$status) == "continues",
             sprintf(paste0(
               '<button class="btn btn-success btn-sm complete-task" data-task-id="%s">Complete</button> ',
               '<button class="btn btn-warning btn-sm archive-task" data-task-id="%s">Archive</button>'
             ), df$id, df$id),
             sprintf('<button class="btn btn-warning btn-sm archive-task" data-task-id="%s">Archive</button>', df$id)
      )
    )
    df_display <- df %>% 
      select(id, title, description, assigned_to, assigned_by, priority, start, end, status, Aksiyonlar)
    
    datatable(
      df_display,
      escape = FALSE,
      selection = "none",
      options = list(
        pageLength = 5,
        autoWidth = TRUE,
        order = list(list(5, 'asc')),
        language = list(
          search = "Search:",
          lengthMenu = "Show _MENU_ record",
          info = "Showing _START_ to _END_ from _TOTAL_ record",
          paginate = list(
            previous = "Previous",
            `next` = "Next"
          )
        )
      )
    ) %>%
      formatStyle(
        'priority',
        backgroundColor = styleEqual(
          c("High", "Medium", "Low"),
          c('#ffcccc', '#ffffcc', '#ccffcc')
        )
      ) %>%
      formatStyle(
        'status',
        backgroundColor = styleEqual(
          c("Not launched", "Continues", "Completed"),
          c('#f8d7da', '#fff3cd', '#d4edda')
        )
      )
  }, server = TRUE)
  
  # JavaScript for Start button
  observe({
    shinyjs::runjs('
      $(document).off("click", ".start-task").on("click", ".start-task", function() {
        var taskId = $(this).data("task-id");
        Shiny.setInputValue("start_task", taskId, {priority: "event"});
      });
    ')
  })
  
  # Start button handler
  observeEvent(input$start_task, {
    req(input$start_task)
    update_task_status(input$start_task, "Continues")
    showNotification("Task launched!", type = "message")
    
    # Update the table now
    df <- tasks_data()
    tasks_data(df)  # Trigger reactive value
  })
  
  # JavaScript for the Complete button
  observe({
    shinyjs::runjs('
      $(document).off("click", ".complete-task").on("click", ".complete-task", function() {
        var taskId = $(this).data("task-id");
        Shiny.setInputValue("complete_task", taskId, {priority: "event"});
        // Refresh the page
        location.reload();
      });
    ')
  })
  
  # Complete button handler
  observeEvent(input$complete_task, {
    req(input$complete_task)
    update_task_status(input$complete_task, "Completed")
    showNotification("Task successfully completed!", type = "success")
  })
  
  observe({
    shinyjs::runjs('
      $(document).on("click", ".archive-task", function() {
        var taskId = $(this).data("task-id");
        Shiny.setInputValue("archive_task", taskId);
      });
    ')
  })
  
  observeEvent(input$archive_task, {
    req(input$archive_task)
    archive_task(input$archive_task, TRUE)
    showNotification("Task moved to the archive!", type = "warning")
  })
  
  # --- ARCHIVED TASKS ---
  output$archived_task_table <- renderDT({
    user <- global_logged_in_user()
    if (is.null(user)) {
      return(datatable(data.frame(Uyari = "Please log in!")))
    }
    df <- tasks_data()
    df <- df[df$archived == TRUE, ]
    
    if (tolower(user$role) == "manager") {
      df <- df[df$assigned_by == user$name | df$assigned_to == user$name, ]
    }
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mesaj = "There is no task in the archive.")))
    }
    
    df$Aksiyonlar <- paste0(
      sprintf('<button class="btn btn-primary btn-sm restore-task" data-task-id="%s">Undo</button>', df$id),
      " ",
      sprintf('<button class="btn btn-danger btn-sm delete-task" data-task-id="%s">Delete</button>', df$id)
    )
    
    df_display <- df %>%
      select(id, title, description, assigned_to, assigned_by, priority, start, end, status, Aksiyonlar)
    
    datatable(
      df_display,
      escape = FALSE,
      selection = "none",
      options = list(pageLength = 5, autoWidth = TRUE)
    )
  }, server = TRUE)
  
  observe({
    shinyjs::runjs('
      $(document).on("click", ".restore-task", function() {
        var taskId = $(this).data("task-id");
        Shiny.setInputValue("restore_task", taskId);
      });
    ')
  })
  
  observe({
    shinyjs::runjs('
      $(document).on("click", ".delete-task", function() {
        var taskId = $(this).data("task-id");
        Shiny.setInputValue("delete_task", taskId);
      });
    ')
  })
  
  observeEvent(input$restore_task, {
    req(input$restore_task)
    archive_task(input$restore_task, FALSE)
    showNotification("Task retrieved from the archive!", type = "message")
  })
  
  observeEvent(input$delete_task, {
    req(input$delete_task)
    delete_task(input$delete_task)
    showNotification("Task permanently deleted!", type = "error")
  })
  
  # --- REPORTS ---
  # Status Distribution
  output$task_status_plot <- renderPlotly({
    req(input$filter_report_btn)  # Trigger when the Filter button is pressed
    df <- tasks_data()
    dr <- input$report_date_range
    df_filtered <- df %>%
      filter(!archived) %>%
      filter((start >= dr[1] & start <= dr[2]) | (end >= dr[1] & end <= dr[2]))
    
    status_count <- df_filtered %>%
      group_by(status) %>%
      summarise(count = n())
    
    p <- ggplot(status_count, aes(x = status, y = count, fill = status)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Distribution of Position Status", x = "Status", y = "Quantity")
    
    ggplotly(p)
  })
  
  # Priority Distribution
  output$task_priority_plot <- renderPlotly({
    req(input$filter_report_btn)
    df <- tasks_data()
    dr <- input$report_date_range
    df_filtered <- df %>%
      filter(!archived) %>%
      filter((start >= dr[1] & start <= dr[2]) | (end >= dr[1] & end <= dr[2]))
    
    priority_count <- df_filtered %>%
      group_by(priority) %>%
      summarise(count = n())
    
    p2 <- ggplot(priority_count, aes(x = priority, y = count, fill = priority)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Task Priority Distribution", x = "Priority", y = "Quantity")
    
    ggplotly(p2)
  })
  
  # Person-based task distribution graph
  output$task_density_plot <- renderPlotly({
    req(input$filter_report_btn)
    df <- tasks_data()
    dr <- input$report_date_range
    df_filtered <- df %>%
      filter(!archived) %>%
      filter((start >= dr[1] & start <= dr[2]) | (end >= dr[1] & end <= dr[2]))
    
    person_tasks <- df_filtered %>%
      group_by(assigned_to) %>%
      summarise(
        `Not launched` = sum(status == "Not launched"),
        `Continue` = sum(status == "Continues"),
        Completed = sum(status == "Completed")
      ) %>%
      tidyr::pivot_longer(
        cols = c(`Not launched`, `Continues`, Completed),
        names_to = "status",
        values_to = "count"
      )
    
    p <- ggplot(person_tasks, aes(x = assigned_to, y = count, fill = status)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Person Based Task Breakdown",
           x = "Person",
           y = "Task Number",
           fill = "Status")
    
    ggplotly(p)
  })
  
  # Department-based task distribution chart
  output$department_task_plot <- renderPlotly({
    req(input$filter_report_btn)
    df <- tasks_data()
    dr <- input$report_date_range
    
    # Combine user information with tasks
    df_with_dept <- df %>%
      filter(!archived) %>%
      filter((start >= dr[1] & start <= dr[2]) | (end >= dr[1] & end <= dr[2])) %>%
      left_join(df_users %>% select(name, department), by = c("assigned_to" = "name"))
    
    dept_tasks <- df_with_dept %>%
      group_by(department) %>%
      summarise(
        `Not launched` = sum(status == "Not launched"),
        `Continues` = sum(status == "Continues"),
        Completed = sum(status == "Completed")
      ) %>%
      tidyr::pivot_longer(
        cols = c(`Not launched`, `Continues`, Completed),
        names_to = "status",
        values_to = "count"
      )
    
    p <- ggplot(dept_tasks, aes(x = department, y = count, fill = status)) +
      geom_bar(stat = "identity", position = "stack") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Departmental Breakdown of Tasks",
           x = "Department",
           y = "Number of Tasks",
           fill = "Status")
    
    ggplotly(p)
  })
  
  # --- Contact Information ---
  output$contacts_table <- renderDT({
    if (is.null(global_logged_in_user())) {
      return(datatable(data.frame(Uyari = "Please log in!")))
    }
    df_users %>%
      select(ID, name, surname, department, phone, email, role) %>%
      datatable(options = list(pageLength = 5))
  })
  
  # --- Admin Panel User Excel Download ---
  output$download_excel <- downloadHandler(
    filename = function() {
      "users.xlsx"
    },
    content = function(file) {
      file.copy(excel_path, file)
    }
  )
  
  # --- Download Task Excel (Manager or Admin) ---
  output$download_tasks_ui <- renderUI({
    user <- global_logged_in_user()
    if (!is.null(user) && tolower(user$role) %in% c("admin", "manager")) {
      downloadButton("download_tasks", "Download Task Excel")
    }
  })
  
  output$download_tasks <- downloadHandler(
    filename = function() {
      "tasks.xlsx"
    },
    content = function(file) {
      user <- global_logged_in_user()
      req(user)
      df <- tasks_data()
      
      if (tolower(user$role) == "manager") {
        manager_dept <- user$department
        df_joined <- df %>%
          left_join(df_users, by = c("assigned_to" = "name"))
        df_filtered <- df_joined %>%
          filter(department == manager_dept)
        
        final_df <- data.frame(
          "Task ID"            = df_filtered$id,
          "Task Name"           = df_filtered$title,
          "Task Text"         = df_filtered$description,
          "Task Owner"      = df_filtered$assigned_to,
          "Task Assignor"       = df_filtered$assigned_by,
          "Priority"             = df_filtered$priority,
          "Archived?"         = df_filtered$archived,
          "Task Time Range" = paste(df_filtered$start, df_filtered$end, sep = " - "),
          "Status"               = df_filtered$status,
          stringsAsFactors = FALSE
        )
      } else {
        final_df <- data.frame(
          "Task ID"            = df$id,
          "Task Name"           = df$title,
          "Task Text"         = df$description,
          "Task Owner"      = df$assigned_to,
          "Task Assignor"       = df$assigned_by,
          "Priority"             = df$priority,
          "Archived?"         = df$archived,
          "Task Time Range" = paste(df$start, df$end, sep = " - "),
          "Status"               = df$status,
          stringsAsFactors = FALSE
        )
      }
      
      write_xlsx(final_df, path = file)
    }
  )
  
  # New UI output for summary statistics
  output$task_stats_summary <- renderUI({
    df <- tasks_data()
    dr <- input$report_date_range
    df_filtered <- df %>%
      filter(!archived) %>%
      filter((start >= dr[1] & start <= dr[2]) | (end >= dr[1] & end <= dr[2]))
    
    total_tasks <- nrow(df_filtered)
    completed_tasks <- sum(df_filtered$status == "Completed")
    ongoing_tasks <- sum(df_filtered$status == "Continues")
    not_started <- sum(df_filtered$status == "Not launched")
    high_priority <- sum(df_filtered$priority == "High")
    
    fluidRow(
      valueBox(
        total_tasks,
        "Total Task",
        icon = icon("tasks"),
        color = "purple",
        width = 6
      ),
      valueBox(
        completed_tasks,
        "Completed",
        icon = icon("check"),
        color = "green",
        width = 6
      ),
      valueBox(
        ongoing_tasks,
        "Continuing",
        icon = icon("spinner", class = "fa-spin"),
        color = "yellow",
        width = 6
      ),
      valueBox(
        high_priority,
        "High Priority",
        icon = icon("exclamation-triangle"),
        color = "red",
        width = 6
      )
    )
  })
  
  # Manage language change on the server side
  app_language <- reactiveVal("tr")
  
  observeEvent(input$save_settings, {
    app_language(input$app_language)
    showNotification(
      if(input$app_language == "tr") "Dil ayarları kaydedildi!" else "Language settings saved!",
      type = "success"
    )
  })
  
  # Translation helper function
  translate <- function(key) {
    lang <- app_language()
    translations[[lang]][[key]]
  }
}

# 7) Launch Shiny App
shinyApp(ui, server)