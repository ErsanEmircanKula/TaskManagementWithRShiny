# Task Management with R Shiny

This project is a task management application developed with R language and Shiny library. It provides a system where users can add, update, and complete tasks.

# Features

- User login system
- Excel-based user database
- Adding a new task
- Listing tasks
- Updating and completing tasks
- Tracking task statuses
- Data visualizations about task statuses
- Exporting data to Excel

# Installation and Use

1. *Make Sure You Have R and R Studio Installed*
   - To download R: [CRAN R](https://cran.r-project.org/)
   - To download R Studio: [RStudio (Posit)](https://posit.co/download/rstudio-desktop/)

2. *Install Required Packages* 
   The following R packages must be installed before running the project:
   ```r
   install.packages(c("shiny", "shinydashboard", "dplyr", "DT", "ggplot2", "plotly", 
                      "timevis", "stringr", "readxl", "shinyjs", "writexl"))