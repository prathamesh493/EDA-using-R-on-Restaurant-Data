# Load necessary libraries
library(shiny)      # For creating interactive web applications
library(ggplot2)    # For creating data visualizations
library(dplyr)      # For data manipulation
library(lubridate)  # For working with dates and times

# Load the dataset from a CSV file
data <- read.csv("/Users/prathameshnaik/Downloads/grouped_df.csv")

# Data preprocessing
data$Date <- as.Date(data$Date)  # Convert 'Date' column to Date format
data$DayOfWeek <- weekdays(data$Date)  # Extract day of the week from Date

# Order Days of Week from Monday to Sunday
data$DayOfWeek <- factor(data$DayOfWeek, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

data$Month <- month(data$Date, label = TRUE, abbr = TRUE)  # Extract month from Date

# Define the user interface (UI) for the Shiny app
ui <- fluidPage(
  titlePanel("Restaurant Sales Dashboard"),
  br(),
  tabsetPanel(
    # Create multiple tabs for different visualizations
    tabPanel("Sales by Hour", br(),
      plotOutput("sales_hourly")
    ),
    tabPanel("Sales by Day of Week", br(),
      plotOutput("sales_dayofweek")
    ),
    tabPanel("Monthly Sales Trend", br(),
      plotOutput("sales_monthly")
    ),
    tabPanel("Sales by Season", br(),
      plotOutput("sales_season")
    ),
    tabPanel("Sales by Category", br(),
      plotOutput("sales_category")
    ),
    tabPanel("Top 10 Standardized Items", br(),
      plotOutput("top_items")
    ),
    tabPanel("Orders by Day and Hour", br(),
      plotOutput("orders_day_hour")
    ),
    tabPanel("Weekday vs Weekend Sales", br(),
      plotOutput("weekday_vs_weekend")
    )
  )
)

# Define the server logic for the Shiny app
server <- function(input, output) {

  # Sales by Hour visualization
  output$sales_hourly <- renderPlot({
    data %>%
      group_by(TimeGroup) %>%  # Group data by time slots
      summarize(TotalSales = sum(Quantity)) %>%  # Calculate total sales for each time slot
      ggplot(aes(x = TimeGroup, y = TotalSales)) +
      geom_bar(stat = "identity", aes(fill = TotalSales), show.legend = FALSE) +  # Create bar chart
      labs(title = "Sales by Hour", x = "Time Slot", y = "Total Sales") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better visibility
  })

  # Sales by day of the week visualization
  output$sales_dayofweek <- renderPlot({
    data %>%
      group_by(DayOfWeek) %>%  # Group data by day of the week
      summarize(TotalSales = sum(Quantity)) %>%  # Calculate total sales for each day
      ggplot(aes(x = DayOfWeek, y = TotalSales)) +
      geom_bar(stat = "identity", fill = "darkgreen") +  # Create bar chart
      labs(title = "Sales by Day of Week", x = "Week Day", y = "Total Sales") +
      theme_minimal()
  })

  # Monthly sales trend visualization (line chart)
  output$sales_monthly <- renderPlot({
    data %>%
      group_by(Month) %>%  # Group data by month
      summarize(TotalSales = sum(Quantity)) %>%  # Calculate total sales for each month
      ggplot(aes(x = Month, y = TotalSales, group = 1)) +  # group = 1 ensures it's a single line
      geom_line(color = "purple", size = 1.2) +  # Create line chart
      geom_point(color = "purple", size = 3) +  # Add points for emphasis
      labs(title = "Monthly Sales Trend", x = "Month", y = "Total Sales") +
      theme_minimal()
  })

  # Sales by season visualization (pie chart)
  output$sales_season <- renderPlot({
    data %>%
      group_by(Season) %>%  # Group data by season
      summarize(TotalSales = sum(Quantity)) %>%  # Calculate total sales for each season
      ggplot(aes(x = "", y = TotalSales, fill = Season)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +  # Convert bar chart to pie chart
      labs(title = "Sales by Season") +
      theme_void()
  })

  # Sales by category visualization (descending order)
  output$sales_category <- renderPlot({
    data %>%
      group_by(Category) %>%  # Group data by category
      summarize(TotalSales = sum(Quantity)) %>%  # Calculate total sales for each category
      arrange(desc(TotalSales)) %>%  # Sort in descending order
      ggplot(aes(x = reorder(Category, TotalSales), y = TotalSales)) +
      geom_bar(stat = "identity", fill = "orange") +
      coord_flip() +  # Flip coordinates for horizontal bars
      labs(title = "Sales by Category", x = "Category", y = "Total Sales") +
      theme_minimal()
  })

  # Top 10 standardized items visualization (horizontal bar chart)
  output$top_items <- renderPlot({
    data %>%
      group_by(Standardized_Item) %>%  # Group data by standardized item
      summarize(Count = n()) %>%  # Count occurrences of each item
      arrange(desc(Count)) %>%  # Sort in descending order
      head(10) %>%  # Select top 10 items
      ggplot(aes(x = reorder(Standardized_Item, Count), y = Count)) +
      geom_bar(stat = "identity", fill = "darkred") +
      coord_flip() +  # Flip coordinates for horizontal bars
      labs(title = "Top 10 Standardized Items", x = "Item", y = "Count") +
      theme_minimal()
  })

  # Orders by day of week and hour visualization (heatmap)
  output$orders_day_hour <- renderPlot({
    data %>%
      group_by(DayOfWeek, TimeGroup) %>%  # Group data by day of week and time group
      summarize(TotalOrders = sum(Quantity)) %>%  # Calculate total orders for each group
      ggplot(aes(x = TimeGroup, y = DayOfWeek, fill = TotalOrders)) +
      geom_tile(color = "white", size = 0.5) +  # Create heatmap tiles
      scale_fill_gradient(low = "#615CBE", high = "#AC6953", name = "Total Orders") +  # Color gradient
      labs(title = "Orders by Day of Week and Hour", x = "Hour", y = "Day of Week") +
      theme_minimal()
  })

  # Weekday vs Weekend Sales visualization
  output$weekday_vs_weekend <- renderPlot({
    data %>%
      mutate(IsWeekend = ifelse(DayOfWeek %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>%  # Create weekend/weekday category
      group_by(IsWeekend) %>%  # Group by weekend/weekday
      summarize(TotalSales = sum(Quantity)) %>%  # Calculate total sales for each group
      ggplot(aes(x = IsWeekend, y = TotalSales, fill = IsWeekend)) +
      geom_bar(stat = "identity") +  # Create bar chart
      labs(title = "Weekday vs Weekend Sales", x = "Type of Day", y = "Total Sales") +
      theme_minimal()
  })
}

# Run the Shiny application 
shinyApp(ui = ui, server = server)