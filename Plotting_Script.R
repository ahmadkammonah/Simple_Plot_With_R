#Load Libraries
pacman::p_load(pacman, dplyr, ggplot2, plotly, readr, lubridate) 

#Set Working Directory
setwd("C:/Users/user/Desktop/Actuator Testing Raw Data")

# Read the CSV file and store it as a data frame named 'data'
data <- read_csv("unit0002-2023-12-30-newsensor3.csv")

#Rename Columns 
colnames(data) <- c('CycleNumber', 'Time', 'Angle', 'Voltage', 'StatusBit', 'ErrorBit' )

# Only Keep first 50 cycles
#data <- subset(data, CycleNumber <= 20)

# Perform data manipulation
# data <- data %>%
#   mutate(Subset = as.integer((`Cycle Num` - 1) / 1) + 1)

# Add Date and Time format
data$Time <- as_datetime(data$Time, origin = "2023-12-29")

# Identify the rows at the desired interval
data <- data %>%
  filter(row_number() %% 10 == 0)

# Detect anomalies based on the threshold
anomaly_threshold <- 3  # Adjust this threshold based on your specific case
data$Anomaly <- ifelse(is.na(lag(data$Angle)) | abs(data$Angle - lag(data$Angle)) > anomaly_threshold, 1, 0)
data[1, "Anomaly"] <- 0

# Plot Time vs. Angle for each subset of 10 cycles
p <- ggplot(data, aes(x = Time, y = Angle, color = factor(Anomaly))) +
  geom_line(aes(x=Time, y=Angle), color="black") +
  geom_point(data = subset(data, Anomaly == 1), color = "red", size = 1) +
  # facet_wrap(~ Subset, scales = "free_x") +
  scale_color_manual(values = c("0" = "black", "1" = "red")) +
  xlab("Time") +
  ylab("Angle") +
  ggtitle("Angle vs. Time") +
  theme_minimal()


peak_data <- data %>%
  group_by(CycleNumber) %>%
  filter(Angle == max(Angle))  # Assuming "Angle" is the variable for which you want to identify the peaks

p <- p +
  geom_text(data = peak_data, aes(label = CycleNumber, x = Time, y = Angle), nudge_y = 2)

# Make the plot interactive
p_interactive <- ggplotly(p)

#Add Scroll bar #1
p_interactive <- p_interactive %>%
  layout(xaxis = list(rangeslider = list()))

# Test
p_interactive

# Save to HTML file
htmlwidgets::saveWidget(p_interactive, "Sensor3_Interactive_Plot_Rev1.html")
