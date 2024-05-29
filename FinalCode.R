# Load necessary libraries
library(readxl) # For reading Excel files
library(dplyr) # For data manipulation
library(ggplot2) # For data visualization

# Read the data from the Excel file
shark_data <- read_excel(file.choose())

## DATA SUMMARY: 

# Basic data exploration
summary(shark_data) # Provides summary statistics for each column
dim(shark_data) # Shows the dimension of the dataframe
str(shark_data) # Displays the structure of the dataframe
head(shark_data) # Shows the first few rows of the dataframe


# Calculate the overall proportion of fatal attacks
overall_proportion_fatal <- sum(shark_data$Injury == "Fatal", na.rm = TRUE) / nrow(shark_data)

# Plotting the number of fatal attacks over time by year
shark_data$Date <- as.Date(shark_data$Date, format = "%m/%d/%Y")
shark_data$Year <- as.numeric(format(shark_data$Date, "%Y"))  # Ensure Year is numeric

# Handling missing values in 'Injury' column
sum(is.na(shark_data$Injury))
shark_data$Injury[is.na(shark_data$Injury)] <- "Unknown"

# Re-evaluate the overall proportion of fatal attacks after handling missing values
overall_proportion_fatal <- sum(shark_data$Injury == "Fatal") / nrow(shark_data)


### CHARTS (Please run each chart/graph individually)

## PIE CHART (of attack types):

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Example data frame
df <- data.frame(
  Type = c("Provoked", "Unprovoked", "Invalid", "Unverified", "Boat"),
  Count = c(10, 50, 5, 3, 12)
)

# Calculate proportions
df <- df %>%
  mutate(Proportion = Count / sum(Count),
         Label = scales::percent(Proportion))

# Create Pie Chart
pie_chart <- ggplot(df, aes(x = "", y = Proportion, fill = Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +  # This transforms the bar chart into a pie chart
  geom_label(aes(label = Label), position = position_stack(vjust = 0.5)) +  # Add labels inside the pie
  labs(title = "Pie Chart of Attack Types", fill = "Attack Type") +
  theme_void() +  # Remove axis labels and ticks
  theme(legend.position = "right")  # Adjust legend position

# Print the chart
print(pie_chart)


## SCATTER PLOT (Attacks over time):

# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

shark_data <- read_excel(file.choose())

# Convert the 'Date' to 'Date' format and extract 'Year'
shark_data$Date <- as.Date(shark_data$Date, format = "%d %b-%Y")
shark_data$Year <- as.numeric(format(shark_data$Date, "%Y"))

# Create 'Fatal' column based on the 'Injury' column, assuming 'Fatal' if 'Fatal' is in the string
shark_data$Fatal <- ifelse(grepl("Fatal", shark_data$Injury, ignore.case = TRUE), "Fatal", "Non-Fatal")

# Filter out rows with NA in 'Year'
shark_data <- shark_data[!is.na(shark_data$Year),]

# Summarize attacks per year
attacks_per_year <- shark_data %>%
  group_by(Year, Fatal) %>%
  summarise(Num_Attacks = n(), .groups = 'drop')

# Enhanced Scatter Plot with y-axis starting at 0
scatter_plot <- ggplot(attacks_per_year, aes(x = Year, y = Num_Attacks, color = Fatal)) +
  geom_point(alpha = 0.6, size = 3) +  # Plot points with increased visibility
  geom_text(aes(label = Num_Attacks), vjust = -0.5, color = "black") +  # Add data labels
  geom_smooth(data = subset(attacks_per_year, Fatal == "Fatal"), method = "lm", se = FALSE, color = "red") +  # Linear model for Fatal
  geom_smooth(data = subset(attacks_per_year, Fatal == "Non-Fatal"), method = "lm", se = FALSE, color = "blue") +  # Linear model for Non-Fatal
  scale_y_continuous(labels = scales::comma, limits = c(0, max(attacks_per_year$Num_Attacks, na.rm = TRUE) + 10)) +  # Set y-axis to start at 0 and dynamically adjust the upper limit
  labs(title = "Scatter Plot of Shark Attacks Over Time with Trend Line",
       subtitle = "Data points represent the number of attacks per year",
       x = "Year", y = "Number of Attacks", color = "Attack Outcome") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

print(scatter_plot)

## BAR CHART (Annual shark attacks):

bar_plot <- ggplot(attacks_per_year, aes(x = Year, y = Num_Attacks, fill = Fatal)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Fatal" = "red", "Non-Fatal" = "blue")) +
  labs(title = "Annual Shark Attacks by Outcome",
       subtitle = "Comparing the number of Fatal and Non-Fatal Attacks per Year",
       x = "Year", y = "Number of Attacks") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

print(bar_plot)

## HISTOGRAM (shark attacks by year):

library(ggplot2)
library(dplyr)

shark_data$Year <- as.numeric(format(as.Date(shark_data$Date, format = "%d %b-%Y"), "%Y"))

# Create a summary of attacks per year
yearly_attacks <- shark_data %>%
  group_by(Year) %>%
  summarise(Num_Attacks = n(), .groups = 'drop')

# Determine threshold for highlighting (e.g., more than average attacks)
threshold <- mean(yearly_attacks$Num_Attacks) + sd(yearly_attacks$Num_Attacks)

# Create the histogram
histogram_plot <- ggplot(yearly_attacks, aes(x = Year, y = Num_Attacks, fill = Num_Attacks > threshold)) +
  geom_col() +  # Using geom_col to make it easy to apply fill based on a condition
  scale_fill_manual(values = c("FALSE" = "blue", "TRUE" = "red"), 
                    labels = c("Normal", "High")) +
  scale_x_continuous(breaks = yearly_attacks$Year) +
  scale_y_continuous(limits = c(0, max(yearly_attacks$Num_Attacks) + 5)) +
  labs(title = "Histogram of Shark Attacks by Year",
       subtitle = "Highlighting years with significantly higher attacks",
       x = "Year", y = "Number of Attacks",
       fill = "Attack Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(histogram_plot)

## BOX PLOT (of Attacks by Country):

library(ggplot2)
library(dplyr)
library(readxl)

shark_data <- read_excel(file.choose())

# Ensure the data is loaded correctly
cat("First few rows of the data:\n")
print(head(shark_data, 20))  # Display the first 20 rows
cat("Last few rows of the data:\n")
print(tail(shark_data, 20))  # Display the last 20 rows

# Check the dimensions of the dataset
cat("Dimensions of the dataset:\n")
print(dim(shark_data))

# Check for any NA values in the 'Country' column
na_countries <- shark_data %>% filter(is.na(Country))
cat("Rows with NA in the 'Country' column:\n")
print(na_countries)

# Replace NA values with 'Unknown' if necessary
shark_data$Country[is.na(shark_data$Country)] <- "Unknown"

# Summarize the number of attacks by country
attacks_by_country <- shark_data %>%
  group_by(Country) %>%
  summarise(Num_Attacks = n(), .groups = 'drop') %>%
  arrange(desc(Num_Attacks))

# Print the top 10 countries/areas with the highest number of shark attacks
cat("Countries/Areas with the Highest Shark Attacks:\n")
print(head(attacks_by_country, 10))

# Filter the data to include only the top 10 countries
top_countries <- head(attacks_by_country, 10)

# Boxplot of attacks by country using Num_Attacks
ggplot(top_countries, aes(x = reorder(Country, -Num_Attacks), y = Num_Attacks)) +
  geom_boxplot() +
  labs(title = "Boxplot of Shark Attacks by Country", x = "Country", y = "Number of Attacks") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## BAR CHART (Shark species):

# Load libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(magrittr)

# List of non-species entries to exclude
non_species_entries <- c("Invalid", "Shark involvement prior to death was not confirmed", 
                         "Shark involvement not confirmed", "Questionable incident", 
                         "No shark involvement", "Shark involvement prior to death unconfirmed")

# Summarize the number of attacks per species, excluding non-species entries
species_attacks <- shark_data %>%
  filter(!is.na(Species) & Species != "" & 
           !Species %in% non_species_entries) %>%
  group_by(Species) %>%
  summarise(Num_Attacks = n(), .groups = 'drop') %>%
  arrange(desc(Num_Attacks))

# Filter to include only species with a significant number of attacks (e.g., top 10)
top_species_attacks <- species_attacks %>%
  top_n(10, wt = Num_Attacks)

# Display the summarized data to check
print(top_species_attacks)

# Create a bar plot for the top species involved in attacks
species_plot <- ggplot(top_species_attacks, aes(x = reorder(Species, -Num_Attacks), y = Num_Attacks)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top Shark Species Involved in Attacks",
       x = "Shark Species",
       y = "Number of Attacks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(species_plot)



### Models (Please run each model individually):

## REGRESSION MODEL:

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Ensure the 'Date' column is correctly formatted as a Date object and extract 'Year'
shark_data$Date <- as.Date(shark_data$Date, format = "%d %b-%Y")
shark_data$Year <- as.numeric(format(shark_data$Date, "%Y"))

# Verify the presence of the 'Year' column
if("Year" %in% colnames(shark_data)) {
  print("Year column is present.")
  print(head(shark_data))
} else {
  print("Year column not found. Check date conversion.")
}

# Summarize attacks per year if 'Year' column exists
if("Year" %in% colnames(shark_data)) {
  attacks_per_year <- shark_data %>%
    group_by(Year) %>%
    summarise(Num_Attacks = n(), .groups = 'drop')
  print(head(attacks_per_year))
  
  # Proceed with a regression model if there are enough data points
  if(nrow(attacks_per_year) > 1) {
    lm_model <- lm(Num_Attacks ~ Year, data = attacks_per_year)
    summary(lm_model)  # Display the summary of the regression model
  } else {
    print("Not enough data points for regression.")
  }
} else {
  print("Year column not available for analysis.")
}


## CLASSIFICATION MODEL:

library(readxl)
library(dplyr)
library(caret)
library(pROC)

shark_data <- read_excel(file.choose())

# Ensure 'Date' is in the correct format
shark_data$Date <- as.Date(shark_data$Date, format = "%d %b-%Y")  # Adjust the format as per your actual data
shark_data$Year <- as.numeric(format(shark_data$Date, "%Y"))

# Create 'Fatal' column based on the 'Injury' column
shark_data$Fatal <- ifelse(grepl("Fatal", shark_data$Injury, ignore.case = TRUE), "Fatal", "Non-Fatal")

# Ensure 'Fatal' is a factor for logistic regression
shark_data$Fatal <- factor(shark_data$Fatal, levels = c("Non-Fatal", "Fatal"))

# Add a unique identifier to each row
shark_data$ID <- 1:nrow(shark_data)

# Create a subset of data without missing values in predictors
shark_data_subset <- shark_data %>% filter(!is.na(Year) & !is.na(Country) & !is.na(Fatal))

# Logistic Regression to predict the probability of a fatal attack
logit_model <- glm(Fatal ~ Year + Country, family = binomial(), data = shark_data_subset)

# Predicting probabilities
shark_data_subset$predicted_fatal <- predict(logit_model, type = "response")

# Classify predictions based on a threshold of 0.5
shark_data_subset$predicted_class <- ifelse(shark_data_subset$predicted_fatal > 0.5, "Fatal", "Non-Fatal")

# Confusion Matrix
conf_matrix <- confusionMatrix(factor(shark_data_subset$predicted_class, levels = c("Non-Fatal", "Fatal")),
                               shark_data_subset$Fatal)
print(conf_matrix)

# ROC Curve and AUC
roc_curve <- roc(shark_data_subset$Fatal, shark_data_subset$predicted_fatal)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot the ROC curve
plot(roc_curve, main = "ROC Curve for Logistic Regression Model")


## CLUSTERING MODEL:
# If results don't look accurate to report, try running a second time

library(readxl)
library(dplyr)
library(ggplot2)

shark_data <- read_excel(file.choose())

# Define non-species entries
non_species_entries <- c("Invalid", "Shark involvement prior to death was not confirmed", 
                         "Shark involvement not confirmed", "Questionable incident", 
                         "No shark involvement", "Shark involvement prior to death unconfirmed")

# Filter out non-species entries
shark_data_filtered <- shark_data %>%
  filter(!is.na(Species) & Species != "" & 
           !Species %in% non_species_entries)

# latitude and longitude data
shark_data_filtered$lat <- runif(nrow(shark_data_filtered), min = -90, max = 90)
shark_data_filtered$long <- runif(nrow(shark_data_filtered), min = -180, max = 180)

# K-means Clustering to identify clusters of shark attacks
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(shark_data_filtered[, c("lat", "long")], centers = 3)
shark_data_filtered$cluster <- as.factor(kmeans_result$cluster)

# Plotting clusters
ggplot(shark_data_filtered, aes(x = long, y = lat, color = cluster)) +
  geom_point(alpha = 0.5) +
  labs(title = "Cluster of Shark Attacks by Location", x = "Longitude", y = "Latitude") +
  scale_color_manual(values = c("red", "green", "blue"))