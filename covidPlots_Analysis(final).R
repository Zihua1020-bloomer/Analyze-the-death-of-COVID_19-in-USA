# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # #  The story of COVID_19 IN USA # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Set the directory 
setwd("~/Library/CloudStorage/GoogleDrive-d.quinterona@student.uw.edu.pl/My Drive/UW/III-Semester/Visualization in R/Final Project/COVID-19_USA/data")

# install the packages
packages_to_install <- c("rmarkdown", "renv", "shiny", "ggplot2", "dplyr", 
                         "tidyr", "usmap", "zoo", "lubridate", "sf", "tmap",
                         "cowplot")

#install.packages(packages_to_install)
library(renv)
library(ggplot2)
library(dplyr)
library(tidyr)
library(usmap)
library(zoo)
library(lubridate)
library(sf) 
library(tmap) 
library(cowplot)

# read and observe the data
data <- read.csv("all-states-history (1).csv",sep=",")
print(summary(data))
str(data)

# data transformation
## change date into the date form, and state into the factor form
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$state <- as.factor(data$state)

# drop columns that are recommended not to use and with wrong data:
data <- data %>%
  select(-c("positiveScore", "totalTestResultsIncrease"))

# check the NAs
na_count_dplyr <- data %>% summarise_all(~sum(is.na(.)))
print(na_count_dplyr)

# The increase variables are without NAs, so we will use them to complete the missing values in the related variables
# In this way, everyday we'll use the closest to real data. 


# Part 1: Identify the oldest record per state with NA
# for death
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(death = ifelse(is.na(death) & row_number() == 1, 0, death)) %>%
  ungroup()

# for hospitalized
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(hospitalized = ifelse(is.na(hospitalized) & row_number() == 1, 
                               0, 
                               hospitalized)) %>%
  ungroup()
# for negative
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(negative = ifelse(is.na(negative) & row_number() == 1, 
                           0, 
                           negative)) %>%
  ungroup()
# for positive
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(positive = ifelse(is.na(positive) & row_number() == 1, 
                           0, 
                           positive)) %>%
  ungroup()
# for totalTestEncountersViral
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(totalTestEncountersViral = ifelse(is.na(totalTestEncountersViral) & row_number() == 1, 
                                           0, 
                                           totalTestEncountersViral)) %>%
  ungroup()

# for totalTestsPeopleViral
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(totalTestsPeopleViral = ifelse(is.na(totalTestsPeopleViral) & row_number() == 1, 
                                        0, 
                                        totalTestsPeopleViral)) %>%
  ungroup()
# for totalTestsViral
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(totalTestsViral = ifelse(is.na(totalTestsViral) & row_number() == 1, 
                                  0, 
                                  totalTestsViral)) %>%
  ungroup()


##Part 2: Replace using the lag values
#For death
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(death = ifelse(is.na(death), 
                        zoo::na.locf(death + deathIncrease, na.rm = FALSE), 
                        death)) %>%
  ungroup()

#For hospitalized
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(hospitalized = ifelse(is.na(hospitalized), 
                               zoo::na.locf(hospitalized + hospitalizedIncrease, na.rm = FALSE), 
                               hospitalized)) %>%
  ungroup()
#For negative
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(negative = ifelse(is.na(negative), 
                           zoo::na.locf(negative + negativeIncrease, na.rm = FALSE), 
                           negative)) %>%
  ungroup()
#For positive
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(positive = ifelse(is.na(positive), 
                           zoo::na.locf(positive + positiveIncrease, na.rm = FALSE), 
                           positive)) %>%
  ungroup()

#For totalTestEncountersViral
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(totalTestEncountersViral = ifelse(is.na(totalTestEncountersViral), 
                                           zoo::na.locf(totalTestEncountersViral + totalTestEncountersViralIncrease, na.rm = FALSE), 
                                           totalTestEncountersViral)) %>%
  ungroup()

# for totalTestsPeopleViral
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(totalTestsPeopleViral = ifelse(is.na(totalTestsPeopleViral), 
                                        zoo::na.locf(totalTestsPeopleViral + totalTestsPeopleViralIncrease, na.rm = FALSE), 
                                        totalTestsPeopleViral)) %>%
  ungroup()
# for totalTestsViral
data <- data %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(totalTestsViral = ifelse(is.na(totalTestsViral), 
                                  zoo::na.locf(totalTestsViral + totalTestsViralIncrease, na.rm = FALSE), 
                                  totalTestsViral)) %>%
  ungroup()

##Check if still NAs
data[is.na(data$death), ]
data[is.na(data$hospitalized), ]
data[is.na(data$negative), ]
data[is.na(data$positive), ]
data[is.na(data$totalTestEncountersViral), ]
data[is.na(data$totalTestsPeopleViral), ]
data[is.na(data$totalTestsViral), ]

# No NAs in those variables anymore :)

# At the beginning of the pandemic the death column was mainly 0 for most states,
# which means if death is 0, then deathConfirmed should be 0 also:
data <- data %>%
  mutate(deathConfirmed = ifelse(is.na(deathConfirmed) 
                                 & death == 0, 0, deathConfirmed))

# The same logic applies for variable deathProbable
data <- data %>%
  mutate(deathProbable = ifelse(is.na(deathProbable) 
                                 & death == 0, 0, deathProbable))

# After those basic imputations, we check the remaining NAs:
# by calculating the total number of NA values for each column
na_counts <- sapply(data, function(x) sum(is.na(x)))

# Convert to a dataframe for plotting
na_counts_df <- data.frame(Variable = names(na_counts), TotalNA = na_counts)

# Sort the data by TotalNA in descending order
na_counts_df <- na_counts_df %>%
  arrange(desc(TotalNA))

# Create a bar plot
ggplot(na_counts_df, aes(x = reorder(Variable, TotalNA), y = TotalNA)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  coord_flip() + # Flip coordinates for horizontal bars
  labs(title = "Total Number of NA Values by Variable",
       x = "Variable",
       y = "Total NA Count")

# Variable totalTestResults has very little NAs. This variable is
# the sum of the totalTestsAntibody, totalTestsAntigen and 
# totalTestsViral; the first 2 are mostly filled by NAs
# but we are not interested in the type of tests, rather in
# total tests perfomed, so we will remove those columns
data <- data %>%
  select(-c("totalTestsAntibody", "totalTestsAntigen", "totalTestsViral"))

# same situation with positive and negatives cases/tests. We will use
# only totals (total positive cases and negatives), so we will get rid of the components 
# (test types) as they also have high NA values:
data <- data %>%
  select(-c("negativeTestsAntibody", "negativeTestsViral", 
            "positiveTestsAntibody", "positiveTestsAntigen", 
            "positiveTestsViral"))

# We now use Last Observation Carried Forward(LOCF) to deal with missing values

# First sort the data by date
data <- data %>%
  arrange(date)

# Apply LOCF to all columns
data <- data %>%
  mutate(across(everything(), ~na.locf(., na.rm = FALSE)))

# Calculate the total number of NA values for each column
na_counts <- sapply(data, function(x) sum(is.na(x)))
print(na_counts)

# We will try to use regression models to predict these missing values, 
# as we want to know how the NA values change with time

# Calculate the number of NA values for each variable for each date
na_trends <- data %>%
  group_by(date) %>%
  summarise(across(-state, ~ sum(is.na(.)))) %>%
  pivot_longer(-date, names_to = "variable", values_to = "na_count")

# Plotting the trends
ggplot(na_trends, aes(x = date, y = na_count, color = variable)) +
  geom_line() +
  labs(title = "Trend of Missing Values Over Time for Each Variable",
       x = "Date",
       y = "Number of Missing Values") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_viridis_d()

# Define the cutoff date, as at the beginning of the pandemic the data collection
# process was not very standardized and most missing values are from that period.
cutoff_date <- as.Date("2020-04-01")

# Filter the data to exclude the initial phase with high missingness
data_subset <- data %>%
  filter(date >= cutoff_date)

# save this subsetted data to a new CSV file 
write.csv(data_subset, "subset_data_2.csv", row.names = FALSE)

# observe new dataset
data <- read.csv("subset_data_2.csv")
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$state <- as.factor(data$state)
summary(data)

### still need to use LOCF to deal with the remaining missing values
data <- data %>%
  arrange(date)

# Apply LOCF to all columns
data <- data %>%
  mutate(across(everything(), ~na.locf(., na.rm = FALSE)))
summary(data)

# check which columns have still NAs
columns_with_na <- colnames(data)[colSums(is.na(data)) > 0]
print(columns_with_na)

# Based on the results,we will remove the columns with NAs from the data set
# we will assume that:
# totalNegativeTestsPeople is represented by negative variable
# onVentilatorCumulative variable will not used in our analysis as we don't know
# how is the distribution of ventilators in hospitals and this can cause biases in our analysis.
# About positiveTestsPeopleAntibody and positiveTestsPeopleAntigen: 
# we will assume that unique people tested positive is represented 
# by the variable 'positiveCasesViral'
# totalTestsPeopleAntibody & totalTestsPeopleAntigen, we will assume that the
# variable totalTestsPeopleViral is very close to total unique people tested

# removing the columns mentioned above:
data <- data %>%
  select(-c(columns_with_na))

# we will now rename the variables to match our new assumptions
data <- data %>%
  rename(negativePeople = negative)

data <- data %>%
  rename(negativePeopleIncrease = negativeIncrease)

data <- data %>%
  rename(positivePeople = positiveCasesViral)

data <- data %>%
  rename(totalPositive = positive)

data <- data %>%
  rename(totalPositiveIncrease = positiveIncrease)

data <- data %>%
  rename(totalTestsPeople = totalTestsPeopleViral)

data <- data %>%
  rename(totalTestsPeopleIncrease = totalTestsPeopleViralIncrease)

# Finally, we remove the 'totalTestsViralIncrease' variable as
# it is connected to the variable removed 'totalTestsViral':
data <- data %>%
  select(-c("totalTestsViralIncrease"))

## Some small feature engineering
# we will calculate total negative tests done using total tests
# and positive tests
data <- data %>%
  mutate(totalNegative = totalTestResults - totalPositive)

# we will calculate the casesDeathRatio aka Mortality rate
data <- data %>%
  mutate(casesDeathRatio = death / totalPositive)

# We want to show time as season too:
data$season <- case_when(
  data$date >= as.Date('2020-04-01') & data$date < as.Date('2020-07-01') ~ 'Summer',
  data$date >= as.Date('2020-07-01') & data$date < as.Date('2020-10-01') ~ 'Autumn',
  data$date >= as.Date('2020-10-01') & data$date < as.Date('2021-01-01') ~ 'Winter',
  data$date >= as.Date('2021-01-01') & data$date < as.Date('2021-04-01') ~ 'Spring'
)

### Now we can begin with Data Exploration and Visualization
## Our main purpose is to be able to represent the main insights of the data
# through spatial plots. We are interested in the behaviour of the pandemic at the country
# level. Through a map is easier to make comparisons between the country averages and the
# state-level statistics.

# To plot our data into a map, we need to add the spatial features to our data

### Now let us combine the spatial data of USA  with our data

usa_map <- st_read("States_shapefile-shp/States_shapefile.shp")
head(usa_map)

usa_map <- usa_map %>%
  rename(state = State_Code)

# subset our data
# oldest records (because they are cumulative)
dataS <- data %>%
  group_by(state) %>%
  filter(date == max(date))

# merge the data
combined_data <- full_join(usa_map, dataS, by = "state")
head(combined_data)

# Part1 : State Wise Pattern analysis

# 1. Plot US mortality map
ggplot(combined_data) +
  geom_sf(aes(fill = casesDeathRatio)) + 
  geom_sf_text(aes(label = state), color = "white", size = 3, check_overlap = TRUE) +  # Add state names with white color
  theme_minimal() +
  labs(fill = "Percentage of death for people who are positive") +
  ggtitle("Covid-19 in the US: Mortality rate") +  
  theme(
    plot.title = element_text(hjust = 0.9, family='', face='bold', colour='black', size=25, margin=margin(t=50,b=-40)),  # Center the title
    plot.title.position = "panel",
    panel.background = element_rect(fill = "lightgray", color = "white"),  # Set background color and remove border
    panel.grid = element_blank(),            
    axis.text.x = element_blank(),          
    axis.text.y = element_blank(),          
    axis.title.x = element_blank(),          
    axis.title.y = element_blank(),          
    legend.position = c(0.1, 0.1),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right"
  )

# in the plot is visible that there were some states more affected by others
# like Pennsylvania, New Jersey, New York, Michigan, Louisiana; all in the
# lighter shades of blue, meaning between 2.5% and 3% mortality rate.

avgMortalityUS <- mean(dataS$casesDeathRatio, na.rm = TRUE)
avgMortalityUS
# those states were almost twice higher than the average.

# 2. Plot US map Recovery rate
# group and summarize recovered cases
dataR <- data %>%
  group_by(state) %>%
  summarize(recoveredRate = sum(recovered)/sum(totalPositive)) %>%
  mutate(recoveredRate = ifelse(is.infinite(recoveredRate), 1, recoveredRate))

# merge the data
combined_dataR <- full_join(usa_map, dataR, by = "state")
head(combined_dataR)

# plot recovered
ggplot(combined_dataR) +
  geom_sf(aes(fill = recoveredRate)) + 
  geom_sf_text(aes(label = state), color = "white", size = 3, check_overlap = TRUE) +
  theme_minimal() +
  labs(fill = "Percentage of patients recovered") +
  ggtitle("Covid-19 in the US: Recovery rate") +  
  theme(
    plot.title = element_text(hjust = 0.9, family='', face='bold', colour='black', size=25, margin=margin(t=50,b=-40)),
    plot.title.position = "panel",
    panel.background = element_rect(fill = "lightgray", color = "white"),
    panel.grid = element_blank(),            # Remove grid
    axis.text.x = element_blank(),           # Remove x-axis labels
    axis.text.y = element_blank(),           # Remove y-axis labels
    axis.title.x = element_blank(),          # Remove x-axis title
    axis.title.y = element_blank(),          # Remove y-axis title
    legend.position = c(0.1, 0.1),
    legend.justification = c("left", "bottom"),
    legend.box.just = "right"
  )

# Plot3 : Explain the reason of death rate and recovery rate with bubble chart
# We are thinking about if the recover rate and death rate is related to Healthcare System Capacity:
# States with better healthcare infrastructure and more resources may have higher recovery rates.
# And about the total test people. When they tested more, there will be more identified cases. the people who are just in mild situation are easier to get recover, it will influence the recovery rate

# Explore the reasons of death rate and recovery rate with 3 indexes :
# 1 ) totalTestsPeople: People who ince attended PCR tests
# 2 ) totalPositive: total number of attending PCR tests and the results are positive
# 3 ) hospitalzied : ever lived in hospita

# Calculate centroids of MULTIPOLYGONS to use as points for the bubble chart
combined_data <- combined_data %>%
  mutate(
    HospitalizationRate = hospitalized / totalPositive,
    PositiveRate = totalPositive / totalTestResults,
    casesDeathRatio = casesDeathRatio,
    centroid = st_centroid(geometry)
  )

# Extract longitude and latitude from centroids
combined_data$lon <- st_coordinates(combined_data$centroid)[,1]
combined_data$lat <- st_coordinates(combined_data$centroid)[,2]

# Bubble chart for hospitalization rate, positive rate, and death rate
ggplot(combined_data) +
  geom_sf(aes(fill = HospitalizationRate), color = NA, size = 0) +  # Use fill for hospitalization rate
  geom_point(aes(x = lon, y = lat, size = casesDeathRatio, color = PositiveRate), alpha = 0.7) +  # Size for caseDeathRatio, color for PositiveRate
  geom_text(aes(x = lon, y = lat, label = state), color = "white", size = 3, check_overlap = TRUE) +  # State labels
  scale_size_continuous(name = "Case Death Ratio", range = c(3, 12)) +
  scale_color_continuous(name = "Positive Rate", low = "yellow", high = "red") +
  labs(title = "Covid-19 in the US: Hospitalization, Positive, and Case Death Rates") +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.key.size = unit(0.5, "cm"),  # Adjust size of legend key
    text = element_text(size = 10),  # Adjust text size of state labels if necessary
    plot.background = element_rect(fill = "lightgray"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "lightgray"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) +
  coord_sf(crs = st_crs(combined_data), datum = NA) 

# Interpretation of this plot and explanation of the reasons
# Hospitalization Rate: This is likely shown by the fill color of each state, where darker shades indicate higher hospitalization rates. 
# Hospitalization rates are a proxy for the severity of the disease within the infected population and the burden on healthcare facilities.
# Positive Rate: Represented by the color of the bubbles on each state, transitioning from yellow to red. Yellow likely indicates a lower positive rate, while red indicates a higher positive rate. 
# The positive rate is a metric that can reflect the extent of the spread of the virus in the population that has been tested. Higher positive rates could suggest widespread transmission.
# Case Death Ratio: Shown by the size of the bubbles on the map, with larger bubbles indicating a higher case death ratio. 
# This metric is a measure of lethality among confirmed cases and can vary due to factors like healthcare quality, demographics, and timeliness of treatment.

# Now combine the reasons of commense for geography and economics to explain the reason
## Geography
# states with high population densities or major urban centers might show different patterns due to the ease of virus transmission in densely populated areas.
## Economics
# wealthier states with more resources may have better healthcare infrastructure, potentially resulting in lower hospitalization and death rates due to better access to medical care and treatment.
# States with higher economic disparities may show higher rates if underserved populations lack access to healthcare or have a higher prevalence of underlying conditions that can exacerbate the effects of COVID-19.
## Traffic 
# states that rely heavily on tourism or have significant international airports might have higher exposure rates due to higher numbers of visitors, potentially increasing positive rates.

# State-Specific Analysis:

# States in the Northeast, such as New York (NY), might show a high positive rate and hospitalization rate due to their dense populations and status as international travel hubs, which could lead to a rapid spread of the virus.
# Midwestern states, like North Dakota (ND) and South Dakota (SD), may exhibit different patterns. Despite lower population densities, certain events or localized outbreaks could have led to temporary spikes in positive rates.
# Southern states like Florida (FL) and Texas (TX) have diverse factors at play, including large populations, varied economic conditions, and different approaches to public health policies, which could contribute to their COVID-19 statistics.

#########

# Part 2 : Time wise pattern analysis


# Part A: Focus on the symptom
# Plot1 : Evolution of COVID-19 mortality and recovery Over Time
data_grouped <- data %>%
  group_by(date) %>%
  summarise(
    deathIncrease = sum(deathIncrease), 
    totalPositive = sum(totalPositive),
    recovered = sum(recovered),
    death_cases_rate = deathIncrease / totalPositive,
    recoveryRate = recovered / totalPositive
  )

deathRecover <- ggplot(data = data_grouped, aes(x = date)) +
  geom_line(aes(y = recoveryRate, color = "Recovery Rate"), size = 0.5, linetype = "solid") +
  geom_line(aes(y = death_cases_rate*100, color = "Mortality Rate"), size = 0.5, linetype = "solid") +
  geom_smooth(aes(x = date, y = recoveryRate), color = "violet", size = 0.4, linetype = "dashed", se = FALSE, inherit.aes = FALSE) +
  geom_smooth(aes(x = date, y = death_cases_rate*100), color = "darkgray", size = 0.4, linetype = "dashed", se = FALSE, inherit.aes = FALSE) +
  labs(
    title = "Recovery Rate vs. Mortality Rate: slow-down point",
    subtitle = "Comparison of recovery and mortality rates based on date",
    x = "Date",
    y = "Recovery Rate",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial"),  # Change font family
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(size = 14),
    axis.title.y.left = element_text(color = "purple"),  # Set color for the main y-axis title
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  ) +
  scale_y_continuous(
    name = "Recovery Rate",
    limits = c(0, 0.55),
    sec.axis = sec_axis(~ ./100, name = "Mortality Rate")
  ) +
  scale_color_manual(values = c("Recovery Rate" = "purple", "Mortality Rate" = "black"), name = "")

print(deathRecover)

# Plot2: Death to Cases Rate evolution
plot_death_cases_rate <- ggplot(data = data_grouped, aes(x = date, y = death_cases_rate)) +
  geom_line(color = "blue", size = 0.5) +
  theme_minimal() +
  scale_y_continuous(name = "Death to Cases Rate")
print(plot_death_cases_rate)

# Interpretation for death and positive cases rate trend
# The rate is presented on a logarithmic scale on the y-axis.
# There is a sharp decline early in the timeline, which flattens out as time progresses. This could indicate an initially high mortality rate when the virus first emerged, possibly due to a lack of preparedness, understanding of the virus, and effective treatments.
# Over time, as treatments improved and better protocols were put in place, the death rate relative to positive cases decreased and stabilized.

# PART2: Explain the reason from hospitalization 
new_data <- data %>%
  select(date, totalPositive, hospitalized, recovered)

## stacked time series plot Plot3
ggplot(data) +
  geom_area(aes(x = date, y = totalPositive, fill = "totalPositive"), alpha = 0.9, color = "deepskyblue4") +
  geom_area(aes(x = date, y = hospitalized, fill = "hospitalized"), alpha = 1, color = "red") +
  geom_area(aes(x = date, y = recovered, fill = "recovered"), alpha = 0.6, color = "olivedrab3") +
  scale_fill_manual(values = c("totalPositive" = "deepskyblue4", "hospitalized" = "red", "recovered" = "olivedrab3")) +
  labs(title = "Time Series of COVID-19 Cases",
       x = "Date",
       y = "Number of Cases",
       fill = "Category") +
  theme_minimal()

# Interpretation for time series stack plot
# This plot shows the cumulative number of COVID-19 cases over time, with different colors representing 'hospitalized', 'recovered', and 'totalPositive' cases.
# The green area, which is the largest, represents the 'recovered' cases. It shows a consistent increase over time, indicating a growing number of individuals who have recovered from the virus.
# The blue area on top of the green represents 'totalPositive' cases, which includes both current active cases and those who have recovered or passed away. This area also increases over time but at a slower rate than the recovered cases, suggesting that while the virus is still spreading, recoveries are also occurring.
# The red area, which is the smallest, represents 'hospitalized' cases. The fact that it is much smaller than the 'recovered' area could indicate that a smaller proportion of the total positive cases result in hospitalization.
# Overall, the plot shows that over time, the number of recovered cases is increasing, which is a positive sign.
# However, the total number of positive cases is also growing, though this could be due to both the spread of the virus and increased testing.


## time series line plots
ggplot(data) +
  geom_line(aes(x = date, y = totalPositive, color = "totalPositive")) +
  geom_line(aes(x = date, y = hospitalized, color = "hospitalized")) +
  geom_line(aes(x = date, y = recovered, color = "recovered")) +
  scale_color_manual(values = c("totalPositive" = "blue", "hospitalized" = "red", "recovered" = "green")) +
  scale_y_continuous("Number of Cases", 
                     trans = 'log10',  # Apply a logarithmic transformation
                     labels = scales::comma) + 
  labs(title = "Time Series of COVID-19 Cases",
       x = "Date",
       y = "Number of Cases",
       color = "Category") +
  theme_minimal()

# Interpretation of the time series line plots
# The trend of the death to positive case rate may be influenced by several factors that can be inferred from the stacked time series plot:

# Increased Testing: More widespread testing over time could lead to a higher number of detected positive cases, which, in proportion to deaths, would lower the death rate.
# Improved Treatments: As the healthcare community learned more about treating COVID-19, the efficacy of treatments likely improved, reducing the death rate.
# Hospital Capacity: The flat red area in the stacked plot suggests that hospitalizations have not overwhelmed capacity. Adequate hospital care can lead to a reduction in the death rate.
# Demographics: The rate of infections among different age groups and demographics could shift over time, potentially affecting the death rate. For example, if more infections occur in younger, healthier populations, the death rate could decrease.
# Vaccinations and Immunity: If the time period covered by these plots includes the development and distribution of vaccines, increased immunity in the population would also contribute to a decreasing death rate
