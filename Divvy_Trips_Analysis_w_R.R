## -- S1: Install necessary libraries -- ##
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(skimr)
library(lubridate)
library(readxl)
library(viridis)
library(ggbeeswarm)

## -- User Functions -- ##
# Define functions to evaluate properties of the data
# -- Function to identify outliers based on IQR -- #
identify_outliers <- function(data, multiplier = 2) {
  # Calculate quartiles
  q1 <- quantile(data, probs = 0.25)
  q3 <- quantile(data, probs = 0.75)
  
  # Calculate IQR
  iqr <- q3 - q1
  
  # Identify outliers based on IQR threshold
  outliers <- data < (q1 - multiplier * iqr) | data > (q3 + multiplier * iqr)
  
  return(outliers)
}

# -- end function -- #

## -- S2: Load, Clean, and Extrapolate the Data -- ##
f1 <- "C:/Users/Anthony Morciglio/OneDrive/Coursera Google Data Analytics/Divvy_Trips_2019_2020/Divvy_Trips_2019_Q1_CL.csv"
f2 <- "C:/Users/Anthony Morciglio/OneDrive/Coursera Google Data Analytics/Divvy_Trips_2019_2020/Divvy_Trips_2020_Q1_CL.csv"

# Read the data that was cleaned in Excel and exported as a CSV file.
Divvy_Trips_2019_Q1 <- read_csv(f1)
Divvy_Trips_2020_Q1 <- read_csv(f2)

# Join the data
# Ensure both dataframes of the same class/type:
Divvy_Trips_2019_Q1$trip_id <- as.character(Divvy_Trips_2019_Q1$trip_id)
Divvy_Trips_2020_Q1$trip_id <- as.character(Divvy_Trips_2020_Q1$trip_id)
df_merge <- merge(Divvy_Trips_2019_Q1, Divvy_Trips_2020_Q1, all.x = TRUE, all.y = TRUE)

# Glance at the data
tail(Divvy_Trips_2019_Q1)
glimpse(Divvy_Trips_2019_Q1)
str(Divvy_Trips_2019_Q1)
summary(Divvy_Trips_2019_Q1)
skim_without_charts(Divvy_Trips_2019_Q1)



## -- Cleaning Data -- ##
df_merge <- df_merge %>% mutate(tripduration_mins = round(tripduration/60, 2)) %>% 
  mutate(gender = ifelse(is.na(df_merge$gender) | df_merge$gender == "" | df_merge$gender == "NA", "Other", gender)) %>% 
  mutate(age = year(today()) - df_merge$birthyear)

# change the format of the datetime col
df_lim$start_time_fmt <- strptime(df_lim$start_time, format = "%m/%d/%Y %H:%M")
df_lim$week_num <- as.numeric(format(df_lim$start_time_fmt, "%U")) # extract the week number
df_lim$year <- as.character(df_lim$year) # change class of year to character

# mini step
# Define breaks for age groups (adjust as needed)
brk <- c(20, 30, 40, 50, 60, 130)

# Create labels for each age group (adjust labels as needed)

# Group the age column using cut and labels
df_merge <- df_merge %>%  
  mutate(age_group = cut(df_merge$age, breaks = brk))

# Export the data into a csv file for analysis in Tableau.
path <- "C:/Users/Anthony Morciglio/OneDrive/Coursera Google Data Analytics/Divvy_Trips_2019_2020"
path <- "C:/Users/Anthony Morciglio/OneDrive/Projects/Programming/R"
file <- paste(path, "Divvy_Trips_2019_2020_Q1_merge.csv", sep = "/")
write.csv(df_merge, file, row.names = FALSE)


## -- Grouping Data -- ##
# Group data based on Gender and User
print("--- Summarizing key statistics based on Gender ---")
df_gender <- df_merge %>%  group_by(gender) %>% 
  drop_na() %>% 
  summarize(avg_tripduration_mins = mean(tripduration_mins), med_tripduration_mins = median(tripduration_mins), sd_tripduration_mins = sd(tripduration_mins), min_tripduration_mins = min(tripduration_mins), max_tripduration_mins = max(tripduration_mins))

print("--- Summarizing key statistics based on Usertype ---")
df_users <- df_merge %>%  group_by(usertype) %>% 
  drop_na() %>% 
  summarize(avg_tripduration_mins = mean(tripduration_mins), med_tripduration_mins = median(tripduration_mins), sd_tripduration_mins = sd(tripduration_mins), min_tripduration_mins = min(tripduration_mins), max_tripduration_mins = max(tripduration_mins))

# Filter and evaluate key metrics: tripduration
# Filter based on Gender
df_gender_M <- filter(df_merge, df_merge$gender == "Male")
df_gender_F <- filter(df_merge, df_merge$gender == "Female")
df_gender_O <- filter(df_merge, df_merge$gender == "Other")

# Filter based on Usertype
df_users_S <- filter(df_merge, df_merge$usertype == "Subscriber")
df_users_C <- filter(df_merge, df_merge$usertype == "Customer")


# There appears to be a difference in avg trip duration between Other and Male/Female
# We will validate the empirical observation using Hypothesis Testing
Fstat_mf <- var.test(df_gender_M$tripduration_mins, df_gender_F$tripduration_mins, alternative = "two.sided")
var_diff_bool <- FALSE
tmp_df <- df_merge %>% filter(df_merge$gender == "Male" | df_merge$gender == "Female")
Tstat_mf <- t.test(tmp_df$tripduration_mins ~ tmp_df$gender, data = tmp_df, var.equal = !var_diff_bool)

if (Tstat_mf[3]$p.value <= 0.05)
{ print("There is a Statistical difference between the average tripduration of Male and Female") 
  } else 
    { print("There is no Statistical difference between the average tripduration of Male and Female")
}

if (Fstat_mf[3]$p.value <= 0.05)
{ print("There is a Statistical difference between the variance of tripduration of Male and Female") 
  } else 
    { print("There is no Statistical difference between the variance of tripduration of Male and Female")
}


# -- Apply same method for M/F and Other in Gender -- #
Fstat_mo <- var.test(df_gender_M$tripduration_mins, df_gender_O$tripduration_mins, alternative = "two.sided")
tmp_df <- df_merge %>% filter(df_merge$gender == "Male" | df_merge$gender == "Other")
Tstat_mo <- t.test(tmp_df$tripduration_mins ~ tmp_df$gender, data = tmp_df, var.equal = !var_diff_bool)

if (Tstat_mo[3]$p.value <= 0.05)
  { print("There is a Statistical difference between the average tripduration of Male and Other")
  } else 
    { print("There is no Statistical difference between the average tripduration of Male and Other")
}

if (Fstat_mo[3]$p.value <= 0.05)
{ print("There is a Statistical difference between the variance of tripduration of Male and Other") 
  } else 
    { print("There is no Statistical difference between the variance of tripduration of Male and Other")
}

# -- Hypothesis testing for UserType -- #
Fstat_sc <- var.test(df_users_S$tripduration_mins, df_users_C$tripduration_mins, alternative = "two.sided")
tmp_df <- df_merge %>% filter(df_merge$usertype == "Subscriber" | df_merge$usertype == "Customer")
Tstat_sc <- t.test(tmp_df$tripduration_mins ~ tmp_df$usertype, data = tmp_df, var.equal = !var_diff_bool)

if (Tstat_sc[3]$p.value <= 0.05)
  { print("There is a Statistical difference between the average tripduration of Subscriber and Customer")
 } else 
   { print("There is no Statistical difference between the average tripduration of Subscriber and Customer")
}

if (Fstat_sc[3]$p.value <= 0.05) 
  { print("There is a Statistical difference between the variance of tripduration of Subscriber and Customer")
 } else 
  { print("There is no Statistical difference between the variance of tripduration of Subscriber and Customer")
  }



## -- Plotting Section -- ##
# Filter data for cleaner plots (remove outliers)
q3 <- quantile(df_merge$tripduration_mins, 0.75, na.rm = TRUE)
q1 <- quantile(df_merge$tripduration_mins, 0.25, na.rm = TRUE)
IQR <- q3 - q1
UB <- mean(df_merge$tripduration_mins, na.rm = TRUE) + 1*sd(df_merge$tripduration_mins, na.rm = TRUE)
LB <- mean(df_merge$tripduration_mins, na.rm = TRUE) - 1*sd(df_merge$tripduration_mins, na.rm = TRUE)
UB <- q3 + 5 * IQR
LB <- q1 - 5 * IQR
df_lim <- df_merge %>% filter(df_merge$tripduration_mins <= UB & df_merge$tripduration_mins >= LB)

# Boxplots
ggplot(data=df_lim, aes(x = gender, y = tripduration_mins, fill = gender)) +
  geom_violin(data = df_lim, trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_boxplot(data=df_lim, aes(df_lim$gender, df_lim$tripduration_mins), width = 0.075) + 
  labs(x = "Gender", y = "Trip Duration (Minutes)", title = "Box Plot: Gender vs Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$gender)))) +
  theme_linedraw()

ggplot(data=df_lim, aes(x = usertype, y = tripduration_mins, fill = usertype)) +
  geom_violin(data = df_lim, trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_boxplot(data=df_lim, aes(df_lim$usertype, df_lim$tripduration_mins), width = 0.075, alpha = 0.5, fill = 'white') + 
  labs(x = "UserType", y = "Trip Duration (Minutes)", title = "Box Plot: UserType vs Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$usertype)))) +
  theme_linedraw()

df_lim %>% 
  ggplot(aes(x = age_group, y = tripduration_mins, fill = age_group)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_boxplot(aes(df_lim$age_group, df_lim$tripduration_mins), width = 0.075, alpha = 0.5, fill = 'white') + 
  labs(x = "Age Group", y = "Trip Duration (Minutes)", title = "Box Plot: Age Group vs Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$age_group)))) +
  theme_linedraw()

ggplot(data=df_lim, aes(x = age_group, y = tripduration_mins, fill = usertype)) +
  geom_boxplot(data=df_lim, aes(df_lim$age_group, df_lim$tripduration_mins)) + 
  labs(x = "Age Group", y = "Trip Duration (Minutes)", title = "Box Plot: Age Group vs Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$usertype)))) +
  theme_linedraw()

# Density plots
ggplot(data=df_lim, aes(x = tripduration_mins, fill = gender)) +
  geom_histogram(data=df_lim, aes(fill=df_lim$gender, x=df_lim$tripduration_mins, alpha = 0.1)) + 
  labs(x = "Trip Duration (Minutes)", y = "Probability", title = "Density Plot: Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$gender)))) +
  theme_linedraw()

ggplot(data=df_lim, aes(x = tripduration_mins, fill = df_lim$usertype)) +
  geom_density(data=df_lim, aes(fill=df_lim$usertype, x=df_lim$tripduration_mins, alpha = 0.1)) + 
  labs(x = "Trip Duration (Minutes)", y = "Probability", title = "Density Plot: Trip Duration (Minutes)", fill = "UserType") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$usertype)))) +
  theme_linedraw()

df_lim %>% 
  mutate(weekday = factor(weekday, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>% 
  ggplot(aes(x = weekday, y = tripduration_mins, fill = weekday)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_boxplot(aes(df_lim$weekday, df_lim$tripduration_mins), width = 0.075, alpha = 0.5, fill = 'white') + 
  labs(x = "Weekday", y = "Trip Duration (Minutes)", title = "Box Plot: UserType vs Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$weekday)))) +
  theme_linedraw()

df_lim %>% 
  mutate(weekday = factor(weekday, levels = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))) %>% 
  ggplot(aes(x = weekday, y = tripduration_mins, fill = usertype)) +
  geom_boxplot(aes(df_lim$weekday, df_lim$tripduration_mins, fill = df_lim$usertype), alpha = 0.5) + 
  labs(x = "Weekday", y = "Trip Duration (Minutes)", title = "Box Plot: UserType vs Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$usertype)))) +
  theme_linedraw()

df_lim %>% 
  ggplot(aes(x = age_group, y = tripduration_mins, fill = usertype)) +
  geom_boxplot(aes(df_lim$age_group, df_lim$tripduration_mins, fill = df_lim$usertype), alpha = 0.5) + 
  labs(x = "Weekday", y = "Trip Duration (Minutes)", title = "Box Plot: UserType vs Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$usertype)))) +
  theme_linedraw()

ggplot(data=df_lim, aes(x = weekday, y = tripduration_mins, fill = weekday)) +
  #geom_violin(data = df_lim, trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) + 
  geom_boxplot(data=df_lim, aes(df_lim$weekday, df_lim$tripduration_mins, fill = df_lim$weekday), alpha = 0.5) + 
  labs(x = "Weekday", y = "Trip Duration (Minutes)", title = "Box Plot: UserType vs Trip Duration (Minutes)") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$weekday)))) +
  theme_linedraw()


# -- Time Plots -- #
ggplot(data = df_lim, aes(x = df_lim$month, fill = df_lim$year)) +
  geom_histogram(data = df_lim, aes(x = df_lim$month, fill = df_lim$year), stat = 'count') + 
  labs(x = "Month", y = "Number of Trips", title = "Number of Trips vs Month", fill = "Year") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$year)))) +
  theme_linedraw()

ggplot(data = df_lim, aes(x = week_num, fill = df_lim$year)) +
  geom_histogram(data = df_lim, aes(fill = df_lim$year, x = df_lim$week_num)) + 
  labs(x = "Week Number", y = "Number of Trips", title = "Number of Trips vs Week Number", fill = "Year") +
  scale_fill_manual(values = viridis(n = length(unique(df_lim$year)))) +
  theme_linedraw()



