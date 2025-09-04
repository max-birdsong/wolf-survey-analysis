### Wolf Report Number Longitudinal ###
library(ggplot2)
library(tidyverse)
library(dplyr)
library(writexl)
library(officer)
library(survey)
library(scales)

# Set working directory 
#setwd("C:/Users/max.birdsong/OneDrive - The University of Montana/WolfData")
setwd("C:/Users/maxbi/OneDrive - The University of Montana/WolfData/WolfDatasets/Final2023")

# Load  data
data <- read.csv("FINAL Quantitative Respondent Data Combined Long.csv")
head(data)


list_of_plots2 <- list()

#### Pre analysis Prep ####


# Filter out cases without weights
data$FinalWt = as.numeric(data$FinalWt)
data <- data %>%
  filter(!is.na(FinalWt))

# Create a survey design object with the survey weights
data$FinalWt = as.numeric(data$FinalWt)

# Grouping
# Step 1: Extract data only for the year '23.
data_23 <- data %>% filter(year == "23")

# Step 2: Identify respondents who are not yet in the 'House' group.
non_house_respondents <- data_23 %>%
  anti_join(data_23 %>% filter(group == "House"), by = "IDNO") %>%
  distinct(IDNO, .keep_all = TRUE)

# Step 3: Assign these respondents to the 'House' group.
house_data <- non_house_respondents
house_data$group <- "House"

# Merge this new 'House' data with the original `data_23` set.
data_23 <- bind_rows(data_23, house_data)

# Additional Group Assignments
# Create a function that assigns new observations based on the provided conditions
assign_new_obs <- function(data_subset, condition_column, condition_value, new_group_value) {
  new_data <- data_subset %>%
    filter(!!sym(condition_column) == condition_value) %>%
    distinct(IDNO, .keep_all = TRUE)
  
  new_data$group <- new_group_value
  return(new_data)
}

# 1. Assign Wolf observation based on condition Q29 = 2
wolf_data <- assign_new_obs(data_23 %>% filter(group %in% c("House", "Deer", "Land")), "Q29", 2, "Wolf")
data_23 <- bind_rows(data_23, wolf_data)

# 2. Assign Land observation based on condition Q34 = 2
land_data <- assign_new_obs(data_23 %>% filter(group %in% c("House", "Deer", "Wolf")), "Q34", 2, "Land")
data_23 <- bind_rows(data_23, land_data)

# 3. Assign Deer observation based on condition Q33 = 2
deer_data <- assign_new_obs(data_23 %>% filter(group %in% c("House", "Wolf", "Land")), "Q33", 2, "Deer")
data_23 <- bind_rows(data_23, deer_data)

# Step 4: Merge the modified year '23 data back with the main dataset.
data <- data %>% filter(year != "23")
data <- bind_rows(data, data_23)

# Sort the data by year and IDNO
data <- data %>%
  arrange(factor(year, levels = c("23", "17", "12")), IDNO)
data[data == 999] <- NA
data[data==9]<- NA

# rename House to GenPop

data <- data %>%
  mutate(
    group = case_when(
      group == "House" ~ "GenPop",
      group == "Deer" ~ "Deer/Elk",
      TRUE ~ group  # This keeps all other values as they are
    )
  )


data <- data %>%
  mutate(
    year = case_when(
      year == 23 ~ "2023",
      year == 17 ~ "2017",
      year == 12 ~ "2012",
      TRUE ~ as.character(year)  # Converts any other numeric year to character
    )
  )

# Set the survey design for survey package
survey_design <- svydesign(ids = ~1, data = data, strata=~strata, weights = ~FinalWt)

# Calculate mean response for Q3
# Calculate mean response and its standard error for Q3
agg_data_Q3 <- svyby(~as.numeric(Q3), by = ~group + year, design = survey_design, 
                     FUN = svymean, na.rm = TRUE)

agg_data_Q3_se <- svyby(~as.numeric(Q3), by = ~group + year, design = survey_design, 
                        FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q3_se$se_mean <- sqrt(agg_data_Q3_se$se)
agg_data_Q3_se$mean_response_total <- agg_data_Q3_se$`as.numeric(Q3)`
agg_data_Q3_se <- agg_data_Q3_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q3 <- merge(agg_data_Q3, agg_data_Q3_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q3)[colnames(agg_data_Q3) == "as.numeric(Q3)"] <- "mean_response"


# Calculate percentage distribution for Q3
percent_data_Q3 <- data %>%
  filter(!is.na(Q3)) %>%
  group_by(group, year, Q3) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q3$year <- as.factor(agg_data_Q3$year)
percent_data_Q3$year <- as.factor(percent_data_Q3$year)

# Percent plot
plot_percent_data_Q3 <- ggplot(percent_data_Q3, aes(x = Q3, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),  # Round and format the label (without % using sprintf)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(group ~ year, scales = "free_y") +  # Include facet titles
  theme_minimal() +
  labs(
    title = "How tolerant are you with wolves being on the Montana landscape?",
    x = "From very intolerant (1) to tolerant (5)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +   # Use grayscale colors
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),  # Remove % symbol from labels
    limits = c(0, 100)  # Set y limit up to 60
  ) +
  theme(
    strip.background = element_blank()  # Remove background behind facet titles
  )

plot_percent_data_Q3


# Create the plots for Q3
plot_agg_data_Q3 <- ggplot(agg_data_Q3, aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -3.8, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How tolerant are you with wolves being on the Montana landscape?",
    x = "From very intolerant (1) to tolerant (5)",
    y = "Mean Response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))


# Print the plot
print(plot_agg_data_Q3)

list_of_plots2 <- append(list_of_plots2, list(plot_agg_data_Q3))
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q3))

# Satisfaction with Montana wolf hunting regulations
# Calculate mean response and its standard error for Q5
agg_data_Q5 <- svyby(~as.numeric(Q5), by = ~group + year, design = survey_design, 
                     FUN = svymean, na.rm = TRUE)

agg_data_Q5_se <- svyby(~as.numeric(Q5), by = ~group + year, design = survey_design, 
                        FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q5_se$se_mean <- sqrt(agg_data_Q5_se$se)
agg_data_Q5_se$mean_response_total <- agg_data_Q5_se$`as.numeric(Q5)`
agg_data_Q5_se <- agg_data_Q5_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q5 <- merge(agg_data_Q5, agg_data_Q5_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q5)[colnames(agg_data_Q5) == "as.numeric(Q5)"] <- "mean_response"

# Calculate percentage distribution for Q5
percent_data_Q5 <- data %>%
  filter(!is.na(Q5)) %>%
  group_by(group, year, Q5) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q5$year <- as.factor(agg_data_Q5$year)
percent_data_Q5$year <- as.factor(percent_data_Q5$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q5 <- ggplot(agg_data_Q5, aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response)), 
            position = position_dodge(0.7), 
            vjust = -2, 
            size = 3.0) +  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How satisfied are you with the Montana wolf hunting regulations?",
    x = "On a scale from 1 (very dissatisfied) to 5 (very satisfied)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))


plot_agg_data_Q5

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_agg_data_Q5))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q5 <- ggplot(percent_data_Q5, aes(x = Q5, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),  # Round and format the label (without % using sprintf)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(group ~ year, scales = "free_y") +  # Include facet titles
  theme_minimal() +
  labs(
    title = "How satisfied are you with the Montana wolf hunting regulations?",
    x = "On a scale from 1 (very dissatisfied) to 5 (very satisfied)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),  # Remove % symbol from labels
    limits = c(0, 100)
  ) +
  theme(
    strip.background = element_blank()  # Remove background behind facet titles
  )

plot_percent_data_Q5

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q5))

# Tolerance of wolf hunting
# Calculate mean response and its standard error for Q6
agg_data_Q6 <- svyby(~as.numeric(Q6), by = ~group + year, design = survey_design, 
                     FUN = svymean, na.rm = TRUE)

agg_data_Q6_se <- svyby(~as.numeric(Q6), by = ~group + year, design = survey_design, 
                        FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q6_se$se_mean <- sqrt(agg_data_Q6_se$se)
agg_data_Q6_se$mean_response_total <- agg_data_Q6_se$`as.numeric(Q6)`
agg_data_Q6_se <- agg_data_Q6_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q6 <- merge(agg_data_Q6, agg_data_Q6_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q6)[colnames(agg_data_Q6) == "as.numeric(Q6)"] <- "mean_response"

# Calculate percentage distribution for Q6
percent_data_Q6 <- data %>%
  filter(!is.na(Q6)) %>%
  group_by(group, year, Q6) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q6$year <- as.factor(agg_data_Q6$year)
percent_data_Q6$year <- as.factor(percent_data_Q6$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q6 <- ggplot(agg_data_Q6, aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response)), 
            position = position_dodge(0.7), 
            vjust = -1.8, 
            size = 3) +  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How tolerant are you with wolf hunting in Montana?",
    x = "On a scale from 1 (very intolerant) to 5 (very tolerant)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))


plot_agg_data_Q6

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_agg_data_Q6))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q6 <- ggplot(percent_data_Q6, aes(x = Q6, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),  # Round and format the label (without % using sprintf)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(group ~ year, scales = "free_y") +  # Include facet titles
  theme_minimal() +
  labs(
    title = "How tolerant are you with wolf hunting in Montana?",
    x = "On a scale from 1 (very intolerant) to 5 (very tolerant)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),  # Remove % symbol from labels
    limits = c(0, 100)
  ) +
  theme(
    strip.background = element_blank()  # Remove background behind facet titles
  )

plot_percent_data_Q6

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q6))

# Interaction with FWP on wolf-related issues
# Calculate mean response and its standard error for Q18
agg_data_Q18 <- svyby(~as.numeric(Q18), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q18_se <- svyby(~as.numeric(Q18), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q18_se$se_mean <- sqrt(agg_data_Q18_se$se)
agg_data_Q18_se$mean_response_total <- agg_data_Q18_se$`as.numeric(Q18)`
agg_data_Q18_se <- agg_data_Q18_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q18 <- merge(agg_data_Q18, agg_data_Q18_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q18)[colnames(agg_data_Q18) == "as.numeric(Q18)"] <- "mean_response"

# Calculate percentage distribution for Q18
percent_data_Q18 <- data %>%
  filter(!is.na(Q18)) %>%
  group_by(group, year, Q18) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q18$year <- as.factor(agg_data_Q18$year)
percent_data_Q18$year <- as.factor(percent_data_Q18$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q18 <- ggplot(agg_data_Q18, aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4.8, -1.5)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How much have you interacted with FWP in the past on wolf-related issues?",
    x = "On a scale from 1 (very infrequently) to 5 (very frequently)",
    y = "Mean Response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))


plot_agg_data_Q18

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_agg_data_Q18))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q18 <- ggplot(percent_data_Q18, aes(x = Q18, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),  # Round and format the label (without % using sprintf)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(group ~ year, scales = "free_y") +  # Include facet titles
  theme_minimal() +
  labs(
    title = "How much have you interacted with FWP in the past on wolf-related issues?",
    x = "On a scale from 1 (very infrequently) to 5 (very frequently)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),  # Remove % symbol from labels
    limits = c(0, 100)
  ) +
  theme(
    strip.background = element_blank()  # Remove background behind facet titles
  )

plot_percent_data_Q18

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q18))

# Interaction with FWP on non-wolf related issues
# Calculate mean response and its standard error for Q19
agg_data_Q19 <- svyby(~as.numeric(Q19), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q19_se <- svyby(~as.numeric(Q19), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q19_se$se_mean <- sqrt(agg_data_Q19_se$se)
agg_data_Q19_se$mean_response_total <- agg_data_Q19_se$`as.numeric(Q19)`
agg_data_Q19_se <- agg_data_Q19_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q19 <- merge(agg_data_Q19, agg_data_Q19_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q19)[colnames(agg_data_Q19) == "as.numeric(Q19)"] <- "mean_response"


# Calculate percentage distribution for Q19
percent_data_Q19 <- data %>%
  filter(!is.na(Q19)) %>%
  group_by(group, year, Q19) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q19$year <- as.factor(agg_data_Q19$year)
percent_data_Q19$year <- as.factor(percent_data_Q19$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q19 <- ggplot(agg_data_Q19, aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -1.7)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How much have you interacted with FWP in the past on non-wolf related issues?",
    x = "On a scale from 1 (very infrequently) to 5 (very frequently)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))


plot_agg_data_Q19

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_agg_data_Q19))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q19 <- ggplot(percent_data_Q19, aes(x = Q19, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),  # Round and format the label (without % using sprintf)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(group ~ year, scales = "free_y") +  # Include facet titles
  theme_minimal() +
  labs(
    title = "How much have you interacted with FWP in the past on non-wolf related issues?",
    x = "On a scale from 1 (very infrequently) to 5 (very frequently)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),  # Remove % symbol from labels
    limits = c(0, 100)
  ) +
  theme(
    strip.background = element_blank()  # Remove background behind facet titles
  )


plot_percent_data_Q19

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q19))

# Calculate percentage distribution for Q20
percent_data_Q20 <- data %>%
  filter(!is.na(Q20)) %>%
  group_by(group, year, Q20) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
percent_data_Q20$year <- as.factor(percent_data_Q20$year)

# Plot percentage distribution of all responses
plot_percent_data_Q20 <- ggplot(percent_data_Q20, aes(x = factor(Q20), y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),  # Round and format the label (without % using sprintf)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(group ~ year, scales = "free_y") +  # Include facet titles
  theme_minimal() +
  labs(
    title = "Did You Follow Season Setting Process?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),  # Remove % symbol from labels
    limits = c(0, 100)  # Set y limit up to 100 (adjust as needed)
  ) +
  scale_x_discrete(labels = c("1" = "No", "2" = "Yes")) +  # Change x-axis labels
  theme(
    strip.background = element_blank()  # Remove background behind facet titles
  )

plot_percent_data_Q20

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q20))

# Calculate percentage distribution for Q23a (Awareness of Montana laws allowing wolves to be legally harvested)
percent_data_Q23a <- data %>%
  filter(!is.na(Q23a)) %>%
  group_by(group, year, Q23a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
percent_data_Q23a$year <- as.factor(percent_data_Q23a$year)

# Plot percentage distribution of all responses
filtered_data_Q23a <- percent_data_Q23a %>% 
  filter(Q23a != 22)

plot_percent_data_Q23a <- ggplot(filtered_data_Q23a, aes(x = factor(Q23a), y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),  # Round and format the label (without % using sprintf)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(group ~ year, scales = "free_y") +  # Include facet titles
  theme_minimal() +
  labs(
    title = "Awareness: Montana Laws Allow Wolves to Be Legally Harvested?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),  # Remove % symbol from labels
    limits = c(0, 105)  # Set y limit up to 100 (adjust as needed)
  ) +
  scale_x_discrete(labels = c("1" = "No", "2" = "Yes")) +  # Change x-axis labels
  theme(
    strip.background = element_blank()  # Remove background behind facet titles
  )

plot_percent_data_Q23a


# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q23a))

# Calculate percentage distribution for Q23b (Awareness that FWP accepts comments from the public)
percent_data_Q23b <- data %>%
  filter(!is.na(Q23b)) %>%
  group_by(group, year, Q23b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
percent_data_Q23b$year <- as.factor(percent_data_Q23b$year)

# Plot percentage distribution of all responses
plot_percent_data_Q23b <- ggplot(percent_data_Q23b, aes(x = factor(Q23b), y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),  # Round and format the label (without % using sprintf)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(group ~ year, scales = "free_y") +  # Include facet titles
  theme_minimal() +
  labs(
    title = "Awareness: FWP Accepts Comments from the Public?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),  # Remove % symbol from labels
    limits = c(0, 100)  # Set y limit up to 100 (adjust as needed)
  ) +
  scale_x_discrete(labels = c("1" = "No", "2" = "Yes")) +  # Change x-axis labels
  theme(
    strip.background = element_blank()  # Remove background behind facet titles
  )

plot_percent_data_Q23b

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q23b))

# Calculate percentage distribution for Q23c (Awareness that dates and times of meetings are identified in public press)
percent_data_Q23c <- data %>%
  filter(!is.na(Q23c)) %>%
  group_by(group, year, Q23c) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
percent_data_Q23c$year <- as.factor(percent_data_Q23c$year)

# Plot percentage distribution of all responses
percent_data_Q23c_filtered <- percent_data_Q23c %>% 
  filter(Q23c != 3)

plot_percent_data_Q23c <- ggplot(percent_data_Q23c_filtered, aes(x = factor(Q23c), y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),
    vjust = 0,  
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(group ~ year, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Awareness: Dates and times of meetings are identified in public press?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  scale_x_discrete(labels = c("1" = "No", "2" = "Yes")) +
  theme(strip.background = element_blank())

plot_percent_data_Q23c

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q23c))

# Calculate percentage distribution for Q29 (Did you purchase a wolf hunting license?)
percent_data_Q29 <- data %>%
  filter(!is.na(Q29)) %>%
  group_by(group, year, Q29) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
percent_data_Q29$year <- as.factor(percent_data_Q29$year)

# Plot percentage distribution of all responses
filtered_data_Q29 <- percent_data_Q29 %>%
  filter(Q29 != 5)

plot_percent_data_Q29 <- ggplot(filtered_data_Q29, aes(x = factor(Q29), y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(group ~ year, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Did you purchase a wolf hunting license?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  scale_x_discrete(labels = c("1" = "No", "2" = "Yes")) +
  theme(strip.background = element_blank())

plot_percent_data_Q29

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q29))

# Calculate percentage distribution for Q33 (Did you purchase a license to hunt deer or elk in Montana?)
percent_data_Q33 <- data %>%
  filter(!is.na(Q33)) %>%
  group_by(group, year, Q33) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
percent_data_Q33$year <- as.factor(percent_data_Q33$year)

# Plot percentage distribution of all responses
# Filter out rows where Q33 has a value of 4
filtered_data_Q33 <- percent_data_Q33 %>%
  filter(Q33 != 4)

plot_percent_data_Q33 <- ggplot(filtered_data_Q33, aes(x = factor(Q33), y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2), 
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(group ~ year, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Did you purchase a license to hunt deer or elk in Montana?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  scale_x_discrete(labels = c("1" = "No", "2" = "Yes")) +
  theme(strip.background = element_blank())

plot_percent_data_Q33

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q33))

# Calculate percentage distribution for Q35a (Do you or anyone in your family raise livestock in Montana?)
percent_data_Q35a <- data %>%
  filter(!is.na(Q35a)) %>%
  group_by(group, year, Q35a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
percent_data_Q35a$year <- as.factor(percent_data_Q35a$year)

# Plot percentage distribution of all responses
plot_percent_data_Q35a <- ggplot(percent_data_Q35a, aes(x = factor(Q35a), y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2), 
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(group ~ year, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Do you or anyone in your family raise livestock in Montana (any)?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  scale_x_discrete(labels = c("1" = "No", "2" = "Yes")) +
  theme(strip.background = element_blank())

plot_percent_data_Q35a

# Add the plot to the list of plots
list_of_plots2 <- append(list_of_plots2, list(plot_percent_data_Q35a))

# Initialize the pptx object
my_pptx2 <- read_pptx()

for (plot in list_of_plots2) {
  my_pptx2 <- my_pptx2 %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = plot, location = ph_location_type(type = "body"))
}

# Save the PowerPoint file
print(my_pptx2, target = "wolves_graphs_longitudinal.pptx")

plots_directory <- "C:/Users/maxbi/OneDrive - The University of Montana/WolfData/WolfDatasets/Final2023/PlotImages"


# Iterate over the list of plots and save each as an image
for (i in seq_along(list_of_plots2)) {
  
  # Define the filename for the plot with "17v23" prefix and plot number
  plot_filename <- paste0(plots_directory, "/long_plot_", i, ".png")
  
  # Save the plot as an image
  ggsave(plot_filename, plot = list_of_plots2[[i]], width = 9, height = 4.95)
}

