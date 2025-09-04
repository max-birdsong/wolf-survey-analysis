### Wolf Report Number 3 - 2023 ###
library(ggplot2)
library(tidyverse)
library(dplyr)
library(writexl)

# Set working directory 
setwd("C:/Users/maxbi/OneDrive - The University of Montana/WolfData/WolfDatasets/Final2023")

# Load  data
data <- read.csv("FINAL Quantitative Respondent Data Combined 2023.csv")
head(data)
library(scales)
library(officer)
library(rvg)
library(magrittr)
library(survey)

list_of_plots3 <- list()

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
data[data==9] <- NA

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

survey_design <- svydesign(ids = ~1, strata=~strata ,data = data, weights = ~FinalWt)

#####################

### Q1a - Wolves are beautiful animals ###
# Calculate mean response and its standard error
agg_data_Q1a <- svyby(~as.numeric(Q1a), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q1a_se <- svyby(~as.numeric(Q1a), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q1a_se$se_mean <- sqrt(agg_data_Q1a_se$se)
agg_data_Q1a_se$mean_response_total <- agg_data_Q1a_se$`as.numeric(Q1a)`
agg_data_Q1a_se <- agg_data_Q1a_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q1a <- merge(agg_data_Q1a, agg_data_Q1a_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q1a)[colnames(agg_data_Q1a) == "as.numeric(Q1a)"] <- "mean_response"
# Calculate percentage distribution
percent_data_Q1a <- data %>%
  filter(!is.na(Q1a)) %>%
  group_by(group, Q1a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q1a <- ggplot(agg_data_Q1a, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -5.8, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I think wolves are beautiful animals",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response"
  ) +
  scale_fill_brewer(palette = "Paired") + 
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q1a
list_of_plots3 <- list(plot_agg_data_Q1a)

# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q1a <- ggplot(percent_data_Q1a, aes(x = Q1a, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1)), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "I think wolves are beautiful animals",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1),limits = c(0, 100)) +  # Format y labels as percentage
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q1a
list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q1a))

### Q1d - Opinion on how wolves affect well-being ###

# Calculate mean response and its standard error
agg_data_Q1d <- svyby(~as.numeric(Q1d), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q1d_se <- svyby(~as.numeric(Q1d), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q1d_se$se_mean <- sqrt(agg_data_Q1d_se$se)
agg_data_Q1d_se$mean_response_total <- agg_data_Q1d_se$`as.numeric(Q1d)`
agg_data_Q1d_se <- agg_data_Q1d_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q1d <- merge(agg_data_Q1d, agg_data_Q1d_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q1d)[colnames(agg_data_Q1d) == "as.numeric(Q1d)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q1d <- data %>%
  filter(!is.na(Q1d)) %>%
  group_by(group, Q1d) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q1d <- ggplot(agg_data_Q1d, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -3.5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  labs(
    title = "Wolves negatively affect my economic well-being",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q1d

# Print the plot
print(plot_agg_data_Q1d)

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q1d))

# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q1d <- ggplot(percent_data_Q1d, aes(x = Q1d, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1)), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "Wolves negatively affect my economic well-being",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +  # Format y labels as percentage
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q1d

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q1d))

### Q1f - Opinion on how wolves are perceived as a burden ###

### Q1f - Opinion on how wolves are perceived as a burden ###

# Calculate mean response and its standard error
agg_data_Q1f <- svyby(~as.numeric(Q1f), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q1f_se <- svyby(~as.numeric(Q1f), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q1f_se$se_mean <- sqrt(agg_data_Q1f_se$se)
agg_data_Q1f_se$mean_response_total <- agg_data_Q1f_se$`as.numeric(Q1f)`
agg_data_Q1f_se <- agg_data_Q1f_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q1f <- merge(agg_data_Q1f, agg_data_Q1f_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q1f)[colnames(agg_data_Q1f) == "as.numeric(Q1f)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q1f <- data %>%
  filter(!is.na(Q1f)) %>%
  group_by(group, Q1f) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q1f <- ggplot(agg_data_Q1f, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -3, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Wolves are a burden I'd rather not deal with",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q1f

# Print the plot
print(plot_agg_data_Q1f)

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q1f))

# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q1f <- ggplot(percent_data_Q1f, aes(x = Q1f, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1)), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "Wolves are a burden I'd rather not deal with",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +  # Format y labels as percentage
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q1f

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q1f))


### Q2a - Wolves' Contribution to Outdoor Economy ###

### Q2a - [Your survey question here for context] ###

# Calculate mean response and its standard error
agg_data_Q2a <- svyby(~as.numeric(Q2a), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q2a_se <- svyby(~as.numeric(Q2a), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q2a_se$se_mean <- sqrt(agg_data_Q2a_se$se)
agg_data_Q2a_se$mean_response_total <- agg_data_Q2a_se$`as.numeric(Q2a)`
agg_data_Q2a_se <- agg_data_Q2a_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q2a <- merge(agg_data_Q2a, agg_data_Q2a_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q2a)[colnames(agg_data_Q2a) == "as.numeric(Q2a)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q2a <- data %>%
  filter(!is.na(Q2a)) %>%
  group_by(group, Q2a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q2a <- ggplot(agg_data_Q2a, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I think wolves contribute positively to the outdoor economy in Montana",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q2a

# Print the plot
print(plot_agg_data_Q2a)

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q2a))

# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q2a <- ggplot(percent_data_Q2a, aes(x = Q2a, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "I think wolves contribute positively to the outdoor economy in Montana",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q2a

# Print the plot
print(plot_percent_data_Q2a)
list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q2a))


### Q2b - Wolves Limiting Recreational Opportunities ###

# Calculate mean response and its standard error
agg_data_Q2b <- svyby(~as.numeric(Q2b), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q2b_se <- svyby(~as.numeric(Q2b), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q2b_se$se_mean <- sqrt(agg_data_Q2b_se$se)
agg_data_Q2b_se$mean_response_total <- agg_data_Q2b_se$`as.numeric(Q2b)`
agg_data_Q2b_se <- agg_data_Q2b_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q2b <- merge(agg_data_Q2b, agg_data_Q2b_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q2b)[colnames(agg_data_Q2b) == "as.numeric(Q2b)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q2b <- data %>%
  filter(!is.na(Q2b)) %>%
  group_by(group, Q2b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q2b <- ggplot(agg_data_Q2b, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -4, -2.2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Wolves Limit Recreational Opportunities",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q2b

# Print the plot
print(plot_agg_data_Q2b)

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q2b))

# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q2b <- ggplot(percent_data_Q2b, aes(x = Q2b, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "Wolves limit recreational opportunities",
    x = "",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q2b
list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q2b))

### Q2c - Concerns about Wolves Damaging Things of Value ###

# Calculate mean response and its standard error
agg_data_Q2c <- svyby(~as.numeric(Q2c), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q2c_se <- svyby(~as.numeric(Q2c), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q2c_se$se_mean <- sqrt(agg_data_Q2c_se$se)
agg_data_Q2c_se$mean_response_total <- agg_data_Q2c_se$`as.numeric(Q2c)`
agg_data_Q2c_se <- agg_data_Q2c_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q2c <- merge(agg_data_Q2c, agg_data_Q2c_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q2c)[colnames(agg_data_Q2c) == "as.numeric(Q2c)"] <- "mean_response"
# Calculate percentage distribution
percent_data_Q2c <- data %>%
  filter(!is.na(Q2c)) %>%
  group_by(group, Q2c) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q2c <- ggplot(agg_data_Q2c, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -3, -2.2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "I am concerned about wolves damaging things I care about",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired") 

plot_agg_data_Q2c

# Print the plot
print(plot_agg_data_Q2c)

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q2c))


# Plot percentage distribution
plot_percent_data_Q2c <- ggplot(percent_data_Q2c, aes(x = Q2c, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "I am concerned about wolves damaging things I care about",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q2c

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q2c))


### Q2d - Right of Wolves to Exist in Montana ###

# Calculate mean response and its standard error
agg_data_Q2d <- svyby(~as.numeric(Q2d), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q2d_se <- svyby(~as.numeric(Q2d), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q2d_se$se_mean <- sqrt(agg_data_Q2d_se$se)
agg_data_Q2d_se$mean_response_total <- agg_data_Q2d_se$`as.numeric(Q2d)`
agg_data_Q2d_se <- agg_data_Q2d_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q2d <- merge(agg_data_Q2d, agg_data_Q2d_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q2d)[colnames(agg_data_Q2d) == "as.numeric(Q2d)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q2d <- data %>%
  filter(!is.na(Q2d)) %>%
  group_by(group, Q2d) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q2d <- ggplot(agg_data_Q2d, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Without black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "Wolves have the right to exist in Montana",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

# Print the plot
plot_agg_data_Q2d

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q2d))

# Plot percentage distribution
plot_percent_data_Q2d <- ggplot(percent_data_Q2d, aes(x = Q2d, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "Wolves have the right to exist in Montana",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q2d

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q2d))


### Q2e - Feeling Personal Safety Threatened by Wolves ###
# Calculate mean response and its standard error
agg_data_Q2e <- svyby(~as.numeric(Q2e), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q2e_se <- svyby(~as.numeric(Q2e), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q2e_se$se_mean <- sqrt(agg_data_Q2e_se$se)
agg_data_Q2e_se$mean_response_total <- agg_data_Q2e_se$`as.numeric(Q2e)`
agg_data_Q2e_se <- agg_data_Q2e_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q2e <- merge(agg_data_Q2e, agg_data_Q2e_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q2e)[colnames(agg_data_Q2e) == "as.numeric(Q2e)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q2e <- data %>%
  filter(!is.na(Q2e)) %>%
  group_by(group, Q2e) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q2e <- ggplot(agg_data_Q2e, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Without black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -6, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "I feel my personal safety is threatened by wolves",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q2e

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q2e))

# Plot percentage distribution
plot_percent_data_Q2e <- ggplot(percent_data_Q2e, aes(x = Q2e, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "I feel my personal safety is threatened by wolves",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q2e
list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q2e))


### Q4 - Montana’s Current Wolf Population ###
# Calculate mean response and its standard error
agg_data_Q4 <- svyby(~as.numeric(Q4), by = ~group, design = survey_design, 
                     FUN = svymean, na.rm = TRUE)

agg_data_Q4_se <- svyby(~as.numeric(Q4), by = ~group, design = survey_design, 
                        FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q4_se$se_mean <- sqrt(agg_data_Q4_se$se)
agg_data_Q4_se$mean_response_total <- agg_data_Q4_se$`as.numeric(Q4)`
agg_data_Q4_se <- agg_data_Q4_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q4 <- merge(agg_data_Q4, agg_data_Q4_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q4)[colnames(agg_data_Q4) == "as.numeric(Q4)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q4 <- data %>%
  filter(!is.na(Q4)) %>%
  group_by(group, Q4) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q4 <- ggplot(agg_data_Q4, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # No black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(
    aes(label = sprintf("%.1f", mean_response), y = mean_response + 0.1),  # Round and format the label
    vjust = -0.5,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "What is your opinion regarding Montana's wolf population right now?",
    x = "On a scale from 1 (much too low) to 3 (about right) to 5 (much too high)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q4

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q4))

# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q4 <- ggplot(percent_data_Q4, aes(x = Q4, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "What is your opinion regarding Montana's wolf population right now",
    x = "On a scale from 1 (much too low) to 5 (much too high)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q4
list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q4))


### Q8a - Opinion on Wolves' Wariness Due to Hunting ###
# Calculate mean response and its standard error
agg_data_Q8a <- svyby(~as.numeric(Q8a), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q8a_se <- svyby(~as.numeric(Q8a), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q8a_se$se_mean <- sqrt(agg_data_Q8a_se$se)
agg_data_Q8a_se$mean_response_total <- agg_data_Q8a_se$`as.numeric(Q8a)`
agg_data_Q8a_se <- agg_data_Q8a_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q8a <- merge(agg_data_Q8a, agg_data_Q8a_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q8a)[colnames(agg_data_Q8a) == "as.numeric(Q8a)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q8a <- data %>%
  filter(!is.na(Q8a)) %>%
  group_by(group, Q8a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q8a <- ggplot(agg_data_Q8a, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # No black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "Hunting wolves makes them more wary of humans",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q8a

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q8a))

# Plot percentage distribution
plot_percent_data_Q8a <- ggplot(percent_data_Q8a, aes(x = Q8a, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "Hunting wolves makes them more wary of humans",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q8a

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q8a))

### Q8b - Opinion on Whether Not Hunting Wolves Makes Them More Comfortable Around Humans ###
# Calculate mean response and its standard error
agg_data_Q8b <- svyby(~as.numeric(Q8b), by = ~group, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q8b_se <- svyby(~as.numeric(Q8b), by = ~group, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q8b_se$se_mean <- sqrt(agg_data_Q8b_se$se)
agg_data_Q8b_se$mean_response_total <- agg_data_Q8b_se$`as.numeric(Q8b)`
agg_data_Q8b_se <- agg_data_Q8b_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q8b <- merge(agg_data_Q8b, agg_data_Q8b_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q8b)[colnames(agg_data_Q8b) == "as.numeric(Q8b)"] <- "mean_response"


# Calculate percentage distribution
percent_data_Q8b <- data %>%
  filter(!is.na(Q8b)) %>%
  group_by(group, Q8b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q8b <- ggplot(agg_data_Q8b, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Removed the black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "NOT hunting wolves makes them more comfortable around humans",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q8b
list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q8b))


# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q8b <- ggplot(percent_data_Q8b, aes(x = Q8b, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "NOT hunting wolves makes them more comfortable around humans",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q8b

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q8b))


### Q16a - Acceptability of Preventative Non-lethal Actions to Manage Conflicts with Wolves ###
# Calculate mean response and its standard error
agg_data_Q16a <- svyby(~as.numeric(Q16a), by = ~group, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q16a_se <- svyby(~as.numeric(Q16a), by = ~group, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q16a_se$se_mean <- sqrt(agg_data_Q16a_se$se)
agg_data_Q16a_se$mean_response_total <- agg_data_Q16a_se$`as.numeric(Q16a)`
agg_data_Q16a_se <- agg_data_Q16a_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q16a <- merge(agg_data_Q16a, agg_data_Q16a_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q16a)[colnames(agg_data_Q16a) == "as.numeric(Q16a)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q16a <- data %>%
  filter(!is.na(Q16a)) %>%
  group_by(group, Q16a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q16a <- ggplot(agg_data_Q16a, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Removed the black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -6.2, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "How acceptable are preventative non-lethal actions?",
    x = "On a scale from 1 (very unacceptable) to 5 (very acceptable)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q16a
list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q16a))


# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q16a <- ggplot(percent_data_Q16a, aes(x = Q16a, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "How acceptable are preventative non-lethal actions?",
    x = "On a scale from 1 (very unacceptable) to 5 (very acceptable)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q16a

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q16a))

### Q16b - Acceptability of Preventative Lethal Actions to Manage Conflicts with Wolves ###
# Calculate mean response and its standard error
agg_data_Q16b <- svyby(~as.numeric(Q16b), by = ~group, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q16b_se <- svyby(~as.numeric(Q16b), by = ~group, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q16b_se$se_mean <- sqrt(agg_data_Q16b_se$se)
agg_data_Q16b_se$mean_response_total <- agg_data_Q16b_se$`as.numeric(Q16b)`
agg_data_Q16b_se <- agg_data_Q16b_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q16b <- merge(agg_data_Q16b, agg_data_Q16b_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q16b)[colnames(agg_data_Q16b) == "as.numeric(Q16b)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q16b <- data %>%
  filter(!is.na(Q16b)) %>%
  group_by(group, Q16b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q16b <- ggplot(agg_data_Q16b, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Removed the black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -6.2, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "How acceptable are preventative lethal actions?",
    x = "On a scale from 1 (very unacceptable) to 5 (very acceptable)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q16b


# Print the plot
print(plot_agg_data_Q16b)

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q16b))


# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q16b <- ggplot(percent_data_Q16b, aes(x = Q16b, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "How acceptable are preventative lethal actions?",
    x = "On a scale from 1 (very unacceptable) to 5 (very acceptable)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q16b

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q16b))


### Q16c - Acceptability of Lethal Removal of Wolves after Livestock Attack ###
# Calculate mean response and its standard error
agg_data_Q16c <- svyby(~as.numeric(Q16c), by = ~group, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q16c_se <- svyby(~as.numeric(Q16c), by = ~group, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q16c_se$se_mean <- sqrt(agg_data_Q16c_se$se)
agg_data_Q16c_se$mean_response_total <- agg_data_Q16c_se$`as.numeric(Q16c)`
agg_data_Q16c_se <- agg_data_Q16c_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q16c <- merge(agg_data_Q16c, agg_data_Q16c_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q16c)[colnames(agg_data_Q16c) == "as.numeric(Q16c)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q16c <- data %>%
  filter(!is.na(Q16c)) %>%
  group_by(group, Q16c) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q16c <- ggplot(agg_data_Q16c, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Removed the black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  coord_cartesian(ylim = c(1, 5)) +
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -2, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How acceptable is lethal removal of wolves after they attack livestock?",
    x = "On a scale from 1 (very unacceptable) to 5 (very acceptable)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q16c


# Print the plot
print(plot_agg_data_Q16c)

list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q16c))


# Plot percentage distribution
# Create a ggplot with rounded data labels and no facet titles
plot_percent_data_Q16c <- ggplot(percent_data_Q16c, aes(x = Q16c, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "How acceptable is lethal removal of wolves after they attack livestock?",
    x = "On a scale from 1 (very unacceptable) to 5 (very acceptable)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q16c

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q16c))


### Q16d - Acceptability of Lethal Removal of Wolves as a Last Resort ###
# Calculate mean response and its standard error
agg_data_Q16d <- svyby(~as.numeric(Q16d), by = ~group, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q16d_se <- svyby(~as.numeric(Q16d), by = ~group, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q16d_se$se_mean <- sqrt(agg_data_Q16d_se$se)
agg_data_Q16d_se$mean_response_total <- agg_data_Q16d_se$`as.numeric(Q16d)`
agg_data_Q16d_se <- agg_data_Q16d_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q16d <- merge(agg_data_Q16d, agg_data_Q16d_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q16d)[colnames(agg_data_Q16d) == "as.numeric(Q16d)"] <- "mean_response"

# Calculate percentage distribution
percent_data_Q16d <- data %>%
  filter(!is.na(Q16d)) %>%
  group_by(group, Q16d) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create the plots
plot_agg_data_Q16d <- ggplot(agg_data_Q16d, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Removed the black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -6, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "How acceptable is lethal removal as a last resort?",
    x = "On a scale from 1 (very unacceptable) to 5 (very acceptable)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q16d
list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q16d))


# Plot percentage distribution
plot_percent_data_Q16d <- ggplot(percent_data_Q16d, aes(x = Q16d, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,  # Adjust vertical justification
    position = position_dodge(width = 0.7),
    size = 2.5  # Adjust label size
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "Lethal removal as a last resort",
    x = "On a scale from 1 (very unacceptable) to 5 (very acceptable)",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +  # Remove % symbol from labels
  theme(
    strip.text = element_blank(),        # Remove facet titles
    strip.background = element_blank()  # Remove background behind facet titles
  )

# To view the plot
plot_percent_data_Q16d

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q16d))

# Insert Q17 weighting
# Calculate the weighted percentage of interactions with wolves (Q17), filtering for values 1 and 2
# Calculate percentage distribution for Q17
percent_data_Q17 <- data %>%
  filter(Q17 %in% c(1, 2)) %>%
  group_by(group, Q17) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Plot percentage distribution with survey weights
plot_percent_data_Q17 <- ggplot(percent_data_Q17, aes(x = group, y = percentage, fill = factor(Q17, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage + 0.5), 
        group = factor(Q17)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "Have you ever had interactions with Wolves?",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9)
  )

plot_percent_data_Q17


list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q17))

# Calculate percentage distribution for Q17a1
percent_data_Q17a1 <- data %>%
  filter(Q17a1 %in% c(1, 2)) %>%
  group_by(group, Q17a1) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create a ggplot with rounded data labels and no facet titles for Q17a1
plot_percent_data_Q17a1 <- ggplot(percent_data_Q17a1, aes(x = group, y = percentage, fill = factor(Q17a1, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage + 0.5),
        group = factor(Q17a1)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "I have seen wolf tracks at least once",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9)
  )

plot_percent_data_Q17a1


list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q17a1))


# Calculate percentage distribution for Q17a2
percent_data_Q17a2 <- data %>%
  filter(Q17a2 %in% c(1, 2)) %>%
  group_by(group, Q17a2) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create a ggplot with rounded data labels and no facet titles for Q17a2
plot_percent_data_Q17a2 <- ggplot(percent_data_Q17a2, aes(x = group, y = percentage, fill = factor(Q17a2, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage + 0.5), 
        group = factor(Q17a2)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "I have seen wolf scat at least once",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9)
  )

plot_percent_data_Q17a2

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q17a2))


# Calculate percentage distribution for Q17a3
percent_data_Q17a3 <- data %>%
  filter(Q17a3 %in% c(1, 2)) %>%
  group_by(group, Q17a3) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create a ggplot with rounded data labels and no facet titles for Q17a3
plot_percent_data_Q17a3 <- ggplot(percent_data_Q17a3, aes(x = group, y = percentage, fill = factor(Q17a3, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage + 0.5), 
        group = factor(Q17a3)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "I have heard wolf howls at least once",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9)
  )

plot_percent_data_Q17a3


list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q17a3))

# Calculate percentage distribution for Q17a4
percent_data_Q17a4 <- data %>%
  filter(Q17a4 %in% c(1, 2)) %>%
  group_by(group, Q17a4) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create a ggplot with rounded data labels and no facet titles for Q17a4
plot_percent_data_Q17a4 <- ggplot(percent_data_Q17a4, aes(x = group, y = percentage, fill = factor(Q17a4, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage + 0.5), 
        group = factor(Q17a4)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "I have seen wolves very close to home",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9)
  )

plot_percent_data_Q17a4
list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q17a4))


# Calculate percentage distribution for Q17a5
percent_data_damaged_property <- data %>%
  filter(Q17a5 %in% c(1, 2)) %>%
  group_by(group, Q17a5) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create a ggplot with rounded data labels and no facet titles for Q17a5
plot_percent_data_damaged_property <- ggplot(percent_data_damaged_property, aes(x = group, y = percentage, fill = factor(Q17a5, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage + 0.5), 
        group = factor(Q17a5)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "Wolves have damaged my property at least once",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9)
  )

plot_percent_data_damaged_property
list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_damaged_property))

# Calculate percentage distribution for Q17a6
percent_data_known_damaged_property <- data %>%
  filter(Q17a6 %in% c(1, 2)) %>%
  group_by(group, Q17a6) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create a ggplot with rounded data labels and no facet titles for Q17a6
plot_percent_data_known_damaged_property <- ggplot(percent_data_known_damaged_property, aes(x = group, y = percentage, fill = factor(Q17a6, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage - 3),  # Ensure labels are within the bars
        group = factor(Q17a6)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "I know people whose property was damaged by wolves",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()  # Optional: remove minor grid lines for a cleaner look
  )

plot_percent_data_known_damaged_property

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_known_damaged_property))

# Calculate percentage distribution for Q17a7
percent_data_fear_interaction <- data %>%
  filter(Q17a7 %in% c(1, 2)) %>%
  group_by(group, Q17a7) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create a ggplot with rounded data labels and no facet titles for Q17a7
plot_percent_data_fear_interaction <- ggplot(percent_data_fear_interaction, aes(x = group, y = percentage, fill = factor(Q17a7, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage - 3), 
        group = factor(Q17a7)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "I have had interactions with wolves that made me fear for my safety",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),  # Optional: remove minor grid lines for a cleaner look
    legend.position = "right"  # Optional: move legend to bottom
  )

plot_percent_data_fear_interaction


list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_fear_interaction))


# Calculate percentage distribution for Q17a8
percent_data_enjoy_interaction <- data %>%
  filter(Q17a8 %in% c(1, 2)) %>%
  group_by(group, Q17a8) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Create a ggplot with rounded data labels and no facet titles for Q17a8
plot_percent_data_enjoy_interaction <- ggplot(percent_data_enjoy_interaction, aes(x = group, y = percentage, fill = factor(Q17a8, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), "%", sep = ""), 
        y = ifelse(percentage < 98, percentage + 2, percentage - 3), 
        group = factor(Q17a8)),
    position = position_dodge(width = 0.7),
    vjust = 0,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "I have had interactions with wolves that I enjoyed",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),  # Optional: remove minor grid lines for a cleaner look
    legend.position = "right"
  )

plot_percent_data_enjoy_interaction

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_enjoy_interaction))

### Q17b - [Provide description of Q17b here] ###

# Calculate mean response and its standard error
agg_data_Q17b <- svyby(~as.numeric(Q17b), by = ~group, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q17b_se <- svyby(~as.numeric(Q17b), by = ~group, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q17b_se$se_mean <- sqrt(agg_data_Q17b_se$se)
agg_data_Q17b_se$mean_response_total <- agg_data_Q17b_se$`as.numeric(Q17b)`
agg_data_Q17b_se <- agg_data_Q17b_se %>% select(group, se_mean, mean_response_total)

# Merge by group, using meaningful column names 
agg_data_Q17b <- merge(agg_data_Q17b, agg_data_Q17b_se, by = "group")

# Rename columns for clarity
colnames(agg_data_Q17b)[colnames(agg_data_Q17b) == "as.numeric(Q17b)"] <- "mean_response"


percent_data_Q17b <- data %>%
  filter(!is.na(Q17b)) %>%
  group_by(group, Q17b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Plot average response
plot_agg_data_Q17b <- ggplot(agg_data_Q17b, aes(x = group, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Removed the black outline
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf", -4, -2.2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal() +
  labs(
    title = "How would you rate the interactions you've had with wolves?",
    x = "On a scale from 1 (very negative) to 5 (very positive)",
    y = "Mean Response"
  ) +
  scale_fill_brewer(palette = "Paired")

plot_agg_data_Q17b

# Assuming list_of_plots3 has been initialized
list_of_plots3 <- append(list_of_plots3, list(plot_agg_data_Q17b))
percent_data_Q17b$Q17b <- as.factor(percent_data_Q17b$Q17b)


# Plot percentage distribution faceted by 'group' and with bars for each 'Q17b' score
plot_percent_data_Q17b <- ggplot(percent_data_Q17b, aes(x = Q17b, y = percentage, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""), y = percentage + 2),  # Round and format the label (without %)
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(~ group, scales = "free_y") +  # Facet by group with free y scales
  theme_minimal() +
  labs(
    title = "How would you rate the interactions you've had with wolves?",
    x = "Rating Score",
    y = "Percentage of Respondents"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 100)) +
  theme(
    strip.text = element_blank(),  # Remove facet titles
    strip.background = element_blank(),  # Remove background behind facet titles
    legend.position = "right"  # Hide the legend
  )

# To view the plot
plot_percent_data_Q17b


# Append to the list of plots
list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q17b))


# Aggregating data for Q34
percent_data_Q34<- data %>%
  filter(Q34 %in% c(1, 2)) %>%
  group_by(group, Q34) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Adding the new row
percent_data_Q34 <- percent_data_Q34 %>% 
  add_row(group = "Land", Q34 = 1, weighted_count = 0, percentage = 0)

percent_data_Q34

# Plot percentage distribution for Q34
plot_percent_data_Q34 <- ggplot(percent_data_Q34, aes(x = group, y = percentage, fill = factor(Q34, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""),  # Adjust for visibility
        group = factor(Q34)),
    position = position_dodge(width = 0.7),
    vjust = -1,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "Do You Personally Own 160 or More Acres of Land in Montana?",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 110)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()  # Optional: remove minor grid lines
  )

plot_percent_data_Q34

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q34))

# Aggregating data for Q28

percent_data_Q28 <- data %>%
  filter(Q28 %in% c(1, 2)) %>%
  group_by(group, Q28) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

plot_percent_data_Q28 <- ggplot(percent_data_Q28, aes(x = group, y = percentage, fill = factor(Q28, levels = c(1, 2), labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste(round(percentage, 1), sep = ""),  
        group = factor(Q28)),
    position = position_dodge(width = 0.7),
    vjust = -1,
    size = 2.5
  ) +
  theme_minimal() +
  labs(
    title = "Do you hunt?",
    x = "",
    y = "Percentage of Respondents",
    fill = "Response"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent_format(scale = 1, suffix = ""), limits = c(0, 110)) +
  theme(
    legend.title = element_text(size = 10), 
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

plot_percent_data_Q28

list_of_plots3 <- append(list_of_plots3, list(plot_percent_data_Q28))


# Initialize the pptx object
my_pptx3 <- read_pptx()

for (plot in list_of_plots3) {
  my_pptx3 <- my_pptx3 %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = plot, location = ph_location_type(type = "body"))
}

# Save the PowerPoint file
print(my_pptx3, target = "wolves_graphs_23.pptx")

plots_directory <- "C:/Users/maxbi/OneDrive - The University of Montana/WolfData/WolfDatasets/Final2023/PlotImages"


# Iterate over the list of plots and save each as an image
for (i in seq_along(list_of_plots3)) {
  
  # Define the filename for the plot with "17v23" prefix and plot number
  plot_filename <- paste0(plots_directory, "/23plot_", i, ".png")
  
  # Save the plot as an image
  ggsave(plot_filename, plot = list_of_plots3[[i]], width = 9, height = 4.95)
}


