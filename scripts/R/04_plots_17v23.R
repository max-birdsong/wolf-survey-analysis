### Wolf Report Number 2 - 17 v 23 ###
library(ggplot2)
library(tidyverse)
library(dplyr)
library(writexl)
library(tidyr)
library(dplyr)
library(survey)
library(hexbin)
library(officer)


# Set working directory 
setwd("C:/Users/maxbi/OneDrive - The University of Montana/WolfData/WolfDatasets/Final2023")

# Load  data
data <- read.csv("FINAL Quantitative Respondent Data Combined Long.csv")
head(data)
library(scales)
library(officer)
library(rvg)
library(magrittr)

list_of_plots <- list()

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
survey_design <- svydesign(ids = ~1, data = data, strata=~strata, weights = ~FinalWt)

#####################

# Perception of wolves posing a safety risk

# Calculate mean response and its standard error for Q1b
agg_data_Q1b <- svyby(~as.numeric(Q1b), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q1b_se <- svyby(~as.numeric(Q1b), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q1b_se$se_mean <- sqrt(agg_data_Q1b_se$se)
agg_data_Q1b_se$mean_response_total <- agg_data_Q1b_se$`as.numeric(Q1b)`
agg_data_Q1b_se <- agg_data_Q1b_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q1b <- merge(agg_data_Q1b, agg_data_Q1b_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q1b)[colnames(agg_data_Q1b) == "as.numeric(Q1b)"] <- "mean_response"
# Calculate percentage distribution for Q1b
percent_data_Q1b <- data %>%
  filter(!is.na(Q1b)) %>%
  group_by(group, year, Q1b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q1b$year <- as.factor(agg_data_Q1b$year)
percent_data_Q1b$year <- as.factor(percent_data_Q1b$year)


# Create the bar plot
plot_agg_data_Q1b <- ggplot(agg_data_Q1b[agg_data_Q1b$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I thnk wolves pose a safety risk to people I care about",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q1b

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q1b))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q1b <- ggplot(percent_data_Q1b, aes(x = factor(Q1b), y = percentage, fill = year)) +
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
    title = "I thnk wolves pose a safety risk to people I care about",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q1b


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q1b))

# Perception of wolves' importance for ecosystem health

# Calculate mean response and its standard error for Q1c
agg_data_Q1c <- svyby(~as.numeric(Q1c), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q1c_se <- svyby(~as.numeric(Q1c), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q1c_se$se_mean <- sqrt(agg_data_Q1c_se$se)
agg_data_Q1c_se$mean_response_total <- agg_data_Q1c_se$`as.numeric(Q1c)`
agg_data_Q1c_se <- agg_data_Q1c_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q1c <- merge(agg_data_Q1c, agg_data_Q1c_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q1c)[colnames(agg_data_Q1c) == "as.numeric(Q1c)"] <- "mean_response"


# Calculate percentage distribution for Q1c
percent_data_Q1c <- data %>%
  filter(!is.na(Q1c)) %>%
  group_by(group, year, Q1c) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q1c$year <- as.factor(agg_data_Q1c$year)
percent_data_Q1c$year <- as.factor(percent_data_Q1c$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q1c <- ggplot(agg_data_Q1c[agg_data_Q1c$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -3.5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I think wolves are important for ecosystem health",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q1c

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q1c))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q1c <- ggplot(percent_data_Q1c, aes(x = factor(Q1c), y = percentage, fill = year)) +
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
    title = "I think wolves are important for ecosystem health",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q1c


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q1c))

# Perception of enjoyment knowing wolves exist in Montana

# Calculate mean response and its standard error for Q1e
agg_data_Q1e <- svyby(~as.numeric(Q1e), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q1e_se <- svyby(~as.numeric(Q1e), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q1e_se$se_mean <- sqrt(agg_data_Q1e_se$se)
agg_data_Q1e_se$mean_response_total <- agg_data_Q1e_se$`as.numeric(Q1e)`
agg_data_Q1e_se <- agg_data_Q1e_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q1e <- merge(agg_data_Q1e, agg_data_Q1e_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q1e)[colnames(agg_data_Q1e) == "as.numeric(Q1e)"] <- "mean_response"
# Calculate percentage distribution for Q1e
percent_data_Q1e <- data %>%
  filter(!is.na(Q1e)) %>%
  group_by(group, year, Q1e) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q1e$year <- as.factor(agg_data_Q1e$year)
percent_data_Q1e$year <- as.factor(percent_data_Q1e$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q1e <- ggplot(agg_data_Q1e[agg_data_Q1e$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4.5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I enjoy knowing that wolves exist in Montana",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q1e

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q1e))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q1e <- ggplot(percent_data_Q1e, aes(x = factor(Q1e), y = percentage, fill = year)) +
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
    title = "I enjoy knowing that wolves exist in Montana",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q1e


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q1e))

# Calculate percentage distribution for Q7a
percent_data_Q7a <- data %>%
  filter(!is.na(Q7a)) %>%
  group_by(group, year, Q7a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Filter out rows with NA values in Q7a
percent_data_Q7a <- percent_data_Q7a %>% filter(!is.na(Q7a))

# Convert 'year' and 'Q7a' to factor variables
percent_data_Q7a$year <- as.factor(percent_data_Q7a$year)
percent_data_Q7a$Q7a <- factor(percent_data_Q7a$Q7a, levels = c(1, 2), labels = c("No", "Yes"))

plot_percent_data_Q7a <- ggplot(percent_data_Q7a, aes(x = Q7a, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2), 
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(~ group, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Has your tolerance of wolf hunting in the state changed over time?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q7a

list_of_plots <- append(list_of_plots, list(plot_percent_data_Q7a))

# Calculate percentage distribution for Q7b
percent_data_Q7b <- data %>%
  filter(!is.na(Q7b)) %>%
  group_by(group, year, Q7b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Filter out rows with NA values in Q7b
percent_data_Q7b <- percent_data_Q7b %>% filter(!is.na(Q7b))

# Convert 'year' and 'Q7b' to factor variables with specified labels
percent_data_Q7b$year <- as.factor(percent_data_Q7b$year)
percent_data_Q7b$Q7b <- factor(percent_data_Q7b$Q7b, levels = c(1, 2), labels = c("More", "Less"))

plot_percent_data_Q7b <- ggplot(percent_data_Q7b, aes(x = Q7b, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2), 
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(~ group, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "How has your tolerance of wolf hunting changed?",
    x = "More or less tolerant",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q7b

list_of_plots <- append(list_of_plots, list(plot_percent_data_Q7b))


# Satisfaction with the 2022-23 Montana wolf TRAPPING regulations

# Calculate mean response and its standard error for Q9
agg_data_Q9 <- svyby(~as.numeric(Q9), by = ~group + year, design = survey_design, 
                     FUN = svymean, na.rm = TRUE)

agg_data_Q9_se <- svyby(~as.numeric(Q9), by = ~group + year, design = survey_design, 
                        FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q9_se$se_mean <- sqrt(agg_data_Q9_se$se)
agg_data_Q9_se$mean_response_total <- agg_data_Q9_se$`as.numeric(Q9)`
agg_data_Q9_se <- agg_data_Q9_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q9 <- merge(agg_data_Q9, agg_data_Q9_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q9)[colnames(agg_data_Q9) == "as.numeric(Q9)"] <- "mean_response"

# Calculate percentage distribution for Q9
percent_data_Q9 <- data %>%
  filter(!is.na(Q9)) %>%
  group_by(group, year, Q9) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q9$year <- as.factor(agg_data_Q9$year)
percent_data_Q9$year <- as.factor(percent_data_Q9$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q9 <- ggplot(agg_data_Q9[agg_data_Q9$year %in% c("2017", "2023"),], 
                           aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How satisfied are you with the Montana wolf trapping regulations?",
    x = "On a scale from 1 (very dissatisfied) to 5 (very satisfied)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q9


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q9))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q9 <- ggplot(percent_data_Q9, aes(x = Q9, y = percentage, fill = year)) +
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
    title = "How satisfied are you with the Montana wolf trapping regulations?",
    x = "On a scale from 1 (very dissatisfied) to 5 (very satisfied)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q9

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q9))

# Tolerance towards wolf TRAPPING in Montana

# Calculate mean response and its standard error for Q10
agg_data_Q10 <- svyby(~as.numeric(Q10), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q10_se <- svyby(~as.numeric(Q10), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q10_se$se_mean <- sqrt(agg_data_Q10_se$se)
agg_data_Q10_se$mean_response_total <- agg_data_Q10_se$`as.numeric(Q10)`
agg_data_Q10_se <- agg_data_Q10_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q10 <- merge(agg_data_Q10, agg_data_Q10_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q10)[colnames(agg_data_Q10) == "as.numeric(Q10)"] <- "mean_response"
# Calculate percentage distribution for Q10
percent_data_Q10 <- data %>%
  filter(!is.na(Q10)) %>%
  group_by(group, year, Q10) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q10$year <- as.factor(agg_data_Q10$year)
percent_data_Q10$year <- as.factor(percent_data_Q10$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q10 <- ggplot(agg_data_Q10[agg_data_Q10$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response)), 
            position = position_dodge(0.7), 
            vjust = -2.5, 
            size = 3.0) +  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How tolerant are you with wolf trapping in Montana?",
    x = "On a scale from 1 (very intolerant) to 5 (very tolerant)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q10


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q10))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q10 <- ggplot(percent_data_Q10, aes(x = Q10, y = percentage, fill = year)) +
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
    title = "How tolerant are you with wolf trapping in Montana?",
    x = "On a scale from 1 (very intolerant) to 5 (very tolerant)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q10

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q10))

# Calculate percentage distribution for Q11a
percent_data_Q11a <- data %>%
  filter(!is.na(Q11a)) %>%
  group_by(group, year, Q11a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Filter out rows with NA values in Q11a
percent_data_Q11a <- percent_data_Q11a %>% filter(!is.na(Q11a))

# Convert 'year' and 'Q11a' to factor variables
percent_data_Q11a$year <- as.factor(percent_data_Q11a$year)
percent_data_Q11a$Q11a <- factor(percent_data_Q11a$Q11a, levels = c(1, 2), labels = c("No", "Yes"))

# Plot for Q11a
plot_percent_data_Q11a <- ggplot(percent_data_Q11a, aes(x = Q11a, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(~ group, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Has your tolerance of wolf trapping in the state changed over time?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  theme(strip.background = element_blank())

# Append to list_of_plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q11a))

# Calculate percentage distribution for Q11b
percent_data_Q11b <- data %>%
  filter(!is.na(Q11b)) %>%
  group_by(group, year, Q11b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Filter out rows with NA values in Q11b
percent_data_Q11b <- percent_data_Q11b %>% filter(!is.na(Q11b))

# Convert 'year' and 'Q11b' to factor variables with specified labels
percent_data_Q11b$year <- as.factor(percent_data_Q11b$year)
percent_data_Q11b$Q11b <- factor(percent_data_Q11b$Q11b, levels = c(1, 2), labels = c("More", "Less"))

# Plot for Q11b
plot_percent_data_Q11b <- ggplot(percent_data_Q11b, aes(x = Q11b, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2),
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(~ group, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "How has your tolerance of wolf hunting changed?",
    x = "More or less tolerant",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q11b

# Append to list_of_plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q11b))


# Opinion regarding the length of the Montana wolf HUNTING season

# Calculate mean response and its standard error for Q12
agg_data_Q12 <- svyby(~as.numeric(Q12), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q12_se <- svyby(~as.numeric(Q12), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q12_se$se_mean <- sqrt(agg_data_Q12_se$se)
agg_data_Q12_se$mean_response_total <- agg_data_Q12_se$`as.numeric(Q12)`
agg_data_Q12_se <- agg_data_Q12_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q12 <- merge(agg_data_Q12, agg_data_Q12_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q12)[colnames(agg_data_Q12) == "as.numeric(Q12)"] <- "mean_response"

# Calculate percentage distribution for Q12
percent_data_Q12 <- data %>%
  filter(!is.na(Q12)) %>%
  group_by(group, year, Q12) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q12$year <- as.factor(agg_data_Q12$year)
percent_data_Q12$year <- as.factor(percent_data_Q12$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q12 <- ggplot(agg_data_Q12[agg_data_Q12$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -3, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "What is your opinion regarding the length of the Montana wolf hunting season?",
    x = "On a scale from 1 (much too short) to 5 (much too long)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q12

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q12))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q12 <- ggplot(percent_data_Q12, aes(x = Q12, y = percentage, fill = year)) +
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
    title = "What is your opinion regarding the length of the Montana wolf hunting season?",
    x = "On a scale from 1 (much too short) to 5 (much too long)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q12

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q12))

# Opinion regarding the length of the Montana wolf TRAPPING season

# Calculate mean response and its standard error for Q13
agg_data_Q13 <- svyby(~as.numeric(Q13), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q13_se <- svyby(~as.numeric(Q13), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Adjust the names right away to avoid confusion later
agg_data_Q13_se$se_mean <- sqrt(agg_data_Q13_se$se)
agg_data_Q13_se$mean_response_total <- agg_data_Q13_se$`as.numeric(Q13)`
agg_data_Q13_se <- agg_data_Q13_se %>% select(group, year, se_mean, mean_response_total)

# Merge by group and year, using meaningful column names 
agg_data_Q13 <- merge(agg_data_Q13, agg_data_Q13_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q13)[colnames(agg_data_Q13) == "as.numeric(Q13)"] <- "mean_response"

# Calculate percentage distribution for Q13
percent_data_Q13 <- data %>%
  filter(!is.na(Q13)) %>%
  group_by(group, year, Q13) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q13$year <- as.factor(agg_data_Q13$year)
percent_data_Q13$year <- as.factor(percent_data_Q13$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q13 <- ggplot(agg_data_Q13[agg_data_Q13$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -3.5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "What is your opinion regarding the length of the wolf trapping season?",
    x = "On a scale from 1 (much too short) to 5 (much too long)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q13

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q13))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q13 <- ggplot(percent_data_Q13, aes(x = Q13, y = percentage, fill = year)) +
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
    title = "What is your opinion regarding the length of the wolf trapping season?",
    x = "On a scale from 1 (much too short) to 5 (much too long)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q13

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q13))

# Opinion regarding the bag limit for Montana wolf HUNTING/TRAPPING season

# Calculate mean response and its standard error for Q14
agg_data_Q14 <- svyby(~as.numeric(Q14), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q14_se <- svyby(~as.numeric(Q14), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q14_se$se_mean <- sqrt(agg_data_Q14_se$se)
agg_data_Q14_se$mean_response_total <- agg_data_Q14_se$`as.numeric(Q14)`
agg_data_Q14_se <- agg_data_Q14_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q14 <- merge(agg_data_Q14, agg_data_Q14_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q14)[colnames(agg_data_Q14) == "as.numeric(Q14)"] <- "mean_response"

# Calculate percentage distribution for Q14
percent_data_Q14 <- data %>%
  filter(!is.na(Q14)) %>%
  group_by(group, year, Q14) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q14$year <- as.factor(agg_data_Q14$year)
percent_data_Q14$year <- as.factor(percent_data_Q14$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q14 <- ggplot(agg_data_Q14[agg_data_Q14$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -3, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Opinion on Bag Limit for Montana Wolf Hunting/Trapping Season",
    x = "On a scale from 1 (way too few) to 5 (way too many)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q14

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q14))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q14 <- ggplot(percent_data_Q14, aes(x = Q14, y = percentage, fill = year)) +
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
    title = "Opinion on Bag Limit for Montana Wolf Hunting/Trapping Season",
    x = "On a scale from 1 (way too few) to 5 (way too many)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q14

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q14))

# Opinion regarding controlling wolf populations threatening big game populations

# Calculate mean response and its standard error for Q15a
agg_data_Q15a <- svyby(~as.numeric(Q15a), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q15a_se <- svyby(~as.numeric(Q15a), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q15a_se$se_mean <- sqrt(agg_data_Q15a_se$se)
agg_data_Q15a_se$mean_response_total <- agg_data_Q15a_se$`as.numeric(Q15a)`
agg_data_Q15a_se <- agg_data_Q15a_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q15a <- merge(agg_data_Q15a, agg_data_Q15a_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q15a)[colnames(agg_data_Q15a) == "as.numeric(Q15a)"] <- "mean_response"

# Calculate percentage distribution for Q15a
percent_data_Q15a <- data %>%
  filter(!is.na(Q15a)) %>%
  group_by(group, year, Q15a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q15a$year <- as.factor(agg_data_Q15a$year)
percent_data_Q15a$year <- as.factor(percent_data_Q15a$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q15a <- ggplot(agg_data_Q15a[agg_data_Q15a$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response)), 
            position = position_dodge(0.7), 
            vjust = -2, 
            size = 3.0) +  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Do you agree that wolf populations should be controlled when they threaten big game?",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q15a


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q15a))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q15a <- ggplot(percent_data_Q15a, aes(x = Q15a, y = percentage, fill = year)) +
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
    title = "Do you agree that wolf populations should be controlled when they threaten big game?",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q15a

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q15a))

# Opinion regarding controlling wolf populations sighted near human development

# Calculate mean response and its standard error for Q15b
agg_data_Q15b <- svyby(~as.numeric(Q15b), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q15b_se <- svyby(~as.numeric(Q15b), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q15b_se$se_mean <- sqrt(agg_data_Q15b_se$se)
agg_data_Q15b_se$mean_response_total <- agg_data_Q15b_se$`as.numeric(Q15b)`
agg_data_Q15b_se <- agg_data_Q15b_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q15b <- merge(agg_data_Q15b, agg_data_Q15b_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q15b)[colnames(agg_data_Q15b) == "as.numeric(Q15b)"] <- "mean_response"

# Calculate percentage distribution for Q15b
percent_data_Q15b <- data %>%
  filter(!is.na(Q15b)) %>%
  group_by(group, year, Q15b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q15b$year <- as.factor(agg_data_Q15b$year)
percent_data_Q15b$year <- as.factor(percent_data_Q15b$year)

# Create a bar plot for average response by group and year
plot_agg_data_Q15b <- ggplot(agg_data_Q15b[agg_data_Q15b$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -3.5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Do you agree that wolf populations should be controlled when they are sighted near human development?",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q15b


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q15b))

# Create a bar plot for percentage distribution by response and year
plot_percent_data_Q15b <- ggplot(percent_data_Q15b, aes(x = Q15b, y = percentage, fill = year)) +
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
    title = "Do you agree that wolf populations should be controlled when they are sighted near human development?",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q15b

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q15b))

# Opinion regarding controlling wolf populations when they threaten livestock

# Calculate mean response and its standard error for Q15c
agg_data_Q15c <- svyby(~as.numeric(Q15c), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q15c_se <- svyby(~as.numeric(Q15c), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q15c_se$se_mean <- sqrt(agg_data_Q15c_se$se)
agg_data_Q15c_se$mean_response_total <- agg_data_Q15c_se$`as.numeric(Q15c)`
agg_data_Q15c_se <- agg_data_Q15c_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q15c <- merge(agg_data_Q15c, agg_data_Q15c_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q15c)[colnames(agg_data_Q15c) == "as.numeric(Q15c)"] <- "mean_response"

# Calculate percentage distribution for Q15c
percent_data_Q15c <- data %>%
  filter(!is.na(Q15c)) %>%
  group_by(group, year, Q15c) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q15c$year <- as.factor(agg_data_Q15c$year)
percent_data_Q15c$year <- as.factor(percent_data_Q15c$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q15c <- ggplot(agg_data_Q15c[agg_data_Q15c$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response)), 
            position = position_dodge(0.7), 
            vjust = -2, 
            size = 3.0) +  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Do you agree that wolf populations should be controlled when they threaten livestock?",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q15c

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q15c))

# Plot percentage distribution
plot_percent_data_Q15c <- ggplot(percent_data_Q15c, aes(x = Q15c, y = percentage, fill = year)) +
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
    title = "Do you agree that wolf populations should be controlled when they threaten livestock?",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q15c

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q15c))

# Opinion regarding controlling wolf populations when they threaten pets

# Calculate mean response and its standard error for Q15d
agg_data_Q15d <- svyby(~as.numeric(Q15d), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q15d_se <- svyby(~as.numeric(Q15d), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q15d_se$se_mean <- sqrt(agg_data_Q15d_se$se)
agg_data_Q15d_se$mean_response_total <- agg_data_Q15d_se$`as.numeric(Q15d)`
agg_data_Q15d_se <- agg_data_Q15d_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q15d <- merge(agg_data_Q15d, agg_data_Q15d_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q15d)[colnames(agg_data_Q15d) == "as.numeric(Q15d)"] <- "mean_response"

# Calculate percentage distribution for Q15d
percent_data_Q15d <- data %>%
  filter(!is.na(Q15d)) %>%
  group_by(group, year, Q15d) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()


# Convert 'year' to a factor variable
agg_data_Q15d$year <- as.factor(agg_data_Q15d$year)
percent_data_Q15d$year <- as.factor(percent_data_Q15d$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q15d <- ggplot(agg_data_Q15d[agg_data_Q15d$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Do you agree that wolf populations should be controlled when they threaten pets?",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q15d

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q15d))

# Plot percentage distribution
plot_percent_data_Q15d <- ggplot(percent_data_Q15d, aes(x = Q15d, y = percentage, fill = year)) +
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
    title = "Do you agree that wolf populations should be controlled when they threaten pets?",
    x = "On a scale from 1 (strongly disagree) to 5 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q15d

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q15d))

# Opinion regarding controlling wolf populations when they threaten pets

# Calculate percentage distribution for Q21
percent_data_Q21 <- data %>%
  filter(!is.na(Q21)) %>%
  group_by(group, year, Q21) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Filter out rows with NA values in Q21
percent_data_Q21 <- percent_data_Q21 %>% filter(!is.na(Q21))

# Convert 'year' and 'Q21' to factor variables
percent_data_Q21$year <- as.factor(percent_data_Q21$year)
percent_data_Q21$Q21 <- factor(percent_data_Q21$Q21, levels = c(1, 2), labels = c("No", "Yes"))

# Plot percentage distribution
plot_percent_data_Q21 <- ggplot(percent_data_Q21, aes(x = Q21, y = percentage, fill = year)) +
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
    title = "Did you participate in the 2022-23 Montana Wolf Hunt/Trapping Authorization Process",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q21
# Append to the list
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q21))


# Satisfaction with wolf management in Montana

# Calculate mean response and its standard error for Q24
agg_data_Q24 <- svyby(~as.numeric(Q24), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q24_se <- svyby(~as.numeric(Q24), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q24_se$se_mean <- sqrt(agg_data_Q24_se$se)
agg_data_Q24_se$mean_response_total <- agg_data_Q24_se$`as.numeric(Q24)`
agg_data_Q24_se <- agg_data_Q24_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q24 <- merge(agg_data_Q24, agg_data_Q24_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q24)[colnames(agg_data_Q24) == "as.numeric(Q24)"] <- "mean_response"

# Calculate percentage distribution for Q24
percent_data_Q24 <- data %>%
  filter(!is.na(Q24)) %>%
  group_by(group, year, Q24) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q24$year <- as.factor(agg_data_Q24$year)
percent_data_Q24$year <- as.factor(percent_data_Q24$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q24 <- ggplot(agg_data_Q24[agg_data_Q24$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How satisfied are you with wolf management in Montana?",
    x = "On a scale from 1 (very unsatisfied) to 5 (very satisfied)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q24

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q24))

# Plot percentage distribution
plot_percent_data_Q24 <- ggplot(percent_data_Q24, aes(x = Q24, y = percentage, fill = year)) +
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
    title = "How satisfied are you with wolf management in Montana?",
    x = "On a scale from 1 (very unsatisfied) to 5 (very satisfied)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q24

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q24))

# Confidence in Montana Fish, Wildlife & Park's wolf management

# Calculate mean response and its standard error for Q25
agg_data_Q25 <- svyby(~as.numeric(Q25), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q25_se <- svyby(~as.numeric(Q25), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q25_se$se_mean <- sqrt(agg_data_Q25_se$se)
agg_data_Q25_se$mean_response_total <- agg_data_Q25_se$`as.numeric(Q25)`
agg_data_Q25_se <- agg_data_Q25_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q25 <- merge(agg_data_Q25, agg_data_Q25_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q25)[colnames(agg_data_Q25) == "as.numeric(Q25)"] <- "mean_response"
# Calculate percentage distribution for Q21
percent_data_Q25 <- data %>%
  filter(!is.na(Q25)) %>%
  group_by(group, year, Q25) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q25$year <- as.factor(agg_data_Q25$year)
percent_data_Q25$year <- as.factor(percent_data_Q25$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q25 <- ggplot(agg_data_Q25[agg_data_Q25$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "How confident are you with FWP's ability to manage wolves?",
    x = "On a scale from 1 (very not confident) to 5 (very confident)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q25

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q25))

# Plot percentage distribution
plot_percent_data_Q25 <- ggplot(percent_data_Q25, aes(x = Q25, y = percentage, fill = year)) +
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
    title = "How confident are you with FWP's ability to manage wolves?",
    x = "On a scale from 1 (very not confident) to 5 (very confident)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q25

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q25))

# Calculate percentage distribution for Q26a
percent_data_Q26a <- data %>%
  filter(!is.na(Q26a)) %>%
  group_by(group, year, Q26a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Filter out rows with NA values in Q26a
percent_data_Q26a <- percent_data_Q26a %>% filter(!is.na(Q26a))

# Convert 'year' and 'Q26a' to factor variables
percent_data_Q26a$year <- as.factor(percent_data_Q26a$year)
percent_data_Q26a$Q26a <- factor(percent_data_Q26a$Q26a, levels = c(1, 2), labels = c("No", "Yes"))

# Plot for Q26a
plot_percent_data_Q26a <- ggplot(percent_data_Q26a, aes(x = Q26a, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2), 
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(~ group, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Has your confidence in FWP changed over time?",
    x = "",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q26a

# Append to list_of_plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q26a))

# Opinion on managing fish and wildlife populations for human benefit

# Calculate percentage distribution for Q26b
percent_data_Q26b <- data %>%
  filter(!is.na(Q26b)) %>%
  group_by(group, year, Q26b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Filter out rows with NA values in Q26b
percent_data_Q26b <- percent_data_Q26b %>% filter(!is.na(Q26b))

# Convert 'year' and 'Q26b' to factor variables with specified labels
percent_data_Q26b$year <- as.factor(percent_data_Q26b$year)
percent_data_Q26b$Q26b <- factor(percent_data_Q26b$Q26b, levels = c(1, 2), labels = c("More", "Less"))

# Plot for Q26b
plot_percent_data_Q26b <- ggplot(percent_data_Q26b, aes(x = Q26b, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2), 
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(~ group, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "How has your confidence changed?",
    x = "More or less confident?",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q26b

# Append to list_of_plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q26b))

# Calculate mean response and its standard error for Q27a
agg_data_Q27a <- svyby(~as.numeric(Q27a), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27a_se <- svyby(~as.numeric(Q27a), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27a_se$se_mean <- sqrt(agg_data_Q27a_se$se)
agg_data_Q27a_se$mean_response_total <- agg_data_Q27a_se$`as.numeric(Q27a)`
agg_data_Q27a_se <- agg_data_Q27a_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27a <- merge(agg_data_Q27a, agg_data_Q27a_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27a)[colnames(agg_data_Q27a) == "as.numeric(Q27a)"] <- "mean_response"
# Calculate percentage distribution for Q27a
percent_data_Q27a <- data %>%
  filter(!is.na(Q27a)) %>%
  group_by(group, year, Q27a) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27a$year <- as.factor(agg_data_Q27a$year)
percent_data_Q27a$year <- as.factor(percent_data_Q27a$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27a <- ggplot(agg_data_Q27a[agg_data_Q27a$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Humans should manage fish and wildlife for human benefit",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27a


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27a))

# Plot percentage distribution
plot_percent_data_Q27a <- ggplot(percent_data_Q27a, aes(x = Q27a, y = percentage, fill = year)) +
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
    title = "Humans should manage fish and wildlife for human benefit",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27a

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27a))

# Opinion on animals having rights similar to humans

# Calculate mean response and its standard error for Q27b
agg_data_Q27b <- svyby(~as.numeric(Q27b), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27b_se <- svyby(~as.numeric(Q27b), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27b_se$se_mean <- sqrt(agg_data_Q27b_se$se)
agg_data_Q27b_se$mean_response_total <- agg_data_Q27b_se$`as.numeric(Q27b)`
agg_data_Q27b_se <- agg_data_Q27b_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27b <- merge(agg_data_Q27b, agg_data_Q27b_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27b)[colnames(agg_data_Q27b) == "as.numeric(Q27b)"] <- "mean_response"

# Calculate percentage distribution for Q27b
percent_data_Q27b <- data %>%
  filter(!is.na(Q27b)) %>%
  group_by(group, year, Q27b) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27b$year <- as.factor(agg_data_Q27b$year)
percent_data_Q27b$year <- as.factor(percent_data_Q27b$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27b <- ggplot(agg_data_Q27b[agg_data_Q27b$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Animals should have rights similar to Humans",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27b


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27b))

# Plot percentage distribution
plot_percent_data_Q27b <- ggplot(percent_data_Q27b, aes(x = Q27b, y = percentage, fill = year)) +
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
    title = "Animals should have rights similar to Humans",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27b

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27b))

# Opinion on viewing all living things as part of one big family

# Calculate mean response and its standard error for Q27c
agg_data_Q27c <- svyby(~as.numeric(Q27c), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27c_se <- svyby(~as.numeric(Q27c), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27c_se$se_mean <- sqrt(agg_data_Q27c_se$se)
agg_data_Q27c_se$mean_response_total <- agg_data_Q27c_se$`as.numeric(Q27c)`
agg_data_Q27c_se <- agg_data_Q27c_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27c <- merge(agg_data_Q27c, agg_data_Q27c_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27c)[colnames(agg_data_Q27c) == "as.numeric(Q27c)"] <- "mean_response"

# Calculate percentage distribution for Q27c
percent_data_Q27c <- data %>%
  filter(!is.na(Q27c)) %>%
  group_by(group, year, Q27c) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27c$year <- as.factor(agg_data_Q27c$year)
percent_data_Q27c$year <- as.factor(percent_data_Q27c$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27c <- ggplot(agg_data_Q27c[agg_data_Q27c$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I view all living things as part of one big family",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27c

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27c))

# Plot percentage distribution
plot_percent_data_Q27c <- ggplot(percent_data_Q27c, aes(x = Q27c, y = percentage, fill = year)) +
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
    title = "I view all living things as part of one big family",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27c

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27c))

# Opinion on the statement "Hunting does not respect the lives of animals"

# Calculate mean response and its standard error for Q27d
agg_data_Q27d <- svyby(~as.numeric(Q27d), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27d_se <- svyby(~as.numeric(Q27d), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27d_se$se_mean <- sqrt(agg_data_Q27d_se$se)
agg_data_Q27d_se$mean_response_total <- agg_data_Q27d_se$`as.numeric(Q27d)`
agg_data_Q27d_se <- agg_data_Q27d_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27d <- merge(agg_data_Q27d, agg_data_Q27d_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27d)[colnames(agg_data_Q27d) == "as.numeric(Q27d)"] <- "mean_response"

# Calculate percentage distribution for Q27d
percent_data_Q27d <- data %>%
  filter(!is.na(Q27d)) %>%
  group_by(group, year, Q27d) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27d$year <- as.factor(agg_data_Q27d$year)
percent_data_Q27d$year <- as.factor(percent_data_Q27d$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27d <- ggplot(agg_data_Q27d[agg_data_Q27d$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response)), 
            position = position_dodge(0.7), 
            vjust = -1.5, 
            size = 3.0) +  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Hunting does not respect the lives of animals'",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27d


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27d))

# Plot percentage distribution
plot_percent_data_Q27d <- ggplot(percent_data_Q27d, aes(x = Q27d, y = percentage, fill = year)) +
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
    title = "Hunting does not respect the lives of animals",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27d

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27d))

# Opinion on the statement "I feel a strong emotional bond with animals"

# Calculate mean response and its standard error for Q27e
agg_data_Q27e <- svyby(~as.numeric(Q27e), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27e_se <- svyby(~as.numeric(Q27e), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27e_se$se_mean <- sqrt(agg_data_Q27e_se$se)
agg_data_Q27e_se$mean_response_total <- agg_data_Q27e_se$`as.numeric(Q27e)`
agg_data_Q27e_se <- agg_data_Q27e_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27e <- merge(agg_data_Q27e, agg_data_Q27e_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27e)[colnames(agg_data_Q27e) == "as.numeric(Q27e)"] <- "mean_response"

# Calculate percentage distribution for Q27e
percent_data_Q27e <- data %>%
  filter(!is.na(Q27e)) %>%
  group_by(group, year, Q27e) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27e$year <- as.factor(agg_data_Q27e$year)
percent_data_Q27e$year <- as.factor(percent_data_Q27e$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27e <- ggplot(agg_data_Q27e[agg_data_Q27e$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -7, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I feel a strong emotional bond with animals'",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27e


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27e))

# Plot percentage distribution
plot_percent_data_Q27e <- ggplot(percent_data_Q27e, aes(x = Q27e, y = percentage, fill = year)) +
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
    title = "I feel a strong emotional bond with animals",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27e

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27e))

# Opinion on the statement "The needs of humans should take priority over fish and wildlife protection"

# Calculate mean response and its standard error for Q27f
agg_data_Q27f <- svyby(~as.numeric(Q27f), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27f_se <- svyby(~as.numeric(Q27f), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27f_se$se_mean <- sqrt(agg_data_Q27f_se$se)
agg_data_Q27f_se$mean_response_total <- agg_data_Q27f_se$`as.numeric(Q27f)`
agg_data_Q27f_se <- agg_data_Q27f_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27f <- merge(agg_data_Q27f, agg_data_Q27f_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27f)[colnames(agg_data_Q27f) == "as.numeric(Q27f)"] <- "mean_response"

# Calculate percentage distribution for Q27f
percent_data_Q27f <- data %>%
  filter(!is.na(Q27f)) %>%
  group_by(group, year, Q27f) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27f$year <- as.factor(agg_data_Q27f$year)
percent_data_Q27f$year <- as.factor(percent_data_Q27f$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27f <- ggplot(agg_data_Q27f[agg_data_Q27f$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "The needs of humans should take priority over fish and wildlife protection",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27f


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27f))

# Plot percentage distribution
plot_percent_data_Q27f <- ggplot(percent_data_Q27f, aes(x = Q27f, y = percentage, fill = year)) +
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
    title = "The needs of humans should take priority over fish and wildlife protection",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_grey(start = 0.8, end = 0.2) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27f

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27f))

# Opinion on the statement "I care about animals as much as I do other people"

# Calculate mean response and its standard error for Q27g
agg_data_Q27g <- svyby(~as.numeric(Q27g), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27g_se <- svyby(~as.numeric(Q27g), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27g_se$se_mean <- sqrt(agg_data_Q27g_se$se)
agg_data_Q27g_se$mean_response_total <- agg_data_Q27g_se$`as.numeric(Q27g)`
agg_data_Q27g_se <- agg_data_Q27g_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27g <- merge(agg_data_Q27g, agg_data_Q27g_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27g)[colnames(agg_data_Q27g) == "as.numeric(Q27g)"] <- "mean_response"

# Calculate percentage distribution for Q27g
percent_data_Q27g <- data %>%
  filter(!is.na(Q27g)) %>%
  group_by(group, year, Q27g) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27g$year <- as.factor(agg_data_Q27g$year)
percent_data_Q27g$year <- as.factor(percent_data_Q27g$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27g <- ggplot(agg_data_Q27g[agg_data_Q27g$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I care about animals as much as I do other people",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27g

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27g))

# Plot percentage distribution
plot_percent_data_Q27g <- ggplot(percent_data_Q27g, aes(x = Q27g, y = percentage, fill = year)) +
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
    title = "I care about animals as much as I do other people",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27g

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27g))

# Opinion on the statement "Fish and wildlife are on earth primarily for people to use"

# Calculate mean response and its standard error for Q27h
agg_data_Q27h <- svyby(~as.numeric(Q27h), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27h_se <- svyby(~as.numeric(Q27h), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27h_se$se_mean <- sqrt(agg_data_Q27h_se$se)
agg_data_Q27h_se$mean_response_total <- agg_data_Q27h_se$`as.numeric(Q27h)`
agg_data_Q27h_se <- agg_data_Q27h_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27h <- merge(agg_data_Q27h, agg_data_Q27h_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27h)[colnames(agg_data_Q27h) == "as.numeric(Q27h)"] <- "mean_response"

# Calculate percentage distribution for Q27h
percent_data_Q27h <- data %>%
  filter(!is.na(Q27h)) %>%
  group_by(group, year, Q27h) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27h$year <- as.factor(agg_data_Q27h$year)
percent_data_Q27h$year <- as.factor(percent_data_Q27h$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27h <- ggplot(agg_data_Q27h[agg_data_Q27h$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4.5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Fish and wildlife are on earth primarily for people to use",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27h

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27h))

# Plot percentage distribution
plot_percent_data_Q27h <- ggplot(percent_data_Q27h, aes(x = Q27h, y = percentage, fill = year)) +
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
    title = "Fish and wildlife are on earth primarily for people to use",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27h

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27h))

# Opinion on the statement "Hunting is cruel and inhumane to the animals"

# Calculate mean response and its standard error for Q27i
agg_data_Q27i <- svyby(~as.numeric(Q27i), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27i_se <- svyby(~as.numeric(Q27i), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27i_se$se_mean <- sqrt(agg_data_Q27i_se$se)
agg_data_Q27i_se$mean_response_total <- agg_data_Q27i_se$`as.numeric(Q27i)`
agg_data_Q27i_se <- agg_data_Q27i_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27i <- merge(agg_data_Q27i, agg_data_Q27i_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27i)[colnames(agg_data_Q27i) == "as.numeric(Q27i)"] <- "mean_response"

# Calculate percentage distribution for Q27i
percent_data_Q27i <- data %>%
  filter(!is.na(Q27i)) %>%
  group_by(group, year, Q27i) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27i$year <- as.factor(agg_data_Q27i$year)
percent_data_Q27i$year <- as.factor(percent_data_Q27i$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27i <- ggplot(agg_data_Q27i[agg_data_Q27i$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response)), 
            position = position_dodge(0.7), 
            vjust = -2, 
            size = 3.0) +  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Hunting is cruel and inhumane to animals",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27i

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27i))

# Plot percentage distribution
plot_percent_data_Q27i <- ggplot(percent_data_Q27i, aes(x = Q27i, y = percentage, fill = year)) +
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
    title = "Hunting is cruel and inhumane to animals",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27i

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27i))

# Opinion on the statement "We should strive for a world where humans and fish and wildlife can live side by side without fear"

# Calculate mean response and its standard error for Q27j
agg_data_Q27j <- svyby(~as.numeric(Q27j), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27j_se <- svyby(~as.numeric(Q27j), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27j_se$se_mean <- sqrt(agg_data_Q27j_se$se)
agg_data_Q27j_se$mean_response_total <- agg_data_Q27j_se$`as.numeric(Q27j)`
agg_data_Q27j_se <- agg_data_Q27j_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27j <- merge(agg_data_Q27j, agg_data_Q27j_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27j)[colnames(agg_data_Q27j) == "as.numeric(Q27j)"] <- "mean_response"

# Calculate percentage distribution for Q27j
percent_data_Q27j <- data %>%
  filter(!is.na(Q27j)) %>%
  group_by(group, year, Q27j) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27j$year <- as.factor(agg_data_Q27j$year)
percent_data_Q27j$year <- as.factor(percent_data_Q27j$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27j <- ggplot(agg_data_Q27j[agg_data_Q27j$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -5.5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "We should strive for a world where humans and wildlife can live side by side without fear",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27j


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27j))

# Plot percentage distribution
plot_percent_data_Q27j <- ggplot(percent_data_Q27j, aes(x = Q27j, y = percentage, fill = year)) +
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
    title = "We should strive for a world where humans and wildlife can live side by side without fear",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27j

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27j))

# Opinion on the statement "I value the sense of companionship I receive from animals"

# Calculate mean response and its standard error for Q27k
agg_data_Q27k <- svyby(~as.numeric(Q27k), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27k_se <- svyby(~as.numeric(Q27k), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27k_se$se_mean <- sqrt(agg_data_Q27k_se$se)
agg_data_Q27k_se$mean_response_total <- agg_data_Q27k_se$`as.numeric(Q27k)`
agg_data_Q27k_se <- agg_data_Q27k_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27k <- merge(agg_data_Q27k, agg_data_Q27k_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27k)[colnames(agg_data_Q27k) == "as.numeric(Q27k)"] <- "mean_response"

# Calculate percentage distribution for Q27k
percent_data_Q27k <- data %>%
  filter(!is.na(Q27k)) %>%
  group_by(group, year, Q27k) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27k$year <- as.factor(agg_data_Q27k$year)
percent_data_Q27k$year <- as.factor(percent_data_Q27k$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27k <- ggplot(agg_data_Q27k[agg_data_Q27k$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -6.5, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "I value companionship with animals",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27k

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27k))

# Plot percentage distribution
plot_percent_data_Q27k <- ggplot(percent_data_Q27k, aes(x = Q27k, y = percentage, fill = year)) +
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
    title = "I value companionship with animals",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27k
# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27k))

# Opinion on the statement "People who want to hunt should be provided the opportunity to do so"

# Calculate mean response and its standard error for Q27m
agg_data_Q27m <- svyby(~as.numeric(Q27m), by = ~group + year, design = survey_design, 
                       FUN = svymean, na.rm = TRUE)

agg_data_Q27m_se <- svyby(~as.numeric(Q27m), by = ~group + year, design = survey_design, 
                          FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q27m_se$se_mean <- sqrt(agg_data_Q27m_se$se)
agg_data_Q27m_se$mean_response_total <- agg_data_Q27m_se$`as.numeric(Q27m)`
agg_data_Q27m_se <- agg_data_Q27m_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q27m <- merge(agg_data_Q27m, agg_data_Q27m_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q27m)[colnames(agg_data_Q27m) == "as.numeric(Q27m)"] <- "mean_response"

# Calculate percentage distribution for Q27m
percent_data_Q27m <- data %>%
  filter(!is.na(Q27m)) %>%
  group_by(group, year, Q27m) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q27m$year <- as.factor(agg_data_Q27m$year)
percent_data_Q27m$year <- as.factor(percent_data_Q27m$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q27m <- ggplot(agg_data_Q27m[agg_data_Q27m$year %in% c("2017", "2023"),], 
                             aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response)), 
            position = position_dodge(0.7), 
            vjust = -2, 
            size = 3.0) +  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "People who want to hunt should be provided the opportunity to do so",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 7))

plot_agg_data_Q27m


# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q27m))

# Plot percentage distribution
plot_percent_data_Q27m <- ggplot(percent_data_Q27m, aes(x = Q27m, y = percentage, fill = year)) +
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
    title = "People who want to hunt should be provided the opportunity to do so",
    x = "On a scale from 1 (strongly disagree) to 7 (strongly agree)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q27m

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q27m))

# Calculate percentage distribution for Q31
percent_data_Q31 <- data %>%
  filter(!is.na(Q31)) %>%
  group_by(group, year, Q31) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Filter out rows with NA values in Q31
percent_data_Q31 <- percent_data_Q31 %>% filter(!is.na(Q31))

# Convert 'year' and 'Q31' to factor variables
percent_data_Q31$year <- as.factor(percent_data_Q31$year)
percent_data_Q31$Q31 <- factor(percent_data_Q31$Q31, levels = c(1, 2), labels = c("No", "Yes"))

# Plot percentage distribution
plot_percent_data_Q31 <- ggplot(percent_data_Q31, aes(x = Q31, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f", round(percentage, 1)), y = percentage + 2), 
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_grid(~ group, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Did Respondents Purchase a Montana TRAPPING License for 2022-23?",
    x = "Response to Q31",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 105)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q31

# Append to the list
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q31))


# Average likelihood to purchase a Montana TRAPPING license in the future

# Calculate mean response and its standard error for Q32
agg_data_Q32 <- svyby(~as.numeric(Q32), by = ~group + year, design = survey_design, 
                      FUN = svymean, na.rm = TRUE)

agg_data_Q32_se <- svyby(~as.numeric(Q32), by = ~group + year, design = survey_design, 
                         FUN = svytotal, na.rm = TRUE)

# Calculate standard error from the total variance
agg_data_Q32_se$se_mean <- sqrt(agg_data_Q32_se$se)
agg_data_Q32_se$mean_response_total <- agg_data_Q32_se$`as.numeric(Q32)`
agg_data_Q32_se <- agg_data_Q32_se %>% select(group, year, se_mean, mean_response_total)

# Merge the mean and its standard error by group and year
agg_data_Q32 <- merge(agg_data_Q32, agg_data_Q32_se, by = c("group", "year"))

# Rename columns for clarity
colnames(agg_data_Q32)[colnames(agg_data_Q32) == "as.numeric(Q32)"] <- "mean_response"

# Calculate percentage distribution for Q32
percent_data_Q32 <- data %>%
  filter(!is.na(Q32)) %>%
  group_by(group, year, Q32) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
agg_data_Q32$year <- as.factor(agg_data_Q32$year)
percent_data_Q32$year <- as.factor(percent_data_Q32$year)

# Plot average response with error bars for standard deviation
plot_agg_data_Q32 <- ggplot(agg_data_Q32[agg_data_Q32$year %in% c("2017", "2023"),], 
                            aes(x = group, y = mean_response, fill = year)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.7) +
  geom_errorbar(aes(ymin = mean_response - se, ymax = mean_response + se), 
                width = 0.2, position = position_dodge(0.7)) + 
  geom_text(aes(label = sprintf("%.1f", mean_response), 
                vjust = ifelse(group == "Wolf" & year == "2023", -4, -2)),
            position = position_dodge(0.7), 
            size = 3.0)+  # Add data labels with adjusted size
  theme_minimal() +
  labs(
    title = "Average Likelihood to Purchase a Montana TRAPPING License in the Future",
    x = "On a scale from 1 (very unlikely) to 5 (very likely)",
    y = "Mean response",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  coord_cartesian(ylim = c(1, 5))

plot_agg_data_Q32

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_agg_data_Q32))

# Plot percentage distribution
plot_percent_data_Q32 <- ggplot(percent_data_Q32, aes(x = Q32, y = percentage, fill = year)) +
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
    title = "How likely are your to purchase a wolf trapping license in the future",
    x = "On a scale from 1 (very unlikely) to 5 (very likely)",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_Q32

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_Q32))

# Calculate percentage distribution for Classification
percent_data_classification <- data %>%
  filter(!is.na(classification)) %>%
  group_by(group, year, classification) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

# Convert 'year' to a factor variable
percent_data_classification$year <- as.factor(percent_data_classification$year)

# Plot percentage distribution for Classification
plot_percent_data_classification <- ggplot(percent_data_classification, aes(x = classification, y = percentage, fill = year)) +
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
    title = "WVO classification",
    x = "Classification Categories",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_classification

# Append to the list of plots
list_of_plots <- append(list_of_plots, list(plot_percent_data_classification))


### Attitude vs Acceptability Plots
data$Q1b_reversed <- 6 - data$Q1b
data$attitude_composite <- rowMeans(data[, c("Q1c", "Q1b_reversed", "Q1e")], na.rm = TRUE)

data$Q15a_reversed <- 6 - data$Q15a
data$Q15b_reversed <- 6 - data$Q15b
data$Q15c_reversed <- 6 - data$Q15c
data$Q15d_reversed <- 6 - data$Q15d
data$Q15e_reversed <- 6 - data$Q15e
data$acceptability_composite <- rowMeans(data[, c("Q15a_reversed", "Q15b_reversed", "Q15c_reversed", "Q15d_reversed", "Q15e_reversed")], na.rm = TRUE)

data <- data %>%
  mutate(
    attitude_quadrant = case_when(
      attitude_composite >= 3 & acceptability_composite >= 3 ~ "Advocate",
      attitude_composite >= 3 & acceptability_composite < 3 ~ "Conditionally Support",
      attitude_composite < 3 & acceptability_composite < 3 ~ "Oppose",
      attitude_composite < 3 & acceptability_composite >= 3 ~ "Tolerate",
      TRUE ~ "Indeterminate"  # Fallback in case of missing values
    )
  )

# Filter data for 2017 and 2023
data_2017 <- data %>% filter(year == "2017" & group == "GenPop")
data_2023 <- data %>% filter(year == "2023" & group == "GenPop")

# Scatterplot for 2017
scatter_plot_2017 <- ggplot(data_2017, aes(x = attitude_composite, y = acceptability_composite, weights=FinalWt)) +
  geom_point(alpha = 0.6) +  # Set transparency to see overlapping points
  labs(
    title = "Scatterplot of Attitude vs Acceptability Composites for 2017",
    x = "Attitude Composite",
    y = "Acceptability Composite"
  ) +
  theme_minimal()

p_hex_density2017 <- ggplot(data_2017, aes(x = attitude_composite, y = acceptability_composite, weight = FinalWt)) +
  geom_hex(bins = 10) +
  scale_fill_gradient(low = "#f5f5dc", high = "#ff0000", name = "Density") +  # Beige to red
  ylab("Acceptability of wildlife species")+
  labs(
    title = "Attitude vs Acceptability Composites for 2017"
  )+
  scale_x_continuous(breaks=c(1:5), name = "Attitude towards wildlife species")+
  theme_classic(base_size=12)+
  theme(legend.position = "none",
        axis.text=element_text(color="black"))+
  coord_cartesian(ylim = c(0.5, 5.5), xlim = c(0.5, 5.5))+
  geom_vline(xintercept = 3, linetype="dotted", colour="grey20")+
  geom_hline(yintercept = 3, linetype="dotted", colour="grey20")+
  annotate("text", x = .5, y = .5, label="Oppose", hjust=0, size=3, fontface=2)+
  annotate("text", x = 5.5, y = .5, label="Conditionally support", hjust=1, size=3, fontface=2)+
  annotate("text", x = .5, y = 5.5, label="Tolerate", hjust=0, size=3, fontface=2)+
  annotate("text", x = 5.5, y = 5.5, label="Advocate", hjust=1, size=3, fontface=2)+
  annotate("text", x = 3, y = 3, label="Indifferent", size=3, fontface=2)

p_hex_density2017

p_hex_density2023 <- ggplot(data_2023, aes(x = attitude_composite, y = acceptability_composite, weight = FinalWt)) +
  geom_hex(bins = 10) +
  scale_fill_gradient(low = "#f5f5dc", high = "#ff0000", name = "Density") +  # Beige to red
  ylab("Acceptability of wildlife species")+
  labs(
    title = "Attitude vs Acceptability Composites for 2023"
  )+
  scale_x_continuous(breaks=c(1:5), name = "Attitude towards wildlife species")+
  theme_classic(base_size=12)+
  theme(legend.position = "none",
        axis.text=element_text(color="black"))+
  coord_cartesian(ylim = c(0.5, 5.5), xlim = c(0.5, 5.5))+
  geom_vline(xintercept = 3, linetype="dotted", colour="grey20")+
  geom_hline(yintercept = 3, linetype="dotted", colour="grey20")+
  annotate("text", x = .5, y = .5, label="Oppose", hjust=0, size=3, fontface=2)+
  annotate("text", x = 5.5, y = .5, label="Conditionally support", hjust=1, size=3, fontface=2)+
  annotate("text", x = .5, y = 5.5, label="Tolerate", hjust=0, size=3, fontface=2)+
  annotate("text", x = 5.5, y = 5.5, label="Advocate", hjust=1, size=3, fontface=2)+
  annotate("text", x = 3, y = 3, label="Indifferent", size=3, fontface=2)

p_hex_density2023

# Scatterplot for 2023
scatter_plot_2023 <- ggplot(data_2023, aes(x = attitude_composite, y = acceptability_composite, weights=FinalWt)) +
  geom_point(alpha = 0.6) +  # Set transparency to see overlapping points
  labs(
    title = "Scatterplot of Attitude vs Acceptability Composites for 2023",
    x = "Attitude Composite",
    y = "Acceptability Composite"
  ) +
  theme_minimal()

scatter_plot_2017
scatter_plot_2023

## Percent plot
# Calculate percentage distribution for attitude_quadrant
percent_data_attitude_quadrant <- data %>%
  filter(!is.na(attitude_quadrant)) %>%
  group_by(group, year, attitude_quadrant) %>%
  summarise(weighted_count = sum(FinalWt, na.rm = TRUE)) %>%
  group_by(group, year) %>%
  mutate(percentage = (weighted_count / sum(weighted_count)) * 100) %>%
  ungroup()

percent_data_attitude_quadrant <- percent_data_attitude_quadrant %>%
  filter(attitude_quadrant != "Indeterminate")

percent_data_attitude_quadrant

# Convert 'year' to a factor variable
percent_data_attitude_quadrant$year <- as.factor(percent_data_attitude_quadrant$year)

# Plot percentage distribution for attitude_quadrant
plot_percent_data_attitude_quadrant <- ggplot(percent_data_attitude_quadrant, aes(x = attitude_quadrant, y = percentage, fill = year)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f%%", round(percentage, 1)), y = percentage + 2),
    vjust = 0,
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  facet_wrap(~ group, scales = "free_y") +  # Changed to facet_wrap to better handle a single faceting variable (group)
  theme_minimal() +
  labs(
    title = "Percentage Distribution by Attitude/Acceptability Quadrant",
    x = "Attitude Quadrant",
    y = "Percentage",
    fill = "Year"
  ) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1, suffix = ""),
    limits = c(0, 100)
  ) +
  theme(strip.background = element_blank())

plot_percent_data_attitude_quadrant

list_of_plots <- append(list_of_plots, list(plot_percent_data_attitude_quadrant))


# Initialize the pptx object
my_pptx <- read_pptx()

for (plot in list_of_plots) {
  my_pptx <- my_pptx %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with(value = plot, location = ph_location_type(type = "body"))
}

# Save the PowerPoint file
print(my_pptx, target = "wolves_graphs_17v23.pptx")

# Define the directory for plot images
plots_directory <- "C:/Users/maxbi/OneDrive - The University of Montana/WolfData/WolfDatasets/Final2023/PlotImages"

# Iterate over the list of plots and save each as an image
for (i in seq_along(list_of_plots)) {
  
  # Define the filename for the plot with "17v23" prefix and plot number
  plot_filename <- paste0(plots_directory, "/17v23plot_", i, ".png")
  
  # Save the plot as an image
  ggsave(plot_filename, plot = list_of_plots[[i]], width = 9, height = 4.95)
}



