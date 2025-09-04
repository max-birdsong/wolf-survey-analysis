library(dplyr)

# Define the file path and names
#folder_path <- "C:/Users/maxbi/OneDrive - The University of Montana/WolfData/WolfDatasets"
folder_path <- "C:/Users/max.birdsong/OneDrive - The University of Montana/WolfData/WolfDatasets"

file_names <- c(
  "Final Respondents Deer Elk License Holders Wolf Survey 2016.csv",
  "Final Respondents Private Landowner Wolf Survey 2016.csv",
  "Final Respondents Wolf License Holders Wolf Survey 2016.csv",
  "Full Household Results 2017.csv",
  "Households 2012 Wolf Hunt Survey Respondents.csv",
  "Prelim Data BLUE Wolf Hunter Survey Aug 4.csv",
  "Prelim Data GREEN Deer Elk Hunter Survey Aug 4.csv",
  "Prelim Data GREY Household Survey Aug 4.csv",
  "Prelim Data YELLOW Landowner Survey Aug 4.csv",
  "Resident Deer Elk License Holders 2012 Wolf Hunt Survey Results.csv",
  "Resident Private Landowners 2012 Wolf Hunt Survey Results.csv",
  "Resident Wolf License Holders 2012 Wolf Hunt Survey Results.csv"
)

# Deduce year and group based on file names and load datasets
datasets <- lapply(file_names, function(file_name) {
  # Determine year
  if (grepl("2016", file_name)) {
    year <- 2017
  } else if (grepl("2017", file_name)) {
    year <- 2017
  } else if (grepl("2012", file_name)) {
    year <- 2012
  } else {
    year <- 2023
  }
  
  # Determine group
  if (grepl("Household", file_name) || grepl("Households", file_name)) {
    group <- "HOUSE"
  } else if (grepl("Deer Elk", file_name)) {
    group <- "DEER"
  } else if (grepl("Landowner", file_name)) {
    group <- "LAND"
  } else {
    group <- "WOLF"
  }
  
  # Load data
  data <- read.csv(paste0(folder_path, "/", file_name))
  
  # Add year and group columns
  data$year <- year
  data$group <- group
  
  return(data)
})

# Deduce year and group based on file names and load datasets
datasets <- setNames(lapply(file_names, function(file_name) {
  # Determine year
  if (grepl("2016", file_name)) {
    year <- 2017
  } else if (grepl("2017", file_name)) {
    year <- 2017
  } else if (grepl("2012", file_name)) {
    year <- 2012
  } else {
    year <- 2023
  }
  
  # Determine group
  if (grepl("Household", file_name) || grepl("Households", file_name)) {
    group <- "HOUSE"
  } else if (grepl("Deer Elk", file_name)) {
    group <- "DEER"
  } else if (grepl("Landowner", file_name)) {
    group <- "LAND"
  } else {
    group <- "WOLF"
  }
  
  # Load data
  data <- read.csv(paste0(folder_path, "/", file_name))
  
  # Add year and group columns
  data$year <- year
  data$group <- group
  
  return(data)
}), sapply(file_names, function(file_name) {
  year_group <- ifelse(grepl("2016", file_name), "2017", 
                       ifelse(grepl("2017", file_name), "2017",
                              ifelse(grepl("2012", file_name), "2012", "2023")))
  
  if (grepl("Household", file_name) || grepl("Households", file_name)) {
    group <- "HOUSE"
  } else if (grepl("Deer Elk", file_name)) {
    group <- "DEER"
  } else if (grepl("Landowner", file_name)) {
    group <- "LAND"
  } else {
    group <- "WOLF"
  }
  
  return(paste0(year_group, "_", group))
}))


# Matching table

  House23 = c('IDNO','Q1a', 'Q1b', 'Q1c', 'Q1d', 'Q1e', 'Q1f', 
              'Q2a', 'Q2b', 'Q2c', 'Q2d', 'Q2e', 
              'Q3', 'Q4', 'Q5', 'Q6', 'Q7a', 'Q7b', 
              'Q8a', 'Q8b', 'Q9', 'Q10', 'Q11a', 'Q11b', 
              'Q12', 'Q13', 'Q14', 'Q15a', 'Q15b', 'Q15c', 
              'Q15d', 'Q15e', 'Q16a', 'Q16b', 'Q16c', 'Q16d', 
              'Q17', 'Q17a1', 'Q17a2', 'Q17a3', 'Q17a4', 'Q17a5', 
              'Q17a6', 'Q17a7', 'Q17a8', 'Q17b', 'Q18', 'Q19', 
              'Q20', 'Q21', 'Q22a', 'Q22b', 'Q22c', 'Q22d', 'Q22e', 
              'Q22f', 'Q23a', 'Q23b', 'Q23c', 'Q24', 'Q25', 
              'Q26a', 'Q26b', 'Q27a', 'Q27b', 'Q27c', 'Q27d', 
              'Q27e', 'Q27f', 'Q27g', 'Q27h', 'Q27i', 'Q27j', 
              'Q27k', 'Q27l', 'Q27m', 'Q28', 'Q29', 'Q30', 'Q31', 
              'Q32', 'Q33', 'Q34', 'Q35a', 'Q35b', 'Q36', 'Q37','wt')
  
  Deer23 = c('IDNO','Q1a', 'Q1b', 'Q1c', 'Q1d', 'Q1e', 'Q1f', 
             'Q2a', 'Q2b', 'Q2c', 'Q2d', 'Q2e', 
             'Q3', 'Q4', 'Q5', 'Q6', 'Q7a', 'Q7b', 
             'Q8a', 'Q8b', 'Q9', 'Q10', 'Q11a', 'Q11b', 
             'Q12', 'Q13', 'Q14', 'Q15a', 'Q15b', 'Q15c', 
             'Q15d', 'Q15e', 'Q16a', 'Q16b', 'Q16c', 'Q16d', 
             'Q17', 'Q17a1', 'Q17a2', 'Q17a3', 'Q17a4', 'Q17a5', 
             'Q17a6', 'Q17a7', 'Q17a8', 'Q17b', 'Q18', 'Q19', 
             'Q20', 'Q21', 'Q22a', 'Q22b', 'Q22c', 'Q22d', 'Q22e', 
             'Q22f', 'Q23a', 'Q23b', 'Q23c', 'Q24', 'Q25', 
             'Q26a', 'Q26b', 'Q27a', 'Q27b', 'Q27c', 'Q27d', 
             'Q27e', 'Q27f', 'Q27g', 'Q27h', 'Q27i', 'Q27j', 
             'Q27k', 'Q27l', 'Q27m', 'Yes', 'Q28', 'Q29', 'Q30', 
             'Q31', 'yes', 'Q32', 'Q33a', 'Q33b', 'Q34', 'Q35','wt')
  
  Land23 = c('IDNO','Q1a', 'Q1b', 'Q1c', 'Q1d', 'Q1e', 'Q1f', 
             'Q2a', 'Q2b', 'Q2c', 'Q2d', 'Q2e', 
             'Q3', 'Q4', 'Q5', 'Q6', 'Q7a', 'Q7b', 
             'Q8a', 'Q8b', 'Q9', 'Q10', 'Q11a', 'Q11b', 
             'Q12', 'Q13', 'Q14', 'Q15a', 'Q15b', 'Q15c', 
             'Q15d', 'Q15e', 'Q16a', 'Q16b', 'Q16c', 'Q16d', 
             'Q17', 'Q17a1', 'Q17a2', 'Q17a3', 'Q17a4', 'Q17a5', 
             'Q17a6', 'Q17a7', 'Q17a8', 'Q17b', 'Q18', 'Q19', 
             'Q20', 'Q21', 'Q22a', 'Q22b', 'Q22c', 'Q22d', 'Q22e', 
             'Q22f', 'Q23a', 'Q23b', 'Q23c', 'Q24', 'Q25', 
             'Q26a', 'Q26b', 'Q27a', 'Q27b', 'Q27c', 'Q27d', 
             'Q27e', 'Q27f', 'Q27g', 'Q27h', 'Q27i', 'Q27j', 
             'Q27k', 'Q27l', 'Q27m', 'Q28', 'Q29', 'Q30', 
             'Q31', 'Q32', 'Q33', 'yes', 'Q34a', 'Q34b', 'Q35', 'Q36','wt')
  
  Wolf23 = c('IDNO','Q1a', 'Q1b', 'Q1c', 'Q1d', 'Q1e', 'Q1f', 
             'Q2a', 'Q2b', 'Q2c', 'Q2d', 'Q2e', 
             'Q3', 'Q4', 'Q5', 'Q6', 'Q7a', 'Q7b', 
             'Q8a', 'Q8b', 'Q9', 'Q10', 'Q11a', 'Q11b', 
             'Q12', 'Q13', 'Q14', 'Q15a', 'Q15b', 'Q15c', 
             'Q15d', 'Q15e', 'Q16a', 'Q16b', 'Q16c', 'Q16d', 
             'Q17', 'Q17a1', 'Q17a2', 'Q17a3', 'Q17a4', 'Q17a5', 
             'Q17a6', 'Q17a7', 'Q17a8', 'Q17b', 'Q18', 'Q19', 
             'Q20', 'Q21', 'Q22a', 'Q22b', 'Q22c', 'Q22d', 'Q22e', 
             'Q22f', 'Q23a', 'Q23b', 'Q23c', 'Q24', 'Q25', 
             'Q26a', 'Q26b', 'Q27a', 'Q27b', 'Q27c', 'Q27d', 
             'Q27e', 'Q27f', 'Q27g', 'Q27h', 'Q27i', 'Q27j', 
             'Q27k', 'Q27l', 'Q27m', 'yes', 'yes', 'NA', 'Q28', 'Q29', 
             'Q30', 'Q31', 'Q32a', 'Q32b', 'Q33', 'Q34','wt')
  
  House17 = c('IDNO','NA', 'Q12b', 'Q12a', 'NA', 'Q12c', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
              'Q1', 'Na', 'Q2', 'Q3', 'Q4a', 'Q4b', 'NA', 'NA', 'Q5', 'Q6', 'Q7a', 
              'Q7b', 'Q8', 'Q9', 'Q10', 'Q11a', 'Q11b', 'Q11c', 'Q11d', 'Q11e', 
              'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
              'NA','NA', 'Q18', 'Q19', 'Q16', 'Q17', 'NA', 'NA', 'NA', 'NA', 'NA','NA', 'Q20a', 
              'Q20b', 'Q20c', 'Q13', 'Q14', 'Q15a', 'Q15b', 'Q26a', 'Q26b', 'Q26c', 
              'Q26d', 'Q26e', 'Q26f', 'Q26g', 'Q26h', 'Q26i', 'Q26j', 'Q26k', 'Q26l', 
              'Q26m', 'NA', 'Q21', 'Q22', 'Q23', 'Q24', 'Q25', 'NA', 'Q27a', 'Q27b', 'Q28', 'Q29','wt')
  
  Deer17 = c('IDNO','NA', 'Q12b', 'Q12a', 'NA', 'Q12c', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'Q1', 'Na', 'Q2', 'Q3', 'Q4a', 'Q4b', 'NA', 'NA', 'Q5', 'Q6', 'Q7a', 'Q7b',
             'Q8', 'Q9', 'Q10', 'Q11a', 'Q11b', 'Q11c', 'Q11d', 'Q11e', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'Q18', 'Q19', 'Q16', 'Q17', 'NA', 'NA', 'NA', 'NA', 'NA','NA', 'Q20a', 'Q20b', 
             'Q20c', 'Q13', 'Q14', 'Q15a', 'Q15b', 'Q25a', 'Q25b', 'Q25c', 'Q25d', 
             'Q25e', 'Q25f', 'Q25g', 'Q25h', 'Q25i', 'Q25j', 'Q25k', 'Q25l', 'Q25m', 
             'NA', 'Q21', 'Q22', 'Q23', 'Q24', 'Yes', 'NA', 'Q26a', 'Q26b', 'Q27', 'Q28','wt')
  
  Land17 = c('IDNO','NA', 'Q12b', 'Q12a', 'NA', 'Q12c', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'Q1', 'Na', 'Q2', 'Q3', 'Q4a', 'Q4b', 'NA', 'NA', 'Q5', 'Q6', 'Q7a', 'Q7b',
             'Q8', 'Q9', 'Q10', 'Q11a', 'Q11b', 'Q11c', 'Q11d', 'Q11e', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'Q18', 'Q19', 'Q16', 'Q17', 'NA', 'NA', 'NA', 'NA', 'NA','NA', 'Q20a', 'Q20b', 
             'Q20c', 'Q13', 'Q14', 'Q15a', 'Q15b', 'Q26a', 'Q26b', 'Q26c', 'Q26d', 
             'Q26e', 'Q26f', 'Q26g', 'Q26h', 'Q26i', 'Q26j', 'Q26k', 'Q26l', 'Q26m', 
             'NA', 'Q21', 'Q22', 'Q23', 'Q24', 'Q25', 'NA', 'Q27a', 'Q27b', 'Q28', 'Q29','wt')
  
  Wolf17 = c('IDNO','NA', 'Q12b', 'Q12a', 'NA', 'Q12c', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'Q1', 'Na', 'Q2', 'Q3', 'Q4a', 'Q4b', 'NA', 'NA', 'Q5', 'Q6', 'Q7a', 'Q7b',
             'Q8', 'Q9', 'Q10', 'Q11a', 'Q11b', 'Q11c', 'Q11d', 'Q11e', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'Q18', 'Q19', 'Q16', 'Q17', 'NA', 'NA', 'NA', 'NA', 'NA','NA', 'Q20a', 'Q20b', 
             'Q20c', 'Q13', 'Q14', 'Q15a', 'Q15b', 'Q25a', 'Q25b', 'Q25c', 'Q25d', 
             'Q25e', 'Q25f', 'Q25g', 'Q25h', 'Q25i', 'Q25j', 'Q25k', 'Q25l', 'Q25m', 
             'Yes', 'Yes', 'Q21', 'Q22', 'Q23', 'Q24', 'NA', 'Q26a', 'Q26b', 'Q27', 'Q28','wt')
  
  House12 = c('IDNO','NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
              'q1', 'Na', 'q5', 'q7b', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
              'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
              'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'q14', 
              'q13', 'q3', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA','NA', 'q2a', 'q2b', 'q2c', 
              'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
              'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'q9', 'q10', 'NA', 'NA',  'q15','NA', 'q17a', 'q17b', 
              'q18', 'q19','wt')
  
  Deer12 = c('IDNO','NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'q1', 'Na', 'q5', 'q7b', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'q14', 
             'q13', 'q3', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA','NA', 'q2a', 'q2b', 'q2c', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA','NA', 'q9', 'q10', 'NA','NA','yes', 'NA', 'q16a', 'q16b', 
             'q17', 'q18','wt')
  
  Land12 = c('IDNO','NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'q1', 'Na', 'q5', 'q7b', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA','NA', 'q14', 'q13', 'q3', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'q2a', 'q2b', 'q2c', 'NA', 'NA', 'NA',
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA',
             'NA','NA','q9', 'q10', 'NA', 'NA', 'q15', 'NA', 'q17a', 'q17b', 'q18', 'q19','wt')
  
  Wolf12 = c('IDNO','NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'q1', 'Na', 'q5', 'q7b', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA','NA', 'q14', 'q13', 'q3', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'q2a', 'q2b', 'q2c', 'NA', 'NA', 
             'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 'NA', 
             'NA', 'NA', 'NA','NA', 'yes', 'q10', 'NA', 'NA', 'q15', 'NA', 'q17a', 'q17b', 'q18', 'q19','wt')



#### Add House 23 ####

# Extract 2023_HOUSE from the list
`2023_HOUSE` <- datasets[["2023_HOUSE"]]

# Identify missing columns from House23 in 2023_HOUSE
missing_cols_house <- House23[!(House23 %in% colnames(`2023_HOUSE`))]

# For columns that contain "yes" or similar, create columns with all values set to 2
yes_cols_house <- missing_cols_house[grepl("yes", missing_cols_house, ignore.case = TRUE)]
for (col in yes_cols_house) {
  new_col_name <- paste0("Col_", col)
  `2023_HOUSE`[[new_col_name]] <- 2
  # Update the House23 vector to match the new column name
  House23[House23 == col] <- new_col_name
}

# For other missing columns, create columns with all NA values
other_missing_cols_house <- setdiff(missing_cols_house, yes_cols_house)
for (col in other_missing_cols_house) {
  `2023_HOUSE`[[col]] <- NA
}

# Reorder the 2023_HOUSE dataset based on the House23 vector
ordered_House23 <- `2023_HOUSE`[, House23]


### Add Deer23

# Extract 2023_DEER from the list
`2023_DEER` <- datasets[["2023_DEER"]]

`2023_DEER`$Col_Yes <- 2
`2023_DEER`$Col_yes <- 2

Deer23[Deer23 == "Yes"] <- "Col_Yes"
Deer23[Deer23 == "yes"] <- "Col_yes"

ordered_Deer23 <- `2023_DEER`[, Deer23]

### Add Land23

# Extract 2023_LAND from the list
`2023_LAND` <- datasets[["2023_LAND"]]

# Identify missing columns
missing_cols_land <- Land23[!(Land23 %in% colnames(`2023_LAND`))]

# If any column in missing_cols_land is "yes" or similar, 
# create a new column in 2023_LAND with all values set to 2
yes_cols_land <- missing_cols_land[grepl("yes", missing_cols_land, ignore.case = TRUE)]
for (col in yes_cols_land) {
  new_col_name <- paste0("Col_", col)
  `2023_LAND`[[new_col_name]] <- 2
  # Update the Land23 vector to match the new column name
  Land23[Land23 == col] <- new_col_name
}

# For other missing columns, create columns with all NA values
other_missing_cols_land <- setdiff(missing_cols_land, yes_cols_land)
for (col in other_missing_cols_land) {
  `2023_LAND`[[col]] <- NA
}

# Now try reordering the columns
ordered_Land23 <- `2023_LAND`[, Land23]

# Let's check the results
head(ordered_Land23)


### Add Wolf23
  
  # Extract 2023_WOLF from the list
  `2023_WOLF` <- datasets[["2023_WOLF"]]
  
  # Identify missing columns
  missing_cols_wolf <- Wolf23[!(Wolf23 %in% colnames(`2023_WOLF`))]
  
  # If any column in missing_cols_wolf is "yes" or similar, 
  # create a new column in 2023_WOLF with all values set to 2
  yes_cols_wolf <- missing_cols_wolf[grepl("yes", missing_cols_wolf, ignore.case = TRUE)]
  for (col in yes_cols_wolf) {
    new_col_name <- paste0("Col_", col)
    `2023_WOLF`[[new_col_name]] <- 2
    # Update the Wolf23 vector to match the new column name
    Wolf23[Wolf23 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_wolf <- setdiff(missing_cols_wolf, yes_cols_wolf)
  for (col in other_missing_cols_wolf) {
    `2023_WOLF`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_Wolf23 <- `2023_WOLF`[, Wolf23]
  
  # Let's check the results
  head(ordered_Wolf23)
  
### Add House17
  
  # Extract 2017_HOUSE from the list
  `2017_HOUSE` <- datasets[["2017_HOUSE"]]
  
  # Identify missing columns
  missing_cols_house <- House17[!(House17 %in% colnames(`2017_HOUSE`))]
  
  # If any column in missing_cols_house is "yes" or similar, 
  # create a new column in 2017_HOUSE with all values set to 2
  yes_cols_house <- missing_cols_house[grepl("yes", missing_cols_house, ignore.case = TRUE)]
  for (col in yes_cols_house) {
    new_col_name <- paste0("Col_", col)
    `2017_HOUSE`[[new_col_name]] <- 2
    # Update the House17 vector to match the new column name
    House17[House17 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_house <- setdiff(missing_cols_house, yes_cols_house)
  for (col in other_missing_cols_house) {
    `2017_HOUSE`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_House17 <- `2017_HOUSE`[, House17]
  
  # Let's check the results
  head(ordered_House17)
  
### Add Deer 17  

  # Extract 2017_DEER from the list
  `2017_DEER` <- datasets[["2017_DEER"]]
  
  # Identify missing columns
  missing_cols_deer <- Deer17[!(Deer17 %in% colnames(`2017_DEER`))]
  
  # If any column in missing_cols_deer is "yes" or similar, 
  # create a new column in 2017_DEER with all values set to 2
  yes_cols_deer <- missing_cols_deer[grepl("yes", missing_cols_deer, ignore.case = TRUE)]
  for (col in yes_cols_deer) {
    new_col_name <- paste0("Col_", col)
    `2017_DEER`[[new_col_name]] <- 2
    # Update the Deer17 vector to match the new column name
    Deer17[Deer17 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_deer <- setdiff(missing_cols_deer, yes_cols_deer)
  for (col in other_missing_cols_deer) {
    `2017_DEER`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_Deer17 <- `2017_DEER`[, Deer17]
  
  # Let's check the results
  head(ordered_Deer17)
  
### Add Land17
  
  # Extract 2017_LAND from the list
  `2017_LAND` <- datasets[["2017_LAND"]]
  
  # Identify missing columns
  missing_cols_land <- Land17[!(Land17 %in% colnames(`2017_LAND`))]
  
  # If any column in missing_cols_land is "yes" or similar, 
  # create a new column in 2017_LAND with all values set to 2
  yes_cols_land <- missing_cols_land[grepl("yes", missing_cols_land, ignore.case = TRUE)]
  for (col in yes_cols_land) {
    new_col_name <- paste0("Col_", col)
    `2017_LAND`[[new_col_name]] <- 2
    # Update the Land17 vector to match the new column name
    Land17[Land17 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_land <- setdiff(missing_cols_land, yes_cols_land)
  for (col in other_missing_cols_land) {
    `2017_LAND`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_Land17 <- `2017_LAND`[, Land17]
  
  # Let's check the results
  head(ordered_Land17)
  
### Add Wolf17
  
  # Extract 2017_WOLF from the list
  `2017_WOLF` <- datasets[["2017_WOLF"]]
  
  # Identify missing columns
  missing_cols_wolf <- Wolf17[!(Wolf17 %in% colnames(`2017_WOLF`))]
  
  # If any column in missing_cols_wolf is "yes" or similar, 
  # create a new column in 2017_WOLF with all values set to 2
  yes_cols_wolf <- missing_cols_wolf[grepl("yes", missing_cols_wolf, ignore.case = TRUE)]
  for (col in yes_cols_wolf) {
    new_col_name <- paste0("Col_", col)
    `2017_WOLF`[[new_col_name]] <- 2
    # Update the Wolf17 vector to match the new column name
    Wolf17[Wolf17 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_wolf <- setdiff(missing_cols_wolf, yes_cols_wolf)
  for (col in other_missing_cols_wolf) {
    `2017_WOLF`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_Wolf17 <- `2017_WOLF`[, Wolf17]
  
  # Let's check the results
  head(ordered_Wolf17)
  
  ### Add House 12
  
  # Extract 2012_HOUSE from the list
  `2012_HOUSE` <- datasets[["2012_HOUSE"]]
  
  # Identify missing columns
  missing_cols_house12 <- House12[!(House12 %in% colnames(`2012_HOUSE`))]
  
  # If any column in missing_cols_house12 is "yes" or similar, 
  # create a new column in 2012_HOUSE with all values set to 2
  yes_cols_house12 <- missing_cols_house12[grepl("yes", missing_cols_house12, ignore.case = TRUE)]
  for (col in yes_cols_house12) {
    new_col_name <- paste0("Col_", col)
    `2012_HOUSE`[[new_col_name]] <- 2
    # Update the House12 vector to match the new column name
    House12[House12 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_house12 <- setdiff(missing_cols_house12, yes_cols_house12)
  for (col in other_missing_cols_house12) {
    `2012_HOUSE`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_House12 <- `2012_HOUSE`[, House12]
  
  # Let's check the results
  head(ordered_House12)
  
  
### Add Deer12

  # Extract 2012_DEER from the list
  `2012_DEER` <- datasets[["2012_DEER"]]
  
  # Identify missing columns
  missing_cols_deer12 <- Deer12[!(Deer12 %in% colnames(`2012_DEER`))]
  
  # If any column in missing_cols_deer12 is "yes" or similar, 
  # create a new column in 2012_DEER with all values set to 2
  yes_cols_deer12 <- missing_cols_deer12[grepl("yes", missing_cols_deer12, ignore.case = TRUE)]
  for (col in yes_cols_deer12) {
    new_col_name <- paste0("Col_", col)
    `2012_DEER`[[new_col_name]] <- 2
    # Update the Deer12 vector to match the new column name
    Deer12[Deer12 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_deer12 <- setdiff(missing_cols_deer12, yes_cols_deer12)
  for (col in other_missing_cols_deer12) {
    `2012_DEER`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_Deer12 <- `2012_DEER`[, Deer12]
  
  # Let's check the results
  head(ordered_Deer12)
  
### Add Land12
  
  # Extract 2012_LAND from the list
  `2012_LAND` <- datasets[["2012_LAND"]]
  
  # Identify missing columns
  missing_cols_land12 <- Land12[!(Land12 %in% colnames(`2012_LAND`))]
  
  # If any column in missing_cols_land12 is "yes" or similar, 
  # create a new column in 2012_LAND with all values set to 2
  yes_cols_land12 <- missing_cols_land12[grepl("yes", missing_cols_land12, ignore.case = TRUE)]
  for (col in yes_cols_land12) {
    new_col_name <- paste0("Col_", col)
    `2012_LAND`[[new_col_name]] <- 2
    # Update the Land12 vector to match the new column name
    Land12[Land12 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_land12 <- setdiff(missing_cols_land12, yes_cols_land12)
  for (col in other_missing_cols_land12) {
    `2012_LAND`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_Land12 <- `2012_LAND`[, Land12]
  
  # Let's check the results
  head(ordered_Land12)
  
### Add Wolf12
  
  # Extract 2012_WOLF from the list
  `2012_WOLF` <- datasets[["2012_WOLF"]]
  
  # Identify missing columns
  missing_cols_wolf12 <- Wolf12[!(Wolf12 %in% colnames(`2012_WOLF`))]
  
  # If any column in missing_cols_wolf12 is "yes" or similar, 
  # create a new column in 2012_WOLF with all values set to 2
  yes_cols_wolf12 <- missing_cols_wolf12[grepl("yes", missing_cols_wolf12, ignore.case = TRUE)]
  for (col in yes_cols_wolf12) {
    new_col_name <- paste0("Col_", col)
    `2012_WOLF`[[new_col_name]] <- 2
    # Update the Wolf12 vector to match the new column name
    Wolf12[Wolf12 == col] <- new_col_name
  }
  
  # For other missing columns, create columns with all NA values
  other_missing_cols_wolf12 <- setdiff(missing_cols_wolf12, yes_cols_wolf12)
  for (col in other_missing_cols_wolf12) {
    `2012_WOLF`[[col]] <- NA
  }
  
  # Now try reordering the columns
  ordered_Wolf12 <- `2012_WOLF`[, Wolf12]
  
  # Let's check the results
  head(ordered_Wolf12)
  

# Function to add group and year
  add_group_year <- function(df, group, year) {
    df$group <- group
    df$year <- year
    return(df)
  }  

  # Adding group and year to each dataset
  ordered_House23 <- add_group_year(ordered_House23, "House", 23)
  ordered_Deer23 <- add_group_year(ordered_Deer23, "Deer", 23)
  ordered_Land23 <- add_group_year(ordered_Land23, "Land", 23)
  ordered_Wolf23 <- add_group_year(ordered_Wolf23, "Wolf", 23)
  
  ordered_House17 <- add_group_year(ordered_House17, "House", 17)
  ordered_Deer17 <- add_group_year(ordered_Deer17, "Deer", 17)
  ordered_Land17 <- add_group_year(ordered_Land17, "Land", 17)
  ordered_Wolf17 <- add_group_year(ordered_Wolf17, "Wolf", 17)
  
  ordered_House12 <- add_group_year(ordered_House12, "House", 12)
  ordered_Deer12 <- add_group_year(ordered_Deer12, "Deer", 12)
  ordered_Land12 <- add_group_year(ordered_Land12, "Land", 12)
  ordered_Wolf12 <- add_group_year(ordered_Wolf12, "Wolf", 12)
    
# Combining Datasets
library(dplyr)
  
  all_ordered_datasets <- list(
    ordered_House23, ordered_Deer23, ordered_Land23, ordered_Wolf23, 
    ordered_House17, ordered_Deer17, ordered_Land17, ordered_Wolf17,
    ordered_House12, ordered_Deer12, ordered_Land12, ordered_Wolf12
  )
  
  
  
  # Set all datasets to have the same column names temporarily
  all_ordered_datasets <- lapply(all_ordered_datasets, function(df) {
    colnames(df) <- 1:90
    return(df)
  })
  
  
  # Now bind rows
  MasterData <- bind_rows(all_ordered_datasets)
  
  colnames(MasterData) <- colnames(ordered_House23)
  
  
  
# View the first few rows of the combined dataset
head(MasterData)
write.csv(MasterData, "C:/Users/max.birdsong/OneDrive - The University of Montana/WolfData/MasterDataGenerate.csv", row.names = FALSE)




