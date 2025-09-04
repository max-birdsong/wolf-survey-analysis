# Reading the data & identifying unique columns

library(haven)
library(dplyr)

# Set the working directory
setwd("C:/Users/maxbi/OneDrive - The University of Montana/WolfData/TrackingData")

# List of your .sav files
files <- c("Final Tracking Deer Elk Hunter Wolf Survey 2023.sav", 
           "Final Tracking Landowner Wolf Survey 2023.sav", 
           "Final Tracking Wolf Household Survey 2023.sav", 
           "Final Tracking Wolf Hunter Wolf Survey 2023.sav")

# Read the datasets into a list and add a 'Group' column
list_of_datasets <- lapply(files, function(file) {
  
  df <- read_sav(file)
  
  group_name <- case_when(
    grepl("Deer", file) ~ "Deer",
    grepl("Land", file) ~ "Land",
    grepl("House", file) ~ "House",
    grepl("Wolf", file) ~ "Wolf",
    TRUE ~ "Unknown"
  )
  
  df$Group <- group_name
  
  return(df)
})


# Identify all unique column names across datasets
columns_in_files <- lapply(list_of_datasets, colnames)
all_columns <- unique(unlist(columns_in_files))

## Harmonizing the datasets

# Harmonize datasets
harmonized_datasets <- lapply(list_of_datasets, function(df) {
  
  # Add missing columns
  missing_cols <- setdiff(all_columns, names(df))
  for (col in missing_cols) {
    df[[col]] <- NA
  }
  
  # Only rename if target column doesn't exist
  if ("ADDRESS1" %in% names(df) && !"ADDRESS" %in% names(df)) {
    df <- df %>%
      rename(ADDRESS = ADDRESS1)
  }
  # If both "ADDRESS" and "ADDRESS1" exist, you need a decision here.
  # For example, combine them, or prioritize one over the other.
  
  # Reorder columns to match the all_columns order
  df <- df[, all_columns]
  
  return(df)
})

# Merging the datasets
merged_data <- do.call(rbind, harmonized_datasets)

merged_data <- merged_data %>%
  mutate(
    ZIP = ifelse(!is.na(ZIP), substr(ZIP, 1, 5), NA_character_),
    COMMON_ADDRESS = case_when(
      !is.na(ADDRESS2) & ADDRESS2 != "" & 
        (grepl("^APT", ADDRESS2) | grepl("^UNIT", ADDRESS2) | grepl("^TRLR", ADDRESS2)) ~ 
        paste(ADDRESS1, ADDRESS2, CITY, STATE, ZIP, sep = " "),
      !is.na(ADDRESS2) & ADDRESS2 != "" ~ paste(ADDRESS2, CITY, STATE, ZIP, sep = " "),
      !is.na(ADDRESS1) & ADDRESS1 != "" ~ paste(ADDRESS1, CITY, STATE, ZIP, sep = " "),
      !is.na(ADDRESS) & ADDRESS != "" ~ paste(ADDRESS, CITY, STATE, ZIP, sep = " "),
      TRUE ~ NA_character_
    )
  )

# Check if hunter or fisher
# Load required library
library(dplyr)

# Read in the hunting and fishing data
hunting_data <- read.csv("Copy of Resident Base Hunting List 2023.csv", stringsAsFactors = FALSE)
fishing_data <- read.csv("Copy of Resident Fishing List 2023.csv", stringsAsFactors = FALSE)

# Create FULLNAME2 in hunting_data
hunting_data <- hunting_data %>%
  mutate(
    Zip = ifelse(!is.na(Zip), substr(Zip, 1, 5), NA_character_), # Limit zip to the first 5 characters
    COMMON_ADDRESS = paste(Address, City, State, Zip, sep = " "),
    FULLNAME2 = paste(First.Name, Last.Name, sep = " ")
  )

# Create FULLNAME2 in fishing_data
fishing_data <- fishing_data %>%
  mutate(
    Zip = ifelse(!is.na(Zip), substr(Zip, 1, 5), NA_character_), # Limit zip to the first 5 characters
    COMMON_ADDRESS = paste(Address, City, State, Zip, sep = " "),
    FULLNAME2 = paste(First.Name, Last.Name, sep = " ")
  )

# Check if the address in merged_data is in the hunting and fishing data
merged_data$hunting_license <- ifelse(merged_data$COMMON_ADDRESS %in% hunting_data$COMMON_ADDRESS, 1, 0)
merged_data$fishing_license <- ifelse(merged_data$COMMON_ADDRESS %in% fishing_data$COMMON_ADDRESS, 1, 0)


# Install and load the necessary packages
library(sf)

# Read the shapefile
cadastral_shp <- st_read("C:/Users/maxbi/OneDrive - The University of Montana/WolfData/TrackingData/Montana_Cadastral/OWNERPARCEL.shp")

# Convert the shapefile's attribute table to a dataframe
cadastral_attributes <- as.data.frame(cadastral_shp)

library(dplyr)
library(fuzzyjoin)
library(stringr)

# 1. Prepare the `cadastral_attributes` dataset
cadastral_attributes <- cadastral_attributes %>%
  mutate(
    COMMON_ADDRESS = case_when(
      !is.na(AddressLin) & AddressLin != "" ~ paste(AddressLin, OwnerCity, OwnerState, str_sub(OwnerZipCo, 1, 5), sep = " "),
      !is.na(OwnerAdd_1) & OwnerAdd_1 != "" ~ paste(OwnerAdd_1, OwnerCity, OwnerState, str_sub(OwnerZipCo, 1, 5), sep = " "),
      !is.na(OwnerAddre) & OwnerAddre != "" ~ paste(OwnerAddre, OwnerCity, OwnerState, str_sub(OwnerZipCo, 1, 5), sep = " "),
      TRUE ~ NA_character_
    )
  )

# Convert the COMMON_ADDRESS in both datasets to uppercase
merged_data$COMMON_ADDRESS <- toupper(merged_data$COMMON_ADDRESS)
cadastral_attributes$COMMON_ADDRESS <- toupper(cadastral_attributes$COMMON_ADDRESS)

# Identify rows where IDNO is between 5001 and 7487
rows_to_fix <- which(merged_data$IDNO >= 5001 & merged_data$IDNO <= 7487)

# Fix COMMON_ADDRESS for these rows by replacing multiple spaces with a single space
merged_data$COMMON_ADDRESS[rows_to_fix] <- gsub("\\s+", " ", merged_data$COMMON_ADDRESS[rows_to_fix])

# Perform the join

final_data <- merged_data %>%
  inner_join(cadastral_attributes, by = "COMMON_ADDRESS", relationship = "many-to-many")

# Check duplicates in merged_data
merged_data %>% 
  group_by(COMMON_ADDRESS) %>% 
  count() %>% 
  filter(n > 1) -> duplicates_in_merged

# Check duplicates in cadastral_attributes
cadastral_attributes %>% 
  group_by(COMMON_ADDRESS) %>% 
  count() %>% 
  filter(n > 1) -> duplicates_in_cadastral

# View the results
View(duplicates_in_merged)
View(duplicates_in_cadastral)

non_matches <- setdiff(merged_data$COMMON_ADDRESS, final_data$COMMON_ADDRESS)
View(merged_data %>% filter(COMMON_ADDRESS %in% non_matches))

## Aggregate the results
# Aggregate using the custom shortened column names as per your dataset
aggregated_data <- final_data %>%
  group_by(IDNO) %>%
  summarise(
    FallowAcre = sum(FallowAcre, na.rm = TRUE),
    FarmsiteAcre = sum(FarmsiteAc, na.rm = TRUE),
    ForestAcre = sum(ForestAcre, na.rm = TRUE),
    GrazingAcre = sum(GrazingAcr, na.rm = TRUE),
    TotalAcres = sum(TotalAcres, na.rm = TRUE),
    TotalBuild = sum(TotalBuild, na.rm = TRUE),
    TotalLandV = sum(TotalLandV, na.rm = TRUE),
    Shape_Leng = sum(Shape_Leng, na.rm = TRUE),
    Shape_Area = sum(Shape_Area, na.rm = TRUE)
  )

# Ensure that only the columns that exist in final_data are being selected
selected_data <- final_data %>%
  select(IDNO, STATUS, FIRST, LAST, CITY, STATE, ZIP, COMMON_ADDRESS, hunting_license, fishing_license, CountyName) %>%
  distinct()

# If 'GROUP' does exist and it was a typo, please include it back in the select function.

# Merge aggregated and selected data
final_aggregated_data <- left_join(selected_data, aggregated_data, by = "IDNO")

# Extract rows from merged_data that are in non_matches
non_matched_data <- merged_data %>% 
  filter(COMMON_ADDRESS %in% non_matches)

# Select the same columns as in final_aggregated_data and add the additional columns with NA
non_matched_data <- non_matched_data %>%
  select(IDNO, STATUS, DOB, ALSNO, FIRST, LAST, CITY, STATE, ZIP, COMMON_ADDRESS, hunting_license, fishing_license) %>%
  mutate(
    FallowAcre = NA_real_,
    FarmsiteAcre = NA_real_,
    ForestAcre = NA_real_,
    GrazingAcre = NA_real_,
    TotalAcres = NA_real_,
    TotalBuild = NA_real_,
    TotalLandV = NA_real_,
    Shape_Leng = NA_real_,
    Shape_Area = NA_real_,
    CountyName = NA_character_
  )


# Concatenate final_aggregated_data and non_matched_data
final_complete_data <- bind_rows(final_aggregated_data, non_matched_data)

# Sort the final_complete_data dataframe by IDNO
final_sorted_data <- final_complete_data %>%
  arrange(IDNO)

# Create the named vector
fips_codes <- c(
  "Beaverhead County" = "30001",
  "Big Horn County" = "30003",
  "Blaine County" = "30005",
  "Broadwater County" = "30007",
  "Carbon County" = "30009",
  "Carter County" = "30011",
  "Cascade County" = "30013",
  "Chouteau County" = "30015",
  "Custer County" = "30017",
  "Daniels County" = "30019",
  "Dawson County" = "30021",
  "Deer Lodge County" = "30023",
  "Fallon County" = "30025",
  "Fergus County" = "30027",
  "Flathead County" = "30029",
  "Gallatin County" = "30031",
  "Garfield County" = "30033",
  "Glacier County" = "30035",
  "Golden Valley County" = "30037",
  "Granite County" = "30039",
  "Hill County" = "30041",
  "Jefferson County" = "30043",
  "Judith Basin County" = "30045",
  "Lake County" = "30047",
  "Lewis and Clark County" = "30049",
  "Liberty County" = "30051",
  "Lincoln County" = "30053",
  "McCone County" = "30055",
  "Madison County" = "30057",
  "Meagher County" = "30059",
  "Mineral County" = "30061",
  "Missoula County" = "30063",
  "Musselshell County" = "30065",
  "Park County" = "30067",
  "Petroleum County" = "30069",
  "Phillips County" = "30071",
  "Pondera County" = "30073",
  "Powder River County" = "30075",
  "Powell County" = "30077",
  "Prairie County" = "30079",
  "Ravalli County" = "30081",
  "Richland County" = "30083",
  "Roosevelt County" = "30085",
  "Rosebud County" = "30087",
  "Sanders County" = "30089",
  "Sheridan County" = "30091",
  "Silver Bow County" = "30093",
  "Stillwater County" = "30095",
  "Sweet Grass County" = "30097",
  "Teton County" = "30099",
  "Toole County" = "30101",
  "Treasure County" = "30103",
  "Valley County" = "30105",
  "Wheatland County" = "30107",
  "Wibaux County" = "30109",
  "Yellowstone County" = "30111"
)

# Assuming `merged_data` has a column named `CountyName`
final_sorted_data$FIPS_Code <- fips_codes[final_sorted_data$CountyName]

# Add the "State FIP" variable
final_sorted_data$State_FIP <- 30

# Writing final_data to a xl file
library(writexl)
write_xlsx(final_sorted_data, "Wolf_Tracking_Data_Cadastral.xlsx")
