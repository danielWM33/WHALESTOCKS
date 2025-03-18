#Initalize packages
# install.packages('geosphere')
library(readr)
library(geosphere)

# get the data we need, stocks, whale data
Blue_Whale_Data <- read_csv("Blue whales Eastern North Pacific 1993-2008 - Argos data.csv")
Unique_Whales_Vector <- unique(Blue_Whale_Data$`tag-local-identifier`)
Unique_Whales_Dataframe <- data.frame(Unique_Whales_Vector)
Money_Data_Categorized <- read_csv("constituents.csv")
Money_Data_Categorized_Unique_Sector <- unique(Money_Data_Categorized$`GICS Sector`)
Money_Data_Categorized_Unique_Sector_Sub <- unique(Money_Data_Categorized$`GICS Sub-Industry`)
Money_Data_Money <- read_csv("constituents-financials.csv")

#adding more stocks to list
stocks <- c(
  Money_Data_Categorized_Unique_Sector_Sub,
  'Gold',
  'Oil & Gas Refining & Marketing',
  'Copper',
  'Biotechnology',
  'Building Products',
  'Specialty Chemicals',
  'Tobacco',
  'Telecom Tower REITs',
  'Human Resource & Employment Services',
  'Brewers',
  'Steel',
  'Other Specialty Retail',
  'Distributors',
  'Metal, Glass & Plastic Containers'
)

Unique_Whales_Dataframe$Names <- c(
  "Moby",
  "Free Willy",
  "Shamu",
  "Keiko",
  "Monstro",
  "Old Tom",
  "Mocha",
  "Humphrey",
  "Luna",
  "Migaloo",
  "Poseidon",
  "Neptune",
  "Triton",
  "Leviathan",
  "Cetus",
  "Jörmungandr",
  "Sedna",
  "Thalassa",
  "Nereus",
  "Aegir",
  "Balaen",
  "Orcinus",
  "Cetacea",
  "Mysticeti",
  "Delphin",
  "Architeuthis",
  "Megaptera",
  "Physeter",
  "Odontus",
  "Blue",
  "Coral",
  "Reef",
  "Deep Blue",
  "Echo",
  "Tempest",
  "Abyss",
  "Tide",
  "Drift",
  "Current",
  "Marlin",
  "Bubbles",
  "Splash",
  "Finley",
  "Fluke",
  "Blubber",
  "Squishy",
  "Wavy",
  "Puddles",
  "Giggles",
  "Nibbles",
  "Dory",
  "Destiny",
  "Willy",
  "Bailey",
  "Pearl",
  "Jonah",
  "Pinocchio",
  "Bruce",
  "Echo",
  "Willy Wonka",
  "Azure",
  "Indigo",
  "Midnight",
  "Cerulean",
  "Sapphire",
  "Onyx",
  "Pearl",
  "Ivory",
  "Obsidian",
  "Cobalt",
  "Titan",
  "Brutus",
  "Goliath",
  "Kraken",
  "Hercules",
  "Thor",
  "Atlas",
  "Colossus",
  "Behemoth",
  "Tsunami",
  "Blubbert",
  "Whaleiam",
  "Moby Trick",
  "Gillbert",
  "Sir Whaley McWhaleface",
  "Flounder",
  "Tuna Turner",
  "Belu-Gaga",
  "Orca Winfrey",
  "Bubble Fett",
  "Stardust",
  "Nebula",
  "Lunar",
  "Eclipse",
  "Shadowfin",
  "Moonbeam",
  "Celestia",
  "Cosmos",
  "Aquarius",
  "Zephyr",
  "Mocha",
  "Latte",
  "Jellybean",
  "Coconut",
  "Marshmallow",
  "Sushi",
  "Boba",
  "Blueberry",
  "Licorice",
  "Honeydew",
  "Voyager",
  "Odyssey",
  "Nomad",
  "Pioneer",
  "Seafarer",
  "Mariner",
  "Polaris",
  "Navigator",
  "Compass",
  "Drifter",
  "Serenity",
  "Harmony",
  "Breeze",
  "Whisper",
  "Tranquil",
  "Stillwater",
  "Calmsea",
  "Pacifica",
  "Haven",
  "Softwave",
  "Gulliver",
  "Barnacle",
  "Siren",
  "Echo Wave",
  "Stormbreaker",
  "Deep Diver",
  "Benthic",
  "Zenith",
  "Voyager Blue",
  "Coraline",
  "Splashington"
)

# View(Blue_Whale_Data)
# Blue_Whale_Data$timestamp[1:10]
# substr(Blue_Whale_Data$timestamp[1], 6, 10)



# substr(Blue_Whale_Data$timestamp[220], 1, 4)
# index <- 0
# length(Blue_Whale_Data$timestamp)
# nrow(data.frame(Blue_Whale_Data$timestamp))
# vector <- c(vector, substr(Blue_Whale_Data$timestamp[2], 6, 10))
# substr(Blue_Whale_Data$timestamp[2], 6, 7) == "08"

# Function to get stock!
get_whale_stock1 <- function(month_filter, day_filter) {
  df <- data.frame(matrix(ncol = 15, nrow = 0))
  colnames(df) <- c(
    "01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12",
    "Whale",
    "Lat",
    "Long"
  )

  na_rows <- data.frame(matrix(NA, nrow = 1, ncol = 15))
  colnames(na_rows) <- colnames(df)
  df <- rbind(na_rows, df)



  create_vector <- function(month) {
    for (n in 1:length(Blue_Whale_Data$timestamp)) {
      if (substr(Blue_Whale_Data$timestamp[n], 6, 7) == month) {
        day <- substr(Blue_Whale_Data$timestamp[n], 9, 10) # Extract only the day
        whale <- Blue_Whale_Data$`tag-local-identifier`[n]
        lat <- Blue_Whale_Data$`location-lat`[n]
        long <- Blue_Whale_Data$`location-long`[n]
        # Create a new row for each day
        new_row <- as.data.frame(matrix(ncol = 15, nrow = 1))
        colnames(new_row) <- c(
          "01",
          "02",
          "03",
          "04",
          "05",
          "06",
          "07",
          "08",
          "09",
          "10",
          "11",
          "12",
          "Whale",
          "Lat",
          "Long"
        )
        new_row[[month]] <- day  # Assign only the day to the month column
        new_row[["Whale"]] <- whale
        new_row[["Lat"]] <- lat
        new_row[["Long"]] <- long
        df <<- rbind(df, new_row)  # Append the row to df

      }
    }
  }
  create_vector(month_filter)

  # View(df)
  # print(any(df$`03` == "11", na.rm = TRUE))

  df_day <- data.frame(matrix(ncol = ncol(df), nrow = 0))
  colnames(df_day) <- colnames(df)

  for (n in 1:nrow(df)) {
    if (!is.na(df[[month_filter]][n]) &&
        df[[month_filter]][n] == day_filter) {
      row_to_add <- df[n, , drop = FALSE]

      # Append this row to the df_day_07 dataframe
      df_day <- rbind(df_day, row_to_add)

    }
  }

  # Blue_Whale_Data$`location-long`[[3]]
  # df_day$`Lat`[[1]]
  #
  #
  # top_dist <- 0
  # used_whales <- c()
  # for(n in 1:length(unique(df_day$Whale))) {
  #
  #   if(df_day$`Whale`[[n]] )
  #
  # }



  ##
  # 1. Find unique whale identifiers
  unique_whales <- unique(df_day$Whale)

  # 2. Initialize an empty dataframe to store results
  whale_locations <- data.frame(
    Whale = character(),
    First_Lat = numeric(),
    First_Long = numeric(),
    Last_Lat = numeric(),
    Last_Long = numeric(),
    Dist = vector(),
    stringsAsFactors = FALSE
  )

  # 3. Loop through each unique whale
  for (whale_id in unique_whales) {
    # Subset df for the current whale
    whale_data <- df_day[df_day$Whale == whale_id, ]

    # Ensure there is data for this whale before proceeding
    if (nrow(whale_data) > 0) {
      # Extract latitude and longitude values, removing NA
      whale_lats <- na.omit(whale_data$Lat)
      whale_longs <- na.omit(whale_data$Long)

      first_lat <- NA
      first_long <- NA
      last_lat <- NA
      last_long <- NA
      Dist <- NA

      # Get first Lat and Long (if available)
      if (length(whale_lats) > 0) {
        first_lat <- whale_lats[1] # Assuming rows are in order, first row is first location
        last_lat <- whale_lats[nrow(whale_data)] # Last row is last location
      }
      if (length(whale_longs) > 0) {
        first_long <- whale_longs[1] # First row is first location
        last_long <- whale_longs[nrow(whale_data)] # Last row is last location
      }

      first_coords <- c(first_long, first_lat)
      last_coords <- c(last_long, last_lat)

      # Add a new row to the results dataframe
      new_row <- data.frame(
        Whale = whale_id,
        First_Lat = first_lat,
        First_Long = first_long,
        Last_Lat = last_lat,
        Last_Long = last_long,
        Dist = distHaversine(first_coords, last_coords, r = 6378137) # geosphere package)
      )
      whale_locations <- rbind(whale_locations, new_row)
    }
  }

  # 4. View the results dataframe
  # View(whale_locations)

  long_dist <- max(whale_locations$Dist, na.rm = TRUE)
  i <- ""

  for (n in 1:nrow(whale_locations)) {
    if (ceiling(whale_locations$Dist)[[n]] == ceiling(long_dist)) {
      i <- whale_locations$Whale[[n]]
    }
  }


  i

  Unique_Whales_Dataframe$stock <- stocks



  j <- ""
  for (n in 1:nrow(Unique_Whales_Dataframe)) {
    if (Unique_Whales_Dataframe$Unique_Whales_Vector[[n]] == i) {
      j <- Unique_Whales_Dataframe$stock[[n]]
    }
  }

  # print which whale corresponds
  print(Unique_Whales_Dataframe$Names[(which(Unique_Whales_Dataframe$Unique_Whales_Vector == i))])
  print("picks...")

  stonk <- which(Money_Data_Money$Sector == j)
  picked_stock <- stonk[sample(1:length(stonk), 1)]

  FINAL_STOCK <- Money_Data_Money$Name[picked_stock]
  print(FINAL_STOCK)
}

#Use Strings
get_whale_stock1("03", "18")

# Make data frame of every month with days, associated with whale
# Pick day, and then sort by whale. Then pick one at random due to undecided factor
# maybe due to biggest displacement by day?
# whale is equal to whatever stock, and then the amount of stock bought is due to
# the amount of distance? (or from second place)

# Unique_Whales_Dataframe$Money_Data_Categorized_Unique_Sector_Sub <- c(Money_Data_Categorized_Unique_Sector_Sub, rep(NA, nrow(df)-length(Money_Data_Categorized_Unique_Sector_Sub)))



# View(Money_Data_Money)


#have each whale associated with a group of stocks (normal to crazy)
