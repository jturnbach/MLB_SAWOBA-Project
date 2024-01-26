##########
# Front Matter #
##########

# Set working directory to DS440 folder
wd <- getwd()

# Load libraries
library(tidyverse)
library(DBI)
library(RMySQL)

# Database credentials
DB_USER <- Sys.getenv("PSU_BASE_USER")
DB_PASSWORD <- Sys.getenv("PSU_BASE_PASSWORD")
DB_DBNAME <- Sys.getenv("PSU_BASE_DBNAME")
DB_PORT <- as.numeric(Sys.getenv("PSU_BASE_PORT"))
DB_HOST <- Sys.getenv("PSU_BASE_HOST")

# Connect to psubase_db
con <- dbConnect(MySQL(), user = DB_USER, password = DB_PASSWORD, dbname = DB_DBNAME, port = DB_PORT, host = DB_HOST)

# Define query function
query_db <- function(query_logic) {
  query <- paste0(query_logic)
  
  rs <- dbSendQuery(con, query)
  df <- fetch(rs, n = -1)
  
  return(df)
}

# Pull Statcast data for regular season games since 2020
query <- paste("SELECT *
                FROM sc_raw
                WHERE game_year >= 2019 AND game_type = 'R';")

sc_raw <- query_db(query)


# Partition data to store on GitHub
sc_2020 <- sc_raw %>% filter(game_year == 2020)
sc_2021 <- sc_raw %>% filter(game_year == 2021)
sc_2022 <- sc_raw %>% filter(game_year == 2022)
sc_2023 <- sc_raw %>% filter(game_year == 2023)

# Save files
write.csv(sc_2020, paste0(wd, "/sc_2020.csv"), row.names = FALSE)
write.csv(sc_2021, paste0(wd, "/sc_2021.csv"), row.names = FALSE)
write.csv(sc_2022, paste0(wd, "/sc_2022.csv"), row.names = FALSE)
write.csv(sc_2023, paste0(wd, "/sc_2023.csv"), row.names = FALSE)