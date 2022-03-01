## Data Visualization (GOVT16-QSS17) Spring 2021
## Data Visualization Project 2, Step 1
##
## Name: Megan Ren
## Date: 5/7/21


# Initial settings --------------------------------------------------------

library(tidyverse)
library(rvest)
library(countrycode)

# Scrape and load data ----------------------------------------------------

libdem <- readRDS("data/V-Dem-CY-Core-v11.1.rds") %>% 
  filter(year >= 2013)

# function to scrape data from Women in National Parliaments website for a
# given year
extract_data <- function(year) {
  if (year < 2013 || year > 2019) {
    stop("year must be between 2013 and 2019", call. = FALSE)
  }
  char_rep <- as.character(year)
  param <- str_extract(char_rep, "..$")
  URL <- paste0("http://archive.ipu.org/wmn-e/arc/classif0101",
                param,
                ".htm")
  cat(URL)
  html <- read_html(URL)
  table <- html_table(html, na.strings = c("---", "\""), convert = TRUE)
  selected <- table[[3]]
  
  names(selected) <- get_row_names(selected)
  return(selected %>% slice(-(1:3)) %>% fill(Rank) %>% mutate(date = year))
}

# function to extract column names from the third row of the tibble
get_row_names <- function(tibble) {
  temp <- as.character(slice(tibble, 3))[1:6]
  full <- append(temp, paste("Upper", as.character(slice(tibble, 3))[7:10]))
  return(full)
}

# see what the raw data looks like 
extract_data(2015)

# take data from 2013-2019
df2013 <- extract_data(2013)
df2014 <- extract_data(2014)
df2015 <- extract_data(2015)
df2016 <- extract_data(2016)
df2017 <- extract_data(2017)
df2018 <- extract_data(2018)
df2019 <- extract_data(2019)

# collect all years
aggregate <- bind_rows(df2013, df2014, df2015, df2016, df2017, df2018, df2019)

aggregate


# Clean data --------------------------------------------------------------


# we have some issues with the percentages, need to do some cleaning
aggregate %>% 
  mutate(percentage = str_extract(`% W`, "[^\\%]+")) %>% 
  mutate(percentage = as.numeric(percentage)) %>% 
  filter(is.na(percentage)) %>% 
  select(Rank, Country, Women, `% W`, percentage)

aggregate %>% 
  filter(str_detect(Country, "\\d")) # these numbers are footnotes

# clean data: remove weird values for percentage, change variable names, clean
# up footnotes
cleaned <- aggregate %>% 
  mutate(percentage = str_extract(`% W`, "[^\\%]+"),
         percentage_upper = ifelse(!is.na(`Upper Elections`),
                                   str_extract(`Upper % W`, "[^\\%]+"),
                                   NA)) %>% 
  mutate(percentage = ifelse(str_detect(percentage, ","),
                             sub(",", ".", percentage),
                             percentage),
         percentage_upper = ifelse(str_detect(percentage_upper, ","),
                                   sub(",", ".", percentage_upper),
                                   percentage_upper)) %>% 
  mutate(percentage = as.numeric(percentage),
         percentage_upper = as.numeric(percentage_upper)) %>% 
  filter(!is.na(percentage)) %>% 
  mutate(Country = ifelse(str_detect(Country, "\\d"),
                          str_extract(Country, ".+?(?=\\s\\d)"),
                          Country)) %>% 
  rename(rank = Rank, country = Country,
         election_date = Elections,
         total_seats = `Seats*`,
         women_seats = Women,
         election_date_upper = `Upper Elections`,
         total_seats_upper = `Upper Seats*`,
         women_seats_upper = `Upper Women`
         ) %>%
  select(-c(`% W`, `Upper % W`))


# double check that there are no NAs in percentage
cleaned %>% 
  filter(is.na(percentage) || is.na(percentage_upper))

# check that we've deleted the footnote markers
cleaned %>% 
  filter(str_detect(country, "\\d")) 


# Transform  --------------------------------------------------------------

# average upper and lower house %'s if upper house exists, else take lower house
averaged <- cleaned %>% 
  mutate(avg = ifelse(is.na(percentage_upper),
                      percentage,
                      (percentage_upper + percentage)/2))

# convert to country codes to be able to join with libdem data 
standardized <- averaged %>% 
  mutate(id = countrycode(sourcevar = country,
                          origin = "country.name",
                          destination = "iso3c"))

# filter out unnecessary variables
libdem1 <- libdem %>% 
  select(country_name, country_text_id, v2x_libdem, year)


# join datasets 
joined <- left_join(libdem1, standardized, by = c("country_text_id" = "id",
                                        "year" = "date"))

# make percentages for women in parliaments decimal representations
final <- joined %>% 
  mutate(avg = ifelse(!is.na(avg),
                      avg/100,
                      NA))


# Save data ---------------------------------------------------------------

saveRDS(final, "output/democracy_and_women.RDS")


