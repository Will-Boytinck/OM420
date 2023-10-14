library(dplyr)
library(nycflights13)
library(lubridate)
library(readxl)

# ctrl + shift + enter to run..
# Why the 'run' key is a step through is beyond me...

# -------------------------------------------------------------------------------------------------------
# REFERENCES:
# 3.2D: https://www.statology.org/r-prop-table/
# all usage of unique function: https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/unique
# na.omit: https://www.statology.org/na-omit-in-r/
# all lectures pertaining to the usage of R, with credit to Ilbin Lee
# gsub: https://www.statology.org/gsub-r/ | I also used chatgpt 3.5 with this function, as I couldn't get the correct syntax down
# -------------------------------------------------------------------------------------------------------


# Part 1
flights <- filter(flights, carrier %in% c("UA", "AA", "DL")) #a.
arrived_late <- filter(flights, arr_delay > 120 & dep_delay <= 0) #b.
flights_departed <- filter(flights, between(month, 7, 9)) #c.


# Part 2

# Reading the data
bus <- read.csv("business.csv")
bus_1_500 <- read.csv("business.csv",nrows=500)
bus_sample <- bus_1_500[sample(nrow(bus_1_500), 20),]

# Writing the data
write.table(bus_sample, file="business_sample.csv")
saveRDS(bus_sample, file="business_sample.rds")

# RDS vs CSV
bus_sample_csv <- read.csv("business_sample.csv")
bus_sample_rds <- readRDS("business_sample.rds")
identical_test_one = identical(bus_sample_csv, bus_sample)
identical_test_two = identical(bus_sample_rds, bus_sample)

# identical check
#print(identical_test_one)
#print(identical_test_two)

# Comment 5b.
# bus_sample_csv (which is the read object of "business_sample.csv") and bus_sample are deemed NOT identical
# bus_sample_rds (which is the read object of "business_sample.rds") and bus_Sample are deemed identical

# Part 3.1
business <- readRDS("business.rds")
review <- readRDS("review.rds")
review$date <- ymd(review$date)
review <- review %>%
  mutate(year_written = year(date),
         month_written = month(date),
         day_of_week_written = wday(date, label = TRUE, week_start = 1))

# Part 3.2
customer <- read_excel("customersT.xlsx")
head(customer, 20)
# finding unique values, printing them
unique_values <- unique(customer$CU_GENDER)
print(unique_values)
# excluding NA values
cleaned_column <- na.omit(customer$CU_GENDER)
percentages <- prop.table(table(cleaned_column)) * 100
print(percentages)
# gender labels now: A\r\n F\r\n M\r\n
# removing the return and new line characters
percentages_with_labels <- gsub("\r\n", "", names(percentages))
cleaned_percentages <- setNames(percentages, percentages_with_labels)
print(cleaned_percentages)








