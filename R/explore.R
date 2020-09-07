library(tidyverse)
library(readxl)
library(lubridate)

loyola_files <- list.files('in', pattern = "loyola", full.names = TRUE) #meter at Loyola Dr and Drexel Dr
third_files <- list.files('in', pattern = "3rd", full.names = TRUE) #meter downtown on 3rd St

##### read Loyola and Drexel data  #####
loyola_data <- tibble()
for(f in loyola_files) {
  print(f)
  file_data <- read_excel(f, skip = 1) %>% 
    select(-contains("...1")) 
  #column names aren't consistent, but they do stay in the same order
  print(names(file_data))
  names(file_data) <- c("time", "total", "inbound", "outbound")
 
  if(!"POSIXct" %in% class(file_data$time)) {
    file_data_times <- file_data %>% 
      mutate(time = parse_date_time(time, 
                                    orders = c("%b %d, %Y %I:%M %p",
                                               "%Y-%m-%d %H:%M",
                                               "%Y-%m-%d %H:%M:%S"),
                                    tz = "America/Los_Angeles"))
  } else {
    file_data_times <- file_data %>% 
      mutate(time = force_tz(time, 'America/Los_Angeles'))
  }
  
  loyola_data <- bind_rows(loyola_data, file_data_times)
}
#double check that columns weren't switched
assertthat::assert_that(all((loyola_data$inbound + loyola_data$outbound) == loyola_data$total))
assertthat::assert_that(!anyDuplicated(loyola_data$time))


##### Read 3rd street data #####
third_data <- tibble()
for(f in third_files) {
  print(f)
  file_data <- read_excel(f, skip = 1, guess_max = 4000) %>% 
    select(-contains("...1")) %>% 
    rename(time = 'Time',
           eastbound = matches("Eastbound direction - South loops$"),
           total = contains("total")) %>% 
    rename_at(vars(contains("cyclist")), function(x) "westbound_cyclist") %>%
    rename_at(vars(matches("Westbound direction - North loops$")), function(x) "westbound")  
    if(!"POSIXct" %in% class(file_data$time)) {
      file_data_times <- file_data %>% 
        mutate(time = parse_date_time(time, 
                                      orders = c("%b %d, %Y %I:%M %p",
                                                  "%Y-%m-%d %H:%M",
                                                  "%Y-%m-%d %H:%M:%S"),
               tz = "America/Los_Angeles"))
    } else {
      file_data_times <- file_data %>% 
        mutate(time = force_tz(time, 'America/Los_Angeles'))
    }
  third_data <- bind_rows(third_data, file_data_times)
  print(names(file_data))
}  
assertthat::assert_that(!anyDuplicated(third_data$time))

##### Exploratory analysis #####

loyola_data_long <- pivot_longer(loyola_data, 
                                 cols = c("inbound", "outbound", "total"),
                                 names_to = "direction",
                                 values_to = "count") %>% 
  mutate(direction = factor(x = direction, levels = c("total", "inbound", "outbound")),
         date = as_date(time),
         hour = hour(time),
         month = month(time),
         day_of_week = wday(time, label = TRUE, abbr = FALSE))

library(ggplot2)
ggplot(loyola_data_long, aes(x= time, y = count, color = direction)) +
  geom_line() + 
  facet_wrap('direction', ncol = 1)

ggplot(loyola_data, aes(x = inbound, y = outbound)) +
  geom_point()

#Time of day
#by month
#by day of week
#inbound vs outbound
loyola_long_weekdays <- loyola_data_long %>% 
  filter(direction == "total") %>% 
  group_by(day_of_week) %>% 
  summarize(n_total = sum(count))
ggplot(loyola_long_weekdays, aes(x = day_of_week, y = n_total)) +
  coord_flip() + geom_col()

# trim down to 12 months to eliminate partial years
loyola_long_12_months <- loyola_data_long %>% 
  filter(time > '2018-02-01 00:00:00',
         time < '2019-01-31 23:59:59') 
loyola_months <- loyola_long_12_months %>% 
  filter(direction == "total") %>% 
  group_by(month) %>% 
  summarize(n_total = sum(count)) %>% 
  mutate(month = month(month, label = TRUE))
ggplot(loyola_months, aes(x = month, y = n_total)) +
  coord_flip() + geom_col()

loyola_hour <- loyola_data_long %>% 
  #filter(direction == "total") %>% 
  group_by(hour, direction) %>%
  summarize(n_total = sum(count),
            n_median = median(count))
ggplot(loyola_hour, aes(x = hour, y = n_median)) +
  geom_col() +
  facet_wrap('direction') 
#morning rush is more pronounced

#are inbound and outbound the same each day?  
# by hour for commuting direction?
loyola_grouped_daily <- loyola_data_long %>% 
  group_by(date, direction) %>% 
  summarize(count_daily = sum(count)) %>% 
  pivot_wider(id_cols = c("date", "direction"),
              names_from = "direction",
              values_from = "count_daily") %>% 
  mutate(diff = inbound - outbound)
ggplot(loyola_grouped_daily, aes(x = diff)) +
  geom_bar() +
  labs(x = "Inbound minus outbound, daily",
       y = "Number of days")

#TODO: 12 months?, grouped by day and hour
#augment with variables
#doy, hour, day of week ; start simple
#then temp and precip
#UC davis schedule?

#linear regression assumptions: linear relationships, independent & uncorrelated residuals
loyola_data_hourly <- loyola_data %>% 
  mutate(hour = hour(time),
         date = as_date(time)) %>% 
  group_by(date, hour) %>% 
  summarize(total = sum(total)) %>% 
  mutate(week = week(date),
         dow = wday(date),
         dow_sq = (dow - 3.5)^2) #similar transformation for hour
plot(loyola_data_hourly)
plot(loyola_data_hourly$dow_sq, loyola_data_hourly$total)

t <- lm(total ~ week + dow_sq + hour, data = loyola_data_hourly)
