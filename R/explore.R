library(tidyverse)
library(readxl)
library(lubridate)

loyola_files <- list.files('in', pattern = "loyola", full.names = TRUE) #meter at Loyola Dr and Drexel Dr
third_files <- list.files('in', pattern = "3rd", full.names = TRUE) #meter downtown on 3rd St

#Loyola and Drexel data
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


######### 3rd street data
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
