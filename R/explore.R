library(tidyverse)
library(readxl)

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
  file_data_times <- file_data %>% 
    #many different date formats here
    mutate(time = ifelse(test = "POSIXct" %in% class(time),
                         yes = time, 
                         no = parse_date_time(time, orders = c("%b %d, %Y %I:%M %p",
                                                                              "%Y-%m-%d %H:%M",
                                                                              "%Y-%m-%d %H:%M:%S"),
                                                         tz = "America/Los_Angeles")))
  
  
  loyola_data <- bind_rows(loyola_data, file_data_times)
}

loyola_data <- mutate(loyola_data, time = as_datetime(time, tz = "America/Los_Angeles"))
#double check that columns weren't switched
assertthat::assert_that(all((loyola_data$inbound + loyola_data$outbound) == loyola_data$total))

######### 3rd street data
third_data <- tibble()
for(f in third_files) {
  print(f)
  file_data <- read_excel(f, skip = 1) %>% 
    select(-contains("...1")) %>% 
    rename(time = 'Time',
           westbound = matches("Westbound direction - North loops$"),
           eastbound = matches("Eastbound direction - South loops$"),
           total = contains("total")) %>% 
    rename_at(vars(contains("cyclist")), function(x) "westbound_cyclist")

  third_data <- bind_rows(third_data, file_data)
  print(names(file_data))
}
