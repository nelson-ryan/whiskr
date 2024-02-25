
library(ggplot2)
library(dplyr)
library(googledrive)
library(lubridate)
library(tidyverse)

history = drive_read_string("litter-robot_4_activity_2-25-2024.csv")
history = readr::read_csv(history)

activityilst = c(
    #"Cat Detected",
    "Pet Weight Recorded"
)

history %>%
    filter(Activity %in% activityilst) %>%
    mutate(
        Timestamp = mdy_hm(
            stringr::str_replace(
                Timestamp, " ", "/2024 "
            )
        )
    ) %>%
    mutate(Value = readr::parse_number(Value)) %>%
    filter(Value > 7) %>%
    select(Value, Timestamp) %>%
    # group_by(Activity) %>%
    # summarise(n = n()) %>%
    ggplot(
        aes(x = Timestamp, y = Value)
    ) +
    geom_point() +
    geom_smooth() + ggdark::dark_mode()


