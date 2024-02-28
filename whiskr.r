
library(ggplot2)
library(dplyr)
library(googledrive)
library(lubridate)
library(tidyverse)
library(purrr)

history =
    drive_ls() %>%
    mutate(modified = map_chr(drive_resource, "modifiedTime")) %>%
    filter(modified == max(modified)) %>% drive_read_string() %>%
    readr::read_csv(col_name = TRUE)

activityilst = c(
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
    ggplot(
        aes(x = Timestamp, y = Value)
    ) +
    geom_point() +
    geom_smooth() + ggdark::dark_mode()

ggsave('whiskr.png')


