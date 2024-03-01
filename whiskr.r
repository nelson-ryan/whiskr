
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
    mutate(
        Value = readr::parse_number(Value)
    ) %>%
    filter(Value > 7) %>%
    select(Value, Timestamp) %>%
    ggplot(
        aes(x = Timestamp, y = Value)
    ) +
    ggtitle("Artemis: Weight over time") +
    geom_point() +
    geom_smooth() + ggdark::dark_mode()

ggsave('whiskr.png')

history %>%
    filter(Activity %in% activityilst) %>%
    mutate(
        Timestamp = mdy_hm(
            stringr::str_replace(
                Timestamp, " ", "/2024 "
            )
        )
    ) %>%
    mutate(
        Time = hms::as_hms(Timestamp),
        Week = lubridate::week(Timestamp)
    ) %>%
    ggplot(
        aes(
            y = Time,
            x = factor(Week)
        )
    ) +
    ggtitle("Artemis' Litter Box Visits throughout the Day, Grouped by Week") +
    scale_x_discrete(name = "Week") +
    scale_y_time(
        breaks = c(
            hms::as_hms("05:00:00"),
            hms::as_hms("07:00:00"),
            hms::as_hms("16:00:00"),
            # hms::as_hms("18:15:00"),
            # hms::as_hms("19:40:00"),
            hms::as_hms("20:00:00")
        ),
        minor_breaks = NULL
    ) +
    geom_violin(adjust = .0001) +
    ggdark::dark_mode()
