
library(ggplot2)
library(dplyr)
library(googledrive)
library(lubridate)
library(tidyverse)
library(purrr)

drive_auth("ryan@nelsonr.dev")

history =
    drive_ls() %>%
    mutate(modified = map_chr(drive_resource, "modifiedTime")) %>%
    filter(modified == max(modified)) %>%
    drive_read_string() %>%
    readr::read_csv(col_name = TRUE)

activityilst = c(
    "Pet Weight Recorded"
)

# Weight over time
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
        Weight = readr::parse_number(Value)
    ) %>%
    filter(Weight > 7) %>%
    select(Weight, Timestamp) %>%
    ggplot(
        aes(x = Timestamp, y = Weight)
    ) +
    ggtitle("Artemis: Weight over time") +
    geom_point() +
    scale_y_continuous(
        breaks = seq(0, 100, .1),
        minor_breaks = seq(0, 100, .5)
    ) +
    geom_smooth()# + ggdark::dark_mode()

ggsave('whiskr.png')

# Violin by week
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
            hms::as_hms("00:00:00"),
            hms::as_hms("06:00:00"),
            hms::as_hms("12:00:00"),
            hms::as_hms("18:00:00"),
            # hms::as_hms("19:40:00"),
            hms::as_hms("24:00:00")
        ),
        minor_breaks = NULL
    ) +
    geom_violin(adjust = .2) +
    ggdark::dark_mode()

# Dot time by day
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
        Date = date(Timestamp),
        Weight = readr::parse_number(Value)
    ) %>%
    filter(Weight > 7) %>%
    ggplot() +
    ggtitle("Artemis' Litter Box Visits by Day") +
    geom_point(
        aes(
            y = Time,
            x = Date
        ),
        size = 1,
        color = "black",
        alpha = .5
    ) +
    scale_x_date(date_minor_breaks = "1 day") +
    scale_y_time(
        breaks = c(
            hms::as_hms("00:00:00"),
            hms::as_hms("05:00:00"),
            hms::as_hms("07:00:00"),
            hms::as_hms("16:00:00"),
            hms::as_hms("20:00:00"),
            hms::as_hms("24:00:00")
        ),
        minor_breaks = NULL
    )
