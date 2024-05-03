
library(ggplot2)
library(dplyr)
library(googledrive)
library(lubridate)
library(tidyverse)
library(purrr)

drive_auth("ryan@nelsonr.dev")

activitylist = c(
    "Pet Weight Recorded",
    "Weight Recorded"
)

files = drive_ls(type = "csv") %>%
    mutate(modified = map_chr(drive_resource, "modifiedTime")) %>%
    filter(stringr::str_starts(name, "litter-robot"))

history = files$id %>%
    map(function(id) {
        readr::read_csv(
            drive_read_string(id),
            col_names = TRUE
        )
    }) %>%
    bind_rows() %>%
    filter(Activity %in% activitylist) %>%
    distinct(Timestamp, Value)

history = history %>%
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
    filter(Weight < 15, Weight > 7)

# Weight over time
weightplot = history %>%
    select(Weight, Timestamp) %>%
    ggplot(
        aes(x = Timestamp, y = Weight)
    ) +
    ggtitle("Artemis' Weight over time") +
    ylab("Weight (lbs)") +
    geom_point() +
    scale_y_continuous(
        breaks = seq(0, 100, .5),
        minor_breaks = seq(0, 100, .1)
    ) +
    scale_x_datetime(
        expand = c(0, 0)
    ) +
    geom_smooth()# + ggdark::dark_mode()
weightplot

# Dot time by day
visits = history %>%
    mutate(
        Time = hms::as_hms(Timestamp),
        Date = date(Timestamp)
    )

visits_time = visits %>%
    ggplot(
        aes(
            y = Time,
            x = Date
        )
    ) +
    ggtitle("Artemis' Litter Box Visits: Time of Day") +
    ylab("Visits") +
    stat_density_2d_filled(
        aes(fill = ..level..),
        contour_var = "ndensity",
        show.legend = FALSE,
        alpha = .5
    ) +
    scale_fill_manual(
        values = colorRampPalette(c("white", "brown"))(10)
    ) +
    geom_point(
        stroke = 0,
        alpha = .5,
        size = 1.5
    ) +
    scale_x_date(
        date_minor_breaks = "1 day",
        expand = c(0, 0)
    ) +
    scale_y_time(
        breaks = hms::as_hms(
            c(
                "00:00:00",
                "05:00:00",
                "07:00:00",
                "16:00:00",
                "20:00:00",
                "24:00:00"
            )
        ),
        minor_breaks = hms::as_hms(
            seq.POSIXt(
                from = as.POSIXct("01-01-01"),
                by = "1 hour",
                length.out = 24
            )
        ),
        expand = c(0, 0),
        limits = c(
            hms::as_hms("00:00:00"),
            hms::as_hms("24:00:00")
        ),
        labels = function(label) strftime(x = label, format = "%H:%M")
    )
visits_time

visits_counts = visits %>%
    group_by(
        Date
    ) %>%
    summarise(
        Visits = n()
    ) %>%
    ggplot() +
    ggtitle("Artemis' Litter Box Visit Counts by Day") +
    geom_col(
        aes(
            y = Visits,
            x = Date,
            fill = Visits
        ),
        alpha = 0.5,
        show.legend = FALSE
    ) +
    scale_fill_gradient(
        low = "deepskyblue",
        high = "brown"
    ) +
    geom_smooth(
        aes(
            x = Date,
            y = Visits
        ),
        method = "gam",
        level = .55
    ) +
    scale_x_date(
        date_minor_breaks = "1 day",
        expand = c(0, 0)
    ) +
    scale_y_continuous(
    )
visits_counts

ggsave("weight.png", plot = weightplot, width = 9.88, height = 4.97, dpi = 120)
ggsave("visits_time.png", plot = visits_time, width = 9.88, height = 4.97, dpi = 120)
ggsave("visits_counts.png", plot = visits_counts, width = 9.88, height = 4.97, dpi = 120)
