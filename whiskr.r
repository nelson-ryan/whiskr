
library(ggplot2)
library(dplyr)
library(googledrive)
library(lubridate)
library(tidyverse)
library(purrr)
library(RSQLite)
library(slider)

dbfile = "whiskr.db"
sqlite = dbConnect(RSQLite::SQLite(), dbfile)

# Flag indicating whether new data is being added or all data will be refreshed
appenddata = dbExistsTable(sqlite, name = "history")
if (!appenddata) {
    stop(paste(dbfile, " not found. Restore file from repo and re-run."))
}

drive_auth(email = "ryan@nelsonr.dev")

drivefile = drive_ls(type = "csv") %>%
    mutate(modified = map_chr(drive_resource, "modifiedTime")) %>%
    filter(stringr::str_starts(name, pattern = "litter-robot"))

if (appenddata) {
    maxrecord = tbl(src = sqlite, "history") %>%
        filter(Timestamp == max(Timestamp)) %>%
        pull(Timestamp) %>%
        as_datetime()
    drivefile = drivefile %>% filter(modified > maxrecord)
}

importdata = drivefile$id %>%
    map(function(id) {
        readr::read_csv(
            drive_read_string(id),
            col_names = TRUE
        )
    }) %>%
    bind_rows() %>%
    filter(str_detect(Activity, "Weight Recorded")) %>%
    distinct(Timestamp, Value) %>%
    mutate(
        Timestamp = mdy_hm(
            stringr::str_replace(
                Timestamp,
                " ",
                stringr::str_c("/", year(today()), " ") # TODO remove -1
            )
        )
    ) %>%
    filter(
        Timestamp < today() # filters previous-year Dec results when in Jan
    ) %>%
    mutate(
        Weight = readr::parse_number(Value),
    ) %>%
    select(Timestamp, Weight)


if (appenddata) {
    dbcontent = tbl(src = sqlite, "history") %>%
        collect() %>%
        mutate(
            Timestamp = as_datetime(Timestamp)
        )
    insertdata = anti_join(
        x = importdata,
        y = dbcontent,
        by = c("Timestamp", "Weight")
    )
    dbAppendTable(conn = sqlite, name = "history", value = insertdata)
} else {
    dbWriteTable(conn = sqlite, name = "history", value = importdata)
}


history = dbReadTable(conn = sqlite, name = "history") %>%
    mutate(
        Timestamp = as_datetime(Timestamp)
    ) %>%
    arrange(Timestamp)

# Define the window of time in which to apply scale() for each data point
windows = slide_index(
    .x = history,
    .i = history$Timestamp,
    .f = ~.x,
    .before = days(300),
    .after = days(300)
)

# Function to apply scale() to each window,
# then return the individual measurement's scale value
get_single_scale_value = function(df, timestamp) {
    scale_vals = scale(df$Weight)
    scale_vals[df$Timestamp == timestamp]
}

# Apply and filter based on scale value
outlier_filtered = history %>%
    mutate(
        scale = unlist(
            map2(
                windows,
                .$Timestamp,
                get_single_scale_value
            )
        )
    ) %>%
    mutate(
        Reten = ifelse(abs(scale) > 4, FALSE, TRUE)
    ) %>%
    filter(
        Reten == TRUE
    )

# Weight over time
weightplot = outlier_filtered %>%
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
    geom_smooth() +
    ggdark::dark_mode()
weightplot


# Dot time by day
visits = outlier_filtered %>%
    mutate(
        Time = hms::as_hms(Timestamp),
        Date = date(Timestamp)
    ) %>%
    mutate(
        Period = if_else(
            Date > "2024-04-05",
            "after",
            "before"
        )
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
    ) +
    ggdark::dark_mode()
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
    scale_x_date(
        expand = c(0, 0)
    ) +
    scale_y_continuous(
    ) +
    ggdark::dark_mode()
visits_counts

ggsave(
    filename = "weight.png",
    plot = weightplot,
    width = 9.88,
    height = 4.97,
    dpi = 120
)
ggsave(
    filename = "visits_time.png",
    plot = visits_time,
    width = 9.88,
    height = 4.97,
    dpi = 120
)
ggsave(
    "visits_counts.png",
    plot = visits_counts,
    width = 9.88,
    height = 4.97,
    dpi = 120
)
