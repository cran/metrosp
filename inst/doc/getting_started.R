## -----------------------------------------------------------------------------
#| label: setup
library(metrosp)
library(dplyr)


## -----------------------------------------------------------------------------
library(sf)

lines


## -----------------------------------------------------------------------------
metro_colors


## -----------------------------------------------------------------------------
glimpse(passengers_entrance)


## -----------------------------------------------------------------------------
#| code-fold: true
library(ggplot2)

theme_series <- theme_minimal(base_family = "Avenir", base_size = 10) +
  theme(
    panel.background = element_rect(fill = "#f5f5f5"),
    plot.background = element_rect(fill = "#f5f5f5"),
    plot.margin = margin(20, 10, 20, 10),
    plot.title = element_text(family = "Lora", size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.25),
    axis.title.x = element_blank(),
    axis.line.x = element_line(color = "gray10", linewidth = 0.5),
    axis.ticks.x = element_line(color = "gray10", linewidth = 0.5),
    strip.background = element_rect(fill = "#0D1B2A"),
    strip.text = element_text(color = "#ffffff"),
    legend.position = "bottom"
  )


## -----------------------------------------------------------------------------
glimpse(passengers_entrance)


## -----------------------------------------------------------------------------
total_entrance <- passengers_entrance |>
  filter(metric_abb == "total", line_number != 99)


## -----------------------------------------------------------------------------
#| code-fold: true
ggplot(total_entrance, aes(x = date, y = value, color = line_name)) +
  geom_line(lwd = 0.8) +
  facet_wrap(vars(line_name), scales = "free_y") +
  scale_color_manual(values = metro_colors) +
  guides(color = "none") +
  labs(
    title = "Total Entrance by Line",
    subtitle = "Total monthly passenger entrances by metro line",
    x = NULL,
    y = "Total Entrance"
  ) +
  theme_series


## -----------------------------------------------------------------------------
glimpse(passengers_transported)


## -----------------------------------------------------------------------------
daily_avg <- passengers_transported |>
  filter(metric_abb == "mdu", line_number != 99)


## -----------------------------------------------------------------------------
#| code-fold: true
ggplot(daily_avg, aes(x = date, y = value, color = line_name)) +
  geom_line(lwd = 0.8) +
  facet_wrap(vars(line_name), scales = "free_y") +
  scale_color_manual(values = metro_colors) +
  labs(
    title = "Daily Average Passenger Transported by Line",
    subtitle = "Monthly averages across business days (thousands)",
    x = NULL,
    y = "Daily Average"
  ) +
  guides(color = "none") +
  theme_series


## -----------------------------------------------------------------------------
glimpse(station_averages)


## -----------------------------------------------------------------------------
#| code-fold: true
line4st <- station_averages |>
  filter(line_number == 4)

ggplot(line4st, aes(x = date, y = avg_passenger)) +
  geom_line(lwd = 0.8, color = metro_colors["Yellow"]) +
  facet_wrap(vars(station_name), scales = "free_y") +
  labs(
    x = NULL,
    y = "Average Passengers",
    title = "Passengers per Station (line 4)"
  ) +
  theme_series


## -----------------------------------------------------------------------------
glimpse(station_daily)


## -----------------------------------------------------------------------------
#| code-fold: true
#| fig-width: 10
#| fig-height: 8
line4st_daily <- station_daily |>
  filter(line_number == 4, year == 2023)

ggplot(line4st_daily, aes(x = date, y = passengers)) +
  geom_smooth(
    lwd = 0.8,
    color = metro_colors["Yellow"],
    method = "loess",
    span = 0.65
  ) +
  facet_wrap(vars(station_name), scales = "free_y", ncol = 3) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Passengers per Station (line 4, 2023)",
    subtitle = "LOESS smoothed trend",
    x = NULL,
    y = "Average Passengers"
  ) +
  theme_series

