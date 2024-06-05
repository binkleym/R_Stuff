################################################################################
### Plot the SPC 2x2 Categorical Risk map for the continental USA            ###
### Data from:  https://www.spc.noaa.gov/gis/                                ###
################################################################################
library(tidyverse)
library(ggpattern)
library(patchwork)
library(tigris)
library(sf)

# Which day X categorical map do you want?  Valid input is 1 or 2
day <- "1"

# CRS projection for continental US
crs_proj <- "EPSG:9311"

# Function to pull the map from the SPC website
read_map <- function(day, type) {
  paste("https://www.spc.noaa.gov/products/outlook/day", day, "otlk_", type, ".lyr.geojson", sep = "") %>%
  read_sf() %>% 
  st_transform(crs = crs_proj) %>%
  mutate(LABEL2 = ifelse(LABEL == "SIGN", "SIG", LABEL2))
}

# Load maps
map_cat  <- read_map(day, "cat")
map_tor  <- read_map(day, "torn")
map_hail <- read_map(day, "hail")
map_wind <- read_map(day, "wind")

# Define names/colors for SPC risk levels
spc_cat_levels <- 
  tribble(
    ~LABEL,  ~LABEL2,  ~fill,     ~color,    ~pattern,
    "TSTM",  "TSTM",   "#c0e8c0", "#646464", "none",
    "MRGL",  "MRGL",   "#7fc57f", "#3c783c", "none",
    "SLGT",  "SLGT",   "#f6f67f", "#ff9600", "none",
    "ENH",   "ENH",    "#e6c27f", "#ff7f00", "none",
    "MDT",   "MDT",    "#e6807f", "#cd0000", "none",
    "HIGH",  "HIGH",   "#ff80ff", "#ff00ff", "none"
  )

spc_tor_levels <- 
  tribble(
    ~LABEL, ~LABEL2,  ~fill,        ~color,        ~pattern,
    "0.02",  "2%",   "#80c580",     "#008200",     "none",
    "0.05",  "5%",   "#c5a393",     "#8b4726",     "none",
    "0.10", "10%",   "#ffeb80",     "#ff9600",     "none",
    "0.15", "15%",   "#ff8080",     "#ff0000",     "none",
    "0.30", "30%",   "#ff80ff",     "#ff00ff",     "none",
    "0.45", "45%",   "#c896f7",     "#912cee",     "none",
    "0.60", "60%",   "#104e8b",     "#104e8b",     "none",
    "SIGN", "SIG",   "transparent", "transparent", "crosshatch"
)

spc_wind_levels <- tribble(
  ~LABEL, ~LABEL2,  ~fill,       ~color,        ~pattern,
  "0.05",  "5%",  "#c5a393",     "#8b4726",     "none",
  "0.15", "15%",  "#ffeb80",     "#ff9600",     "none",
  "0.30", "30%",  "#ff8080",     "#ff0000",     "none",
  "0.45", "45%",  "#ff80ff",     "#ff00ff",     "none",
  "0.60", "60%",  "#c896f7",     "#912cee",     "none",
  "SIGN", "SIG",  "transparent", "transparent", "crosshatch"
)
  
spc_hail_levels <- tribble(
  ~LABEL, ~LABEL2, ~fill,        ~color,        ~pattern,
  "0.05",  "5%",  "#c5a393",     "#8b4726",     "none",
  "0.15", "15%",  "#ffeb80",     "#ff9600",     "none",
  "0.30", "30%",  "#ff8080",     "#ff0000",     "none",
  "0.45", "45%",  "#ff80ff",     "#ff00ff",     "none",
  "0.60", "60%",  "#c896f7",     "#912cee",     "none",
  "SIGN", "SIG",  "transparent", "transparent", "crosshatch"
)

# SPC products return datetime as a string ("20240604 1200")
# Convert that to something more legible.
format_datetime_txt <- function(datetimestring) {
  year  <- substr(datetimestring, 1, 4)
  month <- substr(datetimestring, 5, 6)
  day   <- substr(datetimestring, 7, 8)
  hour  <- substr(datetimestring, 9, 10)
  min   <- substr(datetimestring, 11, 12)

  datetime <- paste(year, "-", month, "-", day, " ", hour, ":", min, ":00", sep = "")
  datetime_str <- paste(weekdays(datetime %>% as.POSIXct(tz = "GMT"), abbreviate = TRUE), " ", datetime, "Z", sep = "")
  return(datetime_str)
}

# Pull the start/end valid dates for the map
weather_valid  <- map_cat %>% st_drop_geometry() %>% select(VALID)  %>% unique() %>% pull() %>% format_datetime_txt(.)
weather_expire <- map_cat %>% st_drop_geometry() %>% select(EXPIRE) %>% unique() %>% pull() %>% format_datetime_txt(.)
weather_issued <- map_cat %>% st_drop_geometry() %>% select(ISSUE)  %>% unique() %>% pull() %>% format_datetime_txt(.)

# Load state/county outline maps from TIGRIS
options(tigris_use_cache = TRUE)
map_states <- 
  tigris::states() %>% 
  filter(!STUSPS %in% c("AS", "MP", "GU", "PR", "VI", "AK", "HI")) %>% 
  st_transform(crs = crs_proj)

# Function to graph SPC data (helps keeps maps consistent)
graph_spc_map <- function(map_type, map_title, map_legend_title, map_legend_levels) {
  ggplot() +
  theme_bw() + 
  theme(
    plot.title           = element_text(hjust = 0.5),
    legend.title         = element_blank(),
    legend.spacing.x     = unit(0, "cm"),
    legend.position      = "bottom",
    legend.margin        = margin(-20, 0, 0, 0),
    legend.key.spacing.x = unit(0, "npc"),
    legend.key.width     = unit(1/(1.8*nrow(map_legend_levels)), "snpc"),
    axis.ticks           = element_blank(),
    axis.text.x          = element_blank(), 
    axis.text.y          = element_blank(),
  ) +
  labs(x = "", y = "", title = map_title) + 

  geom_sf(data = map_type %>% filter(LABEL != "SIGN"), aes(fill = LABEL, color = LABEL), show.legend = TRUE) +
  geom_sf(data = map_type, color = "#000000", fill = "transparent",  show.legend = TRUE, linewidth = 0.5) +
  geom_sf(data = map_type, aes(color = LABEL), fill = "transparent",  show.legend = FALSE, linewidth = 0.5) +
  geom_sf_pattern(data = map_type %>% filter(LABEL == "SIGN"), aes(pattern = LABEL), fill = "transparent", color = "transparent", linewidth = 0.3, linetype = "dashed",
                  pattern_spacing = 0.02, pattern_size = 0.15, pattern_angle = 45, pattern_alpha = 0.5, show.legend = TRUE) +
  
  geom_sf(data = map_states, fill = NA, color = "black", linewidth = 0.7) +
      
  scale_fill_manual(
    name = "",
    drop = FALSE, 
    limits = map_legend_levels %>% pull(LABEL), 
    labels = map_legend_levels %>% pull(LABEL2),
    values = map_legend_levels %>% pull(fill)
  ) + 
  scale_color_manual(
    name = "",
    drop = FALSE, 
    limits = map_legend_levels %>% pull(LABEL), 
    labels = map_legend_levels %>% pull(LABEL2),
    values = map_legend_levels %>% pull(color)
  ) +
  scale_pattern_manual(
    name = "",
    drop = FALSE, 
    limits = map_legend_levels %>% pull(LABEL), 
    labels = map_legend_levels %>% pull(LABEL2),
    values = map_legend_levels %>% pull(pattern)
    ) +
  guides(
    colour  = guide_legend(nrow = 1, title.position = "bottom", title.hjust = 0.5, label.position = "top"),
    fill    = guide_legend(nrow = 1, title.position = "bottom", title.hjust = 0.5, label.position = "top"),
    pattern = guide_legend(nrow = 1, title.position = "bottom", title.hjust = 0.5, label.position = "top"),
    ) 
}

# Render the individual maps
g_spc_cat  <- graph_spc_map(map_cat,  "Categorical Outlook", "Categorical Risk",           spc_cat_levels)
g_spc_tor  <- graph_spc_map(map_tor,  "Tornado Outlook",     "Tornado Probability (in %)", spc_tor_levels)
g_spc_hail <- graph_spc_map(map_hail, "Hail Outlook",        "Hail Probability (in %)",    spc_hail_levels)
g_spc_wind <- graph_spc_map(map_wind, "Wind Outlook",        "Wind Probability (in %)",    spc_wind_levels)

# Assemble the final map
g_final <-
  (g_spc_cat | g_spc_tor) / (g_spc_hail | g_spc_wind) +
  plot_annotation(
    title = paste("Storm Prediction Center - Day", day, "Outlook 4-Panel"),
    subtitle = paste("Issued: ", weather_issued, ", Valid: ", weather_valid, " - ", weather_expire, sep = ""),
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
      plot.subtitle = element_text(hjust = 0.5)
      )
  )
print(g_final)
