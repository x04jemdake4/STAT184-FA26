# Homework 3

# Install packages once in the Console if needed:
# install.packages(c("ggplot2", "dplyr", "tidyr", "readr", "sf"))
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

vehicles <- ggplot2::mpg           # Full dataset
gems <- ggplot2::diamonds         # Full dataset

# Folder for saved plots
dir.create("figures", showWarnings = FALSE)


### Problem 1

# Part A
p_a <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~ drv) +
  scale_x_continuous(limits = c(NA, 5)) +
  labs(x = "Engine displacement (litres)",
       y = "Highway fuel economy (mpg)") +
  theme_bw()
p_a
ggsave("figures/p1_partA.png", p_a, width = 9, height = 3.5)

# Rows dropped by the x limit (displ > 5)
vehicles |> filter(displ > 5) |> count(drv)

# The reference x-axis stops at 5 and its lines match lm fits on displ <= 5 only
# (ex. 4wd goes 26.8 -> 14.7 mpg). scale_x_continuous(limits) drops the 36 cars
# with displ > 5 before fitting, so the lines agree. coord_cartesian() would zoom
# without dropping them and give different lines. R warns about the removed rows.
#   drv       n
#1 4        19
#2 f         1
#3 r        16

# Explanation
# There are three lines because of facet_wrap(~ drv). Each panel only gets its own drv rows,
# so geom_smooth fits a separate lm in each panel. Color is only in geom_point, so the lines
# are not grouped by color, they stay default blue


# Part B
# Count plot
p_b_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  theme_bw()
p_b_count
ggsave("figures/p1_partB_count.png", p_b_count, width = 4.5, height = 3.5)

# Proportion plot
p_b_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion within drive type") +
  theme_bw()
p_b_prop
ggsave("figures/p1_partB_prop.png", p_b_prop, width = 4.5, height = 3.5)

# Sample 
vehicles |> count(drv)
#  drv       n
#1 4       103
#2 f       106
#3 r        25

# Explanation
# position = fill rescales every bar to 1, so each bar shows shares within its own drv.
# All bars are the same height even though r only has 25 cars and 4 / f have 103 and 106.


# Part C
# Smaller table: drop the few very large diamonds (carat >= 3)
smaller <- gems |> filter(carat < 3)

tibble(
  total_records = nrow(gems),
  retained = nrow(smaller),
  excluded = nrow(gems) - nrow(smaller)
)
#  total_records retained excluded
#1         53940    53900       40

p_fine <- ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.01, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  labs(title = "Bin width = 0.01", x = "Carat", y = "Count") +
  theme_bw()

p_coarse <- ggplot(smaller, aes(x = carat)) +
  geom_histogram(binwidth = 0.5, boundary = 0, closed = "right") +
  coord_cartesian(xlim = c(0, 3)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5)) +
  labs(title = "Bin width = 0.5", x = "Carat", y = "Count") +
  theme_bw()

p_fine
p_coarse
ggsave("figures/p1_partC_fine.png", p_fine, width = 7, height = 3.5)
ggsave("figures/p1_partC_coarse.png", p_coarse, width = 7, height = 3.5)

# Diamonds recorded as exactly 0.99 and 1.00 carat (full gems)
gems |>
  filter(near(carat, 0.99) | near(carat, 1.00)) |>
  count(carat)
#  carat     n
#1  0.99    23
#2  1     1558

# Explanation
# The 0.01 bins show tall spikes at round sizes like 0.3, 0.5, 0.7, 1, 1.5 and 2 carats.
# Counts fall off slowly to the right of each spike, but are very low just below it.
# Ex. only 23 diamonds are 0.99 carat but 1558 are exactly 1.00. 


### Problem 2
# Region: City of Chicago. Facility: Chicago Public Library locations.

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean", recursive = TRUE, showWarnings = FALSE)

lib_url <- "https://data.cityofchicago.org/api/views/x8fc-8rcq/rows.csv?accessType=DOWNLOAD"
lib_raw_file <- "data/raw/chicago_libraries_raw.csv"

ca_url <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson?$limit=5000"
ca_raw_file <- "data/raw/chicago_community_areas_raw.geojson"

# Download once, raw files are never edited after this.
# If a download fails, use Export > CSV / GeoJSON on the portal page and save to these paths.
if (!file.exists(lib_raw_file)) {
  download.file(lib_url, lib_raw_file, mode = "wb")
}
if (!file.exists(ca_raw_file)) {
  download.file(ca_url, ca_raw_file, mode = "wb")
}

lib_download_date <- as.Date(file.info(lib_raw_file)$mtime)
ca_download_date <- as.Date(file.info(ca_raw_file)$mtime)

# Data sources
data_sources <- tibble(
  dataset = "Libraries - Locations, Contact Information, and Usual Hours of Operation",
  publisher = "City of Chicago Data Portal (Chicago Public Library)",
  dataset_id = "x8fc-8rcq",
  landing_page = "https://data.cityofchicago.org/d/x8fc-8rcq",
  download_url = lib_url,
  format = "CSV (full export, not paginated)",
  raw_file = lib_raw_file,
  downloaded = as.character(lib_download_date),
  license = "No license listed; City of Chicago Data Portal Terms of Use",
  notes = "Portal metadata lists last update 2024-10-04. LOCATION column holds (latitude, longitude)."
)
write_csv(data_sources, "data_sources.csv")

# Data
libraries_raw <- read_csv(lib_raw_file, show_col_types = FALSE)
names(libraries_raw)
nrow(libraries_raw)

# Clean names (the export has "NAME " with a trailing space)
libraries_named <- libraries_raw |>
  rename_with(~ tolower(trimws(.x))) |>
  rename(any_of(c(name = "branch")))

# LOCATION looks like "(41.97, -87.71)", first number is lat, second is lon
libraries_parsed <- libraries_named |>
  mutate(
    raw_row = row_number(),
    facility_name = trimws(name),
    lat = parse_number(sub(",.*$", "", location)),
    lon = parse_number(sub("^[^,]*,", "", location)),
    exact_duplicate = duplicated(libraries_raw),
    coord_problem = case_when(
      is.na(location) ~ "missing location",
      is.na(lat) | is.na(lon) ~ "lat/lon could not be read",
      lat < 41.6 | lat > 42.1 | lon < -88.0 | lon > -87.5 ~ "outside Chicago range",
      .default = NA_character_
    )
  )

# Coordinate problems
libraries_parsed |>
  filter(!is.na(coord_problem)) |>
  select(raw_row, facility_name, location, coord_problem)

libraries_parsed |>
  filter(exact_duplicate) |>
  select(raw_row, facility_name, location)

# Different records that share coordinates (kept, not removed)
libraries_parsed |>
  filter(is.na(coord_problem), !exact_duplicate) |>
  add_count(lat, lon, name = "n_same_coords") |>
  filter(n_same_coords > 1) |>
  select(raw_row, facility_name, lat, lon, n_same_coords)

# Removals with reasons
library_removals <- libraries_parsed |>
  mutate(reason = case_when(
    !is.na(coord_problem) ~ coord_problem,
    exact_duplicate ~ "exact duplicate of an earlier record",
    .default = NA_character_
  )) |>
  filter(!is.na(reason)) |>
  select(raw_row, facility_name, location, reason)
library_removals
write_csv(library_removals, "data/clean/library_removals.csv")

sites_clean <- libraries_parsed |>
  filter(is.na(coord_problem), !exact_duplicate) |>
  mutate(site_id = sprintf("LIB%03d", row_number())) |>
  select(site_id, facility_name, address, lat, lon)
write_csv(sites_clean, "data/clean/libraries_clean.csv")

n_distinct(sites_clean$site_id) == nrow(sites_clean)
nrow(sites_clean) >= 20

tibble(
  raw_records = nrow(libraries_raw),
  coord_problems = sum(!is.na(libraries_parsed$coord_problem)),
  exact_duplicates = sum(libraries_parsed$exact_duplicate),
  removed = nrow(library_removals),
  retained = nrow(sites_clean)
)

# Projected CRS: NAD83 / UTM zone 16N (EPSG:26916), units are meters
sites_m <- sites_clean |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(26916)
st_crs(sites_m)$Name
st_crs(sites_m)$units_gdal

# City outline for the maps (union of the community areas, checked in Problem 3)
city_outline <- st_read(ca_raw_file, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(26916) |>
  st_union()

# Left aligned, smaller captions so they fit under the maps
map_theme <- theme_bw() +
  theme(plot.caption = element_text(hjust = 0, size = 7),
        plot.caption.position = "plot")

map_caption <- paste0(
  "Source: City of Chicago Data Portal, Libraries (x8fc-8rcq), downloaded ", lib_download_date,
  ".\nCity outline: Boundaries - Community Areas (igwz-8jzy), downloaded ", ca_download_date, "."
)

# Maps
p_sites <- ggplot() +
  geom_sf(data = city_outline, aes(fill = "City of Chicago"), color = "grey40") +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.8) +
  scale_fill_manual(values = c("City of Chicago" = "grey92"), name = NULL) +
  scale_color_manual(values = c("Library" = "navy"), name = NULL) +
  labs(title = paste0("Chicago Public Library locations (n = ", nrow(sites_m), ")"),
       caption = map_caption) +
  map_theme
p_sites
ggsave("figures/p2_sites.png", p_sites, width = 6, height = 7)

# Buffers: union so overlapping circles count once, not clipped to the city
buffer_500 <- sites_m |> st_buffer(dist = 500) |> st_union()
buffer_1000 <- sites_m |> st_buffer(dist = 1000) |> st_union()

buffer_zones <- st_sf(
  radius_m = c(500, 1000),
  geometry = c(buffer_500, buffer_1000)
)

# Keep the points and both combined zones
st_write(sites_m, "data/clean/sites_m.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(buffer_zones, "data/clean/buffer_zones.gpkg", delete_dsn = TRUE, quiet = TRUE)

p_buffers <- ggplot() +
  geom_sf(data = city_outline, fill = "grey92", color = "grey40") +
  geom_sf(data = buffer_500, aes(fill = "Within 500 m of a library"),
          color = "darkorange3", alpha = 0.8) +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.2) +
  scale_fill_manual(values = c("Within 500 m of a library" = "orange"), name = NULL) +
  scale_color_manual(values = c("Library" = "navy"), name = NULL) +
  labs(title = "Libraries and combined 500 m buffer area",
       caption = map_caption) +
  map_theme
p_buffers
ggsave("figures/p2_buffers.png", p_buffers, width = 6, height = 7)

buffer_summary <- buffer_zones |>
  mutate(union_area_km2 = as.numeric(st_area(geometry)) / 1e6) |>
  st_drop_geometry()
buffer_summary

# 1000 m area should be at least as large as 500 m
buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1]
buffer_summary$union_area_km2[2] / buffer_summary$union_area_km2[1]

# Overlap check: separate circles added up vs the union
tibble(
  radius_m = c(500, 1000),
  sum_of_circles_km2 = nrow(sites_m) * pi * c(500, 1000)^2 / 1e6,
  union_area_km2 = buffer_summary$union_area_km2
)

# How much of each zone falls outside the city boundary (kept in the totals)
tibble(
  radius_m = c(500, 1000),
  outside_city_km2 = c(
    as.numeric(st_area(st_difference(buffer_500, city_outline))) / 1e6,
    as.numeric(st_area(st_difference(buffer_1000, city_outline))) / 1e6
  ),
  city_area_km2 = as.numeric(st_area(city_outline)) / 1e6
)

# Explanation
# Raw CSV is read once and never changed. Rows with missing or unreadable LOCATION, or
# coordinates outside Chicago, are removed, and so are exact repeated records. Libraries that
# only share coordinates are kept. Points are in EPSG:26916  so buffers are in meters.
# Each buffer set is unioned, so overlap is counted once, and nothing is clipped to the city.
# The union areas are smaller than the summed circles because nearby libraries overlap. 


### Problem 3

# Boundaries
districts_raw <- st_read(ca_raw_file, quiet = TRUE)
nrow(districts_raw)
names(districts_raw)

data_sources <- bind_rows(
  data_sources,
  tibble(
    dataset = "Boundaries - Community Areas",
    publisher = "City of Chicago Data Portal",
    dataset_id = "igwz-8jzy",
    landing_page = "https://data.cityofchicago.org/d/igwz-8jzy",
    download_url = ca_url,
    format = "GeoJSON (API, $limit = 5000 so all 77 areas come in one page)",
    raw_file = ca_raw_file,
    downloaded = as.character(ca_download_date),
    license = "No license listed; City of Chicago Data Portal Terms of Use",
    notes = "Portal metadata lists last update 2026-03-25. 77 official community areas."
  )
)
write_csv(data_sources, "data_sources.csv")
data_sources |> select(dataset, dataset_id, format, downloaded)

# Boundary checks
nrow(districts_raw) < 5000
tibble(
  areas = nrow(districts_raw),
  missing_ids = sum(is.na(districts_raw$area_num_1)),
  duplicate_ids = sum(duplicated(districts_raw$area_num_1)),
  missing_names = sum(is.na(districts_raw$community)),
  empty_shapes = sum(st_is_empty(districts_raw)),
  invalid_shapes = sum(!st_is_valid(districts_raw))
)

# Repair any invalid shapes, same CRS as sites_m
districts <- districts_raw |>
  st_make_valid() |>
  st_transform(st_crs(sites_m)) |>
  transmute(
    district_id = as.integer(area_num_1),
    district_name = community
  )

sum(!st_is_valid(districts))
st_crs(districts) == st_crs(sites_m)

p_overlay <- ggplot() +
  geom_sf(data = districts, fill = "grey95", color = "grey50") +
  geom_sf(data = sites_m, aes(color = "Library"), size = 1.2) +
  scale_color_manual(values = c("Library" = "navy"), name = NULL) +
  labs(title = "Libraries over Chicago community areas",
       caption = map_caption) +
  map_theme
p_overlay
ggsave("figures/p3_overlay.png", p_overlay, width = 6, height = 7)

# Matches
match_list <- st_intersects(sites_m, districts)

site_match_audit <- sites_m |>
  st_drop_geometry() |>
  mutate(n_matches = lengths(match_list)) |>
  select(site_id, facility_name, n_matches)
write_csv(site_match_audit, "site_match_audit.csv")

site_match_audit |>
  mutate(match_group = case_when(
    n_matches == 0 ~ "zero",
    n_matches == 1 ~ "one",
    .default = "multiple"
  )) |>
  count(match_group) |>
  complete(match_group = c("zero", "one", "multiple"), fill = list(n = 0))

# Sites without exactly one match
site_match_audit |> filter(n_matches != 1)

one_match <- which(lengths(match_list) == 1)
sites_assigned <- sites_m[one_match, ] |>
  mutate(district_id = districts$district_id[unlist(match_list[one_match])])

n_assigned <- nrow(sites_assigned)
n_unassigned <- nrow(sites_m) - n_assigned

# Summary (zero count areas kept)
site_counts <- sites_assigned |>
  st_drop_geometry() |>
  count(district_id, name = "n_sites")

district_counts_sf <- districts |>
  left_join(site_counts, by = join_by(district_id)) |>
  mutate(
    n_sites = coalesce(n_sites, 0L),
    area_km2 = as.numeric(st_area(geometry)) / 1e6,
    sites_per_km2 = n_sites / area_km2
  )

district_counts <- district_counts_sf |>
  st_drop_geometry() |>
  as_tibble() |>
  select(district_id, district_name, n_sites, area_km2, sites_per_km2) |>
  arrange(desc(n_sites), district_id)
print(district_counts, n = Inf)

# Checks
all(district_counts$area_km2 > 0)
sum(district_counts$n_sites) == n_assigned
sum(district_counts$n_sites == 0)

# Maps: same boundaries, CRS, and extent, separate legends
map_box <- st_bbox(district_counts_sf)
zero_areas <- district_counts_sf |> filter(n_sites == 0)

map_caption_3 <- paste0(
  map_caption, "\n", n_unassigned,
  " site(s) left unassigned (not exactly one area match).",
  "\nRed outline = 0 libraries; grey fill = missing value (none)."
)

p_count <- ggplot(district_counts_sf) +
  geom_sf(aes(fill = n_sites), color = "white", linewidth = 0.2) +
  geom_sf(data = zero_areas, aes(color = "0 libraries"), fill = NA, linewidth = 0.6) +
  scale_fill_viridis_c(name = "Libraries\n(count)", limits = c(0, NA), na.value = "grey60") +
  scale_color_manual(values = c("0 libraries" = "red"), name = NULL) +
  coord_sf(xlim = map_box[c("xmin", "xmax")], ylim = map_box[c("ymin", "ymax")]) +
  labs(title = "Libraries per community area", caption = map_caption_3) +
  map_theme
p_count
ggsave("figures/p3_count.png", p_count, width = 6.5, height = 7.5)

p_density <- ggplot(district_counts_sf) +
  geom_sf(aes(fill = sites_per_km2), color = "white", linewidth = 0.2) +
  geom_sf(data = zero_areas, aes(color = "0 libraries"), fill = NA, linewidth = 0.6) +
  scale_fill_viridis_c(name = "Libraries\nper sq. km", limits = c(0, NA), na.value = "grey60") +
  scale_color_manual(values = c("0 libraries" = "red"), name = NULL) +
  coord_sf(xlim = map_box[c("xmin", "xmax")], ylim = map_box[c("ymin", "ymax")]) +
  labs(title = "Libraries per square kilometer by community area", caption = map_caption_3) +
  map_theme
p_density
ggsave("figures/p3_density.png", p_density, width = 6.5, height = 7.5)

# Does the ranking change once area is accounted for?
top_count <- district_counts |> slice_max(n_sites, n = 5, with_ties = FALSE)
top_density <- district_counts |> slice_max(sites_per_km2, n = 5, with_ties = FALSE)
top_count |> select(district_name, n_sites, area_km2)
top_density |> select(district_name, n_sites, area_km2, sites_per_km2)
length(intersect(top_count$district_id, top_density$district_id))

# Explanation
# Each library is matched to community areas with st_intersects, only sites with exactly one
# match are counted, and areas with no library are kept as 0. Counts total the assigned sites
# and every area is positive. Most areas have 0 to 2 libraries, so the count map looks flat and
# big areas can look well served. 