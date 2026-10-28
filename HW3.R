# DS 220 - Homework 3
# Work through each numbered section in RStudio (Cmd+Enter on Mac).
# Data: City of Chicago Data Portal; the two unmodified GeoJSON downloads belong in data_raw/.
# This script generates the assignment plots, checks, tables, and maps in outputs/.

# 0. SETUP --------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(sf)

dir.create("data_raw", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)
vehicles <- ggplot2::mpg
gems <- ggplot2::diamonds
run_date <- as.character(Sys.Date())

# 1A. ENGINE SIZE / HIGHWAY MPG ------------------------------------------
p_A <- ggplot(vehicles, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), alpha = .65) +
  geom_smooth(method = "lm", se = FALSE, color = "royalblue3", linewidth = 1) +
  facet_wrap(~drv, nrow = 1) +
  labs(x = "Engine displacement (litres)", y = "Highway fuel economy (mpg)", color = "drv") +
  theme_bw()
print(p_A)
ggsave("outputs/p_A.png", p_A, width = 10, height = 4, dpi = 200)
# Answer: Faceting subsets data by drv into 3 panels. geom_smooth fits one line to
# the data in each panel even though drv is mapped to color only in geom_point.

# 1B. COUNTS / WITHIN-DRIVE-TYPE PROPORTIONS ------------------------------
p_B_count <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar() +
  scale_fill_manual(values = c("4" = "#F8766D", "5" = "#7CAE00",
                               "6" = "#00BFC4", "8" = "#C77CFF")) +
  labs(x = "drv", y = "count", fill = "factor(cyl)") + theme_bw()
p_B_prop <- ggplot(vehicles, aes(x = drv, fill = factor(cyl))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("4" = "#F8766D", "5" = "#7CAE00",
                               "6" = "#00BFC4", "8" = "#C77CFF")) +
  labs(x = "drv", y = "Proportion within drive type", fill = "factor(cyl)") +
  theme_bw()
print(p_B_count)
print(p_B_prop)
ggsave("outputs/p_B_count.png", p_B_count, width = 6, height = 4.5, dpi = 200)
ggsave("outputs/p_B_prop.png", p_B_prop, width = 6, height = 4.5, dpi = 200)
# Answer: position="fill" scales each drive type to total height 1; the total
# heights are equal even if the numbers of cars in those groups differ.

# 1C. TWO HISTOGRAMS, SAME DOCUMENTED SMALLER TABLE ----------------------
# The sheet says 'same smaller table' but doesn't specify the subset.
# Here we consistently use all diamonds with carat <= 2.00.
gems_small <- gems %>% filter(!is.na(carat), carat <= 2.00)
cat("P1C records retained:", nrow(gems_small), "\n")
cat("P1C records excluded:", nrow(gems) - nrow(gems_small), "\n")
write_csv(data.frame(retained = nrow(gems_small), excluded = nrow(gems) - nrow(gems_small)),
          "outputs/carat_subset_counts.csv")
carat_counts <- data.frame(
  carat = c(.99, 1.00),
  n = c(sum(gems$carat == .99, na.rm = TRUE), sum(gems$carat == 1.00, na.rm = TRUE))
)
print(carat_counts)
write_csv(carat_counts, "outputs/carat_counts.csv")
p_fine <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = .01, boundary = 0, closed = "right", fill = "steelblue", color = "white") +
  coord_cartesian(xlim = c(0, 2)) +
  labs(title = "Diamond carat: bin width 0.01", x = "Carat", y = "Count") + theme_bw()
p_coarse <- ggplot(gems_small, aes(x = carat)) +
  geom_histogram(binwidth = .5, boundary = 0, closed = "right", fill = "steelblue", color = "white") +
  coord_cartesian(xlim = c(0, 2)) +
  labs(title = "Diamond carat: bin width 0.5", x = "Carat", y = "Count") + theme_bw()
print(p_fine)
print(p_coarse)
ggsave("outputs/p_fine.png", p_fine, width = 7, height = 4.5, dpi = 200)
ggsave("outputs/p_coarse.png", p_coarse, width = 7, height = 4.5, dpi = 200)
# Answer: The 0.01-carat bins reveal clustering at specific reported carat values,
# especially around common round carat weights, obscured by the 0.5-carat bins.


# Import original Chicago library CSV and boundary GeoJSON
raw_libs <- readr::read_csv(
  "data_raw/libraries.csv",
  show_col_types = FALSE
)

lib_coords <- tidyr::extract(
  raw_libs,
  LOCATION,
  into = c("latitude", "longitude"),
  regex = "\\(([-0-9.]+),\\s*([-0-9.]+)\\)",
  convert = TRUE,
  remove = FALSE
)

cat("Missing coordinate pairs:",
    sum(is.na(lib_coords$latitude) |
          is.na(lib_coords$longitude)), "\n")

stopifnot(
  !anyNA(lib_coords$latitude),
  !anyNA(lib_coords$longitude)
)

raw_sites <- sf::st_as_sf(
  lib_coords,
  coords = c("longitude", "latitude"),
  crs = 4326
)

raw_areas <- sf::st_read(
  "data_raw/community_areas.geojson",
  quiet = TRUE
)
 

cat("Raw site columns:\n")
print(names(raw_sites))
cat("Raw boundary columns:\n")
print(names(raw_areas))


# Create required data provenance file, including download date and license caveat
source_info <- data.frame(
  dataset = c("Libraries - Locations, Contact Information, and Usual Hours of Operation",
              "Boundaries - Community Areas"),
  publisher = c("City of Chicago / Chicago Public Library", "City of Chicago"),
  source_page = c("https://data.cityofchicago.org/d/x8fc-8rcq",
                  "https://data.cityofchicago.org/d/igwz-8jzy"),
  
  raw_file = c("data_raw/libraries.csv",
               "data_raw/community_areas.geojson"),
  date_accessed = rep(run_date, 2),
  notes = c("Official CSV export; LOCATION coordinates converted to points; hours may not reflect temporary closures",
            "Official GeoJSON download; community-area boundaries")
  
)
write_csv(source_info, "data_sources.csv")

# Clean polygons for use as study area (and reuse unchanged in Problem 3)
find_col <- function(options, actual) {
  matching <- intersect(options, actual)
  if (length(matching) == 0) stop(paste("Expected column not found. Available:", paste(actual, collapse = ", ")))
  matching[[1]]
}
names(raw_areas) <- tolower(names(raw_areas))
id_col <- find_col(c("area_numbe", "area_num_1", "area_num", "area_number", "district_id"), names(raw_areas))
area_col <- find_col(c("community", "community_area", "area_name", "name"), names(raw_areas))
cat("Raw boundary rows:", nrow(raw_areas), "\n")
cat("Raw boundary missing IDs:", sum(is.na(raw_areas[[id_col]]) | trimws(as.character(raw_areas[[id_col]])) == ""), "\n")
cat("Raw boundary duplicate IDs:", sum(duplicated(raw_areas[[id_col]])), "\n")
cat("Raw boundary empty geometries:", sum(st_is_empty(raw_areas)), "\n")
cat("Raw boundary invalid geometries:", sum(!st_is_valid(raw_areas)), "\n")
invalid_areas_before <- sum(!st_is_valid(raw_areas))
areas_m <- raw_areas[!st_is_empty(raw_areas), ]
areas_m <- st_transform(areas_m, 26916)  # NAD83 / UTM zone 16N, METERS
areas_m <- st_make_valid(areas_m)
areas_m <- areas_m[!st_is_empty(areas_m) & st_is_valid(areas_m), ]
areas_m$district_id <- as.character(areas_m[[id_col]])
areas_m$district_name <- as.character(areas_m[[area_col]])
if (anyNA(areas_m$district_id) || anyDuplicated(areas_m$district_id)) {
  stop("Missing/duplicate community area IDs remain: inspect raw boundaries manually.")
}
cat("Boundary geometries requiring repair:", invalid_areas_before, "\n")
cat("Boundary rows removed (empty/unrepairable):", nrow(raw_areas) - nrow(areas_m), "\n")
cat("Clean boundary rows:", nrow(areas_m), "\n")
cat("Clean boundary invalid shapes:", sum(!st_is_valid(areas_m)), "\n")

# Clean library point records, retaining co-located facilities
names(raw_sites) <- tolower(names(raw_sites))
name_col <- find_col(c("branch_", "branch", "name", "library_name", "location_name"), names(raw_sites))
raw_sites$site_id <- sprintf("LIB%03d", seq_len(nrow(raw_sites)))
raw_sites$facility_name <- as.character(raw_sites[[name_col]])
nonempty <- !st_is_empty(raw_sites)
cat("Raw library records:", nrow(raw_sites), "\n")
cat("Missing point geometry:", sum(!nonempty), "\n")
site_work <- raw_sites[nonempty, ]
site_work <- st_transform(site_work, 4326)
coords <- st_coordinates(site_work)
if (nrow(coords) != nrow(site_work) || ncol(coords) < 2) {
  stop("Expected one point geometry per library. Inspect the source geometries.")
}
valid_coords <- is.finite(coords[, 1]) & is.finite(coords[, 2]) &
  coords[, 1] >= -180 & coords[, 1] <= 180 &
  coords[, 2] >= -90 & coords[, 2] <= 90
valid_name <- !is.na(site_work$facility_name) & nzchar(trimws(site_work$facility_name))
cat("Out-of-range or missing coordinates:", sum(!valid_coords), "\n")
cat("Missing facility names:", sum(!valid_name), "\n")
site_work <- site_work[valid_coords & valid_name, ]
# Study area = Chicago community area polygons; include all eligible libraries therein
in_chicago <- lengths(st_intersects(site_work, st_transform(areas_m, 4326))) > 0
cat("Libraries outside community-area study boundary:", sum(!in_chicago), "\n")
site_work <- site_work[in_chicago, ]
site_xy <- st_coordinates(site_work)
cat("Repeated exact name+coordinates:", sum(duplicated(data.frame(
  name = site_work$facility_name, lon = site_xy[, 1], lat = site_xy[, 2]
))), "(reported, NOT automatically dropped)\n")
sites_m <- st_transform(site_work, 26916)
cat("Clean sites retained:", nrow(sites_m), "\n")
cat("Total sites removed:", nrow(raw_sites) - nrow(sites_m), "\n")
cat("Coordinate reference system: EPSG 26916, meters\n")
stopifnot(nrow(sites_m) >= 20, nrow(areas_m) >= 5, !anyDuplicated(sites_m$site_id))
st_write(sites_m, "outputs/sites_m.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(areas_m, "outputs/areas_m.gpkg", delete_dsn = TRUE, quiet = TRUE)
site_lons_lats <- st_coordinates(st_transform(sites_m, 4326))
write_csv(data.frame(site_id = sites_m$site_id,
                     facility_name = sites_m$facility_name,
                     longitude = site_lons_lats[,1], latitude = site_lons_lats[,2]),
          "outputs/cleaned_locations.csv")

# 2. BUFFER GEOMETRIES, AREAS, AND MAPS ----------------------------------
buffer500 <- st_union(st_buffer(sites_m, 500))
buffer1000 <- st_union(st_buffer(sites_m, 1000))
buffer_summary <- data.frame(
  radius_m = c(500, 1000),
  union_area_km2 = c(as.numeric(st_area(buffer500)), as.numeric(st_area(buffer1000))) / 1e6
)
print(buffer_summary)
stopifnot(buffer_summary$union_area_km2[2] >= buffer_summary$union_area_km2[1])
write_csv(buffer_summary, "outputs/buffer_summary.csv")
st_write(st_sf(radius_m = 500, geometry = buffer500),
         "outputs/buffer500.gpkg", delete_dsn = TRUE, quiet = TRUE)
st_write(st_sf(radius_m = 1000, geometry = buffer1000),
         "outputs/buffer1000.gpkg", delete_dsn = TRUE, quiet = TRUE)
source_caption <- paste("Source: City of Chicago Data Portal | Accessed", run_date)
p_sites <- ggplot() +
  geom_sf(data = areas_m, fill = "grey97", color = "grey75", linewidth = .2) +
  geom_sf(data = sites_m, aes(color = "Public libraries"), size = 1.5) +
  scale_color_manual(values = c("Public libraries" = "#146b8c"), name = "Facilities") +
  labs(title = "Chicago Public Library locations", caption = source_caption) +
  theme_bw()
p_buffers <- ggplot() +
  geom_sf(data = areas_m, fill = "grey97", color = "grey75", linewidth = .2) +
  geom_sf(data = st_sf(radius_m = "500 m buffer union", geometry = buffer500),
          aes(fill = radius_m), alpha = .55) +
  geom_sf(data = sites_m, aes(color = "Public libraries"), size = .8) +
  scale_fill_manual(values = c("500 m buffer union" = "#73b8db"), name = "Coverage") +
  scale_color_manual(values = c("Public libraries" = "#08306b"), name = "Facilities") +
  labs(title = "Union of 500 m library buffers (not clipped to boundary)",
       caption = source_caption) +
  theme_bw()
print(p_sites); print(p_buffers)
ggsave("outputs/p_sites.png", p_sites, width = 7, height = 9, dpi = 200)
ggsave("outputs/p_buffers.png", p_buffers, width = 7, height = 9, dpi = 200)
# Interpretation (<=4 sentences): Overlapping buffer regions are counted once
# because the same square metre should not be counted more than once.
# Straight-line buffers ignore street routes, barriers, and operating hours, so
# they do not establish walking access. The union extends beyond Chicago's
# community-area boundary; these reported areas therefore include space outside it.

# 3. AREA MATCHING, COUNTS, AND DENSITY MAPS -----------------------------
match_list <- st_intersects(sites_m, areas_m)
site_match_audit <- data.frame(
  site_id = sites_m$site_id,
  facility_name = sites_m$facility_name,
  n_matches = lengths(match_list),
  district_id = vapply(match_list, function(idx) {
    if (length(idx) == 1) areas_m$district_id[idx] else NA_character_
  }, character(1))
)
write_csv(site_match_audit, "site_match_audit.csv")
cat("Zero area matches:", sum(site_match_audit$n_matches == 0), "\n")
cat("Exactly one match:", sum(site_match_audit$n_matches == 1), "\n")
cat("Multiple matches:", sum(site_match_audit$n_matches > 1), "\n")
print(site_match_audit %>% filter(n_matches != 1))
assigned_counts <- site_match_audit %>%
  filter(n_matches == 1) %>% count(district_id, name = "n_sites")
district_counts <- areas_m %>%
  left_join(assigned_counts, by = "district_id") %>%
  mutate(n_sites = coalesce(n_sites, 0L),
         area_km2 = as.numeric(st_area(areas_m)) / 1e6,
         sites_per_km2 = n_sites / area_km2)
stopifnot(all(district_counts$area_km2 > 0))
stopifnot(sum(district_counts$n_sites) == sum(site_match_audit$n_matches == 1))
write_csv(st_drop_geometry(district_counts) %>%
            select(district_id, district_name, n_sites, area_km2, sites_per_km2),
          "outputs/district_counts.csv")
st_write(district_counts, "district_counts.gpkg", delete_dsn = TRUE, quiet = TRUE)

n_unassigned <- sum(site_match_audit$n_matches != 1)
map_caption <- paste0("City of Chicago Data Portal | Accessed ", run_date,
                      " | Unassigned libraries: ", n_unassigned)
p_boundary_sites <- ggplot() +
  geom_sf(data = areas_m, fill = "grey97", color = "grey60", linewidth = .2) +
  geom_sf(data = sites_m, color = "#13628a", size = 1) +
  labs(title = "Chicago library locations over community areas", caption = map_caption) +
  theme_bw()
p_count <- ggplot(district_counts) +
  geom_sf(aes(fill = n_sites), color = "grey55", linewidth = .2) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", na.value = "#ff69b4",
                      name = "Libraries (count)") +
  coord_sf(expand = FALSE) +
  labs(title = "Libraries per Chicago community area", caption = map_caption) +
  theme_bw()
p_density <- ggplot(district_counts) +
  geom_sf(aes(fill = sites_per_km2), color = "grey55", linewidth = .2) +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", na.value = "#ff69b4",
                      name = "Libraries / km²") +
  coord_sf(expand = FALSE) +
  labs(title = "Library density per km² by community area", caption = map_caption) +
  theme_bw()
print(p_boundary_sites); print(p_count); print(p_density)
ggsave("outputs/p_boundary_sites.png", p_boundary_sites, width = 7, height = 9, dpi = 200)
ggsave("outputs/p_count.png", p_count, width = 7, height = 9, dpi = 200)
ggsave("outputs/p_density.png", p_density, width = 7, height = 9, dpi = 200)

report_table <- st_drop_geometry(district_counts) %>%
  select(district_name, n_sites, area_km2, sites_per_km2)
cat("Areas tied for highest library count:\n")
print(report_table %>% filter(n_sites == max(n_sites)) %>% select(district_name, n_sites))
cat("Areas tied for highest density:\n")
print(report_table %>% filter(sites_per_km2 == max(sites_per_km2)) %>%
        select(district_name, sites_per_km2))

# The highest-count areas are Portage Park, Humboldt Park,
# Austin, Near West Side, and New City, tied at 3 libraries each.
# Douglas has the highest density at 0.4683 libraries/km2.
# Zero-count areas have no uniquely assigned library sites.
# Libraries/km2 measures land coverage, not population or service quality.
# Community-area boundaries may include water or nonresidential space,
# affecting calculated densities; source dates may also differ.

# Interpretation (<=5 sentences): Fill names from the two printed tables above.
# The highest-count area(s) are ______; the highest-density area(s) are ______.
# Zero-count areas contain no uniquely assigned sites in these records.
# Libraries/km² uses land/boundary area, not number of residents or service quality.
# These municipal boundaries may include water and other non-residential space,
# affecting calculated densities; source dates and locations may differ.

cat("FINISHED. Check outputs/, site_match_audit.csv, district_counts.gpkg, data_sources.csv\n")
