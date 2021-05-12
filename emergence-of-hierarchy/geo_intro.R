library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(maps)
library(ggtext)
library(gganimate)


theme_set(theme_void())

world_map <- maps::map("state", fill = TRUE)

world <- sf::st_as_sf(world_map)

coords <- tibble(
	city = c("", "Staunton", "Philadelphia", "Oslo", "Boston", "Los Angeles"),
	years = c(0, 17, 4, 1, 7, 1),
	state = c("", "VA", "PA", "Norway", "MA", "CA"),
	lat  = c(0, 38.1496, 39.9526, 50, 42.3601, 34.0522),
	lon  = c(0, -79.0717, -75.1652, -65, -71.0589, -118.2437),
	label_lat = c(0, 37.3, 43.9526, 54, 40.7601, 37.5522),
	label_lon = c(0, -79.3717, -75.1652, -65, -59, -118.2437),
	description = c("",
									"Grew up!<br>Chess<br>Taekwondo",
									"College (philosophy + math)<br>Met wife<br>General tomfoolery",
									"More study (philosophy)<br>Smoked fish<br>Adventures",
									"Married!<br>Worked @ nonprofit<br>PhD",
									"Hi!<br>First 'real' job<br>New friends")
)

coords <- coords %>%
	mutate(label = glue::glue("**{city}, {state}** ({years} yrs)<br>*{description}*"))

coord_cxns <- coords %>%
	mutate(
		# need this to create transition state ----
		city_order = row_number() + 1,
		# where I moved to next, for curved arrows ----
		lat_next = lead(lat),
		lon_next = lead(lon),
		label_lat_next = lead(label_lat),
		label_lon_next = lead(label_lon),
		# label to show in plot, styled using ggtext ---
		label = glue::glue("**{city}, {state}** ({years} yrs)<br>*{description}*"),
		# label of next location ----
		label_next = lead(label)
	)

coord_cxns <- coord_cxns %>%
		# get first row of residence ----
	slice(1) %>%
		# manually modify for plotting ----
	mutate(
		city_order = 1,
		label_next = label,
		lat_next = lat,
		long_next = lon,
		label_lat_next = label_lat,
		label_long_next = label_lon,
	) %>%
		# combine with all other residences ----
	bind_rows(coord_cxns) %>%
		# last (7th) row irrelevant ----
	slice(1:6) %>%
		# keep what we neeed ----
	dplyr::select(city_order, lat, lon, label_lat, label_lon, lat_next, lon_next, label_lat_next, label_lon_next, label_next) %>%
	mutate(fact = row_number())


plot <- ggplot(data = world) +
	geom_sf(fill = "#b1ccbe") +
	xlab("Longitude") + ylab("Latitude") +
	geom_curve(data = coord_cxns %>% slice(-1),
						 aes(y = lat,
						 		x = lon,
						 		yend = lat_next,
						 		xend = lon_next,
						 		group = seq_along(city_order)),
						 color = "white",
						 curvature = -0.5,
						 # arrow = arrow(type = "closed", length = unit(0.02, "npc")),
						 size  = 0.5) +
	geom_point(data = coords,
						 aes(x = lon, y = lat, fill = city),
						 size = 4,
						 pch = 21) +
	# geom_segment(data = coords,
	# 						 aes(x = lon, xend = lon, y = lat, yend = lat + 0.5),
	# 						 color = "#181818",
	# 						 size = 1) +

	geom_richtext(
		data = coords,
		aes(
			x     = ifelse(label_lon < -100, label_lon+1, label_lon - 1),
			y     = label_lat + 1,
			label = label,
			vjust = "top",
			hjust = ifelse(label_lon < -100, 0, 1),
			# group is used to create the transition ----
			# group = seq_along(city_order),
			fill = city
		),
		size = 2,
		# R ladies purple ----
		# R ladies font used in xaringan theme ----
		family = "Lato",
		alpha = 1.0
	) +
	xlim(-125, -60) +
	ylim(25, 55) +
	guides(color = FALSE, fill = FALSE) +
	scale_color_brewer(palette = "Set3") +
	scale_fill_brewer(palette = "Set3") 

plot

ggsave("img-shared/geo-intro.png", width = 6, height = 4, bg = "transparent")
