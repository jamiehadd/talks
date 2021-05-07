library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(maps)
library(ggtext)
library(gganimate)

theme_set(theme_void())

world_map <- map("state", fill = TRUE)

world <- sf::st_as_sf(world_map)

coords <- tibble(
	city = c("", "Staunton", "Philadelphia", "Oslo", "Boston", "Los Angeles"),
	years = c(0, 17, 4, 1, 7, 1),
	state = c("", "VA", "PA", "Norway", "MA", "CA"),
	lat  = c(0, 38.1496, 39.9526, 50, 42.3601, 34.0522),
	lon  = c(0, -79.0717, -75.1652, -65, -71.0589, -118.2437),
	label_lat = c(0, 37, 43.9526, 53, 40.7601, 36.0522),
	label_lon = c(0, -78.0717, -75.1652, -65, -59, -118.2437),
	description = c("",
									"Grew up!<br>Chess<br>Taekwondo",
									"College (philosophy + math)<br>New friends<br>General tomfoolery",
									"More study (philosophy)<br>New friends<br>Smoked fish",
									"Married!<br>Nonprofit<br>New friends<br>PhD",
									"Hi!<br>First 'real' job.<br>New friends")
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


anim <- ggplot(data = world) +
	geom_sf() +
	xlab("Longitude") + ylab("Latitude") +
	geom_curve(data = coord_cxns %>% slice(-1),
						 aes(y = lat,
						 		x = lon,
						 		yend = lat_next,
						 		xend = lon_next,
						 		group = seq_along(city_order)),
						 color = "#181818",
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
		alpha = 0.9
	) +
	xlim(-130, -60) +
	ylim(25, 54) +
	guides(color = FALSE, fill = FALSE) +
	scale_color_brewer(palette = "Accent") +
	scale_fill_brewer(palette = "Accent")

anim

# +
# 		ggtitle("Home {closest_state} of 6") +
# 		# create animation ----
# 	transition_states(
# 		city_order,
# 		transition_length = 2,
# 		state_length = 5
# 	) +
# 		# style title ----
# 	theme(
# 		plot.title = element_text(
# 			color = "#562457",
# 			size = 12
# 		)
# 	)
#
# animate(anim, nframes = 150, height = 2, width = 3, units = "in", res = 150, renderer=gifski_renderer("test.gif"))
