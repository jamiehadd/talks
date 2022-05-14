library(tidyverse)
library(tidygraph)
library(ggraph)

g <- tibble(
	source = c("b", "b", "e", "c", "b", "d", "c"),
	target = c("c", "b", "b", "d", "a", "c", "e")
) %>% 
	as_tbl_graph()

layout <- tibble(
	x = 0.5*c(1, 2, 2, 2, 0),
	y = c(1, 0, 1, 2, 1)
)

g %>%
	ggraph(layout = layout) + 
	geom_edge_link() + 
	geom_node_label(aes(label = name)) + 
	theme_void()

ggsave("img/basic-graph.png", width = 2, height = 2, dpi = 300)

	

