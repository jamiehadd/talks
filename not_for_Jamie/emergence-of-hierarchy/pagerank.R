library(tidyverse)
library(tidygraph)
library(ggraph)

df <- read_csv("https://philchodrow.github.io/PIC16A/homework/HW3-hamilton-data.csv",
							 col_names = c("source", "target"))

df <- df %>% 
	group_by(source, target) %>% 
	summarise(n = n())

g <- as_tbl_graph(df)

g <- g %>% 
	mutate(centrality = centrality_pagerank(directed = T, weights = n), 
				 deg = centrality_degree(weights = n)) %>% 
	filter(deg > 2)


plot <- g %>% 
	ggraph(layout = "drl") + 
	geom_edge_arc(arrow = arrow(length = unit(0.8, "mm"), type = "closed"),
								color = "gray", alpha = 0.2) + 
	geom_node_label(aes(
		# size = centrality, 
											label = name, 
											fill = centrality,
											color = centrality > 0.05), size = 5) + 
	scale_fill_viridis(option = "inferno") + 
	scale_color_manual(values = c("white", "black")) + 
	guides(color = F, size = F, 
				 fill = guide_colorbar(title = "PageRank"))
	
plot

ggsave("emergence-of-hierarchy/img/hamilton.png", width = 7, height = 4, bg = "transparent")


