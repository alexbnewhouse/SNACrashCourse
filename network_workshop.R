library(igraph)
library(visNetwork)
el1 <- read.csv('https://tinyurl.com/sna-marriage')[2:3]
m <- graph_from_data_frame(el1, directed = FALSE)
plot(m)
degree(m)
sort(degree(m))

diameter(m)

sort(betweenness(m))
sort(evcent(m)$vector)


centr_degree(m)$centralization
centr_eigen(m)$centralization

E(m)$width <- edge_betweenness(m)

plot.igraph(m,
            edge.width = igraph::edge.betweenness(m)+1,
            edge.color = heat.colors(igraph::edge.betweenness(m)+1))  



options(viewer=NULL)

el2 <- read.csv("http://tinyurl.com/snacrashcourse")[2:3]
g <- graph_from_data_frame(el2)



plot(g)
centr_degree(g, mode='all')$centralization
transitivity(g)


g
mean(degree(g, mode = 'all'))

deg <- degree(g, mode = 'all')

in_deg <- degree(g, mode = 'in')
out_deg <- degree(g, mode = 'out')
View(out_deg)

ego_net <- make_ego_graph(g, nodes = 'erik_segelstrom')[[1]]
V(ego_net)$size = degree(ego_net) / 25

plot(ego_net, vertex.label = ifelse(V(ego_net)$name %in% 'erik_segelstrom', V(ego_net)$name, NA), 
     edge.width = .05, 
     edge.arrow.size = .05)


bet <- betweenness(g)

eig <- evcent(g, directed = TRUE)$vector

close_in <- closeness(g, mode = 'in')

close_out <- closeness(g, mode = 'out')


kcore <- coreness(g)
V(g)$core <- kcore
g_core <- induced_subgraph(g, kcore == 66)
V(g_core)$size <- igraph::betweenness(g_core)
plot(g_core, asp = 0, edge.arrow.size = .05, vertex.label.cex = .8, vertex.label.color = 'black', edge.width = .1, layout = layout_with_kk(g_core))






