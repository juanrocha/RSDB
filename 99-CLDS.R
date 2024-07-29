library(tidyverse)
library(network)
library(ggiraph)
## read CLDs
clds <- read_csv2("~/Documents/Projects_old/Cascading Effects/files_figshare/RS_CLD_2018.csv") |> 
    janitor::clean_names()

clds |> arrange(regime_shift) |> pull(regime_shift) |> unique()

# clds <- clds |> 
#     group_by(tail, head, polarity, regime_shift) |> 
#     unique() |> 
#     ungroup()
clds <- clds |> 
    arrange(regime_shift) |> 
    mutate(regime_shift = str_to_sentence(regime_shift))


dat <- read_csv("assets/generic_types_RSDB_new.csv") |> 
    janitor::clean_names()

dat |> pull(regime_shift_name) |> sort()


## correct names to match the current database file
clds$regime_shift |> unique() %>% .[!clds$regime_shift |> unique() %in% dat$regime_shift_name ]
corrected_names <- c(
    "Dryland degradation", "Kelp transitions", "Marine food webs", "Primary production Arctic Ocean",
    "West Antarctic ice sheet collapse")

clds <- clds |> 
    mutate(regime_shift = as_factor(regime_shift)) 

levels(clds$regime_shift)[which(!levels(clds$regime_shift) %in% dat$regime_shift_name)] <- corrected_names

levels(clds$regime_shift) ## corrected!



save(clds, file = "assets/clds.Rda")

## create interactive CLDs
## updated function to add network attributes, from cascading paper
rs_net <- function (dat, i){
    # filter dataset
    dat <- filter(dat, regime_shift == levels(dat$regime_shift)[i],
                  polarity == 1 | polarity == -1)
    # build network
    rs.x <- network(
        select(dat, tail, head, polarity, delay), 
        directed = TRUE, ignore.eval=FALSE, matrix.type = 'edgelist')
    # add cycles to nodes and edges.
    fb.sum <- sna::kcycle.census(
        rs.x, maxlen=network.size(rs.x), mode='digraph',
        tabulate.by.vertex=T, cycle.comembership='sum')
    # number of cycles per nodes
    rs.x %v% 'fb' <-  diag(fb.sum$cycle.comemb)
    # number of cycles per edge
    rs.x %e% 'fb' <- as.sociomatrix(rs.x) * fb.sum$cycle.comemb
    #J161106: Note that cycle.comemb is a matrix with comemberships, so two nodes that are not connected in the network can belong to the same cycles. That's why I multiply by the adjacency matrix, to isolate only the edges. cycle.comemb can be used later for discovery of inconvenient feedbacks.
    ## add vertex attributes
    rs.x %v% 'driver' <- ifelse(colSums(fb.sum$cycle.count)[-1] == 0, TRUE, FALSE) # "#FFD92F"
    rs.x %n% 'name' <- levels(dat$regime_shift)[i]
    return(rs.x)
}

#test
rs_net(dat = clds, 5)

# library(sigmaNet)
# ## Option 1: signaNet and igraph
# net <- intergraph::asIgraph(rs_net(clds, 5)) |> 
#     sigmaFromIgraph() |> 
#     addNodeSize(sizeMetric = "betweenness", minSize = 3, maxSize = 8) |> 
#     addNodeLabels(labelAttr = 'vertex.names') |> 
#     addNodeColors(colorAttr = "col") |> 
#     addEdgeSize(oneSize = 2) |> 
#     addEdgeColors(colorAttr = "polarity", colorPal = "Set2") |> 
#     addEdgeArrows() |> 
#     addInteraction(neighborEvent = "onHover", doubleClickZoom = TRUE, mouseWheelZoom = TRUE)

## needs tweaking of colors and curved arrows. Right now simple feedbacks l=2 are drawn
## on the same edge with double arrow head.
# net

## Option 2: ggnetwork and ggirafe
## girafe does not work direclty with ggnetwork, so I'll need to build the network from 
## scratch, in pure ggplot
net <- rs_net(dat = clds, 5) #|> intergraph::asIgraph()
lyt <- sna::gplot.layout.fruchtermanreingold(net, NULL)
nodes <- tibble(
    node = network.vertex.names(net),
    driver = get.vertex.attribute(net, "driver"),
    fb = get.vertex.attribute(net, "fb"),
    x = lyt[,1],
    y = lyt[,2]
) |> rownames_to_column("id") |> 
    mutate(id = as.numeric(id)) |> 
    mutate(color = case_when(driver == TRUE ~ "#ff8100", .default ="#37207a" ))

edglst <- as.edgelist(net)
edges <- tibble(
    from =edglst[,1], to = edglst[,2], 
    polarity = get.edge.attribute(net, "polarity"),
    fb = get.edge.attribute(net, "fb"),
    delay = get.edge.attribute(net, "delay")
) 

edges <- edges |> 
    left_join(nodes |> select(id, xini = x, yini = y), by = c("from" = "id")) |> 
    left_join(nodes |> select(id, xend = x, yend = y), by = c("to" = "id"))
## create space for arrow gaps (following implementation from ggnetwork:::format_fortify)
edges <- edges |> 
    mutate(xl = xend - xini, yl = yend - yini) |> 
    mutate(gap = 0.1 /sqrt(xl^2+yl^2)) |> 
    mutate(xend = xini + (1-gap) * xl,
           yend = yini + (1-gap) * yl) |> 
    mutate(color = case_when(polarity == -1 ~ "#d6005b", .default = "#0084cd" ))

gg <- ggplot() +
    geom_curve(
        aes(x = xini, y = yini, xend = xend, yend = yend, linetype = as.factor(delay)),
        data = edges, curvature = 0.15, linewidth = 0.4, color = edges$color,
        arrow = arrow(type = "open", length = unit(2, "mm")),
        show.legend = FALSE) +
    geom_point_interactive(
        data = nodes,
        aes(x,y, tooltip = node, data_id = id),
        size = 3, color = nodes$color, show.legend = FALSE, hover_nearest = TRUE) +
    theme_void()

gg


girafe(ggobj = gg)


####geom_curve()#### leftovers ####
### From the cascading effects paper, scripts and data:
#load('~/Documents/projects_old/Cascading Effects/data/170525_ergm_data.RData')



## Updated ploting function
plotnet <- function(net, ...){
    plot.network(net, #mode='circle',
                 vertex.col = alpha(net %v% 'col', 0.7),
                 # label = network.vertex.names(net),
                 # label.cex= 1,
                 main = net %n% 'name', col.main = "gray44", cex.main = 1.2,
                 vertex.border = 0,
                 usecurve=T,
                 vertex.cex= 2, #2 + scale(net %v% 'fb'),
                 label.pos= 5,
                 edge.col= alpha(net %e% 'col' , 1),
                 edge.lwd =  rep(0.01, max(valid.eids(net))), #0.05 + net %e% 'fb',
                 edge.curve = 0.01,
                 displayisolates=T, pad = 0
    )
}

plotnet2 <- function(net, ...){
    plot.network(net, #mode='circle',
                 vertex.col = alpha(net %v% 'col', 0.7),
                 # label = network.vertex.names(net),
                 # label.cex= 1,
                 main = net %n% 'name', col.main = "gray44", cex.main = 1.2,
                 vertex.border = 0,
                 usecurve=T,
                 vertex.cex= 2 + scale(net %v% 'fb'),
                 label.pos= 5,
                 edge.col= alpha(net %e% 'col' , 1),
                 edge.lwd =  0.01 * net %e% 'fb',
                 edge.curve = 0.01,
                 displayisolates=T, pad = 0 # no need of pad for labels
    )
}
## A function to create the network, add polarity and cycles / feedbacks as edge attributes
rs.net <- function (dat, i){
    
    # filter dataset
    dat <- filter(dat, Regime.Shift == levels(dat$Regime.Shift)[i],
                  Polarity == 1 | Polarity == -1)
    # build network
    rs.x <- network(select(dat, Tail, Head, Polarity, col), directed = T, ignore.eval=FALSE, matrix.type = 'edgelist')
    # add polarity to edges: Outated, the way I create the network includes already the polarities by using ignore.eval = 
    # add cycles to nodes and edges.
    fb.sum <- kcycle.census(
        rs.x, maxlen=network.size(rs.x), mode='digraph',
        tabulate.by.vertex=T, cycle.comembership='sum')
    # number of cycles per nodes
    rs.x %v% 'fb' <-  diag(fb.sum$cycle.comemb)
    # number of cycles per edge
    rs.x %e% 'fb' <- as.sociomatrix(rs.x) * fb.sum$cycle.comemb
    #J161106: Note that cycle.comemb is a matrix with comemberships, so two nodes that are not connected in the network can belong to the same cycles. That's why I multiply by the adjacency matrix, to isolate only the edges. cycle.comemb can be used later for discovery of inconvenient feedbacks.
    ## add vertex attributes
    rs.x %v% 'col' <- ifelse(colSums(fb.sum$cycle.count)[-1] == 0, "#E41A1C","#8DA0CB") # "#FFD92F"
    rs.x %n% 'name' <- levels(dat$Regime.Shift)[i]
    return(rs.x)
}

#test
rs.net(dat, 5)

## Updated ploting function

# detach(package:igraph)
library(network); library(sna)
quartz(pointsize = 7)
par(mfrow = c(6,5), mar = c(2,2,2,2))

for (i in 1:length(levels(dat$Regime.Shift)) ){
    net <- rs.net(dat = dat,  i = i)
    plotnet(net)
    
}

quartz.save(file = "rsdb_30_cld.png", type = 'png', dpi = 500, bg = "white",
            width = 10, height = 10, pointsize = 10, family = "Arial")
