## Auxiliary functions
library(tidyverse)
library(network)
library(ggiraph)
## Modify function to work with names instead of index i
## create interactive CLDs
## updated function to add network attributes, from cascading paper
rs_net <- function (dat, rs){ 
    # dat is the dataset with cld edgelist
    # rs is the name of the regime shift, character
    # filter dataset
    dat <- filter(dat, regime_shift == rs,
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
    rs.x %v% 'driver' <- ifelse(colSums(fb.sum$cycle.count)[-1] == 0, TRUE, FALSE) 
    return(rs.x)
}

plot_net <- function(net){
    # create the layout
    lyt <- sna::gplot.layout.fruchtermanreingold(net, NULL)
    # data frame for nodes
    nodes <- tibble(
        node = network.vertex.names(net),
        driver = get.vertex.attribute(net, "driver"),
        fb = get.vertex.attribute(net, "fb"),
        x = lyt[,1],
        y = lyt[,2]) |>
        rownames_to_column("id") |> 
        mutate(id = as.numeric(id)) |> 
        mutate(color = case_when(driver == TRUE ~ "#ff8100", .default ="#37207a" ))
    # extract edge list and fortify data frame
    edglst <- as.edgelist(net)
    edges <- tibble(
        from =edglst[,1], to = edglst[,2], 
        polarity = get.edge.attribute(net, "polarity"),
        fb = get.edge.attribute(net, "fb"),
        delay = get.edge.attribute(net, "delay")
    ) 
    # create coordinates
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
    return(gg)
}