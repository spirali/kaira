
library(lattice)
mortary <- function(bigtable, transition_id, processs=NULL) {
    
    condition <- bigtable[[th_action]] == a_transition_executed &
                 bigtable[[th_id_transition]] == transition_id

    filtered_bt <- bigtable[condition,]
    executed_times <- filtered_bt[[th_send_time]]
    processes <- filtered_bt[[th_rcv_process]]
    values <- filtered_bt[[th_tr_value]]
    count <- length(values) 
    x <- c(1:count)
    y <- c(1:count)
    for (i in 1:count) {
        xy <- strsplit(strsplit(as.character(values[i]), ":")[[1]][2], ";")[[1]]
        x[i] <-  as.double(xy[1])
        y[i] <-  as.double(xy[2])
    }
    data <- data.frame(x=x, y=y, vals=executed_times, processes=processes)

#    plot(x, executed_times)
#    levelplot(vals~x*y | processes, data,
    levelplot(log10(vals)~x*y, data,
    cuts=30,
    pretty=TRUE,
    xlab="X Coordinate", ylab="Y Coordinate",
    main="Executing time of 'find intersection' trans. depends on [x, y] coordinates",
    col.regions=rainbow(50, start=0.3, end=1.0))
}
