
colors = c("black", "red", "green", "blue", "brown", "yellow")
ants_fitness_vs_time <- function(ants, processes) {
    xlist <- vector(mode="list", length=processes)
    ylist <- vector(mode="list", length=processes)
    cols <- c(1:processes)
    names <- c(1:processes)
    for (i in 1:processes) {
        id_proc <- i - 1
        ants_proc = ants[ants[[th_action]] == a_add_token &
                        ants[[th_rcv_process]] == id_proc &
                        ants[[th_token_value]] > -1 &
                        !is.na(ants[[th_token_value]]), ]
        xlist[[i]] <- ants_proc[[th_start_time]] / 1000000
        ylist[[i]] <- fitted(lm(token_value ~ start_time + I(start_time^2) + I(start_time^3) + I(start_time^4), data=ants_proc))
        cols[i] <- colors[i%%length(colors)]
        names[i] <- paste("process", id_proc, sep=" ")
    }

    plot(xlist[[1]], ylist[[1]], type="l", col=cols[1],
         main="Convergence of the fitness value",
         xlab="Time [ms]",
         ylab="Fitness value")
    if (processes > 1) {
        for (i in 2:processes) {
            lines(xlist[[i]], ylist[[i]], type="l", col=cols[i])
        }
    }
    legend("topright", title="Processes", names, inset=0.05, lty=1, col=cols)
}
