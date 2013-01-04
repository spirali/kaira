
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
        ylist[[i]] <- ants_proc[[th_token_value]]
#        ylist[[i]] <- fitted(lm(token_value ~ start_time + I(start_time^2) + I(start_time^3) + I(start_time^4) + I(start_time^5) + I(start_time^6), data=ants_proc))
        cols[i] <- colors[i%%length(colors)]
        names[i] <- paste("process", id_proc, sep=" ")
    }

    return(list(xs=xlist, ys=ylist, cols=cols, names=names))
#    plot(xlist[[1]], ylist[[1]], type="l", col=cols[1],
#         main="Convergence of the fitness value",
#         xlab="Time [ms]",
#         ylab="Fitness value")
#    if (processes > 1) {
#        for (i in 2:processes) {
#            lines(xlist[[i]], ylist[[i]], type="l", col=cols[i])
#        }
#    }
#    legend("topright", title="Processes", names, inset=0.05, lty=1, col=cols)
}

two_ants_tables <- function(ants1, ants2, processes) {

    alist1 <- ants_fitness_vs_time(ants1, processes)
    alist2 <- ants_fitness_vs_time(ants2, processes)

    opar <- par(no.readonly=TRUE)
    par(fig=c(0, 1, 0, 0.5))
    plot(alist1$xs[[1]], alist1$ys[[1]], type="l", col=alist1$cols[1], ylim=c(400,1400),
        xlab="Time [ms]", ylab="Fitness [trail cost]", main="Convergence of the fitness value (no communication)") 
    if (processes > 1) {
        for (i in 2:processes) {
            lines(alist1$xs[[i]], alist1$ys[[i]], type="l", col=alist1$cols[i])
        }
    }
    legend("topright", title="Processes", alist1$names, inset=0.05, lty=1, col=alist1$cols)

    par(fig=c(0, 1, 0.5, 1), new=TRUE)
    plot(alist2$xs[[1]], alist2$ys[[1]], type="l", col=alist2$cols[1], ylim=c(400, 1400),
        xlab="Time [ms]", ylab="Fitness [trail cost]", main="Convergence of the fitness value (with communication)") 
    if (processes > 1) {
        for (i in 2:processes) {
            lines(alist2$xs[[i]], alist2$ys[[i]], type="l", col=alist2$cols[i])
        }
    }
    legend("topright", title="Processes", alist2$names, inset=0.05, lty=1, col=alist2$cols)
}
