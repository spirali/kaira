# th - as table_header
th_action        <- "action"
th_prv_action    <- "prv_action"
th_start_time    <- "start_time"
th_snd_process   <- "snd_process"
th_snd_thread    <- "snd_thread"
th_rcv_process   <- "rcv_process"
th_rcv_thread    <- "rcv_thread"
th_id_transition <- "id_transition"
th_tr_value      <- "tr_value"
th_send_time     <- "send_time"
th_id_place      <- "id_place"
th_token_value   <- "token_value"

table_header <- c(th_action, th_prv_action, th_start_time,
    th_snd_process, th_snd_thread, th_rcv_process, th_rcv_thread,
    th_id_transition, th_tr_value, th_send_time, th_id_place, th_token_value)

# a - as action
a_transition_executed <- "transition_executed"
a_send_receive        <- "send_receive"
a_add_token           <- "add_token"
a_remove_token        <- "remove_token"
a_idle                <- "idle"

load_bigtable <- function(filename, sep=",") {
    bigt <- read.table(filename, header=TRUE, sep=sep, col.names=table_header)
    return(bigt)
}

remove_outliers_1 <- function(x, na.rm=TRUE, ...) {
    qnt <- quantile(x, probs=c(0.25, 0.75), na.rm=na.rm, ...)
    H <- 1.5 * IQR(x, na.rm=na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    return(y)
}

remove_outliers_2 <- function(bigtable, column, na.rm=TRUE, ...) {
    qnt <- quantile(bigtable[[column]], probs=c(0.25, 0.75), na.rm=na.rm, ...)
    H <- 1.5 * IQR(bigtable[[column]], na.rm=na.rm)

    new_bigt <- bigtable[bigtable[[column]] >= (qnt[1] - H) & bigtable[[column]] <= (qnt[2] + H),]
    return(new_bigt)
}

transition_executing_time <- function(
        bigtable, id_process, id_transition=NULL, rm_outliers=TRUE) {
    condition <- bigtable[[th_action]] == a_transition_executed &
                 bigtable[[th_rcv_process]] == id_process &
                 !is.na(bigtable[[th_send_time]])
    if(!is.null(id_transition)) {
        condition <- condition & bigtable[[th_id_transition]] == id_transition
    }

    entry <- bigtable[[th_send_time]][condition]
    if (rm_outliers) {
        entry <- remove_outliers_1(entry)
    }
    return(entry)
}

# tet - transition execution time
tet_projection_through_processes <- function(
        bigtable, nprocesses, id_transition=NULL, rm_outliers=TRUE) {

    p <- vector(mode="list", length=nprocesses)
    for (i in 1:nprocesses) {
        id_process <- i - 1
        p[[i]] <- transition_executing_time(
            bigtable, id_process, id_transition, rm_outliers)
    }
    return(p)
}

sum_processes <- function(bigtable, nprocesses, id_transition=NULL) {
    p <- c(1:nprocesses)

    condition <- bigtable[[th_action]] == a_transition_executed
    if (!is.null(id_transition)) {
        condition <- condition & bigtable[[th_id_transition]] == id_transition
    }

    for (i in 1:nprocesses) {
        proc_id <- i - 1
        # add id process to a general condition
        actual_cond <- condition & bigtable[[th_rcv_process]] == proc_id
        entry <- bigtable[[th_send_time]][actual_cond]
        p[i] <- sum(entry, na.rm=TRUE)
    }
    return(p)
}

time_vs_time_length <- function(bigtable, id_process, id_transition=NULL, rm_outliers=TRUE) {

    condition <- bigtable[[th_action]] == a_transition_executed &
                 bigtable[[th_rcv_process]] == id_process

    if (!is.null(id_transition)) {
        condition <- condition & bigtable[[th_id_transition]] == id_transition
    }

    times <- bigtable[[th_start_time]][condition]
    time_lengths <- bigtable[[th_send_time]][condition]

    if (rm_outliers) {
        times <- remove_outliers_1(times)
        time_lengths <- remove_outliers_1(time_lengths)
    }

    return(list(times=times, time_lengths=time_lengths))
}

#*******************************************************************************
# GRAPHS

boxplot_tet_through_processes <- function(
        bigtable, nprocesses, id_transition=NULL, rm_outliers=TRUE,
        xlim=NULL, ylim=NULL) {

    boxplot_data <- tet_projection_through_processes(
        bigtable, nprocesses, id_transition, rm_outliers)

    title <- "TET on processes"

    boxplot(boxplot_data,
            names=c(0:(nprocesses-1)),
            xlim=xlim, ylim=ylim,
            title=title)

}
