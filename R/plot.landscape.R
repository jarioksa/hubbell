#' @importFrom graphics par plot mtext box points layout
#' @export
`plot.landscape` <-
    function(x, main, ...)
{
    land <- x
    op <- par(no.readonly = TRUE)
    layout(matrix(c(1, 1, 2, 1, 1, 2), 2, 3, byrow = TRUE))
    d <- dim(land)
    nx <- d[2]
    ny <- d[3]
    J <- d[1]
    plot(1:nx, 1:ny, type="n", axes=FALSE, xlab="", ylab="")
    if (!missing(main))
        mtext(main)
    box()
    ijit <- jitter(rep(0,J))
    jjit <- jitter(rep(0,J))*100
    for (j in 1:ny)
        for (i in 1:nx) {
            plt <- land[,i,j]
            plt <- plt[plt != "#FFFFFF"]
            nplt <- length(plt)
            if (nplt) {
                ijit <- 10*jitter(rep(0,nplt))
                jjit <- 10*jitter(rep(0,nplt))
                points(rep(i,nplt) + ijit, rep(j,nplt) + jjit,
                       col=plt, ...)
            }
        }
    rad <- rev(sort(table(as.vector(land))))
    plot(seq_along(rad), as.vector(rad), log="y", type="S", xlab="Rank",
         ylab="Abundance")
    points(seq_along(rad), rad, col=names(rad), pch=16)
    par(ylog=op$ylog)
    par(op)
    invisible()
}
