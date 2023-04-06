#' @importFrom graphics plot lines points legend
#' @importFrom stats sd qnorm nlm lm fitted ppoints
#' @export
`rad.lines` <-
    function(x, sticks=10, main=NULL, legend.add=TRUE, ...)
{
    if (is.null(main))
        main <- rownames(x)
    x <- x[x>0]
    r <- seq(along=x)
    n <- length(r)
    x <- rev(sort(x))
    tot <- sum(x)
    plot(r, x, log="y", type="n", xlab="Rank", ylab="Abundance", main=main, ...)
    for (i in 1:sticks) {lines(r, tot*rbstick(n), col="green")}
    l.m <- mean(log(x))
    l.sd <- sd(log(x))
    l.norm <- qnorm(ppoints(n))
    l.fit <- exp(l.norm * l.sd + l.m)
    l.fit <- rev(l.fit)
    lines(r, l.fit, col="blue", lwd=2)
    ml <- nlm(logn.ml, p=c(l.m, l.sd), x=x)
    lines(r, rev(logn.fun(r, ml$est[1], ml$est[2])), col="black", lwd=2)
    x.glm <- lm(log(x) ~ r)
    lines(r, exp(fitted(x.glm)), col="red", lwd=2)
    points(r, x, pch="+")
    if (legend.add)
        legend(0.6*n, 0.95*max(x),
               c("Brokensticks","Pre-emption","Log-Norm moment","Log-Norm ML"),
               col=c("green","red","blue","black"), lwd=c(1,2,2,2), lty=1)
    invisible(list(rank=r, abund=x, logn.fit=l.fit, preemp.fit=exp(fitted(x.glm))))
}
