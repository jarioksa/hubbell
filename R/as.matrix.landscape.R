### Changes 'landgame' result to a community data frame where each
### site is a row, and each distinct species is a column
#' @export
as.matrix.landscape <-
    function(x, ...)
{
    spec <- unique(x)
    nr <- prod(dim(x)[2:3])
    dim(x) <- c(nr, dim(x)[1])
    df <- matrix(0, nr, length(spec))
    colnames(df) <- sort(spec)
    for(i in 1:nrow(x)) {
        tbl <- table(x[i,])
        df[i, names(tbl)] <- tbl
    }
    df
}
