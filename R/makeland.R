#' @importFrom grDevices rgb
#' @export
`makeland` <-
    function(J, nrow, ncol=nrow)
{
    land <- array(rgb(1,1,1), dim=c(J, nrow,ncol))
    class(land) <- "landscape"
    land
}
