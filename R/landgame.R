#' @importFrom stats rbinom
#' @export
`landgame` <-
    function(land, D, m, theta)
{
    tmp <- dim(land)
    J <- tmp[1]
    Nx <- tmp[2]
    Ny <- tmp[3]
    JM <- J*Nx*Ny
    nu <- theta/2/JM
    ## Shuffle: prepare to kill first D
    for (j in 1:Ny)
        for (i in 1:Nx)
            land[,i,j] <- land[sample(J), i, j]
    ## Recolonize first D slots
    for (j in 1:Ny)
        for (i in 1:Nx) {
            ## Consider immigration
            imm <- rbinom(1, D, m)
            if (imm) {
                pool <- NULL
                for (step in c(-1,1)) {
                    i.step <- i - step
                    if (i.step < 1) i.step <- Nx
                    if (i.step > Nx) i.step <- 1
                    pool <- c(pool, land[(D+1):J, i.step, j])
                    j.step <- j - step
                    if (j.step < 1) j.step <- Ny
                    if (j.step > Ny) j.step <- 1
                    pool <- c(pool, land[(D+1):J, i, j.step])
                }
                land[1:imm, i,j] <- sample(pool, imm, replace=TRUE)
            }
            ## Fill the rest from the local community
            if (imm < D)
                land[(imm+1):D, i,j] <- sample(land[(D+1):J, i,j], D-imm,
                                               replace=TRUE)
            ## Mutate
            evolved <- runif(D) <= nu
            if (any(evolved))
                for (sp in which(evolved))
                    land[sp, i, j] <- mutate(land[sp, i, j])
        }
    land
}
