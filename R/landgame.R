#' @importFrom stats rbinom runif
#' @export
`landgame` <-
    function(land, D, m, theta)
{
    tmp <- dim(land)
    J <- tmp[1]
    Nx <- tmp[2]
    Ny <- tmp[3]
    JM <- J*Nx*Ny
    nu <- theta/2/JM    # probability of speciation
    ## Shuffle: prepare to kill first D
    for (j in 1:Ny)
        for (i in 1:Nx)
            land[,i,j] <- land[sample(J), i, j]
    ## Recolonize first D slots
    for (j in 1:Ny) {
        jup <- if (j == Ny) 1 else j + 1
        jdown <- if (j == 1) Ny else j - 1
        for (i in 1:Nx) {
            ## Consider immigration
            imm <- rbinom(1, D, m)
            if (imm) {
                iup <- if (i == Nx) 1 else i + 1
                idown <- if (i == 1) Nx else i - 1
                pick <- sample(4 * (J - D), imm, replace = TRUE)
                itree <- (pick - 1) %% (J - D) + 1 + D
                for(jim in seq_len(imm))
                    land[jim, i, j] <-
                        switch((pick[jim] - 1) %/% (J - D) + 1,
                               land[itree[jim], i, jup],
                               land[itree[jim], iup, j],
                               land[itree[jim], i, jdown],
                               land[itree[jim], idown, j])
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
    }
    land
}
