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
                for (jim in seq_len(imm)) {
                    ## select plant for yet unknown plot (first D dead)
                    iplant <- sample(J - D, 1) + D
                    ## select neighbour plot with torus wrap-around
                    land[jim, i, j] <-
                        switch(sample(4, 1),
                               land[iplant, i,  (j - 2) %% Ny + 1],
                               land[iplant, i %% Nx + 1, j],
                               land[iplant, i, j %% Ny + 1],
                               land[iplant, (i - 2) %% Nx + 1, j]
                           )
                }
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
