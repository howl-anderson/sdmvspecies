#!/usr/bin/env Rscript

#' autoPCA
#' 
#' easily used PCA analysis
#' 
#' This method implemented an easily used PCA analysis method
#' 
#' @param env.stack a \code{rasterStack} object that contain the environment variable
#' @param nfactors Number of factors to extract, if it's 0 that method will auto descion the number. Default is 0
#' @return \code{rasterStack} object
#' @importFrom psych fa.parallel
#' @importFrom psych principal
#' @importFrom raster values
#' @importFrom raster values<-
#' @encoding utf-8
#' @export
autoPCA <- function(env.stack, nfactors=0) {
    env.data <- values(env.stack)

    if (!nfactors) {
        fa.result <- fa.parallel(env.data, n.iter=1000)
        nfactors <- fa.result$ncomp    
    }
    if (!nfactors) {
        stop("It seems other no principal components exits")
    }

    pc <- principal(env.data, nfactors=nfactors)
    weights <- pc$weights

    PC_list <- list()
    for (index in 1:nfactors) {
        env.name.list <- rownames(weights)
        component_list <- NULL
        for (env.name in env.name.list) {
            # env.name <- env.name.list[1]
            weights.data <- weights[env.name, index]
            env.value <- env.data[,env.name]
            component <- weights.data * env.value
            if (!is.null(component_list)) {
                component_list <- cbind(component_list, component)
            } else {
                component_list <- component
                component_list <- as.data.frame(component_list, byrow=TRUE, ncol=1)
            }
        }
        colnames(component_list) <- env.name.list
        raster.value <- apply(component_list, FUN=sum, MARGIN=1)
        raster.model <- env.stack[[1]]
        values(raster.model) <- raster.value
        PC_list[[index]] <- raster.model
    }
    name.vector <- paste("PC", 1:nfactors, sep="")
    names(PC_list) <- name.vector
    result.stack <- stack(PC_list)
    return(result.stack)
}
