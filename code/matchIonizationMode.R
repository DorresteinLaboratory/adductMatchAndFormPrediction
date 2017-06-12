# See instructions on how to use in the notebook
matchIonizationMode <- function(tab1, tab2, adductdiff, mz.abs.tol, rt.abs.tol) {
    
    rtm <- apply(tab1[,1:2], 1, 
                 function(x) paste(tab2[which((tab2[,1]-x[1])> (adductdiff-mz.abs.tol) & (tab2[,1]-x[1]) < (adductdiff+mz.abs.tol)  
                                    & abs(tab2[,2]-x[2]) < rt.abs.tol  ), 2], 
                                   collapse=";"
                                  )
                )
    pmass <- apply(tab1[,1:2], 1, 
                 function(x) paste(tab2[which((tab2[,1]-x[1])> (adductdiff-mz.abs.tol) & (tab2[,1]-x[1]) < (adductdiff+mz.abs.tol)  
                                    & abs(tab2[,2]-x[2]) < rt.abs.tol  ), 1], 
                                   collapse=";"
                                  )
                )                 
    
    tab1x <- cbind(tab1[,1:2],  unlist(pmass), unlist(rtm) )
    colnames(tab1x)[3:4] <- c( "mass_on_pos_table", "rt_on_pos_table")
    tab1x                 
}
