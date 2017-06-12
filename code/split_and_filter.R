# Methods to filter formulas. This methods are limited and may not work for formulas with atoms other than CHNOPS
split.fm <- function(fm) {
        sfm <- strsplit(fm, "")[[1]]
        elel <- c()
        nun <- c()
        last <- "C"
        nxt <- ""
        for(i in 1:length(sfm)) {
                wis <- suppressWarnings( as.numeric(sfm[i]))
                if(i<length(sfm)) nxt <- suppressWarnings( as.numeric(sfm[i]))
                if(is.na(nxt)) nxt <- "C" else  nxt <- "N"
                if(is.na(wis)) {
                        elel <- c(elel, sfm[i])
                        if((last=="C" | i==length(sfm)) & i > 1) {
                                nun <- c(nun, 1)
                        }
                        last <- "C"
                } else {
                        if(last=="N") {
                                nun[length(nun)] <- paste0(nun[length(nun)], sfm[i])
                        } else {
                                nun <- c(nun, sfm[i])
                        }
                        last <- "N"
                }
        }
        # This rule fixes the two atoms at the end
        # dont know if this general
        if(length(elel)>length(nun)) nun <- c(nun, 1)
        return(list(elel, as.numeric(nun)))
}

# The rule scheme is simple, it stores TRUE if passes the rule FALSE if not
# At the end if the formula passes all rules the function retuns TRUE
# One can skipe a rule by commenting the if clause (three lines)
                    
filter.rules <- function(spfm) {
        rules <- c()
        # molecules should contain carbon
        if(!sum(spfm[[1]]=="C") ) {
                rules <-  c(rules, FALSE)
        }
        if(sum(spfm[[1]]=="C") & sum(spfm[[1]]=="O")) {
                rules <-  c(rules, 1 >= spfm[[2]][spfm[[1]]=="O"]/spfm[[2]][spfm[[1]]=="C"])
        }
        #if(sum(spfm[[1]]=="C") & sum(spfm[[1]]=="H")) {
        #        rules <-  c(rules, 2* spfm[[2]][spfm[[1]]=="C"] + 2 >= spfm[[2]][spfm[[1]]=="H"]/spfm[[2]][spfm[[1]]=="C"])
        #}
        if(sum(spfm[[1]]=="C") & sum(spfm[[1]]=="H")) {
                rules <-  c(rules, 4 >= spfm[[2]][spfm[[1]]=="H"]/spfm[[2]][spfm[[1]]=="C"])
        }
        if(sum(spfm[[1]]=="C")) {
                rules <-  c(rules,  spfm[[2]][spfm[[1]]=="C"] <= 100)
        }
        if(sum(spfm[[1]]=="H")) {
                rules <-  c(rules,  spfm[[2]][spfm[[1]]=="H"] <= 200)
        }
        if(sum(spfm[[1]]=="O")) {
                rules <-  c(rules,  spfm[[2]][spfm[[1]]=="O"] <= 80)
        }
        if(sum(spfm[[1]]=="N")) {
                rules <-  c(rules,  spfm[[2]][spfm[[1]]=="N"] <= 10)
        }
        if(sum(spfm[[1]]=="S")) {
                rules <-  c(rules,  spfm[[2]][spfm[[1]]=="S"] <= 2)
        }

        if(sum(rules)==length(rules)) {
                return(TRUE)
        } else {
                return(FALSE)
        }
}

                    
get.df <- function(fml) as.data.frame(matrix(fml[[2]], dimnames=list(1, fml[[1]]), nrow=1))
