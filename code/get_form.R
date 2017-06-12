# Generate sum formulas with the function decomposeMass (use ?decomposeMass to see manual)
# The ppm and elements parameters are exposed
get_form <- function(mass,  ppm=5, elements = initializeElements(c("C","H", "N", "O", "S"))) {
        #essentialElements <- initializeCHNOPSMgKCaFe() 
        #d <- decomposeMass(mass, elements=essentialElements, ppm=20) 
        d <- decomposeMass(mass, ppm=ppm, elements=elements)
         if(is.null(d)) {
                d
        } else {
                matrix(cbind(d$formula[d$score>0 & d$valid=="Valid"], d$score[d$score>0 & d$valid=="Valid"]), ncol=2)
        }

}
