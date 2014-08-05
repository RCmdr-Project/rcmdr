# this function by Uwe Ligges; used with permission

ismdi <- function(){
    return(mdi = as.logical(.C("ismodemdi", as.integer(0), PACKAGE = "Rcmdr205")[[1]]))
}
