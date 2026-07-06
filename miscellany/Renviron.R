####
## On some Windows platforms, when the username contains non-ASCII characters, loading the Rcmdr package can fail.
##
## This script provides a workaround by creating an .Renviron file that defines the R_USER environment variable.
## Run the script in R, close R completely, and then restart R before trying to load the Rcmdr package again.
##
## https://github.com/RCmdr-Project/rcmdr/issues/8
##
####
## Test if R_USER environment variable is set
if (Sys.getenv('R_USER') != '') {
    message('INFO: R_USER environment variable is set to "', Sys.getenv('R_USER'), '". Skipping...', sep = '')
} else {
    message('INFO: R_USER environment variable is not set. Creating .Renviron file...')
    ## Test if USERPROFILE environment variable is set
    userdir <- Sys.getenv('USERPROFILE')
    if (userdir == '') {
        message('INFO: USERPROFILE environment variable is not set. May be your system is not windows. Skipping...')
        return
    }
    ## Preparing file name and text to write into
    userdocdir <- paste0(Sys.getenv('USERPROFILE'), '\\Documents')
    text <- paste0('R_USER="', userdocdir, '\"\n')
    file <- paste0(userdocdir, "\\.Renviron")
    ## Diverting output to file
    sink(file, append = TRUE)
    cat(text)
    sink()
    ## Finished
    message('INFO: "', file, '" created. Please, restart R to reload it.', sep = '')
}

