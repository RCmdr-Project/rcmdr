Command <- function(fun, ..., to){
  args <- list(...)
  arg.names <- names(args)
  used <- !(is.na(args) | args == "" | sapply(args, is.null))
  arg.names <- arg.names[used]
  args <- args[used]
  arg.names <- ifelse(arg.names == "", "", paste(arg.names, "= "))
  command <- paste0(fun, "(", paste(paste0(arg.names, args), collapse=", "), ")")
  if (!missing(to)) command <- paste(to, "<-", command)
  command
}

Q <- function(string) paste0('"', as.character(string), '"')
