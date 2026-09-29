
# The R Commander and command logger

# last modified 2022-07-13 by John Fox

# contributions by Milan Bouchet-Valat, Richard Heiberger, Duncan Murdoch, Erich Neuwirth, Brian Ripley, Vilmantas Gegzna

#' @export
Commander <- function(){

    # set global options (to be restored on exit from Rcmdr GUI)
    
    putRcmdr("quotes", options(useFancyQuotes=FALSE))
    putRcmdr("max.print", options(max.print=2^30))
    putRcmdr("scipen", getOption("scipen"))
    
    manageRcmdrEnv()
    
    DESCRIPTION <- readLines(file.path(find.package("Rcmdr"), "DESCRIPTION")[1])
    setupRcmdrOptions(DESCRIPTION)
    
     ## setup language
    language.code <- getRcmdr("language.code")
    if (!is.null(language.code) && language.code != "") Sys.setLanguage(lang = language.code)

    createIcons()
    
    setupFonts()
    
    platformIssues()
    
    modelClasses <- scan(file.path(getRcmdr("etc"), "model-classes.txt"), what="", quiet=TRUE, comment.char="#") # default recognized models
    
    Plugins <- processPlugins(modelClasses)
    
    processModelCapabilities(Plugins)
    
    processOperations(Plugins)
    
    Menus <- processMenus(Plugins)

    setupGUI(Menus)
    
    openGraphicsDevices()
    
    # optionally open Markdown editor
    
    if (getRcmdr("open.markdown.editor") && getRcmdr("use.markdown")){
      editMarkdown()
    }
    
    # keep start-up warnings out of Rcmdr log
    messages.connection <- file(open="w+")
    sink(messages.connection, type="message")
    
    library(Rcmdr, quietly=TRUE)
    sink(type="message")
    close(messages.connection)

    ## Restore last active data set (if any)
    .activeDataSet <- getRcmdr('ActiveDataSet', fail = FALSE)
    if (!is.null(.activeDataSet) && .activeDataSet != "") activeDataSet(.activeDataSet)

    ## Restore last active model (if any)
    .activeModel <- getRcmdr('ActiveModel', fail = FALSE)
    if (!is.null(.activeModel) && .activeModel != FALSE) activeModel(.activeModel)

}

manageRcmdrEnv <- function(){
    RcmdrEnv.on.path <- getOption("Rcmdr")[["RcmdrEnv.on.path"]]
    if (is.null(RcmdrEnv.on.path)) RcmdrEnv.on.path <- FALSE
    if (RcmdrEnv.on.path){
        RcmdrEnv <- function() {
            pos <-  match("RcmdrEnv", search())
            if (is.na(pos)) { # Must create it
                RcmdrAttach <- base::attach
                RcmdrEnv <- list()
                RcmdrAttach(RcmdrEnv, pos = length(search()) - 1)
                rm(RcmdrEnv)
                pos <- match("RcmdrEnv", search())
            }
            return(pos.to.env(pos))
        }
        
        # the following two lines to be commented-out for debugging:
        assignInMyNamespace("RcmdrEnv", RcmdrEnv)
        assignInMyNamespace(".RcmdrEnv", NULL)
        
    }
}

setupRcmdrOptions <- function(DESCRIPTION){
    current <- getOption("Rcmdr")
    putRcmdr("messageNumber", 0)
    if (exists(".RcmdrEnv") && is.environment(RcmdrEnv()) &&
        exists("commanderWindow", RcmdrEnv()) &&
        !is.null(get("commanderWindow", RcmdrEnv()))) {
        return(invisible(NULL))
    }
    
    # check for auxiliary software
    putRcmdr("capabilities", RcmdrCapabilities())
    
    setOption("suppress.icon.images", FALSE)
    
    # locate Rcmdr etc directory and directory for menus (usually the same)
    etc <- setOption("etc", system.file("etc", package="Rcmdr"))
    etcMenus <- setOption("etcMenus", etc)
    putRcmdr("etcMenus", etcMenus)
    
    # various initializations
    messageTag(reset=TRUE)
    putRcmdr("installed.packages", installed.packages())
    RcmdrVersion <- trim.blanks(sub("^Version:", "",
                                    grep("^Version:", DESCRIPTION, value=TRUE)))
    putRcmdr("RcmdrVersion", RcmdrVersion)
    RVersion <- paste(R.Version()[c("major", "minor")], collapse=".")
    RVersionStatus <- R.Version()$status
    putRcmdr("RVersion", RVersion)
    putRcmdr("RVersionStatus", RVersionStatus)
    putRcmdr("UserName", getUserName())
    putRcmdr(".activeDataSet", NULL)
    putRcmdr(".activeModel", NULL)
    putRcmdr("nrow", NULL)
    putRcmdr("ncol", NULL)
    putRcmdr("logFileName", NULL)
    putRcmdr("RmdFileName", "RcmdrMarkdown.Rmd")
    putRcmdr("RnwFileName", "RcmdrKnitr.Rnw")
    putRcmdr("outputFileName", NULL)
    putRcmdr("saveFileName", NULL)
    putRcmdr("modelNumber", 0)
    putRcmdr("reset.model", FALSE)
    putRcmdr("rgl", FALSE)
    putRcmdr("rgl.command", FALSE)
    putRcmdr("Identify3d", NULL)
    putRcmdr("open.dialog.here", NULL)
    putRcmdr("restoreTab", FALSE)
    putRcmdr("cancelDialogReopen", FALSE)
    putRcmdr("last.search", "")
    
    putRcmdr("Markdown.editor.open", FALSE)
    putRcmdr("knitr.editor.open", FALSE)
    
    setOption("use.rgl", TRUE)
    
    # set various options
    options(scipen=setOption("scientific.notation", 5))
    setOption("default.contrasts", c("contr.Treatment", "contr.poly"))
    
    setOption("number.messages", TRUE)
    setOption("language.code", "")
    setOption("log.commands", TRUE)
    setOption("use.knitr", FALSE)
    setOption("use.markdown", !getRcmdr("use.knitr"))
    setOption("open.markdown.editor", FALSE)
    setOption("rmarkdown.output", TRUE)
    rmo.defaults <- list(
      command.sections = TRUE, section.level=3, toc=TRUE, toc_float=TRUE, toc_depth=3, 
      number_sections=FALSE, translate.rmd.headers=TRUE
    )
    rmo.options <- applyDefaultValues(getRcmdr("rmarkdown.output"), rmo.defaults)
    putRcmdr("command.sections", rmo.options$command.sections)
    putRcmdr("section.level", paste(rep("#", rmo.options$section.level), collapse=""))
    putRcmdr("translate.rmd.headers", rmo.options$translate.rmd.headers)
    if ((!packageAvailable("markdown") && !packageAvailable("rmarkdown")) || (!packageAvailable("knitr"))) 
        putRcmdr("use.markdown", FALSE)
    if (!packageAvailable("knitr") || !getRcmdr("capabilities")$pdflatex) putRcmdr("use.knitr", FALSE)
    setOption("rmd.output.format", "html")
    putRcmdr("startNewCommandBlock", TRUE)
    putRcmdr("startNewKnitrCommandBlock", TRUE)
    putRcmdr("rmd.generated", FALSE)
    putRcmdr("rnw.generated", FALSE)
    setOption("RStudio", RStudioP())
    setOption("console.output", getRcmdr("RStudio"))
    setOption("retain.selections", TRUE)
    
    setOption("open.graphics.devices", FALSE)
    
    putRcmdr("dialog.values", list())
    putRcmdr("dialog.values.noreset", list())
    putRcmdr("savedTable", NULL)
    log.height <- as.character(setOption("log.height", if (!getRcmdr("log.commands")) 0 else 10))
    log.width <- as.character(setOption("log.width", 80))
    output.height <- as.character(setOption("output.height",
                                            if (getRcmdr("console.output")) 0
                                            else if ((as.numeric(log.height) != 0) || (!getRcmdr("log.commands"))) 2*as.numeric(log.height)
                                            else 20))
    messages.height <- as.character(setOption("messages.height", 4))
    setOption("minimum.width", 1000)
    setOption("minimum.height", 400)
    putRcmdr("saveOptions", options(warn=1, contrasts=getRcmdr("default.contrasts"), width=as.numeric(log.width),
                                    na.action="na.exclude", graphics.record=TRUE))
    setOption("ask.to.exit", TRUE)
    setOption("ask.on.exit", TRUE)
    setOption("double.click", FALSE)
    setOption("sort.names", TRUE)
    setOption("grab.focus", TRUE)
    setOption("attach.data.set", FALSE)
    setOption("log.text.color", "black")
    setOption("command.text.color", "darkred")
    setOption("output.text.color", "darkblue")
    setOption("error.text.color", "red")
    setOption("warning.text.color", "darkgreen")
    setOption("prefixes", c("Rcmdr> ", "Rcmdr+ ", "RcmdrMsg: ", "RcmdrMsg+ "))
    setOption("multiple.select.mode", "extended")
    setOption("suppress.X11.warnings",
              interactive() && .Platform$GUI == "X11") # to address problem in X11 (Linux or Mac OS X)
    setOption("showData.threshold", c(20000, 100))
    setOption("editDataset.threshold", if (getRcmdr("capabilities")$tktable) 10000 else 0)
    setOption("retain.messages", TRUE)
    setOption("crisp.dialogs",  TRUE)
    setOption("length.output.stack", 10)
    setOption("length.command.stack", 10)
    setOption("quit.R.on.close", FALSE)
    setOption("start.rcmdr.with.R", FALSE)
    putRcmdr("outputStack", as.list(rep(NA, getRcmdr("length.output.stack"))))
    putRcmdr("commandStack", as.list(rep(NA, getRcmdr("length.command.stack"))))
    setOption("variable.list.height", 6)
    setOption("variable.list.width", c(20, Inf))
    setOption("valid.classes", c("factor", "ordered", "character", "logical",
                                 "POSIXct", "POSIXlt", "Date", "chron", "yearmon", "yearqtr", "zoo", 
                                 "zooreg", "timeDate", "xts", "its", "ti", "jul", "timeSeries", "fts",
                                 "Period", "hms", "difftime"))
    setOption("discreteness.threshold", 0)
    
    setOption("model.case.deletion", TRUE)
    
    putRcmdr("open.showData.windows", list())
}

createIcons <- function(){
    icon.images <- !getRcmdr("suppress.icon.images")
    tkimage.create("photo", "::image::RlogoIcon", file = system.file("etc", "R-logo.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::okIcon", 
                   file = if (icon.images) system.file("etc", "ok.gif", package="Rcmdr") else system.file("etc", "blank.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::cancelIcon", file = if (icon.images) system.file("etc", "cancel.gif", package="Rcmdr") 
                   else system.file("etc", "blank.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::helpIcon", file = if (icon.images) system.file("etc", "help.gif", package="Rcmdr")
                   else system.file("etc", "blank.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::resetIcon", file = if (icon.images) system.file("etc", "reset.gif", package="Rcmdr")
                   else system.file("etc", "blank.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::applyIcon", file = if (icon.images) system.file("etc", "apply.gif", package="Rcmdr")
                   else system.file("etc", "blank.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::submitIcon", file = system.file("etc", "submit.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::editIcon", file = system.file("etc", "edit.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::viewIcon", file = system.file("etc", "view.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::dataIcon", file = system.file("etc", "data.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::modelIcon", file = system.file("etc", "model.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::removeIcon", file = system.file("etc", "remove.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::copyIcon", file = system.file("etc", "copy.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::cutIcon", file = system.file("etc", "cut.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::deleteIcon", file = system.file("etc", "delete.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::findIcon", file = system.file("etc", "find.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::pasteIcon", file = system.file("etc", "paste.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::redoIcon", file = system.file("etc", "redo.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::undoIcon", file = system.file("etc", "undo.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::saveEditsIcon", file = system.file("etc", "save-edits.gif", package="Rcmdr"))
    
    tkimage.create("photo", "::image::infoIcon", file = system.file("etc", "info.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::warningIcon", file = system.file("etc", "warning.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::errorIcon", file = system.file("etc", "error.gif", package="Rcmdr"))
    tkimage.create("photo", "::image::questionIcon", file = system.file("etc", "question.gif", package="Rcmdr"))

}

setupFonts <- function(){
    current <- getOption("Rcmdr")
    # set up Rcmdr default and text (log) fonts, Tk scaling factor
    default.font.family.val <- tclvalue(.Tcl("font actual TkDefaultFont -family"))
    default.font.family.val <- gsub("\\{", "", gsub("\\}", "", default.font.family.val))
    default.font.family <- setOption("default.font.family", default.font.family.val)
    if (!("RcmdrDefaultFont" %in% as.character(.Tcl("font names")))){
        .Tcl(paste("font create RcmdrDefaultFont", tclvalue(tkfont.actual("TkDefaultFont"))))
        .Tcl("option add *font RcmdrDefaultFont")
    }
    
    .Tcl(paste("font configure RcmdrDefaultFont -family {", default.font.family, "}", sep=""))
    
    if (!("RcmdrTitleFont" %in% as.character(.Tcl("font names")))){
        .Tcl(paste("font create RcmdrTitleFont", tclvalue(tkfont.actual("TkDefaultFont"))))
    }
    .Tcl(paste("font configure RcmdrTitleFont -family {", default.font.family, "}", sep=""))
    if (!("RcmdrOutputMessagesFont" %in% as.character(.Tcl("font names")))){
        .Tcl(paste("font create RcmdrOutputMessagesFont", tclvalue(tkfont.actual("RcmdrTitleFont"))))
    }
    .Tcl(paste("font configure RcmdrTitleFont -family {", default.font.family, "}", sep=""))
    .Tcl(paste("font configure RcmdrOutputMessagesFont -family {", default.font.family, "}", sep=""))
    
    .Tcl(paste("font configure TkDefaultFont -family {",  default.font.family, "}", sep=""))
    log.font.family.val <- tclvalue(.Tcl("font actual TkFixedFont -family"))
    log.font.family.val <- gsub("\\{", "", gsub("\\}", "", log.font.family.val))
    log.font.family <- setOption("log.font.family", log.font.family.val)
    if (!("RcmdrLogFont" %in% as.character(.Tcl("font names")))){
        .Tcl(paste("font create RcmdrLogFont", tclvalue(tkfont.actual("TkFixedFont"))))
    }
    .Tcl(paste("font configure RcmdrLogFont -family {", log.font.family, "}", sep=""))
    .Tcl(paste("font configure TkFixedFont -family {",  log.font.family, "}", sep=""))
    putRcmdr("logFont", "RcmdrLogFont")    
    scale.factor <- current$scale.factor
    
    if (!is.null(scale.factor)) .Tcl(paste("tk scaling ", scale.factor, sep=""))
    # set various font sizes 
    if (WindowsP()){
        default.font.size.val <- abs(as.numeric(.Tcl("font actual TkDefaultFont -size")))
        if (is.na(default.font.size.val)) default.font.size.val <- 10
    }
    else default.font.size.val <- 10
    default.font.size <- setOption("default.font.size", default.font.size.val)
    tkfont.configure("RcmdrDefaultFont", size=default.font.size)
    tkfont.configure("RcmdrTitleFont", size=default.font.size)
    tkfont.configure("RcmdrOutputMessagesFont", size=default.font.size)
    tkfont.configure("TkDefaultFont", size=default.font.size)
    tkfont.configure("TkTextFont", size=default.font.size)
    tkfont.configure("TkCaptionFont", size=default.font.size)
    log.font.size <- setOption("log.font.size", 10)
    tkfont.configure("RcmdrLogFont", size=log.font.size)
    tkfont.configure("TkFixedFont", size=log.font.size)    
    
    .Tcl("ttk::style configure TButton -font RcmdrDefaultFont")
    .Tcl("ttk::style configure TLabel -font RcmdrDefaultFont")
    .Tcl("ttk::style configure TCheckbutton -font RcmdrDefaultFont")
    .Tcl("ttk::style configure TRadiobutton -font RcmdrDefaultFont")
    
    standard.title.color <- as.character(.Tcl("ttk::style lookup TLabelframe.Label -foreground"))
    title.color <- setOption("title.color", standard.title.color) 
    if (tolower(title.color) == "black" || title.color == "#000000"){
        tkfont.configure("RcmdrTitleFont", weight="bold")
    }
    else tkfont.configure("RcmdrTitleFont", weight="normal")
}

platformIssues <- function(){
    current <- getOption("Rcmdr")
    if (getRcmdr("suppress.X11.warnings")) {
        putRcmdr("messages.connection", file(open = "w+"))
        sink(getRcmdr("messages.connection"), type="message")
    }
    if (!(WindowsP())) {
        putRcmdr("oldPager", options(pager=RcmdrPager))
    }
    putRcmdr("restore.help_type", getOption("help_type"))
    if ((!WindowsP()) && getRcmdr("RVersion") == "4.2.0" && (getRcmdr("RVersionStatus") != "Patched")) {
      setOption("help_type", "text")
      } else {
        setOption("help_type", "html")
      }
    options(help_type=getRcmdr("help_type"))
    putRcmdr("restore.device", getOption("device"))
    if (RStudioP()){
        if (WindowsP()) options(device="windows")
        else if (MacOSXP()) options(device="quartz")
        else options(device="x11")
    }
    setOption("tkwait.dialog", FALSE)
    if (getRcmdr("tkwait.dialog")) putRcmdr("editDataset.threshold", 0)
    if (MacOSXP()){
        #       PATH <- system2("/usr/libexec/path_helper", "-s", stdout=TRUE)
        #       PATH <- sub("\"; export PATH;$", "", sub("^PATH=\\\"", "", PATH))
        #       Sys.setenv(PATH=PATH)
        PATH <- Sys.getenv("PATH")
        PATH <- unlist(strsplit(PATH, .Platform$path.sep, fixed=TRUE))
        if (MacOSXP("15.0.0")){
            if (length(grep("^/Library/TeX/texbin$", PATH)) == 0) {
                PATH[length(PATH) + 1] <- "/Library/TeX/texbin"
                Sys.setenv(PATH=paste(PATH, collapse=.Platform$path.sep))
            }
        }
        else{
            if (length(grep("^/usr/texbin$", PATH)) == 0) {
                PATH[length(PATH) + 1] <- "/usr/texbin"
                Sys.setenv(PATH=paste(PATH, collapse=.Platform$path.sep))
            }
        }
    }
}

processPlugins <- function(modelClasses){
    # source additional .R files, plug-ins preferred
    etc <- getRcmdr("etc")
    source.files <- list.files(etc, pattern="\\.[Rr]$")
    for (file in source.files) {
        source(file.path(etc, file))
        cat(paste(gettextRcmdr("Sourced:"), file, "\n"))
    }
    
    # collect plug-ins to be used
    Plugins <- options()$Rcmdr$plugins
    allPlugins <- listPlugins(loaded=TRUE)
    for (plugin in Plugins){
        if (!require(plugin, character.only=TRUE)){
            putRcmdr("commanderWindow", NULL)
            stop(sprintf(gettextRcmdr("the plug-in package %s is missing"), plugin))
        }
        if (!is.element(plugin, allPlugins)){
            putRcmdr("commanderWindow", NULL)
            stop(sprintf(gettextRcmdr("the package %s is not an Rcmdr plug-in"), plugin))
        }
    }
    for (plugin in Plugins){
        description <- readLines(file.path(path.package(package=plugin)[1], "DESCRIPTION"))
        addModels <- description[grep("Models:", description)]
        addModels <- gsub(" ", "", sub("^Models:", "", addModels))
        addModels <- unlist(strsplit(addModels, ","))
        addRcmdrModels <- description[grep("RcmdrModels:", description)]
        addRcmdrModels <- gsub(" ", "", sub("^RcmdrModels:", "", addRcmdrModels))
        addRcmdrModels <- unlist(strsplit(addRcmdrModels, ","))
        if (length(addModels) > 0) modelClasses <- c(modelClasses, addModels)
        if (length(addRcmdrModels) > 0) modelClasses <- c(modelClasses, addRcmdrModels)
        }
    putRcmdr("modelClasses", modelClasses)
    Plugins
}


processModelCapabilities <- function(Plugins){
    modelCapabilities <- read.table(file.path(getRcmdr("etc"), "Rcmdr-model-capabilities.txt"), header=TRUE)
    n.plugins <- length(Plugins)
    if (n.plugins > 0){
        modelCapabilitiesList <- vector(n.plugins + 1, mode="list")
        modelCapabilitiesList[[1]] <- modelCapabilities
        for (i in 1:n.plugins){
          if (file.exists(file.path(path.package(package=Plugins[i])[1], "etc/model-capabilities.txt")))
            modelCapabilitiesList[[i + 1]] <- read.table(file.path(path.package(package=Plugins[i])[1], 
                                                                   "etc/model-capabilities.txt"),
                                                         header=TRUE)
        }
        modelCapabilities <- mergeCapabilities(modelCapabilitiesList)
    }
    putRcmdr("modelCapabilities", modelCapabilities)
    modelClasses <- getRcmdr("modelClasses")
    modelCapabilitiesClasses <- rownames(modelCapabilities)
    modelClasses <- union(modelClasses, modelCapabilitiesClasses)
    putRcmdr("modelClasses", modelClasses)
}

processOperations <- function(Plugins){
  Operations <- read.table(file.path(getRcmdr("etc"), "Rcmdr-operations.txt"),
                           header=TRUE, stringsAsFactors=FALSE)
  for (plugin in Plugins){
    operations.file <- file.path(path.package(package=plugin)[1], "etc", "operations.txt")
    if (file.exists(operations.file)){
      operations <- read.table(operations.file, header=TRUE, stringsAsFactors=FALSE)
      if (any(conflicts <- rownames(operations) %in% rownames(Operations))){
        message(sprintf("The following Markdown section titles in %s\n  conflict with existing titles and were removed:\n  ",
                        plugin), paste(rownames(operations)[conflicts], collapse=", "))
        operations <- operations[!conflicts, ]
      }
      Operations <- rbind(Operations, operations)
    }
  }
  putRcmdr("Operations", Operations)
}


processMenus <- function(Plugins){
    current <- getOption("Rcmdr")
    # build Rcmdr menus
    Menus <- read.table(file.path(getRcmdr("etcMenus"), "Rcmdr-menus.txt"), colClasses = "character")
    addMenus <- function(Menus){
        removeMenus <- function(what){
            children <- Menus[Menus[,3] == what, 2]
            which <- what == Menus[,2] |  what == Menus[,5]
            Menus <<- Menus[!which,]
            for (child in children) removeMenus(child)
        }
        nms <- c("type", "menuOrItem", "operationOrParent", "label",
                 "commandOrMenu", "activation", "install")
        names(Menus) <- nms
        for (plugin in Plugins) {
            MenusToAdd <- read.table(file.path(path.package(package=plugin)[1], "etc/menus.txt"),
                                     colClasses = "character")
            names(MenusToAdd) <- nms
            for (i in 1:nrow(MenusToAdd)){
                line <- MenusToAdd[i,]
                line[, "label"] <- gettext(line[,"label"], domain=paste("R=", plugin, sep=""))
                if (line[1, "type"] == "remove"){
                    removeMenus(line[1, "menuOrItem"])
                    next
                }
                if (line[1, "type"] == "menu"){
                    where <- if (line[1, "operationOrParent"] == "topMenu") 0
                    else max(which((Menus[, "type"] == "menu") &
                                       (Menus[, "menuOrItem"] == line[1, "operationOrParent"])))
                }
                else if (line[1, "type"] == "item"){
                    if ((line[1, "operationOrParent"] == "command") || (line[1, "operationOrParent"] == "separator")){
                        which <- which(((Menus[, "operationOrParent"] == "command") | 
                                            (Menus[, "operationOrParent"] == "separator")) &
                                           (Menus[, "menuOrItem"] == line[1, "menuOrItem"]))
                        where <- if (length(which) == 0)
                            which((Menus[, "type"] == "menu")
                                  & (Menus[, "menuOrItem"] == line[1, "menuOrItem"]))
                        else max(which)
                        if (line[1, "operationOrParent"] == "separator" && line[1, "commandOrMenu"] != "") 
                            where <- max(which(line[1, "commandOrMenu"] == Menus[, "commandOrMenu"]))
                    }
                    else if (line[1, "operationOrParent"] == "cascade"){
                        where <- if (line[1, "menuOrItem"] != "topMenu")
                            max(which((Menus[, "operationOrParent"] == "cascade") &
                                          (Menus[, "menuOrItem"] == line[1, "menuOrItem"]) | (Menus[, "commandOrMenu"] == line[1, "menuOrItem"])))
                        else {
                            max(which((Menus[, "operationOrParent"] == "cascade") &
                                          (Menus[, "menuOrItem"] == "topMenu") &
                                          (Menus[, "commandOrMenu"] != "toolsMenu") &
                                          (Menus[, "commandOrMenu"] != "helpMenu")))
                        }
                    }
                    else stop(sprintf(gettextRcmdr('unrecognized operation, "%s", in plugin menu line %i'),
                                      line[1, "operation"], i))
                }
                else stop(sprintf(gettextRcmdr('unrecognized type, "%s", in plugin menu line %i'),
                                  line[1, "type"], i))
                Menus <- insertRows(Menus, line, where)
            }
        }
        Menus
    }
    Menus <- addMenus(Menus)
    menuNames <- Menus[Menus[,1] == "menu",]
    duplicateMenus <- duplicated(menuNames)
    if (any(duplicateMenus)) stop(paste(gettextRcmdr("Duplicate menu names:"),
                                        menuNames[duplicateMenus]))
    setOption("suppress.menus", FALSE)
    if (RExcelSupported()) # contributed by Erich Neuwirth
        putRExcel(".rexcel.menu.dataframe", Menus)
    Menus
}

setupGUI <- function(Menus){
    current <- getOption("Rcmdr")
    # standard edit actions
    onCopy <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && 
            (tclvalue(focused) != MessagesWindow()$ID) && (tclvalue(focused) != RmdWindow()$ID) && (tclvalue(focused) != RnwWindow()$ID))
            focused <- LogWindow()
        selection <- strsplit(tclvalue(tktag.ranges(focused, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        text <- tclvalue(tkget(focused, selection[1], selection[2]))
        tkclipboard.clear()
        tkclipboard.append(text)
    }
    onDelete <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && 
            (tclvalue(focused) != MessagesWindow()$ID) && (tclvalue(focused) != RmdWindow()$ID) && (tclvalue(focused) != RnwWindow()$ID))
            focused <- LogWindow()
        selection <- strsplit(tclvalue(tktag.ranges(focused, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        tkdelete(focused, selection[1], selection[2])
    }
    onCut <- function(){
        onCopy()
        onDelete()
    }
    onPaste <- function(){
        onDelete()
        focused <- tkfocus()
        if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID)  && 
            (tclvalue(focused) != MessagesWindow()$ID) && (tclvalue(focused) != RmdWindow()$ID) && (tclvalue(focused) != RnwWindow()$ID))
            focused <- LogWindow()
        text <- tclvalue(.Tcl("selection get -selection CLIPBOARD"))
        if (length(text) == 0) return()
        tkinsert(focused, "insert", text)
    }
    onFind <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID)  && 
            (tclvalue(focused) != MessagesWindow()$ID) && (tclvalue(focused) != RmdWindow()$ID) && (tclvalue(focused) != RnwWindow()$ID))
            focused <- LogWindow()
        initializeDialog(title=gettextRcmdr("Find"))
        textFrame <- tkframe(top)
        textVar <- tclVar(getRcmdr("last.search"))
        textEntry <- ttkentry(textFrame, width="20", textvariable=textVar)
        checkBoxes(frame="optionsFrame", boxes=c("regexpr", "case"), initialValues=c("0", "1"),
                   labels=gettextRcmdr(c("Regular-expression search", "Case sensitive")))
        radioButtons(name="direction", buttons=c("foward", "backward"), labels=gettextRcmdr(c("Forward", "Backward")),
                     values=c("-forward", "-backward"), title=gettextRcmdr("Search Direction"))
        onOK <- function(){
            text <- tclvalue(textVar)
            putRcmdr("last.search", text)
            if (text == ""){
                errorCondition(recall=onFind, message=gettextRcmdr("No search text specified."))
                return()
            }
            type <- if (tclvalue(regexprVariable) == 1) "-regexp" else "-exact"
            case <- tclvalue(caseVariable) == 1
            direction <- tclvalue(directionVariable)
            stop <- if (direction == "-forward") "end" else "1.0"
            where.txt <- if (case) tksearch(focused, type, direction, "--", text, "insert", stop)
            else tksearch(focused, type, direction, "-nocase", "--", text, "insert", stop)
            where.txt <- tclvalue(where.txt)
            if (where.txt == "") {
                Message(message=gettextRcmdr("Text not found."),
                        type="note")
                if (GrabFocus()) tkgrab.release(top)
                tkdestroy(top)
                tkfocus(CommanderWindow())
                return()
            }
            if (GrabFocus()) tkgrab.release(top)
            tkfocus(focused)
            tkmark.set(focused, "insert", where.txt)
            tksee(focused, where.txt)
            tkdestroy(top)
        }
        .exit <- function(){
            text <- tclvalue(textVar)
            putRcmdr("last.search", text)
            return("")
        }
        OKCancelHelp()
        tkgrid(labelRcmdr(textFrame, text=gettextRcmdr("Search for:")), textEntry, sticky="w")
        tkgrid(textFrame, sticky="w")
        tkgrid(optionsFrame, sticky="w")
        tkgrid(directionFrame, sticky="w")
        tkgrid(buttonsFrame, sticky="w")
        dialogSuffix(focus=textEntry)
    }
    onSelectAll <- function() {
        focused <- tkfocus()
        if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) 
            && (tclvalue(focused) != MessagesWindow()$ID) && (tclvalue(focused) != RmdWindow()$ID) && (tclvalue(focused) != RnwWindow()$ID))
            focused <- LogWindow()
        tktag.add(focused, "sel", "1.0", "end")
        tkfocus(focused)
    }
    onClear <- function(){
        onSelectAll()
        onDelete()
    }
    onUndo <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && 
            (tclvalue(focused) != MessagesWindow()$ID) && (tclvalue(focused) != RmdWindow()$ID) && (tclvalue(focused) != RnwWindow()$ID))
            focused <- LogWindow()
        tcl(focused, "edit", "undo")
    }
    onRedo <- function(){
        focused <- tkfocus()
        if ((tclvalue(focused) != LogWindow()$ID) && (tclvalue(focused) != OutputWindow()$ID) && 
            (tclvalue(focused) != MessagesWindow()$ID) && (tclvalue(focused) != RmdWindow()$ID) && (tclvalue(focused) != RnwWindow()$ID))
            focused <- LogWindow()
        tcl(focused, "edit", "redo")
    }
    
    .Tcl("ttk::style configure TNotebook.Tab -font RcmdrDefaultFont")
    .Tcl(paste("ttk::style configure TNotebook.Tab -foreground", getRcmdr("title.color")))
    
    all.themes <- tk2theme.list()
    current.theme <- tk2theme()
    all.themes <- union(all.themes, current.theme)
    setOption("theme", current.theme)
    theme <- (getRcmdr("theme"))
    if (!(theme %in% all.themes)){
        warning(gettextRcmdr("non-existent theme"), ', "', theme,  '"\n  ', 
                gettextRcmdr("theme set to"), ' "', current.theme, '"')
        theme <- current.theme
    }
    putRcmdr("theme", theme)
    tk2theme(theme)
    # data-set edit
    onEdit <- function(){
        if (activeDataSet() == FALSE) {
            tkfocus(CommanderWindow())
            return()
        }
        dsnameValue <- ActiveDataSet()
        size <- eval(parse(text=paste("prod(dim(", dsnameValue, "))", sep=""))) #  prod(dim(save.dataset))
        if (size < 1 || size > getRcmdr("editDataset.threshold")){
            save.dataset <- get(dsnameValue, envir=.GlobalEnv)
            command <- paste("fix(", dsnameValue, ")", sep="")
            result <- justDoIt(command)
            if (class(result)[1] !=  "try-error"){ 			
                if (nrow(get(dsnameValue)) == 0){
                    errorCondition(window=NULL, message=gettextRcmdr("empty data set."))
                    justDoIt(paste(dsnameValue, "<- save.dataset"))
                    return()
                }
                else{
                    logger(command, rmd=FALSE)
                    activeDataSet(dsnameValue)
                }
            }
            else{
                errorCondition(window=NULL, message=gettextRcmdr("data set edit error."))
                return()
            }
        }
        else {
            command <- paste("editDataset(", dsnameValue, ")", sep="")
            result <- justDoIt(command)
            if (class(result)[1] !=  "try-error"){
                logger(command, rmd=FALSE)
            }
            else{
                errorCondition(window=NULL, message=gettextRcmdr("data set edit error."))
                return()
            }
        }
        tkwm.deiconify(CommanderWindow())
        tkfocus(CommanderWindow())
    }
    
    # data-set view
    onView <- function(){
        #        if (packageAvailable("relimp")) Library("relimp", rmd=FALSE)
        if (activeDataSet() == FALSE) {
            tkfocus(CommanderWindow())
            return()
        }
        suppress <- if(getRcmdr("suppress.X11.warnings")) ", suppress.X11.warnings=FALSE" else ""
        view.height <- max(getRcmdr("output.height") + getRcmdr("log.height"), 10)
        dim <- dim(get(ActiveDataSet()))
        nrows <- dim[1]
        ncols <- dim[2]
        threshold <- getRcmdr("showData.threshold")
        command <- if (nrows <= threshold[1] && ncols <= threshold[2]){
            posn <- commanderPosition() + c(as.numeric(tkwinfo("width", CommanderWindow())) + 10, 10)
            paste("showData(as.data.frame(", ActiveDataSet(), "), title='", ActiveDataSet(), "', placement='+", posn[1], "+", posn[2],"', font=getRcmdr('logFont'), maxwidth=",
                  getRcmdr("log.width"), ", maxheight=", view.height, suppress, ")", sep="")
        }
        else paste("View(as.data.frame(", ActiveDataSet(), "))", sep="")
        window <- justDoIt(command)
        if (!is.null(window)){
            open.showData.windows <- getRcmdr("open.showData.windows")
            open.window <- open.showData.windows[[ActiveDataSet()]]
            if (!is.null(open.window) && open.window$ID %in% as.character(tkwinfo("children", "."))) tkdestroy(open.window)
            open.showData.windows[[ActiveDataSet()]] <- window
            putRcmdr("open.showData.windows", open.showData.windows)
        }
    }
    
    # submit command in script tab or compile .Rmd file in markdown tab or compile .Rnw file in knitr tab
    onSubmit <- function(){
        .log <- LogWindow()
        .rmd <- RmdWindow()
        .rnw <- RnwWindow()
        if (as.character(tkselect(notebook)) == logFrame$ID) {
            selection <- strsplit(tclvalue(tktag.ranges(.log, "sel")), " ")[[1]]
            if (is.na(selection[1])) {
                tktag.add(.log, "currentLine", "insert linestart", "insert lineend")
                selection <- strsplit(tclvalue(tktag.ranges(.log,"currentLine")), " ")[[1]]
                tktag.delete(.log, "currentLine")
                if (is.na(selection[1])) {
                    Message(message=gettextRcmdr("Nothing is selected."),
                            type="error")
                    tkfocus(CommanderWindow())
                    return()
                }
            }
            lines <- tclvalue(tkget(.log, selection[1], selection[2]))
            lines <- strsplit(lines, "\n")[[1]]
            .console.output <- getRcmdr("console.output")
            .output <- OutputWindow()
            iline <- 1
            nlines <- length(lines)
            while (iline <= nlines){
                while (nchar(lines[iline])==0) iline <- iline + 1
                if (iline > nlines) break
                current.line <- lines[iline]
                if (.console.output) cat(paste("\n", getRcmdr("prefixes")[1], current.line,"\n", sep=""))
                else{
                    tkinsert(.output, "end", paste("\n> ", current.line,"\n", sep="")) 
                    tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
                    tktag.configure(.output, "currentLine", foreground=getRcmdr("command.text.color"))
                }
                jline <- iline + 1
                while (jline <= nlines){
                    if (!inherits(try(parse(text=current.line),silent=TRUE), "try-error")) break
                    if (.console.output)cat(paste(getRcmdr("prefixes")[2], lines[jline],"\n", sep=""))
                    else{
                        tkinsert(.output, "end", paste("+ ", lines[jline],"\n", sep=""))
                        tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
                        tktag.configure(.output, "currentLine", foreground=getRcmdr("command.text.color"))
                    }
                    current.line <- if (nchar(lines[jline]) > 0) paste(current.line, lines[jline],sep="\n")
                    jline <- jline + 1
                    iline <- iline + 1
                }
                
                # protect against misprocessed comments
                    xlines <- strsplit(current.line, "\n")[[1]]
                    xlines <- trimws(sub("#.*$", "", xlines))
                    xlines <- xlines[nchar(xlines) > 0]
                    current.line <- paste(xlines, collapse="\n")
                    if (length(current.line) == 0 || nchar(current.line) == 0) current.line <- NULL
                
                if (!(is.null(current.line) || is.na(current.line))) {
                    doItAndPrint(current.line, log=FALSE, rmd=TRUE)
                }
                iline <- iline + 1
                tkyview.moveto(.output, 1)
                tkfocus(.log)
            }
            if (length(as.character(tksearch(.log, "-regexp", "-forward",  "--", "\\n\\n$", "1.0"))
            ) == 0){
                tkinsert(.log, "end", "\n")
            }
            cursor.line.posn <- 1 + floor(as.numeric(tkindex(.log, "insert")))
            tkmark.set(.log, "insert", paste(cursor.line.posn, ".0", sep=""))
            tktag.remove(.log, "sel", "1.0", "end")
        }
        else if (as.character(tkselect(notebook)) == RmdFrame$ID) {
            compileRmd()
        }
        else{ 
            compileRnw()
        }
    }
    
    # right-click context menus
    contextMenuLog <- function(){
        # focused <- tkfocus()
        # on.exit(tkfocus(focused))
        .log <- LogWindow()
        tkfocus(.log)
        contextMenu <- tkmenu(tkmenu(.log), tearoff=FALSE)
        tkadd(contextMenu, "command", label=gettextRcmdr("Submit"), command=onSubmit)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Cut"), command=onCut)
        tkadd(contextMenu, "command", label=gettextRcmdr("Copy"), command=onCopy)
        tkadd(contextMenu, "command", label=gettextRcmdr("Paste"), command=onPaste)
        tkadd(contextMenu, "command", label=gettextRcmdr("Delete"), command=onDelete)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Find..."), command=onFind)
        tkadd(contextMenu, "command", label=gettextRcmdr("Select all"), command=onSelectAll)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Undo"), command=onUndo)
        tkadd(contextMenu, "command", label=gettextRcmdr("Redo"), command=onRedo)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Clear window"), command=onClear)
        tkpopup(contextMenu, tkwinfo("pointerx", .log), tkwinfo("pointery", .log))
    }
    contextMenuRmd <- function(){
        # focused <- tkfocus()
        # on.exit(tkfocus(focused))
        .rmd <- RmdWindow()
        tkfocus(.rmd)
        contextMenu <- tkmenu(tkmenu(.rmd), tearoff=FALSE)
        tkadd(contextMenu, "command", label=gettextRcmdr("Generate report"), command=onSubmit)
        tkadd(contextMenu, "command", label=gettextRcmdr("Edit R Markdown document"), command=editMarkdown)
        tkadd(contextMenu, "command", label=gettextRcmdr("Remove last Markdown command block"), command=removeLastRmdBlock)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Cut"), command=onCut)
        tkadd(contextMenu, "command", label=gettextRcmdr("Copy"), command=onCopy)
        tkadd(contextMenu, "command", label=gettextRcmdr("Paste"), command=onPaste)
        tkadd(contextMenu, "command", label=gettextRcmdr("Delete"), command=onDelete)
        tkadd(contextMenu, "separator")
        #        tkadd(contextMenu, "command", label=gettextRcmdr("Find..."), command=onFind)  # doesn't work FIXME
        tkadd(contextMenu, "command", label=gettextRcmdr("Select all"), command=onSelectAll)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Undo"), command=onUndo)
        tkadd(contextMenu, "command", label=gettextRcmdr("Redo"), command=onRedo)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Clear window"), command=onClear)
        tkpopup(contextMenu, tkwinfo("pointerx", .rmd), tkwinfo("pointery", .rmd))
    }
    contextMenuRnw <- function(){
        # focused <- tkfocus()
        # on.exit(tkfocus(focused))
        .rnw <- RnwWindow()
        tkfocus(.rnw)
        contextMenu <- tkmenu(tkmenu(.rnw), tearoff=FALSE)
        tkadd(contextMenu, "command", label=gettextRcmdr("Generate PDF report"), command=onSubmit)
        tkadd(contextMenu, "command", label=gettextRcmdr("Edit knitr document"), command=editKnitr)
        tkadd(contextMenu, "command", label=gettextRcmdr("Remove last knitr command block"), command=removeLastRnwBlock)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Cut"), command=onCut)
        tkadd(contextMenu, "command", label=gettextRcmdr("Copy"), command=onCopy)
        tkadd(contextMenu, "command", label=gettextRcmdr("Paste"), command=onPaste)
        tkadd(contextMenu, "command", label=gettextRcmdr("Delete"), command=onDelete)
        tkadd(contextMenu, "separator")
        #        tkadd(contextMenu, "command", label=gettextRcmdr("Find..."), command=onFind) # doesn't work FIXME
        tkadd(contextMenu, "command", label=gettextRcmdr("Select all"), command=onSelectAll)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Undo"), command=onUndo)
        tkadd(contextMenu, "command", label=gettextRcmdr("Redo"), command=onRedo)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Clear window"), command=onClear)
        tkpopup(contextMenu, tkwinfo("pointerx", .rnw), tkwinfo("pointery", .rnw))
    }
    contextMenuOutput <- function(){
        # focused <- tkfocus()
        # on.exit(tkfocus(focused))
        .output <- OutputWindow()
        tkfocus(.output)
        contextMenu <- tkmenu(tkmenu(.output), tearoff=FALSE)
        tkadd(contextMenu, "command", label=gettextRcmdr("Cut"), command=onCut)
        tkadd(contextMenu, "command", label=gettextRcmdr("Copy"), command=onCopy)
        tkadd(contextMenu, "command", label=gettextRcmdr("Paste"), command=onPaste)
        tkadd(contextMenu, "command", label=gettextRcmdr("Delete"), command=onDelete)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Find..."), command=onFind)
        tkadd(contextMenu, "command", label=gettextRcmdr("Select all"), command=onSelectAll)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Undo"), command=onUndo)
        tkadd(contextMenu, "command", label=gettextRcmdr("Redo"), command=onRedo)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Clear window"), command=onClear)
        tkpopup(contextMenu, tkwinfo("pointerx", .output), tkwinfo("pointery", .output))
    }
    contextMenuMessages <- function(){
        # focused <- tkfocus()
        # on.exit(tkfocus(focused))
        .messages <- MessagesWindow()
        tkfocus(.messages)
        contextMenu <- tkmenu(tkmenu(.messages), tearoff=FALSE)
        tkadd(contextMenu, "command", label=gettextRcmdr("Cut"), command=onCut)
        tkadd(contextMenu, "command", label=gettextRcmdr("Copy"), command=onCopy)
        tkadd(contextMenu, "command", label=gettextRcmdr("Paste"), command=onPaste)
        tkadd(contextMenu, "command", label=gettextRcmdr("Delete"), command=onDelete)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Find..."), command=onFind)
        tkadd(contextMenu, "command", label=gettextRcmdr("Select all"), command=onSelectAll)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Undo"), command=onUndo)
        tkadd(contextMenu, "command", label=gettextRcmdr("Redo"), command=onRedo)
        tkadd(contextMenu, "separator")
        tkadd(contextMenu, "command", label=gettextRcmdr("Clear window"), command=onClear)
        tkpopup(contextMenu, tkwinfo("pointerx", .messages), tkwinfo("pointery", .messages))
    }
    
    # main Commander window
    if (getRcmdr("crisp.dialogs")) tclServiceMode(on=FALSE)
    putRcmdr("commanderWindow", tktoplevel(class="Rcommander"))
    .commander <- CommanderWindow()
    tkwm.minsize(.commander, getRcmdr("minimum.width"), getRcmdr("minimum.height"))
    tcl("wm", "iconphoto", .commander, "-default", "::image::RlogoIcon")
    placement <- setOption("placement", "", global=FALSE)
    tkwm.geometry(.commander, placement)
    tkwm.title(.commander, gettextRcmdr("R Commander"))
    tkwm.protocol(.commander, "WM_DELETE_WINDOW", 
                  if (getRcmdr("quit.R.on.close")) closeCommanderAndR else CloseCommander)
    topMenu <- tkmenu(.commander)
    tkconfigure(.commander, menu=topMenu)
    position <- numeric(0)
    
    # install menus
    .Menus <- menus <- list()
    menuItems <- 0
    oldMenu <- ncol(Menus) == 6
    if (!getRcmdr("suppress.menus")){
        for (m in 1:nrow(Menus)){
            install <- if (oldMenu) "" else Menus[m, 7]
            if ((install != "") && (!eval(parse(text=install)))) next
            if (Menus[m, 1] == "menu") {
                position[Menus[m, 2]] <- 0
                assign(Menus[m, 2], tkmenu(get(Menus[m, 3]), tearoff=FALSE))
                menus[[Menus[m, 2]]] <- list(ID=get(Menus[m, 2])$ID, position=0)
            }
            else if (Menus[m, 1] == "item") {
                position[Menus[m, 2]] <- position[Menus[m, 2]] + 1
                if (Menus[m, 3] == "command"){
                    if (Menus[m, 6] == "")
                        tkadd(get(Menus[m, 2]), "command", label=gettextMenus(Menus[m, 4]),
                              command=get(Menus[m, 5]))
                    else {
                        tkadd(get(Menus[m, 2]), "command", label=gettextMenus(Menus[m, 4]),
                              command=get(Menus[m, 5]), state="disabled")
                        menuItems <- menuItems + 1
                        menus[[Menus[m, 2]]]$position <- position[Menus[m, 2]]
                        .Menus[[menuItems]] <- list(ID=menus[[Menus[m, 2]]]$ID, position=position[Menus[m, 2]],
                                                    activation=eval(parse(text=paste("function()", Menus[m, 6]))))
                    }
                }
                else if (Menus[m, 3] == "cascade")
                    tkadd(get(Menus[m, 2]), "cascade", label=gettextMenus(Menus[m, 4]),
                          menu=get(Menus[m, 5]))
                else if (Menus[m, 3] == "separator")
                    tkadd(get(Menus[m, 2]), "separator")
                else stop(paste(gettextRcmdr("menu definition error:"), Menus[m, ], collapse=" "),
                          domain=NA)
            }
            else stop(paste(gettextRcmdr("menu definition error:"), Menus[m, ], collapse=" "),
                      domain=NA)
        }
    }
    putRcmdr("Menus", .Menus)
    putRcmdr("autoRestart", FALSE)
    activateMenus()
    
    # toolbar
    controlsFrame <- tkframe(CommanderWindow())
    editButton <- buttonRcmdr(controlsFrame, text=gettextRcmdr("Edit data set"), command=onEdit, 
                              image="::image::editIcon", compound="left")
    viewButton <- buttonRcmdr(controlsFrame, text=gettextRcmdr("View data set"), command=onView,
                              image="::image::viewIcon", compound="left")
    putRcmdr("dataSetName", tclVar(gettextRcmdr("<No active dataset>")))
    putRcmdr("dataSetLabel", tkbutton(controlsFrame, textvariable=getRcmdr("dataSetName"), foreground="red",
                                      relief="groove", command=selectActiveDataSet, image="::image::dataIcon", compound="left"))
    
    # script and markdown tabs
    notebook <- ttknotebook(CommanderWindow())
    logFrame <- ttkframe(CommanderWindow())    
    putRcmdr("logWindow", tktext(logFrame, bg="white", foreground=getRcmdr("log.text.color"),
                                 font=getRcmdr("logFont"), height=getRcmdr("log.height"), 
                                 width=getRcmdr("log.width"), wrap="none", undo=TRUE))
    .log <- LogWindow()
    logXscroll <- ttkscrollbar(logFrame, orient="horizontal",
                               command=function(...) tkxview(.log, ...))
    logYscroll <- ttkscrollbar(logFrame,
                               command=function(...) tkyview(.log, ...))
    tkconfigure(.log, xscrollcommand=function(...) tkset(logXscroll, ...))
    tkconfigure(.log, yscrollcommand=function(...) tkset(logYscroll, ...))
    RmdFrame <- ttkframe(CommanderWindow())
    putRcmdr("RmdWindow", tktext(RmdFrame, bg="#FAFAFA", foreground=getRcmdr("log.text.color"),
                                 font=getRcmdr("logFont"), height=getRcmdr("log.height"), 
                                 width=getRcmdr("log.width"), wrap="none", undo=TRUE))
    .rmd <- RmdWindow()
    rmd.template <- setOption("rmd.template", 
                              system.file("etc", if (getRcmdr("capabilities")$pandoc) "Rcmdr-RMarkdown-Template.Rmd"
                                          else "Rcmdr-Markdown-Template.Rmd", package="Rcmdr"))
    template <- paste(readLines(rmd.template), collapse="\n")
    
        # template customization and translation:
    template <- sub("Your Name", getRcmdr("UserName"), template)
    template <- sub("Replace with Main Title", 
                    gettextRcmdr("Replace with Main Title"), template)
    template <- sub("include this code chunk as-is to set options",
                    gettextRcmdr("include this code chunk as-is to set options"),
                    template)
    template <- sub("You can edit this R Markdown document, for example to explain what you're\ndoing and to draw conclusions from your data analysis.",
                    gettextRcmdr("You can edit this R Markdown document, for example to explain what you're\ndoing and to draw conclusions from your data analysis."),
                    template)
    template <- sub("Auto-generated section titles, typically preceded by ###, can also be edited.",
                    gettextRcmdr("Auto-generated section titles, typically preceded by ###, can also be edited."),
                    template)
    template <- sub("It's generally not a good idea to edit the R code that the R Commander writes, \nbut you can freely edit between (not within) R \"code blocks.\" Each R code\nblock starts with ```{r} and ends with ```.",
                    gettextRcmdr("It's generally not a good idea to edit the R code that the R Commander writes, \nbut you can freely edit between (not within) R \"code blocks.\" Each R code\nblock starts with ```{r} and ends with ```."),
                    template, fixed = TRUE)
    
    # if (getRcmdr("use.rgl")) template <- paste0(template, 
    #                                             "\n\n```{r echo=FALSE}\n# include this code chunk as-is to enable 3D graphs\nlibrary(rgl)\noptions(rgl.useNULL = TRUE)\n```\n\n")
    tkinsert(.rmd, "end", template)
    putRcmdr("markdown.output", FALSE)
    RmdXscroll <- ttkscrollbar(RmdFrame, orient="horizontal",
                               command=function(...) tkxview(.rmd, ...))
    RmdYscroll <- ttkscrollbar(RmdFrame,
                               command=function(...) tkyview(.rmd, ...))
    tkconfigure(.rmd, xscrollcommand=function(...) tkset(RmdXscroll, ...))
    tkconfigure(.rmd, yscrollcommand=function(...) tkset(RmdYscroll, ...))    
    
    RnwFrame <- ttkframe(CommanderWindow())
    putRcmdr("RnwWindow", tktext(RnwFrame, bg="#FAFAFA", foreground=getRcmdr("log.text.color"),
                                 font=getRcmdr("logFont"), height=getRcmdr("log.height"), 
                                 width=getRcmdr("log.width"), wrap="none", undo=TRUE))
    .rnw <- RnwWindow()
    rnw.template <- setOption("rnw.template", 
                              system.file("etc", "Rcmdr-knitr-Template.Rnw", package="Rcmdr"))
    template <- paste(readLines(rnw.template), collapse="\n")
    template <- sub("Your Name", getRcmdr("UserName"), template)
    template <- sub("Replace with Main Title", 
                    gettextRcmdr("Replace with Main Title"), template)
    tkinsert(.rnw, "end", template)
    putRcmdr("knitr.output", FALSE)
    RnwXscroll <- ttkscrollbar(RnwFrame, orient="horizontal",
                               command=function(...) tkxview(.rnw, ...))
    RnwYscroll <- ttkscrollbar(RnwFrame,
                               command=function(...) tkyview(.rnw, ...))
    tkconfigure(.rnw, xscrollcommand=function(...) tkset(RnwXscroll, ...))
    tkconfigure(.rnw, yscrollcommand=function(...) tkset(RnwYscroll, ...))    
    
    outputFrame <- tkframe(.commander) 
    submitButtonLabel <- tclVar(gettextRcmdr("Submit"))
    submitButton <- if (getRcmdr("console.output"))
        buttonRcmdr(CommanderWindow(), textvariable=submitButtonLabel, borderwidth="2", command=onSubmit,
                    image="::image::submitIcon", compound="left")
    else buttonRcmdr(outputFrame, textvariable=submitButtonLabel, borderwidth="2", command=onSubmit, 
                     image="::image::submitIcon", compound="left")
    
    tkbind(CommanderWindow(), "<Button-1>", function() {
        if (as.character(tkselect(notebook)) == logFrame$ID) tclvalue(submitButtonLabel) <- gettextRcmdr("Submit")
        if (as.character(tkselect(notebook)) == RmdFrame$ID) tclvalue(submitButtonLabel) <- gettextRcmdr("Generate report")
        if (as.character(tkselect(notebook)) == RnwFrame$ID) tclvalue(submitButtonLabel) <- gettextRcmdr("Generate PDF report")
    })
    putRcmdr("outputWindow", tktext(outputFrame, bg="white", foreground=getRcmdr("output.text.color"),
                                    font=getRcmdr("logFont"), height=getRcmdr("output.height"), 
                                    width=getRcmdr("log.width"), wrap="none", undo=TRUE))
    .output <- OutputWindow()
    outputXscroll <- ttkscrollbar(outputFrame, orient="horizontal",
                                  command=function(...) tkxview(.output, ...))
    outputYscroll <- ttkscrollbar(outputFrame,
                                  command=function(...) tkyview(.output, ...))
    tkconfigure(.output, xscrollcommand=function(...) tkset(outputXscroll, ...))
    tkconfigure(.output, yscrollcommand=function(...) tkset(outputYscroll, ...))
    # messages window
    messagesFrame <- tkframe(.commander)
    putRcmdr("messagesWindow", tktext(messagesFrame, bg="lightgray",
                                      font=getRcmdr("logFont"), height=getRcmdr("messages.height"), 
                                      width=getRcmdr("log.width"), wrap="none", undo=TRUE))
    .messages <- MessagesWindow()
    messagesXscroll <- ttkscrollbar(messagesFrame, orient="horizontal",
                                    command=function(...) tkxview(.messages, ...))
    messagesYscroll <- ttkscrollbar(messagesFrame,
                                    command=function(...) tkyview(.messages, ...))
    tkconfigure(.messages, xscrollcommand=function(...) tkset(messagesXscroll, ...))
    tkconfigure(.messages, yscrollcommand=function(...) tkset(messagesYscroll, ...))
    
    # configure toolbar, etc., install various windows and widgets
    putRcmdr("modelName", tclVar(gettextRcmdr("<No active model>")))
    putRcmdr("modelLabel", tkbutton(controlsFrame, textvariable=getRcmdr("modelName"), foreground="red",
                                    relief="groove", command=selectActiveModel, image="::image::modelIcon", compound="left"))
    show.edit.button <- options("Rcmdr")[[1]]$show.edit.button
    show.edit.button <- if (is.null(show.edit.button)) TRUE else show.edit.button
    if (!getRcmdr("suppress.menus")){
        tkgrid(labelRcmdr(controlsFrame, image="::image::RlogoIcon", compound="left"),
               labelRcmdr(controlsFrame, text=gettextRcmdr("   Data set:")), getRcmdr("dataSetLabel"),
               if(show.edit.button) editButton, viewButton,
               labelRcmdr(controlsFrame, text=gettextRcmdr("Model:")), getRcmdr("modelLabel"), sticky="w", pady=c(3, 3))
        tkgrid(controlsFrame, sticky="w")
        tkgrid.configure(getRcmdr("dataSetLabel"), padx=c(2, 5))
        tkgrid.configure(getRcmdr("modelLabel"), padx=c(2, 10))
        tkgrid.configure(editButton, padx=c(10, 1))
        if (show.edit.button) tkgrid.configure(viewButton, padx=c(1, 15))
        else tkgrid.configure(viewButton, padx=c(10, 15))
    }
    .log.commands <-  getRcmdr("log.commands")
    .console.output <- getRcmdr("console.output")
    if (.log.commands) {
        tkgrid(.log, logYscroll, sticky="news", columnspan=2)
        tkgrid(logXscroll)
        tkgrid(logFrame, sticky="news", padx=10, pady=0, columnspan=2)
        tkgrid(.rmd, RmdYscroll, sticky="news", columnspan=2)
        tkgrid(RmdXscroll)
        tkgrid(.rnw, RnwYscroll, sticky="news", columnspan=2)
        tkgrid(RnwXscroll)
        if (getRcmdr("use.markdown")) tkgrid(RmdFrame, sticky="news", padx=10, pady=0, columnspan=2)
        if (getRcmdr("use.knitr")) tkgrid(RnwFrame, sticky="news", padx=10, pady=0, columnspan=2)
    }
    tkadd(notebook, logFrame, text=gettextRcmdr("R Script"), padding=6)
    if (getRcmdr("use.markdown")) tkadd(notebook, RmdFrame, text=gettextRcmdr("R Markdown"), padding=6)
    if (getRcmdr("use.knitr")) tkadd(notebook, RnwFrame, text=gettextRcmdr("knitr Document"), padding=6)
    # tkgrid(notebook, sticky="news")
    if (.log.commands) {
        tkgrid(notebook, sticky="news")
    }
#    if (.log.commands && .console.output) tkgrid(submitButton, sticky="w", pady=c(0, 6))
    if (.log.commands && .console.output) tkgrid(submitButton, sticky="e", pady=c(0, 6), padx=c(0, 6))
    tkgrid(labelRcmdr(outputFrame, text=gettextRcmdr("Output"), font="RcmdrOutputMessagesFont", foreground=getRcmdr("title.color")),
           if (.log.commands && !.console.output) submitButton, sticky="sw", pady=c(6, 6))
    tkgrid(.output, outputYscroll, sticky="news", columnspan=2)
    tkgrid(outputXscroll, columnspan=1 + (.log.commands && !.console.output))
    if (!.console.output) tkgrid(outputFrame, sticky="news", padx=10, pady=0, columnspan=2)
    tkgrid(labelRcmdr(messagesFrame, text=gettextRcmdr("Messages"), font="RcmdrOutputMessagesFont", foreground=getRcmdr("title.color")), 
           sticky="w", pady=c(6, 6))
    tkgrid(.messages, messagesYscroll, sticky="news", columnspan=2)
    tkgrid(messagesXscroll)
    if (!.console.output) tkgrid(messagesFrame, sticky="news", padx=10, pady=0, columnspan=2) ##rmh & J. Fox
    tkgrid.configure(logYscroll, sticky="ns")
    tkgrid.configure(logXscroll, sticky="ew")
    tkgrid.configure(RmdYscroll, sticky="ns")
    tkgrid.configure(RmdXscroll, sticky="ew")
    tkgrid.configure(RnwYscroll, sticky="ns")
    tkgrid.configure(RnwXscroll, sticky="ew")
    tkgrid.configure(outputYscroll, sticky="ns")
    tkgrid.configure(outputXscroll, sticky="ew")
    tkgrid.configure(messagesYscroll, sticky="ns")
    tkgrid.configure(messagesXscroll, sticky="ew")
    .commander <- CommanderWindow()
    tkgrid.rowconfigure(.commander, 0, weight=0)
    tkgrid.rowconfigure(.commander, 1, weight=1)
#    tkgrid.rowconfigure(.commander, 2, weight=1)
    w <- if (.log.commands && !.console.output) 1 else 0
    tkgrid.rowconfigure(.commander, 2, weight=w)
    tkgrid.columnconfigure(.commander, 0, weight=1)
    tkgrid.columnconfigure(.commander, 1, weight=0)
    if (.log.commands){
        tkgrid.rowconfigure(logFrame, 0, weight=1)
        tkgrid.rowconfigure(logFrame, 1, weight=0)
        tkgrid.columnconfigure(logFrame, 0, weight=1)
        tkgrid.columnconfigure(logFrame, 1, weight=0)
        if (getRcmdr("use.markdown")){
            tkgrid.rowconfigure(RmdFrame, 0, weight=1)
            tkgrid.rowconfigure(RmdFrame, 1, weight=0)
            tkgrid.columnconfigure(RmdFrame, 0, weight=1)
            tkgrid.columnconfigure(RmdFrame, 1, weight=0)
        }
        if (getRcmdr("use.knitr")){
            tkgrid.rowconfigure(RnwFrame, 0, weight=1)
            tkgrid.rowconfigure(RnwFrame, 1, weight=0)
            tkgrid.columnconfigure(RnwFrame, 0, weight=1)
            tkgrid.columnconfigure(RnwFrame, 1, weight=0)
        }
    }
    if (!.console.output){
        tkgrid.rowconfigure(outputFrame, 0, weight=0)
        tkgrid.rowconfigure(outputFrame, 1, weight=1)
        tkgrid.rowconfigure(outputFrame, 2, weight=0)
        tkgrid.columnconfigure(outputFrame, 0, weight=1)
        tkgrid.columnconfigure(outputFrame, 1, weight=0)
    }
    tkgrid.rowconfigure(messagesFrame, 0, weight=0)
    tkgrid.rowconfigure(messagesFrame, 1, weight=0)
    tkgrid.rowconfigure(messagesFrame, 2, weight=0)
    tkgrid.columnconfigure(messagesFrame, 0, weight=1)
    tkgrid.columnconfigure(messagesFrame, 1, weight=0)
    .Tcl("update idletasks")
    tkbind(.commander, "<Control-x>", onCut)
    tkbind(.commander, "<Control-X>", onCut)
    tkbind(.commander, "<Control-c>", onCopy)
    tkbind(.commander, "<Control-C>", onCopy)
    tkbind(.commander, "<Control-r>", onSubmit)
    tkbind(.commander, "<Control-R>", onSubmit)
    tkbind(.commander, "<Control-Tab>", onSubmit)
    tkbind(.commander, "<Control-f>", onFind)
    tkbind(.commander, "<Control-F>", onFind)
    tkbind(.commander, "<F3>", onFind)
    tkbind(.commander, "<Control-s>", saveLog)
    tkbind(.commander, "<Control-S>", saveLog)
    tkbind(.commander, "<Control-a>", onSelectAll)
    tkbind(.commander, "<Control-A>", onSelectAll)
    tkbind(.commander, "<Control-w>", onRedo)
    tkbind(.commander, "<Control-W>", onRedo)
    tkbind(.commander, "<Alt-BackSpace>", onUndo)
    tkbind(.log, "<ButtonPress-3>", contextMenuLog)
    tkbind(.rmd, "<ButtonPress-3>", contextMenuRmd)
    tkbind(.rnw, "<ButtonPress-3>", contextMenuRnw)
    tkbind(.output, "<ButtonPress-3>", contextMenuOutput)
    tkbind(.messages, "<ButtonPress-3>", contextMenuMessages)
    tkbind(.log, "<Control-ButtonPress-1>", contextMenuLog)
    tkbind(.rmd, "<Control-ButtonPress-1>", contextMenuRmd)
    tkbind(.rnw, "<Control-ButtonPress-1>", contextMenuRnw)
    tkbind(.output, "<Control-ButtonPress-1>", contextMenuOutput)
    tkbind(.messages, "<Control-ButtonPress-1>", contextMenuMessages)
    tkbind(.rmd, "<Control-e>", editMarkdown)
    tkbind(.rmd, "<Control-E>", editMarkdown)
    tkbind(.rnw, "<Control-e>", editKnitr)
    tkbind(.rnw, "<Control-E>", editKnitr)
    if (MacOSXP()){
        tkbind(.commander, "<Meta-x>", onCut)
        tkbind(.commander, "<Meta-X>", onCut)
        tkbind(.commander, "<Meta-c>", onCopy)
        tkbind(.commander, "<Meta-C>", onCopy)
        tkbind(.commander, "<Meta-v>", onPaste)
        tkbind(.commander, "<Meta-V>", onPaste)
        tkbind(.commander, "<Meta-r>", onSubmit)
        tkbind(.commander, "<Meta-R>", onSubmit)
        tkbind(.commander, "<Meta-Tab>", onSubmit)
        tkbind(.commander, "<Meta-f>", onFind)
        tkbind(.commander, "<Meta-F>", onFind)
        tkbind(.commander, "<Meta-s>", saveLog)
        tkbind(.commander, "<Meta-S>", saveLog)
        tkbind(.commander, "<Meta-a>", onSelectAll)
        tkbind(.commander, "<Meta-A>", onSelectAll)
        tkbind(.commander, "<Meta-w>", onRedo)
        tkbind(.commander, "<Meta-W>", onRedo)
        tkbind(.commander, "<Meta-z>", onUndo)
        tkbind(.commander, "<Meta-Z>", onUndo)
        tkbind(.commander, "<Shift-Meta-z>", onRedo)
        tkbind(.commander, "<Shift-Meta-Z>", onRedo)
        tkbind(.log, "<Meta-ButtonPress-1>", contextMenuLog)
        tkbind(.rmd, "<Meta-ButtonPress-1>", contextMenuRmd)
        tkbind(.rnw, "<Meta-ButtonPress-1>", contextMenuRnw)
        tkbind(.output, "<Meta-ButtonPress-1>", contextMenuOutput)
        tkbind(.messages, "<Meta-ButtonPress-1>", contextMenuMessages)
        tkbind(.rmd, "<Meta-e>", editMarkdown)
        tkbind(.rmd, "<Meta-E>", editMarkdown)
        tkbind(.rnw, "<Meta-e>", editKnitr)
        tkbind(.rnw, "<Meta-E>", editKnitr)
    }
    tkwm.deiconify(.commander)
    tkfocus(.commander)
    if (getRcmdr("crisp.dialogs")) tclServiceMode(on=TRUE)
    tkwait.commander <- options("Rcmdr")[[1]]$tkwait.commander  # to address problem in Debian Linux
    if ((!is.null(tkwait.commander)) && tkwait.commander) {
        putRcmdr(".commander.done", tclVar("0"))
        tkwait.variable(getRcmdr(".commander.done"))
    }
    Message(paste(gettextRcmdr("R Commander Version "), " ", getRcmdr("RcmdrVersion"), ": ", date(), sep=""))
    Message(paste(gettextRcmdr("R Version"),  getRcmdr("RVersion"), getRcmdr("RVersionStatus")))
    Message(paste(gettextRcmdr("Hello "), getRcmdr("UserName"), sep=""))
    if (.Platform$GUI == "Rgui"  && ismdi()) Message(gettextRcmdr(
        "The Windows version of the R Commander works best under\nRGui with the single-document interface (SDI); see ?Commander."),
        type="warning")
    if (RappP()  && mavericksP() && appnap() == "on") Message(gettextRcmdr(
        "The Mac OS X version of the R Commander works best under R.app\nwith app nap turned off. See ?Commander and the Tools menu."),
        type="warning")
    
}


# put commands in script, markdown, and knitr tabs
#' @export
logger <- function(command, rmd=TRUE){
    pushCommand(command)
    .log <- LogWindow()
    .rmd <- RmdWindow()
    .rnw <- RnwWindow()
    .output <- OutputWindow()
    .markdown.editor.open <- getRcmdr("Markdown.editor.open")
    .markdown.editor <- MarkdownEditorWindow()
    .knitr.editor.open <- getRcmdr("knitr.editor.open")
    .knitr.editor <- knitrEditorWindow()
    Rmd <- rmd && is.null(attr(command, "suppressRmd")) && (getRcmdr("use.markdown") || getRcmdr("use.knitr"))
    command <- splitCmd(command)
    if (getRcmdr("log.commands")) {
        last2 <- tclvalue(tkget(.log, "end -2 chars", "end"))
        if (last2 != "\n\n") tkinsert(.log, "end", "\n")
        tkinsert(.log, "end", paste(command,"\n", sep=""))
        tkyview.moveto(.log, 1)
        if (Rmd){
            if (getRcmdr("use.markdown")){
                if (getRcmdr("startNewCommandBlock")){
                    beginRmdBlock()
                    tkinsert(.rmd, "end", paste(command, "\n", sep=""))
                    tkyview.moveto(.rmd, 1)
                    putRcmdr("markdown.output", TRUE)
                    if (.markdown.editor.open){
                        tkinsert(.markdown.editor, "end", paste(command, "\n", sep=""))
                        tkyview.moveto(.markdown.editor, 1)
                    }
                    endRmdBlock()
                }
                else{
                    tkinsert(.rmd, "end", paste(command, "\n", sep=""))
                    tkyview.moveto(.rmd, 1)
                    putRcmdr("markdown.output", TRUE)
                    putRcmdr("rmd.generated", TRUE)
                    if (.markdown.editor.open){
                        tkinsert(.markdown.editor, "end", paste(command, "\n", sep=""))
                        tkyview.moveto(.markdown.editor, 1)
                    }
                }
              if (getRcmdr("command.sections")){
                command.name <- findCommandName(command)
                if(!is.na(command.name)){
                  insertRmdSection(command.name)
                }
              }
            }
            if (getRcmdr("use.knitr")){
                if (getRcmdr("startNewKnitrCommandBlock")){
                    beginRnwBlock()
                    tkinsert(.rnw, "end", paste(command, "\n", sep=""))
                    tkyview.moveto(.rnw, 1)
                    putRcmdr("knitr.output", TRUE)
                    if (.knitr.editor.open){
                        tkinsert(.knitr.editor, "end", paste(command, "\n", sep=""))
                        tkyview.moveto(.knitr.editor, 1)
                    }
                    endRnwBlock()
                }
                else{
                    tkinsert(.rnw, "end", paste(command, "\n", sep=""))
                    tkyview.moveto(.rnw, 1)
                    putRcmdr("knitr.output", TRUE)
                    putRcmdr("rnw.generated", TRUE)
                    if (.knitr.editor.open){
                        tkinsert(.knitr.editor, "end", paste(command, "\n", sep=""))
                        tkyview.moveto(.knitr.editor, 1)
                    }
                }
            }
        }
        
    }
    lines <- strsplit(command, "\n")[[1]]
    tkinsert(.output, "end", "\n")
    if (getRcmdr("console.output")) {
        for (line in seq(along.with=lines)) {
            prompt <- ifelse (line==1, paste("\n", getRcmdr("prefixes")[1], sep=""), paste("\n", getRcmdr("prefixes")[2], sep=""))
            cat(paste(prompt, lines[line]))
        }
        cat("\n")
    }
    else {
        for (line in  seq(along.with=lines)) {
            prompt <- ifelse(line==1, "> ", "+ ")
            tkinsert(.output, "end", paste(prompt, lines[line], "\n", sep=""))
            tktag.add(.output, "currentLine", "end - 2 lines linestart", "end - 2 lines lineend")
            tktag.configure(.output, "currentLine", foreground=getRcmdr("command.text.color"))
            tkyview.moveto(.output, 1)
        }
    }
    command
}

#' @export
justDoIt <- function(command) {
    command <- enc2native(command)
    Message()
    if (!getRcmdr("suppress.X11.warnings")){
        messages.connection <- file(open="w+")
        sink(messages.connection, type="message")
        on.exit({
            sink(type="message")
            close(messages.connection)
        })
    }
    else messages.connection <- getRcmdr("messages.connection")
    capture.output(result <- try(eval(parse(text=command), envir=.GlobalEnv), silent=TRUE))
    if (class(result)[1] ==  "try-error"){
        Message(message=paste(strsplit(result, ":")[[1]][2]), type="error")
        tkfocus(CommanderWindow())
        return(result)
    }
    checkWarnings(readLines(messages.connection))
    if (getRcmdr("RStudio")) Sys.sleep(0)
    result
}

# execute commands, save commands and output
#' @export
doItAndPrint <- function(command, log=TRUE, rmd=log) {
    command <- enc2native(command)
    Message()
    .console.output <- getRcmdr("console.output")
    .output <- OutputWindow()
    if (!.console.output) {
        width <- (as.numeric(tkwinfo("width", .output)) - 2*as.numeric(tkcget(.output, borderwidth=NULL)) - 2)/
            as.numeric(tkfont.measure(tkcget(.output, font=NULL), "0"))
        eval(parse(text=paste("options(width=", floor(width), ")", sep="")))
    }
    if (!getRcmdr("suppress.X11.warnings")){
        messages.connection <- file(open="w+")
        sink(messages.connection, type="message")
        on.exit({
            sink(type="message")
            close(messages.connection)
        })
    }
    else messages.connection <- getRcmdr("messages.connection")
    output.connection <- file(open="w+")
    sink(output.connection, type="output")
    on.exit({
        if (!.console.output) sink(type="output") # if .console.output, output connection already closed
        close(output.connection)
    }, add=TRUE)
    if (log) logger(command, rmd=rmd) 
    else {
        pushCommand(command)
        if (rmd) {
            if (getRcmdr("use.markdown")) enterMarkdown(command)
            if (getRcmdr("use.knitr")) enterKnitr(command)
        }
    }
    
    result <- try(parse(text=paste(command)), silent=TRUE)
    if (class(result)[1] == "try-error"){
        if (rmd) {
            if (getRcmdr("use.markdown")) {
                removeLastRmdBlock()
                putRcmdr("startNewCommandBlock", TRUE)
            }
            if (getRcmdr("use.knitr")) {
                removeLastRnwBlock()
                putRcmdr("startNewKnitrCommandBlock", TRUE)
            }
        }
        Message(message=paste(strsplit(result, ":")[[1]][2]), type="error")
        if (.console.output) sink(type="output")
        tkfocus(CommanderWindow())
        return(result)
    } else {
        exprs <- result
        result <- NULL
    }
    for (i in seq_along(exprs)) {
        ei <- exprs[i]
        tcl("update")
        result <-  try(withVisible(eval(ei, envir=.GlobalEnv)), silent=TRUE)
        if (class(result)[1] ==  "try-error"){
            if (rmd) {
                if (getRcmdr("use.markdown")) {
                    removeLastRmdBlock()
                    putRcmdr("startNewCommandBlock", TRUE)
                }
                if (getRcmdr("use.knitr")) {
                    removeLastRnwBlock()
                    putRcmdr("startNewKnitrCommandBlock", TRUE)
                }
            }
            Message(message=paste(strsplit(result, ":")[[1]][2]), type="error")
            if (.console.output) sink(type="output")
            tkfocus(CommanderWindow())
            return(result)
        }
        result <- if (result$visible == FALSE) NULL else result$value
        if (!is.null(result)) pushOutput(result)
        if (isS4object(result)) show(result) else print(result)
        .Output <- readLines(output.connection)
        if (length(.Output) > 0 && .Output[length(.Output)] == "NULL")
            .Output <- .Output[-length(.Output)] # suppress "NULL" line at end of output
        if (length(.Output) != 0) {  # is there output to print?
            if (.console.output) {
                out <- .Output
                sink(type="output")
                for (line in out) cat(paste(line, "\n", sep=""))
            }
            else{
                for (line in .Output) tkinsert(.output, "end", paste(line, "\n", sep=""))
                tkyview.moveto(.output, 1)
            }
        }
        else if (.console.output) sink(type="output")
        if (RExcelSupported()) # added by Erich Neuwirth
            putRExcel(".rexcel.last.output",.Output)
        # errors already intercepted, display any warnings
        checkWarnings(readLines(messages.connection))
    }
    if (getRcmdr("RStudio")) Sys.sleep(0)
    result
}

checkWarnings <- function(messages){
    if (getRcmdr("suppress.X11.warnings")){
        X11.warning <- grep("X11 protocol error|Warning in structure", messages)
        if (length(X11.warning) > 0){
            messages <- messages[-X11.warning]
        }
        if (length(messages) == 0) Message()
        else if (length(messages) > 10) {
            messages <- c(paste(length(messages), "warnings."),
                gettextRcmdr("First and last 5 warnings:"),
                head(messages,5), ". . .", tail(messages, 5))
            Message(message=paste(messages, collapse="\n"), type="warning")
        }
        else {
            if (length(grep("warning", messages, ignore.case=TRUE)) > 0)
                Message(message=paste(messages, collapse="\n"), type="warning")
            else Message(message=paste(messages, collapse="\n"), type="note")
        }
    }
    else{
        if (length(messages) == 0) Message()
        else if (length(messages) > 10){
            messages <- c(paste(length(messages), "warnings."),
                gettextRcmdr("First and last 5 warnings:"),
                head(messages, 5), ". . .", tail(messages, 5))
            Message(message=paste(messages, collapse="\n"), type="warning")
        }
        else {
            if (length(grep("warning", messages, ignore.case=TRUE)) > 0)
                Message(message=paste(messages, collapse="\n"), type="warning")
            else Message(message=paste(messages, collapse="\n"), type="note")
        }
    }
    tkfocus(CommanderWindow())
}

pause <- function(seconds = 1){
    if (seconds <= 0) stop("seconds must be positive")
    start <- proc.time()[3]
    while (as.numeric(elapsed <- (proc.time()[3] - start)) < seconds) {}
    elapsed
}

#' @export
Message <- function(message, type=c("note", "error", "warning")){
    tcl("update") 
    .message <- MessagesWindow()
    type <- match.arg(type)
    if (type != "note") tkbell()
    if (getRcmdr("retain.messages")) {
        if (missing(message) && !is.null(getRcmdr("last.message"))) {
            putRcmdr("last.message", NULL)
            tkyview.moveto(.message, 1.0)
        }
    }
    else if (type == "note"){
        lastMessage <- tclvalue(tkget(MessagesWindow(),  "end - 2 lines", "end"))
        if (length(c(grep(gettextRcmdr("ERROR:"), lastMessage), grep(gettextRcmdr("WARNING:"), lastMessage))) == 0)
            tkdelete(.message, "1.0", "end")
    }
    else tkdelete(.message, "1.0", "end")
    col <- if (type == "error") getRcmdr("error.text.color")
    else if (type == "warning") getRcmdr("warning.text.color")
    else getRcmdr("output.text.color")
    prefix <- switch(type, error=gettextRcmdr("ERROR"), warning=gettextRcmdr("WARNING"), note=gettextRcmdr("NOTE"))
    if (missing(message)){
        return()
    }
    putRcmdr("last.message", type)
    message <- paste(prefix, ": ", message, sep="")
    if (getRcmdr("retain.messages") && getRcmdr("number.messages")) {
        messageNumber <- getRcmdr("messageNumber") + 1
        putRcmdr("messageNumber", messageNumber)
        message <- paste("[", messageNumber, "] ", message, sep="")
    }
    if (RExcelSupported()) # added by Erich Neuwirth
        putRExcel(".rexcel.last.message",message)
    lines <- strsplit(message, "\n")[[1]]
    console.output <- getRcmdr("console.output")
    if (!console.output){
        width <- (as.numeric(tkwinfo("width", .message)) - 2*as.numeric(tkcget(.message, borderwidth=NULL)) - 2)/
            as.numeric(tkfont.measure(tkcget(.message, font=NULL), "0"))
        eval(parse(text=paste("options(width=", floor(width), ")", sep="")))
    }
    lines <- strwrap(lines)
    if (console.output) {
        if (sink.number() != 0) sink()
        for (jline in seq(along.with=lines)) {
            Header <- if (jline==1) getRcmdr("prefixes")[3] else getRcmdr("prefixes")[4]
            cat(paste(Header, lines[jline], "\n", sep=""))
        } 
    }
    else
        for (line in lines){
            tagName <- messageTag()
            tkinsert(.message, "end", paste(line, "\n", sep=""))
            tktag.add(.message, tagName, "end - 2 lines linestart", "end - 2 lines lineend")
            tktag.configure(.message, tagName, foreground=col)
            tkyview.moveto(.message, 1.0)
        }
}

messageTag <- function(reset=FALSE){
    if (reset){
        putRcmdr("tagNumber", 0)
        return()
    }
    tagNumber <- getRcmdr("tagNumber") + 1
    putRcmdr("tagNumber", tagNumber)
    paste("message", tagNumber, sep="")
}

pushOutput <- function(element) {
    stack <- getRcmdr("outputStack")
    stack <- c(list(element), stack[-getRcmdr("length.output.stack")])
    putRcmdr("outputStack", stack)
}

#' @export
popOutput <- function(keep=FALSE){
    stack <- getRcmdr("outputStack")
    lastOutput <- stack[[1]]
    if (!keep) putRcmdr("outputStack", c(stack[-1], NA))
    lastOutput
}

pushCommand <- function(element) {
    stack <- getRcmdr("commandStack")
    stack <- c(list(element), stack[-getRcmdr("length.command.stack")])
    putRcmdr("commandStack", stack)
}

#' @export
popCommand <- function(keep=FALSE){
    stack <- getRcmdr("commandStack")
    lastCommand <- stack[[1]]
    if (!keep) putRcmdr("commandStack", c(stack[-1], NA))
    lastCommand
}

# handle model capabilities

mergeCapabilities <- function(allCapabilities){
    allCapabilities <- allCapabilities[!sapply(allCapabilities, is.null)]
    allrows <- unlist(lapply(allCapabilities, rownames))
    if (length(allrows) > length(unique(allrows))) 
        stop(gettextRcmdr("redundant model class or classes in plug-in package model capabilities table"))
    capabilities <- lapply(allCapabilities, names)
    all <- unique(unlist(capabilities))
    for (i in 1:length(allCapabilities)) allCapabilities[[i]][, setdiff(all, capabilities[[i]])] <- FALSE
    do.call(rbind, allCapabilities)
}

# optionally open graphics device(s)

openGraphicsDevices <- function(){
  if (!getRcmdr("open.graphics.devices")) return()
  dev.new()
  if (getRcmdr("use.rgl")) {
    Library("rgl")
    if (requireNamespace("rgl")) rgl::open3d()
  }
}

#' @name Commander
#' 
#' @aliases Commander
#'
#' @title R Commander
#'
#' @author John Fox
#'
#' @keywords package
#'
#' @seealso \link{Plugins}, \link{Rcmdr.Utilities}, \link[knitr]{knit}, \link[knitr]{knit2pdf}, (\link{Commander-es} en español)
#' 
#' @usage Commander()
#'
#' @description
#' Start the R Commander GUI (graphical user interface)
#'
#' @section Getting Started:
#'
#' For more detailed information about getting started, see \emph{Help -> Introduction to the R Commander} from the R Commander menus or Fox (2017).
#'
#' The default R Commander interface consists of (from top to bottom) a menu bar, a toolbar, a code window with script and R Markdown tabs, an output window, and a messages window.
#'
#' Commands to read, write, transform, and analyze data are entered using the menus in the menu bar at the top of the \emph{Commander} window.
#' Most menu items lead to dialog boxes requesting further specification.
#' I suggest that you explore the menus to see what is available.
#'
#' Below the menu bar is a toolbar with (from left to right) an information field displaying the name of the active data set; buttons for editing and displaying the active data set; and an information field showing the active statistical model.
#' There is also a \emph{Submit} button for re-executing commands in the Script tab.
#' The information fields for the active data set and active model are actually buttons that can be used to select the active data set and model from among, respectively, data frames or suitable model objects in memory.
#'
#' Almost all commands require an active data set.
#' When the Commander starts, there is no active data set, as indicated in the data set information field.
#' A data set becomes the active data set when it is read into memory from an R package or imported from a text file, SPSS data set, Minitab data set,  STATA data set, SAS XPORT data set; or an Excel spreadsheet.
#' In addition, the active data set can be selected from among R data frames resident in memory. You can therefore switch among data sets during a session.
#'
#' By default, commands are logged to the Script tab (the initially empty text window immediately below the toolbar), and commands and output appear in the Output window (the initially empty text window below the Script tab).
#' Commands that don't require direct user interaction (such as interactive identification of points on a graph) are also used to create an R Markdown document in the tab of the same name.
#' When the R Markdown tab is in front, pressing the "Generate HTML report" button compiles the document to create an html page with input and output, which opens in a web browser.
#' To alter these and other defaults, see the information below on configuration.
#' Note, for example, that the \pkg{knitr} package can be used to create a LaTeX document to be compiled to a PDF report, as an alternative to --- or in addition to --- an R Markdown document (see the \code{use.knitr} option below).
#'
#' Some \pkg{Rcmdr} dialogs (those in the \emph{Statistics -> Fit models} menu) produce linear, generalized linear, or other models.
#' When a model is fit, it becomes the active model, as indicated in the information field in the R Commander toolbar.
#' Items in the \emph{Models} menu apply to the active model. Initially, there is no active model.
#' If there are several models in memory, you can select the active model from among them.
#'
#' If command logging in turned on, R commands that are generated from the menus and dialog boxes are entered into the Script and R Markdown tabs in the Commander.
#' You can edit these commands in the normal manner and can also type new commands.
#' You can also type explanatory text in the R Markdown tab.
#' Individual commands in the Script tab can be continued over more than one line, but the several lines of a multi-line command must be submitted simultaneously.
#' (It is not necessary, as in earlier versions of the R Commander, to begin continuation lines with white space.)
#' The contents of the Script and R Markdown tabs can be saved during or at the end of the session, and a saved script or R Markdown document can be loaded into the respective tabs.
#' The contents of the Output window can also be edited or saved to a text file.
#' Finally, editing operations also work in the Messages window.
#'
#' To re-execute a command or set of commands in the Script tab, select the lines to be executed using the mouse and press the \emph{Submit} button at the right of the toolbar (or \emph{Control-R}, for "run", or \emph{Control-Tab}).
#' If no text is selected, the \emph{Submit} button (or \emph{Control-R} or \emph{Control-Tab}) submits the line containing the text-insertion cursor.
#' Note that an error will be generated if the submitted command or commands are incomplete.
#'
#' Pressing \emph{Control-F} brings up a find-text dialog box (which can also be accessed via \emph{Edit -> Find}) to search for text in the Script tab, R Markdown tab, knitr tab, Output window, or Messages window.
#' Edit functions such as search are performed in the Script tab unless you first click in another tab or window to make it active.
#'
#' Pressing \emph{Control-S} will save the Script tab, R Markdown tab, knitr tab, or Output window.
#'
#' Pressing \emph{Control-A} selects all of the text in the Script tab, R Markdown tab, knitr tab, Output window, or Messages window.
#'
#' In addition, the following Control-key combinations work in these tabs and windows: \emph{Control-X}, cut; \emph{Control-C}, copy; \emph{Control-V}, insert; \emph{Control-Z} or \emph{Alt-Backspace}, undo; and \emph{Control-W}, redo.
#'
#' Under Mac OS X, the \emph{command} key may be used in place of the \emph{Control} key, though the latter works as well.
#'
#' Right-clicking the mouse (clicking button 3 on a three-button mouse, or \emph{Control}-left-clicking) in the tabs or windows brings up a "context" menu with the \emph{Edit}-menu items, plus (in the Script, R Markdown, and knitr tabs) a \emph{Submit} item.
#'
#' You can open a larger editor window with the document in the Markdown or knitr tab by making the corresponding selection from the \emph{Edit} menu, the right-click context menu when the cursor is in the tab, or by pressing \emph{Control-E} when the cursor is in the tab.
#'
#' When you execute commands from the \emph{Commander} window, you must ensure that the sequence of commands is logical.
#' For example, it makes no sense to fit a statistical model to a data set that has not been read into memory.
#'
#' Pressing a letter key (e.g., "a") in a list box will scroll the list box to bring the next entry starting with that letter to the top of the box.
#'
#' You can cancel an R Commander dialog box by pressing the \emph{Esc} key.
#'
#' Most R Commander dialogs remember their state when this is appropriate, and can be restored to pristine state by pressing the Reset button.
#'
#' Some R Commander dialogs have an Apply button that will execute the command generated by the dialog and then re-open the dialog in its previous state.
#'
#' Exit from the Commander via the \emph{File -> Exit} menu or by closing the \emph{Commander} window.
#'
#' @section Customization and Configuration:
#'
#' The preferred way of customizing the R Commander is to write a plug-in package: see \code{help("\link{Plugins}")}.
#'
#' Alternatively, configuration files reside in the \code{etc} subdirectory of the package, or in the locations given by the \code{etc} and \code{etcMenus} options (see below).
#'
#' The \pkg{Rcmdr} menus can be customized by editing the file \code{Rcmdr-menus.txt}.
#'
#' You can add R code to the package, e.g., for creating additional dialogs, by placing files with file type \code{.R} in the \code{etc} directory, also editing \code{Rcmdr-menus.txt} to provide additional menus, sub-menus, or menu-items.
#' Alternatively, you can edit the source package and recompile it.
#'
#' To reiterate, however, the preferred procedure is to write an R Commander plug-in package.
#'
#' A number of functions are provided to assist in writing dialogs, and \pkg{Rcmdr} state information is stored in a separate environment.
#' See \code{help("\link{Rcmdr.Utilities}")} and the manual supplied in the \code{doc} directory of the \pkg{Rcmdr} package for more information.
#'
#' In addition, several features are controlled by run-time options, set via the \code{options("Rcmdr")} command.
#' These options should be set before the package is loaded.
#' If the options are unset, which is the usual situation, defaults are used.
#' Specify options as a list of \emph{name=value} pairs.
#' You can set none, one, several, or all options. The available options are as follows:
#'
#' \describe{
#'    \item{\code{ask.to.exit}}{if \code{TRUE} (the default), then the user is asked whether he or she wants to exit the \pkg{Rcmdr}; if this option is set to \code{FALSE}, then the subsequent option is also set to \code{FALSE}.}
#'
#'    \item{\code{ask.on.exit}}{if \code{TRUE} (the default), then the user is asked whether to save the script file, R Markdown file, and output file when the \pkg{Rcmdr} exits.}
#'
#'    \item{\code{attach.data.set}}{if \code{TRUE} (the default is \code{FALSE}), the active data set is attached to the search path.}
#'
#'    \item{\code{check.packages}}{if \code{TRUE} (the default), on start-up, the presence of all of the \pkg{Rcmdr} recommended packages will be checked, and if any are absent, the \pkg{Rcmdr} will offer to install them.}
#'
#'    \item{\code{command.text.color}}{Color for commands in the output window; the default is \code{"red"}.}
#'
#'    \item{\code{console.output}}{If \code{TRUE}, output is directed to the \emph{R Console}, and the \emph{R Commander} output window is not displayed.
#'          The default is \code{FALSE}, unless the R Commander is running under RStudio, in which case the default is \code{TRUE}.}
#'
#'    \item{\code{crisp.dialogs}}{If \code{TRUE}, dialogs should appear on the screen fully drawn, rather than built up  widget by widget.
#'          Prior to R 2.6.1, this option only works on the Windows version of R, but should in any event be harmless.
#'          The default is \code{TRUE}.
#'          If you encounter stability problems, try setting this option to \code{FALSE}.}
#'
#'    \item{\code{default.contrasts}}{Serves the same function as the general \code{contrasts} option; the default is \code{c("contr.Treatment", "contr.poly")}.
#'          When the Commander exits, the \code{contrasts} option is returned to its pre-existing value.
#'          Note that \code{contr.Treatment} is from the \code{car} package.}
#'
#'    \item{\code{default.font.family}}{The default font for GUI elements such as menus and text labels, in the form of a Tk font family specification, given in a character string.
#'          For example,  \code{"Helvetica"} specifies the sans-serif Helvetica font family.
#'          The default is taken from the \code{TkDefaultFont}. Normally a sans-serif font should be used.}
#'
#'    \item{\code{default.font.size}}{The size, in points, of the default font.
#'          The default is 10 on non-Windows system and the size of the system font on Windows.
#'          To set the font size for R input and output, see the \code{log.font.size} option.
#'          The \pkg{Rcmdr} \code{scale.factor} option may also be used to control font size.}
#'
#'    \item{\code{discreteness.theshold}}{should be a positive integer; if greater than \code{0} (which is the default), the  maximum number of distinct values for a numeric variable to be considered discrete; if \code{0} (or smaller), the threshold is taken as the smallest of 100, twice the squareroot of the number of cases in the active data set (n), and 10 times log10(n).}
#'
#'    \item{\code{double.click}}{Set to \code{TRUE} if you want a double-click of the left mouse button to press the default button in all dialogs. The default is \code{FALSE}.}
#'
#'    \item{\code{editDataset.threshold}}{If the number of values in the current data set exceed this value (the default is 10000), then the standard R data editor is used in preference to the R Commander \code{editDataset} editor.}
#'
#'    \item{\code{error.text.color}}{Color for error messages; the default is \code{"red"}.}
#'
#'    \item{\code{etc}}{Set to the path of the directory containing the \pkg{Rcmdr} configuration files; defaults to the \code{etc} subdirectory of the installed \pkg{Rcmdr} package.}
#'
#'    \item{\code{grab.focus}}{Set to \code{TRUE} for the current Tk window to "grab" the focus --- that is, to prevent the focus from being changed to another Tk window.
#'          On some systems, grabbing the focus in this manner apparently causes problems. The default is \code{TRUE}.
#'          If you experience focus problems, try setting this option to \code{FALSE}.}
#'
#'    \item{\code{help_type}}{This Rcmdr option takes precedence over the global R \code{help_type} option (see \code{\link{options}} and \code{\link{help}}), and by default is set to \code{"html"}.}
#'
#'    \item{\code{iconify.commander}}{If \code{TRUE}, the \emph{Commander} window is minimized on startup; the default is \code{FALSE}.}
#'
#'    \item{\code{length.output.stack}}{The R Commander maintains a list of output objects, by default including the last  several outputs; the default length of the output stack is 10. \code{popOutput()} ``pops'' (i.e., returns and removes) the first entry of the output stack.
#'          Note that, as a stack, the queue is LIFO (``last in, first out'').}
#'
#'    \item{\code{length.command.stack}}{The R Commander also maintains a list of commands that is managed similarly; the default length of this stack is also 10.}
#'
#'    \item{\code{log.commands}}{If \code{TRUE} (the default), commands are echoed to the script window; if \code{FALSE}, the script window is not displayed.}
#'
#'    \item{\code{log.font.family}}{The font family to be used for text in the script window, output window, messages window, etc., specified as a character vector giving a Tk font family. This should normally be a monospaced font like \code{"Courier"}.
#'          The default is taken from the \code{TkFixedFont}.}
#'
#'    \item{\code{log.font.size}}{The font size, in points, to be used in the script window, in the output window, messages window, in recode dialogs, and in compute expressions --- that is, where a monospaced font is used.
#'          The default is 10.
#'          Alternatively the \pkg{Rcmdr} \code{scale factor} option may also be used to control font size.}
#'
#'    \item{\code{log.height}}{The height of the script window, in lines.
#'          The default is 10.
#'          Setting \code{log.height} to 0 has the same effect as setting \code{log.commands} to \code{FALSE}.}
#'
#'    \item{\code{log.text.color}}{Color for text in the script window; the default is \code{"black"}.}
#'
#'    \item{\code{log.width}}{The width of the script and output windows, in characters.
#'          The default is 80.}
#'
#'    \item{\code{messages.height}}{The height of the messages window, in lines.
#'          The default is 4.}
#'
#'    \item{\code{model.case.deletion}}{if \code{TRUE} (the default is \code{FALSE}), include a text box for case deletion in statistical-model dialog boxes (e.g., for linear models).}
#'
#'    \item{\code{minimum.width}}{The minimum width, in pixels, for the main R Commander windows; the default is \code{1000}.}
#'
#'    \item{\code{minimum.height}}{The minimum height, in pixels, for the main R Commander windows; the default is \code{400}.}
#'
#'    \item{\code{multiple.select.mode}}{Affects the way multiple variables are selected in variable-list boxes.
#'          If set to \code{"extended"} (the default), left-clicking on a variable selects it and deselects any other variables that are selected; Control-left-click toggles the selection (and may be used to select additional variables); Shift-left-click extends the selection.
#'          This is the standard Windows convention.
#'          If set to \code{"multiple"}, left-clicking toggles the selection of a variable and may be used to select more than one variable.
#'          This is the behaviour in the \pkg{Rcmdr} prior to version 1.9-10.}
#'
#'    \item{\code{number.messages}}{If \code{TRUE}, the default, messages in the messages window are numbered.}
#'
#'    \item{\code{open.graphics.devices}}{If \code{TRUE} (the default is \code{FALSE}), open the system graphics device and (if 3D RGL graphics are used) the RGL graphics device when the R Commander starts.}
#'
#'    \item{\code{open.markdown.editor}}{If \code{TRUE} (the default is \code{FALSE}), open the R Markdown editor when the R Commander starts.}
#'
#'    \item{\code{output.height}}{The height of the output window, in lines.
#'          The default is twice the height of the script window, or 20 if the script window is suppressed. Setting \code{output.height} to 0 has the same effect as setting \code{console.output} to \code{TRUE}.}
#'
#'    \item{\code{output.text.color}}{Color for output in the output window; the default is \code{"blue"}.}
#'
#'    \item{\code{placement}}{Placement of the \emph{R Commander} window, in pixels; the default is \code{""}, which lets the Tk window manager decide where to place the window; for example, \code{"+20+20"} should put the window near the upper-left corner of the screen, \code{"-20+20"} near the upper-right corner, though this doesn't appear to work reliably on Windows systems.}
#'
#'    \item{\code{plugins}}{A character vector giving the names of \pkg{Rcmdr} plug-in packages to load when the Commander starts up.
#'          Plug-in packages can also be loaded from the \emph{Tools -> Load Rcmdr plug-in(s)} menu.
#'          See \link{Plugins}.}
#'
#'    \item{\code{prefixes}}{A four-item character vector to specify the prefixes used when output is directed to the R console; the default is \code{c("Rcmdr> ", "Rcmdr+ ", "RcmdrMsg: ", "RcmdrMsg+ ")}.}
#'
#'    \item{\code{quit.R.on.close}}{if \code{TRUE}, both the Commander and R are exited when the Commander window is closed.
#'          The default is \code{FALSE}, in which case only the Commander is exited (and can be restarted by the command \code{Commander()}).}
#'
#'    \item{\code{RcmdrEnv.on.path}}{If \code{TRUE} (the default is \code{FALSE}), the environment in which R Commander state information is stored is placed on the search path.
#'          Some plug-ins, at least until they are updated, may require this setting.}
#'
#'    \item{\code{retain.messages}}{If \code{TRUE} (the default), the contents of the message window are not erased between messages.
#'          In any event, a "NOTE" message will not erase a preceding "WARNING" or "ERROR".}
#'
#'     \item{\code{retain.selections}}{If \code{TRUE} (the default), dialogs remember their previous state, where appropriate, as long as the data set isn't changed; some dialogs, e.g., for probabilities, retain selections even when the data set chanages.}
#'
#'    \item{\code{RExcelSupport}}{If \code{TRUE} (the default is \code{FALSE}), menus and output are handled by Excel.}
#'
#'    \item{\code{rmarkdown.output}}{Values of several options for converting R Markdown to a document file.
#'          The default for this option is \code{TRUE}, which corresponds to \code{markdown.output=list(command.sections=TRUE, section.level=3, toc=TRUE, toc_float=TRUE, toc_depth=3, number_sections=FALSE, translate.rmd.headers=TRUE)}.
#'          The sub-option \code{command.sections} controls whether most R commands produce sections in the R Markdown document; the sub-option \code{section.level} controls the level of the sections that are created; the sub-option \code{translate.rmd.headers} controls whether the headers are translated from English into another language, if a translation is available; and the other sub-options are standard for \code{\link[rmarkdown]{rmarkdown}}.
#'          The \code{toc_float}, \code{toc_depth}, and \code{number_sections} sub-options are only effective if Pandoc is installed.}
#'
#'    \item{\code{rmd.output.format}}{The output file type for R Markdown documents if pandoc is installed; one of \code{"html"} (the default), \code{"pdf"} (requires LaTeX), \code{"docx"} (Word), or \code{"rtf"} (rich text file).}
#'
#'    \item{\code{rmd.template}}{The quoted path to a \code{.Rmd} file to serve as a template for R code and output. The default is to use a template included with the package.}
#'
#'    \item{\code{scale.factor}}{A scaling factor to be applied to all Tk elements, such as fonts.
#'          This works well only in Windows.
#'          The default is \code{NULL}.}
#'
#'    \item{\code{scientific.notation}}{Higher numbers cause ordinary (decimal) notation to be increasingly preferred to scientific notation for representing very small and very large numbers; correspond to the \code{scipen} option in R: see \code{\link{options}}.
#'          The default is \code{5}, while the standard default in R is \code{0} (where 0 means that scientific notation is used whenever the resulting printed representation of a number is smaller in scientific than in standard notation).}
#'
#'    \item{\code{showData.threshold}}{a vector with 2 entries, defaulting to \code{c(20000, 100)}.
#'          If the number of cases in the active data set exceeds the first number (default, 20,000) or the number of variables exceeds the second number (default, 100), then \code{View()} rather than \code{showData()} is used to display the data set.
#'          The reason for the option is that \code{showData()} is very slow when the number of cases or variables is large; setting the threshold to \code{c(0, 0)} suppresses the use of \code{showData} altogether.
#'          It's necessary to use \code{showData} however for the view of the active data set to be updated dynamically when, e.g., a variable is added.}
#'
#'    \item{\code{show.edit.button}}{Set to \code{TRUE} (the default) if you want an \emph{Edit} button in the Commander window, permitting you to edit the active data set.
#'          Windows users may wish to set this option to \code{FALSE} to suppress the \emph{Edit} button because changing variable names in the data editor can cause R to crash (though I believe that this problem as been solved).}
#'
#'    \item{\code{sort.names}}{Set to \code{TRUE} (the default) if you want variable names to be sorted alphabetically in variable lists.}
#'
#'    \item{\code{suppress.icon.images}}{Set to \code{TRUE} to suppress the icon images in dialog OK, Cancel, Reset, and Help buttons; the default is \code{FALSE}.}
#'
#'    \item{\code{suppress.menus}}{if \code{TRUE}, the Commander menu bar and tool bar are suppressed, allowing another program (such as Excel) to take over these functions. The default (of course) is \code{FALSE}.}
#'
#'    \item{\code{suppress.X11.warnings}}{On (some?) Linux and Mac OS X systems, multiple X11 warnings are generated by \pkg{Rcmdr} commands after a graphics-device window has been opened.
#'          Set this option to \code{TRUE} (the default when running interactively under X11) to suppress reporting of these warnings.
#'          An undesirable side effect is that then \emph{all} warnings and error messages are intercepted by the \pkg{Rcmdr}, even those for commands entered at the R command prompt.
#'          Messages produced by such commands will be printed in the Commander Messages window after the next \pkg{Rcmdr}-generated command.
#'          Some X11 warnings may be printed when you exit from the Commander.}
#'
#'    \item{\code{theme}}{A ttk theme to control the overall style of the Commander GUI; should be one of the themes returned by \code{tcltk2::tk2theme.list()}.
#'          The default theme varies by operating system, and can be discovered by entering the command \code{tcltk2::tk2theme()} in a fresh R session.}
#'
#'    \item{\code{title.color}}{Color for the titles of some widgets, such as variable-list boxes; can be given as a color name, such as \code{"blue"} or as an RGB value, such as \code{"#0000FF"}.
#'          The default is the standard color for ttk label frames, unless that is \code{"#000000"} or \code{"black"}, in which case \code{"blue"} is used instead.}
#'
#'    \item{\code{tkwait.commander}}{This option addresses a problem that, to my knowledge, is rare, and may occur on some non-Windows systems. If the Commander causes R to hang, then set the \code{tkwait} option to \code{TRUE}; otherwise set the option to \code{FALSE} or ignore it.
#'          An undesirable side effect of setting the \code{tkwait.commander} option to \code{TRUE} is that the R session command prompt is suppressed until the Commander exits.
#'          One can still enter commands via the script window, however.
#'          In particular, there is no reason to use this option under Windows, and it should not be used with the Windows R GUI with buffered output when output is directed to the R console.}
#'
#'    \item{\code{tkwait.dialog}}{If \code{TRUE} (the default is \code{FALSE}), R will wait until an R Commander dialog is closed.
#'          This has the disadvantage of preventing help pages from being displayed until a dialog is closed in the Mac OS X R.app and in RStudio.
#'          This was also the standard behavior of the R Commander in earlier versions and is provided for compatibility with previous behavior.
#'          If this option is \code{TRUE}, then the R Commander data editor is disabled in favor of the standard R platform-specific data editor, and the new-data-set menu item is suppressed.}
#'
#'    \item{\code{use.knitr}}{If \code{TRUE} (the default is \code{FALSE}), a knitr \code{.Rnw} LaTeX document is created in a tab of the main Commander window; this document can be compiled into \code{.tex} and \code{.pdf} reports via the \code{\link[knitr]{knit2pdf}} function in the \pkg{knitr} package.}
#'
#'    \item{\code{use.markdown}}{If \code{TRUE} (the default is the negation of the \code{use.knitr} argument), an R Markdown document is created, which can be compiled into an HTML, PDF, Word, or rich text file report.}
#'
#'    \item{\code{use.rgl}}{If \code{TRUE} (the default), the \code{rgl} package will be loaded if it is present in an accessible library; if \code{FALSE}, the \code{rgl} package will be ignored even if it is available.
#'          The \code{rgl} package can sometimes cause problems when running R under X11.}
#'
#'    \item{\code{"valid.classes"}}{The classes of variables that the R Commander recognizes, in addition to numeric data; other variables in a data set will be suppressed.
#'          The default is \code{"factor", "ordered", "character", "logical", "POSIXct", "POSIXlt", "Date", "chron", "yearmon", "yearqtr", "zoo", "zooreg", "timeDate", "xts", "its", "ti", "jul", "timeSeries", "fts", "Period", "hms", "difftime")}.}
#'
#'    \item{\code{variable.list.height}}{the number of items (typically variables) to display in list boxes; longer lists may be viewed by scrolling.
#'          The default is 6.}
#'
#'    \item{\code{variable.list.width}}{a two-item vector controlling the width of list boxes, in characters, giving the minimum and maximum width to display; the default is \code{c(20, Inf)}.
#'          If the widest item name falls in this range, then its number of characters determines the width of the box. \emph{Note:} This specification works only approximately.}
#'
#'    \item{\code{warning.text.color}}{Color for warning messages; the default is \code{"darkgreen"}.}
#' }
#'
#' Some options can also be set via the \emph{File -> Options} menu, which will restart the Commander after options are set.
#'
#' If you want always to launch the R Commander when R starts up, you can include the following code in one of R's start-up files (e.g., in the \code{Rprofile.site} file in R's \code{etc} subdirectory):
#'
#' \preformatted{
#' local({
#'    old <- getOption("defaultPackages")
#'    options(defaultPackages = c(old, "Rcmdr"))
#' })
#' }
#'
#' R Commander options can also be permanently set in the same manner.
#' For more information about R initialization, see \code{?Startup}.
#'
#' @section Warning:
#'
#' The R Commander Script window does not provide a true console to R, and may have certain limitations.
#' I don't recommend using the R Commander for serious programming or for data analysis that relies primarily on scripts --- use a programming editor instead.
#' If you encounter any problems with the Script tab, however, I'd appreciate it if you brought them to my attention.
#'
#' @note
#' On startup, the R Commander sets \code{options(na.action=na.exclude)}; this is done so that observation statistics such as residuals can be properly added to the active data set when there are missing values.
#' The option is reset to its pre-existing value when the Commander exits.
#' Some functions may not work properly when the default \code{na.action} is set to \code{na.exclude}.
#'
#' This version should be compatiable with the \pkg{RExcel} package, which can use the R Commander menus.
#'
#' @section Platform-Specific Issues:
#'
#' Under Windows, the \pkg{Rcmdr} package can be run under the \emph{Rgui} in the SDI (single-document interface) mode, or under \code{rterm.exe}.
#' You might experience problems running the \pkg{Rcmdr} under \code{ESS} with NTEmacs or XEmacs, or under other R consoles.
#' The R Commander can be run under the \emph{Rgui} in MDI (multiple-document interface) mode but it is relatively inconvenient to do so and isn't recommended.
#'
#' Occasionally, under Windows, after typing some text into a dialog box (e.g., a subsetting expression in the Subset Data Set dialog), buttons in the dialog (e.g., the OK button) will have no effect when they are pressed.
#' Clicking anywhere inside or outside of the dialog box should restore the function of the buttons.
#' As far as I have been able to ascertain, this is a problem with Tcl/Tk for Windows.
#' I have not seen this behavior in some time and the problem may have been solved.
#'
#' Under Mac OS X Mavericks and later, the R Commander may appear to freeze or hesitate when run under \emph{R.app} if the \emph{R.app} window is hidden and "app nap" is turned on.
#' It is recommended that app nap be turned off for \emph{R.app}, which can be most conveniently done via the R Commander \emph{Tools} menu.
#' The app nap setting is permanent until changed and so the current setting will apply whether or not the R Commander is used.
#' When R is first installed, app nap will be on for \emph{R.app}.
#' The \pkg{tcltk} package requires that X Windows is installed under Mac OS X, and as a consequence the \pkg{Rcmdr} package, which depends on \pkg{tcltk}, will not load if X Windows is absent.
#' X Windows for Mac OS X may be obtained from \url{https://www.xquartz.org/}.
#'
#' @references
#'
#' Fox, J. (2017) \emph{Using the R Commander: A Point-and-Click Interface for R.} Chapman and Hall/CRC Press. \doi{10.18637/jss.v075.b03}
#'
#' Fox, J. (2005) \emph{The R Commander: A Basic Statistics Graphical User Interface to R.} Journal of Statistical Software, \bold{14(9)}: 1--42. \doi{10.18637/jss.v014.i09}
#'
#' Fox, J. (2007) \emph{Extending the R Commander by "plug in" packages.} R News, \bold{7(3)}: 46--52. \url{https://cran.r-project.org/doc/Rnews/Rnews_2007-3.pdf}.
#' 


NULL
