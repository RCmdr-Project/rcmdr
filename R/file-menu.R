# last modified 31 July 2008 by J. Fox

# File menu dialogs

loadLog <- function(){
    logFile <- tclvalue(tkgetOpenFile(filetypes=gettextRcmdr('{"Script Files" {".R"}} {"All Files" {"*"}}'),
        defaultextension="log"))
    if (logFile == "") return()
    fileCon <- file(logFile, "r")
    contents <- readLines(fileCon)
    close(fileCon)
    currentLogFileName <- getRcmdr("logFileName")
    putRcmdr("logFileName", logFile)
    .log <- LogWindow()
    if (tclvalue(tkget(.log, "1.0", "end")) != "\n"){
        response2 <- RcmdrTkmessageBox(message=gettextRcmdr("Save current log file?"),
                icon="question", type="yesno", default="yes")
        if ("yes" == tclvalue(response2)) saveLog(currentLogFileName)
        }
    tkdelete(.log, "1.0", "end")
    tkinsert(.log, "end", paste(contents, collapse="\n"))
    }

saveLog <- function() {
    .logFileName <- getRcmdr("logFileName")
    if (is.null(.logFileName)) {
        saveLogAs()
        return()
        }
    log <- tclvalue(tkget(LogWindow(), "1.0", "end"))
    fileCon <- file(.logFileName, "w")
    cat(log, file = fileCon)
    close(fileCon)
    Message(paste(gettextRcmdr("Script saved to"), .logFileName), type="note")
    }

saveLogAs <- function() {
    logFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"Script Files" {".R"}} {"All Files" {"*"}}'),
        defaultextension="R", initialfile="RCommander.R"))
    if (logFile == "") return()
    log <- tclvalue(tkget(LogWindow(), "1.0", "end"))
    fileCon <- file(logFile, "w")
    cat(log, file = fileCon)
    close(fileCon)
    putRcmdr("logFileName", logFile)
     Message(paste(gettextRcmdr("Script saved to"), logFile), type="note")
    }

saveOutput <- function() {
    .outputFileName <- getRcmdr("outputFileName")
    if (is.null(.outputFileName)) {
        saveOutputAs()
        return()
        }
    output <- tclvalue(tkget(OutputWindow(), "1.0", "end"))
    fileCon <- file(.outputFileName, "w")
    cat(output, file = fileCon)
    close(fileCon)
    Message(paste(gettextRcmdr("Output saved to"), .outputFileName), type="note")
    }

saveOutputAs <- function() {
    outputFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"Output Files" {".txt"}} {"All Files" {"*"}}'),
        defaultextension="txt", initialfile="RCommander.txt"))
    if (outputFile == "") return()
    output <- tclvalue(tkget(OutputWindow(), "1.0", "end"))
    fileCon <- file(outputFile, "w")
    cat(output, file = fileCon)
    close(fileCon)
    putRcmdr("outputFileName", outputFile)
    Message(paste(gettextRcmdr("Output saved to"), outputFile), type="note")
    }

saveWorkspaceAs <- function(){
    saveFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"All Files" {"*"}}'),
        defaultextension="", initialfile=".RData"))
    if (saveFile == "") return()
    save(list=ls(envir=.GlobalEnv), file=saveFile)
    putRcmdr("saveFileName", saveFile)
    Message(paste(gettextRcmdr("R workspace saved to"), saveFile), type="note")
    }

saveWorkspace <- function() {
    .saveFileName <- getRcmdr("saveFileName")
    if (is.null(.saveFileName)) {
        saveWorkspaceAs()
        return()
        }
    else save(list=ls(envir=.GlobalEnv), file=.saveFileName)
    Message(paste(gettextRcmdr("R workspace saved to"), .saveFileName), type="note")
    }

CloseCommander <- function() closeCommander(ask.save=getRcmdr("ask.on.exit"))

closeCommander <- function(ask=TRUE, ask.save=ask){
    if (ask){
        response <- tclvalue(RcmdrTkmessageBox(message=gettextRcmdr("Exit?"),
            icon="question", type="okcancel", default="cancel"))
        if (response == "cancel") return(invisible(response))
        }
    sink(type="message")
    if (rglLoaded()) rgl.quit()
    if (!is.null(ActiveDataSet()) && getRcmdr("attach.data.set"))
        justDoIt(logger(paste("detach(", ActiveDataSet(), ")", sep="")))
    putRcmdr(".activeDataSet", NULL)
    putRcmdr(".activeModel", NULL)
    if (ask.save && getRcmdr("log.commands") && tclvalue(tkget(LogWindow(), "1.0", "end")) != "\n"){
         response2 <- RcmdrTkmessageBox(message=gettextRcmdr("Save script file?"),
                 icon="question", type="yesno", default="yes")
         if ("yes" == tclvalue(response2)) saveLog()
         }
    if (ask.save && !getRcmdr("console.output") && tclvalue(tkget(OutputWindow(), "1.0", "end")) != "\n"){
         response3 <- RcmdrTkmessageBox(message=gettextRcmdr("Save output file?"),
                 icon="question", type="yesno", default="yes")
         if ("yes" == tclvalue(response3)) saveOutput()
         }
    if (.Platform$OS.type != "windows") options(getRcmdr("oldPager"))
    if (getRcmdr("suppress.X11.warnings")) {
        sink(type = "message")
        close(getRcmdr("messages.connection"))
    }
    options(getRcmdr("saveOptions"))
    tkdestroy(CommanderWindow())
    putRcmdr("commanderWindow", NULL)
    putRcmdr("logWindow", NULL)
    putRcmdr("messagesWindow", NULL)
    putRcmdr("outputWindow", NULL)
    options(getRcmdr("quotes"))
    tkwait <- options("Rcmdr")[[1]]$tkwait  # to address problem in Debian Linux
    if ((!is.null(tkwait)) && tkwait) tclvalue(.commander.done) <<- "1"
    return(invisible(response))
    }

closeCommanderAndR <- function(){
    response <- CloseCommander()
    if (response == "cancel") return()
    cat("\n")
    quit(save="no")
    }

Options <- function(){
    setOption <- function(option, default) {
        if (is.null(current[[option]])) default else current[[option]]
        }
    initializeDialog(title=gettextRcmdr("Commander Options"))
    current <- options("Rcmdr")[[1]]
    console.output <- setOption("console.output", FALSE)
    log.commands <- setOption("log.commands", TRUE)
    log.font.size <- setOption("log.font.size", 10)
    log.width <- setOption("log.width", 80)
    log.height <- if (!is.null(current$log.height)) current$log.height
                    else if (!log.commands) 0 else 10
    output.height <- if (!is.null(current$output.height)) current$output.height
        else if (console.output) 0 else 2*log.height
    contrasts <- setOption("contrasts", c("contr.Treatment", "contr.poly"))
    grab.focus <- setOption("grab.focus", TRUE)
    double.click <- setOption("double.click", FALSE)
    sort.names <- setOption("sort.names", TRUE)
    show.edit.button <- setOption("show.edit.button", TRUE)
    scale.factor <- current$scale.factor
    default.font.size <- setOption("default.font.size",
        if (.Platform$OS.type != "windows") 12 else 10)
    default.font <- setOption("default.font",
        paste("*helvetica-medium-r-normal-*-", default.font.size, "*", sep=""))
    consoleOutputVar <- tclVar(console.output)
    consoleOutputCheckBox <- tkcheckbutton(top, variable=consoleOutputVar)
    logCommandsVar <- tclVar(log.commands)
    logCommandsCheckBox <- tkcheckbutton(top, variable=logCommandsVar)
    logFontSizeVar <- tclVar(log.font.size)
    logFontSizeSlider <- tkscale(top, from=6, to=20, showvalue=TRUE, variable=logFontSizeVar,
        resolution=1, orient="horizontal")
    logWidthVar <- tclVar(log.width)
    logWidthSlider <- tkscale(top, from=30, to=120, showvalue=TRUE, variable=logWidthVar,
        resolution=5, orient="horizontal")
    logHeightVar <- tclVar(log.height)
    logHeightSlider <- tkscale(top, from=0, to=25, showvalue=TRUE, variable=logHeightVar,
        resolution=1, orient="horizontal")
    outputHeightVar <- tclVar(output.height)
    outputHeightSlider <- tkscale(top, from=0, to=50, showvalue=TRUE, variable=outputHeightVar,
        resolution=5, orient="horizontal")
    contrasts1 <- tclVar(contrasts[1])
    contrasts2 <- tclVar(contrasts[2])
    contrastsFrame <- tkframe(top)
    contrasts1Entry <- ttkentry(contrastsFrame, width="15", textvariable=contrasts1)
    contrasts2Entry <- ttkentry(contrastsFrame, width="15", textvariable=contrasts2)
    grabFocusVar <- tclVar(as.numeric(grab.focus))
    grabFocusCheckBox <- tkcheckbutton(top, variable=grabFocusVar)
    doubleClickVar <- tclVar(as.numeric(double.click))
    doubleClickCheckBox <- tkcheckbutton(top, variable=doubleClickVar)
    sortNamesVar <- tclVar(as.numeric(sort.names))
    sortNamesCheckBox <- tkcheckbutton(top, variable=sortNamesVar)
    showEditButtonVar <- tclVar(as.numeric(show.edit.button))
    showEditButtonCheckBox <- tkcheckbutton(top, variable=showEditButtonVar)
    scaleFactorVar <- tclVar(if (is.null(scale.factor)) 1.0 else scale.factor)
    scaleFactorSlider <- tkscale(top, from=0.2, to=3.0, showvalue=TRUE, variable=scaleFactorVar,
        resolution=0.2, orient="horizontal")
    defaultFont <- tclVar(default.font)
    defaultFontEntry <- ttkentry(top, width="30", textvariable=scaleFactorVar)
    onOK <- function(){
        closeDialog(top)
        log.font.size <- round(as.numeric(tclvalue(logFontSizeVar)))
        log.width <- round(as.numeric(tclvalue(logWidthVar)))
        log.height <- as.numeric(tclvalue(logHeightVar))
        log.commands <- as.logical(tclvalue(logCommandsVar) == "1") && (log.height != 0)
        output.height <- as.numeric(tclvalue(outputHeightVar))
        console.output <- as.logical(tclvalue(consoleOutputVar) == "1") || (output.height == 0)
        contrasts <- c(tclvalue(contrasts1), tclvalue(contrasts2))
        grab.focus <- tclvalue(grabFocusVar) == 1
        double.click <- tclvalue(doubleClickVar) == 1
        sort.names <- tclvalue(sortNamesVar) == 1
        show.edit.button <- tclvalue(showEditButtonVar) == 1
        scale.factor <- round(as.numeric(tclvalue(scaleFactorVar)), 1)
        if (scale.factor == 1) scale.factor <- NULL
        default.font <- tclvalue(defaultFont)
		options <- current
		options$log.font.size <- log.font.size
		options$log.width <- log.width
		options$log.height <- log.height
		options$log.commands <- log.commands
		options$output.height <- output.height
		options$console.output <- console.output
		options$contrasts <- contrasts
		options$grab.focus <- grab.focus
		options$double.click <- double.click
		options$sort.names <- sort.names
		options$show.edit.button <- show.edit.button
        if (.Platform$OS.type == "windows") options$scale.factor <- scale.factor
            else options$default.font <- default.font
        options(Rcmdr=options)
        closeCommander()
        Commander()
        }
    OKCancelHelp(helpSubject="Commander")
	if (.Platform$OS.type == "windows"){
		tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale factor for Tk elements")), scaleFactorSlider, sticky="se")
		tkgrid.configure(scaleFactorSlider, sticky="w")
	}
	else {
		tkgrid(labelRcmdr(top, text=gettextRcmdr("Default font")), defaultFontEntry, sticky="e")
		tkgrid.configure(defaultFontEntry, sticky="w")
	}
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Log-font size (points)")), logFontSizeSlider, sticky="se")
    tkgrid.configure(logFontSizeSlider, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Log width (characters)")), logWidthSlider, sticky="se")
    tkgrid.configure(logWidthSlider, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Log height (lines)")), logHeightSlider, sticky="se")
    tkgrid.configure(logHeightSlider, sticky="w")
	tkgrid(labelRcmdr(top, text=gettextRcmdr("Output height (lines)")), outputHeightSlider, sticky="se")
	tkgrid.configure(outputHeightSlider, sticky="w")
    tkgrid(labelRcmdr(top, text=" "), sticky="w")	
	tkgrid(labelRcmdr(top, text=gettextRcmdr("Log commands to script window")), logCommandsCheckBox, sticky="e")
	tkgrid.configure(logCommandsCheckBox, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Send output to R Console")), consoleOutputCheckBox, sticky="e")
    tkgrid.configure(consoleOutputCheckBox, sticky="w")
    tkgrid(labelRcmdr(contrastsFrame, text=gettextRcmdr("Unordered factors")), labelRcmdr(contrastsFrame, text="   "),
        labelRcmdr(contrastsFrame, text=gettextRcmdr("Ordered factors")), sticky="w")
    tkgrid(contrasts1Entry, labelRcmdr(contrastsFrame, text="   "), contrasts2Entry, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Contrasts")), contrastsFrame, sticky="se")
    tkgrid.configure(contrastsFrame, sticky="sw")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Active window grabs focus")), grabFocusCheckBox, sticky="e")
    tkgrid.configure(grabFocusCheckBox, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Double-click presses OK button")), doubleClickCheckBox, sticky="e")
    tkgrid.configure(doubleClickCheckBox, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Sort variable names alphabetically")), sortNamesCheckBox, sticky="e")
    tkgrid.configure(sortNamesCheckBox, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Show edit button")), showEditButtonCheckBox, sticky="e")
    tkgrid.configure(showEditButtonCheckBox, sticky="w")
    tkconfigure(OKbutton, text=gettextRcmdr("Exit and Restart\nR Commander"), width=18)
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=11, columns=2)
    }

loadPackages <- function(){
    availablePackages <- sort(setdiff(.packages(all.available = TRUE), .packages()))
    if (length(availablePackages) == 0){
        errorCondition(message=gettextRcmdr("No packages available to load."))
        return()
        }
    initializeDialog(title=gettextRcmdr("Load Packages"))
    packagesBox <- variableListBox(top, availablePackages, title=gettextRcmdr("Packages (pick one or more)"),
        selectmode="multiple", listHeight=10)
    onOK <- function(){
        packages <- getSelection(packagesBox)
        closeDialog(top)
        if (length(packages) == 0){
            errorCondition(recall=loadPackages, message=gettextRcmdr("You must select at least one package."))
            return()
            }
        for (package in packages) {
            command <- paste('library("', package, '", character.only=TRUE)', sep="")
            justDoIt(command)
            }
        Message(paste(gettextRcmdr("Packages loaded:"), paste(packages, collapse=", ")), type="note")
        }
    OKCancelHelp(helpSubject="library")
    tkgrid(getFrame(packagesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=1, columns=1)
    }
	
Setwd <- function(){
	wd <- tclvalue(tkchooseDirectory(initialdir=getwd()))
	if (wd != "") doItAndPrint(paste('setwd("', wd, '")', sep=""))
	}

