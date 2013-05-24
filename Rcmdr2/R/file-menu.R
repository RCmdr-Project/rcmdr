# last modified 2013-05-23 by J. Fox
#  applied patch to improve window behaviour supplied by Milan Bouchet-Valat 2011-09-22

# File menu dialogs

loadLog <- function(){
	logFile <- tclvalue(tkgetOpenFile(filetypes=gettextRcmdr('{"All Files" {"*"}} {"Script Files" {".R"}}'),
					defaultextension="log",
					parent=CommanderWindow()))
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

saveLog <- function(logfilename) {
	.logFileName <- if (missing(logfilename)) getRcmdr("logFileName") else logfilename
	if (is.null(.logFileName) || (.logFileName == "%logfilename")) {
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
	logFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"All Files" {"*"}} {"Script Files" {".R"}}'),
					defaultextension="R",
					initialfile="RCommander.R",
					parent=CommanderWindow()))
	if (logFile == "") return()
	log <- tclvalue(tkget(LogWindow(), "1.0", "end"))
	fileCon <- file(logFile, "w")
	cat(log, file = fileCon)
	close(fileCon)
	putRcmdr("logFileName", logFile)
	Message(paste(gettextRcmdr("Script saved to"), logFile), type="note")
}

loadRmd <- function(){
    RmdFile <- tclvalue(tkgetOpenFile(filetypes=gettextRcmdr('{"All Files" {"*"}} {"R Markdown Files" {".Rmd" ".rmd"}}'),
        defaultextension="Rmd",
        parent=CommanderWindow()))
    if (RmdFile == "") return()
    fileCon <- file(RmdFile, "r")
    contents <- readLines(fileCon)
    close(fileCon)
    currentRmdFileName <- getRcmdr("RmdFileName")
    putRcmdr("RmdFileName", RmdFile)
    .rmd <- RmdWindow()
    if (tclvalue(tkget(.rmd, "1.0", "end")) != "\n"){
        response2 <- RcmdrTkmessageBox(message=gettextRcmdr("Save current Rmd file?"),
            icon="question", type="yesno", default="yes")
        if ("yes" == tclvalue(response2)) saveLog(currentRmdFileName)
    }
    tkdelete(.rmd, "1.0", "end")
    tkinsert(.rmd, "end", paste(contents, collapse="\n"))
}

saveRmd <- function(Rmdfilename) {
    .RmdFileName <- if (missing(Rmdfilename)) getRcmdr("RmdFileName") else Rmdfilename
    if ((.RmdFileName == "RcmdrMarkdown.Rmd") || 
            is.null(.RmdFileName) || 
            (.RmdFileName == "%Rmdfilename")) {
        saveRmdAs()
        return()
    }
    .rmd <- tclvalue(tkget(RmdWindow(), "1.0", "end"))
    fileCon <- file(.RmdFileName, "w")
    cat(.rmd, file = fileCon)
    close(fileCon)
    Message(paste(gettextRcmdr("R Markdown file saved to"), .RmdFileName), type="note")
}

saveRmdAs <- function() {
    RmdFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"All Files" {"*"}} {"R Markdown Files" {".Rmd" ".rmd"}}'),
        defaultextension="Rmd",
        initialfile="RCommanderMarkdown.Rmd",
        parent=CommanderWindow()))
    if (RmdFile == "") return()
    .rmd <- tclvalue(tkget(RmdWindow(), "1.0", "end"))
    fileCon <- file(RmdFile, "w")
    cat(.rmd, file = fileCon)
    close(fileCon)
    putRcmdr("RmdFileName", RmdFile)
    Message(paste(gettextRcmdr("R Markdown file saved to"), RmdFile), type="note")
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
	outputFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"All Files" {"*"}} {"Output Files" {".txt"}}'),
					defaultextension="txt",
					initialfile="RCommander.txt",
					parent=CommanderWindow()))
	if (outputFile == "") return()
	output <- tclvalue(tkget(OutputWindow(), "1.0", "end"))
	fileCon <- file(outputFile, "w")
	cat(output, file = fileCon)
	close(fileCon)
	putRcmdr("outputFileName", outputFile)
	Message(paste(gettextRcmdr("Output saved to"), outputFile), type="note")
}

saveWorkspaceAs <- function(){
	saveFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"All Files" {"*"}} {"R Data Files" {".RData" ".rda" ".Rda" ".RDA"}}'),
					defaultextension="",
					initialfile=".RData",
					parent=CommanderWindow()))
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

CloseCommander <- function() closeCommander(ask=getRcmdr("ask.to.exit"), ask.save=getRcmdr("ask.on.exit"))

closeCommander <- function(ask=TRUE, ask.save=ask){
	if (ask){
		response <- tclvalue(RcmdrTkmessageBox(message=gettextRcmdr("Exit?"),
						icon="question", type="okcancel", default="cancel"))
		if (response == "cancel") return(invisible(response))
	}
	else {
		ask.save=FALSE
		response <- "ok"
	}
	sink(type="message")
	if (!is.null(ActiveDataSet()) && getRcmdr("attach.data.set"))
		justDoIt(logger(paste("detach(", ActiveDataSet(), ")", sep="")))
	putRcmdr(".activeDataSet", NULL)
	putRcmdr(".activeModel", NULL)
	if (ask.save && getRcmdr("log.commands") && tclvalue(tkget(LogWindow(), "1.0", "end")) != "\n"){
		response2 <- RcmdrTkmessageBox(message=gettextRcmdr("Save script file?"),
				icon="question", type="yesno", default="yes")
		if ("yes" == tclvalue(response2)) saveLog()
	}
    
	if (ask.save && getRcmdr("markdown.output") && getRcmdr("log.commands") && tclvalue(tkget(RmdWindow(), "1.0", "end")) != "\n"){
	    response2 <- RcmdrTkmessageBox(message=gettextRcmdr("Save R Markdown file?"),
	                                   icon="question", type="yesno", default="yes")
	    if ("yes" == tclvalue(response2)) saveRmd()
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
    options(help_type = getRcmdr("restore.help_type"))
	tkdestroy(CommanderWindow())
	putRcmdr("commanderWindow", NULL)
	putRcmdr("logWindow", NULL)
    putRcmdr("RmdWindow", NULL)
	putRcmdr("messagesWindow", NULL)
	putRcmdr("outputWindow", NULL)
	options(getRcmdr("quotes"))
	tkwait <- options("Rcmdr")[[1]]$tkwait  # to address problem in Debian Linux
	if ((!is.null(tkwait)) && tkwait) putRcmdr(".commander.done", tclVar("1"))
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
        if (!is.null(current[[option]])) return(current[[option]])
        else if (!is.null(getRcmdr(option, fail=FALSE))) return(getRcmdr(option))
        return(default)
    }
    asLogical <- function(x) as.logical(as.numeric(x))
    initializeDialog(title=gettextRcmdr("Commander Options"))
    notebook <- ttknotebook(top)
    closeTab <- tkframe(top)
    fontTab <- tkframe(top)
    outputTab <- tkframe(top)
    otherTab <- tkframe(top)
    current <- options("Rcmdr")[[1]]
    console.output <- getRcmdr("console.output")
    log.commands <- getRcmdr("log.commands")
    log.font.size <- getRcmdr("log.font.size")
    log.width <- setOption("log.width", 80)
    log.height <- if (!is.null(current$log.height)) current$log.height
    else if (!log.commands) 0 else 10
    output.height <- if (!is.null(current$output.height)) current$output.height
    else if (console.output) 0 else 2*log.height
    contrasts <- setOption("default.contrasts", c("contr.Treatment", "contr.poly"))
    grab.focus <- getRcmdr("grab.focus")
    double.click <- getRcmdr("double.click")
    sort.names <- getRcmdr("sort.names")
    show.edit.button <- setOption("show.edit.button", TRUE)
    scale.factor <- current$scale.factor
    default.font.size.val <- as.numeric(regmatches(tclvalue(tkfont.actual("TkDefaultFont")),
                                                   regexec("-size (-?[[:digit:]]+)", tclvalue(tkfont.actual("TkDefaultFont"))))[[1]][2])
    if (is.na(default.font.size.val)) default.font.size.val <- 10
    default.font.size <- setOption("default.font.size", default.font.size.val)
    suppress.icon.images <- getRcmdr("suppress.icon.images")
    number.messages <- getRcmdr("number.messages")
    etc <- getRcmdr("etc")
    etcMenus <- getRcmdr("etcMenus")
    log.font <- tclvalue(tkfont.actual("RcmdrLogFont"))
    title.color <- getRcmdr("title.color")
    use.markdown<- getRcmdr("use.markdown")
    retain.selections <- getRcmdr("retain.selections")
    messages.height <- as.character(getRcmdr("messages.height"))
    ask.to.exit <- getRcmdr("ask.to.exit")
    ask.on.exit <- getRcmdr("ask.on.exit")
    attach.data.set <- getRcmdr("attach.data.set")
    log.text.color <- getRcmdr("log.text.color")
    command.text.color <- getRcmdr("command.text.color")
    output.text.color <- getRcmdr("output.text.color")
    error.text.color <- getRcmdr("error.text.color")
    warning.text.color <- getRcmdr("warning.text.color")
    prefixes <- getRcmdr("prefixes")
    multiple.select.mode <- getRcmdr("multiple.select.mode")
    suppress.X11.warnings <- getRcmdr("suppress.X11.warnings")
    showData.threshold <- getRcmdr("showData.threshold")
    retain.messages <- getRcmdr("retain.messages")
    crisp.dialogs <- getRcmdr("crisp.dialogs")
    length.output.stack <- getRcmdr("length.output.stack")
    length.command.stack <- getRcmdr("length.command.stack")
    quit.R.on.close <- getRcmdr("quit.R.on.close")
    variable.list.height <- getRcmdr("variable.list.height")
    variable.list.width <- getRcmdr("variable.list.width")
    default.font <- tclvalue(tkfont.actual("RcmdrDefaultFont"))
    placement <- setOption("placement", "")
    suppress.menus <- getRcmdr("suppress.menus")
    rmd.template <- getRcmdr("rmd.template")
    rmd.standard <- system.file("etc", "Rcmdr-Markdown-Template.Rmd", package="Rcmdr")
    use.rgl <- setOption("use.rgl", TRUE)
    checkBoxes(closeTab, frame="closeOptionsFrame", boxes=c("askToExit", "askOnExit", "quitR"),
               initialValues=c(ask.to.exit, ask.on.exit, quit.R.on.close),
               labels=gettextRcmdr("Ask to exit Commander", "Ask to save documents on exit", "Quit R on exit"))
    checkBoxes(outputTab, frame="outputOptionsFrame", 
               boxes=c("consoleOutput", "logCommands", "numberMessages", "retainMessages", "useMarkdown"),
               initialValues=c(console.output, log.commands, number.messages, retain.messages, use.markdown),
               labels=gettextRcmdr("Send output to R Console", "Log commands to script window", "Number messages", 
                                   "Retain messages", "Use R Markown"))
    logTextColorVar <- tclVar(log.text.color)
    logTextColorEntry <- ttkentry(fontTab, width="15", textvariable=logTextColorVar)
    commandTextColorVar <- tclVar(command.text.color)
    commandTextColorEntry <- ttkentry(fontTab, width="15", textvariable=commandTextColorVar)
    outputTextColorVar <- tclVar(output.text.color)
    outputTextColorEntry <- ttkentry(fontTab, width="15", textvariable=outputTextColorVar)
    errorTextColorVar <- tclVar(error.text.color)
    errorTextColorEntry <- ttkentry(fontTab, width="15", textvariable=errorTextColorVar)
    warningTextColorVar <- tclVar(warning.text.color)
    warningTextColorEntry <- ttkentry(fontTab, width="15", textvariable=warningTextColorVar)
    titleColorVar <- tclVar(title.color)
    titleColorEntry <- ttkentry(fontTab, width="15", textvariable=titleColorVar)
    logFontSizeVar <- tclVar(log.font.size)
    logFontSizeSlider <- tkscale(fontTab, from=6, to=20, showvalue=TRUE, variable=logFontSizeVar,
                                 resolution=1, orient="horizontal")
    logWidthVar <- tclVar(log.width)
    logWidthSlider <- tkscale(outputTab, from=30, to=120, showvalue=TRUE, variable=logWidthVar,
                              resolution=5, orient="horizontal")
    logHeightVar <- tclVar(log.height)
    logHeightSlider <- tkscale(outputTab, from=0, to=25, showvalue=TRUE, variable=logHeightVar,
                               resolution=1, orient="horizontal")
    outputHeightVar <- tclVar(output.height)
    outputHeightSlider <- tkscale(outputTab, from=0, to=50, showvalue=TRUE, variable=outputHeightVar,
                                  resolution=5, orient="horizontal")
    
    messagesHeightVar <- tclVar(messages.height)
    messagesHeightSlider <- tkscale(outputTab, from=0, to=10, showvalue=TRUE, variable=messagesHeightVar,
                                    resolution=1, orient="horizontal")       
    contrasts1 <- tclVar(contrasts[1])
    contrasts2 <- tclVar(contrasts[2])
    contrastsFrame <- tkframe(otherTab)
    contrasts1Entry <- ttkentry(contrastsFrame, width="15", textvariable=contrasts1)
    contrasts2Entry <- ttkentry(contrastsFrame, width="15", textvariable=contrasts2)
    checkBoxes(otherTab, frame="otherOptionsFrame", 
               boxes=c("grabFocus", "doubleClick", "sortNames", "showEditButton", "SuppressIconImages",
                       "retainSelections", "useRgl"),
               initialValues=c(grab.focus, double.click, sort.names, show.edit.button, suppress.icon.images,
                               retain.selections, use.rgl),
               labels=gettextRcmdr("Active window grabs focus", "Double-click presses OK button", 
                                   "Sort variable names alphabetically", "Show edit button",
                                   "Suppress icon images", "Retain dialog selections", "Use rgl package")
    )
    scaleFactorVar <- tclVar(if (is.null(scale.factor)) 1.0 else scale.factor)
    scaleFactorSlider <- tkscale(otherTab, from=0.2, to=3.0, showvalue=TRUE, variable=scaleFactorVar,
                                 resolution=0.2, orient="horizontal")
    defaultFontSizeVar <- tclVar(default.font.size)
    defaultFontSizeSlider <- tkscale(fontTab, from=6, to=20, showvalue=TRUE, variable=defaultFontSizeVar,
                                     resolution=1, orient="horizontal")
    logFontVar <- tclVar(log.font)
    defaultFontVar <- tclVar(default.font)
    logFontEntry <- ttkentry(fontTab, width="75", textvariable=logFontVar)
    defaultFontEntry <- ttkentry(fontTab, width="75", textvariable=defaultFontVar)
    rmdTemplateVar <- tclVar(rmd.template)
    rmdTemplateEntry <- ttkentry(outputTab, width="75", textvariable=rmdTemplateVar)
    onSelectTemplate <- function(){
        templateFile <- tclvalue(tkgetOpenFile(filetypes=gettextRcmdr('{"All Files" {"*"}} {"R Markdown Files" {".Rmd" ".rmd"}}'),
                                               defaultextension=".Rmd",
                                               parent=outputTab))
        if (templateFile == "") return()
        tclvalue(rmdTemplateVar) <- templateFile
        return(NULL)
    }
    templateButton <- buttonRcmdr(outputTab, text=gettextRcmdr("Select file"), command=onSelectTemplate)
    onOK <- function(){
        closeDialog(top)
        ask.to.exit <- asLogical(tclvalue(askToExitVariable))
        ask.on.exit <- asLogical(tclvalue(askOnExitVariable))
        quit.R.on.close <- asLogical(tclvalue(quitRVariable))
        console.output <- asLogical(tclvalue(consoleOutputVariable))
        number.messages <- asLogical(tclvalue(numberMessagesVariable))
        retain.messages <- asLogical(tclvalue(retainMessagesVariable))
        use.markdown <- asLogical(tclvalue(useMarkdownVariable))
        rmd.template <- tclvalue(rmdTemplateVar)
        if (rmd.template == rmd.standard) rmd.template <- NULL
        log.font <- tclvalue(logFontVar)
        default.font <- tclvalue(defaultFontVar)
        log.font.size <- round(as.numeric(tclvalue(logFontSizeVar)))
        log.width <- round(as.numeric(tclvalue(logWidthVar)))
        log.height <- as.numeric(tclvalue(logHeightVar))
        log.commands <- asLogical(tclvalue(logCommandsVariable)) && (log.height != 0)
        output.height <- as.numeric(tclvalue(outputHeightVar))
        console.output <- asLogical(tclvalue(consoleOutputVariable)) || (output.height == 0)
        contrasts <- c(tclvalue(contrasts1), tclvalue(contrasts2))
        grab.focus <- asLogical(tclvalue(grabFocusVariable))
        double.click <- asLogical(tclvalue(doubleClickVariable))
        sort.names <- asLogical(tclvalue(sortNamesVariable))
        show.edit.button <- asLogical(tclvalue(showEditButtonVariable))
        suppress.icon.images <- asLogical(tclvalue(SuppressIconImagesVariable))
        retain.selections <- asLogical(tclvalue(retainSelectionsVariable))
        use.rgl <- asLogical(tclvalue(useRglVariable))
        scale.factor <- round(as.numeric(tclvalue(scaleFactorVar)), 1)
        if (scale.factor == 1) scale.factor <- NULL
        default.font.size <- tclvalue(defaultFontSizeVar)
        options <- current
        options$ask.to.exit <- ask.to.exit
        options$ask.on.exit <- ask.on.exit
        options$quit.R.on.close <- quit.R.on.close
        options$number.messages <- number.messages
        options$retain.messages <- retain.messages
        options$use.markdown <- use.markdown
        options$rmd.template <- rmd.template
        options$log.font <- log.font
        options$default.font <- default.font
        options$log.font.size <- log.font.size
        options$log.width <- log.width
        options$log.height <- log.height
        options$log.commands <- log.commands
        options$output.height <- output.height
        options$console.output <- console.output
        options$default.contrasts <- contrasts
        options$grab.focus <- grab.focus
        options$double.click <- double.click
        options$sort.names <- sort.names
        options$show.edit.button <- show.edit.button
        options$suppress.icon.images <- suppress.icon.images
        options$retain.selections <- retain.selections
        options$use.rgl <- use.rgl
        if (.Platform$OS.type == "windows") options$scale.factor <- scale.factor
        options$default.font.size <- default.font.size
        options(Rcmdr=options)
        closeCommander()
        Commander()
    }
    OKCancelHelp(helpSubject="Commander")
    tkgrid(closeOptionsFrame, sticky="nw")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Dialog text font size (points)")), defaultFontSizeSlider, sticky="sw")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Script and output font size (points)")), logFontSizeSlider, sticky="sw")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Dialog font")), defaultFontEntry, sticky="w")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Script and output font")), logFontEntry, sticky="w")
    tkgrid(labelRcmdr(fontTab, text=""))
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Script text color ")), logTextColorEntry, sticky="w")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Command text color ")), commandTextColorEntry, sticky="w")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Output text color ")), outputTextColorEntry, sticky="w")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Error text color ")), errorTextColorEntry, sticky="w")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Warning text color ")), warningTextColorEntry, sticky="w")
    tkgrid(labelRcmdr(fontTab, text=gettextRcmdr("Dialog subtitles text color ")), titleColorEntry, sticky="w")
    tkgrid(labelRcmdr(outputTab, text=gettextRcmdr("Script window width (characters)")), logWidthSlider, sticky="sw")
    tkgrid(labelRcmdr(outputTab, text=gettextRcmdr("Script window height (lines)")), logHeightSlider, sticky="sw")
    tkgrid(labelRcmdr(outputTab, text=gettextRcmdr("Output window height (lines)")), outputHeightSlider, sticky="sw")
    tkgrid(labelRcmdr(outputTab, text=gettextRcmdr("Messages window height (lines)")), messagesHeightSlider, sticky="sw")
    tkgrid(labelRcmdr(outputTab, text=" "), sticky="w")    
    tkgrid(outputOptionsFrame, sticky="nw", columnspan = 3)
    tkgrid(labelRcmdr(outputTab, text="R Markdown template file"), rmdTemplateEntry, templateButton, sticky="w")
    if (.Platform$OS.type == "windows"){
        tkgrid(labelRcmdr(otherTab, text=gettextRcmdr("Scale factor for Tk elements")), scaleFactorSlider, sticky="sw")
    }
    tkgrid(labelRcmdr(contrastsFrame, text=gettextRcmdr("Unordered factors")), labelRcmdr(contrastsFrame, text="   "),
           labelRcmdr(contrastsFrame, text=gettextRcmdr("Ordered factors")), sticky="w")
    tkgrid(contrasts1Entry, labelRcmdr(contrastsFrame, text="   "), contrasts2Entry, sticky="w")
    tkgrid(labelRcmdr(otherTab, text=gettextRcmdr("Contrasts")), contrastsFrame, sticky="sw")
    tkgrid(labelRcmdr(otherTab, text=" "), sticky="w")    
    tkgrid(otherOptionsFrame, sticky="w", columnspan=2)
    tkadd(notebook, closeTab, text=gettextRcmdr("Exit"), padding=6)
    tkadd(notebook, fontTab, text=gettextRcmdr("Fonts"), padding=6)
    tkadd(notebook, outputTab, text=gettextRcmdr("Output"), padding=6)
    tkadd(notebook, otherTab, text=gettextRcmdr("Other Options"), padding=6)
    tkgrid(notebook)
    tkconfigure(OKbutton, text=gettextRcmdr("Exit and Restart\nR Commander"), width=18)
    tkgrid(buttonsFrame, columnspan=3, sticky="ew")
    dialogSuffix(rows=11, columns=3)
}

saveOptions <- function(){
    initializeDialog(title=gettextRcmdr("Save Commander Options"))
    onCopy <- function(){
        focused <- tkfocus()
        if (tclvalue(focused) != optionsWindow$ID) focused <- optionsWindow
        selection <- strsplit(tclvalue(tktag.ranges(focused, "sel")), " ")[[1]]
        if (is.na(selection[1])) return()
        text <- tclvalue(tkget(focused, selection[1], selection[2]))
        tkclipboard.clear()
        tkclipboard.append(text)
    }
    onDelete <- function(){
        focused <- tkfocus()
        if (tclvalue(focused) != optionsWindow$ID) focused <- optionsWindow
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
        if (tclvalue(focused) != optionsWindow$ID) focused <- optionsWindow
        text <- tclvalue(.Tcl("selection get -selection CLIPBOARD"))
        if (length(text) == 0) return()
        tkinsert(focused, "insert", text)
    }
    onFind <- function(){
        focused <- tkfocus()
        if (tclvalue(focused) != optionsWindow$ID) focused <- optionsWindow
        initializeDialog(title=gettextRcmdr("Find"))
        textFrame <- tkframe(top)
        textVar <- tclVar("")
        textEntry <- ttkentry(textFrame, width="20", textvariable=textVar)
        checkBoxes(frame="optionsFrame", boxes=c("regexpr", "case"), initialValues=c("0", "1"),
                   labels=gettextRcmdr(c("Regular-expression search", "Case sensitive")))
        radioButtons(name="direction", buttons=c("foward", "backward"), labels=gettextRcmdr(c("Forward", "Backward")),
                     values=c("-forward", "-backward"), title=gettextRcmdr("Search Direction"))
        onOK <- function(){
            text <- tclvalue(textVar)
            if (text == ""){
                errorCondition(recall=onFind, message=gettextRcmdr("No search text specified."))
                return()
            }
            type <- if (tclvalue(regexprVariable) == 1) "-regexp" else "-exact"
            case <- tclvalue(caseVariable) == 1
            direction <- tclvalue(directionVariable)
            stop <- if (direction == "-forward") "end" else "1.0"
            where <- if (case) tksearch(focused, type, direction, "--", text, "insert", stop)
            else tksearch(focused, type, direction, "-nocase", "--", text, "insert", stop)
            where <- tclvalue(where)
            if (where == "") {
                Message(message=gettextRcmdr("Text not found."),
                        type="note")
                if (GrabFocus()) tkgrab.release(top)
                tkdestroy(top)
                tkfocus(CommanderWindow())
                return()
            }
            if (GrabFocus()) tkgrab.release(top)
            tkfocus(focused)
            tkmark.set(focused, "insert", where)
            tksee(focused, where)
            tkdestroy(top)
        }
        OKCancelHelp()
        tkgrid(labelRcmdr(textFrame, text=gettextRcmdr("Search for:")), textEntry, sticky="w")
        tkgrid(textFrame, sticky="w")
        tkgrid(optionsFrame, sticky="w")
        tkgrid(directionFrame, sticky="w")
        tkgrid(buttonsFrame, sticky="w")
        dialogSuffix(rows=4, columns=1, focus=textEntry)
    }
    onSelectAll <- function() {
        focused <- tkfocus()
        if (tclvalue(focused) != optionsWindow$ID) focused <- optionsWindow
        tktag.add(focused, "sel", "1.0", "end")
        tkfocus(focused)
    }
    onClear <- function(){
        onSelectAll()
        onDelete()
    }
    onUndo <- function(){
        focused <- tkfocus()
        if (tclvalue(focused) != optionsWindow$ID) focused <- optionsWindow
        tcl(focused, "edit", "undo")
    }
    onRedo <- function(){
        focused <- tkfocus()
        if (tclvalue(focused) != optionsWindow$ID) focused <- optionsWindow
        tcl(focused, "edit", "redo")
    }
    onOK <- function(){
        optionsFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"All Files" {"*"}}'),
                                              initialfile=".Rprofile",
                                              parent=top))
        if (optionsFile == "") return()
        options <- tclvalue(tkget(optionsWindow, "1.0", "end"))
        fileCon <- file(optionsFile, "w")
        cat(options, file = fileCon)
        close(fileCon)
        closeDialog(top)
        Message(paste(gettextRcmdr("R Profile saved to"), optionsFile), type="note")
    }
    contextMenu <- function(){
        contextMenu <- tkmenu(tkmenu(optionsWindow), tearoff=FALSE)
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
        tkpopup(contextMenu, tkwinfo("pointerx", optionsWindow), tkwinfo("pointery", optionsWindow))
    }
    optionsFrame <- tkframe(top)    
    optionsWindow <- tktext(optionsFrame, bg="white", foreground=getRcmdr("log.text.color"),
                            font=getRcmdr("logFont"), height=20, width=65, wrap="none", undo=TRUE)
    optionsXscroll <- ttkscrollbar(optionsFrame, orient="horizontal",
                                   command=function(...) tkxview(optionsWindow, ...))
    optionsYscroll <- ttkscrollbar(optionsFrame,
                                   command=function(...) tkyview(optionsWindow, ...))
    tkconfigure(optionsWindow, xscrollcommand=function(...) tkset(optionsXscroll, ...))
    tkconfigure(optionsWindow, yscrollcommand=function(...) tkset(optionsYscroll, ...))
    tkbind(top, "<Control-x>", onCut)
    tkbind(top, "<Control-X>", onCut)
    tkbind(top, "<Control-c>", onCopy)
    tkbind(top, "<Control-C>", onCopy)
    tkbind(top, "<Control-f>", onFind)
    tkbind(top, "<Control-F>", onFind)
    tkbind(top, "<Control-a>", onSelectAll)
    tkbind(top, "<Control-A>", onSelectAll)
    tkbind(top, "<Control-w>", onRedo)
    tkbind(top, "<Control-W>", onRedo)
    tkbind(top, "<Alt-BackSpace>", onUndo)
    tkbind(optionsWindow, "<ButtonPress-3>", contextMenu)
    OKCancelHelp(helpSubject="saveOptions")
    menu <- tkmenu(top)
    tkconfigure(top, menu=menu)
    editMenu <- tkmenu(menu, tearoff=FALSE)
    tkadd(editMenu, "command", label=gettextRcmdr("Cut"), command=onCut)
    tkadd(editMenu, "command", label=gettextRcmdr("Copy"), command=onCopy)
    tkadd(editMenu, "command", label=gettextRcmdr("Paste"), command=onPaste)
    tkadd(editMenu, "command", label=gettextRcmdr("Delete"), command=onDelete)
    tkadd(editMenu, "separator")
    tkadd(editMenu, "command", label=gettextRcmdr("Find..."), command=onFind)
    tkadd(editMenu, "command", label=gettextRcmdr("Select all"), command=onSelectAll)
    tkadd(editMenu, "separator")
    tkadd(editMenu, "command", label=gettextRcmdr("Undo"), command=onUndo)
    tkadd(editMenu, "command", label=gettextRcmdr("Redo"), command=onRedo)
    tkadd(editMenu, "separator")
    tkadd(editMenu, "command", label=gettextRcmdr("Clear window"), command=onClear)  
    tkadd(menu, "cascade", label=gettextRcmdr("Edit"), menu=editMenu)
    Rprofile <- if (file.exists(".Rprofile")) readLines(".Rprofile") else ""
    start <- grep("^###! Rcmdr Options Begin !###", Rprofile)
    end <- grep("^###! Rcmdr Options End !###", Rprofile)
    if (length(start) == 1 && length(end) == 1){
        Rprofile <- Rprofile[-(start:end)]
    }
    Rprofile <- sub("\\n*$", "", Rprofile)
    tkinsert(optionsWindow, "end", paste(Rprofile, collapse="\n"))
    options <- getOption("Rcmdr")
    con <- file(open="w+")
    dput(options, con)
    options <- readLines(con)
    close(con)
    options <- paste(c("", "",
                       "###! Rcmdr Options Begin !###",
                       "options(Rcmdr=",
                       options,
                       ")",
                       "",
                       "# Uncomment the following 4 lines (remove the #s)",
                       "#  to start the R Commander automatically when R starts:",
                       "",
                       "# local({",
                       "#    old <- getOption('defaultPackages')",
                       "#    options(defaultPackages = c(old, 'Rcmdr'))",
                       "# })",
                       "",
                       "###! Rcmdr Options End !###"),
                     collapse="\n"
    )
    tkinsert(optionsWindow, "end", options)
    tkgrid(labelRcmdr(top, text=
                          paste(gettextRcmdr("The following commands will be saved in the file .Rprofile.",
                                             "You may edit this file before saving it."), collapse="\n")),
           sticky="w")
    tkgrid(optionsWindow, optionsYscroll, sticky="news")
    tkgrid(optionsXscroll, sticky="ew", columnspan=2)
    tkgrid(optionsFrame, sticky="news", padx=10, pady=0)
    tkgrid(buttonsFrame, sticky="ew")
    dialogSuffix(rows=2, columns=1)
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
			Library(package)
		}
		Message(paste(gettextRcmdr("Packages loaded:"), paste(packages, collapse=", ")), type="note")
	}
	OKCancelHelp(helpSubject="library")
	tkgrid(getFrame(packagesBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=1, columns=1)
}

Setwd <- function(){
	wd <- tclvalue(tkchooseDirectory(initialdir=getwd(), parent=CommanderWindow()))
	if (wd != "") doItAndPrint(paste('setwd("', wd, '")', sep=""))
}

