# last modified 2022-06-14 by J. Fox

# Data menu dialogs

newDataSet <- function() {
    initializeDialog(title=gettextRcmdr("New Data Set"))
    dsname <- tclVar("Dataset")
    entryDsname <- ttkentry(top, width="20", textvariable=dsname)
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=newDataSet,
                           message=gettextRcmdr("You must enter the name of a data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=newDataSet,
                           message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                newDataSet()
                return()
            }
        }
        closeDialog()
        command <- paste("editDataset(dsname='", dsnameValue, "')", sep="")
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error"){
            if (!getRcmdr("dataset.modified")) return()
            .data <- try(get(dsnameValue, envir=.GlobalEnv), silent=TRUE)
            if (nrow(.data) == 0){
                errorCondition(recall=newDataSet, message=gettextRcmdr("empty data set."))
                return()
            }
            tempdir <- tempdir()
            tempdir <- gsub("\\\\", "/", tempdir)
            savefile <- paste(tempdir, "/", dsnameValue, sep="")
            save(".data", file=savefile)
            if (getRcmdr("use.markdown")) {
                removeNullRmdBlocks()
                enterMarkdown(paste('load("', savefile, '")', sep=""))
            }
            if (getRcmdr("use.knitr")) {
                removeNullRnwBlocks()
                enterKnitr(paste('load("', savefile, '")', sep=""))
            }
       }
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="editDataset")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
    tkgrid(buttonsFrame, columnspan="2", sticky="w")
    tkgrid.configure(entryDsname, sticky="w")
    dialogSuffix(focus=entryDsname)
}

selectActiveDataSet <- function(){
	dataSets <- listDataSets()
	.activeDataSet <- ActiveDataSet()
	if ((length(dataSets) == 1) && !is.null(.activeDataSet)) {
		Message(message=gettextRcmdr("There is only one dataset in memory."),
				type="warning")
		tkfocus(CommanderWindow())
		return()
	}
	if (length(dataSets) == 0){
		Message(message=gettextRcmdr("There are no data sets from which to choose."),
				type="error")
		tkfocus(CommanderWindow())
		return()
	}
	initializeDialog(title=gettextRcmdr("Select Data Set"))
	dataSetsBox <- variableListBox(top, dataSets, title=gettextRcmdr("Data Sets (pick one)"),
			initialSelection=if (is.null(.activeDataSet)) NULL else which(.activeDataSet == dataSets) - 1)
	onOK <- function(){
		selection <- getSelection(dataSetsBox)
		closeDialog()
		setBusyCursor()
		on.exit(setIdleCursor())
		activeDataSet(selection)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp()
	tkgrid(getFrame(dataSetsBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix()
}

listDataSetsInPackages <- function() doItAndPrint("data()")

RecodeDialog <- function () {
  processRecode <- function(recode) {
    parts <- strsplit(recode, "=")[[1]]
    if (length(grep(",", parts[1])) > 0) 
      paste("c(", parts[1], ") = ", parts[2], sep = "")
    else paste(parts, collapse = "=")
  }
  dataSet <- activeDataSet()
  defaults <- list (initial.asFactor = 1, initial.variables = NULL, initial.name = "variable",
                    initial.recode.directives="", 
                    initial.to.value="=", initial.interval=":", initial.separator=";")
  dialog.values <- getDialog ("RecodeDialog", defaults)
  initializeDialog(title = gettextRcmdr("Recode Variables"))
  variablesBox <- variableListBox(top, Variables(), selectmode = "multiple", 
                                  title = gettextRcmdr("Variables to recode (pick one or more)"),
                                  initialSelection = varPosn (dialog.values$initial.variables, "all"))
  variablesFrame <- tkframe(top)
  newVariableName <- tclVar(dialog.values$initial.name)
  newVariable <- ttkentry(variablesFrame, width = "20", textvariable = newVariableName)
  recodesFrame <- tkframe(top)
  recodes <- tktext(recodesFrame, bg = "white", font = getRcmdr("logFont"), 
                    height = "5", width = "40", wrap = "none")
  recodesXscroll <- ttkscrollbar(recodesFrame, orient = "horizontal", 
                                 command = function(...) tkxview(recodes, ...))
  recodesYscroll <- ttkscrollbar(recodesFrame, command = function(...) tkyview(recodes, 
                                                                               ...))
  tkconfigure(recodes, xscrollcommand = function(...) tkset(recodesXscroll, 
                                                            ...))
  tkconfigure(recodes, yscrollcommand = function(...) tkset(recodesYscroll, 
                                                            ...))
  tkinsert(recodes, "1.0", dialog.values$initial.recode.directives)
  asFactorFrame <- tkframe(top)
  asFactorVariable <- tclVar(dialog.values$initial.asFactor)
  asFactorCheckBox <- ttkcheckbutton(asFactorFrame, variable = asFactorVariable)
  operatorsFrame <- tkframe(top)
  to.valueVariable <- tclVar(dialog.values$initial.to.value)
  to.valueBox <- ttkentry(operatorsFrame, width = "3", textvariable = to.valueVariable)
  intervalVariable <- tclVar(dialog.values$initial.interval)
  intervalBox <- ttkentry(operatorsFrame, width = "3", textvariable = intervalVariable)
  separatorVariable <- tclVar(dialog.values$initial.separator)
  separatorBox <- ttkentry(operatorsFrame, width = "3", textvariable = separatorVariable)
  onOK <- function() {
    asFactor <- tclvalue(asFactorVariable) == "1"
    to.value <- tclvalue(to.valueVariable)
    interval <- tclvalue(intervalVariable)
    separator <- tclvalue(separatorVariable)
    save.recodes <- trim.blanks(tclvalue(tkget(recodes, "1.0", "end")))
    recode.directives <- gsub("\n", paste0(separator, " "), save.recodes)
    check.empty <- gsub(separator, "", gsub(" ", "", recode.directives))
    if ("" == check.empty) {
      errorCondition(recall = RecodeDialog, message = gettextRcmdr("No recode directives specified."))
      return()
    }
    if (0 != length(grep("'", recode.directives))) {
      errorCondition(recall = RecodeDialog, message = gettextRcmdr("Use only double-quotes (\" \") in recode directives"))
      return()
    }
    recode.directives <- strsplit(recode.directives, separator)[[1]]
    recode.directives <- paste(sapply(recode.directives, 
                                      processRecode), collapse = separator)
    recode.directives <- sub(paste0(" *", separator, " *$"), "", recode.directives)
    variables <- getSelection(variablesBox)
    closeDialog()
    if (length(variables) == 0) {
      errorCondition(recall = RecodeDialog, message = gettextRcmdr("You must select a variable."))
      return()
    }
    multiple <- if (length(variables) > 1) 
      TRUE
    else FALSE
    name <- trim.blanks(tclvalue(newVariableName))
    putDialog ("RecodeDialog", list (initial.asFactor = asFactor, initial.variables = variables,
                                     initial.name = name, initial.recode.directives=save.recodes,
                                     initial.to.value=to.value, initial.interval=interval,
                                     initial.separator=separator))
    command <- paste(dataSet, " <- within(", dataSet, ", {", sep="")
    nvar <- length(variables)
    for (i in 1:nvar) {
      variable <- variables[nvar - i + 1]
      newVar <- if (multiple) 
        paste(name, variable, sep = "")
      else name
      if (!is.valid.name(newVar)) {
        errorCondition(recall = RecodeDialog, message = paste("\"", 
                                                              newVar, "\" ", gettextRcmdr("is not a valid name."), 
                                                              sep = ""))
        return()
      }
      if (is.element(newVar, Variables())) {
        if ("no" == tclvalue(checkReplace(newVar))) {
          RecodeDialog()
          return()
        }
      }
      command <- paste(command, "\n  ", newVar, " <- Recode(", variable, ", '", 
                       recode.directives, "', as.factor=", asFactor, 
                       ', to.value="', to.value, '", interval="', interval, 
                       '", separator="', separator, '")', sep = "")  
    }
    command <- paste(command, "\n})", sep="")
    result <- doItAndPrint(command)
    if (class(result)[1] != "try-error")
      activeDataSet(dataSet, flushModel = FALSE, flushDialogMemory = FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "RecodeDialog", reset = "RecodeDialog", apply = "RecodeDialog")
  tkgrid(getFrame(variablesBox), sticky = "nw")
  tkgrid(labelRcmdr(variablesFrame, text = ""))
  tkgrid(labelRcmdr(variablesFrame, text = gettextRcmdr("New variable name or prefix for multiple recodes: ")), 
         newVariable, sticky = "w")
  tkgrid(asFactorCheckBox, labelRcmdr(asFactorFrame, text = gettextRcmdr("Make (each) new variable a factor")), 
         sticky = "w")
  tkgrid(labelRcmdr(operatorsFrame, text = gettextRcmdr("Old/new values assignment character(s)  ")),
         to.valueBox, sticky="w")
  tkgrid(labelRcmdr(operatorsFrame, text = gettextRcmdr("Recode interval character(s)  ")),
         intervalBox, sticky="w")
  tkgrid(labelRcmdr(operatorsFrame, text = gettextRcmdr("Recode separator character(s)  ")),
         separatorBox, sticky="w")
  tkgrid(labelRcmdr(operatorsFrame, text = ""))
  tkgrid(labelRcmdr(recodesFrame, text = gettextRcmdr("Enter recode directives"), 
                    fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w")
  tkgrid(recodes, recodesYscroll, sticky = "nw")
  tkgrid(recodesXscroll)
  tkgrid(variablesFrame, sticky = "w")
  tkgrid(asFactorFrame, sticky = "w")
  tkgrid(operatorsFrame, sticky = "w")
  tkgrid(recodesFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w", columnspan = 2)
  tkgrid.configure(recodesXscroll, sticky = "ew")
  tkgrid.configure(recodesYscroll, sticky = "ns")
  dialogSuffix(bindReturn = FALSE)
}

Compute <- function(){
    onDoubleClick <-function(){
        var <- trim.blanks(getSelection(variablesBox))
        word <- paste("\\[", gettextRcmdr("factor"), "\\]", sep="")
        if (length(grep(word, var)) == 1)
            var <- trim.blanks(sub(word, "",  var))
        tkfocus(compute)
        expr <- tclvalue(computeVar)
        tclvalue(computeVar) <- if (expr == "") var
        else paste(expr, var, sep=if (rev(strsplit(expr, "")[[1]])[1] =="(" ) "" else " ")
        tkicursor(compute, "end")
        tkxview.moveto(compute, "1")
    }
    defaults <- list(initial.name = "variable", initial.expression = "")
    dialog.values <- getDialog("Compute", defaults)
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Compute New Variable"))
    .variables <- Variables()
    variables <- paste(.variables, ifelse(is.element(.variables, Factors()), gettextRcmdr("[factor]"), ""))
    variablesBox <- variableListBox(top, variables, title=gettextRcmdr("Current variables (double-click to expression)"))
    tkbind(variablesBox$listbox, "<Double-ButtonPress-1>", onDoubleClick)
    variablesFrame <- tkframe(top)
    newVariableName <- tclVar(dialog.values$initial.name)
    newVariable <- ttkentry(variablesFrame, width="20", textvariable=newVariableName)
    computeFrame <- tkframe(top)
    computeVar <- tclVar(dialog.values$initial.expression)
    compute <- ttkentry(computeFrame, font=getRcmdr("logFont"), width="30", textvariable=computeVar)
    computeXscroll <- ttkscrollbar(computeFrame,
                                   orient="horizontal", command=function(...) tkxview(compute, ...))
    tkconfigure(compute, xscrollcommand=function(...) tkset(computeXscroll, ...))
    onOK <- function(){
        closeDialog()
        newVar <- trim.blanks(tclvalue(newVariableName))
        if (!is.valid.name(newVar)){
            errorCondition(recall=Compute,
                           message=paste('"', newVar, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        express <- tclvalue(computeVar)
        check.empty <- gsub(";", "", gsub(" ", "", express))
        if ("" == check.empty) {
            errorCondition(recall=Compute,
                           message=gettextRcmdr("No expression specified."))
            return()
        }
        putDialog("Compute", list(initial.name=newVar, initial.expression=express))
        if (is.element(newVar, Variables())) {
            if ("no" == tclvalue(checkReplace(newVar, gettextRcmdr("Variable")))){
                Compute()
                return()
            }
        }
        command <-  paste(dataSet,"$",newVar, " <- with(", ActiveDataSet(),
                          ", ", express, ")", sep="")
        logger(command)
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error") activeDataSet(dataSet, flushModel=FALSE, flushDialogMemory=FALSE)
        else{
            if (getRcmdr("use.markdown")) removeLastRmdBlock()
            if (getRcmdr("use.knitr")) removeLastRnwBlock()
        }
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="Compute", reset = "Compute", apply = "Compute")
    tkgrid(getFrame(variablesBox), sticky="nw", columnspan=2)
    tkgrid(labelRcmdr(variablesFrame, text=gettextRcmdr("New variable name")), sticky="w")
    tkgrid(newVariable, labelRcmdr(variablesFrame, text="     "), sticky="w")
    tkgrid(labelRcmdr(computeFrame, text=gettextRcmdr("Expression to compute")), sticky="w")
    tkgrid(compute, sticky="w")
    tkgrid(computeXscroll, sticky="ew")
    tkgrid(variablesFrame, computeFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    dialogSuffix(focus=compute)
}

deleteVariable <- function(){
  dataSet <- activeDataSet()
  initializeDialog(title=gettextRcmdr("Delete Variables"))
  variablesBox <- variableListBox(top, Variables(),
                                  title=gettextRcmdr("Variable(s) to delete (pick one or more)"), selectmode="multiple",
                                  initialSelection=NULL)
  onOK <- function(){
    variables <- getSelection(variablesBox)
    closeDialog()
    if (length(variables) == 0) {
      errorCondition(recall=deleteVariable, message=gettextRcmdr("You must select one or more variables."))
      return()
    }
    if (length(variables) == 1){
      response <- tclvalue(RcmdrTkmessageBox(message=sprintf(gettextRcmdr("Delete %s?\nPlease confirm."), variables), icon="warning", type="okcancel", default="cancel"))
      if (response == "cancel") {
        onCancel()
        return()
      }
    }
    else{
      response <- tclvalue(RcmdrTkmessageBox(message=
                                               sprintf(gettextRcmdr("Delete %d variables?\nPlease confirm."), length(variables)),
                                             icon="warning", type="okcancel", default="cancel"))
      if (response == "cancel") {
        onCancel()
        return()
      }
    }
    command <- paste(dataSet, " <- within(", dataSet, ", {", sep="")
    for (variable in variables){
      command <- paste(command, "\n  ", variable, " <- NULL", sep="")
    }
    command <- paste(command, "\n})")
    doItAndPrint(command)
    activeDataSet(dataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="NULL")
  tkgrid(getFrame(variablesBox), sticky="nw")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix()
}

readDataSet <- function() {
    initializeDialog(title=gettextRcmdr("Read Text Data From File, Clipboard, or URL"))
    optionsFrame <- tkframe(top)
    dsname <- tclVar("Dataset")
    entryDsname <- ttkentry(optionsFrame, width="20", textvariable=dsname)
    radioButtons(optionsFrame, "location", buttons=c("local", "clipboard", "url"), 
                 labels=gettextRcmdr(c("Local file system", "Clipboard", "Internet URL")), title=gettextRcmdr("Location of Data File"))
    headerVariable <- tclVar("1")
    headerCheckBox <- ttkcheckbutton(optionsFrame, variable=headerVariable)
    stringsAsFactorsVariable <- tclVar("1")
    stringsAsFactorsCheckBox <- ttkcheckbutton(optionsFrame, variable=stringsAsFactorsVariable)
    radioButtons(optionsFrame, "delimiter", buttons=c("whitespace", "commas", "semicolons", "tabs"),
                 labels=gettextRcmdr(c("White space", "Commas [,]", "Semicolons [;]", "Tabs")), title=gettextRcmdr("Field Separator"),
                 columns=2) 
    otherDelimiterFrame <- tkframe(optionsFrame)
    otherButton <- ttkradiobutton(otherDelimiterFrame, variable=delimiterVariable, value="other", text=gettextRcmdr("Other"))
    otherVariable <- tclVar("")
    otherEntry <- ttkentry(otherDelimiterFrame, width="4", textvariable=otherVariable)
    radioButtons(optionsFrame, "decimal", buttons=c("period", "comma"),
                 labels=gettextRcmdr(c("Period [.]", "Comma [,]")), title=gettextRcmdr("Decimal-Point Character"))
    missingVariable <- tclVar("NA")
    missingEntry <- ttkentry(optionsFrame, width="8", textvariable=missingVariable)
    onOK <- function(){
        closeDialog()
        setBusyCursor()
        on.exit(setIdleCursor())
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=readDataSet,
                           message=gettextRcmdr("You must enter a name for the data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=readDataSet,
                           message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                readDataSet()
                return()
            }
        }
        location <- tclvalue(locationVariable)
        file <- if (location == "clipboard") {
            if (MacOSXP()) 'pipe("pbpaste")' else "clipboard" 
        }
        else if (location == "local") tclvalue(tkgetOpenFile(filetypes=
                                                                 gettextRcmdr('{"All Files" {"*"}} {"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}}')))
        else {
            initializeDialog(subdialog, title=gettextRcmdr("Internet URL"))
            onOKsub <- function(){
                closeDialog(subdialog)
            }
            urlFrame <- tkframe(subdialog)
            urlVar <- tclVar("")
            url <- ttkentry(urlFrame, font=getRcmdr("logFont"), width="30", textvariable=urlVar)
            urlXscroll <- ttkscrollbar(urlFrame,
                                       orient="horizontal", command=function(...) tkxview(url, ...))
            tkconfigure(url, xscrollcommand=function(...) tkset(urlXscroll, ...))
            .subexit <- function() tclvalue(urlVar) <- ""
            subOKCancelHelp()
            tkgrid(url, sticky="w")
            tkgrid(urlXscroll, sticky="ew")
            tkgrid(urlFrame, sticky="nw")
            tkgrid(subButtonsFrame, sticky="w")
            dialogSuffix(subdialog, focus=url, onOK=onOKsub, force.wait=TRUE)
            tclvalue(urlVar)
        }
        if (file == "") {
            if (getRcmdr("grab.focus")) tkgrab.release(top)
            tkdestroy(top)
            return()
        }
        head <- tclvalue(headerVariable) == "1"
        stringsAsFactors <- tclvalue(stringsAsFactorsVariable) == "1"
        delimiter <- tclvalue(delimiterVariable)
        del <- if (delimiter == "whitespace") ""
        else if (delimiter == "commas") ","
        else if (delimiter == "semicolons") ";"
        else if (delimiter == "tabs") "\\t"
        else tclvalue(otherVariable)
        miss <- tclvalue(missingVariable)
        dec <- if (tclvalue(decimalVariable) == "period") "." else ","
        if (!(location == "clipboard" && MacOSXP())) file <- paste0('"', file, '"')
        command <- paste("read.table(", file, ", header=", head,
                         ', stringsAsFactors=', stringsAsFactors,
                         ', sep="', del, '", na.strings="', miss, '", dec="', dec, '", strip.white=TRUE)', sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error"){
            gassign(dsnameValue, result)
            activeDataSet(dsnameValue)
        }
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="read.table")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Variable names in file:")), headerCheckBox, sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Convert character variables to factors")), stringsAsFactorsCheckBox, sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Missing data indicator:")), missingEntry, sticky="w")
    tkgrid(locationFrame, sticky="w")
    tkgrid(otherButton, 
           labelRcmdr(otherDelimiterFrame, text=gettextRcmdr("    Specify:")), otherEntry, sticky="ew", padx="3")
    tkgrid(delimiterFrame, sticky="nw", columnspan=2)
    tkgrid(otherDelimiterFrame, sticky="w")
    tkgrid(decimalFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

readDataFromPackage <- function() {
	env <- environment()
	datasets <- NULL
	initializeDialog(title=gettextRcmdr("Read Data From Package"))
	dsname <- tclVar("")
	package <- NULL
	enterFrame <- tkframe(top)
	entryDsname <- ttkentry(enterFrame, width="20", textvariable=dsname)
	packages <- sort(.packages())
	packages <- packages[! packages %in% c("base", "stats")]
	packages <- packages[sapply(packages, function(package){
						ds <- utils::data(package=package)$results
						if (nrow(ds) == 0) return(FALSE)
						ds <- ds[, "Item"]
						# ds <- trim.blanks(sub("\\(.*\\)", "", ds))
						valid <- sapply(ds, is.valid.name)
						length(ds[valid]) > 0
					})]
	packageDatasetFrame <- tkframe(top)
	packageFrame <- tkframe(packageDatasetFrame)
	max.height <- getRcmdr("variable.list.height")
	packageBox <- tklistbox(packageFrame, height=min(max.height, length(packages)), 
            exportselection="FALSE",
			selectmode="single", background="white")
	packageScroll <- ttkscrollbar(packageFrame,
			command=function(...) tkyview(packageBox, ...))
	tkconfigure(packageBox, yscrollcommand=function(...) tkset(packageScroll, ...))
	for (p in packages) tkinsert(packageBox, "end", p)
	datasetFrame <- tkframe(packageDatasetFrame)
	datasetBox <- tklistbox(datasetFrame, height=max.height, exportselection="FALSE",
			selectmode="single", background="white")
	datasetScroll <- ttkscrollbar(datasetFrame,
			command=function(...) tkyview(datasetBox, ...))
	tkconfigure(datasetBox, yscrollcommand=function(...) tkset(datasetScroll, ...))
	onPackageSelect <- function(){
		assign("package", packages[as.numeric(tkcurselection(packageBox)) + 1], envir=env)
		datasets <<- utils::data(package=package)$results[,3]
		valid <- sapply(datasets, is.valid.name)
		datasets <<- datasets[valid]
		tkdelete(datasetBox, "0", "end")
		for (dataset in datasets) tkinsert(datasetBox, "end", dataset)
		tkconfigure(datasetBox, height=min(max.height, length(datasets)))
		firstChar <- tolower(substr(datasets, 1, 1))
		len <- length(datasets)
		onLetter <- function(letter){
			letter <- tolower(letter)
			current <- 1 + round(as.numeric(unlist(strsplit(tclvalue(tkyview(datasetBox) ), " "))[1])*len)
			mat <- match(letter, firstChar[-(1:current)])
			if (is.na(mat)) return()
			tkyview.scroll(datasetBox, mat, "units")
		}
		onA <- function() onLetter("a")
		onB <- function() onLetter("b")
		onC <- function() onLetter("c")
		onD <- function() onLetter("d")
		onE <- function() onLetter("e")
		onF <- function() onLetter("f")
		onG <- function() onLetter("g")
		onH <- function() onLetter("h")
		onI <- function() onLetter("i")
		onJ <- function() onLetter("j")
		onK <- function() onLetter("k")
		onL <- function() onLetter("l")
		onM <- function() onLetter("m")
		onN <- function() onLetter("n")
		onO <- function() onLetter("o")
		onP <- function() onLetter("p")
		onQ <- function() onLetter("q")
		onR <- function() onLetter("r")
		onS <- function() onLetter("s")
		onT <- function() onLetter("t")
		onU <- function() onLetter("u")
		onV <- function() onLetter("v")
		onW <- function() onLetter("w")
		onX <- function() onLetter("x")
		onY <- function() onLetter("y")
		onZ <- function() onLetter("z")
		for (letter in c(letters, LETTERS)){
			tkbind(datasetBox, paste("<", letter, ">", sep=""),
					get(paste("on", toupper(letter), sep="")))
		}
		onClick <- function() tkfocus(datasetBox)
		tkbind(datasetBox, "<ButtonPress-1>", onClick)
	}
	onDatasetSelect <- function(){
		tclvalue(dsname) <- datasets[as.numeric(tkcurselection(datasetBox)) + 1]
	}
	firstChar <- tolower(substr(packages, 1, 1))
	len <- length(packages)
	onLetter <- function(letter){
		letter <- tolower(letter)
		current <- 1 + round(as.numeric(unlist(strsplit(tclvalue(tkyview(packageBox) ), " "))[1])*len)
		mat <- match(letter, firstChar[-(1:current)])
		if (is.na(mat)) return()
		tkyview.scroll(packageBox, mat, "units")
	}
	onA <- function() onLetter("a")
	onB <- function() onLetter("b")
	onC <- function() onLetter("c")
	onD <- function() onLetter("d")
	onE <- function() onLetter("e")
	onF <- function() onLetter("f")
	onG <- function() onLetter("g")
	onH <- function() onLetter("h")
	onI <- function() onLetter("i")
	onJ <- function() onLetter("j")
	onK <- function() onLetter("k")
	onL <- function() onLetter("l")
	onM <- function() onLetter("m")
	onN <- function() onLetter("n")
	onO <- function() onLetter("o")
	onP <- function() onLetter("p")
	onQ <- function() onLetter("q")
	onR <- function() onLetter("r")
	onS <- function() onLetter("s")
	onT <- function() onLetter("t")
	onU <- function() onLetter("u")
	onV <- function() onLetter("v")
	onW <- function() onLetter("w")
	onX <- function() onLetter("x")
	onY <- function() onLetter("y")
	onZ <- function() onLetter("z")
	for (letter in c(letters, LETTERS)){
		tkbind(packageBox, paste("<", letter, ">", sep=""),
				get(paste("on", toupper(letter), sep="")))
	}
	onClick <- function() tkfocus(packageBox)
	tkbind(packageBox, "<ButtonPress-1>", onClick)
	onOK <- function(){
		datasetName <- datasets[as.numeric(tkcurselection(datasetBox)) + 1]
		dsnameValue <- tclvalue(dsname)
		if (dsnameValue != "" && is.null(package)){
			closeDialog()
			if (is.element(dsnameValue, listDataSets())) {
				if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
					if (GrabFocus()) tkgrab.release(top)
					tkdestroy(top)
					readDataFromPackage()
					return()
				}
			}
			save.options <- options(warn=2)
			check <- try(eval(parse(text=logger(paste("data(", dsnameValue, ")", sep=""))),
							envir=.GlobalEnv), silent=TRUE)
			options(save.options)
			if (inherits(check, "try-error")){
				errorCondition(recall=readDataFromPackage,
						message=sprintf(gettextRcmdr("Data set %s does not exist"), dsnameValue))
				return()
			}
			activeDataSet(dsnameValue)
			tkfocus(CommanderWindow())
		}
		else{
			if (is.null(package)) {
				errorCondition(recall=readDataFromPackage, message=gettextRcmdr("You must select a package."))
				return()
			}
			if (length(datasetName) == 0) {
				errorCondition(recall=readDataFromPackage, message=gettextRcmdr("You must select a data set.")    )
				return()
			}
			if (is.element(datasetName, listDataSets())) {
				if ("no" == tclvalue(checkReplace(datasetName, gettextRcmdr("Data set")))){
					if (GrabFocus()) tkgrab.release(top)
					tkdestroy(top)
					readDataFromPackage()
					return()
				}
			}
			closeDialog()
			command <- paste("data(", datasetName, ', package="', package, '")', sep="")
			result <- justDoIt(command)
			logger(command)
			if (class(result)[1] !=  "try-error") activeDataSet(datasetName)
			tkfocus(CommanderWindow())
		}
	}
	onDataHelp <- function(){
#		datasetName <- data(package=package)$results[as.numeric(tkcurselection(datasetBox)) + 1,3]
    datasetName <- datasets[as.numeric(tkcurselection(datasetBox)) + 1]
		dsnameValue <- tclvalue(dsname)
		if (dsnameValue == "") dsnameValue <- datasetName
		if (length(dsnameValue) == 0) Message(gettextRcmdr("No data set selected."), type="warning")
		else if (is.null(package)) doItAndPrint(paste('help("', dsnameValue, '")', sep=""))
		else doItAndPrint(paste('help("', dsnameValue, '", package="', package, '")', sep=""))
	}
	OKCancelHelp(helpSubject="data")
	dataHelpButton <- buttonRcmdr(top, text=gettextRcmdr("Help on selected data set"), command=onDataHelp)
	tkgrid(labelRcmdr(packageDatasetFrame, text=gettextRcmdr("Package (Double-click to select)"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"),
			labelRcmdr(packageDatasetFrame, text="   "), labelRcmdr(packageDatasetFrame, text=gettextRcmdr("Data set (Double-click to select)"),
					fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
	tkgrid(packageBox, packageScroll, sticky="nw")
	tkgrid(datasetBox, datasetScroll, sticky="nw")
	tkgrid(packageFrame, labelRcmdr(packageDatasetFrame, text="   "), datasetFrame, sticky="nw")
	tkgrid(packageDatasetFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=gettextRcmdr("OR"), fg="red"), sticky="w")
	tkgrid(labelRcmdr(enterFrame, text=gettextRcmdr("Enter name of data set:  "), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), entryDsname, sticky="w")
	tkgrid(enterFrame, sticky="w")
	tkgrid(dataHelpButton, sticky="w")
	tkgrid(buttonsFrame, sticky="ew")
	tkgrid.configure(packageScroll, sticky="ns")
	tkgrid.configure(datasetScroll, sticky="ns")
	tkbind(packageBox, "<Double-ButtonPress-1>", onPackageSelect)
	tkbind(datasetBox, "<Double-ButtonPress-1>", onDatasetSelect)
	dialogSuffix(focus=entryDsname)
}

importSAS <- function() {
    # the following local function is adapted from ?chartr
    capwords <- function(s) {
        cap <- function(s) paste(toupper(substring(s,1,1)), {s <- substring(s, 2); tolower(s)}, sep = "", collapse = " " )
        sapply(strsplit(s, split = " "), cap)
    }
    Library("foreign")
    file <- tclvalue(tkgetOpenFile(
        filetypes=gettextRcmdr('{"All Files" {"*"}} {"SAS xport files" {".xpt" ".XPT" ".xport" ".XPORT"}}')))
    if (file == "") {
        tkfocus(CommanderWindow())
        return()
    }
    command <- paste('read.xport("', file,'")', sep="")
    logger(paste(".Datasets <- ", command, sep=""))
    result <- justDoIt(command)
    if (class(result)[1] !=  "try-error"){
        gassign(".Datasets", result)
        if (is.data.frame(.Datasets)){
            getdsname <- function(){
                initializeDialog(title=gettextRcmdr("Data Set Name"))
                dsname <- tclVar("Dataset")
                entryDsname <- ttkentry(top, width="20", textvariable=dsname)
                onOK <- function(){
                    closeDialog()
                    setBusyCursor()
                    on.exit(setIdleCursor())
                    dsnameValue <- trim.blanks(tclvalue(dsname))
                    if (dsnameValue == ""){
                        errorCondition(recall=getdsname,
                                       message=gettextRcmdr("You must enter the name of a data set."))
                        return()
                    }
                    if (!is.valid.name(dsnameValue)){
                        errorCondition(recall=getdsname,
                                       message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
                        return()
                    }
                    if (is.element(dsnameValue, listDataSets())) {
                        if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                            getdsname()
                            return()
                        }
                    }
                    doItAndPrint(paste(dsnameValue, " <- .Datasets", sep=""))
                    logger("remove(.Datasets)")
                    remove(".Datasets", envir=.GlobalEnv)
                    activeDataSet(dsnameValue)
                }
                OKCancelHelp()
                tkgrid(labelRcmdr(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="w")
                tkgrid(buttonsFrame, columnspan="2", sticky="w")
                tkgrid.configure(entryDsname, sticky="w")
                dialogSuffix(focus=entryDsname)
            }
            getdsname()
        }
        else {
            fmt <- grep("^FORMAT", names(.Datasets))
            if (length(fmt) >= 1) gassign(".Datasets", .Datasets[-fmt])
            if (length(.Datasets) == 1){
                dsname <- capwords(names(.Datasets))
                if (is.element(dsname, listDataSets())) {
                    if ("no" == tclvalue(checkReplace(dsname, gettextRcmdr("Data set")))){
                        importSAS()
                        return()
                    }
                }
                doItAndPrint(paste(dsname, " <- .Datasets[[1]]", sep=""))
                doItAndPrint(paste("colnames(", dsname, ") <- ", "tolower(colnames(", 
                                   dsname, "))", sep=""))
                logger("remove(.Datasets)")
                remove(".Datasets", envir=.GlobalEnv)
                activeDataSet(dsname)
            }
            else {
                dsnames <- capwords(names(.Datasets))
                datasets <- listDataSets()
                initializeDialog(title=gettextRcmdr("Select Dataset"))
                datasetsBox <- variableListBox(top, dsnames, 
                                               title=gettextRcmdr("Datasets in file (pick one)"),
                                               initialSelection=0)
                onOK <- function() {
                    dsname <- getSelection(datasetsBox)
                    for (ds in 1:length(dsnames)){
                        if (is.element(dsnames[ds], datasets)) {
                            if ("no" == tclvalue(checkReplace(dsnames[ds], gettextRcmdr("Data set")))){
                                next()
                            }
                        }
                        doItAndPrint(paste(dsnames[ds], " <- .Datasets[[", ds, "]]", sep=""))
                        doItAndPrint(paste("colnames(", dsnames[ds], ") <- ", "tolower(colnames(", 
                                           dsnames[ds], "))", sep=""))
                    }
                    logger("remove(.Datasets)")
                    remove(".Datasets", envir=.GlobalEnv)
                    activeDataSet(dsname)
                    closeDialog()
                    tkfocus(CommanderWindow())
                }
                OKCancelHelp(helpSubject="read.xport")
                tkgrid(getFrame(datasetsBox), sticky="w")
                tkgrid(buttonsFrame, sticky="w")
                dialogSuffix()
            }
        }
    }
    tkfocus(CommanderWindow())
}

importSASb7dat <- function() {
    initializeDialog(title=gettextRcmdr("Import SAS b7dat Data Set"))
    dsname <- tclVar("Dataset")
    dsnameFrame <- tkframe(top)
    entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
    optionsFrame <- tkframe(top)
    asFactor <- tclVar("1")
    asFactorCheckBox <- ttkcheckbutton(optionsFrame, variable=asFactor)
    asDate <- tclVar("1")
    rownames <- tclVar("0")
    rownamesCheckBox <- ttkcheckbutton(optionsFrame, variable=rownames)
    onOK <- function(){
        closeDialog()
        setBusyCursor()
        on.exit(setIdleCursor())
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=importSASb7dat,
                           message=gettextRcmdr("You must enter the name of a data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=importSASb7dat,
                           message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                importSASb7dat()
                return()
            }
        }
        file <- tclvalue(tkgetOpenFile(
            filetypes=gettextRcmdr('{"All Files" {"*"}} {"SAS b7dat dataset" {".sas7bdat" ".SAS7BDAT"}}')))
        if (file == "") {
            tkfocus(CommanderWindow())
            return()
        }
        factor <- tclvalue(asFactor) == "1"
        has.rownames <- tclvalue(rownames) ==  "1"
        command <- paste('readSAS("', file, '", stringsAsFactors=', factor, ", rownames=", has.rownames, ")", sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error"){
            gassign(dsnameValue, result)
            activeDataSet(dsnameValue)
        }
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="readSAS")
    tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Enter name for data set:  ")), entryDsname, sticky="w")
    tkgrid(dsnameFrame, columnspan=2, sticky="w")
    tkgrid(asFactorCheckBox, labelRcmdr(optionsFrame, text=gettextRcmdr("Convert character variables to factors"), justify="left"),
           sticky="nw")
    tkgrid(rownamesCheckBox, labelRcmdr(optionsFrame, text=gettextRcmdr("First column contains row names"), justify="left"),
           sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="ew")
    dialogSuffix(focus=entryDsname)
}

importSPSS <- function() {
    initializeDialog(title=gettextRcmdr("Import SPSS Data Set"))
    dsnameFrame <- tkframe(top)
    dsname <- tclVar("Dataset")
    entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
    asFactorFrame <- tkframe(top)
    asFactor <- tclVar("1")
    asFactorCheckBox <- ttkcheckbutton(asFactorFrame, variable=asFactor)
    toLowerFrame <- tkframe(top)
    toLower <- tclVar("0")
    toLowerCheckBox <- ttkcheckbutton(toLowerFrame, variable=toLower)
    rowNamesFrame <- tkframe(top)
    rownames <- tclVar("0")
    rownamesCheckBox <- ttkcheckbutton(rowNamesFrame, variable=rownames)
    onOK <- function(){
        closeDialog()
        setBusyCursor()
        on.exit(setIdleCursor())
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=importSPSS,
                           message=gettextRcmdr("You must enter the name of a data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=importSPSS,
                           message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                importSPSS()
                return()
            }
        }
        file <- tclvalue(tkgetOpenFile(
            filetypes=gettextRcmdr('{"All Files" {"*"}} {"SPSS portable files" {".por" ".POR"}} {"SPSS save files" {".sav" ".SAV"}}')))
        if (file == "") {
            tkfocus(CommanderWindow())
            return()
        }
        factor <- tclvalue(asFactor) == "1"
        lower <- tclvalue(toLower) == "1"
        rows <- tclvalue(rownames) == "1"
        command <- paste('readSPSS("', file,'", rownames=', rows, ", stringsAsFactors=", factor, ", tolower=", lower, ")", sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error"){
            gassign(dsnameValue, result)
            activeDataSet(dsnameValue)
        }
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="readSPSS")
    tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Enter name for data set:  ")), entryDsname, sticky="w")
    tkgrid(dsnameFrame, sticky="w")
    tkgrid(asFactorCheckBox, labelRcmdr(asFactorFrame, text=gettextRcmdr("Convert character variables to factors"), justify="left"),
           sticky="nw")
    tkgrid(asFactorFrame, sticky="w")
    tkgrid(rownamesCheckBox, labelRcmdr(rowNamesFrame, text=gettextRcmdr("First column contains row names"), justify="left"),
           sticky="w")
    tkgrid(rowNamesFrame, sticky="w")
    tkgrid(toLowerCheckBox, labelRcmdr(toLowerFrame, text=gettextRcmdr("Convert variable names to lower case"), justify="left"),
           sticky="nw")
    tkgrid(toLowerFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="ew")
    dialogSuffix(focus=entryDsname)
}

# importSPSS <- function() {
#     Library("foreign")
#     initializeDialog(title=gettextRcmdr("Import SPSS Data Set"))
#     dsnameFrame <- tkframe(top)
#     dsname <- tclVar("Dataset")
#     entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
#     asFactorFrame <- tkframe(top)
#     asFactor <- tclVar("1")
#     asFactorCheckBox <- ttkcheckbutton(asFactorFrame, variable=asFactor)
#     toLowerFrame <- tkframe(top)
#     toLower <- tclVar("1")
#     toLowerCheckBox <- ttkcheckbutton(toLowerFrame, variable=toLower)
#     maxLevelsFrame <- tkframe(top)
#     maxLevels <- tclVar("Inf")
#     entryMaxLevels <- ttkentry(maxLevelsFrame, width="5", textvariable=maxLevels)
#     onOK <- function(){
#         closeDialog()
#         setBusyCursor()
#         on.exit(setIdleCursor())
#         dsnameValue <- trim.blanks(tclvalue(dsname))
#         if (dsnameValue == ""){
#             errorCondition(recall=importSPSS,
#                            message=gettextRcmdr("You must enter the name of a data set."))
#             return()
#         }
#         if (!is.valid.name(dsnameValue)){
#             errorCondition(recall=importSPSS,
#                            message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
#             return()
#         }
#         if (is.element(dsnameValue, listDataSets())) {
#             if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
#                 importSPSS()
#                 return()
#             }
#         }
#         file <- tclvalue(tkgetOpenFile(
#             filetypes=gettextRcmdr('{"All Files" {"*"}} {"SPSS portable files" {".por" ".POR"}} {"SPSS save files" {".sav" ".SAV"}}')))
#         if (file == "") {
#             tkfocus(CommanderWindow())
#             return()
#         }
#         factor <- tclvalue(asFactor) == "1"
#         levels <- as.numeric(tclvalue(maxLevels))
#         command <- paste('read.spss("', file,'", use.value.labels=', factor,
#                          ", max.value.labels=", levels, ", to.data.frame=TRUE)", sep="")
#         logger(paste(dsnameValue, " <- ", command, sep=""))
#         result <- justDoIt(command)
#         if (class(result)[1] !=  "try-error"){
#             gassign(dsnameValue, result)
#             if (tclvalue(toLower) == "1") 
#                 doItAndPrint(paste("colnames(", dsnameValue, ") <- tolower(colnames(",
#                                    dsnameValue, "))", sep=""))
#             activeDataSet(dsnameValue)
#         }
#         tkfocus(CommanderWindow())
#     }
#     OKCancelHelp(helpSubject="read.spss")
#     tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Enter name for data set:  ")), entryDsname, sticky="w")
#     tkgrid(dsnameFrame, sticky="w")
#     tkgrid(asFactorCheckBox, labelRcmdr(asFactorFrame, text=gettextRcmdr("Convert value labels\nto factor levels"), justify="left"),
#            sticky="nw")
#     tkgrid(asFactorFrame, sticky="w")
#     tkgrid(toLowerCheckBox, labelRcmdr(toLowerFrame, text=gettextRcmdr("Convert variable names\nto lower case"), justify="left"),
#            sticky="nw")
#     tkgrid(toLowerFrame, sticky="w")
#     tkgrid(labelRcmdr(maxLevelsFrame, text=gettextRcmdr("Maximum number\nof value labels\nfor factor conversion"), justify="left"),
#            entryMaxLevels, sticky="nw")
#     tkgrid(maxLevelsFrame, sticky="w")
#     tkgrid(buttonsFrame, sticky="ew")
#     dialogSuffix(focus=entryDsname)
# }

importMinitab <- function() {
	Library("foreign")
	initializeDialog(title=gettextRcmdr("Import Minitab Data Set"))
	dsname <- tclVar("Dataset")
	entryDsname <- ttkentry(top, width="20", textvariable=dsname)
	onOK <- function(){
		closeDialog()
		setBusyCursor()
		on.exit(setIdleCursor())
		dsnameValue <- trim.blanks(tclvalue(dsname))
		if (dsnameValue == ""){
			errorCondition(recall=importMinitab,
					message=gettextRcmdr("You must enter the name of a data set."))
			return()
		}
		if (!is.valid.name(dsnameValue)){
			errorCondition(recall=importMinitab,
					message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsnameValue, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
				importMinitab()
				return()
			}
		}
		file <- tclvalue(tkgetOpenFile(
						filetypes=gettextRcmdr('{"All Files" {"*"}} {"Minitab portable files" {".mtp" ".MTP"}}')))
		if (file == "") {
			tkfocus(CommanderWindow())
			return()
		}
		command <- paste('read.mtp("', file,'")', sep="")
		datalist <- justDoIt(command)
		lengths <- sapply(datalist, length)
		datalist <- datalist[lengths != 0]
		lengths <- lengths[lengths != 0]
		if (!all(lengths == length(datalist[[1]]))){
			Message(message=
							paste(gettextRcmdr("Minitab data set contains elements of unequal length.\nData set cannot be converted.")),
					type="error")
			tkdestroy(top)
			tkfocus(CommanderWindow())
			return()
		}
		doItAndPrint(paste(dsnameValue, " <- as.data.frame(", command, ")", sep=""))
		activeDataSet(dsnameValue)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="read.mtp")
	tkgrid(labelRcmdr(top, text=gettextRcmdr("Enter name for data set:")), entryDsname, sticky="e")
	tkgrid(buttonsFrame, columnspan="2", sticky="w")
	tkgrid.configure(entryDsname, sticky="w")
	dialogSuffix(focus=entryDsname)
}

# the following function was contributed by Michael Ash (modified by J. Fox)

importSTATA <- function() {
    initializeDialog(title=gettextRcmdr("Import STATA Data Set"))
    dsname <- tclVar("Dataset")
    dsnameFrame <- tkframe(top)
    entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
    optionsFrame <- tkframe(top)
    asFactor <- tclVar("1")
    asFactorCheckBox <- ttkcheckbutton(optionsFrame, variable=asFactor)
    asDate <- tclVar("1")
    asDateCheckBox <- ttkcheckbutton(optionsFrame, variable=asDate)
    rownames <- tclVar("0")
    rownamesCheckBox <- ttkcheckbutton(optionsFrame, variable=rownames)
    onOK <- function(){
        closeDialog()
        setBusyCursor()
        on.exit(setIdleCursor())
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == ""){
            errorCondition(recall=importSTATA,
                           message=gettextRcmdr("You must enter the name of a data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)){
            errorCondition(recall=importSTATA,
                           message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                importSTATA()
                return()
            }
        }
        file <- tclvalue(tkgetOpenFile(
            filetypes=gettextRcmdr('{"All Files" {"*"}} {"STATA datasets" {".dta" ".DTA"}}')))
        if (file == "") {
            tkfocus(CommanderWindow())
            return()
        }
        convert.date <- tclvalue(asDate) == "1"
        factor <- tclvalue(asFactor) == "1"
        has.rownames <- tclvalue(rownames) ==  "1"
        command <- paste('readStata("', file,'", convert.dates=', convert.date,
                         ", stringsAsFactors=", factor, ", rownames=", has.rownames, ")", sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error"){
            gassign(dsnameValue, result)
            activeDataSet(dsnameValue)
        }
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="readStata")
    tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Enter name for data set:  ")), entryDsname, sticky="w")
    tkgrid(dsnameFrame, columnspan=2, sticky="w")
    tkgrid(asFactorCheckBox, labelRcmdr(optionsFrame, text=gettextRcmdr("Convert character variables to factors"), justify="left"),
           sticky="nw")
    tkgrid(asDateCheckBox, labelRcmdr(optionsFrame, text=gettextRcmdr("Convert dates to R format"), justify="left"),
           sticky="w")
    tkgrid(rownamesCheckBox, labelRcmdr(optionsFrame, text=gettextRcmdr("First column contains row names"), justify="left"),
           sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan="2", sticky="ew")
    dialogSuffix(focus=entryDsname)
}

# The following function was contributed by Matthieu Lesnoff
#  (added with small changes by J. Fox)

# importRODBCtable <- function(){
# 	Library("RODBC")
# 	# close all databases in case of error
# 	on.exit(odbcCloseAll())
# # Enter the name of data set, by default : Dataset
# 	initializeDialog(title = gettextRcmdr("Import from Excel, Access or dBase data set"))
# 	dsname <- tclVar(gettextRcmdr("Dataset"))
# 	entryDsname <- ttkentry(top, width = "35", textvariable = dsname)
# 	onOK <- function(){
# 		closeDialog()
# 		setBusyCursor()
# 		on.exit(setIdleCursor())
# 		dsnameValue <- trim.blanks(tclvalue(dsname))
# 		if(dsnameValue == ""){
# 			errorCondition(recall = importRODBCtable,
# 					message = gettextRcmdr("You must enter the name of a data set."))
# 			return()
# 		}
# 		if(!is.valid.name(dsnameValue)){
# 			errorCondition(recall = queryimportRODBCtable,
# 					message = paste('"', dsnameValue, '" ',
# 							gettextRcmdr("is not a valid name."), sep = ""))
# 			return()
# 		}
# 		if(is.element(dsnameValue, listDataSets())){
# 			if("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
# 				importRODBCtable()
# 				return()
# 			}
# 		}
# 		File <- tclvalue(tkgetOpenFile(filetypes = gettextRcmdr(
# 								'{"All Files" {"*"}} {"MS Access database" {".mdb" ".MDB"}} {"MS Access 2007 database" {".accdb" ".ACCDB"}} {"dBase-like file" {".dbf" ".DBF"}} {"MS Excel 2007 file" {".xlsx" ".XLSX"}} {"MS Excel file" {".xls" ".XLS"}}'
# 						), parent=CommanderWindow()))
# 		if(File == ""){
# 			tkfocus(CommanderWindow())
# 			return()
# 		}
# 		sop <- match(".", rev(strsplit(File, NULL)[[1]]))[1]
# 		ext <- tolower(substring(File, nchar(File) - sop + 2, nchar(File)))
# 		channel <- switch(EXPR = ext,
# 				xls = odbcConnectExcel(File),
# 				xlsx = odbcConnectExcel2007(File),
# 				mdb = odbcConnectAccess(File),
# 				accdb = odbcConnectAccess2007(File),
# 				dbf = odbcConnectDbase(File))
# 		# For Excel and Access cases, need to select a particular sheet or table
# 		if(ext != "dbf"){
# 			tabdat <- sqlTables(channel)
# 			names(tabdat) <- tolower(names(tabdat))
# 			if(ext == "mdb" || ext == "accdb")
# 				tabdat <- tabdat[tabdat$table_type == "TABLE", 3]
# 			if(ext == "xls" || ext == "xlsx"){
# 				tabname <- tabdat$table_name
# 				tabdat <- ifelse(tabdat$table_type =="TABLE",
# 						substring(tabname, 2, nchar(tabname) - 2),
# 						substring(tabname, 1, nchar(tabname) - 1))
# 			}
# 			# if there are several tables
# 			if(length(tabdat)>1)
# 				fil <- tk_select.list(sort(tabdat),
# 						title = gettextRcmdr("Select one table"))
# 			else
# 				fil <- tabdat
# 			if(fil == ""){
# 				errorCondition(message=gettextRcmdr("No table selected"))
# 				return()
# 			}
# 			if(ext == "xls" || ext == "xlsx")
# 				fil <- paste("[", fil, "$]", sep = "")
# 		}
# 		# dBase file
# 		else{
# 			sop <- match(".", rev(strsplit(File, NULL)[[1]]))[1]
# 			root <- tolower(substring(File, 1, nchar(File) - sop))
# 			revstr <- rev(strsplit(root, NULL)[[1]])
# 			sop <- if(is.na(match(c("/", "\\"), revstr)[1]))
# 						length(revstr) else match(c("/", "\\"), revstr)[1] - 1
# 			toor <- revstr[seq(sop)]
# 			fil <- paste(rev(toor), collapse = "")
# 		}
# 		# Retrieve the data
# 		dat <- sqlQuery(channel = channel, query = paste("select * from", fil))
# 		names(dat)<- trim.blanks(names(dat))
# 		dat <- trim.col.na(dat)
# 		odbcCloseAll()
# 		gassign(dsnameValue, as.data.frame(dat))
# 		command <- paste("sqlQuery(channel = ",channel,", select * from ", fil,")",
# 				sep = "")
# 		logger(paste(dsnameValue, " <- ", command, sep = ""))
# 		activeDataSet(dsnameValue)
# 		tkfocus(CommanderWindow())
# 	}  ## End of function onOK
# 	OKCancelHelp(helpSubject="odbcConnect")
# 	tkgrid(labelRcmdr(top, text=gettextRcmdr("Enter name of data set:  ")),
# 			entryDsname, sticky="e")
# 	tkgrid(buttonsFrame, columnspan="2", sticky="w")
# 	tkgrid.configure(entryDsname, sticky="w")
# 	dialogSuffix(focus=entryDsname)
# }

# importExcel <- function(){
#   Library("XLConnect")
#   initializeDialog(title = gettextRcmdr("Import Excel Data Set"))
#   dsname <- tclVar(gettextRcmdr("Dataset"))
#   dsnameFrame <- tkframe(top)
#   entryDsname <- ttkentry(dsnameFrame, width = "35", textvariable = dsname)
#   checkBoxFrame <- tkframe(top)
#   variableNames <- tclVar("1")
#   variableNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable=variableNames)
#   rowNames <- tclVar("0")
#   rowNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable=rowNames)
#   onOK <- function(){
#     closeDialog()
#     setBusyCursor()
#     on.exit(setIdleCursor())
#     dsnameValue <- trim.blanks(tclvalue(dsname))
#     variableNamesValue <- tclvalue(variableNames)
#     rowNamesValue <- tclvalue(rowNames)
#     if(dsnameValue == ""){
#       errorCondition(recall = importExcel,
#                      message = gettextRcmdr("You must enter the name of a data set."))
#       return()
#     }
#     if(!is.valid.name(dsnameValue)){
#       errorCondition(recall = importExcel,
#                      message = paste('"', dsnameValue, '" ',
#                                      gettextRcmdr("is not a valid name."), sep = ""))
#       return()
#     }
#     if(is.element(dsnameValue, listDataSets())){
#       if("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
#         importExcel()
#         return()
#       }
#     }
#     File <- tclvalue(tkgetOpenFile(filetypes = gettextRcmdr(
#       '{"All Files" {"*"}} {"MS Excel 2007 file" {".xlsx" ".XLSX"}} {"MS Excel file" {".xls" ".XLS"}}'
#     ), parent=CommanderWindow()))
#     if(File == ""){
#       tkfocus(CommanderWindow())
#       return()
#     }
#     command <- paste('loadWorkbook("', File, '")', sep="")
#     doItAndPrint(paste(".Workbook <- ", command, sep=""))
#     worksheets <- getSheets(.Workbook)
#     if(length(worksheets)>1)
#       worksheet <- tk_select.list(worksheets,
#                                   title = gettextRcmdr("Select one table"))
#     else
#       worksheet <- worksheets
#     if(worksheet == ""){
#       errorCondition(message=gettextRcmdr("No table selected"))
#       return()
#     }
#     command <- paste('readWorksheet(.Workbook, "', worksheet,
#                      '", header=', if (variableNamesValue == "1") 'TRUE' else 'FALSE',
#                      ', rownames=', if(rowNamesValue == "1") '1' else 'NULL',
#                      ')', sep="")
#     logger(paste(dsnameValue, " <- ", command, sep=""))
#     result <- justDoIt(command)
#     if (class(result)[1] !=  "try-error"){
#       gassign(dsnameValue, result)
#     }
#     logger("remove(.Workbook)")
#     justDoIt("remove(.Workbook, envir=.GlobalEnv)")
#     if (class(result)[1] !=  "try-error"){
#       factors <- sapply(get(dsnameValue, envir=.GlobalEnv), is.character)
#       if (any(factors)){
#         factors <- which(factors)
#         command <- paste(dsnameValue, "[, c(", paste(factors, collapse=", "), 
#                          ")] <- lapply(", dsnameValue, "[, c(", 
#                          paste(factors, collapse=", "), "), drop=FALSE], as.factor)",
#                          sep="")
#         doItAndPrint(command)
#       }
#       activeDataSet(dsnameValue)
#     }
#   }
#   OKCancelHelp(helpSubject="readWorksheet")
#   tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Enter name of data set: ")),
#          entryDsname, sticky="w")
#   tkgrid(dsnameFrame, sticky="w")
#   tkgrid(variableNamesCheckBox, labelRcmdr(checkBoxFrame, text=gettextRcmdr("Variable names in first row of spreadsheet")),
#          sticky="w")
#   tkgrid(rowNamesCheckBox, labelRcmdr(checkBoxFrame, text=gettextRcmdr("Row names in first column of spreadsheet")),
#          sticky="w")
#   tkgrid(checkBoxFrame, sticky="w")
#   tkgrid(buttonsFrame, sticky="w")
#   dialogSuffix(focus=entryDsname)
# }

importExcel <- function(){
    initializeDialog(title = gettextRcmdr("Import Excel Data Set"))
    dsname <- tclVar("Dataset")
    dsnameFrame <- tkframe(top)
    entryDsname <- ttkentry(dsnameFrame, width = "35", textvariable = dsname)
    checkBoxFrame <- tkframe(top)
    variableNames <- tclVar("1")
    variableNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable=variableNames)
    rowNames <- tclVar("0")
    rowNamesCheckBox <- ttkcheckbutton(checkBoxFrame, variable=rowNames)
    stringsAsFactors <- tclVar("1")
    stringsAsFactorsCheckBox <- ttkcheckbutton(checkBoxFrame, variable=stringsAsFactors)
    missingFrame <- tkframe(top)
    missingVariable <- tclVar(gettextRcmdr("<empty cell>"))
    missingEntry <- ttkentry(missingFrame, width="15", textvariable=missingVariable)
    onOK <- function(){
        closeDialog()
        setBusyCursor()
        on.exit(setIdleCursor())
        dsnameValue <- trim.blanks(tclvalue(dsname))
        variableNamesValue <- tclvalue(variableNames)
        rowNamesValue <- tclvalue(rowNames)
        stringsAsFactorsValue <- tclvalue(stringsAsFactors)
        missingValues <- as.character(tclvalue(missingVariable))
        if (missingValues == gettextRcmdr("<empty cell>")) missingValues <- ""
        if(dsnameValue == ""){
            errorCondition(recall = importExcel,
                message = gettextRcmdr("You must enter the name of a data set."))
            return()
        }
        if(!is.valid.name(dsnameValue)){
            errorCondition(recall = importExcel,
                message = paste('"', dsnameValue, '" ',
                    gettextRcmdr("is not a valid name."), sep = ""))
            return()
        }
        if(is.element(dsnameValue, listDataSets())){
            if("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                importExcel()
                return()
            }
        }
        File <- tclvalue(tkgetOpenFile(filetypes = gettextRcmdr(
            '{"All Files" {"*"}} {"MS Excel file" {".xlsx" ".XLSX" ".xls" ".XLS"}}'
        ), parent=CommanderWindow()))
        if(File == ""){
            tkfocus(CommanderWindow())
            return()
        }
        worksheets <- excel_sheets(File)
        if(length(worksheets) > 1)
            worksheet <- tk_select.list(worksheets,
                title = gettextRcmdr("Select one table"))
        else
            worksheet <- worksheets
        if(worksheet == ""){
            errorCondition(message=gettextRcmdr("No table selected"))
            return()
        }
        command <- paste('readXL("', File, '", rownames=', if (rowNamesValue == "1") "TRUE" else "FALSE",
            ", header=", if (variableNamesValue == "1") "TRUE" else "FALSE",
            ', na="', missingValues, '", sheet="', worksheet,
            '", stringsAsFactors=', if(stringsAsFactorsValue == "1") "TRUE" else "FALSE",
            ')', sep="")
        logger(paste(dsnameValue, " <- ", command, sep=""))
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error"){
            gassign(dsnameValue, result)
            activeDataSet(dsnameValue)
        }
    }
    OKCancelHelp(helpSubject="readXL")
    tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Enter name of data set: ")),
        entryDsname, sticky="w")
    tkgrid(dsnameFrame, sticky="w")
    tkgrid(variableNamesCheckBox, labelRcmdr(checkBoxFrame, text=gettextRcmdr("Variable names in first row of spreadsheet")),
        sticky="w")
    tkgrid(rowNamesCheckBox, labelRcmdr(checkBoxFrame, text=gettextRcmdr("Row names in first column of spreadsheet")),
        sticky="w")
    tkgrid(stringsAsFactorsCheckBox, labelRcmdr(checkBoxFrame, text=gettextRcmdr("Convert character data to factors")),
        sticky="w")
    tkgrid(labelRcmdr(missingFrame, text=gettextRcmdr("Missing data indicator:")), missingEntry, sticky="w")
    tkgrid(checkBoxFrame, sticky="w")
    tkgrid(missingFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(focus=entryDsname)
}


numericToFactor <- function(){
  initializeDialog(title=gettextRcmdr("Convert Numeric Variables to Factors"))
  dataFrame <- tkframe(top)
  variableBox <- variableListBox(dataFrame, Numeric(), selectmode="multiple",
                                 title=gettextRcmdr("Variables (pick one or more)"))
  radioButtons(dataFrame, name="levels", buttons=c("names", "numbers"),
               labels=gettextRcmdr(c("Supply level names", "Use numbers")), title=gettextRcmdr("Factor Levels"))
  factorNameFrame <- tkframe(top)
  factorName <- tclVar(gettextRcmdr("<same as variables>"))
  factorNameField <- ttkentry(factorNameFrame, width="20", textvariable=factorName)
  onOK <- function(){
    variables <- getSelection(variableBox)
    closeDialog()
    facname <- trim.blanks(tclvalue(factorName))
    .activeDataSet <- ActiveDataSet()
    cmd <- paste("apply(", .activeDataSet, "[, c(", paste(
      paste('"', variables, '"', sep=""),
      collapse=","), "), drop=FALSE], 2, function(x) sort(unique(x)))", sep="")
    levs <- eval(parse(text=cmd), envir=.GlobalEnv)
    sameLevels <- (length(variables) == 1) ||
      ((is.matrix(levs)) && (all(0 == apply(levs, 1, var))))
    if (length(variables) == 0) {
      errorCondition(recall=numericToFactor, message=gettextRcmdr("You must select a variable."))}
    else command <- paste(.activeDataSet, " <- within(", .activeDataSet, ", {", sep="")
    for (name in variables){
      fname <- if (facname == gettextRcmdr("<same as variables>")) name
      else if (length(variables) == 1) facname
      else paste(facname, name, sep="")
      if (!is.valid.name(fname)){
        errorCondition(recall=numericToFactor,
                       message=paste('"', fname, '" ', gettextRcmdr("is not a valid name."), sep=""))
        return()
      }
      if (is.element(fname, Variables())) {
        if ("no" == tclvalue(checkReplace(fname))){
          numericToFactor()
          return()
        }
      }
      levelsType <- tclvalue(levelsVariable)
      env <- environment()
      if (((name == variables[1]) || (!sameLevels)) && (levelsType == "names")){
        values <- sort(unique(eval(parse(text=paste(.activeDataSet, "$", name, sep="")),
                                   envir=.GlobalEnv)))
        nvalues <- length(values)
        if (nvalues > 30) {
          errorCondition(recall=numericToFactor,
                         message=sprintf(gettextRcmdr("Number of levels (%d) too large."), nvalues))
          return()
        }
        initializeDialog(subdialog,
                         title=paste(gettextRcmdr("Level Names for"),
                                     if(sameLevels && length(variables) > 1) "Factors" else fname))
        names <- rep("", nvalues)
        onOKsub <- function() {
          closeDialog(subdialog)
          for (i in 1:nvalues){
            names[i] <- eval(parse(text=paste("tclvalue(levelName", i, ")", sep="")))
          }
          if (length(unique(names)) != nvalues){
            errorCondition(recall=numericToFactor,
                           message=gettextRcmdr("Levels names are not unique."))
            return()
          }
          if (any(names == "")){
            errorCondition(recall=numericToFactor,
                           message=gettextRcmdr("A level name is empty."))
            return()
          }
          assign("labels", paste(paste("'", names, "'", sep=""), collapse=","),
                 envir=env)
        }
        subOKCancelHelp()
        tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Numeric value")), labelRcmdr(subdialog, text=gettextRcmdr("Level name")), sticky="w")
        for (i in 1:nvalues){
          valVar <- paste("levelName", i, sep="")
          assign(valVar, tclVar(""))
          assign(paste("entry", i, sep=""), ttkentry(subdialog, width="20",
                                                     textvariable=get(valVar)))
          tkgrid(labelRcmdr(subdialog, text=values[i]), get(paste("entry", i, sep="")), sticky="w")
        }
        tkgrid(subButtonsFrame, sticky="w", columnspan=2)
        dialogSuffix(subdialog, focus=entry1, onOK=onOKsub, force.wait=TRUE)
      }
      if (levelsType == "names"){
        if (!exists("labels", mode="character")) return()
        cmd <- paste("factor(", name,
                     ", labels=c(", labels, "))", sep="")
        command <- paste(command, "\n  ", fname, " <- ", cmd, sep="")
      }
      else{
        command <- paste(command, "\n  ", fname, " <- as.factor(", name, ")", sep="")
      }
    }
    command <- paste(command, "\n})", sep="")
    result <- doItAndPrint(command)
    if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="factor")
  tkgrid(getFrame(variableBox), labelRcmdr(dataFrame, text="  "), levelsFrame, sticky="nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(labelRcmdr(factorNameFrame,
                    text=gettextRcmdr("New variable name or prefix for multiple variables:  ")),
         factorNameField, sticky="w")
  tkgrid(factorNameFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="ew", columnspan=2)
  tkgrid.configure(numbersButton, sticky="w")
  tkgrid.configure(namesButton, sticky="w")
  dialogSuffix(preventGrabFocus=TRUE)
}

binVariable <- function () {
  # Author: Dan Putler (revision by J. Fox, 2 Feb 05)
  defaults <- list (initial.levels = "specify", initial.bins = "3", initial.varName = NULL, 
                    initial.newVar = "variable", initial.method = "intervals")
  dialog.values <- getDialog ("binVariable", defaults)
  env <- environment()
  initializeDialog(title = gettextRcmdr("Bin a Numeric Variable"))
  variableFrame <- tkframe(top)
  variableBox <- variableListBox(variableFrame, Numeric(), 
                                 title = gettextRcmdr("Variable to bin (pick one)"), 
                                 initialSelection = varPosn (dialog.values$initial.varName, "numeric"))
  newVariableFrame <- tkframe(variableFrame)
  newVariableName <- tclVar(dialog.values$initial.newVar)
  newVariable <- ttkentry(newVariableFrame, width = "18", textvariable = newVariableName)
  binsFrame <- tkframe(top)
  binsVariable <- tclVar(dialog.values$initial.bins)
  slider <- tkscale(binsFrame, from = 2, to = 20, showvalue = TRUE, 
                    variable = binsVariable, resolution = 1, orient = "horizontal")
  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "levels", buttons = c("specify", 
                                                          "numbers", "ranges"), labels = gettextRcmdr(c("Specify names", 
                                                                                                        "Numbers", "Ranges")), title = gettextRcmdr("Level Names"),
               initialValue = dialog.values$initial.levels)
  radioButtons(optionsFrame, name = "method", buttons = c("intervals", 
                                                          "proportions", "natural"), labels = gettextRcmdr(c("Equal-width bins", 
                                                                                                             "Equal-count bins", "Natural breaks\n(from K-means clustering)")), 
               title = gettextRcmdr("Binning Method"), 
               initialValue = dialog.values$initial.method)
  onOK <- function() {
    levels <- tclvalue(levelsVariable)
    bins <- as.numeric(tclvalue(binsVariable))
    varName <- getSelection(variableBox)
    closeDialog()
    if (length(varName) == 0) {
      errorCondition(recall = binVariable, message = gettextRcmdr("You must select a variable."))
      return()
    }
    newVar <- tclvalue(newVariableName)
    if (is.element(newVar, Variables())) {
      if ("no" == tclvalue(checkReplace(newVar))) {
        binVariable()
        return()
      }
    }
    if (!is.valid.name(newVar)) {
      errorCondition(message = paste("\"", newVar, "\" ", 
                                     gettextRcmdr("is not a valid name."), sep = ""), 
                     recall = binVariable)
      return()
    }
    method <- tclvalue(methodVariable)
    putDialog ("binVariable", list (initial.levels = levels, initial.bins = bins, initial.varName = varName, 
                                    initial.newVar = newVar, initial.method = method))
    if (levels == "specify") {
      initializeDialog(subdialog, title = gettextRcmdr("Bin Names"))
      onOKsub <- function() {
        closeDialog(subdialog)
        level <- character(bins)
        for (i in 1:bins) {
          level[i] <- eval(parse(text = paste("tclvalue(levelName", 
                                              i, ")", sep = "")))
        }
        if (length(unique(level)) != length(level)) {
          errorCondition(window = subdialog, message = gettextRcmdr("Level names must be unique."), 
                         recall = onOK)
          return()
        }
        assign("levelNames", level, envir = env)
      }
      subOKCancelHelp()
      tkgrid(labelRcmdr(subdialog, text = gettextRcmdr("Bin"), 
                        fg = getRcmdr("title.color"), font="RcmdrTitleFont"), labelRcmdr(subdialog, text = gettextRcmdr("Name"), 
                                                                                         fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w")
      for (i in 1:bins) {
        valVar <- paste("levelName", i, sep = "")
        assign(valVar, tclVar(i))
        assign(paste("entry", i, sep = ""), ttkentry(subdialog, 
                                                     width = "20", textvariable = get(valVar)))
        tkgrid(labelRcmdr(subdialog, text = as.character(i)), 
               get(paste("entry", i, sep = "")), sticky = "w")
      }
      tkgrid(subButtonsFrame, sticky = "w", columnspan = 2)
      dialogSuffix(subdialog, focus = entry1, bindReturn = FALSE, force.wait=TRUE)
    }
    labels <- if (levels == "numbers") 
      "FALSE"
    else if (levels == "ranges") 
      "NULL"
    else {
      if (!exists("levelNames")) {
        onCancel()
        binVariable()
        return()
      }
      paste("c('", paste(levelNames, collapse = "','"), 
            "')", sep = "")
    }
    .activeDataSet <- ActiveDataSet()
    command <- paste(.activeDataSet, "$", newVar, " <- ", 
                     "with(", .activeDataSet, ", binVariable(", varName, ", bins=", 
                     bins, ", method=", "'", method, "', labels=", labels, 
                     "))", sep = "")
    logger(command)
    result <- justDoIt(command)
    if (class(result)[1] != "try-error") 
      activeDataSet(.activeDataSet, flushModel = FALSE, 
                    flushDialogMemory = FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "bin.var", reset = "binVariable")
  tkgrid(labelRcmdr(newVariableFrame, text = gettextRcmdr("New variable name"), 
                    fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w")
  tkgrid(newVariable, sticky = "w")
  tkgrid(getFrame(variableBox), labelRcmdr(variableFrame, text = "    "), 
         newVariableFrame, sticky = "nw")
  tkgrid(variableFrame, sticky = "w")
  tkgrid(labelRcmdr(binsFrame, text = gettextRcmdr("Number of bins:")), 
         slider, sticky = "s")
  tkgrid(binsFrame, sticky = "w")
  tkgrid(levelsFrame, labelRcmdr(optionsFrame, text = "    "), 
         methodFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}

reorderFactor <- function(){
  initializeDialog(title=gettextRcmdr("Reorder Factor Levels"))
  variableBox <- variableListBox(top, Factors(), title=gettextRcmdr("Factor (pick one)"))
  orderedFrame <- tkframe(top)
  orderedVariable <- tclVar("0")
  orderedCheckBox <- ttkcheckbutton(orderedFrame, variable=orderedVariable)
  factorName <- tclVar(gettextRcmdr("<same as original>"))
  factorNameField <- ttkentry(top, width="20", textvariable=factorName)
  onOK <- function(){
    variable <- getSelection(variableBox)
    closeDialog()
    if (length(variable) == 0) {
      errorCondition(recall=reorderFactor, message=gettextRcmdr("You must select a variable."))
      return()
    }
    name <- trim.blanks(tclvalue(factorName))
    if (name == gettextRcmdr("<same as original>")) name <- variable
    if (!is.valid.name(name)){
      errorCondition(recall=reorderFactor,
                     message=paste('"', name, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(name, Variables())) {
      if ("no" == tclvalue(checkReplace(name))){
        reorderFactor()
        return()
      }
    }
    .activeDataSet <- ActiveDataSet()
    old.levels <- eval(parse(text=paste("levels(", .activeDataSet, "$", variable, ")",
                                        sep="")), envir=.GlobalEnv)
    nvalues <- length(old.levels)
    ordered <- tclvalue(orderedVariable)
    if (nvalues > 30) {
      errorCondition(recall=reorderFactor,
                     message=sprintf(gettextRcmdr("Number of levels (%d) too large."), nvalues))
      return()
    }
    initializeDialog(subdialog, title=gettextRcmdr("Reorder Levels"))
    order <- 1:nvalues
    onOKsub <- function() {
      closeDialog(subdialog)
      opt <- options(warn=-1)
      for (i in 1:nvalues){
        order[i] <- as.numeric(eval(parse(text=paste("tclvalue(levelOrder", i, ")", sep=""))))
      }
      options(opt)
      if (any(sort(order) != 1:nvalues) || any(is.na(order))){
        errorCondition(recall=reorderFactor,
                       message=paste(gettextRcmdr("Order of levels must include all integers from 1 to "), nvalues, sep=""))
        return()
      }
      levels <- old.levels[order(order)]
      ordered <- if (ordered == "1") ", ordered=TRUE" else ""
      command <- paste("with(", .activeDataSet, ", factor(", variable,
                       ", levels=c(", paste(paste("'", levels, "'", sep=""), collapse=","), ")",
                       ordered, "))", sep="")
      result <- justDoIt(paste(.activeDataSet, "$", name, " <- ", command, sep=""))
      logger(paste(.activeDataSet,"$", name," <- ", command, sep=""))
      if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    }
    subOKCancelHelp()
    tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Old Levels"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"),
           labelRcmdr(subdialog, text=gettextRcmdr("New order"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
    for (i in 1:nvalues){
      valVar <- paste("levelOrder", i, sep="")
      assign(valVar, tclVar(i))
      assign(paste("entry", i, sep=""), ttkentry(subdialog, width="2",
                                                 textvariable=get(valVar)))
      tkgrid(labelRcmdr(subdialog, text=old.levels[i]), get(paste("entry", i, sep="")), sticky="w")
    }
    tkgrid(subButtonsFrame, sticky="w", columnspan=2)
    dialogSuffix(subdialog, focus=entry1, force.wait=TRUE)
  }
  OKCancelHelp(helpSubject="factor")
  tkgrid(getFrame(variableBox), sticky="nw")
  tkgrid(labelRcmdr(top, text=gettextRcmdr("Name for factor")), sticky="w")
  tkgrid(factorNameField, sticky="w")
  tkgrid(orderedCheckBox, labelRcmdr(orderedFrame, text=gettextRcmdr("Make ordered factor")), sticky="w")
  tkgrid(orderedFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(preventGrabFocus=TRUE)
}

standardize <- function(X){
  initializeDialog(title=gettextRcmdr("Standardize Variables"))
  xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variables (pick one or more)"),
                          selectmode="multiple")
  onOK <- function(){
    x <- getSelection(xBox)
    closeDialog()
    if (length(x) == 0) {
      errorCondition(recall=standardize, message=gettextRcmdr("You must select one or more variables."))
      return()
    }
    xx <- paste('"', x, '"', sep="")
    .activeDataSet <- ActiveDataSet()
    command <- paste(.activeDataSet, " <- ", "local({\n  .Z <- scale(", 
                     .activeDataSet, "[,c(", paste(xx, collapse=","),
                     ")])\n  within(", .activeDataSet, ", {", sep="")
    for (i in length(x):1){
      Z <- paste("Z.", x[i], sep="")
      if (is.element(Z, Variables())) {
        if ("no" == tclvalue(checkReplace(Z))){
          if (GrabFocus()) tkgrab.release(top)
          tkdestroy(top)
          next
        }
      }
      command <- paste(command, "\n    ", Z, " <- .Z[,", i, "]", sep="")
    }
    command <- paste(command, "\n  })\n})")
    result <- doItAndPrint(command)
    if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkfocus(CommanderWindow()) 
  }
  OKCancelHelp(helpSubject="scale")
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix()
}

helpDataSet <- function(){
	.activeDataSet <- ActiveDataSet()
	if (as.numeric(R.Version()$major) >= 2) doItAndPrint(paste('help("', .activeDataSet, '")', sep=""))
	else {
		justDoIt(paste("help('", .activeDataSet, "')", sep=""))
		logger(paste('help("', .activeDataSet, '")', sep=""))
	}
	NULL
}

variablesDataSet <- function(){
	doItAndPrint(paste("names(", ActiveDataSet(), ")", sep=""))
}

exportDataSet <- function() {
    dsname <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Export Active Data Set"))
    checkBoxes(frame="optionsFrame", boxes=c("colnames", "rownames", "quotes"),
               initialValues=rep(1,3), labels=gettextRcmdr(c("Write variable names", "Write row names", "Quotes around character values")))
    missingFrame <- tkframe(optionsFrame)
    missingVariable <- tclVar("NA")
    missingEntry <- ttkentry(missingFrame, width="8", textvariable=missingVariable)
    radioButtons(name="delimiter", buttons=c("spaces", "tabs", "commas", "semicolons"), 
                 labels=gettextRcmdr(c("Spaces", "Tabs", "Commas [,]", "Semicolons [;]")),
                 title=gettextRcmdr("Field Separator"), columns=2)
    otherDelimiterFrame <- tkframe(top)
    otherButton <- ttkradiobutton(otherDelimiterFrame, variable=delimiterVariable, value="other", text=gettextRcmdr("Other"))
    otherVariable <- tclVar("")
    otherEntry <- ttkentry(otherDelimiterFrame, width="4", textvariable=otherVariable)
    onOK <- function(){
        closeDialog()
        col <- tclvalue(colnamesVariable) == 1
        row <- tclvalue(rownamesVariable) == 1
        quote <- tclvalue(quotesVariable) == 1
        delim <- tclvalue(delimiterVariable)
        missing <- tclvalue(missingVariable)
        sep <- if (delim == "tabs") "\\t"
        else if (delim == "spaces") " "
        else if (delim == "commas") ","
        else if (delim == "semicolons") ";"
        else trim.blanks(tclvalue(otherVariable))
        saveFile <- tclvalue(tkgetSaveFile(filetypes=gettextRcmdr('{"All Files" {"*"}} {"Text Files" {".txt" ".TXT" ".dat" ".DAT" ".csv" ".CSV"}}'),
                                           defaultextension=if (delim == "commas") ".csv" else ".txt",
                                           initialfile=paste(dsname, if (delim == "commas") ".csv" else ".txt", sep=""),
                                           parent=CommanderWindow()))
        saveFile <- removeRedundantExtension(saveFile)
        if (saveFile == "") {
            tkfocus(CommanderWindow())
            return()
        }
        command <- paste("write.table(", dsname, ', "', saveFile, '", sep="', sep,
                         '", col.names=', col, ", row.names=", row, ", quote=", quote,
                         ', na="', missing, '")', sep="")
        justDoIt(command)
        logger(command)
        Message(paste(gettextRcmdr("Active dataset exported to file"), saveFile), type="note")
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="write.table")
    tkgrid(labelRcmdr(missingFrame, text=gettextRcmdr("Missing values:  ")), missingEntry, sticky="w")
    tkgrid(missingFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(delimiterFrame, stick="nw")
    tkgrid(otherButton,
           labelRcmdr(otherDelimiterFrame, text=gettextRcmdr("Specify:")), otherEntry, sticky="w", padx="3")
    tkgrid(otherDelimiterFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

filterNA <- function(){
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Remove Missing Data"))
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- ttkcheckbutton(allVariablesFrame, variable=allVariables, 
                                           text=gettextRcmdr("Include all variables"))
    variablesBox <- variableListBox(top, Variables(), selectmode="multiple", initialSelection=NULL,
                                    title=gettextRcmdr("Variables (select one or more)"))
    newDataSetName <- tclVar(gettextRcmdr("<same as active data set>"))
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        x <- getSelection(variablesBox)
        closeDialog()
        newName <- trim.blanks(tclvalue(newDataSetName))
        .activeDataSet <- ActiveDataSet()
        if (newName == gettextRcmdr("<same as active data set>")) newName <- .activeDataSet
        if (!is.valid.name(newName)){
            errorCondition(recall=filterNA,
                           message=paste('"', newName, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName, gettextRcmdr("Data set")))){
                filterNA()
                return()
            }
        }
        if (tclvalue(allVariables) == "1"){
            command <- paste(newName, " <- na.omit(", .activeDataSet, ")", sep="")
            logger(command)
            result <- justDoIt(command)
            if (class(result)[1] !=  "try-error") activeDataSet(newName)
            tkfocus(CommanderWindow())
        }
        else {
            if (length(x) == 0) {
                errorCondition(recall=filterNA, message=gettextRcmdr("No variables were selected."))
                return()
            }
            x <- paste('"', x, '"', sep="")
            command <- paste(newName, " <- na.omit(", .activeDataSet, "[,c(", paste(x, collapse=","),
                             ')])', sep="")
            logger(command)
            result <- justDoIt(command)
            if (class(result)[1] !=  "try-error") activeDataSet(newName)
            tkfocus(CommanderWindow())
        }
    }
    OKCancelHelp(helpSubject="na.omit")
    tkgrid(allVariablesCheckBox, sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("   OR"), fg="red"), sticky="w")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(labelRcmdr(dataSetNameFrame, text=gettextRcmdr("Name for new data set")), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

subsetDataSet <- function(){
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Subset Data Set"))
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar("1")
    allVariablesCheckBox <- ttkcheckbutton(allVariablesFrame, variable=allVariables)
    variablesBox <- variableListBox(top, Variables(), selectmode="multiple",
                                    initialSelection=NULL, title=gettextRcmdr("Variables (select one or more)"))
    subsetVariable <- tclVar(gettextRcmdr("<all cases>"))
    subsetFrame <- tkframe(top)
    subsetEntry <- ttkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- ttkscrollbar(subsetFrame, orient="horizontal",
                                 command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    newDataSetName <- tclVar(gettextRcmdr("<same as active data set>"))
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        newName <- trim.blanks(tclvalue(newDataSetName))
        if (newName == gettextRcmdr("<same as active data set>")) newName <- ActiveDataSet()
        if (!is.valid.name(newName)){
            errorCondition(recall=subsetDataSet,
                           message=paste('"', newName, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName, type=gettextRcmdr("Data set")))){
                closeDialog()
                subsetDataSet()
                return()
            }
        }
        selectVars <- if (tclvalue(allVariables) == "1") ""
        else {
            x <- getSelection(variablesBox)
            if (0 == length(x)) {
                errorCondition(recall=subsetDataSet,
                               message=gettextRcmdr("No variables were selected."))
                return()
            }
            paste(", select=c(", paste(x, collapse=","), ")", sep="")
        }
        closeDialog()
        cases <- tclvalue(subsetVariable)
        selectCases <- if (cases == gettextRcmdr("<all cases>")) ""
        else paste(", subset=", cases, sep="")
        if (selectVars == "" && selectCases ==""){
            errorCondition(recall=subsetDataSet,
                           message=gettextRcmdr("New data set same as active data set."))
            return()
        }
        command <- paste(newName, " <- subset(", ActiveDataSet(), selectCases, selectVars, ")",
                         sep="")
        logger(command)
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error") activeDataSet(newName)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="subset")
    tkgrid(allVariablesCheckBox, labelRcmdr(allVariablesFrame, text=gettextRcmdr("Include all variables")),
           sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("   OR"), fg="red"), sticky="w")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(labelRcmdr(subsetFrame, text=gettextRcmdr("Subset expression")), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(labelRcmdr(dataSetNameFrame, text=gettextRcmdr("Name for new data set")), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

setCaseNames <- function(){
	dataSet <- activeDataSet()
	initializeDialog(title=gettextRcmdr("Set Case Names"))
	variablesBox <- variableListBox(top, Variables(), title=gettextRcmdr("Select variable containing row names"),
			initialSelection=NULL)
	onOK <- function(){
		variable <- getSelection(variablesBox)
		closeDialog()
		if (length(variable) == 0) {
			errorCondition(recall=setCaseNames, message=gettextRcmdr("You must select a variable."))
			return()
		}
		var <- eval(parse(text=paste(dataSet, "$", variable, sep="")), envir=.GlobalEnv)
		if (length(var) != length(unique(var))){
			errorCondition(recall=setCaseNames, message=gettextRcmdr("Case names must be unique."))
			return()
		}
		command <- paste("row.names(", dataSet, ") <- as.character(", dataSet, "$", variable, ")", sep="")
		result <- justDoIt(command)
		logger(command)
		eval(parse(text=paste(dataSet, "$", variable, "<- NULL", sep="")), envir=.GlobalEnv)
		logger(paste(dataSet, "$", variable, " <- NULL", sep=""))
		if (class(result)[1] !=  "try-error") activeDataSet(dataSet, flushModel=FALSE, flushDialogMemory=FALSE)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="row.names")
	tkgrid(getFrame(variablesBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix()
}

renameVariables <- function(){
	initializeDialog(title=gettextRcmdr("Rename Variables"))
	variableBox <- variableListBox(top, Variables(), title=gettextRcmdr("Variables (pick one or more)"),
			selectmode="multiple", initialSelection=NULL)
	onOK <- function(){
		variables <- getSelection(variableBox)
		closeDialog()
		nvariables <- length(variables)
		if (nvariables < 1) {
			errorCondition(recall=renameVariables, message=gettextRcmdr("No variables selected."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		unordered.names <- names(get(.activeDataSet))
		which.variables <- match(variables, unordered.names)
		initializeDialog(subdialog, title=gettextRcmdr("Variable Names"))
		newnames <- rep("", nvariables)
		onOKsub <- function() {
			closeDialog(subdialog)
			for (i in 1:nvariables){
				newnames[i] <- eval(parse(text=paste("tclvalue(newName", i, ")", sep="")))
			}
			if (any(newnames == "")){
				errorCondition(recall=renameVariables, message=gettextRcmdr("A variable name is empty."))
				return()
			}
			test.names <- newnames == make.names(newnames)
			if (!all(test.names)){
				errorCondition(recall=renameVariables,
						message=paste(gettextRcmdr("The following variable names are not valid:\n"),
								paste(newnames[!test.names], collapse=", ")))
				return()
			}
			all.names <- names(get(.activeDataSet))
			all.names[which.variables] <- newnames
			if (length(unique(all.names)) != length(all.names)){
				errorCondition(recall=renameVariables, message=gettextRcmdr("Variable names are not unique"))
				return()
			}
			command <- paste("names(", .activeDataSet, ")[c(", paste(which.variables, collapse=","),
					")] <- c(", paste('"', newnames, '"', collapse=",", sep=""), ")", sep="")
			result <- justDoIt(command)
			logger(command)
			if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
			tkfocus(CommanderWindow())
		}
		subOKCancelHelp()
		tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Old Name"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"),
				labelRcmdr(subdialog, text=gettextRcmdr("New name"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
		for (i in 1:nvariables){
			valVar <- paste("newName", i, sep="")
			assign(valVar, tclVar(""))
			assign(paste("entry", i, sep=""), ttkentry(subdialog, width="20",
							textvariable=get(valVar)))
			tkgrid(labelRcmdr(subdialog, text=variables[i]), get(paste("entry", i, sep="")), sticky="w")
		}
		tkgrid(subButtonsFrame, sticky="w", columnspan=2)
		dialogSuffix(subdialog, focus=entry1, onOK=onOKsub, force.wait=TRUE)
	}
	OKCancelHelp(helpSubject="names")
	tkgrid(getFrame(variableBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix()
}

setContrasts <- function(){
  initializeDialog(title=gettextRcmdr("Set Contrasts for Factor"))
  variableBox <- variableListBox(top, Factors(), title=gettextRcmdr("Factor (pick one)"))
  radioButtons(name="contrasts", buttons=c("treatment", "sum", "helmert", "poly", "specify"),
               values=c("contr.Treatment", "contr.Sum", "contr.helmert", "contr.poly", "specify"),
               labels=gettextRcmdr(c("Treatment (dummy) contrasts", "Sum (deviation) contrasts", "Helmert contrasts",
                                     "Polynomial contrasts", "Other (specify)")), title=gettextRcmdr("Contrasts"),
               columns=2)
  onOK <- function(){
    variable <- getSelection(variableBox)
    closeDialog()
    if (length(variable) == 0) {
      errorCondition(recall=setContrasts, message=gettextRcmdr("You must select a variable."))
      return()
    }
    contrasts <- tclvalue(contrastsVariable)
    if (contrasts != "specify"){
      command <- paste("contrasts(", ActiveDataSet(), "$", variable, ') <- "', contrasts, '"', sep="")
      result <- justDoIt(command)
      logger(command)
      if (class(result)[1] !=  "try-error") activeDataSet(ActiveDataSet())
      tkfocus(CommanderWindow())
    }
    else{
      initializeDialog(subdialog, title=gettextRcmdr("Specify Contrasts"))
      tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Enter Contrast Coefficients"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
      env <- environment()
      tableFrame <- tkframe(subdialog)
      row.names <- eval(parse(text=paste("levels(", ActiveDataSet(), "$", variable, ")")))
      row.names <- substring(paste(abbreviate(row.names, 12), "            "), 1, 12)
      nrows <- length(row.names)
      ncols <- nrows - 1
      make.col.names <- paste("labelRcmdr(tableFrame, text='", gettextRcmdr("Contrast Name:"), "')", sep="")
      for (j in 1:ncols) {
        varname <- paste(".col.", j, sep="")
        assign(varname, tclVar(paste(".", j, sep="")), envir=env)
        make.col.names <- paste(make.col.names, ", ",
                                "ttkentry(tableFrame, width='12', textvariable=", varname, ")", sep="")
      }
      eval(parse(text=paste("tkgrid(", make.col.names, ", sticky='w')", sep="")), envir=env)
      for (i in 1:nrows){
        make.row <- paste("labelRcmdr(tableFrame, text='", row.names[i], "')")
        for (j in 1:ncols){
          varname <- paste(".tab.", i, ".", j, sep="")
          assign(varname, tclVar("0"), envir=env)
          make.row <- paste(make.row, ", ", "ttkentry(tableFrame, width='5', textvariable=",
                            varname, ")", sep="")
        }
        eval(parse(text=paste("tkgrid(", make.row, ", sticky='w')", sep="")), envir=env)
      }
      tkgrid(tableFrame, sticky="w")
      onOKsub <- function(){
        closeDialog(subdialog)
        cell <- 0
        values <- rep(NA, nrows*ncols)
        for (j in 1:ncols){
          for (i in 1:nrows){
            cell <- cell + 1
            varname <- paste(".tab.", i, ".", j, sep="")
            values[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
          }
        }
        values <- na.omit(values)
        if (length(values) != nrows*ncols){
          errorCondition(subdialog, recall=setContrasts,
                         message=sprintf(gettextRcmdr(
                           "Number of valid entries in contrast matrix(%d)\nnot equal to number of levels (%d) * number of contrasts (%d)."), length(values), nrows, ncols))
          return()
        }
        if (qr(matrix(values, nrows, ncols))$rank < ncols) {
          errorCondition(subdialog, recall=setContrasts, message=gettextRcmdr("Contrast matrix is not of full column rank"))
          return()
        }
        contrast.names <- rep("", ncols)
        for (j in 1:ncols){
          varname <- paste(".col.", j, sep="")
          contrast.names[j] <- eval(parse(text=paste("tclvalue(", varname,")", sep="")))
        }
        if (length(unique(contrast.names)) < ncols) {
          errorCondition(subdialog, recall=setContrasts, message=gettextRcmdr("Contrast names must be unique"))
          return()
        }
        command <- paste(".Contrasts <- matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
                         ")", sep="")
        doItAndPrint(command)
        command <- paste("colnames(.Contrasts) <- c(",
                         paste("'", contrast.names, "'", sep="", collapse=", "), ")", sep="")
        doItAndPrint(command)
        command <- paste("contrasts(", ActiveDataSet(), "$", variable, ") <- .Contrasts", sep="")
        result <- doItAndPrint(command)
        logger("remove(.Contrasts)")
        remove(.Contrasts, envir=.GlobalEnv)
        if (class(result)[1] !=  "try-error") activeDataSet(ActiveDataSet(), flushModel=FALSE, flushDialogMemory=FALSE)
        tkfocus(CommanderWindow())
      }
      subOKCancelHelp(helpSubject="contrasts")
      tkgrid(tableFrame, sticky="w")
      tkgrid(labelRcmdr(subdialog, text=""))
      tkgrid(subButtonsFrame, sticky="w")
      dialogSuffix(subdialog, focus=subdialog, force.wait=TRUE)
    }
  }
  OKCancelHelp(helpSubject="contrasts")
  tkgrid(getFrame(variableBox), sticky="nw")
  tkgrid(contrastsFrame, sticky="nw")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix()
}

refreshActiveDataSet <- function() activeDataSet(ActiveDataSet())

addObsNumbers <- function(){
	dsname <- ActiveDataSet()
	if (is.element("ObsNumber", listVariables())) {
		if ("no" == tclvalue(checkReplace("ObsNumber", gettextRcmdr("Variable")))){
			return()
		}
	}
	nrows <- nrow(get(dsname, envir=.GlobalEnv))
	command <- paste(dsname, "$ObsNumber <- 1:", nrows, sep="")
	logger(command)
	result <- justDoIt(command)
	if (class(result)[1] !=  "try-error") activeDataSet(dsname, flushModel=FALSE, flushDialogMemory=FALSE)
}

Stack <- function(){
	initializeDialog(title=gettextRcmdr("Stack Variables"))
	variableBox <- variableListBox(top, Numeric(), selectmode="multiple",
			title=gettextRcmdr("Variables (pick two or more)"))
	factorName <- tclVar("factor")
	factorNameField <- ttkentry(top, width="20", textvariable=factorName)
	variableName <- tclVar("variable")
	variableNameField <- ttkentry(top, width="20", textvariable=variableName)
	datasetName <- tclVar("StackedData")
	datasetNameField <- ttkentry(top, width="20", textvariable=datasetName)
	onOK <- function(){
		variables <- getSelection(variableBox)
		facname <- tclvalue(factorName)
		varname <- tclvalue(variableName)
		dsname <- tclvalue(datasetName)
		closeDialog()
		if (length(variables) < 2) {
			errorCondition(recall=Stack,
					message=gettextRcmdr("You must select at least two variables."))
			return()
		}
		if (!is.valid.name(facname)){
			errorCondition(recall=Stack,
					message=paste('"', facname, '" ', gettextRcmdr("is not a valid name."), sep=""))
			return()
		}
		if (!is.valid.name(varname)){
			errorCondition(recall=Stack,
					message=paste('"', varname, '" ', gettextRcmdr("is not a valid name."), sep=""))
			return()
		}
		if (!is.valid.name(dsname)){
			errorCondition(recall=Stack,
					message=paste('"', dsname, '" ', gettextRcmdr("is not a valid name."), sep=""))
			return()
		}
		if (is.element(dsname, listDataSets())) {
			if ("no" == tclvalue(checkReplace(dsname, gettextRcmdr("Data set")))){
				Stack()
				return()
			}
		}
		command <- paste(dsname, " <- stack(", activeDataSet(), "[, c(",
				paste(paste('"', variables, '"', sep=""), collapse=","), ")])", sep="")
		logger(command)
		result <- justDoIt(command)
		command <- paste("names(", dsname, ') <- c("', varname, '", "', facname, '")',
				sep="")
		logger(command)
		justDoIt(command)
		if (class(result)[1] !=  "try-error") activeDataSet(dsname)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="stack")
	tkgrid(getFrame(variableBox), sticky="nw", columnspan=2)
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(labelRcmdr(top,
					text=gettextRcmdr("Name for stacked data set:")), datasetNameField, sticky="w")
	tkgrid(labelRcmdr(top,
					text=gettextRcmdr("Name for variable:")), variableNameField, sticky="w")
	tkgrid(labelRcmdr(top,
					text=gettextRcmdr("Name for factor:")), factorNameField, sticky="w")
	tkgrid(buttonsFrame, sticky="w", columnspan=2)
	dialogSuffix(preventGrabFocus=TRUE)
}

loadDataSet <- function() {
	file <- tclvalue(tkgetOpenFile(filetypes=
							gettextRcmdr('{"All Files" {"*"}} {"R Data Files" {".RData" ".rda" ".Rda" ".RDA"}}')))
	if (file == "") return()
	setBusyCursor()
	on.exit(setIdleCursor())
	command <- paste('load("', file,'")', sep="")
	dsname <- justDoIt(command)
	logger(command)
	if (class(dsname)[1] !=  "try-error") {
	    if (length(dsname) > 1) {
	        Message(message=paste(gettextRcmdr("There is more than one object in the file, with the following names:\n"),
	                                     paste(dsname, collapse=", ")), type="error")
	        return()
	    }
	    activeDataSet(dsname)
	}
	tkfocus(CommanderWindow())
}

saveDataSet <- function() {
	file <- tclvalue(tkgetSaveFile(filetypes=
							gettextRcmdr('{"All Files" {"*"}} {"R Data Files" {".RData" ".rda" ".Rda" ".RDA"}}'),
					defaultextension=".RData", initialfile=paste(activeDataSet(), ".RData", sep="")))
	file <- removeRedundantExtension(file)
	if (file == "") return()
	file <- sub(".RData.RData$", ".RData", file)
	setBusyCursor()
	on.exit(setIdleCursor())
	command <- paste('save("', activeDataSet(), '", file="', file, '")', sep="")
	justDoIt(command)
	logger(command)
}

RemoveRows <- function(){
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Remove Rows from Active Data Set"))
    removeVariable <- tclVar(gettextRcmdr(""))
    removeFrame <- tkframe(top)
    removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
    removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                                 command=function(...) tkxview(removeEntry, ...))
    tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
    newDataSetName <- tclVar(gettextRcmdr("<same as active data set>"))
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        newName <- trim.blanks(tclvalue(newDataSetName))
        if (newName == gettextRcmdr("<same as active data set>")) newName <- ActiveDataSet()
        if (!is.valid.name(newName)){
            errorCondition(recall=RemoveRows,
                           message=paste('"', newName, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName, type=gettextRcmdr("Data set")))){
                closeDialog()
                RemoveRows()
                return()
            }
        }
        remove <- tclvalue(removeVariable)
        if (remove==""){
            errorCondition(recall=RemoveRows,
                           message="No rows to remove")
            closeDialog()
            return()
        }
        rowsToRemove <- removeRows <- paste("c(", gsub(" +", ", ", remove), ")", sep="")
        remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
        if (inherits(remove.rows, "try-error")){
          rowsToRemove <- removeRows <- paste("c('", gsub(" +", "', '", remove), "')", sep="")
          remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
          if (inherits(remove.rows, "try-error")){
            errorCondition(recall=RemoveRows,
                           message=remove.rows)
            closeDialog()
            return()
          }
        }
        if (is.numeric(remove.rows)){
          n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
          if (any(which.bad <- !remove.rows %in% 1:n)){
            errorCondition(recall=RemoveRows,
                           message=paste(gettextRcmdr("bad row numbers:"), 
                                         paste(as.character(remove.rows[which.bad]), collapse=", ")))
            closeDialog()
            return()
          }
        } else {
          if (any(which.bad <- eval(parse(text=paste("!", rowsToRemove, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
            errorCondition(recall=RemoveRows,
                           message=paste(gettextRcmdr("bad row names:"), 
                                         paste(remove.rows[which.bad], collapse=", ")))
            closeDialog()
            return()          
          }
        }
        closeDialog()
        removeRows <- if (is.numeric(remove.rows)) paste("-", removeRows, sep="")
                      else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
        command <- paste(newName, " <- ", ActiveDataSet(), "[", removeRows, ",]", sep="")
        logger(command)
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error") activeDataSet(newName)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="[.data.frame")
    tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or quoted names of row(s) to remove"),
                      foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
    tkgrid(removeEntry, sticky="w")
    tkgrid(removeScroll, sticky="ew")
    tkgrid(removeFrame, sticky="w")
    tkgrid(labelRcmdr(dataSetNameFrame, text=gettextRcmdr("Name for new data set")), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

SelectRows <- function(){
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Select Rows from Active Data Set"))
    selectVariable <- tclVar(gettextRcmdr(""))
    selectFrame <- tkframe(top)
    selectEntry <- ttkentry(selectFrame, width="60", textvariable=selectVariable)
    selectScroll <- ttkscrollbar(selectFrame, orient="horizontal",
                                 command=function(...) tkxview(selectEntry, ...))
    tkconfigure(selectEntry, xscrollcommand=function(...) tkset(selectScroll, ...))
    newDataSetName <- tclVar(gettextRcmdr("<same as active data set>"))
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        newName <- trim.blanks(tclvalue(newDataSetName))
        if (newName == gettextRcmdr("<same as active data set>")) newName <- ActiveDataSet()
        if (!is.valid.name(newName)){
            errorCondition(recall=SelectRows,
                           message=paste('"', newName, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName, type=gettextRcmdr("Data set")))){
                closeDialog()
                SelectRows()
                return()
            }
        }
        select <- tclvalue(selectVariable)
        if (select==""){
            errorCondition(recall=SelectRows, message="No rows to select")
            closeDialog()
            return()
        }
        
        rowsToSelect <- selectRows <- paste("c(", gsub(" +", ", ", select), ")", sep="")
        select.rows <- try(eval(parse(text=selectRows)), silent=TRUE)
        if (inherits(select.rows, "try-error")){
          rowsToSelect <- selectRows <- paste("c('", gsub(" +", "', '", select), "')", sep="")
          select.rows <- try(eval(parse(text=selectRows)), silent=TRUE)
          if (inherits(select.rows, "try-error")){
            errorCondition(recall=SelectRows,
                           message=select.rows)
            closeDialog()
            return()
          }
        }
        if (is.numeric(select.rows)){
          n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
          if (any(which.bad <- !select.rows %in% 1:n)){
            errorCondition(recall=SelectRows,
                           message=paste(gettextRcmdr("bad row numbers:"), 
                                         paste(as.character(select.rows[which.bad]), collapse=", ")))
            closeDialog()
            return()
          }
        } else {
          if (any(which.bad <- eval(parse(text=paste("!", rowsToSelect, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
            errorCondition(recall=SelectRows,
                           message=paste(gettextRcmdr("bad row names:"), 
                                         paste(select.rows[which.bad], collapse=", ")))
            closeDialog()
            return()          
          }
        }
        
        closeDialog()
        if (!is.numeric(select.rows)) selectRows <- paste0("rownames(", ActiveDataSet(), ") %in% ", selectRows)
        command <- paste0(newName, " <- ", ActiveDataSet(), "[", selectRows, ", ")
        if (ncol(get(ActiveDataSet())) == 1) command <- paste0(command, ", drop = FALSE")
        command <- paste0(command, "]")
        logger(command)
        result <- justDoIt(command)
        if (class(result)[1] !=  "try-error") activeDataSet(newName)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="[.data.frame")
    tkgrid(labelRcmdr(selectFrame, text=gettextRcmdr("Indices or quoted names of row(s) to select"),
                      foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
    tkgrid(selectEntry, sticky="w")
    tkgrid(selectScroll, sticky="ew")
    tkgrid(selectFrame, sticky="w")
    tkgrid(labelRcmdr(dataSetNameFrame, text=gettextRcmdr("Name for new data set")), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

mergeDataSets <- function(){
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()
    initializeDialog(title=gettextRcmdr("Merge Data Sets"))
    dsname <- tclVar("MergedDataset")
    dsnameFrame <- tkframe(top)
    entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
    dataSet1Box <- variableListBox(top, dataSets, title=gettextRcmdr("First Data Set (pick one)"),
                                   initialSelection=if (is.null(.activeDataSet)) NULL else which(.activeDataSet == dataSets) - 1)
    dataSet2Box <- variableListBox(top, dataSets, title=gettextRcmdr("Second Data Set (pick one)"))
    commonVar <- tclVar("0")
    commonFrame <- tkframe(top)
    commonButton <- ttkcheckbutton(commonFrame, variable=commonVar)    
    radioButtons(top, "direction", buttons=c("rows", "columns"), 
                 labels=gettextRcmdr(c("Merge rows", "Merge columns")), title=gettextRcmdr("Direction of Merge"))
    onOK <- function(){
        dsnameValue <- trim.blanks(tclvalue(dsname))
        if (dsnameValue == "") {
            errorCondition(recall=mergeDataSets,
                           message=gettextRcmdr("You must enter the name of a data set."))
            return()
        }
        if (!is.valid.name(dsnameValue)) {
            errorCondition(recall=mergeDataSets,
                           message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(dsnameValue, listDataSets())) {
            if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
                closeDialog()
                mergeDataSets()
                return()
            }
        }
        name1 <- getSelection(dataSet1Box)
        name2 <- getSelection(dataSet2Box)
        if (length(name1) == 0){
            errorCondition(recall=mergeDataSets,
                           message=gettextRcmdr("You must select a data set."))
            return()
        }
        if (length(name2) == 0){
            errorCondition(recall=mergeDataSets,
                           message=gettextRcmdr("You must select a data set."))
            return()
        }
        if (name1 == name2){
            errorCondition(recall=mergeDataSets,
                           message=gettextRcmdr("You cannot merge a data set with itself."))
            return()
        }
        common <- if (tclvalue(commonVar) == "1") TRUE else FALSE
        direction <- tclvalue(directionVariable)
        if (direction == "rows"){
            command <- paste(dsnameValue, " <- mergeRows(", name1, ", ", name2,
                             ", common.only=", common, ")", sep="")
            doItAndPrint(command)	
        }
        else {
            command <- paste(dsnameValue, " <- merge(", name1, ", ", name2,
                             ", all=", !common, ', by="row.names")', sep="")
            doItAndPrint(command)
            command <- paste("rownames(", dsnameValue, ") <- ", dsnameValue, "$Row.names", sep="")
            doItAndPrint(command)
            command <- paste(dsnameValue, "$Row.names <- NULL", sep="")
            doItAndPrint(command)
        }
        activeDataSet(dsnameValue)
        closeDialog()
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="mergeRows")
    tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Name for merged data set:  ")), entryDsname)
    tkgrid(dsnameFrame, sticky="w", columnspan=2)
    tkgrid(getFrame(dataSet1Box), getFrame(dataSet2Box), sticky="nw")
    tkgrid(commonButton, labelRcmdr(commonFrame, text=gettextRcmdr("Merge only common\nrows or columns")), 
           sticky="nw")
    tkgrid(directionFrame, commonFrame, sticky="sw")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    dialogSuffix()
}

Aggregate <- function(){
  .activeDataSet <- ActiveDataSet()
  initializeDialog(title=gettextRcmdr("Aggregate Observations"))
  dsname <- tclVar("AggregatedData")
  dsnameFrame <- tkframe(top)
  entryDsname <- ttkentry(dsnameFrame, width="20", textvariable=dsname)
  variablesBox <- variableListBox(top, Variables(), 
                                  title=gettextRcmdr("Variables to aggregate\n(pick one or more)"),
                                  selectmode="multiple")
  byBox <- variableListBox(top, Factors(), 
                           title=gettextRcmdr("Aggregate by\n(pick one or more)"),
                           selectmode="multiple")
  radioButtons(name="statistic", buttons=c("mean", "sum"), labels=gettextRcmdr(c("Mean", "Sum")), 
               title=gettextRcmdr("Statistic"))
  otherFrame <- tkframe(statisticFrame)
  otherVariable <- tclVar("")
  otherButton <- ttkradiobutton(otherFrame, variable=statisticVariable, value="other", 
                                text=gettextRcmdr("Other (specify)  "))
  otherEntry <- ttkentry(otherFrame, width="20", textvariable=otherVariable)   
  tkgrid(otherButton,  otherEntry, sticky="w")
  tkgrid(otherFrame, sticky="w")
  onOK <- function(){
    dsnameValue <- trim.blanks(tclvalue(dsname))
    if (dsnameValue == "") {
      errorCondition(recall=Aggregate,
                     message=gettextRcmdr("You must enter the name of a data set."))
      return()
    }
    if (!is.valid.name(dsnameValue)) {
      errorCondition(recall=Aggregate,
                     message=paste('"', dsnameValue, '" ', gettextRcmdr("is not a valid name."), sep=""))
      return()
    }
    if (is.element(dsnameValue, listDataSets())) {
      if ("no" == tclvalue(checkReplace(dsnameValue, gettextRcmdr("Data set")))){
        Aggregate()
        return()
      }
    }
    variables <- getSelection(variablesBox)
    byVariables <- getSelection(byBox)
    if (length(variables) == 0){
      errorCondition(recall=Aggregate,
                     message=gettextRcmdr("You must select at least one variable to aggregate."))
      return()
    }
    if (length(byVariables) == 0){
      errorCondition(recall=Aggregate,
                     message=gettextRcmdr("You must select at least one variable to aggregate by."))
      return()
    }
    if (any(byVariables %in% variables)){
      errorCondition(recall=Aggregate,
                     message=gettextRcmdr("Variables to aggregate and those to aggregate by must be different."))
      return()
    }
    statistic <- tclvalue(statisticVariable)
    if (statistic == "other") statistic <- tclvalue(otherVariable)
response <- if (length(variables) > 1) {
  paste("cbind(", paste(variables, collapse=", "), ")", sep="")
} else variables
rhs <- paste(byVariables, collapse=" + ")
command <- paste(dsnameValue, "<- aggregate(", response ," ~ ", rhs, ", data=", .activeDataSet, ", FUN=", 
                 statistic, ")", sep="")
doItAndPrint(command)
if (exists(dsnameValue) && is.data.frame(eval(parse(text=dsnameValue)))) activeDataSet(dsnameValue)
closeDialog()
tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="aggregate")
  tkgrid(labelRcmdr(dsnameFrame, text=gettextRcmdr("Name for aggregated data set:  ")), entryDsname)
  tkgrid(dsnameFrame, sticky="w", columnspan=2)
  tkgrid(getFrame(variablesBox), getFrame(byBox), sticky="nw")
  tkgrid(statisticFrame, sticky="w", columnspan=2)
  tkgrid(buttonsFrame, sticky="w", columnspan=2)
  dialogSuffix()
}

dropUnusedFactorLevels <- function(){
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Drop Unused Factor Levels"))
    allfactorsVariable <- tclVar("0")
    allFrame <- tkframe(top)
    allfactorsCheckBox <- ttkcheckbutton(allFrame, variable = allfactorsVariable)
    variablesBox <- variableListBox(top, Factors(),
        title=gettextRcmdr("Factors(s) to drop levels (pick one or more)"), selectmode="multiple",
        initialSelection=NULL)
    onOK <- function(){
        all <- tclvalue(allfactorsVariable)
        variables <- getSelection(variablesBox)
        closeDialog()
        if (all == 0 && length(variables) == 0) {
            errorCondition(recall=deleteVariable, message=gettextRcmdr("You must select one or more variables."))
            return()
        }
        response <- tclvalue(RcmdrTkmessageBox(message=gettextRcmdr("Drop unused factor levels\nPlease confirm."), 
            icon="warning", type="okcancel", default="cancel"))
        if (response == "cancel") {
            onCancel()
            return()
        }
        if (all == 1) command <- paste(dataSet, " <- droplevels(", dataSet, ")", sep="")
        else{
            command <- paste(dataSet, " <- within(", dataSet, ", {", sep="")
            for (variable in variables){
                command <- paste(command, "\n  ", variable, " <- droplevels(", variable, ")", sep="")
            }
            command <- paste(command, "\n})")
        }
        doItAndPrint(command)
        activeDataSet(dataSet, flushModel=FALSE, flushDialogMemory=FALSE)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="droplevels")
    tkgrid(allfactorsCheckBox, labelRcmdr(allFrame, text=gettextRcmdr("all factors")), sticky="w")
    tkgrid(allFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("OR"), fg="red"), sticky="w")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

viewData <- function(){
    defaults <- list (initial.allVariables = "1", initial.variables = NULL, 
                      initial.subset=gettextRcmdr("<all cases>"))
    dialog.values <- getDialog ("viewData", defaults)
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("View Data"))
    allVariablesFrame <- tkframe(top)
    allVariables <- tclVar(dialog.values$initial.allVariables)
    allVariablesCheckBox <- ttkcheckbutton(allVariablesFrame, variable=allVariables)
    variablesBox <- variableListBox(top, Variables(), selectmode="multiple",
                                    initialSelection= varPosn (dialog.values$initial.variables), 
                                    title=gettextRcmdr("Variables (select one or more)"))
    subsetVariable <- tclVar(gettextRcmdr(dialog.values$initial.subset))
    subsetFrame <- tkframe(top)
    subsetEntry <- ttkentry(subsetFrame, width="20", textvariable=subsetVariable)
    subsetScroll <- ttkscrollbar(subsetFrame, orient="horizontal",
                                 command=function(...) tkxview(subsetEntry, ...))
    tkconfigure(subsetEntry, xscrollcommand=function(...) tkset(subsetScroll, ...))
    onOK <- function(){
        selectVars <- if (tclvalue(allVariables) == "1") {
            x <- ""
            x
        }
        else {
            x <- getSelection(variablesBox)
            if (0 == length(x)) {
                errorCondition(recall=viewData,
                               message=gettextRcmdr("No variables were selected."))
                return()
            }
            paste(", select=c(", paste(x, collapse=","), ")", sep="")
        }
        closeDialog()
        cases <- tclvalue(subsetVariable)
        selectCases <- if (cases == gettextRcmdr("<all cases>")) ""
        else paste(", subset=", cases, sep="")
        view.height <- max(getRcmdr("output.height") + getRcmdr("log.height"), 10)
        dim <- dim(get(ActiveDataSet()))
        nrows <- dim[1]
        ncols <- dim[2]
        threshold <- getRcmdr("showData.threshold")
        suppress <- if(getRcmdr("suppress.X11.warnings")) ", suppress.X11.warnings=FALSE" else ""
        result <- try(assign(dataSet, eval(parse(text=paste("subset(", dataSet, 
                                                      selectCases, selectVars, ")", sep="")))),
                      silent=TRUE)
        if (class(result)[1] ==  "try-error"){
            errorCondition(recall=viewData,
                           message=gettextRcmdr("Bad subset expression."))
            return()
        }
        if (nrows == 0){
            errorCondition(recall=viewData,
                           message=gettextRcmdr("No data to show."))
            return()
        }
        posn <- commanderPosition() + c(as.numeric(tkwinfo("width", CommanderWindow())) + 10, 10)
        command <- if (nrows <= threshold[1] && ncols <= threshold[2]){
            paste("showData(as.data.frame(", dataSet, "), title='", ActiveDataSet(), "', placement='+", posn[1], "+", posn[2],
                  "', font=getRcmdr('logFont'), maxwidth=",
                  getRcmdr("log.width"), ", maxheight=", view.height, suppress, ")", sep="")
        }
        else paste("View(as.data.frame(", dataSet, "))", sep="")
        result <- try(eval(parse(text=command)), silent=TRUE)
        if (class(result)[1] ==  "try-error"){
            errorCondition(recall=viewData,
                           message=gettextRcmdr("View data error."))
            return()
        }
        putDialog ("viewData", list(initial.allVariables = tclvalue(allVariables), 
                                    initial.variables = if (x[1] == "") NULL else x, initial.subset=cases))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="showData", helpPackage="relimp", reset="viewData")
    tkgrid(allVariablesCheckBox, labelRcmdr(allVariablesFrame, text=gettextRcmdr("Include all variables")),
           sticky="w")
    tkgrid(allVariablesFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("   OR"), fg="red"), sticky="w")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(labelRcmdr(subsetFrame, text=gettextRcmdr("Subset expression")), sticky="w")
    tkgrid(subsetEntry, sticky="w")
    tkgrid(subsetScroll, sticky="ew")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

sortDataSet <- function(){
    dataSet <- activeDataSet()
    initializeDialog(title=gettextRcmdr("Sort Active Data Set"))
    variablesBox <- variableListBox(top, Variables(), selectmode="multiple",
                                    initialSelection=NULL, 
                                    title=gettextRcmdr("Sort Keys (select one or more)"))
    radioButtons(name="direction",
                 buttons=c("increasing", "decreasing"),
                 values=c("FALSE", "TRUE"),
                 labels=gettextRcmdr(c("Increasing", "Decreasing")), 
                 title=gettextRcmdr("Sort Direction"))
    newDataSetName <- tclVar(gettextRcmdr("<same as active data set>"))
    dataSetNameFrame <- tkframe(top)
    dataSetNameEntry <- ttkentry(dataSetNameFrame, width="25", textvariable=newDataSetName)
    onOK <- function(){
        newName <- trim.blanks(tclvalue(newDataSetName))
        if (newName == gettextRcmdr("<same as active data set>")) newName <- ActiveDataSet()
        if (!is.valid.name(newName)){
            errorCondition(recall=sortDataSet,
                           message=paste('"', newName, '" ', gettextRcmdr("is not a valid name."), sep=""))
            return()
        }
        if (is.element(newName, listDataSets())) {
            if ("no" == tclvalue(checkReplace(newName, type=gettextRcmdr("Data set")))){
                closeDialog()
                sortDataSet()
                return()
            }
        }
        x <- getSelection(variablesBox)
        if (0 == length(x)) {
            errorCondition(recall=sortDataSet,
                           message=gettextRcmdr("No variables were selected."))
            return()
        }
        direction <- tclvalue(directionVariable)
        closeDialog()
        if (length(x) > 1){
            initializeDialog(subdialog, title=gettextRcmdr("Reorder Sort Keys"))
            nvalues <- length(x)
            order <- 1:nvalues
            onOKsub <- function() {
                closeDialog(subdialog)
                opt <- options(warn=-1)
                for (i in 1:nvalues){
                    order[i] <- as.numeric(eval(parse(text=paste("tclvalue(keyOrder", i, ")", sep=""))))
                }
                options(opt)
                if (any(sort(order) != 1:nvalues) || any(is.na(order))){
                    errorCondition(recall=sortDataSet,
                                   message=paste(gettextRcmdr("Order of keys must include all integers from 1 to "), 
                                                 nvalues, sep=""))
                    return()
                }
                x <- x[order(order)]
                doItAndPrint(paste(newName, " <- with(", dataSet, ", ", dataSet, "[order(", 
                                   paste(x, collapse=", "), ", decreasing=", direction, "), ])", sep=""))
                activeDataSet(newName)
            }
            subOKCancelHelp()
            tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Sort Keys"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"),
                   labelRcmdr(subdialog, text=gettextRcmdr("Order"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
            for (i in 1:length(x)){
                valVar <- paste("keyOrder", i, sep="")
                assign(valVar, tclVar(i))
                assign(paste("entry", i, sep=""), ttkentry(subdialog, width="2",
                                                           textvariable=get(valVar)))
                tkgrid(labelRcmdr(subdialog, text=x[i]), get(paste("entry", i, sep="")), sticky="w")
            }
            tkgrid(subButtonsFrame, sticky="w", columnspan=2)
            dialogSuffix(subdialog, focus=entry1, force.wait=TRUE)
        }
        else {
            doItAndPrint(paste(newName, " <- with(", dataSet, ", ", dataSet, "[order(", x, ", decreasing=", direction, "), ])", sep=""))
            activeDataSet(newName)
        }

        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="order")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(directionFrame, sticky="w")
    tkgrid(labelRcmdr(dataSetNameFrame, text=gettextRcmdr("Name for new data set")), sticky="w")
    tkgrid(dataSetNameEntry, sticky="w")
    tkgrid(dataSetNameFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
    }

reshapeLong2Wide <- function () {
  defaults <- list(initial.id = NULL, initial.within=NULL, initial.varying=NULL, initial.ignore=NULL, initial.makeactive="1")
  dialog.values <- getDialog("reshapeLong2Wide", defaults)
  initializeDialog(title = gettextRcmdr("Reshape Data Set from Long to Wide Format"))
  .activeDataSet <- ActiveDataSet()
  optionsFrame <- tkframe(top)
  makeactiveVariable <- tclVar(dialog.values$initial.makeactive)
  makeactiveCheckBox <- ttkcheckbutton(optionsFrame, variable = makeactiveVariable)
  newDatasetName <- tclVar(paste0(.activeDataSet, "Wide"))
  newDatasetField <- ttkentry(optionsFrame, width = "20", 
                              textvariable = newDatasetName)
  dataFrame <- tkframe(top)
  idBox <- variableListBox(dataFrame, Variables(), title = gettextRcmdr("Subject ID variable (pick one)"),
                           initialSelection = varPosn(dialog.values$initial.id, "all"))
  withinBox <- variableListBox(dataFrame, Factors(), title = gettextRcmdr("Within-subjects factors (pick one or more)"),
                               initialSelection = varPosn(dialog.values$initial.within, "factor"), selectmode="multiple")
  varyingBox <- variableListBox(dataFrame, Variables(), title = gettextRcmdr("Variables that vary by occasion (pick one or more)"),
                                initialSelection = varPosn(dialog.values$initial.varying, "all"), selectmode="multiple")
  ignoreBox <- variableListBox(dataFrame, Variables(), title = gettextRcmdr("Variables to ignore (pick zero or more)"),
                               initialSelection = varPosn(dialog.values$initial.ignore, "all"), selectmode="multiple")
  onOK <- function() {
    id <- getSelection(idBox)
    within <- getSelection(withinBox)
    varying <- getSelection(varyingBox)
    ignore <- getSelection(ignoreBox)
    if (length(id) == 0) {
      errorCondition(recall = reshapeLong2Wide, message = gettextRcmdr("You must select an ID variable."))
      return()
    }
    if (length(within) == 0 ){
      errorCondition(recall = reshapeLong2Wide, message = gettextRcmdr("You must select one or more within-subjects factors."))
      return()
    }
    if (length(varying) == 0 ){
      errorCondition(recall = reshapeLong2Wide, message = gettextRcmdr("You must select one or more occasion-varying variables."))
      return()
    }
    all <- c(id, within, varying, ignore)
    duplicated <- unique(all[duplicated(all)])
    if (length(duplicated) > 0) {
      errorCondition(recall = reshapeLong2Wide, 
                     message = paste(gettextRcmdr("the following variables appear more than once:"), 
                                                  paste(duplicated, collapse=", ")))
      return()
    }
    makeactive <- tclvalue(makeactiveVariable)
    putDialog ("reshapeLong2Wide", list (initial.id = id, initial.within=within, 
                                         initial.varying=varying, initial.ignore=ignore,
                                         initial.makeactive=makeactive))
    newDatasetNameValue <- tclvalue(newDatasetName)
    if (newDatasetNameValue %in% listDataSets()){
      if ("no" == tclvalue(checkReplace(newDatasetNameValue, type=gettextRcmdr("Data set")))){
        errorCondition(recall = reshapeLong2Wide, message = gettextRcmdr("Save data set aborted"))
        return()
      }
    }
    closeDialog()
    within <- if (length(within) > 1) paste0("c(", paste(paste0('"', within, '"'), collapse=", "), ")") else paste0('"', within, '"')
    varying <-if (length(varying) > 1) paste0("c(", paste(paste0('"', varying, '"'), collapse=", "), ")") else paste0('"', varying, '"')
    ignore <-if (length(ignore) == 0) {
      NULL 
    } else if (length(ignore) > 1) {
      paste0(", ignore=c(", paste(paste0('"', ignore, '"'), collapse=", "), ")") 
    } else {
      paste0(', ignore="', ignore, '"')
    }
    id <- paste0('"', id, '"')
    doItAndPrint(paste0(newDatasetNameValue, " <- reshapeL2W(", .activeDataSet, 
                        ", within=", within, ", id=", id, ", varying=", varying, ignore, ")"))
    if (makeactive == "1") activeDataSet(newDatasetNameValue)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "ReshapeDatasetDialogs", reset = "reshapeLong2Wide")
  tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Name for wide data set: ")),
         newDatasetField, sticky="w")
  tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Make the wide data set active")),
         makeactiveCheckBox, sticky="w")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(getFrame(idBox), labelRcmdr(dataFrame, text="  "), getFrame(withinBox), sticky = "nw")
  tkgrid(getFrame(varyingBox), labelRcmdr(dataFrame, text="  "), getFrame(ignoreBox),sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  dialogSuffix(grid.buttons=TRUE)
}

reshapeWide2Long <- function () {
  checkResponses <- function(x){
    rows <- any(apply(x, 1, 
                      function(r){
                        if (!any(r)) FALSE else max(which(r)) > sum(r)
                      }
    ))
    cols <- any(apply(x, 2, 
                      function(c) {
                        if (!any(c)) FALSE else max(which(c)) > sum(c)
                      }
    ))
    test1 <- !(rows || cols)
    r <- sum(x[, 1])
    c <- sum(x[1, ])
    test2 <- all(x[r, c])
    test1 && test2
  }
  defaults <- list(initial.rm=matrix("", 5, 5), initial.rm.1=rep("", 8),
                   initial.level=paste0("Level-", 1:8),
                   initial.colLevel=paste0("Column-", 1:5),
                   initial.rowLevel=paste0("Row-", 1:5),
                   initial.wsrowfactorName="RowFactor",
                   initial.wscolfactorName="ColumnFactor",
                   initial.wsfactorName="trials", initial.tab=0,
                   initial.makeactive="1",
                   initial.ignore=NULL)
  dialog.values <- getDialog ("reshapeWide2Long", defaults)
  initializeDialog(title = gettextRcmdr("Reshape Data Set from Wide to Long Format"),
                   use.tabs=TRUE, tabs=c("oneFactorTab", "twoFactorsTab", "optionsTab"))
  
  .activeDataSet <- ActiveDataSet()
  makeactiveVariable <- tclVar(dialog.values$initial.makeactive)
  makeactiveCheckBox <- ttkcheckbutton(optionsTab, variable = makeactiveVariable)
  newDatasetName <- tclVar(paste0(.activeDataSet, "Long"))
  newDatasetField <- ttkentry(optionsTab, width = "20", textvariable = newDatasetName)
  idName <- tclVar("id")
  idField <- ttkentry(optionsTab, width = "20", textvariable = idName)
  responseName <- tclVar("response")
  responseField <- ttkentry(optionsTab, width = "20", textvariable = responseName)
  ignoreBox <- variableListBox(optionsTab, Variables(), title = gettextRcmdr("Variables to ignore (pick zero or more)"),
                               initialSelection = varPosn(dialog.values$initial.ignore, "all"), selectmode="multiple")
  onOK <- function() {
    tab <- if (as.character(tkselect(notebook)) == oneFactorTab$ID) 0 
    else if (as.character(tkselect(notebook)) == twoFactorsTab$ID) 1 
    else 2
    makeactive <- tclvalue(makeactiveVariable)
    ignoreValue <- getSelection(ignoreBox)
    ignore <-if (length(ignoreValue) == 0) {
      NULL 
    } else if (length(ignoreValue) > 1) {
      paste0(", ignore=c(", paste(paste0('"', ignoreValue, '"'), collapse=", "), ")") 
    } else {
      paste0(', ignore="', ignoreValue, '"')
    }
    id <- tclvalue(idName)
    response <- tclvalue(responseName)
    newDatasetNameValue <- tclvalue(newDatasetName)
    if (newDatasetNameValue %in% listDataSets()){
      if ("no" == tclvalue(checkReplace(newDatasetNameValue, type=gettextRcmdr("Data set")))){
        errorCondition(recall = reshapeWide2Long, message = gettextRcmdr("Save data set aborted"))
        return()
      }
    }
    rm1 <- getSelection(rm1ComboxBox)  
    rm2 <- getSelection(rm2ComboxBox)  
    rm3 <- getSelection(rm3ComboxBox)  
    rm4 <- getSelection(rm4ComboxBox)  
    rm5 <- getSelection(rm5ComboxBox)  
    rm6 <- getSelection(rm6ComboxBox) 
    rm7 <- getSelection(rm7ComboxBox)  
    rm8 <- getSelection(rm8ComboxBox)  
    responses.1 <- list(rm1, rm2, rm3, rm4, rm5, rm6, rm7, rm8)
    save.responses.1 <- unlist(responses.1)
    selected.1 <- !(responses.1 %in% c("", "<no selection>"))
    oneway <- any(selected.1)
    if (oneway && max(which(selected.1)) > sum(selected.1)){
      Message(gettextRcmdr("the specified responses are not contiguous\n missing responses are removed"), type="warning")
    }
    responses.1 <- lapply(responses.1, function(x) if (x %in% c("", gettextRcmdr("<no selection>"))) NULL else x)
    wsfactorName <- tclvalue(wsfactorNameVariable)
    if (oneway && !is.valid.name(wsfactorName)){
      errorCondition(recall=reshapeWide2Long, message=paste(wsfactorName, gettextRcmdr("is not a valid name")))
      return()
    }
    responses.1 <- unlist(responses.1)
    duplicates.1 <- duplicated(responses.1)
    if (oneway && any(duplicates.1)){
      errorCondition(recall=reshapeWide2Long, message=paste(gettextRcmdr("the following responses appear more than once:"),
                                                            paste(responses.1[duplicates.1], collapse=", ")))
      return()
    }
    m.1 <- length(responses.1)
    if (oneway && m.1 < 2){
      errorCondition(recall=reshapeWide2Long, message=gettextRcmdr("at least 2 responses must be specified"))
      return()
    }
    rm11 <- getSelection(rm11ComboxBox)  
    rm12 <- getSelection(rm12ComboxBox)  
    rm13 <- getSelection(rm13ComboxBox)  
    rm14 <- getSelection(rm14ComboxBox)  
    rm15 <- getSelection(rm15ComboxBox)  
    rm21 <- getSelection(rm21ComboxBox) 
    rm22 <- getSelection(rm22ComboxBox)  
    rm23 <- getSelection(rm23ComboxBox)  
    rm24 <- getSelection(rm24ComboxBox)  
    rm25 <- getSelection(rm25ComboxBox)  
    rm31 <- getSelection(rm31ComboxBox)  
    rm32 <- getSelection(rm32ComboxBox)  
    rm33 <- getSelection(rm33ComboxBox)  
    rm34 <- getSelection(rm34ComboxBox) 
    rm35 <- getSelection(rm35ComboxBox)  
    rm41 <- getSelection(rm41ComboxBox)  
    rm42 <- getSelection(rm42ComboxBox)  
    rm43 <- getSelection(rm43ComboxBox)  
    rm44 <- getSelection(rm44ComboxBox)  
    rm45 <- getSelection(rm45ComboxBox)  
    rm51 <- getSelection(rm51ComboxBox)  
    rm52 <- getSelection(rm52ComboxBox) 
    rm53 <- getSelection(rm53ComboxBox)  
    rm54 <- getSelection(rm54ComboxBox)
    rm55 <- getSelection(rm55ComboxBox)
    responses <- list(rm11, rm12, rm13, rm14, rm15, 
                      rm21, rm22, rm23, rm24, rm25,
                      rm31, rm32, rm33, rm34, rm35,
                      rm41, rm42, rm43, rm44, rm45,
                      rm51, rm52, rm53, rm54, rm55)
    responses <- matrix(responses, nrow=5, ncol=5, byrow=TRUE)
    selected <- !(responses %in% c("", gettextRcmdr("<no selection>")))
    if (oneway && any(selected)){
      errorCondition(recall=reshapeWide2Long,
                     message=gettextRcmdr("both one and two repeated-measures factors were specified\nspecify either a one-way or two-way design"))
      return()
    }
    selected <- matrix(selected, nrow=5, ncol=5)
    if (!oneway && !checkResponses(selected)){
      errorCondition(recall=reshapeWide2Long, message=gettextRcmdr("the specified responses are not contiguous"))
      return()
    }
    row1 <- selected[1, ]
    ncol <- if (any(row1)) max(which(row1)) else 0
    col1 <- selected[, 1]
    nrow <- if (any(col1)) max(which(col1)) else 0
    if (!oneway && ncol == 0){
      errorCondition(recall=reshapeWide2Long, message=gettextRcmdr("no responses specified"))
      return()
    }
    if (!oneway && (ncol < 2 || nrow < 2)){
      errorCondition(recall=reshapeWide2Long, message=gettextRcmdr("at least 2 rows and 2 columns of responses must be specified"))
      return()
    }
    responses <- as.vector(responses)
    save.responses <- unlist(responses)
    save.responses <- matrix(responses, 5, 5)
    responses <- responses[selected]
    wsrowfactorName <- tclvalue(wsrowfactorNameVariable)
    wscolfactorName <- tclvalue(wscolfactorNameVariable)
    if (!oneway && !is.valid.name(wsrowfactorName)){
      errorCondition(recall=reshapeWide2Long, message=paste(wsrowfactorName, gettextRcmdr("is not a valid name")))
      return()
    }
    if (!oneway && !is.valid.name(wscolfactorName)){
      errorCondition(recall=reshapeWide2Long, message=paste(wscolfactorName, gettextRcmdr("is not a valid name")))
      return()
    }
    level1 <- tclvalue(level1variable)
    level2 <- tclvalue(level2variable)
    level3 <- tclvalue(level3variable)
    level4 <- tclvalue(level4variable)
    level5 <- tclvalue(level5variable)
    level6 <- tclvalue(level6variable)
    level7 <- tclvalue(level7variable)
    level8 <- tclvalue(level8variable)
    levels <- c(level1, level2, level3, level4, level5, level6, level7, level8)[selected.1]
    nlevels <- length(levels)
    if(nlevels < 8) levels <- c(levels, paste0("Level-", as.character((nlevels + 1):8)))
    duplicated.levels <- duplicated(levels)
    if (oneway && any(duplicated(levels))){
      errorCondition(recall=reshapeWide2Long, 
                     message=paste0(gettextRcmdr("there are duplicated level names"), ":\n ", 
                                    paste(unique(levels[duplicated.levels]), collapse=", ")))
      return()
    }
    colLevel1 <- tclvalue(colLevel1variable)
    colLevel2 <- tclvalue(colLevel2variable)
    colLevel3 <- tclvalue(colLevel3variable)
    colLevel4 <- tclvalue(colLevel4variable)
    colLevel5 <- tclvalue(colLevel5variable)
    colLevels <- c(colLevel1, colLevel2, colLevel3, colLevel4, colLevel5)
    rowLevel1 <- tclvalue(rowLevel1variable)
    rowLevel2 <- tclvalue(rowLevel2variable)
    rowLevel3 <- tclvalue(rowLevel3variable)
    rowLevel4 <- tclvalue(rowLevel4variable)
    rowLevel5 <- tclvalue(rowLevel5variable)
    rowLevels <- c(rowLevel1, rowLevel2, rowLevel3, rowLevel4, rowLevel5)
    duplicated.levels <- duplicated(rowLevels)
    if (!oneway && any(duplicated(rowLevels))){
      errorCondition(recall=reshapeWide2Long, 
                     message=paste0(gettextRcmdr("there are duplicated row level names"), ":\n ", 
                                    paste(unique(rowLevels[duplicated.levels]), collapse=", ")))
      return()
    }    
    duplicated.levels <- duplicated(colLevels)
    if (!oneway && any(duplicated(colLevels))){
      errorCondition(recall=reshapeWide2Long, 
                     message=paste0(gettextRcmdr("there are duplicated column level names"), ":\n ", 
                                    paste(unique(colLevels[duplicated.levels]), collapse=", ")))
      return()
    }    
    putDialog ("reshapeWide2Long", list(initial.rm=save.responses, initial.tab=tab, 
                                        initial.wsrowfactorName=wsrowfactorName,
                                        initial.wscolfactorName=wscolfactorName,
                                        initial.rm.1=save.responses.1, initial.wsfactorName=wsfactorName,
                                        initial.level=levels, initial.rowLevel=rowLevels, initial.colLevel=colLevels,
                                        initial.makeactive=makeactive, initial.ignore=ignoreValue))
    responses <- unlist(responses)
    duplicates <- duplicated(responses)
    if (any(duplicates)){
      errorCondition(recall=reshapeWide2Long, message=paste(gettextRcmdr("the following responses appear more than once:"),
                                                            paste(responses[duplicates], collapse=", ")))
      return()
    }
    if (any(bad <- ignoreValue %in% c(responses, responses.1))){
      errorCondition(recall=reshapeWide2Long, message=paste(gettextRcmdr("the following ignored variables are also responses:"),
                                                            paste(ignoreValue[bad], collapse=", ")))
      return()
    }
    closeDialog()
    m <- length(responses)
    res <- if (m > 0) as.vector(t(matrix(responses, nrow, ncol)))
    command <- if (oneway){
      paste0(newDatasetNameValue,' <- reshapeW2L(', .activeDataSet, ', within="', wsfactorName, 
             '", levels=list(', wsfactorName, '=c(', paste(paste0('"', levels[1:m.1], '"'), collapse=", "), 
             ')), varying=list(', response, '=c(', paste(paste0('"', responses.1, '"'), collapse=', '),'))', ignore, ', id="', id, '")')
    } else {
      paste0(newDatasetNameValue,' <- reshapeW2L(', .activeDataSet, ', within=c("', wsrowfactorName, '", "', wscolfactorName, 
             '"), levels=list(', wsrowfactorName, '=c(', paste(paste0('"', rowLevels[1:nrow], '"'), collapse=", "), '), ', 
             wscolfactorName, '=c(', paste(paste0('"', colLevels[1:ncol], '"'), collapse=", "), ')), varying=list(',
             response, '=c(', paste(paste0('"', res, '"'), collapse=', '),'))', ignore, ', id="', id, '")')
    }
    doItAndPrint(command)
    if (makeactive == "1") activeDataSet(newDatasetNameValue)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "ReshapeDatasetDialogs", reset = "reshapeWide2Long")
  oneFactorFrame <- ttklabelframe(oneFactorTab, labelwidget=tklabel(top, text = gettextRcmdr("Specify one within-subject factor"),
                                                                    font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  variableNameFrame.1 <- tkframe(oneFactorFrame)
  wsfactorNameVariable <- tclVar(dialog.values$initial.wsfactorName)
  wsfactorNameBox <-ttkentry(variableNameFrame.1, width="20", textvariable=wsfactorNameVariable)
  rm1ComboxBox <- variableComboBox(oneFactorFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm.1[[1]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm2ComboxBox <- variableComboBox(oneFactorFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm.1[[2]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm3ComboxBox <- variableComboBox(oneFactorFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm.1[[3]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm4ComboxBox <- variableComboBox(oneFactorFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm.1[[4]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm5ComboxBox <- variableComboBox(oneFactorFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm.1[[5]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm6ComboxBox <- variableComboBox(oneFactorFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm.1[[6]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm7ComboxBox <- variableComboBox(oneFactorFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm.1[[7]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm8ComboxBox <- variableComboBox(oneFactorFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm.1[[8]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  level1variable <- tclVar(dialog.values$initial.level[[1]])
  level1 <-ttkentry(oneFactorFrame, width="10", textvariable=level1variable, background="gray")
  level2variable <- tclVar(dialog.values$initial.level[[2]])
  level2 <-ttkentry(oneFactorFrame, width="10", textvariable=level2variable)
  level3variable <- tclVar(dialog.values$initial.level[[3]])
  level3 <-ttkentry(oneFactorFrame, width="10", textvariable=level3variable)
  level4variable <- tclVar(dialog.values$initial.level[[4]])
  level4 <-ttkentry(oneFactorFrame, width="10", textvariable=level4variable)
  level5variable <- tclVar(dialog.values$initial.level[[5]])
  level5 <-ttkentry(oneFactorFrame, width="10", textvariable=level5variable)
  level6variable <- tclVar(dialog.values$initial.level[[6]])
  level6 <-ttkentry(oneFactorFrame, width="10", textvariable=level6variable)
  level7variable <- tclVar(dialog.values$initial.level[[7]])
  level7 <-ttkentry(oneFactorFrame, width="10", textvariable=level7variable)
  level8variable <- tclVar(dialog.values$initial.level[[8]])
  level8 <-ttkentry(oneFactorFrame, width="10", textvariable=level8variable)
  tkgrid(labelRcmdr(variableNameFrame.1, text=gettextRcmdr("Name for the within-subjects factor: ")), wsfactorNameBox, sticky="w")
  tkgrid(variableNameFrame.1, sticky="w", columnspan=4)
  tkgrid(labelRcmdr(oneFactorFrame, text=gettextRcmdr("Specify up to 8 levels (responses) and level names for the within-subjects factor")), 
         sticky="w", columnspan=4)
  tkgrid(level1, level2, level3, level4, sticky="w")
  tkgrid(getFrame(rm1ComboxBox), getFrame(rm2ComboxBox), getFrame(rm3ComboxBox), getFrame(rm4ComboxBox), sticky="w")
  tkgrid(level5, level6, level7, level8, sticky="w")
  tkgrid(getFrame(rm5ComboxBox), getFrame(rm6ComboxBox), getFrame(rm7ComboxBox), getFrame(rm8ComboxBox), sticky="w")
  tkgrid(oneFactorFrame, sticky = "w")
  twoFactorsFrame <- ttklabelframe(twoFactorsTab, labelwidget=tklabel(top, text = gettextRcmdr("Specify two within-subject factors"),
                                                                      font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  variableNameFrame <- tkframe(twoFactorsFrame)
  wsrowfactorNameVariable <- tclVar(dialog.values$initial.wsrowfactorName)
  wsrowfactorNameBox <-ttkentry(variableNameFrame, width="20", textvariable=wsrowfactorNameVariable)
  wscolfactorNameVariable <- tclVar(dialog.values$initial.wscolfactorName)
  wscolfactorNameBox <-ttkentry(variableNameFrame, width="20", textvariable=wscolfactorNameVariable)
  rm11ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 1]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm12ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 2]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm13ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 3]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm14ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 4]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm15ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 5]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm21ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 1]], nullSelection=gettextRcmdr("<no selection>"))
  rm22ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 2]], nullSelection=gettextRcmdr("<no selection>"))
  rm23ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 3]], nullSelection=gettextRcmdr("<no selection>"))
  rm24ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 4]], nullSelection=gettextRcmdr("<no selection>"))
  rm25ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 5]], nullSelection=gettextRcmdr("<no selection>"))
  rm31ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 1]], nullSelection=gettextRcmdr("<no selection>"))
  rm32ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 2]], nullSelection=gettextRcmdr("<no selection>"))
  rm33ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 3]], nullSelection=gettextRcmdr("<no selection>"))
  rm34ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 4]], nullSelection=gettextRcmdr("<no selection>"))
  rm35ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 5]], nullSelection=gettextRcmdr("<no selection>"))
  rm41ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 1]], nullSelection=gettextRcmdr("<no selection>"))
  rm42ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 2]], nullSelection=gettextRcmdr("<no selection>"))
  rm43ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 3]], nullSelection=gettextRcmdr("<no selection>"))
  rm44ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 4]], nullSelection=gettextRcmdr("<no selection>"))
  rm45ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 5]], nullSelection=gettextRcmdr("<no selection>"))
  rm51ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 1]], nullSelection=gettextRcmdr("<no selection>"))
  rm52ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 2]], nullSelection=gettextRcmdr("<no selection>"))
  rm53ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 3]], nullSelection=gettextRcmdr("<no selection>"))
  rm54ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 4]], nullSelection=gettextRcmdr("<no selection>"))
  rm55ComboxBox <- variableComboBox(twoFactorsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 5]], nullSelection=gettextRcmdr("<no selection>"))
  colLevel1variable <- tclVar(dialog.values$initial.colLevel[[1]])
  colLevel1 <-ttkentry(twoFactorsFrame, width="10", textvariable=colLevel1variable, background="gray")
  colLevel2variable <- tclVar(dialog.values$initial.colLevel[[2]])
  colLevel2 <-ttkentry(twoFactorsFrame, width="10", textvariable=colLevel2variable)
  colLevel3variable <- tclVar(dialog.values$initial.colLevel[[3]])
  colLevel3 <-ttkentry(twoFactorsFrame, width="10", textvariable=colLevel3variable)
  colLevel4variable <- tclVar(dialog.values$initial.colLevel[[4]])
  colLevel4 <-ttkentry(twoFactorsFrame, width="10", textvariable=colLevel4variable)
  colLevel5variable <- tclVar(dialog.values$initial.colLevel[[5]])
  colLevel5 <-ttkentry(twoFactorsFrame, width="10", textvariable=colLevel5variable)
  rowLevel1variable <- tclVar(dialog.values$initial.rowLevel[[1]])
  rowLevel1 <-ttkentry(twoFactorsFrame, width="10", textvariable=rowLevel1variable, background="gray")
  rowLevel2variable <- tclVar(dialog.values$initial.rowLevel[[2]])
  rowLevel2 <-ttkentry(twoFactorsFrame, width="10", textvariable=rowLevel2variable)
  rowLevel3variable <- tclVar(dialog.values$initial.rowLevel[[3]])
  rowLevel3 <-ttkentry(twoFactorsFrame, width="10", textvariable=rowLevel3variable)
  rowLevel4variable <- tclVar(dialog.values$initial.rowLevel[[4]])
  rowLevel4 <-ttkentry(twoFactorsFrame, width="10", textvariable=rowLevel4variable)
  rowLevel5variable <- tclVar(dialog.values$initial.rowLevel[[5]])
  rowLevel5 <-ttkentry(twoFactorsFrame, width="10", textvariable=rowLevel5variable)
  tkgrid(labelRcmdr(variableNameFrame, text=gettextRcmdr("Name for the within-subjects row factor: ")), wsrowfactorNameBox, sticky="w")
  tkgrid(labelRcmdr(variableNameFrame, text=gettextRcmdr("Name for the within-subjects column factor: ")), wscolfactorNameBox, sticky="w")
  tkgrid(twoFactorsFrame, sticky="w")
  tkgrid(variableNameFrame, sticky="w", columnspan=4)
  tkgrid(labelRcmdr(twoFactorsFrame, text=gettextRcmdr("Specify up to 5 levels and level names for each within-subjects factor")), 
         sticky="w", columnspan=4)
  tkgrid(labelRcmdr(twoFactorsFrame, text=""), colLevel1, colLevel2, colLevel3, colLevel4, colLevel5, sticky="w")
  tkgrid(rowLevel1,
         getFrame(rm11ComboxBox), getFrame(rm12ComboxBox), getFrame(rm13ComboxBox), getFrame(rm14ComboxBox),  getFrame(rm15ComboxBox), sticky="sw")
  tkgrid(rowLevel2,
         getFrame(rm21ComboxBox), getFrame(rm22ComboxBox), getFrame(rm23ComboxBox), getFrame(rm24ComboxBox),  getFrame(rm25ComboxBox), sticky="sw")
  tkgrid(rowLevel3,
         getFrame(rm31ComboxBox), getFrame(rm32ComboxBox), getFrame(rm33ComboxBox), getFrame(rm34ComboxBox),  getFrame(rm35ComboxBox), sticky="sw")
  tkgrid(rowLevel4,
         getFrame(rm41ComboxBox), getFrame(rm42ComboxBox), getFrame(rm43ComboxBox), getFrame(rm44ComboxBox),  getFrame(rm45ComboxBox), sticky="sw")
  tkgrid(rowLevel5,
         getFrame(rm51ComboxBox), getFrame(rm52ComboxBox), getFrame(rm53ComboxBox), getFrame(rm54ComboxBox),  getFrame(rm55ComboxBox), sticky="sw")
  tkgrid(twoFactorsFrame, sticky = "w")
  
  tkgrid(labelRcmdr(optionsTab, text = gettextRcmdr("Name for long data set: ")),
         newDatasetField, sticky="w")
  tkgrid(labelRcmdr(optionsTab, text = gettextRcmdr("Response variable: ")),
         responseField, sticky="w")
  tkgrid(labelRcmdr(optionsTab, text = gettextRcmdr("Subject ID variable: ")),
         idField, sticky="w")
  tkgrid(labelRcmdr(optionsTab, text = gettextRcmdr("Make the wide data set active")),
         makeactiveCheckBox, sticky="w")
  tkgrid(getFrame(ignoreBox),sticky = "nw")
  dialogSuffix(use.tabs=TRUE, tabs=c("oneFactorTab", "twoFactorsTab", "optionsTab"),
               tab.names=c("One Repeated-Measures Factor", "Two Repeated-Measures Factors", "Options"), grid.buttons=TRUE)
}

characterToFactor <- function(){
  initializeDialog(title=gettextRcmdr("Convert Character Variables to Factors"))
  dataFrame <- tkframe(top)
  variableBox <- variableListBox(dataFrame, Character(), selectmode="multiple",
                                 title=gettextRcmdr("Variables (pick one or more)"))
  factorNameFrame <- tkframe(top)
  factorName <- tclVar(gettextRcmdr("<same as variables>"))
  factorNameField <- ttkentry(factorNameFrame, width="20", textvariable=factorName)
  onOK <- function(){
    variables <- getSelection(variableBox)
    closeDialog()
    facname <- trim.blanks(tclvalue(factorName))
    .activeDataSet <- ActiveDataSet()
    if (length(variables) == 0) {
      errorCondition(recall=characterToFactor, message=gettextRcmdr("You must select a variable."))}
    else command <- paste(.activeDataSet, " <- within(", .activeDataSet, ", {", sep="")
    for (name in variables){
      fname <- if (facname == gettextRcmdr("<same as variables>")) name
      else if (length(variables) == 1) facname
      else paste(facname, name, sep="")
      if (!is.valid.name(fname)){
        errorCondition(recall=characterToFactor,
                       message=paste('"', fname, '" ', gettextRcmdr("is not a valid name."), sep=""))
        return()
      }
      if (is.element(fname, Variables())) {
        if ("no" == tclvalue(checkReplace(fname))){
          characterToFactor()
          return()
        }
      }
      command <- paste(command, "\n  ", fname, " <- as.factor(", name, ")", sep="")
    }
    command <- paste(command, "\n})", sep="")
    result <- doItAndPrint(command)
    if (class(result)[1] !=  "try-error") activeDataSet(.activeDataSet, flushModel=FALSE, flushDialogMemory=FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="factor")
  tkgrid(getFrame(variableBox), labelRcmdr(dataFrame, text="  "), sticky="nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(labelRcmdr(factorNameFrame,
                    text=gettextRcmdr("New variable name or prefix for multiple variables:  ")),
         factorNameField, sticky="w")
  tkgrid(factorNameFrame, sticky="w")
  dialogSuffix(grid.buttons=TRUE)
}
