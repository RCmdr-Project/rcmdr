# Statistics Menu dialogs

# last modified 27 June 2011 by J. Fox

    # Tables menu

#twoWayTable <- function(){
#    Library("abind")
#    initializeDialog(title=gettextRcmdr("Two-Way Table"))
#    variablesFrame <- tkframe(top)
#    .factors <- Factors()
#    rowBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Row variable (pick one)"))
#    columnBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Column variable (pick one)"))
#    subsetBox()
#    onOK <- function(){
#        row <- getSelection(rowBox)
#        column <- getSelection(columnBox)
#        if (length(row) == 0 || length(column) == 0){
#            errorCondition(recall=twoWayTable, message=gettextRcmdr("You must select two variables."))
#            return()
#            }
#        if (row == column) {
#            errorCondition(recall=twoWayTable, message=gettextRcmdr("Row and column variables are the same."))
#            return()
#            }
#        percents <- as.character(tclvalue(percentsVariable))
#        chisq <- tclvalue(chisqTestVariable)
#        chisqComp <- tclvalue(chisqComponentsVariable)
#        expected <- tclvalue(expFreqVariable)
#        fisher <- tclvalue(fisherTestVariable)
#        subset <- tclvalue(subsetVariable)
#        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
#            else paste(", subset=", subset, sep="")
#        closeDialog()
#        command <- paste("xtabs(~", row, "+", column, ", data=", ActiveDataSet(),
#            subset, ")", sep="")
#        logger(paste(".Table <- ", command, sep=""))
#        assign(".Table", justDoIt(command), envir=.GlobalEnv)
#        doItAndPrint(".Table")
#        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
#        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
#        if (percents == "total") doItAndPrint("totPercents(.Table) # Percentage of Total")
#        if (chisq == 1) {
#            command <- "chisq.test(.Table, correct=FALSE)"
#            logger(paste(".Test <- ", command, sep=""))
#            assign(".Test", justDoIt(command), envir=.GlobalEnv)
#            doItAndPrint(".Test")
#            if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
#            warnText <- NULL
#            if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
#                gettextRcmdr("expected frequencies are less than 1"))
#            if (0 < (nlt5 <- sum(.Test$expected < 5))) warnText <- paste(warnText, "\n", nlt5,
#                gettextRcmdr(" expected frequencies are less than 5"), sep="")
#            if (!is.null(warnText)) Message(message=warnText,
#                type="warning")
#            if (chisqComp == 1) {
#                command <- "round(.Test$residuals^2, 2) # Chi-square Components"
#                doItAndPrint(command)
#                }
#            logger("remove(.Test)")
#            remove(.Test, envir=.GlobalEnv)
#            }
#        if (fisher == 1) doItAndPrint("fisher.test(.Table)")
#        logger("remove(.Table)")
#        remove(.Table, envir=.GlobalEnv)
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="xtabs")
#    radioButtons(name="percents",
#        buttons=c("rowPercents", "columnPercents", "totalPercents", "nonePercents"),
#        values=c("row", "column", "total", "none"), initialValue="none",
#        labels=gettextRcmdr(c("Row percentages", "Column percentages", "Percentages of total", "No percentages")), title=gettextRcmdr("Compute Percentages"))
#    checkBoxes(frame="testsFrame", boxes=c("chisqTest", "chisqComponents", "expFreq", "fisherTest"), initialValues=c("1", "0", "0", "0"),
#        labels=gettextRcmdr(c("Chi-square test of independence", "Components of chi-square statistic",
#            "Print expected frequencies", "Fisher's exact test")))
#    tkgrid(getFrame(rowBox), labelRcmdr(variablesFrame, text="    "), getFrame(columnBox), sticky="nw")
#    tkgrid(variablesFrame, sticky="w")
#    tkgrid(percentsFrame, sticky="w")
#    tkgrid(labelRcmdr(top, text=gettextRcmdr("Hypothesis Tests"), fg="blue"), sticky="w")
#    tkgrid(testsFrame, sticky="w")
#    tkgrid(subsetFrame, sticky="w")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=6, columns=1)
#    }

twoWayTable <- function(){ # dialog memory 2011-06-27 J. Fox
	Library("abind")
	defaults <- list(initial.row=NULL, initial.column=NULL, 
			initial.percents="none", initial.chisq=1, initial.chisqComp=0, initial.expected=0, 
			initial.fisher=0, initial.subset=gettextRcmdr("<all valid cases>"))
	dialog.values <- getDialog("twoWayTable", defaults)
	initializeDialog(title=gettextRcmdr("Two-Way Table"))
	variablesFrame <- tkframe(top)
	.factors <- Factors()
	rowBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Row variable (pick one)"),
			initialSelection=dialog.values$initial.row)
	columnBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Column variable (pick one)"),
			initialSelection=dialog.values$initial.column)
	subsetBox(subset.expression=dialog.values$initial.subset)
	onOK <- function(){
		row <- getSelection(rowBox)
		column <- getSelection(columnBox)
		percents <- as.character(tclvalue(percentsVariable))
		chisq <- tclvalue(chisqTestVariable)
		chisqComp <- tclvalue(chisqComponentsVariable)
		expected <- tclvalue(expFreqVariable)
		fisher <- tclvalue(fisherTestVariable)
		initial.subset <- subset <- tclvalue(subsetVariable)
		subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
				else paste(", subset=", subset, sep="")
		putDialog("twoWayTable", list(
						initial.row=varPosn(row, "factor"), 
						initial.column=varPosn(column, "factor"), 
						initial.percents=percents, initial.chisq=chisq, initial.chisqComp=chisqComp, 
						initial.expected=expected, initial.fisher=fisher, initial.subset=initial.subset
				))
		if (length(row) == 0 || length(column) == 0){
			errorCondition(recall=twoWayTable, message=gettextRcmdr("You must select two variables."))
			return()
		}
		if (row == column) {
			errorCondition(recall=twoWayTable, message=gettextRcmdr("Row and column variables are the same."))
			return()
		}
		closeDialog()
		command <- paste("xtabs(~", row, "+", column, ", data=", ActiveDataSet(),
				subset, ")", sep="")
		logger(paste(".Table <- ", command, sep=""))
		assign(".Table", justDoIt(command), envir=.GlobalEnv)
		doItAndPrint(".Table")
		if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
		if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
		if (percents == "total") doItAndPrint("totPercents(.Table) # Percentage of Total")
		if (chisq == 1) {
			command <- "chisq.test(.Table, correct=FALSE)"
			logger(paste(".Test <- ", command, sep=""))
			assign(".Test", justDoIt(command), envir=.GlobalEnv)
			doItAndPrint(".Test")
			if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
			warnText <- NULL
			if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
						gettextRcmdr("expected frequencies are less than 1"))
			if (0 < (nlt5 <- sum(.Test$expected < 5))) warnText <- paste(warnText, "\n", nlt5,
						gettextRcmdr(" expected frequencies are less than 5"), sep="")
			if (!is.null(warnText)) Message(message=warnText,
						type="warning")
			if (chisqComp == 1) {
				command <- "round(.Test$residuals^2, 2) # Chi-square Components"
				doItAndPrint(command)
			}
			logger("remove(.Test)")
			remove(.Test, envir=.GlobalEnv)
		}
		if (fisher == 1) doItAndPrint("fisher.test(.Table)")
		logger("remove(.Table)")
		remove(.Table, envir=.GlobalEnv)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="xtabs", reset="twoWayTable")
	radioButtons(name="percents",
			buttons=c("rowPercents", "columnPercents", "totalPercents", "nonePercents"),
			values=c("row", "column", "total", "none"), initialValue=dialog.values$initial.percents,
			labels=gettextRcmdr(c("Row percentages", "Column percentages", "Percentages of total", "No percentages")), title=gettextRcmdr("Compute Percentages"))
	checkBoxes(frame="testsFrame", boxes=c("chisqTest", "chisqComponents", "expFreq", "fisherTest"), 
			initialValues=c(dialog.values$initial.chisq, dialog.values$initial.chisqComp, 
					dialog.values$initial.expected, dialog.values$initial.fisher),
			labels=gettextRcmdr(c("Chi-square test of independence", "Components of chi-square statistic",
							"Print expected frequencies", "Fisher's exact test")))
	tkgrid(getFrame(rowBox), labelRcmdr(variablesFrame, text="    "), getFrame(columnBox), sticky="nw")
	tkgrid(variablesFrame, sticky="w")
	tkgrid(percentsFrame, sticky="w")
	tkgrid(labelRcmdr(top, text=gettextRcmdr("Hypothesis Tests"), fg="blue"), sticky="w")
	tkgrid(testsFrame, sticky="w")
	tkgrid(subsetFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=6, columns=1)
}

multiWayTable <- function(){
    Library("abind")
    initializeDialog(title=gettextRcmdr("Multi-Way Table"))
    variablesFrame <- tkframe(top)
    .factors <- Factors()
    rowBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Row variable (pick one)"))
    columnBox <- variableListBox(variablesFrame, .factors, title=gettextRcmdr("Column variable (pick one)"))
    controlBox <- variableListBox(variablesFrame, .factors, selectmode="multiple",
        title=gettextRcmdr("Control variable(s) (pick one or more)"))
    subsetBox()
    onOK <- function(){
        row <- getSelection(rowBox)
        column <- getSelection(columnBox)
        controls <- getSelection(controlBox)
        if (length(row) == 0 || length(column) == 0 || length(controls) == 0) {
            errorCondition(recall=multiWayTable, message=gettextRcmdr("You must select row, column, and control variables"))
            return()
            }
        if ((row == column) || is.element(row, controls) || is.element(column, controls)) {
            errorCondition(recall=multiWayTable, message=gettextRcmdr("Row, column, and control variables must be different."))
            return()
            }
        percents <- as.character(tclvalue(percentsVariable))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
            else paste(", subset=", subset, sep="")
        closeDialog()
        command <- paste("xtabs(~", row, "+", column, "+", paste(controls, collapse="+"),
            ", data=", ActiveDataSet(), subset, ")", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="xtabs")
    radioButtons(name="percents", buttons=c("rowPercents", "columnPercents", "nonePercents"), values=c("row", "column", "none"),
        initialValue="none", labels=gettextRcmdr(c("Row percentages", "Column percentages", "No percentages")), title=gettextRcmdr("Compute Percentages"))
    tkgrid(getFrame(rowBox), labelRcmdr(variablesFrame, text="    "), getFrame(columnBox), labelRcmdr(variablesFrame, text="    "),
        getFrame(controlBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

enterTable <- function(){
    Library("abind")
    env <- environment()
    initializeDialog(title=gettextRcmdr("Enter Two-Way Table"))
    outerTableFrame <- tkframe(top)
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)
    setUpTable <- function(...){
        tkdestroy(get(".tableFrame", envir=env))
        assign(".tableFrame", tkframe(outerTableFrame), envir=env)
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(colsValue))
        make.col.names <- "labelRcmdr(.tableFrame, text='')"
        for (j in 1:ncols) {
            col.varname <- paste(".colname.", j, sep="")
            assign(col.varname, tclVar(j), envir=env)
            make.col.names <- paste(make.col.names, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
                    col.varname, ")", sep="")
            }
        eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
        for (i in 1:nrows){
            varname <- paste(".tab.", i, ".1", sep="")
            assign(varname, tclVar("") , envir=env)
            row.varname <- paste(".rowname.", i, sep="")
            assign(row.varname, tclVar(i), envir=env)
            make.row <- paste("ttkentry(.tableFrame, width='5', textvariable=",
                row.varname, ")", sep="")
            make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
                varname, ")", sep="")
            for (j in 2:ncols){
                varname <- paste(".tab.", i, ".", j, sep="")
                assign(varname, tclVar(""), envir=env)
                make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=",
                    varname, ")", sep="")
                }
            eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
            }
        tkgrid(get(".tableFrame", envir=env), sticky="w")
        }
    rowColFrame <- tkframe(top)
    rowsValue <- tclVar("2")
    rowsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=rowsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    rowsShow <- labelRcmdr(rowColFrame, textvariable=rowsValue, width=2, justify="right")
    colsValue <- tclVar("2")
    colsSlider <- tkscale(rowColFrame, from=2, to=10, showvalue=FALSE, variable=colsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    colsShow <- labelRcmdr(rowColFrame, textvariable=colsValue, width=2, justify="right")
    onOK <- function(){
        nrows <- as.numeric(tclvalue(rowsValue))
        ncols <- as.numeric(tclvalue(colsValue))
        cell <- 0
        counts <- rep(NA, nrows*ncols)
        row.names <- rep("", nrows)
        col.names <- rep("", ncols)
        for (i in 1:nrows) row.names[i] <-
            eval(parse(text=paste("tclvalue(", paste(".rowname.", i, sep=""),")", sep="")))
        for (j in 1:ncols) col.names[j] <-
            eval(parse(text=paste("tclvalue(", paste(".colname.", j, sep=""),")", sep="")))
        for (i in 1:nrows){
            for (j in 1:ncols){
                cell <- cell+1
                varname <- paste(".tab.", i, ".", j, sep="")
                counts[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                }
            }
        counts <- na.omit(counts)
        if (length(counts) != nrows*ncols){
            errorCondition(recall=enterTable, message=sprintf(gettextRcmdr("Number of valid entries (%d)\nnot equal to number of rows (%d) * number of columns (%d)."), length(counts), nrows, ncols))
            return()
            }
        if (length(unique(row.names)) != nrows){
            errorCondition(recall=enterTable, message=gettextRcmdr("Row names are not unique."))
            return()
            }
        if (length(unique(col.names)) != ncols){
            errorCondition(recall=enterTable, message=gettextRcmdr("Column names are not unique."))
            return()
            }
        percents <- as.character(tclvalue(percentsVariable))
        chisq <- tclvalue(chisqVariable)
        chisqComp <- tclvalue(chisqComponentsVariable)
        expected <- tclvalue(expFreqVariable)
        fisher <- tclvalue(fisherVariable)
        closeDialog()
        command <- paste("matrix(c(", paste(counts, collapse=","), "), ", nrows, ", ", ncols,
            ", byrow=TRUE)", sep="")
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Table <- ", command, sep=""))
        command <- paste("c(",paste(paste("'", row.names, "'", sep=""), collapse=", "), ")", sep="")
        justDoIt(paste("rownames(.Table) <- ", command, sep=""))
        logger(paste("rownames(.Table) <- ", command, sep=""))
        command <- paste("c(",paste(paste("'", col.names, "'", sep=""), collapse=", "), ")", sep="")
        justDoIt(paste("colnames(.Table) <- ", command, sep=""))
        logger(paste("colnames(.Table) <- ", command, sep=""))
        doItAndPrint(".Table  # Counts")
        if (percents == "row") doItAndPrint("rowPercents(.Table) # Row Percentages")
        if (percents == "column") doItAndPrint("colPercents(.Table) # Column Percentages")
        if (percents == "total") doItAndPrint("totPercents(.Table) # Percentage of Total")
        if (chisq == 1) {
            command <- "chisq.test(.Table, correct=FALSE)"
            logger(paste(".Test <- ", command, sep=""))
            assign(".Test", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(".Test")
            if (expected == 1) doItAndPrint(".Test$expected # Expected Counts")
            warnText <- NULL
            if (0 < (nlt1 <- sum(.Test$expected < 1))) warnText <- paste(nlt1,
                gettextRcmdr("expected frequencies are less than 1"))
            if (0 < (nlt5 <- sum(.Test$expected < 5))) warnText <- paste(warnText, "\n", nlt5,
                gettextRcmdr(" expected frequencies are less than 5"), sep="")
            if (!is.null(warnText)) Message(message=warnText,
                type="warning")
            if (chisqComp == 1) {
                command <- "round(.Test$residuals^2, 2) # Chi-square Components"
                doItAndPrint(command)
                }
            logger("remove(.Test)")
            remove(.Test, envir=.GlobalEnv)
            }
        if (fisher == 1) doItAndPrint("fisher.test(.Table)")
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="chisq.test")
    radioButtons(name="percents", buttons=c("rowPercents", "columnPercents", "totalPercents", "nonePercents"), values=c("row", "column", "total", "none"),
        initialValue="none", labels=gettextRcmdr(c("Row percentages", "Column percentages",  "Percentages of total", "No percentages")), title=gettextRcmdr("Compute Percentages"))
    checkBoxes(frame="testsFrame", boxes=c("chisq", "chisqComponents", "expFreq", "fisher"), initialValues=c("1", "0", "0", "0"),
        labels=gettextRcmdr(c("Chi-square test of independence", "Components of chi-square statistic",
            "Print expected frequencies", "Fisher's exact test")))
    tkgrid(labelRcmdr(rowColFrame, text=gettextRcmdr("Number of Rows:")), rowsSlider, rowsShow, sticky="w")
    tkgrid(labelRcmdr(rowColFrame, text=gettextRcmdr("Number of Columns:")), colsSlider, colsShow, sticky="w")
    tkgrid(rowColFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Enter counts:"), fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")
    tkgrid(percentsFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Hypothesis Tests"), fg="blue"), sticky="w")
    tkgrid(testsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=7, columns=2)
    }
