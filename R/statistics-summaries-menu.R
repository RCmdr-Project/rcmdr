# Statistics Menu dialogs

# last modified 2011-12-21 by J. Fox

    # Summaries menu
    
summarizeDataSet <- function(){
    nvar <- length(Variables())
    .activeDataSet <- ActiveDataSet()
    if (nvar > 10){
        response <- RcmdrTkmessageBox(message=sprintf(gettextRcmdr("There are %d variables in the data set %s.\nDo you want to proceed?"), nvar, .activeDataSet),
            icon="question", type="okcancel", default="cancel")
        if ("cancel" == tclvalue(response)) {
            tkfocus(CommanderWindow())
            return()
            }
        }
    doItAndPrint(paste("summary(", .activeDataSet, ")", sep=""))
    }

#numericalSummaries <- function(){
#    Library("abind")
#    initializeDialog(title=gettextRcmdr("Numerical Summaries"))
#    xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick one or more)"))
#    checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sd"), initialValues=c("1", "1"), labels=gettextRcmdr(c("Mean", "Standard Deviation")))
#    quantilesVariable <- tclVar("1")
#    quantilesFrame <- tkframe(top)
#    quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable)
#    quantiles <- tclVar("0, .25, .5, .75, 1")
#    quantilesEntry <- ttkentry(quantilesFrame, width="20", textvariable=quantiles)
#    groupsBox(recall=numericalSummaries, label=gettextRcmdr("Summarize by:"), initialLabel=gettextRcmdr("Summarize by groups"))
#    onOK <- function(){
#        x <- getSelection(xBox)
#        if (length(x) == 0){
#            errorCondition(recall=numericalSummaries, message=gettextRcmdr("You must select a variable."))
#            return()
#            }
#        closeDialog()
#        quants <- paste("c(", gsub(",+", ",", gsub(" ", ",", tclvalue(quantiles))), ")", sep="")
#        .activeDataSet <- ActiveDataSet()
#        vars <- if (length(x) == 1) paste('"', x, '"', sep="") 
#            else paste("c(", paste('"', x, '"', collapse=", ", sep=""), ")", sep="")
#        vars <- paste(.activeDataSet, "[,", vars, "]", sep="")
#        stats <- paste("c(",
#            paste(c('"mean"', '"sd"', '"quantiles"')
#                [c(tclvalue(meanVariable), tclvalue(sdVariable), tclvalue(quantilesVariable)) == 1], 
#                collapse=", "), ")", sep="")
#        if (stats == "c()"){
#             errorCondition(recall=numericalSummaries, message=gettextRcmdr("No statistics selected."))
#            return()
#            }               
#        command <- if (.groups != FALSE) {
#            grps <- paste(.activeDataSet, "$", .groups, sep="")
#            paste("numSummary(", vars, ", groups=", grps, ", statistics=", stats, 
#				", quantiles=", quants, ")", sep="")
#            }
#        else  paste("numSummary(", vars, ", statistics=", stats, 
#			", quantiles=", quants, ")", sep="")
#        doItAndPrint(command) 
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="numSummary")
#    tkgrid(getFrame(xBox), sticky="nw")    
#    tkgrid(checkBoxFrame, sticky="w")
#    tkgrid(labelRcmdr(quantilesFrame, text=gettextRcmdr("Quantiles")), quantilesCheckBox,
#        labelRcmdr(quantilesFrame, text=gettextRcmdr(" quantiles:")), quantilesEntry, sticky="w")
#    tkgrid(quantilesFrame, sticky="w")
#    tkgrid(groupsFrame, sticky="w")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=6, columns=1)
#    }

numericalSummaries <- function(){ # dialog memory 2011-06-27  J. Fox
	Library("abind")
	Library("e1071")
	defaults <- list(initial.x=NULL, initial.mean="1", initial.sd="1", initial.cv="0",
			initial.quantiles.variable="1", 
			initial.quantiles="0, .25, .5, .75, 1", 
			initial.skewness="0", initial.kurtosis="0", initial.type="2",
			initial.group=NULL)
	dialog.values <- getDialog("numericalSummaries", defaults)
	initial.group <- dialog.values$initial.group
	initializeDialog(title=gettextRcmdr("Numerical Summaries"))
	xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick one or more)"),
			initialSelection=varPosn(dialog.values$initial.x, "numeric"))
	selectFrame <- tkframe(top)
	checkBoxes(frame="checkBoxFrame", boxes=c("mean", "sd", "cv"), 
			initialValues=c(dialog.values$initial.mean, dialog.values$initial.sd, dialog.values$initial.cv), 
			labels=gettextRcmdr(c("Mean", "Standard Deviation", "Coefficient of Variation")))
	checkBoxes(window=selectFrame, frame="skCheckBoxFrame", boxes=c("skewness", "kurtosis"), 
			initialValues=c(dialog.values$initial.skewness, dialog.values$initial.kurtosis), 
			labels=gettextRcmdr(c("Skewness", "Kurtosis")))
	radioButtons(window=selectFrame, name="typeButtons", buttons=c("b1", "b2", "b3"), values=c("1", "2", "3"), 
			initialValue=dialog.values$initial.type,
			labels=gettextRcmdr(c("Type 1", "Type 2", "Type 3")))
	quantilesVariable <- tclVar(dialog.values$initial.quantiles.variable)
	quantilesFrame <- tkframe(top)
	quantilesCheckBox <- tkcheckbutton(quantilesFrame, variable=quantilesVariable)
	quantiles <- tclVar(dialog.values$initial.quantiles)
	quantilesEntry <- ttkentry(quantilesFrame, width="20", textvariable=quantiles)
	groupsBox(recall=numericalSummaries, label=gettextRcmdr("Summarize by:"), 
			initialLabel=if (is.null(initial.group)) gettextRcmdr("Summarize by groups") 
					else paste(gettextRcmdr("Summarize by:"), initial.group), 
			initialGroup=initial.group)
	onOK <- function(){
		x <- getSelection(xBox)
		quants <- tclvalue(quantiles)
		meanVar <- tclvalue(meanVariable)
		sdVar <- tclvalue(sdVariable)
		cvVar <- tclvalue(cvVariable)
		quantsVar <- tclvalue(quantilesVariable)
		skewnessVar <- tclvalue(skewnessVariable)
		kurtosisVar <- tclvalue(kurtosisVariable)
		typeVar <- tclvalue(typeButtonsVariable)
		putDialog("numericalSummaries", list(
						initial.x=x, initial.mean=meanVar, initial.sd=sdVar, initial.cv=cvVar,
						initial.quantiles.variable=quantsVar, initial.quantiles=quants,
						initial.skewness=skewnessVar, initial.kurtosis=kurtosisVar, initial.type=typeVar,
						initial.group=if (.groups != FALSE) .groups else NULL
				))		
		if (length(x) == 0){
			errorCondition(recall=numericalSummaries, message=gettextRcmdr("You must select a variable."))
			return()
		}
		closeDialog()
		quants <- paste("c(", gsub(",+", ",", gsub(" ", ",", quants)), ")", sep="")
		.activeDataSet <- ActiveDataSet()
		vars <- if (length(x) == 1) paste('"', x, '"', sep="") 
				else paste("c(", paste('"', x, '"', collapse=", ", sep=""), ")", sep="")
		vars <- paste(.activeDataSet, "[,", vars, "]", sep="")
		stats <- paste("c(",
				paste(c('"mean"', '"sd"', '"quantiles"', '"cv"', '"skewness"', '"kurtosis"')
								[c(meanVar, sdVar, quantsVar, cvVar, skewnessVar, kurtosisVar) == 1], 
						collapse=", "), ")", sep="")
		if (stats == "c()"){
			errorCondition(recall=numericalSummaries, message=gettextRcmdr("No statistics selected."))
			return()
		}
		type.text <- if (skewnessVar == 1 || kurtosisVar == 1) paste(', type="', typeVar, '"', sep="") else ""
		command <- if (.groups != FALSE) {
					grps <- paste(.activeDataSet, "$", .groups, sep="")
					paste("numSummary(", vars, ", groups=", grps, ", statistics=", stats, 
							", quantiles=", quants, type.text, ")", sep="")
				}
				else  paste("numSummary(", vars, ", statistics=", stats, 
							", quantiles=", quants, type.text, ")", sep="")
		doItAndPrint(command) 
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="numSummary", reset="numericalSummaries")
	tkgrid(getFrame(xBox), sticky="nw")    
	tkgrid(checkBoxFrame, sticky="w")
	tkgrid(skCheckBoxFrame, typeButtonsFrame, sticky="nw")
	tkgrid(selectFrame, sticky="w")
	tkgrid(labelRcmdr(quantilesFrame, text=gettextRcmdr("Quantiles")), quantilesCheckBox,
			labelRcmdr(quantilesFrame, text=gettextRcmdr(" quantiles:")), quantilesEntry, sticky="w")
	tkgrid(quantilesFrame, sticky="w")
	tkgrid(groupsFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=7, columns=1)
}
	
#frequencyDistribution <- function(){
#    initializeDialog(title=gettextRcmdr("Frequency Distributions"))
#    xBox <- variableListBox(top, Factors(), selectmode="multiple",
#        title=gettextRcmdr("Variables (pick one or more)"))
#    optionsFrame <- tkframe(top)
#    goodnessOfFitVariable <- tclVar("0")
#    goodnessOfFitCheckBox <- tkcheckbutton(optionsFrame, variable=goodnessOfFitVariable)
#    onOK <- function(){
#        x <- getSelection(xBox)
#        if (length(x) == 0){
#            errorCondition(recall=frequencyDistribution, message=gettextRcmdr("You must select a variable."))
#            return()
#            }
#        goodnessOfFit <- tclvalue(goodnessOfFitVariable)
#        if (length(x) > 1 && goodnessOfFit == "1"){
#            errorCondition(recall=frequencyDistribution, 
#                message=gettextRcmdr("Goodness-of-fit test not available when more than one variable is selected."))
#            return()
#            }
#        closeDialog()
#        .activeDataSet <- ActiveDataSet()
#        for (variable in x){
#            command <- paste("table(", .activeDataSet, "$", variable, ")", sep="")
#            logger(paste(".Table <-", command))
#            assign(".Table", justDoIt(command), envir=.GlobalEnv)
#            doItAndPrint(paste(".Table  # counts for", variable))
#            doItAndPrint(paste("round(100*.Table/sum(.Table), 2)  # percentages for", variable))
#            }
#        env <- environment()
#        if (goodnessOfFit == 1){
#            initializeDialog(subwin, title=gettextRcmdr("Goodness-of-Fit Test"))
#            hypothesisFrame <- tkframe(subwin)
#            levs <- eval(parse(text=paste("levels(", .activeDataSet, "$", x, ")", sep="")))
#            n.levs <- length(levs)
#            assign(".entry.1", tclVar(paste("1/", n.levs, sep="")), envir=env)
#            make.entries <- "labelRcmdr(hypothesisFrame, text='Hypothesized probabilities:   ')"
#            make.lev.names <- "labelRcmdr(hypothesisFrame, text='Factor levels:')"
#            for (i in 1:n.levs) {
#                entry.varname <- paste(".entry.", i, sep="")
#                assign(entry.varname, tclVar(paste("1/", n.levs, sep="")), envir=env)
#                make.entries <- paste(make.entries, ", ", "ttkentry(hypothesisFrame, width='5', textvariable=", 
#                        entry.varname, ")", sep="")
#                make.lev.names <- paste(make.lev.names, ", labelRcmdr(hypothesisFrame, text='", levs[i], "')", sep="")
#                }
#            eval(parse(text=paste("tkgrid(", make.lev.names, ", sticky='w')", sep="")), envir=env)
#            eval(parse(text=paste("tkgrid(", make.entries, ", stick='w')", sep="")), envir=env)
#            tkgrid(hypothesisFrame, sticky="w")
#            onOKsub <- function(){
#                probs <- rep(NA, n.levs)
#                for (i in 1:n.levs){
#                    entry.varname <- paste(".entry.", i, sep="")
#                    res <- try(
#                        entry <- eval(parse(text=eval(parse(text=paste("tclvalue(", entry.varname,")", sep="")), envir=env))),
#                        silent=TRUE)
#                    if (class(res) == "try-error"){
#                        errorCondition(subwin, message=gettextRcmdr("Invalid entry."))
#                        return()
#                        }
#                    if (length(entry) == 0){
#                        errorCondition(subwin, message=gettextRcmdr("Missing entry."))
#                        return()
#                        }
#                    opts <- options(warn=-1)
#                    probs[i] <- as.numeric(entry)
#                    options(opts)
#                    }
#                probs <- na.omit(probs)
#                if (length(probs) != n.levs){
#                    errorCondition(subwin, message=sprintf(gettextRcmdr("Number of valid entries (%d)\nnot equal to number levels (%d)."), length(probs), 
#                        n.levs))
#                    return()
#                    }
#                if (any(probs < 0)){
#                    errorCondition(subwin, message=gettextRcmdr("Negative probabilities not allowed."))
#                    return()
#                    }
#                if (abs(sum(probs) - 1) > 0.001){
#                    Message(message=gettextRcmdr("Probabilities rescaled to sum to 1."), type="warning")
#                    probs <- probs/sum(probs)
#                    }
#                closeDialog(subwin)
#                command <- paste("c(", paste(probs, collapse=","), ")", sep="")
#                logger(paste(".Probs <-", command))
#                assign(".Probs", justDoIt(command), envir=.GlobalEnv)
#                doItAndPrint("chisq.test(.Table, p=.Probs)")
#                logger("remove(.Probs)")
#                remove(.Probs, envir=.GlobalEnv)
#                }
#            subOKCancelHelp(subwin)
#            tkgrid(subButtonsFrame, sticky="w")
#            dialogSuffix(subwin, rows=2, columns=1, onOK=onOKsub, focus=subwin)
#            }            
#        logger("remove(.Table)") 
#        remove(.Table, envir=.GlobalEnv)  
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="table")
#    tkgrid(getFrame(xBox), sticky="nw")    
#    tkgrid(labelRcmdr(optionsFrame, 
#        text=gettextRcmdr("Chi-square goodness-of-fit test (for one variable only)")), 
#            goodnessOfFitCheckBox, sticky="w")
#    tkgrid(optionsFrame, sticky="w")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=3, columns=2)
#    }

frequencyDistribution <- function () {
	defaults <- list (initial.x = NULL, initial.goodnessOfFit = "0")
	dialog.values <- getDialog ("frequencyDistribution", defaults)
	initializeDialog(title = gettextRcmdr("Frequency Distributions"))
	xBox <- variableListBox(top, Factors(), selectmode = "multiple", 
			title = gettextRcmdr("Variables (pick one or more)"),
			initialSelection = varPosn (dialog.values$initial.x, "factor"))
	optionsFrame <- tkframe(top)
	goodnessOfFitVariable <- tclVar(dialog.values$initial.goodnessOfFit)
	goodnessOfFitCheckBox <- tkcheckbutton(optionsFrame, variable = goodnessOfFitVariable)
	onOK <- function() {
		x <- getSelection(xBox)
		if (length(x) == 0) {
			errorCondition(recall = frequencyDistribution, message = gettextRcmdr("You must select a variable."))
			return()
		}
		goodnessOfFit <- tclvalue(goodnessOfFitVariable)
		putDialog ("frequencyDistribution", list (initial.x = x, initial.goodnessOfFit = goodnessOfFit))
		if (length(x) > 1 && goodnessOfFit == "1") {
			errorCondition(recall = frequencyDistribution, message = gettextRcmdr("Goodness-of-fit test not available when more than one variable is selected."))
			return()
		}
		closeDialog()
		.activeDataSet <- ActiveDataSet()
		for (variable in x) {
			command <- paste("table(", .activeDataSet, "$", variable, 
					")", sep = "")
			logger(paste(".Table <-", command))
			assign(".Table", justDoIt(command), envir = .GlobalEnv)
			doItAndPrint(paste(".Table  # counts for", variable))
			doItAndPrint(paste("round(100*.Table/sum(.Table), 2)  # percentages for", 
							variable))
		}
		env <- environment()
		if (goodnessOfFit == 1) {
			initializeDialog(subwin, title = gettextRcmdr("Goodness-of-Fit Test"))
			hypothesisFrame <- tkframe(subwin)
			levs <- eval(parse(text = paste("levels(", .activeDataSet, 
									"$", x, ")", sep = "")))
			n.levs <- length(levs)
			assign(".entry.1", tclVar(paste("1/", n.levs, sep = "")), 
					envir = env)
			make.entries <- "labelRcmdr(hypothesisFrame, text='Hypothesized probabilities:   ')"
			make.lev.names <- "labelRcmdr(hypothesisFrame, text='Factor levels:')"
			for (i in 1:n.levs) {
				entry.varname <- paste(".entry.", i, sep = "")
				assign(entry.varname, tclVar(paste("1/", n.levs, 
										sep = "")), envir = env)
				make.entries <- paste(make.entries, ", ", "ttkentry(hypothesisFrame, width='5', textvariable=", 
						entry.varname, ")", sep = "")
				make.lev.names <- paste(make.lev.names, ", labelRcmdr(hypothesisFrame, text='", 
						levs[i], "')", sep = "")
			}
			eval(parse(text = paste("tkgrid(", make.lev.names, 
									", sticky='w')", sep = "")), envir = env)
			eval(parse(text = paste("tkgrid(", make.entries, 
									", stick='w')", sep = "")), envir = env)
			tkgrid(hypothesisFrame, sticky = "w")
			onOKsub <- function() {
				probs <- rep(NA, n.levs)
				for (i in 1:n.levs) {
					entry.varname <- paste(".entry.", i, sep = "")
					res <- try(entry <- eval(parse(text = eval(parse(text = paste("tclvalue(", 
																	entry.varname, ")", sep = "")), envir = env))), 
							silent = TRUE)
					if (class(res) == "try-error") {
						errorCondition(subwin, message = gettextRcmdr("Invalid entry."))
						return()
					}
					if (length(entry) == 0) {
						errorCondition(subwin, message = gettextRcmdr("Missing entry."))
						return()
					}
					opts <- options(warn = -1)
					probs[i] <- as.numeric(entry)
					options(opts)
				}
				probs <- na.omit(probs)
				if (length(probs) != n.levs) {
					errorCondition(subwin, message = sprintf(gettextRcmdr("Number of valid entries (%d)\nnot equal to number levels (%d)."), 
									length(probs), n.levs))
					return()
				}
				if (any(probs < 0)) {
					errorCondition(subwin, message = gettextRcmdr("Negative probabilities not allowed."))
					return()
				}
				if (abs(sum(probs) - 1) > 0.001) {
					Message(message = gettextRcmdr("Probabilities rescaled to sum to 1."), 
							type = "warning")
					probs <- probs/sum(probs)
				}
				closeDialog(subwin)
				command <- paste("c(", paste(probs, collapse = ","), 
						")", sep = "")
				logger(paste(".Probs <-", command))
				assign(".Probs", justDoIt(command), envir = .GlobalEnv)
				doItAndPrint("chisq.test(.Table, p=.Probs)")
				logger("remove(.Probs)")
				remove(.Probs, envir = .GlobalEnv)
			}
			subOKCancelHelp(subwin)
			tkgrid(subButtonsFrame, sticky = "w")
			dialogSuffix(subwin, rows = 2, columns = 1, onOK = onOKsub, 
					focus = subwin)
		}
		logger("remove(.Table)")
		remove(.Table, envir = .GlobalEnv)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "table", reset = "frequencyDistribution")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Chi-square goodness-of-fit test (for one variable only)")), 
			goodnessOfFitCheckBox, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 2)
}

#statisticsTable <- function(){
#	initializeDialog(title=gettextRcmdr("Table of Statistics"))
#	variablesFrame <- tkframe(top)
#	groupBox <- variableListBox(variablesFrame, Factors(), selectmode="multiple", 
#		title=gettextRcmdr("Factors (pick one or more)"))
#	responseBox <- variableListBox(variablesFrame, Numeric(), selectmode="multiple", 
#		title=gettextRcmdr("Response variables (pick one or more)"))
#	radioButtons(name="statistic", buttons=c("mean", "median", "sd"), labels=gettextRcmdr(c("Mean", "Median", "Standard deviation")), title=gettextRcmdr("Statistic"))
#	otherVariable <- tclVar("")
#	otherButton <- ttkradiobutton(statisticFrame, variable=statisticVariable, value="other")
#	otherEntry <- ttkentry(statisticFrame, width="20", textvariable=otherVariable)   
#	tkgrid(labelRcmdr(statisticFrame, text=gettextRcmdr("Other (specify)")), otherButton, otherEntry, sticky="w")
#	onOK <- function(){
#		groups <- getSelection(groupBox)
#		if (0 == length(groups)) {
#			errorCondition(recall=statisticsTable, message=gettextRcmdr("No factors selected."))
#			return()
#		}
#		responses <- getSelection(responseBox)
#		if (0 == length(responses)) {
#			errorCondition(recall=statisticsTable, message=gettextRcmdr("You must select a response variable."))
#			return()
#		}
#		statistic <- tclvalue(statisticVariable)
#		if (statistic == "other") statistic <- tclvalue(otherVariable)
#		closeDialog()
#		.activeDataSet <- ActiveDataSet()
#		groups.list <- paste(paste(groups, "=", .activeDataSet, "$", groups, sep=""), collapse=", ")
#		for (response in responses){
#			if (length(responses) > 1) 
#				doItAndPrint(paste("# Table for ", response, ":", sep=""))                
#			doItAndPrint(paste("tapply(", .activeDataSet, "$", response, 
#					", list(", groups.list, "), ", statistic, ", na.rm=TRUE)", sep=""))
#		}
#		tkfocus(CommanderWindow())
#	}
#	OKCancelHelp(helpSubject="tapply")
#	tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "),getFrame(responseBox), sticky="nw")
#	tkgrid(variablesFrame, sticky="w")
#	tkgrid(statisticFrame, sticky="w")
#	tkgrid(buttonsFrame, sticky="w")
#	dialogSuffix(rows=3, columns=1, focus=otherEntry)
#}

statisticsTable <- function () {
	defaults <- list (initial.group=NULL, initial.response=NULL, initial.statistic="mean", initial.other = "")
	dialog.values <- getDialog ("statisticsTable", defaults)
	initializeDialog(title = gettextRcmdr("Table of Statistics"))
	variablesFrame <- tkframe(top)
	groupBox <- variableListBox(variablesFrame, Factors(), selectmode = "multiple", 
			title = gettextRcmdr("Factors (pick one or more)"), 
			initialSelection = varPosn(dialog.values$initial.group,"factor"))
	responseBox <- variableListBox(variablesFrame, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.response, "numeric"),
			title = gettextRcmdr("Response variables (pick one or more)"))
	radioButtons(name = "statistic", buttons = c("mean", "median", 
					"sd"), labels = gettextRcmdr(c("Mean", "Median", "Standard deviation")), 
			initialValue = dialog.values$initial.statistic, 
			title = gettextRcmdr("Statistic"))
	otherVariable <- tclVar(dialog.values$initial.other)
	if (!(dialog.values$initial.statistic %in% c("mean", "median", "sd"))) statisticVariable <- tclVar("other")
	otherButton <- ttkradiobutton(statisticFrame, variable = statisticVariable, 
			value = "other")
	otherEntry <- ttkentry(statisticFrame, width = "20", textvariable = otherVariable)
	tkgrid(labelRcmdr(statisticFrame, text = gettextRcmdr("Other (specify)")), 
			otherButton, otherEntry, sticky = "w")
	onOK <- function() {
		groups <- getSelection(groupBox)
		if (0 == length(groups)) {
			errorCondition(recall = statisticsTable, message = gettextRcmdr("No factors selected."))
			return()
		}
		responses <- getSelection(responseBox)
		if (0 == length(responses)) {
			errorCondition(recall = statisticsTable, message = gettextRcmdr("You must select a response variable."))
			return()
		}
		stat <- statistic <- tclvalue(statisticVariable)
		if (statistic == "other") 
			statistic <- tclvalue(otherVariable)
		putDialog ("statisticsTable", list(initial.group=groups, initial.response=responses, 
						initial.statistic=statistic, initial.other = if(stat == "other") statistic else ""))  
		closeDialog()
		.activeDataSet <- ActiveDataSet()
		groups.list <- paste(paste(groups, "=", .activeDataSet, 
						"$", groups, sep = ""), collapse = ", ")
		for (response in responses) {
			if (length(responses) > 1) 
				doItAndPrint(paste("# Table for ", response, 
								":", sep = ""))
			doItAndPrint(paste("tapply(", .activeDataSet, "$", 
							response, ", list(", groups.list, "), ", statistic, 
							", na.rm=TRUE)", sep = ""))
		}
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "tapply", reset="statisticsTable")
	tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text = "    "), 
			getFrame(responseBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "w")
	tkgrid(statisticFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1, focus = otherEntry)
}
    
#correlationMatrix <- function(){
#	initializeDialog(title=gettextRcmdr("Correlation Matrix"))
#	xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick two or more)"))
#	radioButtons(name="correlations", buttons=c("pearson", "spearman", "partial"), values=c("Pearson", "Spearman", "partial"),
#			labels=gettextRcmdr(c("Pearson product-moment", "Spearman rank-order", "Partial")), title=gettextRcmdr("Type of Correlations"))
#	radioButtons(name="missing", buttons=c("complete", "pairwise"),
#			labels=gettextRcmdr(c("Use complete cases", "Use pairwise-complete cases")), title=gettextRcmdr("Missing Data"))
#	pvaluesFrame <- tkframe(top)
#	pvaluesVar <- tclVar("0")
#	pvaluesCheckbox <- tkcheckbutton(pvaluesFrame, variable=pvaluesVar)
#	onOK <- function(){
#		correlations <- tclvalue(correlationsVariable)
#		missing <- tclvalue(missingVariable)
#		x <- getSelection(xBox)
#		if (2 > length(x)) {
#			errorCondition(recall=correlationMatrix, message=gettextRcmdr("Fewer than 2 variables selected."))
#			return()
#		}
#		if ((correlations == "partial") && (3 > length(x))) {
#			errorCondition(recall=correlationMatrix, message=gettextRcmdr("Fewer than 3 variables selected\nfor partial correlations."))
#			return()
#		}
#		closeDialog()
#		x <- paste('"', x, '"', sep="")
#		.activeDataSet <- ActiveDataSet()
#		pvalues <- tclvalue(pvaluesVar)
#		if (correlations == "Pearson"){
#			if (pvalues == 0){
#				doItAndPrint(paste("cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
#								')], use="', missing, '")', sep=""))
#			}
#			else{
#				Library("Hmisc")
#				doItAndPrint(paste("rcorr.adjust(", .activeDataSet, "[,c(", paste(x, collapse=","),
#								')], type="pearson", use="', missing, '")', sep=""))
#			}
#		}
#		else if (correlations == "Spearman"){
#			logger("# Spearman rank-order correlations")
#			if (pvalues == 0){
#				doItAndPrint(paste("cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
#								')], use="', missing,'", method="spearman")', sep=""))
#			}
#			else{
#				Library("Hmisc")
#				doItAndPrint(paste("rcorr.adjust(", .activeDataSet, "[,c(", paste(x, collapse=","),
#								')], type="spearman", use="', missing, '")', sep=""))				
#			}
#		}
#		else doItAndPrint(paste("partial.cor(", .activeDataSet, "[,c(", paste(x, collapse=","),
#							')], use="', missing, '")', sep=""))    
#		tkfocus(CommanderWindow())
#	}
#	OKCancelHelp(helpSubject="rcorr.adjust")
#	tkgrid(getFrame(xBox), sticky="nw")
#	tkgrid(correlationsFrame, sticky="w")
#	tkgrid(missingFrame, sticky="w")
#	tkgrid(labelRcmdr(pvaluesFrame, 
#					text=gettextRcmdr("Pairwise p-values\nfor Pearson or Spearman correlations")), 
#			pvaluesCheckbox, sticky="w")
#	tkgrid(pvaluesFrame, sticky="w")
#	tkgrid(buttonsFrame, sticky="w")
#	dialogSuffix(rows=5, columns=1)
#}

correlationMatrix <- function (){
	defaults <- list (initial.x = NULL, initial.correlations = "Pearson", initial.pvaluesVar="0")  
	dialog.values <- getDialog ("correlationMatrix", defaults)
	initializeDialog(title = gettextRcmdr("Correlation Matrix"))
	xBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			title = gettextRcmdr("Variables (pick two or more)"),
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	radioButtons(name = "correlations", buttons = c("pearson", 
					"spearman", "partial"), values = c("Pearson", "Spearman", 
					"partial"), labels = gettextRcmdr(c("Pearson product-moment", 
							"Spearman rank-order", "Partial")), title = gettextRcmdr("Type of Correlations"),
			initialValue = dialog.values$initial.correlations)
	pvaluesFrame <- tkframe(top)
	pvaluesVar <- tclVar(dialog.values$initial.pvaluesVar)
	pvaluesCheckbox <- tkcheckbutton(pvaluesFrame, variable = pvaluesVar)
	onOK <- function() {
		correlations <- tclvalue(correlationsVariable)
		x <- getSelection(xBox)
		pvalues <- tclvalue(pvaluesVar)
		if (2 > length(x)) {
			errorCondition(recall = correlationMatrix, message = gettextRcmdr("Fewer than 2 variables selected."))
			return()
		}
		if ((correlations == "partial") && (3 > length(x))) {
			errorCondition(recall = correlationMatrix, message = gettextRcmdr("Fewer than 3 variables selected\nfor partial correlations."))
			return()
		}
		closeDialog()
		putDialog ("correlationMatrix", list (initial.x=x, initial.correlations=correlations, 
						initial.pvaluesVar=pvalues))
		x <- paste("\"", x, "\"", sep = "")
		.activeDataSet <- ActiveDataSet()
		if (correlations == "Pearson") {
			if (pvalues == 0) {
				doItAndPrint(paste("cor(", .activeDataSet, "[,c(", 
								paste(x, collapse = ","), ")], use=\"complete.obs\")", 
								sep = ""))
			}
			else {
				Library("Hmisc")
				doItAndPrint(paste("rcorr.adjust(", .activeDataSet, 
								"[,c(", paste(x, collapse = ","), ")], type=\"pearson\")", 
								sep = ""))
			}
		}
		else if (correlations == "Spearman") {
			logger("# Spearman rank-order correlations")
			if (pvalues == 0) {
				doItAndPrint(paste("cor(", .activeDataSet, "[,c(", 
								paste(x, collapse = ","), ")], use=\"complete.obs\", method=\"spearman\")", 
								sep = ""))
			}
			else {
				Library("Hmisc")
				doItAndPrint(paste("rcorr.adjust(", .activeDataSet, 
								"[,c(", paste(x, collapse = ","), ")], type=\"spearman\")", 
								sep = ""))
			}
		}
		else doItAndPrint(paste("partial.cor(", .activeDataSet, 
							"[,c(", paste(x, collapse = ","), ")], use=\"complete.obs\")", 
							sep = ""))
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "rcorr.adjust", reset="correlationMatrix")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(correlationsFrame, sticky = "w")
	tkgrid(labelRcmdr(pvaluesFrame, text = gettextRcmdr("Pairwise p-values\nfor Pearson or Spearman correlations")), 
			pvaluesCheckbox, sticky = "w")
	tkgrid(pvaluesFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 4, columns = 1)
}
	
#correlationTest <- function(){
#  initializeDialog(title=gettextRcmdr("Correlation Test"))
#  xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick two)"))
#  radioButtons(name="correlations", buttons=c("pearson", "spearman", "kendall"),
#               labels=gettextRcmdr(c("Pearson product-moment", "Spearman rank-order", "Kendall's tau")),
#               title=gettextRcmdr("Type of Correlation"))
#  radioButtons(name="alternative", buttons=c("two.sided", "less", "greater"), values=c("two.sided", "less", "greater"),
#               labels=gettextRcmdr(c("Two-sided", "Correlation < 0", "Correlation > 0")), title=gettextRcmdr("Alternative Hypothesis"))  
#  onOK <- function(){
#    alternative <- as.character(tclvalue(alternativeVariable))
#    correlations <- as.character(tclvalue(correlationsVariable))
#    x <- getSelection(xBox)
#    if (2 > length(x)) {
#      errorCondition(recall=correlationTest,
#        message=gettextRcmdr("Fewer than 2 variables selected."))
#      return()
#    }
#    if(2 < length(x)) {
#      errorCondition(recall=correlationTest,
#        message=gettextRcmdr("More than 2 variables selected."))
#      return()
#    }
#    closeDialog()
#    .activeDataSet <- ActiveDataSet()
#    command <- paste("cor.test(", .activeDataSet, "$", x[1], ", ", .activeDataSet, "$", x[2],
#        ', alternative="', alternative, '", method="', correlations, '")', sep="")
#    doItAndPrint(command)  
#    tkfocus(CommanderWindow())
#  }
#  OKCancelHelp(helpSubject="cor.test")
#  tkgrid(getFrame(xBox), sticky="nw")
#  tkgrid(labelRcmdr(top, text=""))
#  tkgrid(correlationsFrame,alternativeFrame, sticky="w")
#  tkgrid(buttonsFrame,columnspan=2,sticky="w")
#  dialogSuffix(rows=4, columns=1)
#}

# the following dialog contributed by Stefano Calza, modified by J. Fox

correlationTest <- function(){
	defaults <- list(initial.x=NULL,initial.correlations="pearson",initial.alternative ="two.sided")
	dialog.values <- getDialog("correlationTest", defaults)
	initializeDialog(title=gettextRcmdr("Correlation Test"))
	xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick two)"),initialSelection=varPosn(dialog.values$initial.x, "numeric"))
	radioButtons(name="correlations", buttons=c("pearson", "spearman", "kendall"),
			labels=gettextRcmdr(c("Pearson product-moment", "Spearman rank-order", "Kendall's tau")),
			initialValue=dialog.values$initial.correlations, 
			title=gettextRcmdr("Type of Correlation"))
	radioButtons(name="alternative", buttons=c("two.sided", "less", "greater"), 
			values=c("two.sided", "less", "greater"),
			initialValue=dialog.values$initial.alternative, 
			labels=gettextRcmdr(c("Two-sided", "Correlation < 0", "Correlation > 0")), 
			title=gettextRcmdr("Alternative Hypothesis"))  
	onOK <- function(){
		alternative <- as.character(tclvalue(alternativeVariable))
		correlations <- as.character(tclvalue(correlationsVariable))
		x <- getSelection(xBox)
		putDialog("correlationTest", list(initial.alternative=alternative, initial.correlations=correlations, initial.x=x))
		if (2 > length(x)) {
			errorCondition(recall=correlationTest,
					message=gettextRcmdr("Fewer than 2 variables selected."))
			return()
		}
		if(2 < length(x)) {
			errorCondition(recall=correlationTest,
					message=gettextRcmdr("More than 2 variables selected."))
			return()
		}
		closeDialog()
		.activeDataSet <- ActiveDataSet()
		command <- paste("cor.test(", .activeDataSet, "$", x[1], ", ", .activeDataSet, "$", x[2],
				', alternative="', alternative, '", method="', correlations, '")', sep="")
		doItAndPrint(command)  
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="cor.test", reset="correlationTest")
	tkgrid(getFrame(xBox), sticky="nw")
	tkgrid(labelRcmdr(top, text=""))
	tkgrid(correlationsFrame,alternativeFrame, sticky="w")
	tkgrid(buttonsFrame,columnspan=2,sticky="w")
	dialogSuffix(rows=4, columns=1)
}

countMissing <- function(){
  command <- paste("sapply(", activeDataSet(), 
    ", function(x)(sum(is.na(x)))) # NA counts", sep="")
  doItAndPrint(command)
  invisible(NULL)
  }
  
#ShapiroTest <- function(){
#    initializeDialog(title=gettextRcmdr("Shapiro-Wilk Test for Normality"))
#    variableBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
#    onOK <- function(){
#        var <- getSelection(variableBox)
#        if (length(var) == 0) {
#            errorCondition(recall=ShapiroTest, message=gettextRcmdr("You must select a variable."))
#            return()
#            }
#        closeDialog()
#        doItAndPrint(paste("shapiro.test(",ActiveDataSet(), "$", var, ")", sep=""))
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="shapiro.test")
#    tkgrid(getFrame(variableBox), sticky="nw")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=2, columns=1)
#    }

ShapiroTest <- function () {
	defaults <- list (initial.var = NULL)
	dialog.values <- getDialog ("ShapiroTest", defaults)
	initializeDialog(title = gettextRcmdr("Shapiro-Wilk Test for Normality"))
	variableBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),
			initialSelection = varPosn (dialog.values$initial.var, "numeric"))
	onOK <- function() {
		var <- getSelection(variableBox)
		putDialog ("ShapiroTest", list (initial.var = var))
		if (length(var) == 0) {
			errorCondition(recall = ShapiroTest, message = gettextRcmdr("You must select a variable."))
			return()
		}
		closeDialog()
		doItAndPrint(paste("shapiro.test(", ActiveDataSet(), 
						"$", var, ")", sep = ""))
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "shapiro.test", reset = "ShapiroTest")
	tkgrid(getFrame(variableBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}
