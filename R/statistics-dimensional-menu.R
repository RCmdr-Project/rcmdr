# Statistics Menu dialogs

# last modified 2011-11-24 by J. Fox

    # Dimensional-analysis menu
    
Reliability <- function(){
    initializeDialog(title=gettextRcmdr("Scale Reliability"))
    xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick three or more)"))
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
        if (3 > length(x)) {
            errorCondition(recall=Reliability, message=gettextRcmdr("Fewer than 3 variables selected."))
            return()
            }
        x <- paste('"', x, '"', sep="")
        doItAndPrint(paste("reliability(cov(", ActiveDataSet(), "[,c(", paste(x, collapse=","),
            ')], use="complete.obs"))', sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="reliability")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

principalComponents <- function(){
    initializeDialog(title=gettextRcmdr("Principal Components Analysis"))
    xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick two or more)"))
    subsetBox()
    checkBoxes(frame="optionsFrame", boxes=c("correlations", "screeplot", "addPC"), initialValues=c("1", "0", "0"),
        labels=gettextRcmdr(c("Analyze correlation matrix", "Screeplot", "Add principal components to data set")))
    onOK <- function(){
        putRcmdr("ncomponents", 0)
        x <- getSelection(xBox)
        nvar <- length(x)
        correlations <- tclvalue(correlationsVariable)
        subset <- tclvalue(subsetVariable)
        screeplot <- tclvalue(screeplotVariable)
        addPC <- tclvalue(addPCVariable)
        closeDialog()
        if (2 > length(x)) {
            errorCondition(recall=principalComponents, message=gettextRcmdr("Fewer than 2 variables selected."))
            return()
            }
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="")
        correlations <- if (correlations == "1") "TRUE" else "FALSE"
        .activeDataSet <- ActiveDataSet()
        command <- paste("princomp(~", paste(x, collapse="+"), ", cor=", correlations,
            ", data=", .activeDataSet, subset, ")", sep="")
        assign(".PC", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".PC <- ", command, sep=""))
        doItAndPrint("unclass(loadings(.PC))  # component loadings")
        doItAndPrint(".PC$sd^2  # component variances")
		doItAndPrint("summary(.PC) # proportions of variance")
        if (screeplot == "1") {
            justDoIt("screeplot(.PC)")
            logger("screeplot(.PC)")
            }
        if (addPC == "1") {
            initializeDialog(subdialog, title=gettextRcmdr("Number of Components"))
            tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Number of components to retain:"), fg="blue"), sticky="w")    
            sliderFrame <- tkframe(subdialog)
            sliderValue <- tclVar("1")
            componentsSlider <- tkscale(sliderFrame, from=1, to=nvar, showvalue=FALSE, variable=sliderValue,
                resolution=1, orient="horizontal")
            componentsShow <- labelRcmdr(sliderFrame, textvariable=sliderValue, width=2, justify="right")
            onOKsub <- function() {
                closeDialog(subdialog)
                putRcmdr("ncomponents", as.numeric(tclvalue(sliderValue)))
                    }
            subOKCancelHelp()
            tkgrid(componentsSlider, componentsShow, sticky="nw")
            tkgrid(sliderFrame, sticky="w")
            tkgrid(subButtonsFrame, sticky="w")
            dialogSuffix(subdialog, onOK=onOKsub, rows=2, columns=1, focus=subdialog)
            if ((ncomponents <- getRcmdr("ncomponents")) > 0){
                for(i in 1:ncomponents){
                    var <- paste("PC", i, sep="")
                    if (is.element(var, Variables())) {
                        if ("no" == tclvalue(checkReplace(var))) next
                        }
                    justDoIt(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", i, "]", sep=""))
                    logger(paste(.activeDataSet, "$PC", i, " <- .PC$scores[,", i, "]", sep=""))
                    }
                activeDataSet(.activeDataSet)
                }
            }
        remove(.PC, envir=.GlobalEnv)   
        logger("remove(.PC)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="princomp")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

#factorAnalysis <- function(){
#	initializeDialog(title=gettextRcmdr("Factor Analysis"))
#	xBox <- variableListBox(top, Numeric(), selectmode="multiple", title=gettextRcmdr("Variables (pick three or more)"))
#	subsetBox()
#	optionsFrame <- tkframe(top)
#	checkFrame <- tkframe(top)
#	radioButtons(checkFrame, name="rotation", buttons=c("noRotate", "varimax", "promax"), 
#			values=c("none", "varimax", "promax"), initialValue="varimax", labels=gettextRcmdr(c("None", "Varimax", "Promax")),
#			title=gettextRcmdr("Factor Rotation"))
#	radioButtons(checkFrame, name="scores", buttons=c("noScores", "bartlett", "regression"),
#			values=c("none", "Bartlett", "regression"), labels=gettextRcmdr(c("None", "Bartlett's method", "Regression method")),
#			title=gettextRcmdr("Factor Scores"))
#	onOK <- function(){
#		x <- getSelection(xBox)
#		nvar <- length(x)
#		subset <- tclvalue(subsetVariable)
#		rotation <- tclvalue(rotationVariable)
#		scores <- tclvalue(scoresVariable)
#		closeDialog()
#		if (3 > length(x)) {
#			errorCondition(recall=factorAnalysis, message=gettextRcmdr("Fewer than 3 variables selected."))
#			return()
#		}
#		f <- function(k, p) ((p - k)^2 - p - k)^2
#		max.factors <- floor(optimize(f, c(0, nvar), tol=.0001, p=nvar)$minimum)
#		if (max.factors == 1){
#			putRcmdr("nfactors", 1)
#		}
#		else{
#			initializeDialog(subdialog, title=gettextRcmdr("Number of Factors"))
#			tkgrid(labelRcmdr(subdialog, text=gettextRcmdr("Number of factors to extract:"), fg="blue"), sticky="w")    
#			sliderFrame <- tkframe(subdialog)
#			sliderValue <- tclVar("1")
#			componentsSlider <- tkscale(sliderFrame, from=1, to=max.factors, showvalue=FALSE, variable=sliderValue,
#					resolution=1, orient="horizontal")
#			componentsShow <- labelRcmdr(sliderFrame, textvariable=sliderValue, width=2, justify="right")
#			onOKsub <- function() {
#				closeDialog(subdialog)
#				putRcmdr("nfactors", as.numeric(tclvalue(sliderValue)))
#			}
#			subOKCancelHelp()
#			tkgrid(componentsSlider, componentsShow, sticky="nw")
#			tkgrid(sliderFrame, sticky="w")
#			tkgrid(subButtonsFrame, sticky="w")
#			dialogSuffix(subdialog, onOK=onOKsub, rows=2, columns=1, focus=subdialog)
#		}
#		subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) "" else paste(", subset=", subset, sep="")
#		.activeDataSet <- ActiveDataSet()
#		command <- paste("factanal(~", paste(x, collapse="+"), ", factors=", getRcmdr("nfactors"), ', rotation="', rotation,
#				'", scores="', scores, '", data=', .activeDataSet, subset, ")", sep="")
#		assign(".FA", justDoIt(command), envir=.GlobalEnv)
#		logger(paste(".FA <- ", command, sep=""))
#		doItAndPrint(".FA")
#		if (scores != "none") {
#			for(i in 1:getRcmdr("nfactors")){
#				var <- paste("F", i, sep="")
#				if (is.element(var, Variables())) {
#					if ("no" == tclvalue(checkReplace(var))) next
#				}
#				justDoIt(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", i, "]", sep=""))
#				logger(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", i, "]", sep=""))
#			}
#			activeDataSet(.activeDataSet)
#		}
#		logger("remove(.FA)")
#		remove(.FA, envir=.GlobalEnv)
#		tkfocus(CommanderWindow())
#	}
#	OKCancelHelp(helpSubject="factanal")
#	tkgrid(getFrame(xBox), sticky="nw")
#	tkgrid(subsetFrame, sticky="w")
#	tkgrid(optionsFrame, sticky="w")
#	tkgrid(rotationFrame, labelRcmdr(checkFrame, text="    "), scoresFrame, sticky="w")
#	tkgrid(checkFrame, sticky="w")
#	tkgrid(buttonsFrame,  sticky="w")
#	dialogSuffix(rows=5, columns=1)
#}

factorAnalysis <- function () {
	defaults <- list(initial.x = NULL, initial.subset = gettextRcmdr ("<all valid cases>"), 
			initial.rotation = "varimax", initial.scores = "none")
	dialog.values <- getDialog("factorAnalysis", defaults)
	initializeDialog(title = gettextRcmdr("Factor Analysis"))
	xBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.x, "numeric"),
			title = gettextRcmdr("Variables (pick three or more)"))
	subsetBox(subset.expression = dialog.values$initial.subset)
	optionsFrame <- tkframe(top)
	checkFrame <- tkframe(top)
	radioButtons(checkFrame, name = "rotation", buttons = c("noRotate", 
					"varimax", "promax"), values = c("none", "varimax", "promax"), 
			initialValue = dialog.values$initial.rotation, labels = gettextRcmdr(c("None", 
							"Varimax", "Promax")), title = gettextRcmdr("Factor Rotation"))
	radioButtons(checkFrame, name = "scores", buttons = c("noScores", 
					"bartlett", "regression"), values = c("none", "Bartlett", 
					"regression"), initialValue = dialog.values$initial.scores,
			labels = gettextRcmdr(c("None", "Bartlett's method", 
							"Regression method")), title = gettextRcmdr("Factor Scores"))
	onOK <- function() {
		x <- getSelection(xBox)
		nvar <- length(x)
		subset <- tclvalue(subsetVariable)
		rotation <- tclvalue(rotationVariable)
		scores <- tclvalue(scoresVariable)
		closeDialog()
		putDialog ("factorAnalysis", list (initial.x = x, initial.subset = subset, 
						initial.scores = scores, initial.rotation = rotation))
		if (3 > length(x)) {
			errorCondition(recall = factorAnalysis, message = gettextRcmdr("Fewer than 3 variables selected."))
			return()
		}
		f <- function(k, p) ((p - k)^2 - p - k)^2
		max.factors <- floor(optimize(f, c(0, nvar), tol = 1e-04, 
						p = nvar)$minimum)
		if (max.factors == 1) {
			putRcmdr("nfactors", 1)
		}
		else {
			initializeDialog(subdialog, title = gettextRcmdr("Number of Factors"))
			tkgrid(labelRcmdr(subdialog, text = gettextRcmdr("Number of factors to extract:"), 
							fg = "blue"), sticky = "w")
			sliderFrame <- tkframe(subdialog)
			sliderValue <- tclVar("1")
			componentsSlider <- tkscale(sliderFrame, from = 1, 
					to = max.factors, showvalue = FALSE, variable = sliderValue, 
					resolution = 1, orient = "horizontal")
			componentsShow <- labelRcmdr(sliderFrame, textvariable = sliderValue, 
					width = 2, justify = "right")
			onOKsub <- function() {
				closeDialog(subdialog)
				putRcmdr("nfactors", as.numeric(tclvalue(sliderValue)))
			}
			subOKCancelHelp()
			tkgrid(componentsSlider, componentsShow, sticky = "nw")
			tkgrid(sliderFrame, sticky = "w")
			tkgrid(subButtonsFrame, sticky = "w")
			dialogSuffix(subdialog, onOK = onOKsub, rows = 2, 
					columns = 1, focus = subdialog)
		}
		subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) 
					""
				else paste(", subset=", subset, sep = "")
		.activeDataSet <- ActiveDataSet()
		command <- paste("factanal(~", paste(x, collapse = "+"), 
				", factors=", getRcmdr("nfactors"), ", rotation=\"", 
				rotation, "\", scores=\"", scores, "\", data=", .activeDataSet, 
				subset, ")", sep = "")
		assign(".FA", justDoIt(command), envir = .GlobalEnv)
		logger(paste(".FA <- ", command, sep = ""))
		doItAndPrint(".FA")
		if (scores != "none") {
			for (i in 1:getRcmdr("nfactors")) {
				var <- paste("F", i, sep = "")
				if (is.element(var, Variables())) {
					if ("no" == tclvalue(checkReplace(var))) 
						next
				}
				justDoIt(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", 
								i, "]", sep = ""))
				logger(paste(.activeDataSet, "$F", i, " <- .FA$scores[,", 
								i, "]", sep = ""))
			}
			activeDataSet(.activeDataSet, flushDialogMemory = FALSE)
		}
		logger("remove(.FA)")
		remove(.FA, envir = .GlobalEnv)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "factanal", reset = "factorAnalysis")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(rotationFrame, labelRcmdr(checkFrame, text = "    "), 
			scoresFrame, sticky = "w")
	tkgrid(checkFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 5, columns = 1)
}
