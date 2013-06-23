# Statistics Menu dialogs

# last modified 2013-06-23 by J. Fox

# Nonparametric tests menu

twoSampleWilcoxonTest <- function () {
    defaults <- list(initial.group = NULL, initial.response = NULL, initial.alternative = "two.sided", 
        initial.test = "default", initial.label=NULL, initial.tab=0)
    dialog.values <- getDialog("twoSampleWilcoxonTest", defaults)
    initializeDialog(title = gettextRcmdr("Two-Sample Wilcoxon Test"))
    notebook <- ttknotebook(top)
    dataTab <- tkframe(top)
    optionsTab <- tkframe(top)
    groupBox <- variableListBox(dataTab, TwoLevelFactors(), title = gettextRcmdr("Groups (pick one)"),
        initialSelection = varPosn(dialog.values$initial.group, "twoLevelFactor"))
    responseBox <- variableListBox(dataTab, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
        initialSelection = varPosn(dialog.values$initial.response, "numeric"))
    onOK <- function() {
        tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall = twoSampleWilcoxonTest, message = gettextRcmdr("You must select a groups variable."))
            return()
        }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall = twoSampleWilcoxonTest, message = gettextRcmdr("You must select a response variable."))
            return()
        }
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        closeDialog()
        putDialog("twoSampleWilcoxonTest", list(initial.group = group, initial.response = response, 
            initial.test = test, initial.alternative = alternative, initial.label=.groupsLabel, initial.tab=tab))
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", 
            response, sep = ""), ", ", paste(.activeDataSet, 
                "$", group, sep = ""), ", median, na.rm=TRUE)", sep = ""))
        if (test == "default") {
            doItAndPrint(paste("wilcox.test(", response, " ~ ", 
                group, ", alternative=\"", alternative, "\", data=", 
                .activeDataSet, ")", sep = ""))
        }
        else doItAndPrint(paste("wilcox.test(", response, " ~ ", 
            group, ", alternative='", alternative, "', exact=", 
            test == "exact", ", correct=", test == "correct", 
            ", data=", .activeDataSet, ")", sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "wilcox.test", reset = "twoSampleWilcoxonTest",
        apply = "twoSampleWilcoxonTest")
    radioButtons(optionsTab, name = "alternative", buttons = c("twosided", 
        "less", "greater"), values = c("two.sided", "less", "greater"), 
        labels = gettextRcmdr(c("Two-sided", "Difference < 0", 
            "Difference > 0")), initialValue = dialog.values$initial.alternative,
        title = gettextRcmdr("Alternative Hypothesis"))
    radioButtons(optionsTab, name = "test", buttons = c("default", "exact", 
        "normal", "correct"), labels = gettextRcmdr(c("Default", 
            "Exact", "Normal approximation", "Normal approximation with\ncontinuity correction")), 
        initialValue = dialog.values$initial.test,
        title = gettextRcmdr("Type of Test"))
    tkgrid(getFrame(groupBox), labelRcmdr(dataTab, text="  "), getFrame(responseBox), sticky = "nw")
    groupsLabel(optionsTab, groupsBox = groupBox, columnspan = 3, initialText=dialog.values$initial.label)
    tkgrid(alternativeFrame, labelRcmdr(optionsTab, text="  "), testFrame, sticky = "nw")
    tkadd(notebook, dataTab, text=gettextRcmdr("Data"), padding=6, sticky="nsew")
    tkadd(notebook, optionsTab, text=gettextRcmdr("Options"), padding=6, sticky="nsew")
    tkgrid(notebook, sticky="nsew")
    tkgrid(buttonsFrame, sticky = "ew")
    if (getRcmdr("restoreTab")) tkselect(notebook, dialog.values$initial.tab)
    dialogSuffix(rows = 4, columns = 1)
}

pairedWilcoxonTest <- function () {
    defaults <- list(initial.x = NULL, initial.y = NULL, initial.alternative = "two.sided", 
        initial.test = "default", initial.tab=0)
    dialog.values <- getDialog("pairedWilcoxonTest", defaults)
    initializeDialog(title = gettextRcmdr("Paired Wilcoxon Test"))
    notebook <- ttknotebook(top)
    dataTab <- tkframe(top)
    optionsTab <- tkframe(top)
    .numeric <- Numeric()
    xBox <- variableListBox(dataTab, .numeric, title = gettextRcmdr("First variable (pick one)"), 
        initialSelection = varPosn(dialog.values$initial.x, "numeric"))
    yBox <- variableListBox(dataTab, .numeric, title = gettextRcmdr("Second variable (pick one)"), 
        initialSelection = varPosn(dialog.values$initial.y, "numeric"))
    onOK <- function() {
        tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        closeDialog()
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        putDialog("pairedWilcoxonTest", list(initial.x = x, initial.y = y, 
            initial.test = test, initial.alternative = alternative, initial.tab=tab))
        if (length(x) == 0 | length(y) == 0) {
            errorCondition(recall = pairedWilcoxonTest, message = gettextRcmdr("You must select two variables."))
            return()
        }
        if (x == y) {
            errorCondition(recall = pairedWilcoxonTest, message = gettextRcmdr("The two variables must be different."))
            return()
        }
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("median(", .activeDataSet, "$", x, 
            " - ", .activeDataSet, "$", y, ", na.rm=TRUE) # median difference", 
            sep = ""))
        if (test == "default") {
            doItAndPrint(paste("wilcox.test(", .activeDataSet, 
                "$", x, ", ", .activeDataSet, "$", y, ", alternative='", 
                alternative, "', paired=TRUE)", sep = ""))
        }
        else if (test == "exact") {
            doItAndPrint(paste("wilcox.test(", .activeDataSet, 
                "$", x, ", ", .activeDataSet, "$", y, ", alternative='", 
                alternative, "', exact=TRUE, paired=TRUE)", sep = ""))
        }
        else {
            doItAndPrint(paste("wilcox.test(", .activeDataSet, 
                "$", x, ", ", .activeDataSet, "$", y, ", alternative='", 
                alternative, "', correct=", test == "correct", 
                ", exact=FALSE, paired=TRUE)", sep = ""))
        }
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "wilcox.test", reset = "pairedWilcoxonTest",
        apply = "pairedWilcoxonTest")
    radioButtons(optionsTab, name = "alternative", buttons = c("twosided", 
        "less", "greater"), values = c("two.sided", "less", "greater"), 
        labels = gettextRcmdr(c("Two-sided", "Difference < 0", 
            "Difference > 0")), title = gettextRcmdr("Alternative Hypothesis"), 
        initialValue = dialog.values$initial.alternative)
    radioButtons(optionsTab, name = "test", buttons = c("default", "exact", 
        "normal", "correct"), labels = gettextRcmdr(c("Default", 
            "Exact", "Normal approximation", "Normal approximation with\ncontinuity correction")), 
        title = gettextRcmdr("Type of Test"), initialValue = dialog.values$initial.test)
    tkgrid(getFrame(xBox), labelRcmdr(dataTab, text="  "), getFrame(yBox), sticky = "nw")
    tkgrid(alternativeFrame, labelRcmdr(optionsTab, text="  "), testFrame, sticky = "nw")
    tkadd(notebook, dataTab, text=gettextRcmdr("Data"), padding=6, sticky="nsew")
    tkadd(notebook, optionsTab, text=gettextRcmdr("Options"), padding=6, sticky="nsew")
    tkgrid(notebook, sticky="nsew")
    tkgrid(buttonsFrame, sticky = "ew")
    if (getRcmdr("restoreTab")) tkselect(notebook, dialog.values$initial.tab)
    dialogSuffix(rows = 3, columns = 1)
}

KruskalWallisTest <- function () {
    defaults <- list(initial.group = NULL, initial.response = NULL)
    dialog.values <- getDialog("KruskalWallisTest", defaults)
    initializeDialog(title = gettextRcmdr("Kruskal-Wallis Rank Sum Test"))
    dataFrame <- tkframe(top)
    groupBox <- variableListBox(dataFrame, Factors(), title = gettextRcmdr("Groups (pick one)"),
        initialSelection = varPosn(dialog.values$initial.group, "factor"))
    responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
        initialSelection = varPosn(dialog.values$initial.response, "numeric"))
    onOK <- function() {
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall = KruskalWallisTest, message = gettextRcmdr("You must select a groups variable."))
            return()
        }
        response <- getSelection(responseBox)
        closeDialog()
        putDialog("KruskalWallisTest", list(initial.group = group, initial.response = response))
        if (length(response) == 0) {
            errorCondition(recall = KruskalWallisTest, message = gettextRcmdr("You must select a response variable."))
            return()
        }
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", 
            response, sep = ""), ", ", paste(.activeDataSet, 
                "$", group, sep = ""), ", median, na.rm=TRUE)", sep = ""))
        doItAndPrint(paste("kruskal.test(", response, " ~ ", 
            group, ", data=", .activeDataSet, ")", sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "kruskal.test", reset = "KruskalWallisTest",
        apply = "KruskalWallisTest")
    tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="   "), getFrame(responseBox), sticky = "nw")
    tkgrid(dataFrame, sticky="w")
    tkgrid(buttonsFrame, sticky = "ew")
    dialogSuffix(rows = 2, columns = 1)
}

FriedmanTest <- function () {
	defaults <- list(initial.response = NULL)
	dialog.values <- getDialog("FriedmanTest", defaults)
	initializeDialog(title = gettextRcmdr("Friedman Rank Sum Test"))
	responseBox <- variableListBox(top, Numeric(), selectmode = "multiple", 
			initialSelection = varPosn(dialog.values$initial.response, "numeric"),
			title = gettextRcmdr("Repeated-Measures Variables (pick two or more)"))
	onOK <- function() {
		responses <- getSelection(responseBox)
		closeDialog()
		putDialog("FriedmanTest", list (initial.response = responses))
		if (length(responses) < 2) {
			errorCondition(recall = FriedmanTest, message = gettextRcmdr("You must select at least two variables."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		command <- paste("na.omit(with(", .activeDataSet, ", cbind(", 
				paste(responses, collapse = ", "), ")))", sep = "")
# 		logger(paste(".Responses <- ", command, sep = ""))
# 		assign(".Responses", justDoIt(command), envir = .GlobalEnv)
		doItAndPrint(paste(".Responses <- ", command, sep = ""))
		doItAndPrint("apply(.Responses, 2, median)")
		doItAndPrint("friedman.test(.Responses)")
		logger("remove(.Responses)")
		remove(.Responses, envir = .GlobalEnv)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "friedman.test", reset = "FriedmanTest",
	             apply = "FriedmanTest")
	tkgrid(getFrame(responseBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

