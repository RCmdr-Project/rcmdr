# Statistics Menu dialogs

# last modified 1 July 05 by J. Fox

    # Nonparametric tests menu
    
twoSampleWilcoxonTest <- function(){
    initializeDialog(title=gettextRcmdr("Two-Sample Wilcoxon Test"))
    groupBox <- variableListBox(top, TwoLevelFactors(), title=gettextRcmdr("Groups (pick one)"))
    responseBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=twoSampleWilcoxonTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=twoSampleWilcoxonTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet,"$", response, sep=""),
            ", ", paste(.activeDataSet,"$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        if (test == "default"){
            doItAndPrint(paste("wilcox.test(", response, " ~ ", group, ', alternative="', 
            alternative, '", data=', .activeDataSet, ")", sep=""))
            }
        else doItAndPrint(paste("wilcox.test(", response, " ~ ", group, ", alternative='", 
            alternative, "', exact=", test=="exact", 
            ", correct=", test=="correct",", data=", .activeDataSet, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="wilcox.test")
    radioButtons(name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))
    radioButtons(name="test", buttons=c("default", "exact", "normal", "correct"), 
        labels=gettextRcmdr(c("Default", "Exact", "Normal approximation", "Normal approximation with\ncontinuity correction")), 
        title=gettextRcmdr("Type of Test"))
    tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    groupsLabel(groupsBox=groupBox, columnspan=2)
    tkgrid(alternativeFrame, testFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=4, columns=2)
    }    

pairedWilcoxonTest <- function(){
    initializeDialog(title=gettextRcmdr("Paired Wilcoxon Test"))
    .numeric <- Numeric()
    xBox <- variableListBox(top, .numeric, title=gettextRcmdr("First variable (pick one)"))
    yBox <- variableListBox(top, .numeric, title=gettextRcmdr("Second variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        closeDialog()
        alternative <- as.character(tclvalue(alternativeVariable))
        test <- as.character(tclvalue(testVariable))
        if (length(x) == 0 | length(y) == 0) {
            errorCondition(recall=pairedWilcoxonTest, message=gettextRcmdr("You must select two variables."))
            return()
            }
        if (x == y) {
            errorCondition(recall=pairedWilcoxonTest, message=gettextRcmdr("The two variables must be different."))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("median(", .activeDataSet, "$", x, " - ", .activeDataSet, "$", y, 
            ", na.rm=TRUE) # median difference", sep=""))
        if (test == "default"){
             doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative,
                "', paired=TRUE)", sep=""))           
            }
        else if (test == "exact"){
            doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative,
                "', exact=TRUE, paired=TRUE)", sep=""))
                }
        else {
            doItAndPrint(paste("wilcox.test(", .activeDataSet, "$", x, ", ", 
                .activeDataSet, "$", y,
                ", alternative='", alternative, "', correct=", test=="correct",
                ", exact=FALSE, paired=TRUE)", sep=""))
                }
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="wilcox.test")
    radioButtons(name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))
    radioButtons(name="test", buttons=c("default", "exact", "normal", "correct"), 
        labels=gettextRcmdr(c("Default", "Exact", "Normal approximation", "Normal approximation with\ncontinuity correction")), 
        title=gettextRcmdr("Type of Test"))
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")    
    tkgrid(alternativeFrame, testFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }
    
KruskalWallisTest <- function(){
    initializeDialog(title=gettextRcmdr("Kruskal-Wallis Rank Sum Test"))
    groupBox <- variableListBox(top, Factors(), title=gettextRcmdr("Groups (pick one)"))
    responseBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=KruskalWallisTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        closeDialog()
        if (length(response) == 0) {
            errorCondition(recall=KruskalWallisTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("tapply(", paste(.activeDataSet, "$", response, sep=""),
            ", ", paste(.activeDataSet, "$", group, sep=""), ", median, na.rm=TRUE)", sep=""))
        doItAndPrint(paste("kruskal.test(", response, " ~ ", group, ", data=",
            .activeDataSet, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="kruskal.test")
    tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=2, columns=2)
    }
	
FriedmanTest <- function(){
	initializeDialog(title=gettextRcmdr("Friedman Rank Sum Test"))
	responseBox <- variableListBox(top, Numeric(), selectmode="multiple", 
			title=gettextRcmdr("Repeated-Measures Variables (pick two or more)"))
	onOK <- function(){
		responses <- getSelection(responseBox)
		closeDialog()
		if (length(responses) < 2) {
			errorCondition(recall=FriedmanTest, message=gettextRcmdr("You must select at least two variables."))
			return()
			}
		.activeDataSet <- ActiveDataSet()
		command <- paste('na.omit(with(', .activeDataSet, ', cbind(', paste(responses, collapse=", "), ')))', sep="")
		logger(paste(".Responses <- ", command, sep=""))
		assign(".Responses", justDoIt(command), envir=.GlobalEnv)
		doItAndPrint("apply(.Responses, 2, median)")
		doItAndPrint("friedman.test(.Responses)")
		logger("remove(.Responses)")
		remove(.Responses, envir=.GlobalEnv)
		tkfocus(CommanderWindow())
		}
	OKCancelHelp(helpSubject="friedman.test")
	tkgrid(getFrame(responseBox), sticky="nw")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=2, columns=1)
	}
