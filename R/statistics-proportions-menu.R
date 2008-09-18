# Statistics Menu dialogs

# last modified 26 March 2008 by J. Fox

    # Proportions menu
    
singleProportionTest <- function(){
    initializeDialog(title=gettextRcmdr("Single-Sample Proportion Test"))
    xBox <- variableListBox(top, TwoLevelFactors(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        if (length(x) == 0) {
            errorCondition(recall=singleProportionTest, message=gettextRcmdr("You must select a variable."))
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        test <- as.character(tclvalue(testVariable))
        p <- tclvalue(pVariable)
        closeDialog()
        command <- paste("xtabs(~", x, ", data=", ActiveDataSet(), ")")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(".Table")
        if (test == "normal") doItAndPrint(paste("prop.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ", correct=FALSE)", sep=""))
        else if (test == "corrected") doItAndPrint(paste("prop.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ", correct=TRUE)", sep=""))
        else doItAndPrint(paste("binom.test(rbind(.Table), alternative='", 
            alternative, "', p=", p, ", conf.level=", level, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="prop.test")
    radioButtons(top, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Population proportion = p0", "Population proportion < p0", "Population proportion > p0")), title=gettextRcmdr("Alternative Hypothesis"))
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    pFrame <- tkframe(rightFrame)
    pVariable <- tclVar(".5")
    pField <- ttkentry(pFrame, width="6", textvariable=pVariable)
    radioButtons(name="test", buttons=c("normal", "corrected", "exact"), 
        labels=gettextRcmdr(c("Normal approximation", "Normal approximation with\ncontinuity correction", "Exact binomial")), 
        title=gettextRcmdr("Type of Test"))
    tkgrid(getFrame(xBox), sticky="nw")    
    tkgrid(labelRcmdr(pFrame, text=gettextRcmdr("Null hypothesis: p = "), fg="blue"), pField, sticky="w")
    tkgrid(pFrame, sticky="w")
    tkgrid(labelRcmdr(rightFrame, text=""))
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level: "), fg="blue"), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(testFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(confidenceField, sticky="e")
    dialogSuffix(rows=4, columns=2)
    }

twoSampleProportionsTest <- function(){
    require("abind")
    initializeDialog(title=gettextRcmdr("Two-Sample Proportions Test"))
    .twoLevelFactors <- TwoLevelFactors()
    groupsBox <- variableListBox(top, .twoLevelFactors, title=gettextRcmdr("Groups (pick one)"))
    xBox <- variableListBox(top, .twoLevelFactors, title=gettextRcmdr("Response Variable (pick one)"))
    onOK <- function(){
        groups <- getSelection(groupsBox)
        if (length(groups) == 0) {
            errorCondition(recall=twoSampleProportionsTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        x <- getSelection(xBox)
        if (length(x) == 0) {
            errorCondition(recall=twoSampleProportionsTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        if (x == groups) {
            errorCondition(recall=twoSampleProportionsTest, message=gettextRcmdr("Groups and response variables must be different."))
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        test <- as.character(tclvalue(testVariable))
        closeDialog()
        command <- paste("xtabs(~", groups, "+", x, ", data=", ActiveDataSet(), ")", sep="")
        logger(paste(".Table <-", command))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint("rowPercents(.Table)")
        if (test == "normal") doItAndPrint(paste("prop.test(.Table, alternative='", 
            alternative, "', conf.level=", level, ", correct=FALSE)", sep=""))
        else doItAndPrint(paste("prop.test(.Table, alternative='", 
            alternative, "', conf.level=", level, ", correct=TRUE)", sep=""))
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="prop.test")
    radioButtons(name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    radioButtons(name="test", buttons=c("normal", "corrected"), 
        labels=gettextRcmdr(c("Normal approximation", "Normal approximation with\ncontinuity correction")), title=gettextRcmdr("Type of Test"))
    tkgrid(getFrame(groupsBox), getFrame(xBox), sticky="nw")    
    groupsLabel(columnspan=2)
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level: "), fg="blue"), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(testFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(confidenceField, sticky="e")
    dialogSuffix(rows=5, columns=2)
    }
