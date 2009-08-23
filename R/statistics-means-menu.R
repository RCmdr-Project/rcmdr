# Statistics Menu dialogs

# last modified 22 August 2009 by J. Fox

    # Means menu

independentSamplesTTest <- function(){
    initializeDialog(title=gettextRcmdr("Independent Samples t-Test"))
    variablesFrame <- tkframe(top)
    groupBox <- variableListBox(variablesFrame, TwoLevelFactors(), title=gettextRcmdr("Groups (pick one)"))
    responseBox <- variableListBox(variablesFrame, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
    onOK <- function(){
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall=independentSamplesTTest, message=gettextRcmdr("You must select a groups variable."))
            return()
            }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall=independentSamplesTTest, message=gettextRcmdr("You must select a response variable."))
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        variances <- as.character(tclvalue(variancesVariable))
        closeDialog()
        doItAndPrint(paste("t.test(", response, "~", group,
            ", alternative='", alternative, "', conf.level=", level,
            ", var.equal=", variances,
            ", data=", ActiveDataSet(), ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="t.test")
    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))
    confidenceFrame <- tkframe(optionsFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    radioButtons(optionsFrame, name="variances", buttons=c("yes", "no"), values=c("TRUE", "FALSE"), initialValue="FALSE",
        labels=gettextRcmdr(c("Yes", "No")), title=gettextRcmdr("Assume equal variances?"))
    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text="    "), getFrame(responseBox), sticky="nw")
    tkgrid(variablesFrame, sticky="nw")
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level"), fg="blue"),sticky="w")
    tkgrid(confidenceField, sticky="w")
    groupsLabel(groupsBox=groupBox)
    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="    "), confidenceFrame, labelRcmdr(optionsFrame, text="    "),
        variancesFrame, sticky="nw")
    tkgrid(optionsFrame, sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

pairedTTest <- function(){
    initializeDialog(title=gettextRcmdr("Paired t-Test"))
    .numeric <- Numeric()
    xBox <- variableListBox(top, .numeric, title=gettextRcmdr("First variable (pick one)"))
    yBox <- variableListBox(top, .numeric, title=gettextRcmdr("Second variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        if (length(x) == 0 | length(y) == 0){
            errorCondition(recall=pairedTTest, message=gettextRcmdr("You must select two variables."))
            return()
            }
        if (x == y){
            errorCondition(recall=pairedTTest, message=gettextRcmdr("Variables must be different."))
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        closeDialog()
        .activeDataSet <- ActiveDataSet()
        doItAndPrint(paste("t.test(", .activeDataSet, "$", x, ", ",
            .activeDataSet, "$", y,
            ", alternative='", alternative, "', conf.level=", level,
            ", paired=TRUE)", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="t.test")
    radioButtons(top, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Two-sided", "Difference < 0", "Difference > 0")), title=gettextRcmdr("Alternative Hypothesis"))
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level"), fg="blue"))
    tkgrid(confidenceField, sticky="w")
    tkgrid(alternativeFrame, confidenceFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }

singleSampleTTest <- function(){
    initializeDialog(title=gettextRcmdr("Single-Sample t-Test"))
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        if (length(x) == 0){
            errorCondition(recall=singleSampleTTest, message=gettextRcmdr("You must select a variable."))
            return()
            }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        mu <- tclvalue(muVariable)
        closeDialog()
        doItAndPrint(paste("t.test(", ActiveDataSet(), "$", x,
            ", alternative='", alternative, "', mu=", mu, ", conf.level=", level,
            ")", sep=""))
        tkdestroy(top)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="t.test")
    radioButtons(top, name="alternative", buttons=c("twosided", "less", "greater"), values=c("two.sided", "less", "greater"),
        labels=gettextRcmdr(c("Population mean != mu0", "Population mean < mu0", "Population mean > mu0")),
        title=gettextRcmdr("Alternative Hypothesis"))
    rightFrame <- tkframe(top)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(".95")
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    muFrame <- tkframe(rightFrame)
    muVariable <- tclVar("0.0")
    muField <- ttkentry(muFrame, width="8", textvariable=muVariable)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(labelRcmdr(rightFrame, text=""), sticky="w")
    tkgrid(labelRcmdr(muFrame, text=gettextRcmdr("Null hypothesis: mu = ")), muField, sticky="w")
    tkgrid(muFrame, sticky="w")
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level: ")), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    tkgrid(alternativeFrame, rightFrame, sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(confidenceField, sticky="e")
    dialogSuffix(rows=4, columns=2)
    }

	oneWayAnova <- function(){
		Library("multcomp")
		Library("abind")
		initializeDialog(title=gettextRcmdr("One-Way Analysis of Variance"))
		UpdateModelNumber()
		modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"), sep=""))
		modelFrame <- tkframe(top)
		model <- ttkentry(modelFrame, width="20", textvariable=modelName)
		groupBox <- variableListBox(top, Factors(), title=gettextRcmdr("Groups (pick one)"))
		responseBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
		optionsFrame <- tkframe(top)
		pairwiseVariable <- tclVar("0")
		pairwiseCheckBox <- tkcheckbutton(optionsFrame, variable=pairwiseVariable)
		onOK <- function(){
			modelValue <- trim.blanks(tclvalue(modelName))
			if (!is.valid.name(modelValue)){
				UpdateModelNumber(-1)
				errorCondition(recall=oneWayAnova, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
				return()
			}
			if (is.element(modelValue, listAOVModels())) {
				if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
					UpdateModelNumber(-1)
					tkdestroy(top)
					oneWayAnova()
					return()
				}
			}
			group <- getSelection(groupBox)
			response <- getSelection(responseBox)
			closeDialog()
			if (length(group) == 0){
				errorCondition(recall=oneWayAnova, message=gettextRcmdr("You must select a groups factor."))
				return()
			}
			if (length(response) == 0){
				errorCondition(recall=oneWayAnova, message=gettextRcmdr("You must select a response variable."))
				return()
			}
			.activeDataSet <- ActiveDataSet()
			command <- paste(modelValue, " <- aov(", response, " ~ ", group, ", data=", .activeDataSet, ")", sep="")
			justDoIt(command)
			logger(command)
			doItAndPrint(paste("summary(", modelValue, ")", sep=""))
			doItAndPrint(paste("numSummary(", .activeDataSet, "$", response, " , groups=", .activeDataSet, "$", group,
					', statistics=c("mean", "sd"))', sep=""))
			activeModel(modelValue)
			pairwise <- tclvalue(pairwiseVariable)
			if (pairwise == 1) {
				if (eval(parse(text=paste("length(levels(", .activeDataSet, "$", group, ")) < 3"))))
					Message(message=gettextRcmdr("Factor has fewer than 3 levels; pairwise comparisons omitted."),
						type="warning")
				# the following lines modified by Richard Heiberger and subsequently by J. Fox
				else {
					command <- paste(".Pairs <- glht(", modelValue, ", linfct = mcp(", group, ' = "Tukey"))', sep="")
					justDoIt(command)
					logger(command)
					doItAndPrint("confint(.Pairs)")
					justDoIt("old.oma <- par(oma=c(0,5,0,0))")
					logger("old.oma <- par(oma=c(0,5,0,0))")
					justDoIt("plot(confint(.Pairs))")
					logger("plot(confint(.Pairs))")
					justDoIt("par(old.oma)")
					logger("par(old.oma)")
					logger("remove(.Pairs)")
					remove(.Pairs, envir=.GlobalEnv)
				}
			}
			tkfocus(CommanderWindow())
		}
		OKCancelHelp(helpSubject="anova", model=TRUE)
		tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model: ")), model, sticky="w")
		tkgrid(modelFrame, sticky="w", columnspan=2)
		tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
		tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Pairwise comparisons of means")), pairwiseCheckBox, sticky="w")
		tkgrid(optionsFrame, sticky="w", columnspan=2)
		tkgrid(buttonsFrame, columnspan=2, sticky="w")
		dialogSuffix(rows=4, columns=2)
	}

	multiWayAnova <- function(){
		initializeDialog(title=gettextRcmdr("Multi-Way Analysis of Variance"))
		UpdateModelNumber()
		modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"), sep=""))
		modelFrame <- tkframe(top)
		model <- ttkentry(modelFrame, width="20", textvariable=modelName)
		groupBox <- variableListBox(top, Factors(), selectmode="multiple", title=gettextRcmdr("Factors (pick one or more)"))
		responseBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
		onOK <- function(){
			modelValue <- trim.blanks(tclvalue(modelName))
			if (!is.valid.name(modelValue)){
				UpdateModelNumber(-1)
				errorCondition(recall=multiWayAnova, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
				return()
			}
			if (is.element(modelValue, listAOVModels())) {
				if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
					UpdateModelNumber(-1)
					tkdestroy(top)
					multiWayAnova()
					return()
				}
			}
			groups <- getSelection(groupBox)
			response <- getSelection(responseBox)
			closeDialog()
			if (length(groups) == 0){
				errorCondition(recall=multiWayAnova, message=gettextRcmdr("You must select at least one factor."))
				return()
			}
			if (length(response) == 0){
				errorCondition(recall=multiWayAnova, message=gettextRcmdr("You must select a response variable."))
				return()
			}
			.activeDataSet <- ActiveDataSet()
			groups.list <- paste(paste(groups, "=", .activeDataSet, "$", groups, sep=""), collapse=", ")
			doItAndPrint(paste(modelValue, " <- (lm(", response, " ~ ", paste(groups, collapse="*"),
					", data=", .activeDataSet, "))", sep=""))
			doItAndPrint(paste("Anova(", modelValue, ")", sep=""))
			doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
					"), mean, na.rm=TRUE) # means", sep=""))
			doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
					"), sd, na.rm=TRUE) # std. deviations", sep=""))
			doItAndPrint(paste("tapply(", .activeDataSet, "$", response, ", list(", groups.list,
					"), function(x) sum(!is.na(x))) # counts", sep=""))
			activeModel(modelValue)
			tkfocus(CommanderWindow())
		}
		OKCancelHelp(helpSubject="Anova", model=TRUE)
		tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model: ")), model, sticky="w")
		tkgrid(modelFrame, sticky="w", columnspan=2)
		tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
		tkgrid(buttonsFrame, columnspan=2, sticky="w")
		dialogSuffix(rows=4, columns=2)
	}
	