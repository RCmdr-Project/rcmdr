# Statistics Menu dialogs

# last modified 2011-11-27 by J. Fox

    # Models menu

#linearRegressionModel <- function(){
#    initializeDialog(title=gettextRcmdr("Linear Regression"))
#    variablesFrame <- tkframe(top)
#    .numeric <- Numeric()
#    xBox <- variableListBox(variablesFrame, .numeric, selectmode="multiple",
#        title=gettextRcmdr("Explanatory variables (pick one or more)"))
#    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Response variable (pick one)"))
#    UpdateModelNumber()
#    modelName <- tclVar(paste("RegModel.", getRcmdr("modelNumber"), sep=""))
#    modelFrame <- tkframe(top)
#    model <- ttkentry(modelFrame, width="20", textvariable=modelName)
#    subsetBox()
#    onOK <- function(){
#        x <- getSelection(xBox)
#        y <- getSelection(yBox)
#        closeDialog()
#        if (0 == length(y)) {
#            UpdateModelNumber(-1)
#            errorCondition(recall=linearRegressionModel, message=gettextRcmdr("You must select a response variable."))
#            return()
#            }
#        if (0 == length(x)) {
#            UpdateModelNumber(-1)
#            errorCondition(recall=linearRegressionModel, message=gettextRcmdr("No explanatory variables selected."))
#            return()
#            }
#        if (is.element(y, x)) {
#            UpdateModelNumber(-1)
#            errorCondition(recall=linearRegressionModel, message=gettextRcmdr("Response and explanatory variables must be different."))
#            return()
#            }
#        subset <- tclvalue(subsetVariable)
#        if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
#            subset <- ""
#            putRcmdr("modelWithSubset", FALSE)
#            }
#        else{
#            subset <- paste(", subset=", subset, sep="")
#            putRcmdr("modelWithSubset", TRUE)
#            }
#        modelValue <- trim.blanks(tclvalue(modelName))
#        if (!is.valid.name(modelValue)){
#            UpdateModelNumber(-1)
#            errorCondition(recall=linearRegressionModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
#            return()
#            }
#        if (is.element(modelValue, listLinearModels())) {
#            if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
#                UpdateModelNumber(-1)
#                linearRegressionModel()
#                return()
#                }
#            }
#        command <- paste("lm(", y, "~", paste(x, collapse="+"),
#            ", data=", ActiveDataSet(), subset, ")", sep="")
#        logger(paste(modelValue, " <- ", command, sep=""))
#        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
#        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
#        activeModel(modelValue)
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="lm", model=TRUE)
#    tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
#    tkgrid(modelFrame, sticky="w")
#    tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text="    "), getFrame(xBox), sticky="nw")
#    tkgrid(variablesFrame, sticky="w")
#    tkgrid(subsetFrame, sticky="w")
#    tkgrid(buttonsFrame, stick="w")
#    tkgrid.configure(helpButton, sticky="e")
#    dialogSuffix(rows=4, columns=1)
#    }

linearRegressionModel <- function () {
	defaults <- list(initial.x = NULL, initial.y = NULL, 
			initial.subset = gettextRcmdr("<all valid cases>"))
	dialog.values <- getDialog("linearRegressionModel", defaults)
	initializeDialog(title = gettextRcmdr("Linear Regression"))
	variablesFrame <- tkframe(top)
	.numeric <- Numeric()
	xBox <- variableListBox(variablesFrame, .numeric, selectmode = "multiple", 
			title = gettextRcmdr("Explanatory variables (pick one or more)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	yBox <- variableListBox(variablesFrame, .numeric, title = gettextRcmdr("Response variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.y, "numeric"))
	UpdateModelNumber()
	modelName <- tclVar(paste("RegModel.", getRcmdr("modelNumber"), 
					sep = ""))
	modelFrame <- tkframe(top)
	model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
	subsetBox(subset.expression = dialog.values$initial.subset)
	onOK <- function() {
		x <- getSelection(xBox)
		y <- getSelection(yBox)
		closeDialog()
		if (0 == length(y)) {
			UpdateModelNumber(-1)
			errorCondition(recall = linearRegressionModel, message = gettextRcmdr("You must select a response variable."))
			return()
		}
		if (0 == length(x)) {
			UpdateModelNumber(-1)
			errorCondition(recall = linearRegressionModel, message = gettextRcmdr("No explanatory variables selected."))
			return()
		}
		if (is.element(y, x)) {
			UpdateModelNumber(-1)
			errorCondition(recall = linearRegressionModel, message = gettextRcmdr("Response and explanatory variables must be different."))
			return()
		}
		subset <- tclvalue(subsetVariable)
		putDialog ("linearRegressionModel", list (initial.x = x, initial.y = y, 
						initial.subset = subset))
		if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || 
				trim.blanks(subset) == "") {
			subset <- ""
			putRcmdr("modelWithSubset", FALSE)
		}
		else {
			subset <- paste(", subset=", subset, sep = "")
			putRcmdr("modelWithSubset", TRUE)
		}
		modelValue <- trim.blanks(tclvalue(modelName))
		if (!is.valid.name(modelValue)) {
			UpdateModelNumber(-1)
			errorCondition(recall = linearRegressionModel, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
							modelValue))
			return()
		}
		if (is.element(modelValue, listLinearModels())) {
			if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
				UpdateModelNumber(-1)
				linearRegressionModel()
				return()
			}
		}
		command <- paste("lm(", y, "~", paste(x, collapse = "+"), 
				", data=", ActiveDataSet(), subset, ")", sep = "")
		logger(paste(modelValue, " <- ", command, sep = ""))
		assign(modelValue, justDoIt(command), envir = .GlobalEnv)
		doItAndPrint(paste("summary(", modelValue, ")", sep = ""))
		activeModel(modelValue)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "lm", model = TRUE, reset = "linearRegressionModel")
	tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model:")), 
			model, sticky = "w")
	tkgrid(modelFrame, sticky = "w")
	tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text = "    "), 
			getFrame(xBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "w")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(buttonsFrame, stick = "w")
	tkgrid.configure(helpButton, sticky = "e")
	dialogSuffix(rows = 4, columns = 1)
}

linearModel <- function(){
    initializeDialog(title=gettextRcmdr("Linear Model"))
    .activeModel <- ActiveModel()
    currentModel <- if (!is.null(.activeModel))
        class(get(.activeModel, envir=.GlobalEnv))[1] == "lm"
#        eval(parse(text=paste("class(", .activeModel, ")[1] == 'lm'", sep="")),
#            envir=.GlobalEnv)
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
#        currentFields <- formulaFields(eval(parse(text=.activeModel),
#            envir=.GlobalEnv))
        if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
        }
    UpdateModelNumber()
    modelName <- tclVar(paste("LinearModel.", getRcmdr("modelNumber"), sep=""))
    modelFrame <- tkframe(top)
    model <- ttkentry(modelFrame, width="20", textvariable=modelName)
    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()
        if (!is.valid.name(modelValue)){
            errorCondition(recall=linearModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
            subset <- ""
            putRcmdr("modelWithSubset", FALSE)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            putRcmdr("modelWithSubset", TRUE)
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=linearModel, message=gettextRcmdr("Left-hand side of model empty."), model=TRUE)
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=linearModel, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
            return()
            }
        if (is.element(modelValue, listLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
                UpdateModelNumber(-1)
                linearModel()
                return()
                }
            }
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        command <- paste("lm(", formula,
            ", data=", ActiveDataSet(), subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        activeModel(modelValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="linearModel", model=TRUE)
    tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    modelFormula()
    subsetBox(model=TRUE)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
    }

generalizedLinearModel <- function(){
    families <- c("gaussian", "binomial", "poisson", "Gamma", "inverse.gaussian",
        "quasibinomial", "quasipoisson")
    links <- c("identity", "inverse", "log", "logit", "probit",
        "cloglog", "sqrt", "1/mu^2")
    availableLinks <- matrix(c(
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
        FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
        TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE,
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, FALSE,
        TRUE,  TRUE,  TRUE,  FALSE, FALSE, FALSE, FALSE, TRUE,
        FALSE, FALSE, FALSE, TRUE,  TRUE,  TRUE,  FALSE, FALSE,
        TRUE,  FALSE, TRUE,  FALSE, FALSE, FALSE, TRUE,  FALSE),
        7, 8, byrow=TRUE)
    rownames(availableLinks) <- families
    colnames(availableLinks) <- links
    canonicalLinks <- c("identity", "logit", "log", "inverse", "1/mu^2", "logit", "log")
    names(canonicalLinks) <- families
    initializeDialog(title=gettextRcmdr("Generalized Linear Model"))
    .activeModel <- ActiveModel()
    currentModel <- if (!is.null(.activeModel))
        class(get(.activeModel, envir=.GlobalEnv))[1] == "glm"
#        eval(parse(text=paste("class(", .activeModel, ")[1] == 'glm'", sep="")),
#            envir=.GlobalEnv)
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv), glm=TRUE)
#        currentFields <- formulaFields(eval(parse(text=.activeModel),
#            envir=.GlobalEnv), glm=TRUE)
        if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
        }
    modelFormula()
    UpdateModelNumber()
    modelName <- tclVar(paste("GLM.", getRcmdr("modelNumber"), sep=""))
    modelFrame <- tkframe(top)
    model <- ttkentry(modelFrame, width="20", textvariable=modelName)
    linkFamilyFrame <- tkframe(top)
    familyFrame <- tkframe(linkFamilyFrame)
    familyBox <- tklistbox(familyFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white")
    familyScroll <- ttkscrollbar(familyFrame,
        command=function(...) tkyview(familyBox, ...))
    tkconfigure(familyBox, yscrollcommand=function(...) tkset(familyScroll, ...))
    for (fam in families) tkinsert(familyBox, "end", fam)
    linkFrame <- tkframe(linkFamilyFrame)
    linkBox <- tklistbox(linkFrame, height="4", exportselection="FALSE",
        selectmode="single", background="white")
    subsetBox(model=TRUE)
    onFamilySelect <- function(){
        family <- families[as.numeric(tkcurselection(familyBox)) + 1]
        availLinks <- links[availableLinks[family,]]
        tkdelete(linkBox, "0", "end")
        for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
        canLink <- canonicalLinks[family]
        tkconfigure(linkBox, height=length(availLinks))
        tkselection.set(linkBox, which(canLink == availLinks) - 1)
        }
    onOK <- function(){
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=generalizedLinearModel, model=TRUE, message=gettextRcmdr("Left-hand side of model empty."))
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=generalizedLinearModel, model=TRUE, message=gettextRcmdr("Right-hand side of model empty."))
            return()
            }
        modelValue <- trim.blanks(tclvalue(modelName))
        if (!is.valid.name(modelValue)){
            errorCondition(recall=generalizedLinearModel, model=TRUE, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
            return()
            }
        if (is.element(modelValue, listGeneralizedLinearModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
                UpdateModelNumber(-1)
                closeDialog()
                generalizedLinearModel()
                return()
                }
            }
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        family <- families[as.numeric(tkcurselection(familyBox)) + 1]
        availLinks <- links[availableLinks[family,]]
        link <- availLinks[as.numeric(tkcurselection(linkBox)) + 1]
        subset <- tclvalue(subsetVariable)
        closeDialog()
        if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
            subset <- ""
            putRcmdr("modelWithSubset", FALSE)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            putRcmdr("modelWithSubset", TRUE)
            }
        command <- paste("glm(", formula, ", family=", family, "(", link,
            "), data=", ActiveDataSet(), subset, ")", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        activeModel(modelValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="generalizedLinearModel")
    helpButton <- buttonRcmdr(buttonsFrame, text="Help", width="12", command=onHelp)
    tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
        tkgrid(labelRcmdr(linkFamilyFrame, text=gettextRcmdr("Family (double-click to select)"), fg="blue"),
        labelRcmdr(linkFamilyFrame, text="   "), labelRcmdr(linkFamilyFrame, text=gettextRcmdr("Link function"), fg="blue"), sticky="w")
    tkgrid(familyBox, familyScroll, sticky="nw")
    tkgrid(linkBox, sticky="nw")
    tkgrid(familyFrame, labelRcmdr(linkFamilyFrame, text="   "), linkFrame, sticky="nw")
    tkgrid(linkFamilyFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    tkgrid.configure(familyScroll, sticky="ns")
    fam <- if (currentModel) which(currentFields$family == families) - 1
        else 1
    tkselection.set(familyBox, fam)
    availLinks <- links[availableLinks[fam + 1,]]
    for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
    tkconfigure(linkBox, height=length(availLinks))
    lnk <- if (currentModel) which(currentFields$link == availLinks) - 1
            else 0
    tkselection.set(linkBox, lnk)
    tkbind(familyBox, "<Double-ButtonPress-1>", onFamilySelect)
    dialogSuffix(rows=7, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
    }

ordinalRegressionModel <- function(){
    Library("MASS")
    initializeDialog(title=gettextRcmdr("Ordinal Regression Model"))
    .activeModel <- ActiveModel()
    .activeDataSet <- ActiveDataSet()
    currentModel <- if (!is.null(.activeModel))
        class(get(.activeModel, envir=.GlobalEnv))[1] == "polr"
#        eval(parse(text=paste("class(", .activeModel, ")[1] == 'polr'", sep="")),
#            envir=.GlobalEnv)
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
#        currentFields <- formulaFields(eval(parse(text=.activeModel),
#            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    UpdateModelNumber()
    modelName <- tclVar(paste("OrdRegModel.", getRcmdr("modelNumber"), sep=""))
    modelFrame <- tkframe(top)
    model <- ttkentry(modelFrame, width="20", textvariable=modelName)
    radioButtons(name="modelType",
        buttons=c("logistic", "probit"),
        labels=gettextRcmdr(c("Proportional-odds logit", "Ordered probit")),
        title=gettextRcmdr("Type of Model"))
    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()
        if (!is.valid.name(modelValue)){
            errorCondition(recall=proportionalOddsModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
            subset <- ""
            putRcmdr("modelWithSubset", FALSE)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            putRcmdr("modelWithSubset", TRUE)
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Left-hand side of model empty."), model=TRUE)
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
            return()
            }
        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=get(.activeDataSet, envir=.GlobalEnv)))){
#        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=eval(parse(text=.activeDataSet), envir=.GlobalEnv)))){
            errorCondition(recall=proportionalOddsModel, message=gettextRcmdr("Response variable must be a factor"))
            return()
            }
        if (is.element(modelValue, listProportionalOddsModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
                UpdateModelNumber(-1)
                proportionalOddsModel()
                return()
                }
            }
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        command <- paste("polr(", formula, ', method="', tclvalue(modelTypeVariable),
            '", data=', .activeDataSet, subset, ", Hess=TRUE)", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ")", sep=""))
        activeModel(modelValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="polr", model=TRUE)
    tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    modelFormula()
    subsetBox(model=TRUE)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(modelTypeFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=7, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
    }

multinomialLogitModel <- function(){
    Library("nnet")
    initializeDialog(title=gettextRcmdr("Multinomial Logit Model"))
    .activeModel <- ActiveModel()
    .activeDataSet <- ActiveDataSet()
    currentModel <- if (!is.null(.activeModel))
        class(get(.activeModel, envir=.GlobalEnv))[1] == "multinom"
#        eval(parse(text=paste("class(", .activeModel, ")[1] == 'multinom'", sep="")),
#            envir=.GlobalEnv)
        else FALSE
    if (currentModel) {
        currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
#        currentFields <- formulaFields(eval(parse(text=.activeModel),
#            envir=.GlobalEnv))
        if (currentFields$data != .activeDataSet) currentModel <- FALSE
        }
    UpdateModelNumber()
    modelName <- tclVar(paste("MLM.", getRcmdr("modelNumber"), sep=""))
    modelFrame <- tkframe(top)
    model <- ttkentry(modelFrame, width="20", textvariable=modelName)
    onOK <- function(){
        modelValue <- trim.blanks(tclvalue(modelName))
        closeDialog()
        if (!is.valid.name(modelValue)){
            errorCondition(recall=multinomialLogitModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
            return()
            }
        subset <- tclvalue(subsetVariable)
        if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
            subset <- ""
            putRcmdr("modelWithSubset", FALSE)
            }
        else{
            subset <- paste(", subset=", subset, sep="")
            putRcmdr("modelWithSubset", TRUE)
            }
        check.empty <- gsub(" ", "", tclvalue(lhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=multinomialLogitModel, message=gettextRcmdr("Left-hand side of model empty."), model=TRUE)
            return()
            }
        check.empty <- gsub(" ", "", tclvalue(rhsVariable))
        if ("" == check.empty) {
            errorCondition(recall=multinomialLogitModel, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
            return()
            }
        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=get(.activeDataSet, envir=.GlobalEnv)))){
#        if (!is.factor(eval(parse(text=tclvalue(lhsVariable)), envir=eval(parse(text=.activeDataSet), envir=.GlobalEnv)))){
            errorCondition(recall=multinomialLogitModel, message=gettextRcmdr("Response variable must be a factor"))
            return()
            }
        if (is.element(modelValue, listMultinomialLogitModels())) {
            if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
                UpdateModelNumber(-1)
                multinomialLogitModel()
                return()
                }
            }
        formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
        command <- paste("multinom(", formula,
            ", data=", .activeDataSet, subset, ", trace=FALSE)", sep="")
        logger(paste(modelValue, " <- ", command, sep=""))
        assign(modelValue, justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("summary(", modelValue, ", cor=FALSE, Wald=TRUE)", sep=""))
        activeModel(modelValue)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="multinom", model=TRUE)
    tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
    tkgrid(modelFrame, sticky="w")
    modelFormula()
    subsetBox(model=TRUE)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(outerOperatorsFrame, sticky="w")
    tkgrid(formulaFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=lhsEntry, preventDoubleClick=TRUE)
    }

formulaFields <- function(model, hasLhs=TRUE, glm=FALSE){
	formula <- as.character(model$call$formula)
	if (hasLhs){
		lhs <- formula[2]
		rhs <- formula[3]
	} else {
		lhs <- NULL
		rhs <- formula[2]
	}
	data <- as.character(model$call$data)
	which.subset <- which("subset" == names(model$call))
	subset <- if (0 == length(which.subset)) ""
		else as.character(model$call)[[which.subset]]
	if (glm) {
		fam <- as.character(model$call$family)
		family <- fam[1]
		link <- fam[2]
	}
	else {
		family <- NULL
		link <- NULL
	}
	list(lhs=lhs, rhs=rhs, data=data, subset=subset, family=family, link=link)
}