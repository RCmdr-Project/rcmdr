# Model menu dialogs

# last modified 25 June 2010 by J. Fox

selectActiveModel <- function(){
    models <- listAllModels()
    .activeModel <- ActiveModel()
    if ((length(models) == 1) && !is.null(.activeModel)) {
        Message(message=gettextRcmdr("There is only one model in memory."),
                type="warning")
        tkfocus(CommanderWindow())
        return()
        }
    if (length(models) == 0){
        Message(message=gettextRcmdr("There are no models from which to choose."),
                type="error")
        tkfocus(CommanderWindow())
        return()
        }
    initializeDialog(title=gettextRcmdr("Select Model"))
    .activeDataSet <- ActiveDataSet()
    initial <- if (is.null(.activeModel)) NULL else which(.activeModel == models) - 1
    modelsBox <- variableListBox(top, models, title=gettextRcmdr("Models (pick one)"), 
        initialSelection=initial)
    onOK <- function(){
        model <- getSelection(modelsBox)
        closeDialog()
        if (length(model) == 0) {
            tkfocus(CommanderWindow())
            return()
            }
        dataSet <- as.character(get(model)$call$data)
#        dataSet <- eval(parse(text=paste("as.character(", model, "$call$data)")))
        if (length(dataSet) == 0){
            errorCondition(message=gettextRcmdr("There is no dataset associated with this model."))
            return()
            }
        dataSets <- listDataSets()
        if (!is.element(dataSet, dataSets)){
            errorCondition(message=sprintf(gettextRcmdr("The dataset associated with this model, %s, is not in memory."), dataSet))
            return()
            }
        if (is.null(.activeDataSet) || (dataSet != .activeDataSet)) activeDataSet(dataSet)
        activeModel(model)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp()
    nameFrame <- tkframe(top)
    tkgrid(labelRcmdr(nameFrame, fg="blue", text=gettextRcmdr("Current Model: ")), 
        labelRcmdr(nameFrame, text=tclvalue(getRcmdr("modelName"))), sticky="w")
    tkgrid(nameFrame, sticky="w", columnspan="2")
    tkgrid(getFrame(modelsBox), columnspan="2", sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=3, columns=2)
    }

summarizeModel <- function(){
    .activeModel <- ActiveModel()
    if (is.null(.activeModel) || !checkMethod("summary", .activeModel)) return()
    doItAndPrint(paste("summary(", .activeModel, ", cor=FALSE)", sep=""))
    }

plotModel <- function(){
    .activeModel <- ActiveModel()
    if (is.null(.activeModel) || !checkMethod("plot", .activeModel)) return()
    command <- "oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))"
    justDoIt(command)
    logger(command)
    doItAndPrint(paste("plot(", .activeModel, ")", sep=""))
    command <- "par(oldpar)"
    justDoIt(command)
    logger(command)
    }

CRPlots <- function(){
    Library("car")
    .activeModel <- ActiveModel()
    if (is.null(.activeModel) || !checkMethod("crPlot", .activeModel)) return()
    doItAndPrint(paste("crPlots(", .activeModel, ")", sep=""))
    activateMenus()
    }

	AVPlots <- function(){
		Library("car")
		.activeModel <- ActiveModel()
		if (is.null(.activeModel) || !checkMethod("avPlot", .activeModel)) return()
		response <- tclvalue(RcmdrTkmessageBox(
						message=gettextRcmdr("Identify points with mouse?\nRight-click to exit."), 
						icon="question", type="yesno", default="no"))
		idmethod <- if (response == "yes") ', id.method="identify"' else ""
		doItAndPrint(paste("avPlots(", .activeModel, idmethod, ")", sep=""))
		activateMenus()
	}

anovaTable <- function(){
    Library("car")
    .activeModel <- ActiveModel()
    if (is.null(.activeModel)) return()
	initializeDialog(title=gettextRcmdr("ANOVA Table"))
	radioButtons(name="type",
			buttons=c("I", "II", "III"),
			values=c("I", "II", "III"), initialValue="II",
			labels=gettextRcmdr(c('Sequential ("Type I")', 'Partial, obeying marginality ("Type II")', 'Partial, ignoring marginality ("Type III")')),
			title=gettextRcmdr("Type of Tests"))
	onOK <- function(){
		type <- as.character(tclvalue(typeVariable))
		closeDialog()
		if (is.glm <- glmP()){
			family <- eval(parse(text=paste(.activeModel, "$family$family", sep="")))
		}
		if (type == "I"){
			if (!checkMethod("anova", .activeModel)) {
				errorCondition(message=gettextRcmdr("There is no appropriate anova method for a model of this class."))
				return()
				}
			if (is.glm){
				test <- if (family %in% c("binomial", "poisson")) "Chisq"
					else "F"
				doItAndPrint(paste("anova(", .activeModel, ', test="',  test, '")', sep=""))
			}
			else doItAndPrint(paste("anova(", .activeModel, ")", sep=""))
			}
		else {
			if (!checkMethod("Anova", .activeModel)) {
				errorCondition(message=gettextRcmdr("There is no appropriate Anova method for a model of this class."))
				return()
				}
			if (is.glm){
				test <- if (family %in% c("binomial", "poisson")) "LR"
					else "F"
				doItAndPrint(paste("Anova(", .activeModel, ', type="', type, '", test="', test, '")', sep=""))
			}
			else doItAndPrint(paste("Anova(", .activeModel, ', type="', type, '")', sep=""))
			if (type == "III") Message(message=gettextRcmdr("Type III tests require careful attention to contrast coding."),
				type="warning")
			}
		}
	OKCancelHelp(helpSubject="Anova")
	tkgrid(typeFrame, sticky="w")
	tkgrid(buttonsFrame, sticky="w")
	dialogSuffix(rows=2, columns=1)
    }

VIF <- function(){
    Library("car")
    .activeModel <- ActiveModel()
    if (is.null(.activeModel) || !checkMethod("vif", .activeModel)) return()
    doItAndPrint(paste("vif(", .activeModel, ")", sep=""))
    }
            
InfluencePlot <- function(){
	Library("car")
	.activeModel <- ActiveModel()
	if (is.null(.activeModel) || !checkMethod("influencePlot", .activeModel)) return()
	response <- tclvalue(RcmdrTkmessageBox(
					message=gettextRcmdr("Identify points with mouse?\nRight-click to exit."), 
					icon="question", type="yesno", default="no"))
	idmethod <- if (response == "yes") ', id.method="identify"' else ""
	doItAndPrint(paste("influencePlot(", .activeModel, idmethod, ")", sep=""))
}  
    
effectPlots <- function(){
    Library("effects")
    .activeModel <- ActiveModel()
    if (is.null(.activeModel) || !checkMethod("effect", .activeModel)) return()
    doItAndPrint('trellis.device(theme="col.whitebg")')
    command <- paste("plot(allEffects(", .activeModel, "), ask=FALSE)", sep="")
    justDoIt(command)
    logger(command)
    activateMenus()
    NULL
    }

addObservationStatistics <- function(){
    if (is.null(.activeModel)) return()
    addVariable <- function(name){
        variable <- paste(name, ".", .activeModel, sep="")
        if (is.element(variable, .variables)) {
            ans <- checkReplace(variable)
            if (tclvalue(ans) == "no") return()
            }
        command <- paste(name, "(", .activeModel, ")", sep="")
        justDoIt(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        logger(paste(.activeDataSet, "$", variable, " <- ", command, sep=""))
        }
    if (getRcmdr("modelWithSubset")){
        Message(message=
            gettextRcmdr("Observation statistics not available\nfor a model fit to a subset of the data."),
            type="error")
        tkfocus(.commander)
        return()
        }
    initializeDialog(title=gettextRcmdr("Add Observation Statistics to Data"))
    .activeModel <- ActiveModel()
    .activeDataSet <- ActiveDataSet()
    .variables <- Variables()
    obsNumberExists <- is.element("obsNumber", .variables)
    activate <- c(  checkMethod("fitted", .activeModel, default=TRUE, reportError=FALSE),
                    checkMethod("residuals", .activeModel, default=TRUE, reportError=FALSE),
                    checkMethod("rstudent", .activeModel, reportError=FALSE),
                    checkMethod("hatvalues", .activeModel, reportError=FALSE),
                    checkMethod("cooks.distance", .activeModel, reportError=FALSE))
    checkBoxes(frame="selectFrame", boxes=c(c("fitted", "residuals", "rstudent", "hatvalues", "cookd")[activate],
        "obsNumbers"),
        initialValues=c(rep(1, sum(activate)), if(obsNumberExists) "0" else "1"),
        labels=c(gettextRcmdr(c("Fitted values", "Residuals", "Studentized residuals", "Hat-values", "Cook's distances"))[activate],
        gettextRcmdr("Observation indices")))
    onOK <- function(){
        closeDialog()
        if (activate[1] && tclvalue(fittedVariable) == 1) addVariable("fitted")
        if (activate[2] && tclvalue(residualsVariable) == 1) addVariable("residuals")
        if (activate[3] && tclvalue(rstudentVariable) == 1) addVariable("rstudent")
        if (activate[4] && tclvalue(hatvaluesVariable) == 1) addVariable("hatvalues")
        if (activate[5] && tclvalue(cookdVariable) == 1) addVariable("cooks.distance")
        if (tclvalue(obsNumbersVariable) == 1){
            proceed <- if (obsNumberExists) tclvalue(checkReplace("obsNumber")) else "yes"
            if (proceed == "yes") {
                command <- paste(.activeDataSet, "$obsNumber <- 1:nrow(", .activeDataSet, ")", sep="")
                justDoIt(command)
                logger(command)
                }
            }
        activeDataSet(.activeDataSet, flushModel=FALSE)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="influence.measures")
    tkgrid(selectFrame, sticky="w")  
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=5, columns=1)
    }

residualQQPlot <- function(){
    Library("car")
    .activeModel <- ActiveModel()
    if (is.null(.activeModel) || !checkMethod("qqPlot", .activeModel)) return()
    initializeDialog(title=gettextRcmdr("Residual Quantile-Comparison Plot"))
    selectFrame <- tkframe(top)
    simulateVar <- tclVar("1")
    identifyVar <- tclVar("0")
    simulateCheckBox <- tkcheckbutton(selectFrame, variable=simulateVar)
    identifyCheckBox <- tkcheckbutton(selectFrame, variable=identifyVar)
    onOK <- function(){
        closeDialog()
        simulate <- tclvalue(simulateVar) == 1
        if (tclvalue(identifyVar) == 1){
			identify <- ', id.method="identify"'
			RcmdrTkmessageBox(title="Identify Points",
					message=gettextRcmdr("Use left mouse button to identify points,\nright button to exit."),
					icon="info", type="ok")
		}
		else identify <- ""
        command <- paste("qqPlot(", .activeModel, ", simulate=", simulate, identify,
            ")", sep="")
        doItAndPrint(command)
        activateMenus()
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qq.plot.lm")
    tkgrid(labelRcmdr(selectFrame, text=gettextRcmdr("Simulated confidence envelope")), simulateCheckBox, sticky="w")
    tkgrid(labelRcmdr(selectFrame, text=gettextRcmdr("Identify points with mouse")), identifyCheckBox, sticky="w")
    tkgrid(selectFrame, sticky="w")  
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2, columns=1)
    }

testLinearHypothesis <- function(){
    Library("car")
    .activeModel <- ActiveModel()
    if (is.null(.activeModel) || !checkMethod("linearHypothesis", .activeModel, default=TRUE)) return()
    env <- environment()
    initializeDialog(title=gettextRcmdr("Test Linear Hypothesis"))
    outerTableFrame <- tkframe(top)
    assign(".tableFrame", tkframe(outerTableFrame), envir=env)
    setUpTable <- function(...){
        tkdestroy(get(".tableFrame", envir=env))
        assign(".tableFrame", tkframe(outerTableFrame), envir=env)
        nrows <- as.numeric(tclvalue(rowsValue))
        col.names <- names(coef(get(.activeModel)))
#        col.names <- eval(parse(text=paste("names(coef(", .activeModel, "))")))
        col.names <- substring(paste(abbreviate(col.names, 12), "            "), 1, 12)
        make.col.names <- "labelRcmdr(.tableFrame, text='')"
        for (j in 1:ncols) {
            make.col.names <- paste(make.col.names, ", ", 
                "labelRcmdr(.tableFrame, text='", col.names[j], "')", sep="")
            }
        rhsText <- gettextRcmdr("Right-hand side")
        make.col.names <- paste(make.col.names, ", labelRcmdr(.tableFrame, text='          ')",
            ", labelRcmdr(.tableFrame, text='", rhsText, "')", sep="")
        eval(parse(text=paste("tkgrid(", make.col.names, ")", sep="")), envir=env)
        for (i in 1:nrows){   
            varname <- paste(".tab.", i, ".1", sep="") 
            rhs.name <- paste(".rhs.", i, sep="")
            assign(varname, tclVar("0") , envir=env)
            assign(rhs.name, tclVar("0"), envir=env)
            make.row <- paste("labelRcmdr(.tableFrame, text=", i, ")")
            make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=", 
                varname, ")", sep="")
            for (j in 2:ncols){
                varname <- paste(".tab.", i, ".", j, sep="")
                assign(varname, tclVar("0"), envir=env)
                make.row <- paste(make.row, ", ", "ttkentry(.tableFrame, width='5', textvariable=", 
                    varname, ")", sep="")
                }
            make.row <- paste(make.row, ", labelRcmdr(.tableFrame, text='     '),",
                "ttkentry(.tableFrame, width='5', textvariable=", rhs.name, ")", sep="")
            eval(parse(text=paste("tkgrid(", make.row, ")", sep="")), envir=env)
            }
        tkgrid(get(".tableFrame", envir=env), sticky="w")
        }
    ncols <- length(coef(get(.activeModel)))
#    ncols <- eval(parse(text=paste("length(coef(", .activeModel, "))")))
    rowsFrame <- tkframe(top)
    rowsValue <- tclVar("1")
    rowsSlider <- tkscale(rowsFrame, from=1, to=ncols, showvalue=FALSE, variable=rowsValue,
        resolution=1, orient="horizontal", command=setUpTable)
    rowsShow <- labelRcmdr(rowsFrame, textvariable=rowsValue, width=2, justify="right")
    onOK <- function(){
        nrows <- as.numeric(tclvalue(rowsValue))
        cell <- 0
        values <- rep(NA, nrows*ncols)
        rhs <- rep(NA, nrows)
        for (i in 1:nrows){
            rhs.name <- paste(".rhs.", i, sep="")
            rhs[i] <- as.numeric(eval(parse(text=paste("tclvalue(", rhs.name,")", sep=""))))
            for (j in 1:ncols){
                cell <- cell+1
                varname <- paste(".tab.", i, ".", j, sep="")
                values[cell] <- as.numeric(eval(parse(text=paste("tclvalue(", varname,")", sep=""))))
                }
            }
        values <- na.omit(values)
        closeDialog()
        if (length(values) != nrows*ncols){
            Message(message=sprintf(gettextRcmdr("Number of valid entries in hypothesis matrix(%d)\nnot equal to number of rows (%d) * number of columns (%d)."), 
                length(values), nrows, ncols), type="error")
            testLinearHypothesis()
            return()
            }
        if (qr(matrix(values, nrows, ncols, byrow=TRUE))$rank < nrows) {
            Message(message=gettextRcmdr("Hypothesis matrix is not of full row rank."),
                type="error")
            testLinearHypothesis()
            return()
            }            
        rhs <- na.omit(rhs)
        if (length(rhs) != nrows){
            errorCondition(recall=testLinearHypothesis, message=sprintf(gettextRcmdr("Number of valid entries in rhs vector (%d)\nis not equal to number of rows (%d)."), length(rhs), nrows))
            return()
            }
        command <- paste("matrix(c(", paste(values, collapse=","), "), ", nrows, ", ", ncols,
            ", byrow=TRUE)", sep="")
        assign(".Hypothesis", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".Hypothesis <- ", command, sep=""))
        command <- paste("c(", paste(rhs, collapse=","), ")", sep="")
        assign(".RHS", justDoIt(command), envir=.GlobalEnv)
        logger(paste(".RHS <- ", command, sep=""))
        command <- paste("linearHypothesis(", .activeModel, ", .Hypothesis, rhs=.RHS)", sep="")
        doItAndPrint(command)
        justDoIt("remove(.Hypothesis, .RHS, envir=.GlobalEnv)") 
        logger("remove(.Hypothesis, .RHS)")                                              
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="linearHypothesis")
    tkgrid(labelRcmdr(rowsFrame, text=gettextRcmdr("Number of Rows:")), rowsSlider, rowsShow, sticky="w")
    tkgrid(rowsFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Enter hypothesis matrix and right-hand side vector:"), fg="blue"), sticky="w")
    tkgrid(outerTableFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=""))
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)       
    } 

compareModels <- function(){
    models <- listAllModels()
    if (length(models) < 2){
        Message(message=gettextRcmdr("There are fewer than two models."),
                type="error")
        tkfocus(CommanderWindow())
        return()
        }
    initializeDialog(title=gettextRcmdr("Compare Models"))
    modelsBox1 <- variableListBox(top, models, title=gettextRcmdr("First model (pick one)"))
    modelsBox2 <- variableListBox(top, models, title=gettextRcmdr("Second model (pick one)"))
    onOK <- function(){
        model1 <- getSelection(modelsBox1)
        model2 <- getSelection(modelsBox2)
        closeDialog()
        if (length(model1) == 0 || length(model2) == 0) {
            errorCondition(recall=compareModels, message=gettextRcmdr("You must select two models."))
            return()
            }
        if (!checkMethod("anova", model1)) {
            return()
            }
        if (!class(get(model1, envir=.GlobalEnv))[1] == class(get(model2, envir=.GlobalEnv))[1]){
#        if (!eval(parse(text=paste("class(", model1, ")[1] == class(", model2, ")[1]",
#            sep="")), envir=.GlobalEnv)){
                Message(message=gettextRcmdr("Models are not of the same class."),
                    type="error")
                compareModels()
                return()
                }
		if (glmP()){
			family1 <- eval(parse(text=paste(model1, "$family$family", sep="")))
			family2 <- eval(parse(text=paste(model2, "$family$family", sep="")))
			if (family1 != family2){
				Message(message=gettextRcmdr("Models do not have the same family."),
					type="error")
				compareModels()
				return()
			}
			test <- if (family1 %in% c("binomial", "poisson")) "Chisq"
				else "F"
			doItAndPrint(paste("anova(", model1, ", ", model2, ', test="', test, '")', sep=""))
		}
        else doItAndPrint(paste("anova(", model1, ", ", model2, ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="anova")
    tkgrid(getFrame(modelsBox1), getFrame(modelsBox2), sticky="nw")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=2, columns=2)
    }
    
BreuschPaganTest <- function(){
    if (is.null(.activeModel)) return()
    Library("lmtest")
	currentModel <- FALSE
    initializeDialog(title=gettextRcmdr("Breusch-Pagan Test"))
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Score Test for Nonconstant Error Variance"), fg="blue"), sticky="w")
    optionsFrame <- tkframe(top)
    onOK <- function(){
        .activeModel <- ActiveModel()
        var <- tclvalue(varVariable)
        closeDialog()
        type <- if (var == "fitted") paste(", varformula = ~ fitted.values(",
                    .activeModel, ")", sep="") 
                else if (var == "predictors") ""
                else paste(", varformula = ~", tclvalue(rhsVariable), sep="")
        student <- if (tclvalue(studentVariable) == 1) "TRUE" else "FALSE"
        model.formula <- as.character(formula(get(.activeModel)))
#        model.formula <- as.character(eval(parse(text=paste("formula(", .activeModel, ")", sep=""))))
        model.formula <- paste(model.formula[2], "~", model.formula[3])
        command <- paste("bptest(", model.formula, type, ", studentize=", student,
            ", data=", ActiveDataSet(), ")", sep="")
        doItAndPrint(command)  
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="bptest")
    studentVariable <- tclVar("0")
    studentFrame <- tkframe(optionsFrame)
    studentCheckBox <- tkcheckbutton(studentFrame, variable=studentVariable)
    tkgrid(labelRcmdr(studentFrame, text=gettextRcmdr("Studentized test statistic"), justify="left"),
        studentCheckBox, sticky="w")
    tkgrid(studentFrame, sticky="w")
    radioButtons(optionsFrame, name="var", buttons=c("fitted", "predictors", "other"), 
        labels=gettextRcmdr(c("Fitted values", "Explanatory variables", "Other (specify)")), title=gettextRcmdr("Variance Formula"))
    tkgrid(varFrame, sticky="w")
    modelFormula(optionsFrame, hasLhs=FALSE)
    tkgrid(formulaFrame, sticky="w")
    tkgrid(outerOperatorsFrame)
    tkgrid(getFrame(xBox), sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

DurbinWatsonTest <- function(){
    if (is.null(.activeModel)) return()
    Library("lmtest")
    initializeDialog(title=gettextRcmdr("Durbin-Waton Test"))
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Test for First-Order Error Autocorrelation"), fg="blue"), sticky="w")
    onOK <- function(){
        altHypothesis <- tclvalue(altHypothesisVariable)
        closeDialog()
        model.formula <- as.character(formula(get(ActiveModel())))
#        model.formula <- as.character(eval(parse(text=paste("formula(", ActiveModel(), ")", sep=""))))
        model.formula <- paste(model.formula[2], "~", model.formula[3])
        command <- paste("dwtest(", model.formula, ', alternative="', altHypothesis,
             '", data=', ActiveDataSet(), ')', sep="")
        doItAndPrint(command)  
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dwtest")
    radioButtons(name="altHypothesis", buttons=c("greater", "notequal", "less"), values=c("greater", "two.sided", "less"),
        labels=c("rho >  0", "rho != 0", "rho <  0"), title=gettextRcmdr("Alternative Hypothesis"))
    tkgrid(altHypothesisFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }

RESETtest <- function(){
    if (is.null(.activeModel)) return()
    Library("lmtest")
    initializeDialog(title=gettextRcmdr("RESET Test"))
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Test for Nonlinearity"), fg="blue"), sticky="w")
    onOK <- function(){
        type <- tclvalue(typeVariable)
        square <- tclvalue(squareVariable)
        cube <- tclvalue(cubeVariable)
        closeDialog()
        model.formula <- as.character(formula(get(ActiveModel())))
#        model.formula <- as.character(eval(parse(text=paste("formula(", ActiveModel(), ")", sep=""))))
        model.formula <- paste(model.formula[2], "~", model.formula[3])
        if (square == "0" && cube == "0"){
            errorCondition(recall=RESETtest, message=gettextRcmdr("No powers are checked."))
            return()
            }
        powers <- if (square == "1" && cube == "1") "2:3"
            else if (square == "1" && cube == "0") "2"
            else if (square == "0" && cube == "1") "3"
        command <- paste("resettest(", model.formula, ", power=", powers,
            ', type="', type, '", data=', ActiveDataSet(), ')', sep="")
        doItAndPrint(command)  
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="reset")
    optionsFrame <- tkframe(top)
    squareVariable <- tclVar("1")
    squareCheckBox <- tkcheckbutton(optionsFrame, variable=squareVariable)
    cubeVariable <- tclVar("1")
    cubeCheckBox <- tkcheckbutton(optionsFrame, variable=cubeVariable)
    typeVariable <- tclVar("regressor")
    radioButtons(optionsFrame, name="type", buttons=c("regressor", "fitted", "princomp"),
        labels=gettextRcmdr(c("Explanatory variables", "Fitted values", "First principal component")),
        title=gettextRcmdr("Type of Test")) 
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Powers to Include"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("2 (squares)")), squareCheckBox, sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("3 (cubes)   ")), cubeCheckBox, sticky="w")
    tkgrid(typeFrame, sticky="w")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3, columns=1)
    }

OutlierTest <- function(){
    if (is.null(.activeModel)) return()
    Library("car")
    .activeModel <- ActiveModel()
    if (!checkMethod("outlierTest", .activeModel)) {
        errorCondition(gettextRcmdr("There is no appropriate outlierTest method for a model of this class."))
        return()
        }
    doItAndPrint(paste("outlierTest(", .activeModel, ")", sep=""))
    }
    
confidenceIntervals <- function(){
    if (is.null(.activeModel)) return()
    Library("MASS")
    initializeDialog(title=gettextRcmdr("Confidence Intervals"))
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Confidence Intervals for Individual Coefficients"), fg="blue"), sticky="w")
    onOK <- function(){
        level <- tclvalue(confidenceLevel)
        opts <- options(warn=-1)
        lev <- as.numeric(level)
        options(opts)
        closeDialog()
        if ((is.na(lev)) || (lev < 0) || (lev > 1)) {
            Message(gettextRcmdr("Confidence level must be a number between 0 and 1."))
            tkfocus(CommanderWindow())
            return()
            }
        command <- if (glm) paste("Confint(", .activeModel, ", level=", level,
            ', type="', tclvalue(typeVariable), '")', sep="")
            else paste("Confint(", .activeModel, ", level=", level, ")", sep="")
        doItAndPrint(command)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="Confint")
    confidenceFrame <- tkframe(top)
    confidenceLevel <- tclVar(".95")
    confidenceField <- ttkentry(confidenceFrame, width="6", textvariable=confidenceLevel)
    radioButtons(top, name="type", buttons=c("LR", "Wald"),
        labels=gettextRcmdr(c("Likelihood-ratio statistic", "Wald statistic")), title=gettextRcmdr("Test Based On"))
    tkgrid(labelRcmdr(confidenceFrame, text=gettextRcmdr("Confidence Level: ")), confidenceField, sticky="w")
    tkgrid(confidenceFrame, sticky="w")
    .activeModel <- ActiveModel()
    glm <- class(get(.activeModel))[1] == "glm"
#    glm <- eval(parse(text=paste("class(", .activeModel, ")")))[1] == "glm"
    if (glm) tkgrid(typeFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=3 + glm, columns=1)
    }
	
aic <- function(){
	.activeModel <- ActiveModel()
	if (is.null(.activeModel)) return()
	doItAndPrint(paste("AIC(", .activeModel, ")", sep=""))
	}
	
bic <- function(){
	.activeModel <- ActiveModel()
	if (is.null(.activeModel)) return()
	doItAndPrint(paste("AIC(", .activeModel, ", k = log(nobs(", .activeModel, "))) # BIC", sep=""))
}

stepwiseRegression <- function(){
	initializeDialog(title=gettextRcmdr("Stepwise Model Selection"))
	onOK <- function(){
		direction <- as.character(tclvalue(directionVariable))
		criterion <- as.character(tclvalue(criterionVariable))
		closeDialog()
		doItAndPrint(paste("stepwise(", ActiveModel(),
						", direction='", direction, "', criterion='", criterion,
						"')", sep=""))
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="stepwise")
	radioButtons(top, name="direction", buttons=c("bf", "fb", "b", "f"), 
			values=c("backward/forward", "forward/backward", "backward", "forward"),
			labels=gettextRcmdr(c("backward/forward", "forward/backward", "backward", "forward")),
			title=gettextRcmdr("Direction"))
	radioButtons(top, name="criterion", buttons=c("bic", "aic"), 
			values=c("BIC", "AIC"),
			labels=gettextRcmdr(c("BIC", "AIC")),
			title=gettextRcmdr("Criterion"))
	tkgrid(directionFrame, criterionFrame, sticky="nw")
	tkgrid(buttonsFrame, columnspan=2, sticky="w")
	dialogSuffix(rows=2, columns=2)
}

subsetRegression <- function(){
	Library("leaps")
	initializeDialog(title=gettextRcmdr("Subset Model Selection"))
	onOK <- function(){
		formula <- paste(sub("^[ ]*", "", deparse(formula(get(ActiveModel())))), collapse="")
		criterion <- as.character(tclvalue(criterionVariable))
		nbest <- as.numeric(tclvalue(nbestValue))
		nvmax <- as.numeric(tclvalue(nvmaxValue))
		really.big <- if (nvmax > 50) "TRUE" else "FALSE"
		closeDialog()
		doItAndPrint(paste("plot(regsubsets(", formula, ", data=", ActiveDataSet(),
						", nbest=", nbest, ", nvmax=", nvmax, "), scale='", criterion,
						"')", sep=""))
		tkdestroy(top)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject="regsubsets")
	radioButtons(top, name="criterion", buttons=c("bic", "Cp", "adjr2", "r2"), 
			labels=gettextRcmdr(c("BIC", "Mallows Cp", "Adjusted R-sq.", "R-squared")),
			title=gettextRcmdr("Criterion for Model Plot"))
	nvar <- ncol(model.matrix(get(ActiveModel())))
	nbestValue <- tclVar("1")
	nvmaxValue <- tclVar(as.character(min(25, nvar)))
	slidersFrame <- tkframe(top)
	nbestSlider <- tkscale(slidersFrame, from=1, to=10, showvalue=TRUE, variable=nbestValue,
			resolution=1, orient="horizontal")
	nvmaxSlider <- tkscale(slidersFrame, from=1, to=nvar, 
			showvalue=TRUE, variable=nvmaxValue, resolution=1, orient="horizontal")
	tkgrid(tklabel(slidersFrame, text="     "),
		tklabel(slidersFrame, text=gettextRcmdr("Number of best models\nof each size:"), fg="blue"), 
			nbestSlider, sticky="w")
	tkgrid(tklabel(slidersFrame, text="     "),
		tklabel(slidersFrame, text=gettextRcmdr("Maximum size:"), fg="blue"),nvmaxSlider, sticky="e")
	tkgrid(criterionFrame, slidersFrame, sticky="nw")
	tkgrid(buttonsFrame, columnspan=2, sticky="w")
	dialogSuffix(rows=2, columns=2)
}