# Statistics Menu dialogs

# last modified 2022-06-12 by J. Fox

    # Models menu

linearRegressionModel <- function () {
  defaults <- list(initial.x = NULL, initial.y = NULL, 
                   initial.subset = gettextRcmdr("<all valid cases>"),
                   initial.delete.cases=gettextRcmdr("<use all valid cases>"))
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
  removeVariable <- tclVar(dialog.values$initial.delete.cases)
  removeFrame <- tkframe(variablesFrame)
  removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
  removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                               command=function(...) tkxview(removeEntry, ...))
  tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
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
    subset.save <- subset <- tclvalue(subsetVariable)
    remove <- trim.blanks(tclvalue(removeVariable))
    remove.cases <- if (remove == gettextRcmdr("<use all valid cases>") || remove == ""){
      ""
    } else {
      rowsToRemove <- removeRows <- paste("c(", gsub(" ", ",", remove), ")", sep="")
      remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
      if (inherits(remove.rows, "try-error")){
        UpdateModelNumber(-1)
        errorCondition(recall=linearRegressionModel,
                       message=remove.rows)
        return()
      }
      removeRows <- if (is.numeric(remove.rows)) paste("-", removeRows, sep="") 
      else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
      if (is.numeric(remove.rows)){
        n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
        if (any(which.bad <- !remove.rows %in% 1:n)){
          UpdateModelNumber(-1)
          errorCondition(recall=linearRegressionModel,
                         message=paste(gettextRcmdr("bad row numbers:"), 
                                       paste(as.character(remove.rows[which.bad]), collapse=", ")))
          return()
        }
      } else {
        if (any(which.bad <- eval(parse(text=paste("!", rowsToRemove, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
          UpdateModelNumber(-1)
          errorCondition(recall=linearRegressionModel,
                         message=paste(gettextRcmdr("bad row names:"), 
                                       paste(remove.rows[which.bad], collapse=", ")))
          return()          
        }
      }
      paste(", subset =", removeRows)
    }
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || 
        trim.blanks(subset) == "") {
      subset <- ""
    }
    else {
      subset <- paste(", subset=", subset, sep = "")
    }
    if (subset != "" && remove.cases != ""){
      UpdateModelNumber(-1)
      errorCondition(recall = linearRegressionModel, 
                     message = gettextRcmdr("You cannot specify both case removal and subset cases"))
      return()
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
                     ", data=", ActiveDataSet(), subset, remove.cases, ")", sep = "")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep = ""))
    activeModel(modelValue)
    if (subset != "" || remove.cases != "") putRcmdr("modelWithSubset", TRUE) else putRcmdr("modelWithSubset", FALSE)
    initial.delete.cases <- if (remove.cases == "") gettextRcmdr("<use all valid cases>") else remove 
    putDialog ("linearRegressionModel", list (initial.x = x, initial.y = y, 
                                              initial.subset = if (subset == "") gettextRcmdr("<all valid cases>") else subset.save,
                                              initial.delete.cases = initial.delete.cases))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "lm", model = TRUE, reset = "linearRegressionModel", apply = "linearRegressionModel")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model:")), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text = "    "), 
         getFrame(xBox), sticky = "nw")
  tkgrid(variablesFrame, sticky = "w")
  tkgrid(labelRcmdr(removeFrame, text=" "))
  tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or quoted names of row(s) to remove"),
                    foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(removeEntry, sticky="w")
  tkgrid(removeScroll, sticky="ew")
  tkgrid(removeFrame, sticky="nw", columnspan=3)
  tkgrid(subsetFrame, sticky = "w")
  tkgrid(buttonsFrame, stick = "w")
  tkgrid.configure(helpButton, sticky = "e")
  dialogSuffix()
}

linearModel <- function(){
  initializeDialog(title=gettextRcmdr("Linear Model"))
  defaults <- list(initial.weight = gettextRcmdr("<no variable selected>"),
                   initial.delete.cases=gettextRcmdr("<use all valid cases>"))
  dialog.values <- getDialog("linearModel", defaults)
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel))
    class(get(.activeModel, envir=.GlobalEnv))[1] == "lm"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  UpdateModelNumber()
  modelName <- tclVar(paste("LinearModel.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  removeVariable <- tclVar(dialog.values$initial.delete.cases)
  removeFrame <- tkframe(top)
  removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
  removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                               command=function(...) tkxview(removeEntry, ...))
  tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
  onOK <- function(){
    closeDialog()
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)){
      errorCondition(recall=linearModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      return()
    }
    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
      subset <- ""
    }
    else{
      subset <- paste(", subset=", subset, sep="")
    }
    weight.var <- getSelection(weightComboBox)
    weights <- if (weight.var == gettextRcmdr("<no variable selected>")) ""
    else paste(", weights=", weight.var, sep="")
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
    remove <- trim.blanks(tclvalue(removeVariable))
    remove.cases <- if (remove == gettextRcmdr("<use all valid cases>") || remove == ""){
      ""
    } else {
      rowsToRemove <- removeRows <- paste("c(", gsub(" ", ",", remove), ")", sep="")
      remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
      if (inherits(remove.rows, "try-error")){
        errorCondition(recall=linearModel,
                       message=remove.rows, model=TRUE)
        return()
      }
      removeRows <- if (is.numeric(remove.rows)) paste("-", removeRows, sep="") 
      else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
      if (is.numeric(remove.rows)){
        n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
        if (any(which.bad <- !remove.rows %in% 1:n)){
          errorCondition(recall=linearModel,
                         message=paste(gettextRcmdr("bad row numbers:"), 
                                       paste(as.character(remove.rows[which.bad]), collapse=", ")),
                         model=TRUE)
          return()
        }
      } else {
        if (any(which.bad <- eval(parse(text=paste("!", rowsToRemove, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
          errorCondition(recall=linearModel,
                         message=paste(gettextRcmdr("bad row names:"), 
                                       paste(remove.rows[which.bad], collapse=", ")), model=TRUE)
          return()          
        }
      }
      paste(", subset =", removeRows)
    }
    if (subset != "" && remove.cases != ""){
      errorCondition(recall = linearModel, 
                     message = gettextRcmdr("You cannot specify both case removal and subset cases"), 
                     model=TRUE)
      return()
    }
    formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
    command <- paste("lm(", formula,
                     ", data=", ActiveDataSet(), subset, weights, remove.cases, ")", sep="")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep=""))
    activeModel(modelValue)
    if (subset != "" || remove.cases != "") putRcmdr("modelWithSubset", TRUE) else putRcmdr("modelWithSubset", FALSE)
    initial.delete.cases <- gettextRcmdr("<use all valid cases>")
    putDialog("linearModel", list(initial.weight = weight.var, 
                                  initial.delete.cases = initial.delete.cases))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="linearModel", model=TRUE, reset="resetLinearModel", apply="linearModel")
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  modelFormula()
  subsetWeightFrame <- tkframe(top)
  subsetBox(window=subsetWeightFrame, model=TRUE)
  weightComboBox <- variableComboBox(subsetWeightFrame, variableList=Numeric(), 
                                     initialSelection=dialog.values$initial.weight,
                                     title=gettextRcmdr("Weights"))
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(outerOperatorsFrame, sticky="w")
  tkgrid(formulaFrame, sticky="w")
  tkgrid(labelRcmdr(removeFrame, text=" "))
  tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or quoted names of row(s) to remove"),
                    foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(removeEntry, sticky="w")
  tkgrid(removeScroll, sticky="ew")
  tkgrid(removeFrame, sticky="nw", columnspan=3)
  tkgrid(subsetFrame, tklabel(subsetWeightFrame, text="   "),
         getFrame(weightComboBox), sticky="nw")
  tkgrid(subsetWeightFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(focus=lhsEntry, preventDoubleClick=TRUE)
}

resetLinearModel <- function(){
	putRcmdr("reset.model", TRUE)
	putDialog("linearModel", NULL)
	putDialog("linearModel", NULL, resettable=FALSE)
	linearModel()
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
  defaults <- list(initial.weight = gettextRcmdr("<no variable selected>"),
                   initial.delete.cases=gettextRcmdr("<use all valid cases>"))
  dialog.values <- getDialog("generalizedLinearModel", defaults)
  initializeDialog(title=gettextRcmdr("Generalized Linear Model"))
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel))
    class(get(.activeModel, envir=.GlobalEnv))[1] == "glm"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv), glm=TRUE)
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  modelFormula()
  UpdateModelNumber()
  modelName <- tclVar(paste("GLM.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  linkFamilyFrame <- tkframe(top)
  familyFrame <- tkframe(linkFamilyFrame)
  max.height <- getRcmdr("variable.list.height")
  familyBox <- tklistbox(familyFrame, height=length(families), # height=min(max.height, length(families)), 
                         exportselection="FALSE",
                         selectmode="single", background="white")
  # familyScroll <- ttkscrollbar(familyFrame,
  #                              command=function(...) tkyview(familyBox, ...))
  # tkconfigure(familyBox, yscrollcommand=function(...) tkset(familyScroll, ...))
  for (fam in families) tkinsert(familyBox, "end", fam)
  linkFrame <- tkframe(linkFamilyFrame)
  linkBox <- tklistbox(linkFrame, height=max.height, exportselection="FALSE",
                       selectmode="single", background="white")
  subsetWeightFrame <- tkframe(top)
  subsetBox(window=subsetWeightFrame, model=TRUE)
  weightComboBox <- variableComboBox(subsetWeightFrame, variableList=Numeric(), 
                                     initialSelection=dialog.values$initial.weight,
                                     title=gettextRcmdr("Weights"))
  onFamilySelect <- function(){
    family <- families[as.numeric(tkcurselection(familyBox)) + 1]
    availLinks <- links[availableLinks[family,]]
    tkdelete(linkBox, "0", "end")
    for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
    canLink <- canonicalLinks[family]
    tkconfigure(linkBox, height=length(availLinks))
    tkselection.set(linkBox, which(canLink == availLinks) - 1)
  }
  removeVariable <- tclVar(dialog.values$initial.delete.cases)
  removeFrame <- tkframe(top)
  removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
  removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                               command=function(...) tkxview(removeEntry, ...))
  tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
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
    }
    else{
      subset <- paste(", subset=", subset, sep="")
    }
    weight.var <- getSelection(weightComboBox)
    weights <- if (weight.var == gettextRcmdr("<no variable selected>")) ""
    else paste(", weights=", weight.var, sep="")
    
    remove <- trim.blanks(tclvalue(removeVariable))
    remove.cases <- if (remove == gettextRcmdr("<use all valid cases>") || remove == ""){
      ""
    } else {
      rowsToRemove <- removeRows <- paste("c(", gsub(" ", ",", remove), ")", sep="")
      remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
      if (inherits(remove.rows, "try-error")){
        errorCondition(recall=generalizedLinearModel,
                       message=remove.rows, model=TRUE)
        return()
      }
      removeRows <- if (is.numeric(remove.rows)) paste("-", removeRows, sep="") 
      else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
      if (is.numeric(remove.rows)){
        n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
        if (any(which.bad <- !remove.rows %in% 1:n)){
          errorCondition(recall=generalizedLinearModel,
                         message=paste(gettextRcmdr("bad row numbers:"), 
                                       paste(as.character(remove.rows[which.bad]), collapse=", ")), model=TRUE)
          return()
        }
      } else {
        if (any(which.bad <- eval(parse(text=paste("!", rowsToRemove, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
          errorCondition(recall=generalizedLinearModel,
                         message=paste(gettextRcmdr("bad row names:"), 
                                       paste(remove.rows[which.bad], collapse=", ")), model=TRUE)
          return()          
        }
      }
      paste(", subset =", removeRows)
    }
    if (subset != "" && remove.cases != ""){
      errorCondition(recall = generalizedLinearModel, 
                     message = gettextRcmdr("You cannot specify both case removal and subset cases"), model=TRUE)
      return()
    }
    initial.delete.cases <- gettextRcmdr("<use all valid cases>")
    putDialog("generalizedLinearModel", list(initial.weight = weight.var, 
                                             initial.delete.cases = initial.delete.cases))
    
    command <- paste("glm(", formula, ", family=", family, "(", link, 
                     "), data=", ActiveDataSet(), subset, weights, remove.cases, ")", sep="")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep=""))
    activeModel(modelValue)
    if ((family == "binomial" || family =="quasibinomial") && link == "logit"){
      doItAndPrint(paste0("exp(coef(", modelValue,
                          '))  # Exponentiated coefficients ("odds ratios")'))
    }
    if ((family == "poisson" || family =="quasipoisson") && link == "log"){
      doItAndPrint(paste0("exp(coef(", modelValue,
                          '))  # Exponentiated coefficients'))
    }
    if (subset != "" || remove.cases != "") putRcmdr("modelWithSubset", TRUE) else putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="generalizedLinearModel", model=TRUE, reset="resetGLM", apply="generalizedLinearModel")
  helpButton <- buttonRcmdr(buttonsFrame, text="Help", width="12", command=onHelp)
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(outerOperatorsFrame, sticky="w")
  tkgrid(formulaFrame, sticky="w")
  tkgrid(subsetFrame, tklabel(subsetWeightFrame, text="   "),
         getFrame(weightComboBox), sticky="nw")
  tkgrid(labelRcmdr(removeFrame, text=" "))
  tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or quoted names of row(s) to remove"),
                    foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(removeEntry, sticky="w")
  tkgrid(removeScroll, sticky="ew")
  tkgrid(removeFrame, sticky="nw", columnspan=3)
  tkgrid(subsetWeightFrame, sticky="w")  
  tkgrid(labelRcmdr(linkFamilyFrame, text=gettextRcmdr("Family (double-click to select)"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"),
         labelRcmdr(linkFamilyFrame, text="   "), labelRcmdr(linkFamilyFrame, text=gettextRcmdr("Link function"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  #  tkgrid(familyBox, familyScroll, sticky="nw")
  tkgrid(familyBox, sticky="nw")
  tkgrid(linkBox, sticky="nw")
  tkgrid(familyFrame, labelRcmdr(linkFamilyFrame, text="   "), linkFrame, sticky="nw")
  tkgrid(linkFamilyFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  #  tkgrid.configure(familyScroll, sticky="ns")
  fam <- if (currentModel) which(currentFields$family == families) - 1 else 1
  tkselection.set(familyBox, fam)
  availLinks <- links[availableLinks[fam + 1,]]
  for (lnk in availLinks) tkinsert(linkBox, "end", lnk)
  tkconfigure(linkBox, height=length(availLinks))
  lnk <- if (currentModel) which(currentFields$link == availLinks) - 1 else 0
  tkselection.set(linkBox, lnk)
  tkbind(familyBox, "<Double-ButtonPress-1>", onFamilySelect)
  dialogSuffix(focus=lhsEntry, preventDoubleClick=TRUE)
}

resetGLM <- function(){
	putRcmdr("reset.model", TRUE)
	putDialog("generalizedLinearModel", NULL)
	putDialog("generalizedLinearModel", NULL, resettable=FALSE)
	generalizedLinearModel()
}

ordinalRegressionModel <- function(){
  defaults <- list(initial.type="logistic",
                   initial.delete.cases=gettextRcmdr("<use all valid cases>"))
  dialog.values <- getDialog("ordinalRegressionModel", defaults)
  Library("MASS")
  initializeDialog(title=gettextRcmdr("Ordinal Regression Model"))
  .activeModel <- ActiveModel()
  .activeDataSet <- ActiveDataSet()
  currentModel <- if (!is.null(.activeModel))
    class(get(.activeModel, envir=.GlobalEnv))[1] == "polr"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
    if (currentFields$data != .activeDataSet) currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  UpdateModelNumber()
  modelName <- tclVar(paste("OrdRegModel.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  radioButtons(name="modelType",
               buttons=c("logistic", "probit"), initialValue=dialog.values$initial.type,
               labels=gettextRcmdr(c("Proportional-odds logit", "Ordered probit")),
               title=gettextRcmdr("Type of Model"))
  removeVariable <- tclVar(dialog.values$initial.delete.cases)
  removeFrame <- tkframe(top)
  removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
  removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                               command=function(...) tkxview(removeEntry, ...))
  tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
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
    }
    else{
      subset <- paste(", subset=", subset, sep="")
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
    remove <- trim.blanks(tclvalue(removeVariable))
    remove.cases <- if (remove == gettextRcmdr("<use all valid cases>") || remove == ""){
      ""
    } else {
      rowsToRemove <- removeRows <- paste("c(", gsub(" ", ",", remove), ")", sep="")
      remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
      if (inherits(remove.rows, "try-error")){
        errorCondition(recall=ordinalRegressionModel,
                       message=remove.rows, model=TRUE)
        return()
      }
      removeRows <- if (is.numeric(remove.rows)) paste("-", removeRows, sep="") 
      else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
      if (is.numeric(remove.rows)){
        n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
        if (any(which.bad <- !remove.rows %in% 1:n)){
          errorCondition(recall=ordinalRegressionModel,
                         message=paste(gettextRcmdr("bad row numbers:"), 
                                       paste(as.character(remove.rows[which.bad]), collapse=", ")), model=TRUE)
          return()
        }
      } else {
        if (any(which.bad <- eval(parse(text=paste("!", rowsToRemove, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
          errorCondition(recall=ordinalRegressionModel,
                         message=paste(gettextRcmdr("bad row names:"), 
                                       paste(remove.rows[which.bad], collapse=", ")), model=TRUE)
          return()          
        }
      }
      paste(", subset =", removeRows)
    }
    if (subset != "" && remove.cases != ""){
      errorCondition(recall = ordinalRegressionModel, 
                     message = gettextRcmdr("You cannot specify both case removal and subset cases"), model=TRUE)
      return()
    }
    initial.delete.cases <- gettextRcmdr("<use all valid cases>")
    putDialog("ordinalRegressionModel", list(initial.type = tclvalue(modelTypeVariable),
                                             initial.delete.cases = initial.delete.cases))
    formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
    command <- paste("polr(", formula, ', method="', tclvalue(modelTypeVariable),
                     '", data=', .activeDataSet, subset, remove.cases, ", Hess=TRUE)", sep="")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep=""))
    activeModel(modelValue)
    if (subset != "" || remove.cases != "") putRcmdr("modelWithSubset", TRUE) else putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="polr", model=TRUE, reset = "resetPOLR", apply = "ordinalRegressionModel")
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  modelFormula()
  subsetBox(model=TRUE)
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(outerOperatorsFrame, sticky="w")
  tkgrid(formulaFrame, sticky="w")
  tkgrid(labelRcmdr(removeFrame, text=" "))
  tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or quoted names of row(s) to remove"),
                    foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(removeEntry, sticky="w")
  tkgrid(removeScroll, sticky="ew")
  tkgrid(removeFrame, sticky="nw", columnspan=3)
  tkgrid(subsetFrame, sticky="w")
  tkgrid(modelTypeFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(focus=lhsEntry, preventDoubleClick=TRUE)
}

resetPOLR <- function(){
	putRcmdr("reset.model", TRUE)
	putDialog("ordinalRegressionModel", NULL)
	ordinalRegressionModel()
}

multinomialLogitModel <- function(){
  Library("nnet")
  dialog.values <- getDialog("multinomialLogitModel", 
                             list(initial.delete.cases=gettextRcmdr("<use all valid cases>")))
  initializeDialog(title=gettextRcmdr("Multinomial Logit Model"))
  .activeModel <- ActiveModel()
  .activeDataSet <- ActiveDataSet()
  currentModel <- if (!is.null(.activeModel))
    class(get(.activeModel, envir=.GlobalEnv))[1] == "multinom"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
    if (currentFields$data != .activeDataSet) currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  UpdateModelNumber()
  modelName <- tclVar(paste("MLM.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  removeVariable <- tclVar(dialog.values$initial.delete.cases)
  removeFrame <- tkframe(top)
  removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
  removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                               command=function(...) tkxview(removeEntry, ...))
  tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
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
    }
    else{
      subset <- paste(", subset=", subset, sep="")
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
    remove <- trim.blanks(tclvalue(removeVariable))
    remove.cases <- if (remove == gettextRcmdr("<use all valid cases>") || remove == ""){
      ""
    } else {
      rowsToRemove <- removeRows <- paste("c(", gsub(" ", ",", remove), ")", sep="")
      remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
      if (inherits(remove.rows, "try-error")){
        errorCondition(recall=multinomialLogitModel,
                       message=remove.rows, model=TRUE)
        return()
      }
      removeRows <- if (is.numeric(remove.rows)) paste("-", removeRows, sep="") 
      else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
      if (is.numeric(remove.rows)){
        n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
        if (any(which.bad <- !remove.rows %in% 1:n)){
          errorCondition(recall=multinomialLogitModel,
                         message=paste(gettextRcmdr("bad row numbers:"), 
                                       paste(as.character(remove.rows[which.bad]), collapse=", ")), model=TRUE)
          return()
        }
      } else {
        if (any(which.bad <- eval(parse(text=paste("!", rowsToRemove, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
          errorCondition(recall=multinomialLogitModel,
                         message=paste(gettextRcmdr("bad row names:"), 
                                       paste(remove.rows[which.bad], collapse=", ")), model=TRUE)
          return()          
        }
      }
      paste(", subset =", removeRows)
    }
    if (subset != "" && remove.cases != ""){
      errorCondition(recall = multinomialLogitModel, 
                     message = gettextRcmdr("You cannot specify both case removal and subset cases"), model=TRUE)
      return()
    }
    initial.delete.cases <- gettextRcmdr("<use all valid cases>")
    putDialog("multinomialLogitModel", list(initial.delete.cases = initial.delete.cases))
    formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
    command <- paste("multinom(", formula,
                     ", data=", .activeDataSet, subset, remove.cases, ", trace=FALSE)", sep="")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ", cor=FALSE, Wald=TRUE)", sep=""))
    activeModel(modelValue)
    if (subset != "" || remove.cases != "") putRcmdr("modelWithSubset", TRUE) else putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="multinom", model=TRUE, reset="resetMNL", apply="multinomialLogitModel")
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  modelFormula()
  subsetBox(model=TRUE)
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(outerOperatorsFrame, sticky="w")
  tkgrid(formulaFrame, sticky="w")
  tkgrid(labelRcmdr(removeFrame, text=" "))
  tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or quoted names of row(s) to remove"),
                    foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(removeEntry, sticky="w")
  tkgrid(removeScroll, sticky="ew")
  tkgrid(removeFrame, sticky="nw", columnspan=3)
  tkgrid(subsetFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(focus=lhsEntry, preventDoubleClick=TRUE)
}

resetMNL <- function(){
	putRcmdr("reset.model", TRUE)
	multinomialLogitModel()
}

formulaFields <- function(model, hasLhs=TRUE, glm=FALSE){
	formula <- as.character(formula(model)) # as.character(model$call$formula)
	if (hasLhs){
		lhs <- formula[2]
		rhs <- formula[3]
	} else {
		lhs <- NULL
		rhs <- formula[2]
	}
	data <- as.character(getCall(model)$data) # as.character(model$call$data)
	which.subset <- which("subset" == names(getCall(model))) # which("subset" == names(model$call))
	subset <- if (0 == length(which.subset)) ""
		else as.character(getCall(model))[[which.subset]] # as.character(model$call)[[which.subset]]
	if (glm) {
		fam <- as.character(getCall(model)$family) # as.character(model$call$family)
		family <- fam[1]
		link <- fam[2]
	}
	else {
		family <- NULL
		link <- NULL
	}
	list(lhs=lhs, rhs=rhs, data=data, subset=subset, family=family, link=link)
}

linearMixedModel <- function(){
  Library("lme4")
  initializeDialog(title=gettextRcmdr("Linear Mixed Model"))
  defaults <- list(initial.weight = gettextRcmdr("<no variable selected>"), initial.estimType="reml",
                   initial.delete.cases=gettextRcmdr("<use all valid cases>"))
  dialog.values <- getDialog("linearMixedModel", defaults)
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel))
    class(get(.activeModel, envir=.GlobalEnv))[1] == "lmerMod"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv))
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  UpdateModelNumber()
  modelName <- tclVar(paste("LMM.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  radioButtons(name="estimType",
               buttons=c("reml", "ml"), initialValue=dialog.values$initial.estimType,
               labels=gettextRcmdr(c("Restricted maximum likelihood (REML)", "Maximum likelihood (ML)")),
               title=gettextRcmdr("Estimation Criterion"))
  removeVariable <- tclVar(dialog.values$initial.delete.cases)
  removeFrame <- tkframe(top)
  removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
  removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                               command=function(...) tkxview(removeEntry, ...))
  tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
  onOK <- function(){
    modelValue <- trim.blanks(tclvalue(modelName))
    closeDialog()
    if (!is.valid.name(modelValue)){
      errorCondition(recall=linearMixedModel, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue), model=TRUE)
      return()
    }
    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
      subset <- ""
    }
    else{
      subset <- paste(", subset=", subset, sep="")
    }
    weight.var <- getSelection(weightComboBox)
    estimType <- tclvalue(estimTypeVariable)
    weights <- if (weight.var == gettextRcmdr("<no variable selected>")) ""
    else paste(", weights=", weight.var, sep="")
    check.empty <- gsub(" ", "", tclvalue(lhsVariable))
    if ("" == check.empty) {
      errorCondition(recall=linearMixedModel, message=gettextRcmdr("Left-hand side of model empty."), model=TRUE)
      return()
    }
    check.empty <- gsub(" ", "", tclvalue(rhsVariable))
    if ("" == check.empty) {
      errorCondition(recall=linearMixedModel, message=gettextRcmdr("Right-hand side of model empty."), model=TRUE)
      return()
    }
    if (!grepl("\\(.*\\|.*\\)", tclvalue(rhsVariable))) {
      errorCondition(recall=linearMixedModel, message=gettextRcmdr("There are no random effects in the model."), model=TRUE)
      return()
    }
    if (is.element(modelValue, listLMMs())) {
      if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
        UpdateModelNumber(-1)
        linearMixedModel()
        return()
      }
    }
    remove <- trim.blanks(tclvalue(removeVariable))
    remove.cases <- if (remove == gettextRcmdr("<use all valid cases>") || remove == ""){
      ""
    } else {
      rowsToRemove <- removeRows <- paste("c(", gsub(" ", ",", remove), ")", sep="")
      remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
      if (inherits(remove.rows, "try-error")){
        errorCondition(recall=linearMixedModel,
                       message=remove.rows, model=TRUE)
        return()
      }
      removeRows <- if (is.numeric(remove.rows)) paste("-", removeRows, sep="") 
      else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
      if (is.numeric(remove.rows)){
        n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
        if (any(which.bad <- !remove.rows %in% 1:n)){
          errorCondition(recall=linearMixedModel,
                         message=paste(gettextRcmdr("bad row numbers:"), 
                                       paste(as.character(remove.rows[which.bad]), collapse=", ")), model=TRUE)
          return()
        }
      } else {
        if (any(which.bad <- eval(parse(text=paste("!", rowsToRemove, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
          errorCondition(recall=linearMixedModel,
                         message=paste(gettextRcmdr("bad row names:"), 
                                       paste(remove.rows[which.bad], collapse=", ")), model=TRUE)
          return()          
        }
      }
      paste(", subset =", removeRows)
    }
    if (subset != "" && remove.cases != ""){
      errorCondition(recall = linearMixedModel, 
                     message = gettextRcmdr("You cannot specify both case removal and subset cases"), model=TRUE)
      return()
    }
    initial.delete.cases <- gettextRcmdr("<use all valid cases>")
    putDialog("linearMixedModel", list(initial.weight = weight.var, initial.estimType=estimType,
                                       initial.delete.cases = initial.delete.cases))
    
    formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
    reml <- as.character(estimType == "reml")
    command <- paste("lmer(", formula,
                     ", data=", ActiveDataSet(), subset, remove.cases, weights, ", REML=", reml, ")", sep="")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep=""))
    activeModel(modelValue)
    if (subset != "" || remove.cases != "") putRcmdr("modelWithSubset", TRUE) else putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="lmer", model=TRUE, reset="resetLMM", apply="linearMixedModel")
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  modelFormula(showBar=TRUE)
  subsetWeightFrame <- tkframe(top)
  subsetBox(window=subsetWeightFrame, model=TRUE)
  weightComboBox <- variableComboBox(subsetWeightFrame, variableList=Numeric(), 
                                     initialSelection=dialog.values$initial.weight,
                                     title=gettextRcmdr("Weights"))
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(outerOperatorsFrame, sticky="w")
  tkgrid(formulaFrame, sticky="w")
  tkgrid(subsetFrame, tklabel(subsetWeightFrame, text="   "),
         getFrame(weightComboBox), sticky="nw")
  tkgrid(labelRcmdr(removeFrame, text=" "))
  tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or quoted names of row(s) to remove"),
                    foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(removeEntry, sticky="w")
  tkgrid(removeScroll, sticky="ew")
  tkgrid(removeFrame, sticky="nw", columnspan=3)
  tkgrid(subsetWeightFrame, sticky="w")	
  tkgrid(estimTypeFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
  dialogSuffix(focus=lhsEntry, preventDoubleClick=TRUE)
}

resetLMM <- function(){
  putRcmdr("reset.model", TRUE)
  putDialog("linearMixedModel", NULL)
  putDialog("linearMixedModel", NULL, resettable=FALSE)
  linearMixedModel()
}

generalizedLinearMixedModel <- function(){
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
  defaults <- list(initial.weight = gettextRcmdr("<no variable selected>"),
                   initial.delete.cases=gettextRcmdr("<use all valid cases>"))
  dialog.values <- getDialog("generalizedLinearMixedModel", defaults)
  initializeDialog(title=gettextRcmdr("Generalized Linear Mixed Model"))
  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel))
    class(get(.activeModel, envir=.GlobalEnv))[1] == "glmerMod"
  else FALSE
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir=.GlobalEnv), glm=TRUE)
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }
  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }
  modelFormula(showBar=TRUE)
  UpdateModelNumber()
  modelName <- tclVar(paste("GLMM.", getRcmdr("modelNumber"), sep=""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width="20", textvariable=modelName)
  linkFamilyFrame <- tkframe(top)
  familyFrame <- tkframe(linkFamilyFrame)
  max.height <- getRcmdr("variable.list.height")
  familyBox <- tklistbox(familyFrame, height=length(families), 
                         exportselection="FALSE",
                         selectmode="single", background="white")
  for (fam in families) tkinsert(familyBox, "end", fam)
  linkFrame <- tkframe(linkFamilyFrame)
  linkBox <- tklistbox(linkFrame, height=max.height, exportselection="FALSE",
                       selectmode="single", background="white")
  subsetWeightFrame <- tkframe(top)
  subsetBox(window=subsetWeightFrame, model=TRUE)
  weightComboBox <- variableComboBox(subsetWeightFrame, variableList=Numeric(), 
                                     initialSelection=dialog.values$initial.weight,
                                     title=gettextRcmdr("Weights"))
  removeVariable <- tclVar(dialog.values$initial.delete.cases)
  removeFrame <- tkframe(top)
  removeEntry <- ttkentry(removeFrame, width="60", textvariable=removeVariable)
  removeScroll <- ttkscrollbar(removeFrame, orient="horizontal",
                               command=function(...) tkxview(removeEntry, ...))
  tkconfigure(removeEntry, xscrollcommand=function(...) tkset(removeScroll, ...))
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
      errorCondition(recall=generalizedLinearMixedModel, model=TRUE, message=gettextRcmdr("Left-hand side of model empty."))
      return()
    }
    check.empty <- gsub(" ", "", tclvalue(rhsVariable))
    if ("" == check.empty) {
      errorCondition(recall=generalizedLinearMixedModel, model=TRUE, message=gettextRcmdr("Right-hand side of model empty."))
      return()
    }
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)){
      errorCondition(recall=generalizedLinearMixedModel, model=TRUE, message=sprintf(gettextRcmdr('"%s" is not a valid name.'), modelValue))
      return()
    }
    if (!grepl("\\(.*\\|.*\\)", tclvalue(rhsVariable))) {
      errorCondition(recall=generalizedLinearMixedModel, message=gettextRcmdr("There are no random effects in the model."), model=TRUE)
      return()
    }
    if (is.element(modelValue, listGLMMs())) {
      if ("no" == tclvalue(checkReplace(modelValue, type=gettextRcmdr("Model")))){
        UpdateModelNumber(-1)
        closeDialog()
        generalizedLinearMixedModel()
        return()
      }
    }
    remove <- trim.blanks(tclvalue(removeVariable))
    remove.cases <- if (remove == gettextRcmdr("<use all valid cases>") || remove == ""){
      ""
    } else {
      rowsToRemove <- removeRows <- paste("c(", gsub(" ", ",", remove), ")", sep="")
      remove.rows <- try(eval(parse(text=removeRows)), silent=TRUE)
      if (inherits(remove.rows, "try-error")){
        errorCondition(recall=generalizedLinearMixedModel,
                       message=remove.rows, model=TRUE)
        return()
      }
      removeRows <- if (is.numeric(remove.rows)) paste("-", removeRows, sep="") 
      else paste("!(rownames(", ActiveDataSet(), ") %in% ", removeRows, ")", sep="")
      if (is.numeric(remove.rows)){
        n <- eval(parse(text=paste0("nrow(", ActiveDataSet(), ")")))
        if (any(which.bad <- !remove.rows %in% 1:n)){
          errorCondition(recall=generalizedLinearMixedModel,
                         message=paste(gettextRcmdr("bad row numbers:"), 
                                       paste(as.character(remove.rows[which.bad]), collapse=", ")), model=TRUE)
          return()
        }
      } else {
        if (any(which.bad <- eval(parse(text=paste("!", rowsToRemove, "%in% rownames(", ActiveDataSet(), ")", sep=""))))){
          errorCondition(recall=generalizedLinearMixedModel,
                         message=paste(gettextRcmdr("bad row names:"), 
                                       paste(remove.rows[which.bad], collapse=", ")), model=TRUE)
          return()          
        }
      }
      paste(", subset =", removeRows)
    }
    initial.delete.cases <- gettextRcmdr("<use all valid cases>")
    formula <- paste(tclvalue(lhsVariable), tclvalue(rhsVariable), sep=" ~ ")
    family <- families[as.numeric(tkcurselection(familyBox)) + 1]
    availLinks <- links[availableLinks[family,]]
    link <- availLinks[as.numeric(tkcurselection(linkBox)) + 1]
    subset <- tclvalue(subsetVariable)
    closeDialog()
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") || trim.blanks(subset) == ""){
      subset <- ""
    }
    else{
      subset <- paste(", subset=", subset, sep="")
    }
    if (subset != "" && remove.cases != ""){
      errorCondition(recall = generalizedLinearMixedModel, 
                     message = gettextRcmdr("You cannot specify both case removal and subset cases"), model=TRUE)
      return()
    }
    weight.var <- getSelection(weightComboBox)
    putDialog("generalizedLinearMixedModel", list(initial.weight = weight.var, 
                                                  initial.delete.cases = initial.delete.cases))
    weights <- if (weight.var == gettextRcmdr("<no variable selected>")) ""
    else paste(", weights=", weight.var, sep="")
    command <- paste("glmer(", formula, ", family=", family, "(", link,
                     "), data=", ActiveDataSet(), subset, remove.cases, weights, ")", sep="")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep=""))
    activeModel(modelValue)
    if ((family == "binomial" || family =="quasibinomial") && link == "logit"){
      doItAndPrint(paste0("exp(coef(", modelValue,
                          '))  # Exponentiated coefficients ("odds ratios")'))
    }
    if ((family == "poisson" || family =="quasipoisson") && link == "log"){
      doItAndPrint(paste0("exp(coef(", modelValue,
                          '))  # Exponentiated coefficients'))
    }
    if (subset != "" || remove.cases != "") putRcmdr("modelWithSubset", TRUE) else putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject="glmer", model=TRUE, reset="resetGLMM", apply="generalizedLinearMixedModel")
  helpButton <- buttonRcmdr(buttonsFrame, text="Help", width="12", command=onHelp)
  tkgrid(labelRcmdr(modelFrame, text=gettextRcmdr("Enter name for model:")), model, sticky="w")
  tkgrid(modelFrame, sticky="w")
  tkgrid(getFrame(xBox), sticky="w")
  tkgrid(outerOperatorsFrame, sticky="w")
  tkgrid(formulaFrame, sticky="w")
  tkgrid(subsetFrame, tklabel(subsetWeightFrame, text="   "),
         getFrame(weightComboBox), sticky="nw")
  tkgrid(labelRcmdr(removeFrame, text=" "))
  tkgrid(labelRcmdr(removeFrame, text=gettextRcmdr("Indices or quoted names of row(s) to remove"),
                    foreground=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(removeEntry, sticky="w")
  tkgrid(removeScroll, sticky="ew")
  tkgrid(removeFrame, sticky="nw", columnspan=3)
  tkgrid(subsetWeightFrame, sticky="w")  
  tkgrid(labelRcmdr(linkFamilyFrame, text=gettextRcmdr("Family (double-click to select)"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"),
         labelRcmdr(linkFamilyFrame, text="   "), labelRcmdr(linkFamilyFrame, text=gettextRcmdr("Link function"), fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
  tkgrid(familyBox, sticky="nw")
  tkgrid(linkBox, sticky="nw")
  tkgrid(familyFrame, labelRcmdr(linkFamilyFrame, text="   "), linkFrame, sticky="nw")
  tkgrid(linkFamilyFrame, sticky="w")
  tkgrid(buttonsFrame, sticky="w")
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
  dialogSuffix(focus=lhsEntry, preventDoubleClick=TRUE)
}

resetGLMM <- function(){
  putRcmdr("reset.model", TRUE)
  putDialog("generalizedLinearMixedModel", NULL)
  putDialog("generalizedLinearMixedModel", NULL, resettable=FALSE)
  generalizedLinearMixedModel()
}
