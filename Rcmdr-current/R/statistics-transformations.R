# last modified 2018-07-27 by J. Fox

transformVariables <- function () {
  defaults <- list(initial.variables = NULL, initial.family="bcPower", initial.formula="")
  dialog.values <- getDialog("transformVariables", defaults)
  initializeDialog(title = gettextRcmdr("Transform Variables Toward Normality"), use.tabs=TRUE)
  
  variablesBox <- variableListBox(dataTab, Numeric(), title = gettextRcmdr("Select variables to transform (one or more)"),
                                  selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.variables, "numeric"))
  radioButtons(optionsTab, name = "family", 
               buttons = c("bcPower", "bcnPower", "yjPower"), 
               labels = gettextRcmdr(c("Box-Cox", "Box-Cox with negatives", "Yeo-Johnson")),
               title = gettextRcmdr("Transformation Family"),
               initialValue = dialog.values$initial.family)
  onOK <- function() {
    variables <- getSelection(variablesBox)
    family <- tclvalue(familyVariable)
    rhs <- trimws(tclvalue(rhsVariable))
    closeDialog()
    putDialog("transformVariables", list(initial.variables=variables, initial.family=family, initial.formula=rhs))
    if (rhs == "") rhs <- "1"
    .activeDataSet <- ActiveDataSet()
    if (length(variables) < 1){
      errorCondition(recall = transformVariables, message = gettextRcmdr("You must select one or more variables."))
      return()
    }
    vars <- if (length(variables) > 1) 
      paste0("cbind(", paste(variables, collapse=", "), ")") 
      else variables
    command <- paste0("summary(powerTransform(", vars, " ~ ", rhs, ", data=", 
                      .activeDataSet, ', family="', family, '"))')
    doItAndPrint(command)
    tkfocus(CommanderWindow())
    }
  OKCancelHelp(helpSubject = "powerTransform", reset = "transformVariables", apply = "transformVariables")
  tkgrid(getFrame(variablesBox), sticky = "nw")
  tkgrid(familyFrame, sticky = "w")
  currentModel <- TRUE
  currentFields <- list(rhs=dialog.values$initial.formula, lhs="", subset="")
  tkgrid(tklabel(optionsTab, text=gettextRcmdr("Condition on:"), fg=getRcmdr("title.color")), sticky="w")
  modelFormula(optionsTab, hasLhs = FALSE, rhsExtras=TRUE, formulaLabel="")
  tkgrid(getFrame(xBox), sticky = "w")
  tkgrid(outerOperatorsFrame)
  tkgrid(formulaFrame, sticky = "w")
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE)
}
