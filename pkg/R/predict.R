#' @title Input data to predict
#'
#' @description
#'
#' Show a data.frame editor to input new data and predict values using active model
#' 
#' @export
input2predict <- function() {
    ## To ensure that menu name is included in pot file
    gettext("Predict using active model", domain="R-Rcmdr")
    gettext("Input data and predict...", domain="R-Rcmdr")
    ## Build empty data.frame with the predictor variables and call editor
    variables <- all.vars(formula(get(ActiveModel())))[-1]
    justDoIt(paste0(".data <- edit(", ActiveDataSet(), "[0, c('", paste0(variables, collapse = "', '"), "'), drop = FALSE])"))
    ## Build and execute a command to recreate inputed data.frame
    command <- ".data <- data.frame("
    for(i in seq(1, length(variables))) {
        if (is.numeric(.data[, variables[i]])) {
            values <- paste0(.data[, variables[i]], collapse = ", ")
        } else {
            values <- paste0("\"", paste0(.data[, variables[i]], collapse = "\", \""), "\"")
        }
        command <- paste0(command, variables[i], " = c(", values, ")")
        if (i < length(variables)) command <- paste0(command, ", ")
    }
    command <- paste0(command, ")")
    doItAndPrint(command)
    ## Make the predict call
    doItAndPrint(paste0("predict(", ActiveModel(), ", .data)"))
}

### Function to predict values for existing data set
predict4dataset <- function() {
    ## To ensure that menu name is included in pot file
    gettext("Predict values for existing dataset...", domain="R-RcmdrPlugin.UCA")
    dataSets <- listDataSets()
    .activeDataSet <- ActiveDataSet()
    initializeDialog(title=gettextRcmdr("Select Data Set"))
    dataSetsBox <- variableListBox(top, dataSets, title=gettextRcmdr("Data Sets (pick one)"), initialSelection=if (is.null(.activeDataSet)) NULL else which(.activeDataSet == dataSets) - 1)
    onOK <- function(){
        selection <- getSelection(dataSetsBox)
        closeDialog()
        setBusyCursor()
        on.exit(setIdleCursor())
        doItAndPrint(paste0(selection, "$fitted.", ActiveModel(), " <- predict(", ActiveModel(), ", ", selection, ")"))
        if (selection != .activeDataSet) activeDataSet(selection)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp()
    tkgrid(getFrame(dataSetsBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix()
}

