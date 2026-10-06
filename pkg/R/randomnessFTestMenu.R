#' @name randomnessFTestMenu
#'
#' @title Randomness test for two level factor
#'
#' @author Manuel Munoz-Marquez <manuel.munoz@uca.es>
#'
#' @keywords htest nonparametric
#'
#' @description
#' 
#' This menu option performs a randomness test for two level factor calling \link[tseries]{runs.test} function.
#'
#' @details
#' This is an example of how to use option "Randomness test for two level factor..." of the menu.
#'
#' Load "Chile" data set selecting from Rcmdr menu: "Data" -> "Data in packages" -> "Read data set from an attached package..." then double-click on "carData", click on "Chile" and on "OK".
#'
#' Rcmdr reply with the following command in source pane (R Script)
#'
#' \code{data(Chile, package="carData")}
#'
#' To test the randomness of variable \code{sex}, select from Rcmdr menu: "Statistics" -> "Non parametric tests" -> "Randomness test for two level factor...".
#' After selecting \code{sex} variable click on "OK".
#' 
#' Rcmdr reply with the following command in source pane (R Script)
#'
#' \code{with(Chile, twolevelfactor.runs.test(sex))}
#'
#' And the result shown in the Output panel is
#'
#' \preformatted{
#' 	Runs Test
#' data:  sex
#' Standard Normal = 3.8755, p-value = 0.0001064
#' alternative hypothesis: two.sided
#' }
#'
#' @export
randomnessFTestMenu <- function() {
    ## To ensure that menu name is included in pot file
    gettext("Randomness test for two level factor...", domain="R-Rcmdr")
    ## Build dialog
    initializeDialog(title=gettext("Randomness test for two level factor", domain="R-Rcmdr"))
    variablesBox <- variableListBox(top, TwoLevelFactors(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(variablesBox)
        if (length(x) == 0) {
            errorCondition(recall=randomnessNTestMenu, message=gettextRcmdr("No variable were selected."))
            return()
        }
        closeDialog()
        ## Apply test
        doItAndPrint(paste("with(", ActiveDataSet(), ", twolevelfactor.runs.test(", x, "))", sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="randomnessFTestMenu", reset = "randomnessFTestMenu", apply = "randomnessFTestMenu")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
}

#' @name randomnessFTest
#'
#' @title Randomness test for two level factor
#'
#' @keywords internal
#'
#' @import tseries
#'
#' @export twolevelfactor.runs.test
twolevelfactor.runs.test <- tseries::runs.test
