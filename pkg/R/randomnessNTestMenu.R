#' @name randomnessNTestMenu
#'
#' @title Randomness test for numeric variable
#'
#' @author Manuel Munoz-Marquez <manuel.munoz@uca.es>
#'
#' @keywords htest nonparametric
#'
#' @description
#'
#' This menu option performs a randomness test for numeric variable calling \link[randtests]{runs.test} function.
#' 
#' @details
#' This is an example of how to use option "Randomness test for numeric variable..." of the menu.
#'
#' Load "Chile" data set selecting from Rcmdr menu: "Data" -> "Data in packages" -> "Read data set from an attached package..." then double-click on "carData", click on "Chile" and on "OK".
#'
#' Rcmdr reply with the following command in source pane (R Script)
#'
#' \code{data(Chile, package="carData")}
#'
#' To test the randomness of variable \code{sex}, select from Rcmdr menu: "Statistics" -> "Non parametric tests" -> actor...".
#' After selecting \code{age} variable click on "OK".
#' 
#' Rcmdr reply with the following command in source pane (R Script)
#'
#' \code{with(Chile, numeric.runs.test(age))}
#'
#' And the result shown in the Output panel is
#'
#' \preformatted{
#' 	Runs Test
#' data:  age
#' statistic = 8.3167, runs = 1514, n1 = 1338, n2 = 1266, n = 2604, p-value < 2.2e-16
#' alternative hypothesis: nonrandomness
#' }
#'
#' @export
randomnessNTestMenu <- function() {
    ## To ensure that menu name is included in pot file
    gettext("Randomness test for numeric variable...", domain="R-Rcmdr")
    ## Build dialog
    initializeDialog(title=gettext("Randomness test for numeric variable", domain="R-Rcmdr"))
    variablesBox <- variableListBox(top, Numeric(), selectmode="single", initialSelection=NULL, title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(variablesBox)
        if (length(x) == 0) {
            errorCondition(recall=randomnessNTestMenu, message=gettextRcmdr("No variables were selected."))
            return()
        }
        closeDialog()
        ## Apply test
        doItAndPrint(paste("with(", ActiveDataSet(), ", numeric.runs.test(", x, "))", sep = ""))
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="randomnessNTestMenu", reset = "randomnessNTestMenu", apply = "randomnessNTestMenu")
    tkgrid(getFrame(variablesBox), sticky="nw")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=6, columns=1)
}

#' @name randomnessNTest
#'
#' @title Randomness test for numeric variable
#'
#' @keywords internal
#'
#' @import randtests
#' 
#' @export numeric.runs.test
numeric.runs.test <- randtests::runs.test
