## Splitted from Model menu dialog file: model-menu.R
#' Stepwise Model Selection
#'
#' @keywords models
#'
#' @details
#' This menu option performs stepwise model selection using either BIC or AIC for active model.
#'
#' The selection direction determines whether the process starts with the full set of variables and removes them (backward step) or starts with an empty model and adds variables to it (forward step).
#' If \code{"backward/forward"} or \code{"forward/backward"} is selected, the backward and forward steps are alternated.
#' 
#' The selection criterion can be \code{"BIC"} (Bayesian Information Criterion) or \code{"AIC"} (Akaike Information Criterion).
#' 
#' Note that \code{\link[RcmdrMisc]{stepwise}} labels the criterion in the output as \code{"AIC"} regardless of which criterion is actually employed.
#' 
#' This menu option calls the \code{\link[RcmdrMisc]{stepwise}} function in the \pkg{RcmdrMisc} package.
#'
#' @seealso See also \code{\link{lm}}, \code{\link{linearModel}}.
#'
#' @usage NULL
#' 
stepwiseRegression <- function () {
    Library("MASS")
    defaults <- list (initial.direction = "backward/forward", initial.criterion = "BIC")
    dialog.values <- getDialog ("stepwiseRegression", defaults)
    initializeDialog(title = gettextRcmdr("Stepwise Model Selection"))
    onOK <- function() {
        direction <- as.character(tclvalue(directionVariable))
        criterion <- as.character(tclvalue(criterionVariable))
        putDialog ("stepwiseRegression",
                   list (initial.direction = tclvalue(directionVariable), 
                         initial.criterion = tclvalue(criterionVariable)))
        closeDialog()
        doItAndPrint(paste("stepwise(", ActiveModel(), ", direction='", 
                           direction, "', criterion='", criterion, "')", sep = ""))
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "stepwiseRegression",
                 reset = "stepwiseRegression", 
                 apply =  "stepwiseRegression")
    radioButtons(top,
                 name = "direction",
                 buttons = c("bf", "fb", "b", "f"),
                 values = c("backward/forward", "forward/backward", "backward", "forward"),
                 labels = gettextRcmdr(c("backward/forward", "forward/backward", "backward", "forward")),
                 title = gettextRcmdr("Direction"), 
                 initialValue = dialog.values$initial.direction)
    radioButtons(top,
                 name = "criterion",
                 buttons = c("bic", "aic"),
                 values = c("BIC", "AIC"),
                 labels = gettextRcmdr(c("BIC", "AIC")),
                 title = gettextRcmdr("Criterion"), initialValue = dialog.values$initial.criterion)
    tkgrid(directionFrame, criterionFrame, sticky = "nw")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    dialogSuffix()
}
