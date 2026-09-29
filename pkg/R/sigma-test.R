#' @name sigmaTestMenu
#'
#' @title Variance test
#'
#' @author Manuel Munoz-Marquez <manuel.munoz@uca.es>
#'
#' @keywords package
#'
#### Para ayuda en español, véase \code{\link{sigmaTestMenu.es}}. (For Spanish help see \code{\link{sigmaTestMenu.es}}.) 
#'
#' @description
#' Within the "Statistics" -> "Variances" menu, a entry is provided for calculate confidence intervals and make contrasts on variance in a normal population.
#' This option uses the function \code{sigma.test} of the package \code{TeachingDemos}.
#' For more information see \code{\link[TeachingDemos]{sigma.test}}.
#'
#' @details
#' Here is an example of "Single-Sample Variance Test..." menu entry.
#'
#' Load data "BJsales" selecting from Rcmdr menu: "Data" -> "Data in packages" -> "Read data set from an attached package..." then double-click on "datasets", click on "BJsales" and on "OK".
#' 
#' Rcmdr reply with the following command in source pane (R Script)
#'
#' \code{data(BJsales, package="datasets")}
#'
#' \code{BJsales <- as.data.frame(BJsales)}
#'
#' To build a confidence interval for sigma on variable \code{x}, select from Rcmdr menu: "Statistics" -> "Variances" -> "Single-sample Variance Test..." select "x".
#' Enter 500 in the "Null hypothesis sigma0^2" field to test the hypothesis that the population variance is 500, and click OK.
#' Rcmdr reply with the following command in source pane (R Script)
#' 
#' \code{with(na.omit(BJsales), sigma.test(x, alternative='two.sided', sigmasq=500, conf.level=0.95))}
#'
#' And the result shown in the Output panel is
#'
#' \preformatted{
#' 	One sample Chi-squared test for variance
#' 
#' data:  x
#' X-squared = 137.49, df = 149, p-value = 0.5184
#' alternative hypothesis: true variance is not equal to 500
#' 95 percent confidence interval:
#' 372.2253 587.0738
#' sample estimates:
#' var of x
#' 461.3769 
#' }
#' 
#' @export
sigmaTestMenu <- function() {
    ## This function is developed from singleSampleTTest in Rcmdr and use function sigma.test in package TeachingDemos
    ## This code was initially on RcmdrPlugin.UCA package
    ## To ensure that menu name is included in pot file
    gettextRcmdr("Single-Sample Variance Test...")
    defaults <- list (initial.x = NULL, initial.alternative = "two.sided", initial.level = "0.95", initial.sigma = "1.0")
    dialog.values <- getDialog ("sigmaTestMenu", defaults)
    initializeDialog(title = gettextRcmdr("Single-Sample Variance Test"))
    xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"), initialSelection = varPosn(dialog.values$initial.x, "numeric"))
    onOK <- function() {
        x <- getSelection(xBox)
        if (length(x) == 0) {
            errorCondition(recall = sigmaTestMenu, message = gettextRcmdr("You must select a variable."))
            return()
        }
        alternative <- tclvalue(alternativeVariable)
        level <- tclvalue(confidenceLevel)
        sigma <- tclvalue(sigmaVariable)
        putDialog("sigmaTestMenu", list (initial.x = x, initial.alternative = alternative, initial.level = level, initial.sigma = sigma))
        closeDialog()
        doItAndPrint(paste("with(na.omit(", ActiveDataSet (), "), sigma.test(", x, ", alternative='", alternative, "', sigmasq=", sigma, ", conf.level=", level, "))", sep = ""))
        tkdestroy(top)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "sigmaTestMenu", reset = "sigmaTestMenu", apply = "sigmaTestMenu")
    optionsFrame <- tkframe(top)
    radioButtons(optionsFrame, name = "alternative", buttons = c("twosided", "less", "greater"), values = c("two.sided", "less", "greater"), labels = paste(gettextRcmdr("Population variance"), c("!= sigma0^2", "< sigma0^2", "> sigma0^2")), title = gettextRcmdr("Alternative Hypothesis"), initialValue = dialog.values$initial.alternative)
    rightFrame <- tkframe(optionsFrame)
    confidenceFrame <- tkframe(rightFrame)
    confidenceLevel <- tclVar(dialog.values$initial.level)
    confidenceField <- ttkentry(confidenceFrame, width = "6", textvariable = confidenceLevel)
    sigmaFrame <- tkframe(rightFrame)
    sigmaVariable <- tclVar(dialog.values$initial.sigma)
    sigmaField <- ttkentry(sigmaFrame, width = "8", textvariable = sigmaVariable)
    tkgrid(getFrame(xBox), sticky = "nw")
    tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
    tkgrid(labelRcmdr(sigmaFrame, text = gettextRcmdr("Null hypothesis: sigma0^2 =")),  sigmaField, sticky = "w", padx=c(10, 0))
    tkgrid(sigmaFrame, sticky = "w")
    tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level: ")), confidenceField, sticky = "w", padx=c(10, 0))
    tkgrid(confidenceFrame, sticky = "w")
    tkgrid(alternativeFrame, rightFrame, sticky = "nw")
    tkgrid(optionsFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
    tkgrid.configure(confidenceField, sticky = "e")
    dialogSuffix()
}

#' @title sigma.test
#' 
#' @importFrom TeachingDemos sigma.test
#' 
#' @keywords internal
#' 
#' @export sigma.test
sigma.test <- TeachingDemos::sigma.test

## @name sigmaTestMenu.es
##
## @title Test de varianza
##
## @author Manuel Munoz-Marquez <manuel.munoz@uca.es>
##
## @keywords package
## @description
## Dentro del menú "Estadísticos" -> "Varianzas", se proporcionan una entrada para calcular intervalos de confianza y hacer contrastes sobre la varianza en una población normal.
## Esta opción usa la función \code{sigma.test} del paquete \code{TeachingDemos}.
## Para más información véase \code{\link[TeachingDemos]{sigma.test}}.
## 
## @details
## Ejemplo de uso del menú "Test de varianza para una muestra...".
## 
## Cargar los datos "BJsales" eligiendo del menu de Rcmdr: "Datos" -> "Conjuntos de datos en paquetes" -> "Leer conjunto de datos desde paquete adjunto..." pulsar dos veces sobre "datasets", pulsar sobre "BJsales" y sobre "Aceptar".
## Rcmdr responde con la siguiente instrucción en el cuadro de instrucciones (R Script)
## 
## \code{data(BJsales, package="datasets")}
## 
## \code{BJsales <- as.data.frame(BJsales)}
## 
## Para construir el intervalo de confianza para sigma para la variable "x", seleccione del menú de Rcmdr: "Estadísticos" -> "Varianzas" -> "Test de varianza para una muestra..." seleccione "x" y "Aceptar".
## Rcmdr responde con la siguiente instruccion en el cuadro de instrucciones (R Script)
## 
## \code{with(BJsales, sigma.test(x[!is.na(x)], alternative='two.sided', sigmasq=1.0, conf.level=0.95))}
## 
