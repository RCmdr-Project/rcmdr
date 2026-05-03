#' @name Rcmdr-package
#'
#' @title R Commander
#'
#' @aliases Rcmdr Rcmdr-package
#' 
#' @details
#' A platform-independent basic-statistics GUI (graphical user interface) for R, based on the tcltk package.
#'
#' \tabular{ll}{
#' Package: \tab Rcmdr\cr
#' Version: \tab 2.12.3\cr
#' Date: \tab 2026-05-04\cr
#' Depends: \tab R (>= 3.5.0), grDevices, graphics, methods, stats, utils, splines, RcmdrMisc (>= 2.10.1), car (>= 3.1-0), effects (>= 4.0-3)\cr
#' Imports: \tab tcltk, tcltk2 (>= 1.2-6), abind, relimp (>= 1.0-5), lme4, tools\cr
#' Suggests: \tab aplpack, boot, colorspace, e1071, foreign, grid, Hmisc, knitr, lattice, leaps, lmtest, markdown, MASS, mgcv, multcomp (>= 0.991-2), nlme, nnet, nortest, readxl, rgl (>= 0.110.2), rmarkdown (>= 0.9.5), sem (>= 2.1-1)  \cr
#' ByteCompile: \tab yes\cr
#' License: \tab GPL (>= 3)\cr
#' URL: \tab https://github.com/RCmdr-Project/rcmdr, https://www.r-project.org, https://www.john-fox.ca/RCommander/index.html \cr
#' }
#'
#' @references
#' Fox, J. (2017)
#' \emph{Using the R Commander: A Point-and-Click Interface for R}.
#' Chapman and Hall/CRC Press.
#'
#' Fox, J. (2005)
#' The R Commander: A Basic Statistics Graphical User Interface to R.
#' \emph{Journal of Statistical Software}, \bold{14(9)}: 1--42.
#'
#' @section Translations:
#' 
#' The R Commander comes with translations from English into several other languages.
#' I am grateful to the following individuals and groups for preparing these translations:
#' Basque, Jose Ramon Rueda;
#' Brazilian Portuguese, Adriano Azevedo-Filho and Marilia Sa Carvalho;
#' Catalan, Manel Salamero;
#' Chinese, Tsungwu Ho, Frank C. S. Liu, and Cheng-shun Lee;
#' Chinese (Simplified), Shulin Yang;
#' French, Philippe Grosjean and Milan Bouchet-Valat;
#' Galician, Anton Meixome;
#' German: Friedrich Leisch and Gerhard Schoen;
#' Greek: Vasileios Dimitropoulos, Anastasios Vikatos, and Andreas Vikatos;
#' Indonesian, I Made Tirta;
#' Italian, Stefano Calza;
#' Japanese, Takaharu Araki;
#' Korean, Chel Hee Lee, Dae-Heung Jang, and Shin Jong-Hwa;
#' Polish, Lukasz Daniel;
#' Romanian, Adrian Dusa;
#' Russian, Alexey Shipunov;
#' Slovenian, Jaro Lajovic and Matjaz Jeran;
#' Spanish, Spanish R-UCA Project, http://knuth.uca.es/R.
#'
#' @author
#' John Fox, Manuel Munoz-Marquez, and Milan Bouchet-Valat, with contributions from Liviu Andronic, Michael Ash, Theophilius Boye, Stefano Calza, Andy Chang, Vilmantas Gegzna, Philippe Grosjean, Richard Heiberger, G. Jay Kerns, Renaud Lancelot, Matthieu Lesnoff, Uwe Ligges, Samir Messad, Martin Maechler, Robert Muenchen, Duncan Murdoch, Erich Neuwirth, Dan Putler, Brian Ripley, Miroslav Ristic,  Peter Wolf, and Kevin Wright.
#'
#' Maintainer: Manuel Munoz-Marquez <manuel.munoz@uca.es>
#'
#' @import car effects RcmdrMisc splines tcltk
#' 
#' @importFrom abind abind
#' @importFrom graphics text
#' @importFrom grDevices col2rgb colorConverter colorspaces convertColor dev.cur dev.size palette rgb dev.new
#' @importFrom lme4 fixef
#' @importFrom methods as getClass isVirtualClass show
#' @importFrom relimp showData
#' @importFrom stats coef confint family formula getCall model.matrix na.omit optim optimize qnorm var vcov
#' @importFrom tcltk2 tk2tip tk2theme tk2theme.list tk2table is.ttk tk2style
#' @importFrom utils available.packages install.packages assignInMyNamespace browseURL capture.output globalVariables head help help.start installed.packages methods read.table tail
#'

NULL

#' @export
relimp::showData

#' @export
tcltk::.Tcl

#' @export
tcltk::tcl

#' @export
tcltk::tkfocus

#' @export
tcltk::ttkcheckbutton

#' @export
tcltk::tclRequire

#' @export
tcltk::tclServiceMode

#' @export
tcltk::"tclvalue<-"

#' @export
tcltk::tclVar

#' @export
tcltk::tkbind

#' @export
tcltk::tkcget

#' @export
tcltk::tkcheckbutton

#' @export
tcltk::tkconfigure

#' @export
tcltk::tkchooseDirectory

#' @export
tcltk::tkcurselection

#' @export
tcltk::tkdelete

#' @export
tcltk::tkdestroy

#' @export
tcltk::tkentry

#' @export
tcltk::tkframe

#' @export
tcltk::tkgrab.release

#' @export
tcltk::tkgrab.set

#' @export
tcltk::tkgetOpenFile

#' @export
tcltk::tkgetSaveFile

#' @export
tcltk::tkgrid

#' @export
tcltk::tkgrid.columnconfigure

#' @export
tcltk::tkgrid.configure

#' @export
tcltk::tkgrid.rowconfigure

#' @export
tcltk::tkicursor

#' @export
tcltk::tkindex

#' @export
tcltk::tkinsert

#' @export
tcltk::tkinvoke

#' @export
tcltk::tkitemconfigure

#' @export
tcltk::tklabel

#' @export
tcltk::tklistbox

#' @export
tcltk::tkmark.set

#' @export
tcltk::tkmessageBox

#' @export
tcltk::tkpack

#' @export
tcltk::tkradiobutton

#' @export
tcltk::tkraise

#' @export
tcltk::tkscale

#' @export
tcltk::tkscrollbar

#' @export
tcltk::tk_select.list

#' @export
tcltk::tkselection.clear

#' @export
tcltk::tkselection.present

#' @export
tcltk::tkselection.set

#' @export
tcltk::tkset

#' @export
tcltk::tktag.configure

#' @export
tcltk::tktext

#' @export
tcltk::tktoplevel

#' @export
tcltk::tkwait.window

#' @export
tcltk::tkwm.deiconify

#' @export
tcltk::tkwm.iconbitmap

#' @export
tcltk::tkwm.geometry

#' @export
tcltk::tkwm.protocol

#' @export
tcltk::tkwm.resizable

#' @export
tcltk::tkwm.title

#' @export
tcltk::tkwm.transient

#' @export
tcltk::tkxview.moveto

#' @export
tcltk::tkxview

#' @export
tcltk::tkyview

#' @export
tcltk::tkyview.scroll

#' @export
tcltk::ttklabelframe



NULL
