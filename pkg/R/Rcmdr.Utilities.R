#' @name Rcmdr.Utilities
#' 
#' @aliases activateMenus
#' @aliases activeDataSet
#' @aliases ActiveDataSet
#' @aliases activeDataSetP
#' @aliases activeModel
#' @aliases ActiveModel
#' @aliases activeModelP
#' @aliases anovaP
#' @aliases beginRmdBlock
#' @aliases beginRnwBlock
#' @aliases buttonRcmdr
#' @aliases Character
#' @aliases characterP
#' @aliases checkActiveDataSet
#' @aliases checkActiveModel
#' @aliases checkBoxes
#' @aliases checkClass
#' @aliases checkFactors
#' @aliases checkMethod
#' @aliases checkNumeric
#' @aliases checkReplace
#' @aliases checkTwoLevelFactors
#' @aliases checkVariables
#' @aliases closeCommander
#' @aliases closeDialog
#' @aliases Coef
#' @aliases Coefficients
#' @aliases CoefficientsP
#' @aliases commanderPosition
#' @aliases CommanderWindow
#' @aliases dataSetsP
#' @aliases defmacro
#' @aliases dialogSuffix
#' @aliases DiscreteNumeric
#' @aliases discreteNumericP
#' @aliases doItAndPrint
#' @aliases EffectP
#' @aliases endRmdBlock
#' @aliases endRnwBlock
#' @aliases enterKnitr
#' @aliases enterMarkdown
#' @aliases errorCondition
#' @aliases exists.method
#' @aliases Factors
#' @aliases factorsP
#' @aliases flushDialogMemory
#' @aliases formulaFields
#' @aliases gassign
#' @aliases getCases
#' @aliases getDialog
#' @aliases getFrame
#' @aliases getFrame.combobox
#' @aliases getFrame.listbox
#' @aliases getRcmdr
#' @aliases getSelection
#' @aliases getSelection.combobox
#' @aliases getSelection.listbox
#' @aliases gettextRcmdr
#' @aliases glmP
#' @aliases GrabFocus
#' @aliases groupsBox
#' @aliases groupsLabel
#' @aliases hclustSolutionsP
#' @aliases initializeDialog
#' @aliases insertRmdSection
#' @aliases is.SciViews
#' @aliases is.valid.name
#' @aliases is.valid.number
#' @aliases justDoIt
#' @aliases knitrP
#' @aliases labelRcmdr
#' @aliases Library
#' @aliases listAllModels
#' @aliases listAOVModels
#' @aliases listCharacter
#' @aliases listDataSets
#' @aliases listDiscreteNumeric
#' @aliases listFactors
#' @aliases listGeneralizedLinearModels
#' @aliases listLinearModels
#' @aliases listMultinomialLogitModels
#' @aliases listNumeric
#' @aliases listProportionalOddsModels
#' @aliases listTwoLevelFactors
#' @aliases lmP
#' @aliases logger
#' @aliases logLikP
#' @aliases LogWindow
#' @aliases MacOSXP
#' @aliases manualTranslationP
#' @aliases MarkdownP
#' @aliases mavericksP
#' @aliases Message
#' @aliases MessagesWindow
#' @aliases modelCapability
#' @aliases modelFormula
#' @aliases modelsP
#' @aliases multinomP
#' @aliases Numeric
#' @aliases numericP
#' @aliases OKCancelHelp
#' @aliases OutputWindow
#' @aliases packageAvailable
#' @aliases polrP
#' @aliases popCommand
#' @aliases popOutput
#' @aliases Predictors
#' @aliases PredictorsP
#' @aliases putDialog
#' @aliases putRcmdr
#' @aliases R~Commander~Utilities
#' @aliases RappP
#' @aliases Rcmdr~Utilities
#' @aliases RcmdrEditor
#' @aliases RcmdrTclSet
#' @aliases RcmdrTkmessageBox
#' @aliases Rcmdr.Utilities
#' @aliases removeLastRmdBlock
#' @aliases removeLastRnwBlock
#' @aliases removeNullRmdBlocks
#' @aliases removeNullRnwBlocks
#' @aliases removeStrayRmdBlocks
#' @aliases removeStrayRnwBlocks
#' @aliases RExcelSupported
#' @aliases rglLoaded
#' @aliases RmdWindow
#' @aliases RnwWindow
#' @aliases setBusyCursor
#' @aliases setIdleCursor
#' @aliases sortVarNames
#' @aliases subOKCancelHelp
#' @aliases subsetBox
#' @aliases suppressMarkdown
#' @aliases tclvalue
#' @aliases titleLabel
#' @aliases tkspinbox
#' @aliases trim.blanks
#' @aliases ttkentry
#' @aliases ttkframe
#' @aliases ttkradiobutton
#' @aliases ttkscrollbar
#' @aliases TwoLevelFactors
#' @aliases twoLevelFactorsP
#' @aliases UpdateModelNumber
#' @aliases variableComboBox
#' @aliases variableListBox
#' @aliases WindowsP
#' @aliases X11P
#' 
#' @title Rcmdr Utility Functions
#'
#' @author John Fox
#' 
#' @keywords misc
#' 
#' @description
#' Funtions to support additions to the Rcmdr.
#' 
#' These functions support writing additions to the Rcmdr package, preferably by writing  an Rcmdr plug-in package.
#' Although it is not recommended, additional R code can also be placed in files with file type \code{.R} in the \code{etc} subdirectory of the \pkg{Rcmdr} package.
#' In this case, you can add menus, submenus, and menu items by editing the file \code{Rcmdr-menus.txt} in the same directory.
#' 
#'
#' @usage
#' activateMenus()
#' activeDataSet(dsname, flushModel=TRUE, flushDialogMemory=TRUE)
#' ActiveDataSet(name)
#' activeDataSetP()
#' activeModel(model)
#' ActiveModel(name)
#' activeModelP()
#' anovaP()
#' beginRmdBlock()
#' beginRnwBlock()
#' Character(names)
#' characterP(n=1)
#' checkActiveDataSet()
#' checkActiveModel()
#' checkBoxes(window=top, frame=stop("frame not supplied"),
#'     boxes=stop("boxes not supplied"),
#'     initialValues=NULL, labels=stop("labels not supplied"),
#'     title=NULL, ttk=FALSE, columns=1)  # macro
#' checkClass(object, class, message=NULL)  # macro
#' checkFactors(n=1)
#' checkMethod(generic, object, message=NULL, default=FALSE, strict=FALSE,
#'     reportError=TRUE)  # macro
#' checkNumeric(n=1)
#' checkReplace(name, type=gettextRcmdr("Variable"))
#' checkTwoLevelFactors(n=1)
#' checkVariables(n=1)
#' closeCommander(ask=TRUE, ask.save=ask)
#' closeDialog(window, release=TRUE)  # macro
#' Coef(object, ...)
#' Predictors(type=c("all", "numeric", "factor"))
#' PredictorsP(n=1, type=c("all", "numeric", "factor"))
#' CommanderWindow()
#' dataSetsP(n=1)
#' defmacro(..., expr)
#' dialogSuffix(window=top, onOK=onOK, onCancel=onCancel, rows, columns,
#' 	focus=top, bindReturn=TRUE,
#'     preventGrabFocus=FALSE, preventDoubleClick=FALSE, preventCrisp,
#'     use.tabs=FALSE, notebook=notebook, tabs=c("dataTab", "optionsTab"),
#'     tab.names=c("Data", "Options"), grid.buttons=FALSE, resizable=FALSE,
#'     force.wait=FALSE)  # macro
#' DiscreteNumeric(names)
#' discreteNumericP(n=1)
#' doItAndPrint(command, log=TRUE, rmd=log)
#' EffectP()
#' endRmdBlock()
#' endRnwBlock()
#' enterMarkdown(command)
#' enterKnitr(command)
#' errorCondition(window=top, recall=NULL, message, model=FALSE)  # macro
#' exists.method(generic, object, default=TRUE, strict=FALSE)
#' Factors(names)
#' factorsP(n=1)
#' formulaFields(model, hasLhs=TRUE, glm=FALSE)
#' flushDialogMemory(what)
#' gassign(x, value)
#' getCases(cases, remove=TRUE)
#' getDialog(dialog, defaults=NULL)
#' \method{getFrame}{combobox}(object)
#' \method{getFrame}{listbox}(object)
#' \method{getSelection}{combobox}(object)
#' \method{getSelection}{listbox}(object)
#' getRcmdr(x, mode="any", fail=TRUE)
#' gettextRcmdr(...)
#' glmP()
#' GrabFocus(value)
#' groupsBox(recall=NULL, label=gettextRcmdr("Plot by:"),
#'     initialLabel=gettextRcmdr("Plot by groups"),
#'     errorText=gettextRcmdr("There are no factors in the active data set."),
#'     variables=Factors(),
#'     plotLinesByGroup=FALSE, positionLegend=FALSE,
#'     plotLinesByGroupsText=gettextRcmdr("Plot lines by group"),
#'     initialGroup=NULL, initialLinesByGroup=1, window=top)  # macro
#' groupsLabel(frame=top, groupsBox=groupsBox, columnspan=1,
#'     initialText=NULL, ratio=FALSE)  # macro
#' hclustSolutionsP()
#' initializeDialog(window=top, title="", offset=10, preventCrisp,
#'     use.tabs=FALSE, notebook=notebook,
#'     tabs=c("dataTab", "optionsTab"),
#'     suppress.window.resize.buttons=TRUE)  # macro
#' insertRmdSection(text)
#' is.valid.name(x)
#' is.valid.number(string)
#' is.SciViews()
#' justDoIt(command)
#' knitrP()
#' Library(package, pos=length(search()), rmd=TRUE)
#' listAllModels(envir=.GlobalEnv, ...)
#' listAOVModels(envir=.GlobalEnv, ...)
#' listCharacter(dataSet=ActiveDataSet())
#' listDataSets(envir=.GlobalEnv, ...)
#' listDiscreteNumeric(dataSet=ActiveDataSet())
#' listFactors(dataSet=ActiveDataSet())
#' listGeneralizedLinearModels(envir=.GlobalEnv, ...)
#' listLinearModels(envir=.GlobalEnv, ...)
#' listMultinomialLogitModels(envir=.GlobalEnv, ...)
#' listNumeric(dataSet=ActiveDataSet())
#' listProportionalOddsModels(envir=.GlobalEnv, ...)
#' listTwoLevelFactors(dataSet=ActiveDataSet())
#' listVariables(dataSet=ActiveDataSet())
#' lmP()
#' logger(command, rmd=TRUE)
#' logLikP()
#' LogWindow()
#' MacOSXP(release)
#' manualTranslationP()
#' MarkdownP()
#' mavericksP()
#' Message(message, type=c("note", "error", "warning"))
#' MessagesWindow()
#' modelCapability(capability)
#' modelFormula(frame=top, hasLhs=TRUE, rhsExtras=NULL,
#'     formulaLabel=gettextRcmdr("Model Formula"), showBar=FALSE)  # macro
#' modelsP(n=1)
#' multinomP()
#' Numeric(names)
#' numericP(n=1)
#' OKCancelHelp(window=top, helpSubject=NULL, model=FALSE,
#'     reset=NULL, apply=NULL, helpPackage=NULL)  # macro
#' OutputWindow()
#' packageAvailable(name)
#' polrP()
#' popCommand(keep=FALSE)
#' popOutput(keep=FALSE)
#' putDialog(dialog, values=NULL, resettable=TRUE)
#' putRcmdr(x, value)
#' RappP()
#' RcmdrEditor(buffer, title="R Commander Editor", ok,
#'     help=NULL, file.menu=NULL, edit.menu=NULL, context.menu=NULL,
#'     toolbar.buttons=NULL)
#' RcmdrTclSet(name, value)
#' RcmdrTkmessageBox(message, icon=c("info", "question", "warning",
#'     "error"), type=c("okcancel", "yesno", "ok"), default, title="")
#' removeLastRmdBlock()
#' removeLastRnwBlock()
#' removeNullRmdBlocks()
#' removeNullRnwBlocks()
#' removeStrayRmdBlocks()
#' removeStrayRnwBlocks()
#' RExcelSupported()
#' rglLoaded()
#' RmdWindow()
#' RnwWindow()
#' setBusyCursor()
#' setIdleCursor()
#' sortVarNames(x)
#' subOKCancelHelp(window=subdialog, helpSubject=NULL)  # macro
#' subsetBox(window = top, subset.expression = NULL, model = FALSE)  # macro
#' suppressMarkdown(command)
#' tclvalue(x)
#' titleLabel(...)
#' tkspinbox(parent, ...)
#' trim.blanks(text)
#' TwoLevelFactors(names)
#' twoLevelFactorsP(n=1)
#' UpdateModelNumber(increment=1)
#' variableComboBox(parentWindow, variableList=Variables(),
#'     export="FALSE", state="readonly",
#'     initialSelection=gettextRcmdr(nullSelection),
#'     title="", nullSelection="<no variable selected>",
#'     adjustWidth = FALSE)
#' variableListBox(parentWindow, variableList=Variables(), bg="white",
#'     selectmode="single", export="FALSE", initialSelection=NULL,
#'     listHeight=getRcmdr("variable.list.height"), title)
#' Variables(names)
#' WindowsP()
#' X11P()
## the following function is exported for technical reasons,
## but are not meant to be called directly
#' commanderPosition()
#' 
#' @param adjustWidth adjust width of combo box to accommodate widest entry (default \code{FALSE}).
#' @param ask ask for confirmation.
#' @param ask.save ask whether to save contents of script and output windows.
#' @param apply if non-null (the default is \code{NULL}), an Apply button is included in the dialog's button bar. This argument should be set to the quoted name of the function that initiates the dialog; when the button is pressed, the \code{onOK} function for the dialog is executed, and then the function named in \code{apply} is (re)called.
#' @param bg background color.
#' @param bindReturn if \code{TRUE}, the \emph{Return} key is bound to the \code{onOK} function in the dialog.
#' @param boxes vector of quoted names for check boxes, used to generate each box and its associated variable.
#' @param buffer a text string, typically representing the contents of a text widget, such as an R Markdown or knitr document.
#' @param buttons vector of quoted names for buttons in a set of related radio buttons.
#' @param capability character string giving the name of a column in the R Commander model-capabilities table, including the name of a column added by a plug-in; see \code{model-capabilities.txt} in the \pkg{Rcmdr} sources for the standard table; e.g., \code{"sum"} indicates the availability of an applicable \code{summary()} method for the current model.
#' @param cases a character string of case number or names to be removed or retained, separated by blanks.
#' @param class quoted name of class.
#' @param columns number of columns into which to arrange check boxes or radio buttons; boxes and buttons are filled by rows; the default is 1 and the allowed values are 1, 2, 3, and 4.
#' @param columnspan number of dialog-box columns to be spanned by frame.
#' @param command a character string that evaluates to an R command or (in the case of \code{radioButton}) a function to be called when a button is pressed.
#' @param context.menu \code{NULL} or a list containing one or more two-element lists: the first element, \code{label}, supplies the text label for a menu item in the \code{RcmdrEditor} right-click context menu; the second element, \code{command}, is a call-back function to be evaluated when the menu item is selected. If \code{NULL} (the default), no item will be added to the file menu.
#' @param dataSet An optional character string specifying the name of a data set. If omitted, the function defaults to the active data set.
#' @param default default button: if not specified, "ok" for "okcancel", "yes" for "yesno", and "ok" for "ok"; or look for a default method; for \code{putDialog}, a list of defaults for the dialog box if there are no stored previous values.
#' @param defaults a list of named default values for options in a dialog if no previous selections are stored.
#' @param dialog the quoted name of a dialog box under which previous selections are stored.
#' @param dsname name of the data set to activate.
#' @param edit.menu \code{NULL} or a list containing one or more two-element lists: the first element, \code{label}, supplies the text label for a menu item in the \code{RcmdrEditor} Edit menu; the second element, \code{command}, is a call-back function to be evaluated when the menu item is selected. If \code{NULL} (the default), no item will be added to the file menu.
#' @param envir the environment to be searched; should generally be left at the default.
#' @param errorText error message to print if a suitable factor isn't available.
#' @param export export selection?
#' @param expr An expression constituting the body of the macro; typically a compound expression.
#' @param fail if \code{TRUE}, the default, \code{getRcmdr} will generate an error if the object sought doesn't exist; if \code{FALSE} and the object doesn't exist, \code{NULL} is returned.
#' @param file.menu \code{NULL} or a list containing one or more two-element lists: the first element, \code{label}, supplies the text label for a menu item in the \code{RcmdrEditor} File menu; the second element, \code{command}, is a call-back function to be evaluated when the menu item is selected. If \code{NULL} (the default), no item will be added to the file menu.
#' @param flushDialogMemory remove saved values of dialog options so that \code{getDialog} returns \code{NULL} for all dialogs.
#' @param flushModel set (or reset) the active model to NULL? Should normally be \code{TRUE} when the active data set is changed; an exception is when variables are simply added to, deleted from, or modified in the data set set.
#' @param focus Tk window to get the focus.
#' @param force.wait call \code{tkwait.window} so that processing is suspended until the dialog is closed; overrides the \pkg{Rcmdr} \code{tkwait.dialog} option (see \code{\link{Commander}}) if the latter is set to \code{FALSE} (its default). The \code{force.wait} argument should be set to \code{TRUE} for subdialogs.
#' @param formulaLabel text label printed above the formula widget.
#' @param frame frame or quoted name for frame depending upon the function.
#' @param generic quoted name of generic function.
#' @param glm \code{TRUE} if the model is a \code{glm} object, \code{FALSE} otherwise.
#' @param grid.buttons insert call to \code{tkgrid} for the buttons frame (default \code{FALSE}); use \code{TRUE} for tabbed dialogs and optionally for other dialogs.
#' @param groupsBox listbox object for selecting groups variable.
#' @param initialText initial text to display in the groups label; if \code{NULL}, \code{"<No groups selected>"} will be displayed.
#' @param hasLhs does the model formula have a left-hand side?
#' @param help a two element list: the first element, \code{label}, supplies the text label for a menu item in the \code{RcmdrEditor} Help menu; the second element, \code{command}, is a call-back function to be evaluated when the menu item is selected. If \code{NULL} (the default), no item will be added to the editor Help menu.
#' @param helpSubject the quoted name of a help subject, to be called as \code{help(helpSubject)} when the dialog \emph{Help} button is pressed.
#' @param helpPackage the quoted name of the package in which to look for help; the default, \code{NULL}, produces a search in all loaded packages --- see \code{\link{help}}.
#' @param icon Message-box icon.
#' @param increment increment to model number; -1 to set back after error.
#' @param initialGroup quoted name of variable to define groups, set as initial selection in Groups variable list; \code{NULL} (the default) for no initial selection.
#' @param initialLinesByGroup if 1, the lines-by-groups check box is initially checked; 0 to uncheck.
#' @param initialLabel label for groups button before a selection is made.
#' @param initialSelection index of item initially selected, 0-base indexing.
#' @param initialValues for a set of related check boxes.
#' @param keep if \code{TRUE}, keep (rather than pop) last output or command in the stack; the default is \code{FALSE}.
#' @param label label prefix for groups button after a selection is made.
#' @param labels a vector of character strings to label a set of radio buttons or check boxes.
#' @param listHeight Maximum number of elements displayed simultaneously in list box.
#' @param log echo command to the script window, as well as executing it and printing its output.
#' @param message error (or other) message.
#' @param mode mode of object to retrieve.
#' @param model the name of a model, as a character string, or a model object, or \code{TRUE} or \code{FALSE}, depending upon the function.
#' @param n number of items to check for.
#' @param name quoted name.
#' @param names optional names to be stored.
#' @param notebook notebook windows for a tabbed dialog (default \code{notebook}).
#' @param nullSelection what user selects in combo box to indicate nothing selected (default \code{"<no variable selected>"}).
#' @param object an object (depends on context).
#' @param offset in pixels, from top-left of Commander window.
#' @param ok a function called when the \emph{OK} button is pressed in a \code{RcmdrEditor} window that saves the editor buffer to the appropriate \emph{Tcl} variable.
#' @param onOK function to execute when the \emph{OK} button is pressed.
#' @param onCancel function to execute when the \emph{Cancel} button or \emph{Esc} key is pressed.
#' @param package quoted name of package to load.
#' @param parent A Tk window object serving as the parent container.
#' @param parentWindow A Tk window object serving as the parent container.
#' @param plotLinesByGroup include a check box for plotting lines by group?
#' @param plotLinesByGroupsText the label for the plot-lines-by-group check box.
#' @param pos position on search path at which to load package; default is just before the end of the path.
#' @param positionLegend include a check box for a legend?
#' @param preventGrabFocus prevent the dialog box from grabbing the focus.
#' @param preventDoubleClick prevent double-clicking from pressing the OK button, even when the double.click option is set; necessary for statistical modelling dialogs, which use double-clicking to build the model formula.
#' @param preventCrisp this argument is ignored, and is present only for backwards compatibility.
#' @param ratio If \code{FALSE}, the default, the test will be expressed as a test for a difference (e.g., a difference in means); if \code{TRUE}, as a test for a ratio (e.g., a ratio of variances).
#' @param recall function to call after error --- usually the function that initiates the dialog.
#' @param release release the focus if the grab.focus option has been set. In \code{MacOSXP}, the minimum release (version) required.
#' @param remove If \code{TRUE}, the default, return a character string that evaluates to an expression of case numbers or names to remove from the data set; if \code{FALSE}, return a character string that evaluates for an expression of case numbers or names to retain.
#' @param reportError if \code{TRUE}, report an error message.
#' @param reset quoted name of dialog function, to be invoked with all defaults by Reset button.
#' @param resettable should dialog state be reset when the data set changes? The default is \code{TRUE}.
#' @param resizable should the dialog be resizable by the user? The default is \code{FALSE}.
#' @param rhsExtras show controls for splines and polynomials for a model formula; for backwards compatibility, defaults to \code{TRUE} for a two-sided formula and \code{FALSE} for a one-sided formula.
#' @param rmd enter the command in the R Markdown tab.
#' @param rows numbers of rows of widgets in the dialog box; this is actually no longer used, but is still present for backwards compatibility. The \code{columns} argument is similarly ignored, except for radio buttons and check boxes.
#' @param selectmode \code{"single"} or \code{"multiple"}.
#' @param showBar include a \emph{bar} (|) button in the model-formula widget (default is \code{FALSE}).
#' @param state state of the combobox widget; default \code{"readonly"} means user can't type in the box; set to \code{"normal"} to permit typing.
#' @param strict if \code{TRUE}, only use first element of class vector.
#' @param string a character string, or vector of strings, to be tested whether it can be coerced to a number or numbers; returns either \code{TRUE} or \code{FALSE}.
#' @param subset.expression a quoted expression to subset the data set.
#' @param suppress.window.resize.buttons if \code{TRUE}, the default, the window maximize/minimize buttons will not be displayed.
#' @param tab.names text to print as tab labels (default \code{c("Data", "Options")}).
#' @param tabs quoted names of tabs for a tabbed dialog (default \code{c("dataTab", "optionsTab")}).
#' @param text a text string.
#' @param title Window or dialog-box-element title.
#' @param toolbar.buttons \code{NULL} or a list containing one or more three-element lists: the first element, \code{label}, supplies the text label for a button in the \code{RcmdrEditor} toolbar; the second element, \code{command}, is a call-back function to be evaluated when the button is pressed; the third element is the name of a tk image to display as an icon in the button. If \code{NULL} (the default), no buttons will be added to the toolbar.
#' @param ttk use ttk themed widget for check boxes.
#' @param type quoted type of object to check; used to generate check-replace dialog box; or type of message to print in Message window. For \code{varPosn}, type of variable list. For \code{Predictor} and \code{nPredictor}, type of explanatory variable in the active model.
#' @param use.tabs (default \code{FALSE}) construct a tabbed dialog.
#' @param value an object to be stored or assigned.
#' @param values vector of quoted values associated with radio buttons or check boxes; for \code{putDialog}, a list of current selections to be stored in support of dialog memory.
#' @param variableList a vector of variable names.
#' @param variables a vector of one or more variable names.
#' @param what optional character vector of one or more dialog names for which the memory is to be flushed; if not specified, all dialog memory will be flushed.
#' @param window A Tk window object serving as the parent container.
#' @param x an R object name, as a character string, or a tcl variable or object, or a vector of variable names to be sorted.
#' @param \dots For \code{gettextRcmdr}, text string or vector of text strings to translate; for \code{titleLabel}, arguments to be passed to \code{labelRcmdr} and from there to \code{ttklabel}; for \code{defmacro}, arguments for the macro; otherwise you should disregard this argument.
#'
#' @details
#'
#' There are several groups of functions exported by the \pkg{Rcmdr} package and documented briefly here.
#' To see how these functions work, it is simplest to examine the dialog-generating functions in the \pkg{Rcmdr} package.
#' Also see the \pkg{RcmdrPlugin.survival} package for examples.
#'
#' @section Executing and logging commands:
#' 
#' The functions \code{doItAndPrint}, \code{justDoIt}, and \code{logger} control the execution, logging, and printing of commands generated by menus and dialogs.
#' \code{logger(command)} adds \code{command} to the log/script window and to the output window.
#' \code{justDoIt(command)} causes \code{command} to be executed.
#' \code{doItAndPrint(command)} does both of these operations, and also prints the output produced by the command.
#' The R Commander maintains a list of output objects, by default including the last 10 outputs.
#' \code{popOutput()} ``pops'' (i.e., returns and removes) the first entry of the output stack.
#' Note that, as a stack, the queue is LIFO (``last in, first out'').
#' Use \code{popOutput(keep=TRUE)} to access the last output but keep it in the stack.
#' There is also a stack of commands, which is accessed similarly by \code{popCommand()}.
#' Ocassionally, it's necessary to assign an object directly in the global environment, and this can be done with the \code{gassign} function.
#' 
#' Normally commands also generate an R Markdown block.
#' \code{suppressMarkdown} takes a command in character-string form and adds an attribute to it that will cause the command \emph{not} to be entered in the R Markdown tab. This is useful when a command, such as \code{identify}, requires direct user interaction and won't generate useful Markdown.
#' \code{enterMarkdown} can be used to enter command blocks directly in the R Markdown tab; this should rarely be required.
#' The functions \code{beginRmdBlock}, \code{endRmdBlock}, \code{removeNullRmdBlocks}, \code{removeLastRmdBlock}, and \code{removeStrayRmdBlocks} should normally not be called directly.
#' The functions \code{enterKnitr}, \code{beginRnwBlock}, \code{endRnwBlock}, \code{removeNullRnwBlocks}, \code{removeLastRnwBlock}, and \code{removeStrayRnwBlocks} perform similar functions for Knitr documents.
#' \code{insertRmdSection} Inserts a Markdown section title immediately above the last R command block, with the specified text as the title.
#' In most instances it's unnecessary to do this directly because most commands automatically generate a Markdown section title.
#'
#' @section Checking for errors:
#' The function \code{is.valid.name} checks whether a character string specifies a valid name for an R object.
#' The function \code{is.valid.number} checks whether a character string  (or vector) can be coerced to a number (or numbers).
#' The functions \code{checkActiveDataSet}, \code{checkActiveModel}, \code{checkFactors}, \code{checkNumeric}, \code{checkTwoLevelFactors}, and \code{checkVariables} check for the existence of objects and write an error message to the log if they are absent (or insufficiently numerous, in the case of different kinds of variables).
#' The function \code{checkReplace} opens a dialog to query whether an existing object should be replaced.
#' The function \code{checkMethod}, checks whether a method exists for a particular generic that is appropriate for a particular object.
#' The function \code{checkClass} checks whether an object is of a specific class.
#' Both of these functions write error messages to the log if the condition fails.
#' The function \code{errorCondition} reports an error to the user and (optionally) re-starts a dialog.
#'
#' @section Information:
#'
#' Several functions return vectors of object names: \code{listAllModels}, \code{listAOVModels}, \code{listCharacter}, \code{listDataSets}, \code{listDiscreteNumeric}, \code{listGeneralizedLinearModels}, \code{listFactors}, \code{listLinearModels}, \code{listMultinomialLogitModels}, \code{listNumeric}, \code{listProportionalOddsModels}, \code{listTwoLevelFactors}, \code{listVariables}.
#' The functions \code{Predictors} and \code{Coefficients} return information about the active model, or \code{NULL} if there is no active model.
#' The functions \code{activeDataSet} and \code{activeModel} respectively report or set the active data set and model.
#' The function \code{packageAvailable} reports whether the named package is available to be loaded (or has possibly already been loaded).
#' The function \code{exists.method} checks whether a method exists for a particular generic that is appropriate for a particular object, and returns \code{TRUE} or \code{FALSE}.
#' The function \code{is.SciViews} always returns \code{FALSE} since the SciViews GUI is no longer supported.
#'
#' The function \code{modelCapability()} returns \code{TRUE} if there is an active statistical model and if it has the specified capability.
#' For example, \code{modelCapability("sum")} returns \code{TRUE} if the model-capabilities table indicates that there's an applicable \code{summary()} method for the active model. Otherwise, \code{FALSE} is returned. If the specified capability doesn't exist, a warning is printed.
#'
#' @section Building dialog boxes:
#' Several functions simplify the process of constructing Tk dialogs: initializing a dialog box, \code{initializeDialog}, and completing the definition of a dialog box, \code{dialogSuffix}; a set of check boxes, \code{checkBoxes}; a set of radio buttons, \code{radioButtons}; a list box with associated scrollbars and state variable, \code{variableListBox} (and associated methods for the generic functions \code{getFrame} and \code{getSelection}); a drop-down "combo" box, \code{variablecomboBox} (with \code{getFrame} and \code{getSelection} methods); a button and subdialog for selecting a "grouping" variable, \code{groupsBox}; displaying the currently defined groups in a dialog, \code{groupsLabel}; a dialog-box structure for entering a model formula, \code{modelFormula}; a text box for entering a subsetting expression, \code{subsetBox}; \emph{OK}, \emph{Cancel}, and \emph{Help} buttons for dialogs, \code{OKCancelHelp}, and subdialogs, \code{subOKCancelHelp}.
#' The functions \code{putDialog}, \code{getDialog}, and \code{varPosn} support dialog-box memory---i.e., retaining selections across invocations of a dialog.
#' The \code{tkspinbox} function is omitted from the tcltk package and may be used to create a spinbox widget.
#' The \code{titleLabel} function may be used to format a title label to use the standard title label font and color.
#'
#' @section ``Themed'' Tk widgets:
#'
#' Tk 8.5 introduced so-called ``themed'' widgets, which look better than the traditional Tk widgets.
#' Several functions, contributed by Brian Ripley, are written to access the new widgets by switching automatically between the new and old widget sets depending upon the availability of the former: \code{buttonRcmdr}, to access either \code{\link{ttkbutton}} or \code{\link{tkbutton}}; \code{labelRcmdr}, to access either \code{\link{ttklabel}} or \code{\link{tklabel}}; \code{ttkentry}, to access either \code{\link{ttkentry}} or \code{\link{tkentry}}; \code{ttkframe}, to access either \code{\link{ttkframe}} or \code{\link{tkframe}}; \code{ttkradiobutton}, to access either \code{\link{ttkradiobutton}} or \code{\link{tkradiobutton}}; and \code{ttkscrollbar}, to access either \code{\link{ttkscrollbar}} or \code{\link{tkscrollbar}}.
#' Note that the last four functions mask functions of the same names in the \pkg{tcltk} package.
#'
#' @section ``Predicate'' functions:
#'
#' A number of functions of the form \emph{name}\code{P} are `predicate' functions, which return \code{TRUE} or \code{FALSE} depending upon whether some condition obtains.
#' For example, \code{lmP()} returns \code{TRUE} if there is an active model that is a linear model; and \code{factorsP(2)} returns \code{TRUE} if there are at least two factors in the active data set.
#' \code{WindowsP()}, \code{MacOSXP()}, and \code{X11P()} return \code{TRUE} if the R Commander is running under Windows, Mac OS X, or X-Windows, consecutively.
#'
#' @section Translating text:
#'
#' The \code{gettextRcmdr} function simply passes its argument(s) to \code{\link[base]{gettext}}, adding the argument \code{domain="R-Rcmdr"}.
#' It is not meant to be used in plug-in packages.
#'
#' @section Miscellaneous:
#'
#' The function \code{trim.blanks} removes spaces from the beginning and end of a character string.
#' 
#' The function \code{installPlugin} installs an Rcmdr plug-in from a ZIP file or directory; this function may be used to create self-installing plug-ins in the form of packages.
#' 
#' The function \code{nobs} returns the number of observations on which a statistical model is based.
#' 
#' The function \code{formulaFields} returns information about the left-hand side, right-hand side, data, subset, and (for GLMs) family and link, of a model object.
#' 
#' The function \code{sortVarNames} sorts variable names, including those containing numerals, into a more ``natural'' order than does the standard \code{sort} function.
#' 
#' The function \code{Library} may be used to load packages; it checks whether a package is already loaded, and if not by default puts it in position 4 on the search path.
#' 
#' The function \code{Coef}, with several methods, returns the coefficients of a model as a vector; the default method just calls \code{coef}.
#'
#' The function \code{RExcelSupported} is used for the RExcel interface.
#'
#' The function \code{getCases} takes a character string of case names or numbers as an argument and returns a character string that evaluates to an expression either to delete or retain these cases.
#'
#' Some of these functions, marked \code{# macro} under \emph{Usage}, are "macro-like" in their behaviour, in that they execute in the environment from which they are called.
#' These were defined with an adaptation (used with permission) of Thomas Lumley's \code{defmacro} function, described in Lumley (2001), and are used in the R Commander to deal with scoping issues related to Tcl/Tk.
#'
#' The \code{tkfocus} function is exported for historical reasons.
#'
#' @references
#' T. Lumley (2001) Programmer's niche: Macros in R. \emph{R News}, \bold{1(3)}, 11--13.
NULL
