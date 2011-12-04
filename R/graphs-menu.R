# Graphs menu dialogs

# last modified 2011-12-04 by J. Fox
#  applied patch to improve window behaviour supplied by Milan Bouchet-Valat 2011-09-22

#indexPlot <- function(){
#    initializeDialog(title=gettextRcmdr("Index Plot"))
#    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
#    onOK <- function(){
#        x <- getSelection(xBox)
#        closeDialog()
#        if (length(x) == 0){
#            errorCondition(recall=indexPlot, message=gettextRcmdr("You must select a variable"))
#            return()
#            }
#        type <- if (tclvalue(typeVariable) == "spikes") "h" else "p"
#        identify <- tclvalue(identifyVariable) == "1"
#        .activeDataSet <- ActiveDataSet()
#        command <- paste("plot(", .activeDataSet, "$", x, ', type="', type, '")', sep="")
#        doItAndPrint(command)
#        if (par("usr")[3] <= 0) doItAndPrint('abline(h=0, col="gray")')
#        if (identify) {
#            RcmdrTkmessageBox(title="Identify Points",
#                message=paste(gettextRcmdr("Use left mouse button to identify points,\n"),
#						gettextRcmdr(if (MacOSXP()) "esc key to exit." else "right button to exit."), sep=""),
#                icon="info", type="ok")
#            command <- paste("identify(", .activeDataSet, "$", x,
#                ", labels=rownames(", .activeDataSet, "))", sep="")
#            doItAndPrint(command)
#            }
#        activateMenus()
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="plot")
#    optionsFrame <- tkframe(top)
#    typeVariable <- tclVar("spikes")
#    spikesButton <- ttkradiobutton(optionsFrame, variable=typeVariable, value="spikes")
#    pointsButton <- ttkradiobutton(optionsFrame, variable=typeVariable, value="points")
#    identifyVariable <- tclVar("0")
#    identifyCheckBox <- tkcheckbutton(optionsFrame, variable=identifyVariable)
#    tkgrid(getFrame(xBox), sticky="nw")
#    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Identify observations\nwith mouse"), justify="left"),
#        identifyCheckBox, sticky="w")
#    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Spikes")), spikesButton, sticky="w")
#    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Points")), pointsButton, sticky="w")
#    tkgrid(optionsFrame, sticky="w")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=2, columns=1)
#    }

indexPlot <- function () {
	defaults <- list(initial.x = NULL, initial.type = "spikes", initial.identify = 0) 
	dialog.values <- getDialog("indexPlot", defaults)
	initializeDialog(title = gettextRcmdr("Index Plot"))
	xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	onOK <- function() {
		x <- getSelection(xBox)
		initial.type <- type <- tclvalue(typeVariable)
		identify <- tclvalue(identifyVariable) == "1"
		putDialog ("indexPlot", list(initial.x = x, initial.type = type, initial.identify = identify))
		closeDialog()
		if (length(x) == 0) {
			errorCondition(recall = indexPlot, message = gettextRcmdr("You must select a variable"))
			return()
		}
		type <- if (tclvalue(typeVariable) == "spikes") 
					"h"
				else "p"
		.activeDataSet <- ActiveDataSet()
		command <- paste("plot(", .activeDataSet, "$", x, ", type=\"", 
				type, "\")", sep = "")
		doItAndPrint(command)
		if (par("usr")[3] <= 0) 
			doItAndPrint("abline(h=0, col=\"gray\")")
		if (identify) {
			RcmdrTkmessageBox(title = "Identify Points", message = paste(gettextRcmdr("Use left mouse button to identify points,\n"), 
							gettextRcmdr(if (MacOSXP()) 
												"esc key to exit."
											else "right button to exit."), sep = ""), icon = "info", 
					type = "ok")
			command <- paste("identify(", .activeDataSet, "$", 
					x, ", labels=rownames(", .activeDataSet, "))", 
					sep = "")
			doItAndPrint(command)
		}
		activateMenus()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "plot", reset = "indexPlot")
	optionsFrame <- tkframe(top)
	typeVariable <- tclVar(dialog.values$initial.type)
	spikesButton <- ttkradiobutton(optionsFrame, variable = typeVariable, 
			value = "spikes")
	pointsButton <- ttkradiobutton(optionsFrame, variable = typeVariable, 
			value = "points")
	identifyVariable <- tclVar(dialog.values$initial.identify)
	identifyCheckBox <- tkcheckbutton(optionsFrame, variable = identifyVariable)
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Identify observations\nwith mouse"), 
					justify = "left"), identifyCheckBox, sticky = "w")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Spikes")), 
			spikesButton, sticky = "w")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Points")), 
			pointsButton, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

#Histogram <- function(){
#    initializeDialog(title=gettextRcmdr("Histogram"))
#    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
#    onOK <- function(){
#        x <- getSelection(xBox)
#        closeDialog()
#        if (length(x) == 0){
#            errorCondition(recall=Histogram, message=gettextRcmdr("You must select a variable"))
#            return()
#            }
#        bins <- tclvalue(binsVariable)
#        opts <- options(warn=-1)
#        bins <- if (bins == gettextRcmdr("<auto>")) '"Sturges"' else as.numeric(bins)
#        options(opts)
#        scale <- tclvalue(scaleVariable)
#        command <- paste("Hist(", ActiveDataSet(), "$", x, ', scale="',
#            scale, '", breaks=', bins, ', col="darkgray")', sep="")
#        doItAndPrint(command)
#        activateMenus()
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="Hist")
#    radioButtons(name="scale", buttons=c("frequency", "percent", "density"),
#        labels=gettextRcmdr(c("Frequency counts", "Percentages", "Densities")), title=gettextRcmdr("Axis Scaling"))
#    binsFrame <- tkframe(top)
#    binsVariable <- tclVar(gettextRcmdr("<auto>"))
#    binsField <- ttkentry(binsFrame, width="8", textvariable=binsVariable)
#    tkgrid(getFrame(xBox), sticky="nw")
#    tkgrid(labelRcmdr(binsFrame, text=gettextRcmdr("Number of bins: ")), binsField, sticky="w")
#    tkgrid(binsFrame, sticky="w")
#    tkgrid(scaleFrame, sticky="w")
#    tkgrid(buttonsFrame, sticky="w")
#    tkgrid.configure(binsField, sticky="e")
#    dialogSuffix(rows=4, columns=1)
#    }

Histogram <- function () {
	defaults <- list(initial.x = NULL, initial.scale = "frequency", 
			initial.bins = gettextRcmdr ("<auto>")) 
	dialog.values <- getDialog("Histogram", defaults)
	initializeDialog(title = gettextRcmdr("Histogram"))
	xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	onOK <- function() {
		x <- getSelection(xBox)
		closeDialog()
		if (length(x) == 0) {
			errorCondition(recall = Histogram, message = gettextRcmdr("You must select a variable"))
			return()
		}
		bins <- tclvalue(binsVariable)
		opts <- options(warn = -1)
		binstext <- if (bins == gettextRcmdr("<auto>")) 
					"\"Sturges\""
				else as.numeric(bins)
		options(opts)
		scale <- tclvalue(scaleVariable)
		putDialog ("Histogram", list (initial.x = x, initial.bins = bins, initial.scale = scale))
		command <- paste("Hist(", ActiveDataSet(), "$", x, ", scale=\"", 
				scale, "\", breaks=", binstext, ", col=\"darkgray\")", 
				sep = "")
		doItAndPrint(command)
		activateMenus()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "Hist", reset = "Histogram")
	radioButtons(name = "scale", buttons = c("frequency", "percent", 
					"density"), labels = gettextRcmdr(c("Frequency counts", 
							"Percentages", "Densities")), title = gettextRcmdr("Axis Scaling"), 
			initialValue = dialog.values$initial.scale)
	binsFrame <- tkframe(top)
	binsVariable <- tclVar(dialog.values$initial.bins)
	binsField <- ttkentry(binsFrame, width = "8", textvariable = binsVariable)
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelRcmdr(binsFrame, text = gettextRcmdr("Number of bins: ")), 
			binsField, sticky = "w")
	tkgrid(binsFrame, sticky = "w")
	tkgrid(scaleFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	tkgrid.configure(binsField, sticky = "e")
	dialogSuffix(rows = 4, columns = 1)
}

#stemAndLeaf <- function(){
#	Library("aplpack")
#    initializeDialog(title=gettextRcmdr("Stem and Leaf Display"), preventCrisp=TRUE)
#    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
#    displayDigits <- tclVar("1")
#    onDigits <- function(...){
#        tclvalue(displayDigits) <- formatC(10^as.numeric(tclvalue(leafsDigitValue)),
#            format="fg", big.mark=",")
#        tclvalue(leafsAutoVariable) <- "0"
#        }
#    radioButtons(name="parts", buttons=c("auto", "one", "two", "five"),
#        values=c("auto", "1", "2", "5"), labels=c(gettextRcmdr("Automatic"), "   1", "   2", "   5"),
#        title=gettextRcmdr("Parts Per Stem"))
#    radioButtons(name="style", buttons=c("Tukey", "bare"), labels=gettextRcmdr(c("Tukey", "Repeated stem digits")),
#        title=gettextRcmdr("Style of Divided Stems"))
#    checkBoxes(frame="optionsFrame", boxes=c("trimOutliers", "showDepths", "reverseNegative"),
#        initialValues=rep(1, 3), labels=gettextRcmdr(c("Trim outliers", "Show depths", "Reverse negative leaves")))
#    leafsFrame <- tkframe(top)
#    leafsDigitValue <- tclVar("0")
#    leafsDigitSlider <- tkscale(leafsFrame, from=-6, to=6, showvalue=FALSE, variable=leafsDigitValue,
#        resolution=1, orient="horizontal", command=onDigits)
#    leafsDigitShow <- labelRcmdr(leafsFrame, textvariable=displayDigits, width=8, justify="right")
#    leafsAutoVariable <- tclVar("1")
#    leafsDigitCheckBox <- tkcheckbutton(leafsFrame, variable=leafsAutoVariable)
#    onOK <- function(){
#        x <- getSelection(xBox)
#        closeDialog()
#        if (length(x) == 0){
#            errorCondition(recall=stemAndLeaf, message=gettextRcmdr("You must select a variable"))
#            return()
#            }
#        unit <- if (tclvalue(leafsAutoVariable) == "1") ""
#            else paste(", unit=", 10^as.numeric(tclvalue(leafsDigitValue)), sep="")
#        m <- if (tclvalue(partsVariable) == "auto") ""
#            else paste(", m=", tclvalue(partsVariable), sep="")
#        trim <- if (tclvalue(trimOutliersVariable) == "1") ""
#            else ", trim.outliers=FALSE"
#        depths <- if (tclvalue(showDepthsVariable) == "1") ""
#            else ", depths=FALSE"
#        reverse <- if (tclvalue(reverseNegativeVariable) == "1") ""
#            else ", reverse.negative.leaves=FALSE"
#        style <- if (tclvalue(styleVariable) == "Tukey") ""
#            else ', style="bare"'
#        command <- paste("stem.leaf(", ActiveDataSet(), "$", x, style, unit, m, trim,
#            depths, reverse, ", na.rm=TRUE)", sep="")
#        doItAndPrint(command)
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="stem.leaf")
#    tkgrid(getFrame(xBox), sticky="nw")
#    tkgrid(labelRcmdr(leafsFrame, text=gettextRcmdr("Leafs Digit:  "), fg="blue"),
#        labelRcmdr(leafsFrame, text=gettextRcmdr("Automatic")), leafsDigitCheckBox,
#        labelRcmdr(leafsFrame, text=gettextRcmdr("  or set:"), fg="red"), leafsDigitShow, leafsDigitSlider, sticky="w")
#    tkgrid(leafsFrame, sticky="w")
#    tkgrid(partsFrame, sticky="w")
#    tkgrid(styleFrame, sticky="w")
#    tkgrid(labelRcmdr(top, text=gettextRcmdr("Options"), fg="blue"), sticky="w")
#    tkgrid(optionsFrame, sticky="w")
#    tkgrid(buttonsFrame, sticky="w")
#    tclvalue(leafsAutoVariable) <- "1"
#    dialogSuffix(rows=7, columns=1, preventCrisp=TRUE)
#    }

stemAndLeaf <- function () {
	Library("aplpack")
	defaults <- list(initial.x = NULL, initial.leafs.auto="1", initial.unit = 0,  initial.m = "auto", 
			initial.trim = 1, initial.depths = 1, initial.reverse = 1, initial.style = "Tukey") 
	dialog.values <- getDialog("stemAndLeaf", defaults)
	initializeDialog(title = gettextRcmdr("Stem and Leaf Display"), 
			preventCrisp = TRUE)
	xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	displayDigits <- tclVar(formatC(10^dialog.values$initial.unit))
	leafsDigitValue <- tclVar(dialog.values$initial.unit)
	onDigits <- function(...) {
		tclvalue(displayDigits) <- formatC(10^as.numeric(tclvalue(leafsDigitValue)), 
				format = "fg", big.mark = ",")
		tclvalue(leafsAutoVariable) <- "0"
	}
	radioButtons(name = "parts", buttons = c("auto", "one", "two", 
					"five"), values = c("auto", "1", "2", "5"), labels = c(gettextRcmdr("Automatic"), 
					"   1", "   2", "   5"), title = gettextRcmdr("Parts Per Stem"), 
			initialValue = dialog.values$initial.m)
	radioButtons(name = "style", buttons = c("Tukey", "bare"), 
			labels = gettextRcmdr(c("Tukey", "Repeated stem digits")), 
			title = gettextRcmdr("Style of Divided Stems"), 
			initialValue = dialog.values$initial.style)
	checkBoxes(frame = "optionsFrame", boxes = c("trimOutliers", 
					"showDepths", "reverseNegative"), initialValues = c(dialog.values$initial.trim,
					dialog.values$initial.depths, dialog.values$initial.reverse),
			labels = gettextRcmdr(c("Trim outliers", "Show depths", 
							"Reverse negative leaves")))
	leafsFrame <- tkframe(top)
	leafsDigitValue <- tclVar(dialog.values$initial.unit) #tclVar("0")
	leafsDigitSlider <- tkscale(leafsFrame, from = -6, to = 6, 
			showvalue = FALSE, variable = leafsDigitValue, resolution = 1, 
			orient = "horizontal", command = onDigits)
	leafsDigitShow <- labelRcmdr(leafsFrame, textvariable = displayDigits, 
			width = 8, justify = "right")
	leafsAutoVariable <- tclVar("1") # tclVar(dialog.values$initial.leafs.auto)
	leafsDigitCheckBox <- tkcheckbutton(leafsFrame, variable = leafsAutoVariable)
	onOK <- function() {
		x <- getSelection(xBox)
		m <- tclvalue(partsVariable)
		style <- tclvalue (styleVariable)
		trim <- tclvalue (trimOutliersVariable)
		depths <- tclvalue (showDepthsVariable)
		reverse <- tclvalue (reverseNegativeVariable)
		unit <- if (tclvalue(leafsAutoVariable) == "1") 
					""
				else paste(", unit=", 10^as.numeric(tclvalue(leafsDigitValue)), 
							sep = "")
		putDialog ("stemAndLeaf", list(initial.x = x, initial.leafs.auto=tclvalue(leafsAutoVariable),
						initial.unit = as.numeric(tclvalue(leafsDigitValue)),  initial.m = m, 
						initial.trim = trim, initial.depths = depths, initial.reverse = reverse, 
						initial.style = style))
		closeDialog()
		if (length(x) == 0) {
			errorCondition(recall = stemAndLeaf, message = gettextRcmdr("You must select a variable"))
			return()
		}
		trim <- if (tclvalue(trimOutliersVariable) == "1") 
					""
				else ", trim.outliers=FALSE"
		depths <- if (tclvalue(showDepthsVariable) == "1") 
					""
				else ", depths=FALSE"
		reverse <- if (tclvalue(reverseNegativeVariable) == "1") 
					""
				else ", reverse.negative.leaves=FALSE"
		m <- if (tclvalue(partsVariable) == "auto") 
					""
				else paste(", m=", tclvalue(partsVariable), sep = "")
		style <- if (tclvalue(styleVariable) == "Tukey") 
					""
				else ", style=\"bare\""
		command <- paste("stem.leaf(", ActiveDataSet(), "$", 
				x, style, unit, m, trim, depths, reverse, ", na.rm=TRUE)", 
				sep = "")
		doItAndPrint(command)
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "stem.leaf", reset = "stemAndLeaf")
	tkgrid(getFrame(xBox), sticky = "nw")
	tkgrid(labelRcmdr(leafsFrame, text = gettextRcmdr("Leafs Digit:  "), 
					fg = "blue"), labelRcmdr(leafsFrame, text = gettextRcmdr("Automatic")), 
			leafsDigitCheckBox, labelRcmdr(leafsFrame, text = gettextRcmdr("  or set:"), 
					fg = "red"), leafsDigitShow, leafsDigitSlider, sticky = "w")
	tkgrid(leafsFrame, sticky = "w")
	tkgrid(partsFrame, sticky = "w")
	tkgrid(styleFrame, sticky = "w")
	tkgrid(labelRcmdr(top, text = gettextRcmdr("Options"), fg = "blue"), 
			sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	tclvalue(leafsAutoVariable) <- dialog.values$initial.leafs.auto
	dialogSuffix(rows = 7, columns = 1, preventCrisp = TRUE)
}

boxPlot <- function(){
    initializeDialog(title=gettextRcmdr("Boxplot"))
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    identifyCheckBox <- tkcheckbutton(identifyFrame, variable=identifyVariable)
    .groups <- FALSE
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
        if (length(x) == 0){
            errorCondition(recall=boxPlot, message=gettextRcmdr("You must select a variable"))
            return()
            }
        identifyPoints <- "1" == tclvalue(identifyVariable)
        .activeDataSet <- ActiveDataSet()
        var <- paste(.activeDataSet, "$", x, sep="")
        if (.groups == FALSE) {
            command <- (paste("boxplot(", var, ', ylab="', x, '")', sep=""))
            logger(command)
            justDoIt(command)
            if (identifyPoints) {
                RcmdrTkmessageBox(title="Identify Points",
						message=paste(gettextRcmdr("Use left mouse button to identify points,\n"),
							gettextRcmdr(if (MacOSXP()) "esc key to exit." else "right button to exit."), sep=""),
                    icon="info", type="ok")
                doItAndPrint(paste("identify(rep(1, length(", var,
                    ")), ", var, ", rownames(", .activeDataSet,"))", sep=""))
                }
            }
        else {
            command <- (paste("boxplot(", x, "~", .groups, ', ylab="', x,
                '", xlab="', .groups,'"',
                ", data=", .activeDataSet, ")", sep=""))
            logger(command)
            justDoIt(command)
            if (identifyPoints) {
                RcmdrTkmessageBox(title="Identify Points",
						message=paste(gettextRcmdr("Use left mouse button to identify points,\n"),
							gettextRcmdr(if (MacOSXP()) "esc key to exit." else "right button to exit."), sep=""),
                    icon="info", type="ok")
                doItAndPrint(paste("identify(", .activeDataSet, "$", .groups, ", ", var,
                    ", rownames(", .activeDataSet,"))", sep=""))
                }
            }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    groupsBox(boxPlot)
    OKCancelHelp(helpSubject="boxplot")
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(labelRcmdr(identifyFrame, text=gettextRcmdr("Identify outliers with mouse"), justify="left"),
        identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, stick="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=4, columns=1)
    }

scatterPlot <- function(){
    require("car")
    initializeDialog(title=gettextRcmdr("Scatterplot"))
    .numeric <- Numeric()
    variablesFrame <- tkframe(top)
    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("x-variable (pick one)"))
    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("y-variable (pick one)"))
    optionsParFrame <- tkframe(top)
    checkBoxes(window=optionsParFrame, frame="optionsFrame", 
		boxes=c("identify", "jitterX", "jitterY", "logX", "logY", "boxplots", "lsLine", "smoothLine", "spread"),
        initialValues=c(0, 0, 0, 0, 0, 1, 1, 1, 1), 
		labels=gettextRcmdr(c("Identify points", "Jitter x-variable", "Jitter y-variable", "Log x-axis", "Log y-axis",
        "Marginal boxplots", "Least-squares line", "Smooth line", "Show spread")), title="Options")
    sliderValue <- tclVar("50")
    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
        resolution=5, orient="horizontal")
    subsetBox()
    labelsFrame <- tkframe(top)
    xlabVar <- tclVar(gettextRcmdr("<auto>"))
    ylabVar <- tclVar(gettextRcmdr("<auto>"))
    xlabFrame <- tkframe(labelsFrame)
    xlabEntry <- ttkentry(xlabFrame, width="25", textvariable=xlabVar)
    xlabScroll <- ttkscrollbar(xlabFrame, orient="horizontal",
        command=function(...) tkxview(xlabEntry, ...))
    tkconfigure(xlabEntry, xscrollcommand=function(...) tkset(xlabScroll, ...))
    tkgrid(labelRcmdr(xlabFrame, text=gettextRcmdr("x-axis label"), fg="blue"), sticky="w")
    tkgrid(xlabEntry, sticky="w")
    tkgrid(xlabScroll, sticky="ew")
    ylabFrame <- tkframe(labelsFrame)
    ylabEntry <- ttkentry(ylabFrame, width="25", textvariable=ylabVar)
    ylabScroll <- ttkscrollbar(ylabFrame, orient="horizontal",
        command=function(...) tkxview(ylabEntry, ...))
    tkconfigure(ylabEntry, xscrollcommand=function(...) tkset(ylabScroll, ...))
    tkgrid(labelRcmdr(ylabFrame, text=gettextRcmdr("y-axis label"), fg="blue"), sticky="w")
    tkgrid(ylabEntry, sticky="w")
    tkgrid(ylabScroll, sticky="ew")
    tkgrid(xlabFrame, labelRcmdr(labelsFrame, text="     "), ylabFrame, sticky="w")
    parFrame <- tkframe(optionsParFrame)
    pchVar <- tclVar(gettextRcmdr("<auto>"))
    pchEntry <- ttkentry(parFrame, width=25, textvariable=pchVar)
    cexValue <- tclVar("1")
    cex.axisValue <- tclVar("1")
    cex.labValue <- tclVar("1")
    cexSlider <- tkscale(parFrame, from=0.5, to=2.5, showvalue=TRUE, variable=cexValue,
        resolution=0.1, orient="horizontal")
    cex.axisSlider <- tkscale(parFrame, from=0.5, to=2.5, showvalue=TRUE, variable=cex.axisValue,
        resolution=0.1, orient="horizontal")
    cex.labSlider <- tkscale(parFrame, from=0.5, to=2.5, showvalue=TRUE, variable=cex.labValue,
        resolution=0.1, orient="horizontal")
    onOK <- function(){
        x <- getSelection(xBox)
        y <- getSelection(yBox)
        closeDialog()
        if (length(x) == 0 || length(y) == 0){
            errorCondition(recall=scatterPlot, message=gettextRcmdr("You must select two variables"))
            return()
            }
        if (x == y) {
            errorCondition(recall=scatterPlot, message=gettextRcmdr("x and y variables must be different"))
            return()
            }
        .activeDataSet <- ActiveDataSet()
        jitter <- if ("1" == tclvalue(jitterXVariable) && "1" == tclvalue(jitterYVariable)) ", jitter=list(x=1, y=1)"
            else if ("1" == tclvalue(jitterXVariable)) ", jitter=list(x=1)"
            else if ("1" == tclvalue(jitterYVariable)) ", jitter=list(y=1)"
            else ""
		logstring <- ""
		if ("1" == tclvalue(logXVariable)) logstring <- paste(logstring, "x", sep="")
		if ("1" == tclvalue(logYVariable)) logstring <- paste(logstring, "y", sep="")
		log <- if(logstring != "") paste(', log="', logstring, '"', sep="") else ""
		if("1" == tclvalue(identifyVariable)){
			RcmdrTkmessageBox(title="Identify Points",
					message=paste(gettextRcmdr("Use left mouse button to identify points,\n"),
						gettextRcmdr(if (MacOSXP()) "esc key to exit." else "right button to exit."), sep=""),
					icon="info", type="ok")
			idtext <- ', id.method="identify"'
		}
        else idtext <- ""
        box <- if ("1" == tclvalue(boxplotsVariable)) "'xy'" else "FALSE"
        line <- if("1" == tclvalue(lsLineVariable)) "lm" else "FALSE"
        smooth <- as.character("1" == tclvalue(smoothLineVariable))
		spread <- as.character("1" == tclvalue(spreadVariable))
        span <- as.numeric(tclvalue(sliderValue))
        subset <- tclvalue(subsetVariable)
        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
            else paste(", subset=", subset, sep="")
        xlab <- trim.blanks(tclvalue(xlabVar))
        xlab <- if(xlab == gettextRcmdr("<auto>")) "" else paste(', xlab="', xlab, '"', sep="")
        ylab <- trim.blanks(tclvalue(ylabVar))
        ylab <- if(ylab == gettextRcmdr("<auto>")) "" else paste(', ylab="', ylab, '"', sep="")
        cex <- as.numeric(tclvalue(cexValue))
        cex <- if(cex == 1) "" else paste(', cex=', cex, sep="")
        cex.axis <- as.numeric(tclvalue(cex.axisValue))
        cex.axis <- if(cex.axis == 1) "" else paste(', cex.axis=', cex.axis, sep="")
        cex.lab <- as.numeric(tclvalue(cex.labValue))
        cex.lab <- if(cex.lab == 1) "" else paste(', cex.lab=', cex.lab, sep="")
        pch <- gsub(" ", ",", tclvalue(pchVar))
        if ("" == pch) {
            errorCondition(recall=scatterPlot, message=gettextRcmdr("No plotting characters."))
            return()
            }
        pch <- if(trim.blanks(pch) == gettextRcmdr("<auto>")) "" else paste(", pch=c(", pch, ")", sep="")
        if (.groups == FALSE) {
            doItAndPrint(paste("scatterplot(", y, "~", x, log,
                ", reg.line=", line, ", smooth=", smooth, ", spread=", spread, idtext,
                ", boxplots=", box, ", span=", span/100, jitter, xlab, ylab,
                cex, cex.axis, cex.lab, pch,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        else {
            doItAndPrint(paste("scatterplot(", y, "~", x," | ", .groups,
                ", reg.line=", line, ", smooth=", smooth, ", spread=", spread, idtext,
                ", boxplots=", box, ", span=", span/100, jitter, xlab, ylab,
                cex, cex.axis, cex.lab, pch,
                ", by.groups=", .linesByGroup,
                ", data=", .activeDataSet, subset, ")", sep=""))
            }
        activateMenus()
        tkfocus(CommanderWindow())
        }
    groupsBox(scatterPlot, plotLinesByGroup=TRUE)
    OKCancelHelp(helpSubject="scatterplot")
    tkgrid(getFrame(xBox), getFrame(yBox), sticky="nw")
    tkgrid(variablesFrame, sticky="w")
    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Span for smooth")), slider, sticky="w")
    tkgrid(labelRcmdr(parFrame, text=gettextRcmdr("Plotting Parameters"), fg="blue"), sticky="w")
    tkgrid(labelRcmdr(parFrame, text=gettextRcmdr("Plotting characters")), pchEntry, stick="w")
    tkgrid(labelRcmdr(parFrame, text=gettextRcmdr("Point size")), cexSlider, sticky="w")
    tkgrid(labelRcmdr(parFrame, text=gettextRcmdr("Axis text size")), cex.axisSlider, sticky="w")
    tkgrid(labelRcmdr(parFrame, text=gettextRcmdr("Axis-labels text size")), cex.labSlider, sticky="w")
    tkgrid(optionsFrame, parFrame, sticky="nw")
    tkgrid(optionsParFrame, sticky="w")
    tkgrid(labelsFrame, sticky="w")
    tkgrid(subsetFrame, sticky="w")
    tkgrid(groupsFrame, sticky="w")
    tkgrid(labelRcmdr(top, text=" "))
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    dialogSuffix(rows=8, columns=2)
    }

#scatterPlotMatrix <- function(){
#    require("car")
#    initializeDialog(title=gettextRcmdr("Scatterplot Matrix"))
#    variablesBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Select variables (three or more)"),
#        selectmode="multiple", initialSelection=NULL)
#    checkBoxes(frame="optionsFrame", boxes=c("lsLine", "smoothLine", "spread"), initialValues=c(1,1,0),
#        labels=gettextRcmdr(c("Least-squares lines", "Smooth lines", "Show spread")))
#    sliderValue <- tclVar("50")
#    slider <- tkscale(optionsFrame, from=0, to=100, showvalue=TRUE, variable=sliderValue,
#        resolution=5, orient="horizontal")
#    radioButtons(name="diagonal", buttons=c("density", "histogram", "boxplot", "oned", "qqplot", "none"),
#        labels=gettextRcmdr(c("Density plots", "Histograms", "Boxplots", "One-dimensional scatterplots", "Normal QQ plots", "Nothing (empty)")),
#        title=gettextRcmdr("On Diagonal"))
#    subsetBox()
#    onOK <- function(){
#        variables <- getSelection(variablesBox)
#        closeDialog()
#        if (length(variables) < 3) {
#            errorCondition(recall=scatterPlotMatrix, message=gettextRcmdr("Fewer than 3 variable selected."))
#            return()
#            }
#        line <- if("1" == tclvalue(lsLineVariable)) "lm" else "FALSE"
#        smooth <- as.character("1" == tclvalue(smoothLineVariable))
#		spread <- as.character("1" == tclvalue(spreadVariable))
#        span <- as.numeric(tclvalue(sliderValue))
#        diag <- as.character(tclvalue(diagonalVariable))
#        subset <- tclvalue(subsetVariable)
#        subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
#            else paste(", subset=", subset, sep="")
#        .activeDataSet <- ActiveDataSet()
#        if (.groups == FALSE) {
#           command <- paste("scatterplotMatrix(~", paste(variables, collapse="+"),
#                ", reg.line=", line, ", smooth=", smooth, ", spread=", spread,
#                ", span=", span/100, ", diagonal = '", diag,
#                "', data=", .activeDataSet, subset, ")", sep="")
#           logger(command)
#           justDoIt(command)
#            }
#        else {
#            command <- paste("scatterplotMatrix(~", paste(variables, collapse="+")," | ", .groups,
#                ", reg.line=", line, ", smooth=", smooth, ", spread=", spread,
#                ", span=", span/100, ", diagonal= '", diag,
#                "', by.groups=", .linesByGroup,
#                ", data=", .activeDataSet, subset, ")", sep="")
#            logger(command)
#            justDoIt(command)
#            }
#        activateMenus()
#        tkfocus(CommanderWindow())
#        }
#    groupsBox(scatterPlot, plotLinesByGroup=TRUE)
#    OKCancelHelp(helpSubject="scatterplotMatrix")
#    tkgrid(getFrame(variablesBox), sticky="nw")
#	tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Span for smooth")), slider, sticky="w")    
#	tkgrid(optionsFrame, sticky="w")
#    tkgrid(diagonalFrame, sticky="w")
#    tkgrid(subsetFrame, sticky="w")
#    tkgrid(groupsFrame, sticky="w")
#    tkgrid(buttonsFrame, columnspan=2, sticky="w")
#    dialogSuffix(rows=6, columns=2)
#    }

scatterPlotMatrix <- function () {
	require("car")
	defaults <- list(initial.variables = NULL, initial.line = 1, initial.smooth = 1, initial.spread = 0, 
			initial.span = 50, initial.diag = "density", initial.subset = gettextRcmdr ("<all valid cases>")) 
	dialog.values <- getDialog("scatterPlotMatrix", defaults)
	initializeDialog(title = gettextRcmdr("Scatterplot Matrix"))
	variablesBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Select variables (three or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.variables, "numeric"))
	checkBoxes(frame = "optionsFrame", boxes = c("lsLine", "smoothLine", 
					"spread"), initialValues = c(dialog.values$initial.line, dialog.values$initial.smooth,
					dialog.values$initial.spread), labels = gettextRcmdr(c("Least-squares lines", 
							"Smooth lines", "Show spread")))
	sliderValue <- tclVar(dialog.values$initial.span)
	slider <- tkscale(optionsFrame, from = 0, to = 100, showvalue = TRUE, 
			variable = sliderValue, resolution = 5, orient = "horizontal")
	radioButtons(name = "diagonal", buttons = c("density", "histogram", 
					"boxplot", "oned", "qqplot", "none"), labels = gettextRcmdr(c("Density plots", 
							"Histograms", "Boxplots", "One-dimensional scatterplots", 
							"Normal QQ plots", "Nothing (empty)")), title = gettextRcmdr("On Diagonal"), 
			initialValue = dialog.values$initial.diag)
	subsetBox(subset.expression = dialog.values$initial.subset)
	onOK <- function() {
		variables <- getSelection(variablesBox)
		closeDialog()
		if (length(variables) < 3) {
			errorCondition(recall = scatterPlotMatrix, message = gettextRcmdr("Fewer than 3 variable selected."))
			return()
		}
		line <- if ("1" == tclvalue(lsLineVariable)) 
					"lm"
				else "FALSE"
		smooth <- as.character("1" == tclvalue(smoothLineVariable))
		spread <- as.character("1" == tclvalue(spreadVariable))
		span <- as.numeric(tclvalue(sliderValue))
		diag <- as.character(tclvalue(diagonalVariable))
		initial.subset <- subset <- tclvalue(subsetVariable)
		subset <- if (trim.blanks(subset) == gettextRcmdr("<all valid cases>")) ""
				else paste(", subset=", subset, sep="")
		putDialog("scatterPlotMatrix", list(initial.variables = variables, initial.line = tclvalue (lsLineVariable), 
						initial.smooth = tclvalue(smoothLineVariable),initial.spread = tclvalue (spreadVariable), 
						initial.span = span, initial.diag = diag, initial.subset = initial.subset))
		.activeDataSet <- ActiveDataSet()
		if (.groups == FALSE) {
			command <- paste("scatterplotMatrix(~", paste(variables, 
							collapse = "+"), ", reg.line=", line, ", smooth=", 
					smooth, ", spread=", spread, ", span=", span/100, 
					", diagonal = '", diag, "', data=", .activeDataSet, 
					subset, ")", sep = "")
			logger(command)
			justDoIt(command)
		}
		else {
			command <- paste("scatterplotMatrix(~", paste(variables, 
							collapse = "+"), " | ", .groups, ", reg.line=", 
					line, ", smooth=", smooth, ", spread=", spread, 
					", span=", span/100, ", diagonal= '", diag, "', by.groups=", 
					.linesByGroup, ", data=", .activeDataSet, subset, 
					")", sep = "")
			logger(command)
			justDoIt(command)
		}
		activateMenus()
		tkfocus(CommanderWindow())
	}
	groupsBox(scatterPlot, plotLinesByGroup = TRUE)
	OKCancelHelp(helpSubject = "scatterplotMatrix", reset = "scatterPlotMatrix")
	tkgrid(getFrame(variablesBox), sticky = "nw")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Span for smooth")), 
			slider, sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(diagonalFrame, sticky = "w")
	tkgrid(subsetFrame, sticky = "w")
	tkgrid(groupsFrame, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 6, columns = 2)
}

#barGraph <- function(){
#    initializeDialog(title=gettextRcmdr("Bar Graph"))
#    variableBox <- variableListBox(top, Factors(), title=gettextRcmdr("Variable (pick one)"))
#    onOK <- function(){
#        variable <- getSelection(variableBox)
#        closeDialog()
#        if (length(variable) == 0){
#            errorCondition(recall=barGraph, message=gettextRcmdr("You must select a variable"))
#            return()
#            }
#        command <- paste("barplot(table(", ActiveDataSet(), "$", variable, '), xlab="',
#            variable, '", ylab="Frequency")', sep="")
#        logger(command)
#        justDoIt(command)
#        activateMenus()
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="barplot")
#    tkgrid(getFrame(variableBox), sticky="nw")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=2, columns=1)
#    }

barGraph <- function () {
	defaults <- list (initial.variable = NULL)
	dialog.values <- getDialog ("barGraph", defaults)
	initializeDialog(title = gettextRcmdr("Bar Graph"))
	variableBox <- variableListBox(top, Factors(), title = gettextRcmdr("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.variable, "factor"))
	onOK <- function() {
		variable <- getSelection(variableBox)
		putDialog ("barGraph", list (initial.variable = variable))
		closeDialog()
		if (length(variable) == 0) {
			errorCondition(recall = barGraph, message = gettextRcmdr("You must select a variable"))
			return()
		}
		command <- paste("barplot(table(", ActiveDataSet(), "$", 
				variable, "), xlab=\"", variable, "\", ylab=\"Frequency\")", 
				sep = "")
		logger(command)
		justDoIt(command)
		activateMenus()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "barplot", reset = "barGraph")
	tkgrid(getFrame(variableBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 2, columns = 1)
}

#pieChart <- function(){
#	Library("colorspace")
#    initializeDialog(title=gettextRcmdr("Pie Chart"))
#    variableBox <- variableListBox(top, Factors(), title=gettextRcmdr("Variable (pick one)"))
#    onOK <- function(){
#        variable <- getSelection(variableBox)
#        closeDialog()
#        if (length(variable) == 0){
#            errorCondition(recall=pieChart, message=gettextRcmdr("You must select a variable"))
#            return()
#            }
#        .activeDataSet <- ActiveDataSet()
#        command <- (paste("pie(table(", .activeDataSet, "$", variable, "), labels=levels(",
#            .activeDataSet, "$", variable, '), main="', variable, '", col=rainbow_hcl(length(levels(',
#            .activeDataSet, "$", variable, "))))", sep=""))
#        logger(command)
#        justDoIt(command)
#        activateMenus()
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="pie")
#    tkgrid(getFrame(variableBox), sticky="nw")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=3, columns=1)
#    }

pieChart <- function () {
	Library("colorspace")
	defaults <- list (initial.variable = NULL)
	dialog.values <- getDialog ("pieChart", defaults)
	initializeDialog(title = gettextRcmdr("Pie Chart"))
	variableBox <- variableListBox(top, Factors(), title = gettextRcmdr("Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.variable, "factor"))
	onOK <- function() {
		variable <- getSelection(variableBox)
		putDialog ("pieChart", list (initial.variable = variable))
		closeDialog()
		if (length(variable) == 0) {
			errorCondition(recall = pieChart, message = gettextRcmdr("You must select a variable"))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		command <- (paste("pie(table(", .activeDataSet, "$", 
							variable, "), labels=levels(", .activeDataSet, "$", 
							variable, "), main=\"", variable, "\", col=rainbow_hcl(length(levels(", 
							.activeDataSet, "$", variable, "))))", sep = ""))
		logger(command)
		justDoIt(command)
		activateMenus()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "pie", reset = "pieChart")
	tkgrid(getFrame(variableBox), sticky = "nw")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

#linePlot <- function(){
#    initializeDialog(title=gettextRcmdr("Line Plot"))
#    variablesFrame <- tkframe(top)
#    .numeric <- Numeric()
#    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("x variable (pick one)"))
#    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("y variables (pick one or more)"),
#        selectmode="multiple", initialSelection=NULL)
#    axisLabelVariable <- tclVar(gettextRcmdr("<use y-variable names>"))
#    axisLabelFrame <- tkframe(top)
#    axisLabelEntry <- ttkentry(axisLabelFrame, width="40", textvariable=axisLabelVariable)
#    axisLabelScroll <- ttkscrollbar(axisLabelFrame, orient="horizontal",
#        command=function(...) tkxview(axisLabelEntry, ...))
#    tkconfigure(axisLabelEntry, xscrollcommand=function(...) tkset(axisLabelScroll, ...))
#    legendFrame <- tkframe(top)
#    legendVariable <- tclVar("0")
#    legendCheckBox <- tkcheckbutton(legendFrame, variable=legendVariable)
#    onOK <- function(){
#        y <- getSelection(yBox)
#        x <- getSelection(xBox)
#        closeDialog()
#        if (0 == length(x)) {
#            errorCondition(recall=linePlot, message=gettextRcmdr("No x variable selected."))
#            return()
#            }
#        if (0 == length(y)) {
#            errorCondition(recall=linePlot, message=gettextRcmdr("No y variables selected."))
#            return()
#            }
#        if (is.element(x, y)) {
#            errorCondition(recall=linePlot, message=gettextRcmdr("x and y variables must be different."))
#            return()
#            }
#        .activeDataSet <- ActiveDataSet()
#        .x <- na.omit(eval(parse(text=paste(.activeDataSet, "$", x, sep="")), envir=.GlobalEnv))
#        if (!identical(order(.x), seq(along.with=.x))){
#            response <- tclvalue(RcmdrTkmessageBox(message=gettextRcmdr("x-values are not in order.\nContinue?"),
#                icon="warning", type="okcancel", default="cancel"))
#            if (response == "cancel") {
#                onCancel()
#                return()
#                }
#            }
#        axisLabel <- tclvalue(axisLabelVariable)
#        legend <- tclvalue(legendVariable) == "1"
#        if (axisLabel == gettextRcmdr("<use y-variable names>")){
#            axisLabel <- if (legend) ""
#                else if(length(y) == 1) y
#                else paste(paste("(", 1:length(y), ") ", y, sep=""), collapse=", ")
#            }
#        pch <- if (length(y) == 1) ", pch=1" else ""
#        if (legend && length(y) > 1){
#            mar <- par("mar")
#            top <- 3.5 + length(y)
#            command <- paste(".mar <- par(mar=c(", mar[1], ",", mar[2], ",", top, ",", mar[4], "))", sep="")
#            logger(command)
#            justDoIt(command)
#            }
#        command <- paste("matplot(", .activeDataSet, "$", x, ", ", .activeDataSet, "[, ",
#            paste("c(", paste(paste('"', y, '"', sep=""), collapse=","), ")", sep=""),
#            '], type="b", lty=1, ylab="', axisLabel, '"', pch, ")", sep="")
#        logger(command)
#        justDoIt(command)
#        if (legend && length(y) > 1){
#            n <- length(y)
#            cols <- rep(1:6, 1 + n %/% 6)[1:n]
#            logger(".xpd <- par(xpd=TRUE)")
#            justDoIt(".xpd <- par(xpd=TRUE)")
#            usr <- par("usr")
#            command <- paste("legend(", usr[1], ", ", usr[4] + 1.2*top*strheight("x"), ", legend=",
#                paste("c(", paste(paste('"', y, '"', sep=""), collapse=","), ")", sep=""),
#                ", col=c(", paste(cols, collapse=","), "), lty=1, pch=c(",
#                paste(paste('"', as.character(1:n), '"', sep=""), collapse=","), "))", sep="")
#            logger(command)
#            justDoIt(command)
#            logger("par(mar=.mar)")
#            justDoIt("par(mar=.mar)")
#            logger("par(xpd=.xpd)")
#            justDoIt("par(xpd=.xpd)")
#            }
#        activateMenus()
#        tkfocus(CommanderWindow())
#        }
#    OKCancelHelp(helpSubject="matplot")
#    tkgrid(getFrame(xBox), labelRcmdr(variablesFrame, text="    "), getFrame(yBox), sticky="nw")
#    tkgrid(variablesFrame, sticky="nw")
#    tkgrid(labelRcmdr(axisLabelFrame, text=gettextRcmdr("Label for y-axis"), fg="blue"), sticky="w")
#    tkgrid(axisLabelEntry, sticky="w")
#    tkgrid(axisLabelScroll, sticky="ew")
#    tkgrid(axisLabelFrame, sticky="w")
#    tkgrid(labelRcmdr(legendFrame, text=gettextRcmdr("Plot legend")),
#        legendCheckBox, sticky="w")
#    tkgrid(legendFrame, sticky="w")
#    tkgrid(buttonsFrame, stick="w")
#    dialogSuffix(rows=4, columns=1)
#    }

linePlot <- function () {
	defaults <- list(initial.x = NULL, initial.y = NULL, initial.axisLabel = gettextRcmdr ("<use y-variable names>"), 
			initial.legend = 0) 
	dialog.values <- getDialog("linePlot", defaults)
	initializeDialog(title = gettextRcmdr("Line Plot"))
	variablesFrame <- tkframe(top)
	.numeric <- Numeric()
	xBox <- variableListBox(variablesFrame, .numeric, title = gettextRcmdr("x variable (pick one)"),
			initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	yBox <- variableListBox(variablesFrame, .numeric, title = gettextRcmdr("y variables (pick one or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.y, "numeric"))
	axisLabelVariable <- tclVar(dialog.values$initial.axisLabel)
	axisLabelFrame <- tkframe(top)
	axisLabelEntry <- ttkentry(axisLabelFrame, width = "40", 
			textvariable = axisLabelVariable)
	axisLabelScroll <- ttkscrollbar(axisLabelFrame, orient = "horizontal", 
			command = function(...) tkxview(axisLabelEntry, ...))
	tkconfigure(axisLabelEntry, xscrollcommand = function(...) tkset(axisLabelScroll, 
						...))
	legendFrame <- tkframe(top)
	legendVariable <- tclVar(dialog.values$initial.legend)
	legendCheckBox <- tkcheckbutton(legendFrame, variable = legendVariable)
	onOK <- function() {
		y <- getSelection(yBox)
		x <- getSelection(xBox)
		closeDialog()
		if (0 == length(x)) {
			errorCondition(recall = linePlot, message = gettextRcmdr("No x variable selected."))
			return()
		}
		if (0 == length(y)) {
			errorCondition(recall = linePlot, message = gettextRcmdr("No y variables selected."))
			return()
		}
		if (is.element(x, y)) {
			errorCondition(recall = linePlot, message = gettextRcmdr("x and y variables must be different."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		.x <- na.omit(eval(parse(text = paste(.activeDataSet, 
										"$", x, sep = "")), envir = .GlobalEnv))
		if (!identical(order(.x), seq(along.with = .x))) {
			response <- tclvalue(RcmdrTkmessageBox(message = gettextRcmdr("x-values are not in order.\nContinue?"), 
							icon = "warning", type = "okcancel", default = "cancel"))
			if (response == "cancel") {
				onCancel()
				return()
			}
		}
		axisLabel <- tclvalue(axisLabelVariable)
		legend <- tclvalue(legendVariable) == "1"
		putDialog ("linePlot", list(initial.x = x, initial.y = y, initial.axisLabel = axisLabel, 
						initial.legend = legend))
		if (axisLabel == gettextRcmdr("<use y-variable names>")) {
			axisLabel <- if (legend) 
						""
					else if (length(y) == 1) 
						y
					else paste(paste("(", 1:length(y), ") ", y, sep = ""), 
								collapse = ", ")
		}
		pch <- if (length(y) == 1) 
					", pch=1"
				else ""
		if (legend && length(y) > 1) {
			mar <- par("mar")
			top <- 3.5 + length(y)
			command <- paste(".mar <- par(mar=c(", mar[1], ",", 
					mar[2], ",", top, ",", mar[4], "))", sep = "")
			logger(command)
			justDoIt(command)
		}
		command <- paste("matplot(", .activeDataSet, "$", x, 
				", ", .activeDataSet, "[, ", paste("c(", paste(paste("\"", 
										y, "\"", sep = ""), collapse = ","), ")", sep = ""), 
				"], type=\"b\", lty=1, ylab=\"", axisLabel, "\"", 
				pch, ")", sep = "")
		logger(command)
		justDoIt(command)
		if (legend && length(y) > 1) {
			n <- length(y)
			cols <- rep(1:6, 1 + n%/%6)[1:n]
			logger(".xpd <- par(xpd=TRUE)")
			justDoIt(".xpd <- par(xpd=TRUE)")
			usr <- par("usr")
			command <- paste("legend(", usr[1], ", ", usr[4] + 
							1.2 * top * strheight("x"), ", legend=", paste("c(", 
							paste(paste("\"", y, "\"", sep = ""), collapse = ","), 
							")", sep = ""), ", col=c(", paste(cols, collapse = ","), 
					"), lty=1, pch=c(", paste(paste("\"", as.character(1:n), 
									"\"", sep = ""), collapse = ","), "))", sep = "")
			logger(command)
			justDoIt(command)
			logger("par(mar=.mar)")
			justDoIt("par(mar=.mar)")
			logger("par(xpd=.xpd)")
			justDoIt("par(xpd=.xpd)")
		}
		activateMenus()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "matplot", reset = "linePlot")
	tkgrid(getFrame(xBox), labelRcmdr(variablesFrame, text = "    "), 
			getFrame(yBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "nw")
	tkgrid(labelRcmdr(axisLabelFrame, text = gettextRcmdr("Label for y-axis"), 
					fg = "blue"), sticky = "w")
	tkgrid(axisLabelEntry, sticky = "w")
	tkgrid(axisLabelScroll, sticky = "ew")
	tkgrid(axisLabelFrame, sticky = "w")
	tkgrid(labelRcmdr(legendFrame, text = gettextRcmdr("Plot legend")), 
			legendCheckBox, sticky = "w")
	tkgrid(legendFrame, sticky = "w")
	tkgrid(buttonsFrame, stick = "w")
	dialogSuffix(rows = 4, columns = 1)
}

QQPlot <- function()
# this function modified by Martin Maechler
{
    require("car")
    initializeDialog(title=gettextRcmdr("Quantile-Comparison (QQ) Plot"))
    xBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Variable (pick one)"))
    onOK <- function(){
        x <- getSelection(xBox)
        closeDialog()
       if (0 == length(x)) {
            errorCondition(recall=QQPlot, message=gettextRcmdr("You must select a variable."))
            return()
            }
        dist <- tclvalue(distVariable)
        save <- options(warn=-1)
        on.exit(save)
        retryMe <- function(msg) {
            Message(message= msg, type="error")
            QQPlot()
        }
        switch(dist,
               "norm" = { args <- 'dist="norm"' },
               "t" =  {
                   df <- tclvalue(tDfVariable)
                   df.num <- as.numeric(df)
                   if (is.na(df.num) || df.num < 1) {
                       retryMe(gettextRcmdr("df for t must be a positive number."))
                       return()
                   }
                   args <- paste('dist="t", df=', df, sep="")
               },
               "chisq" = {
                   df <- tclvalue(chisqDfVariable)
                   df.num <- as.numeric(df)
                   if (is.na(df.num) || df.num < 1) {
                       retryMe(gettextRcmdr("df for chi-square must be a positive number."))
                       return()
                   }
                   args <- paste('dist="chisq", df=', df, sep="")
               },
               "f" = {
                   df1 <- tclvalue(FDf1Variable)
                   df2 <- tclvalue(FDf2Variable)
                   df.num1 <- as.numeric(df1)
                   df.num2 <- as.numeric(df2)
                   if (is.na(df.num1) || df.num1 < 1 ||
                       is.na(df.num2) || df.num2 < 1) {
                       retryMe(gettextRcmdr("numerator and denominator \ndf for F must be positive numbers."))
                       return()
                   }
                   args <- paste('dist="f", df1=', df1, ', df2=', df2, sep="")
               },
               ## else -- other `dist' :
           {
               dist <- tclvalue(otherNameVariable)
               params <- tclvalue(otherParamsVariable)
               args <- paste('dist="', dist,'", ', params, sep="")
           }) # end{switch}
        .activeDataSet <- ActiveDataSet()
        if ("1" == tclvalue(identifyVariable)){
            RcmdrTkmessageBox(title="Identify Points",
					message=paste(gettextRcmdr("Use left mouse button to identify points,\n"),
						gettextRcmdr(if (MacOSXP()) "esc key to exit." else "right button to exit."), sep=""),
                icon="info", type="ok")
            idtext <- paste(", labels=rownames(", .activeDataSet, '), id.method="identify"', sep="")
            }
        else idtext <- ""
        command <- paste("qqPlot", "(", .activeDataSet, "$", x, ", ", args,
                          idtext, ")", sep="")
        doItAndPrint(command)
        activateMenus()
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject="qqPlot")
    distFrame <- tkframe(top)
    distVariable <- tclVar("norm")
    normalButton <- ttkradiobutton(distFrame, variable=distVariable, value="norm")
    tButton <- ttkradiobutton(distFrame, variable=distVariable, value="t")
    chisqButton <- ttkradiobutton(distFrame, variable=distVariable, value="chisq")
    FButton <- ttkradiobutton(distFrame, variable=distVariable, value="f")
    otherButton <- ttkradiobutton(distFrame, variable=distVariable, value="other")
    tDfFrame <- tkframe(distFrame)
    tDfVariable <- tclVar("")
    tDfField <- ttkentry(tDfFrame, width="6", textvariable=tDfVariable)
    chisqDfFrame <- tkframe(distFrame)
    chisqDfVariable <- tclVar("")
    chisqDfField <- ttkentry(chisqDfFrame, width="6", textvariable=chisqDfVariable)
    FDfFrame <- tkframe(distFrame)
    FDf1Variable <- tclVar("")
    FDf1Field <- ttkentry(FDfFrame, width="6", textvariable=FDf1Variable)
    FDf2Variable <- tclVar("")
    FDf2Field <- ttkentry(FDfFrame, width="6", textvariable=FDf2Variable)
    otherParamsFrame <- tkframe(distFrame)
    otherParamsVariable <- tclVar("")
    otherParamsField <- ttkentry(otherParamsFrame, width="30", textvariable=otherParamsVariable)
    otherNameVariable <- tclVar("")
    otherNameField <- ttkentry(otherParamsFrame, width="10", textvariable=otherNameVariable)
    identifyVariable <- tclVar("0")
    identifyFrame <- tkframe(top)
    identifyCheckBox <- tkcheckbutton(identifyFrame, variable=identifyVariable)
    tkgrid(getFrame(xBox), sticky="nw")
    tkgrid(labelRcmdr(identifyFrame, text=gettextRcmdr("Identify observations with mouse")),
           identifyCheckBox, sticky="w")
    tkgrid(identifyFrame, sticky="w")
    tkgrid(labelRcmdr(distFrame, text=gettextRcmdr("Distribution"), fg="blue"), columnspan=6, sticky="w")
    tkgrid(labelRcmdr(distFrame, text=gettextRcmdr("Normal")), normalButton, sticky="w")
    tkgrid(labelRcmdr(tDfFrame, text=gettextRcmdr("df = ")), tDfField, sticky="w")
    tkgrid(labelRcmdr(distFrame, text="t"), tButton, tDfFrame, sticky="w")
    tkgrid(labelRcmdr(chisqDfFrame, text=gettextRcmdr("df = ")), chisqDfField, sticky="w")
    tkgrid(labelRcmdr(distFrame, text=gettextRcmdr("Chi-square")), chisqButton,
           chisqDfFrame, sticky="w")
    tkgrid(labelRcmdr(FDfFrame, text=gettextRcmdr("Numerator df = ")), FDf1Field,
           labelRcmdr(FDfFrame, text=gettextRcmdr("Denominator df = ")), FDf2Field, sticky="w")
    tkgrid(labelRcmdr(distFrame, text="F"), FButton, FDfFrame, sticky="w")
    tkgrid(labelRcmdr(otherParamsFrame, text=gettextRcmdr("Specify: ")),
           otherNameField, labelRcmdr(otherParamsFrame, text=gettextRcmdr("Parameters: ")),
           otherParamsField, sticky="w")
    tkgrid(labelRcmdr(distFrame, text=gettextRcmdr("Other")), otherButton,
           otherParamsFrame, sticky="w")
    tkgrid(distFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=5, columns=1)
    }

#PlotMeans <- function(){
#    initializeDialog(title=gettextRcmdr("Plot Means"))
#    groupBox <- variableListBox(top, Factors(), title=gettextRcmdr("Factors (pick one or two)"), selectmode="multiple")
#    responseBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
#    onOK <- function(){
#        groups <- getSelection(groupBox)
#        response <- getSelection(responseBox)
#        closeDialog()
#        if (0 == length(groups)) {
#            errorCondition(recall=PlotMeans, message=gettextRcmdr("No factors selected."))
#            return()
#            }
#        if (2 < length(groups)) {
#            errorCondition(recall=PlotMeans, message=gettextRcmdr("More than two factors selected."))
#            return()
#            }
#        if (0 == length(response)) {
#            errorCondition(recall=PlotMeans, message=gettextRcmdr("No response variable selected."))
#            return()
#            }
#        .activeDataSet <- ActiveDataSet()
#        error.bars <- tclvalue(errorBarsVariable)
#        level <- if (error.bars == "conf.int") paste(", level=", tclvalue(levelVariable), sep="") else ""
#        if (length(groups) == 1) doItAndPrint(paste("plotMeans(", .activeDataSet, "$", response,
#            ", ", .activeDataSet, "$", groups[1],
#            ', error.bars="', error.bars, '"', level, ')', sep=""))
#        else {
#            if (eval(parse(text=paste("length(levels(", .activeDataSet, "$", groups[1],
#                ")) < length(levels(", .activeDataSet, "$", groups[2], "))", sep=""))))
#                groups <- rev(groups)
#            doItAndPrint(paste("plotMeans(", .activeDataSet, "$", response, ", ", .activeDataSet, "$", groups[1],
#                ", ", .activeDataSet, "$", groups[2], ', error.bars="', error.bars, '"', level, ')', sep=""))
#            }
#        activateMenus()
#        tkfocus(CommanderWindow())
#        }
#    optionsFrame <- tkframe(top)
#    errorBarsVariable <- tclVar("se")
#    seButton <- ttkradiobutton(optionsFrame, variable=errorBarsVariable, value="se")
#    sdButton <- ttkradiobutton(optionsFrame, variable=errorBarsVariable, value="sd")
#    confIntButton <- ttkradiobutton(optionsFrame, variable=errorBarsVariable, value="conf.int")
#    noneButton <- ttkradiobutton(optionsFrame, variable=errorBarsVariable, value="none")
#    levelVariable <- tclVar("0.95")
#    levelEntry <- ttkentry(optionsFrame, width="6", textvariable=levelVariable)
#    buttonsFrame <- tkframe(top)
#    OKCancelHelp(helpSubject="plotMeans")
#    tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
#    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Error Bars"), fg="blue"), sticky="w")
#    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Standard errors")), seButton, sticky="w")
#    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Standard deviations")), sdButton, sticky="w")
#    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("Confidence intervals")), confIntButton,
#        labelRcmdr(optionsFrame, text=gettextRcmdr("   Level of confidence:")), levelEntry, sticky="w")
#    tkgrid(labelRcmdr(optionsFrame, text=gettextRcmdr("No error bars")), noneButton, sticky="w")
#    tkgrid(optionsFrame, columnspan=2, sticky="w")
#    tkgrid(buttonsFrame, columnspan=2, sticky="w")
#    dialogSuffix(rows=3, columns=2)
#    }

PlotMeans <- function () {
	defaults <- list(initial.groups = NULL, initial.response = NULL, initial.error.bars = "se",
			initial.level = "0.95") 
	dialog.values <- getDialog("PlotMeans", defaults)
	initializeDialog(title = gettextRcmdr("Plot Means"))
	groupBox <- variableListBox(top, Factors(), title = gettextRcmdr("Factors (pick one or two)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.groups, "factor"))
	responseBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
			initialSelection = varPosn (dialog.values$initial.response, "numeric"))
	onOK <- function() {
		groups <- getSelection(groupBox)
		response <- getSelection(responseBox)
		closeDialog()
		if (0 == length(groups)) {
			errorCondition(recall = PlotMeans, message = gettextRcmdr("No factors selected."))
			return()
		}
		if (2 < length(groups)) {
			errorCondition(recall = PlotMeans, message = gettextRcmdr("More than two factors selected."))
			return()
		}
		if (0 == length(response)) {
			errorCondition(recall = PlotMeans, message = gettextRcmdr("No response variable selected."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		error.bars <- tclvalue(errorBarsVariable)
		level <- if (error.bars == "conf.int") 
					paste(", level=", tclvalue(levelVariable), sep = "")
				else ""
		putDialog ("PlotMeans", list(initial.groups = groups, initial.response = response, 
						initial.error.bars = error.bars, initial.level = tclvalue(levelVariable)))
		if (length(groups) == 1) 
			doItAndPrint(paste("plotMeans(", .activeDataSet, 
							"$", response, ", ", .activeDataSet, "$", groups[1], 
							", error.bars=\"", error.bars, "\"", level, ")", 
							sep = ""))
		else {
			if (eval(parse(text = paste("length(levels(", .activeDataSet, 
									"$", groups[1], ")) < length(levels(", .activeDataSet, 
									"$", groups[2], "))", sep = "")))) 
				groups <- rev(groups)
			doItAndPrint(paste("plotMeans(", .activeDataSet, 
							"$", response, ", ", .activeDataSet, "$", groups[1], 
							", ", .activeDataSet, "$", groups[2], ", error.bars=\"", 
							error.bars, "\"", level, ")", sep = ""))
		}
		activateMenus()
		tkfocus(CommanderWindow())
	}
	optionsFrame <- tkframe(top)
	errorBarsVariable <- tclVar(dialog.values$initial.error.bars)
	seButton <- ttkradiobutton(optionsFrame, variable = errorBarsVariable, 
			value = "se")
	sdButton <- ttkradiobutton(optionsFrame, variable = errorBarsVariable, 
			value = "sd")
	confIntButton <- ttkradiobutton(optionsFrame, variable = errorBarsVariable, 
			value = "conf.int")
	noneButton <- ttkradiobutton(optionsFrame, variable = errorBarsVariable, 
			value = "none")
	levelVariable <- tclVar(dialog.values$initial.level)
	levelEntry <- ttkentry(optionsFrame, width = "6", textvariable = levelVariable)
	buttonsFrame <- tkframe(top)
	OKCancelHelp(helpSubject = "plotMeans", reset = "PlotMeans")
	tkgrid(getFrame(groupBox), getFrame(responseBox), sticky = "nw")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Error Bars"), 
					fg = "blue"), sticky = "w")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Standard errors")), 
			seButton, sticky = "w")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Standard deviations")), 
			sdButton, sticky = "w")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Confidence intervals")), 
			confIntButton, labelRcmdr(optionsFrame, text = gettextRcmdr("   Level of confidence:")), 
			levelEntry, sticky = "w")
	tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("No error bars")), 
			noneButton, sticky = "w")
	tkgrid(optionsFrame, columnspan = 2, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 3, columns = 2)
}

#Scatter3D <- function(){
#    use.rgl <- options("Rcmdr")[[1]]$use.rgl
#    if (length(use.rgl) == 0 || use.rgl) {
#		Library("car")
#        Library("rgl")
#        Library("mgcv")
#        }
#    initializeDialog(title=gettextRcmdr("3D Scatterplot"))
#    variablesFrame <- tkframe(top)
#    .numeric <- Numeric()
#    xBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Explanatory variables (pick two)"), selectmode="multiple",
#        initialSelection=NULL)
#    yBox <- variableListBox(variablesFrame, .numeric, title=gettextRcmdr("Response variable (pick one)"))
#    surfacesFrame <- tkframe(top)
#    identifyPoints <- tclVar("0")
#    identifyPointsCheckBox <- tkcheckbutton(surfacesFrame, variable=identifyPoints)
#    axisScales <- tclVar("1")
#    axisScalesCheckBox <- tkcheckbutton(surfacesFrame, variable=axisScales)
#    gridLines <- tclVar("1")
#    gridLinesCheckBox <- tkcheckbutton(surfacesFrame, variable=gridLines)
#    squaredResiduals <- tclVar("0")
#    squaredResidualsCheckBox <- tkcheckbutton(surfacesFrame, variable=squaredResiduals)
#    linearLSSurface <- tclVar("1")
#    linearLSCheckBox <- tkcheckbutton(surfacesFrame, variable=linearLSSurface)
#    quadLSSurface <- tclVar("0")
#    quadLSCheckBox <- tkcheckbutton(surfacesFrame, variable=quadLSSurface)
#    nonparSurface <- tclVar("0")
#    nonparCheckBox <- tkcheckbutton(surfacesFrame, variable=nonparSurface)
#    dfNonparVariable <- tclVar(gettextRcmdr("<auto>"))
#    dfNonparField <- ttkentry(surfacesFrame, width="6", textvariable=dfNonparVariable)
#    additiveSurface <- tclVar("0")
#    additiveCheckBox <- tkcheckbutton(surfacesFrame, variable=additiveSurface)
#    dfAddVariable <- tclVar(gettextRcmdr("<auto>"))
#    dfAddField <- ttkentry(surfacesFrame, width="6", textvariable=dfAddVariable)
#    ellipsoid <- tclVar("0")
#    ellipsoidCheckBox <- tkcheckbutton(surfacesFrame, variable=ellipsoid)
#    bgFrame <- tkframe(top)
#    bgVariable <-tclVar("white")
#    whiteButton <- ttkradiobutton(bgFrame, variable=bgVariable, value="white")
#    blackButton <- ttkradiobutton(bgFrame, variable=bgVariable, value="black")
#    onOK <- function(){
#        x <- getSelection(xBox)
#        y <- getSelection(yBox)
#        closeDialog()
#        if (length(y) == 0) {
#            errorCondition(recall=Scatter3D, message=gettextRcmdr("You must select a response variable."))
#            return()
#            }
#        if (2 != length(x)) {
#            errorCondition(recall=Scatter3D, message=gettextRcmdr("You must select 2 explanatory variables."))
#            return()
#            }
#        if (is.element(y, x)) {
#            errorCondition(recall=Scatter3D, message=gettextRcmdr("Response and explanatory variables must be different."))
#            return()
#            }
#        scales <- if (tclvalue(axisScales) == 1) "TRUE" else "FALSE"
#        grid <- if (tclvalue(gridLines) == 1) "TRUE" else "FALSE"
#        resids <- if(tclvalue(squaredResiduals) == 1) ', residuals="squares"' else ", residuals=TRUE"
#        lin <- if(tclvalue(linearLSSurface) == 1) '"linear"'
#        quad <- if(tclvalue(quadLSSurface) == 1) '"quadratic"'
#        nonpar <- if (tclvalue(nonparSurface) == 1) '"smooth"'
#        additive <- if (tclvalue(additiveSurface) == 1) '"additive"'
#        surfaces <- c(lin, quad, nonpar, additive)
#        nsurfaces <- length(surfaces)
#        if (nsurfaces > 1) resids <- ""
#        ellips <- if(tclvalue(ellipsoid) == 1) "TRUE" else "FALSE"
#        opts <- options(warn=-1)
#        dfNonpar <- tclvalue(dfNonparVariable)
#        dfNonpar <- if (dfNonpar == gettextRcmdr("<auto>")) "" else paste(", df.smooth=", as.numeric(dfNonpar), sep="")
#        dfAdd <- tclvalue(dfAddVariable)
#        dfAdd <- if (dfAdd == gettextRcmdr("<auto>")) "" else paste(", df.additive=", as.numeric(dfAdd), sep="")
#        options(opts)
#        fit <- if (nsurfaces == 0) ", surface=FALSE"
#            else if (nsurfaces == 1) paste(", fit=", surfaces, sep="")
#            else paste(", fit=c(", paste(surfaces, collapse=","), ")", sep="")
#        bg <- tclvalue(bgVariable)
#        .activeDataSet <- ActiveDataSet()
#        if (.groups != FALSE){
#            groups <- paste(", groups=", .activeDataSet, "$", .groups, sep="")
#            parallel <- paste(", parallel=", .linesByGroup, sep="")
#            }
#        else groups <- parallel <- ""
#        command <- paste("scatter3d(", .activeDataSet, "$", x[1], ", ",
#            .activeDataSet, "$", y, ", ", .activeDataSet, "$", x[2], fit, resids, dfNonpar,
#            dfAdd, groups, parallel, ', bg="', bg, '", axis.scales=', scales, ', grid=', grid,
#            ', ellipsoid=', ellips, ', xlab="', x[1], '", ylab="', y, '", zlab="', x[2], '")', sep="")
#        doItAndPrint(command)
#        putRcmdr("rgl", TRUE)
#        command <- paste("identify3d(", .activeDataSet, "$", x[1], ", ",
#            .activeDataSet, "$", y, ", ", .activeDataSet, "$", x[2], groups,
#            ', axis.scales=', scales,
#            ", labels=row.names(", .activeDataSet, "))", sep="")
#        putRcmdr("Identify3d", command)
#        .Tcl("update")
#        if (tclvalue(identifyPoints) == 1){
#            RcmdrTkmessageBox(title="Identify Points",
#					message=paste(gettextRcmdr("Use left mouse button to identify points,\n"),
#						gettextRcmdr(if (MacOSXP()) "esc key to exit." else "right button to exit."), sep=""),
#                icon="info", type="ok")
#            doItAndPrint(command)
#            }
#        activateMenus()
#        tkfocus(CommanderWindow())
#        rgl.bringtotop()
#        }
#    groupsBox(Scatter3D, plotLinesByGroup=TRUE, plotLinesByGroupsText=gettextRcmdr("Parallel regression surfaces"))
#    OKCancelHelp(helpSubject="Scatter3DDialog")
#    tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text="  "), getFrame(xBox), sticky="nw")
#    tkgrid(variablesFrame, sticky="nw")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Identify observations\nwith mouse")), identifyPointsCheckBox, sticky="w")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Show axis scales")), axisScalesCheckBox, sticky="w")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Show surface grid lines")), gridLinesCheckBox, sticky="w")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Show squared residuals")), squaredResidualsCheckBox, sticky="w")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Surfaces to Fit"), fg="blue"), sticky="w")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Linear least-squares")), linearLSCheckBox, sticky="w")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Quadratic least-squares")), quadLSCheckBox, sticky="w")
#    dfLabel <- labelRcmdr(surfacesFrame, text=gettextRcmdr("df = "))
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Smooth regression")), nonparCheckBox,
#        dfLabel, dfNonparField, sticky="w")
#    tkgrid.configure(dfLabel, sticky="e")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Additive regression")), additiveCheckBox,
#        labelRcmdr(surfacesFrame, text=gettextRcmdr("df(each term) = ")), dfAddField, sticky="w")
#    tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Plot 50% concentration ellipsoid")), ellipsoidCheckBox, sticky="w")
#    tkgrid(surfacesFrame, sticky="w")
#    tkgrid(labelRcmdr(bgFrame, text=gettextRcmdr("Background Color"), fg="blue"), sticky="w", columnspan=2)
#    tkgrid(labelRcmdr(bgFrame, text=gettextRcmdr("Black")), blackButton, sticky="w")
#    tkgrid(labelRcmdr(bgFrame, text=gettextRcmdr("White")), whiteButton, sticky="w")
#    tkgrid(bgFrame, sticky="w")
#    tkgrid(groupsFrame, sticky="w")
#    tkgrid(buttonsFrame, stick="w")
#    dialogSuffix(rows=5, columns=1)
#    }

Scatter3D <- function () {
	use.rgl <- options("Rcmdr")[[1]]$use.rgl
	if (length(use.rgl) == 0 || use.rgl) {
		Library("car")
		Library("rgl")
		Library("mgcv")
	}
	defaults <- list (initial.x = NULL, initial.y = NULL, initial.scales = 1, initial.grid = 1, 
			initial.resids = 0, initial.lin = 1, initial.quad = 0, initial.nonpar = 0, 
			initial.additive = 0, initial.ellips = 0, initial.dfNonpar = gettextRcmdr ("<auto>"), 
			initial.dfAdd = gettextRcmdr ("<auto>"), initial.bg = "white", initial.identify = 0)
	dialog.values <- getDialog ("Scatter3D", defaults)
	initializeDialog(title = gettextRcmdr("3D Scatterplot"))
	variablesFrame <- tkframe(top)
	.numeric <- Numeric()
	xBox <- variableListBox(variablesFrame, .numeric, title = gettextRcmdr("Explanatory variables (pick two)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.x, "numeric"))
	yBox <- variableListBox(variablesFrame, .numeric, title = gettextRcmdr("Response variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.y, "numeric"))
	surfacesFrame <- tkframe(top)
	identifyPoints <- tclVar(dialog.values$initial.identify)
	identifyPointsCheckBox <- tkcheckbutton(surfacesFrame, variable = identifyPoints)
	axisScales <- tclVar(dialog.values$initial.scales)
	axisScalesCheckBox <- tkcheckbutton(surfacesFrame, variable = axisScales)
	gridLines <- tclVar(dialog.values$initial.grid)
	gridLinesCheckBox <- tkcheckbutton(surfacesFrame, variable = gridLines)
	squaredResiduals <- tclVar(dialog.values$initial.resids)
	squaredResidualsCheckBox <- tkcheckbutton(surfacesFrame, 
			variable = squaredResiduals)
	linearLSSurface <- tclVar(dialog.values$initial.lin)
	linearLSCheckBox <- tkcheckbutton(surfacesFrame, variable = linearLSSurface)
	quadLSSurface <- tclVar(dialog.values$initial.quad)
	quadLSCheckBox <- tkcheckbutton(surfacesFrame, variable = quadLSSurface)
	nonparSurface <- tclVar(dialog.values$initial.nonpar)
	nonparCheckBox <- tkcheckbutton(surfacesFrame, variable = nonparSurface)
	dfNonparVariable <- tclVar(dialog.values$initial.dfNonpar)
	dfNonparField <- ttkentry(surfacesFrame, width = "6", textvariable = dfNonparVariable)
	additiveSurface <- tclVar(dialog.values$initial.additive)
	additiveCheckBox <- tkcheckbutton(surfacesFrame, variable = additiveSurface)
	dfAddVariable <- tclVar(dialog.values$initial.dfAdd)
	dfAddField <- ttkentry(surfacesFrame, width = "6", textvariable = dfAddVariable)
	ellipsoid <- tclVar(dialog.values$initial.ellips)
	ellipsoidCheckBox <- tkcheckbutton(surfacesFrame, variable = ellipsoid)
	bgFrame <- tkframe(top)
	bgVariable <- tclVar(dialog.values$initial.bg)
	whiteButton <- ttkradiobutton(bgFrame, variable = bgVariable, 
			value = "white")
	blackButton <- ttkradiobutton(bgFrame, variable = bgVariable, 
			value = "black")
	onOK <- function() {
		x <- getSelection(xBox)
		y <- getSelection(yBox)
		scales <- tclvalue(axisScales)
		grid <- tclvalue(gridLines)
		resids <- tclvalue(squaredResiduals)
		lin <- tclvalue(linearLSSurface)
		quad <- tclvalue(quadLSSurface)
		nonpar <- tclvalue(nonparSurface)
		additive <- tclvalue(additiveSurface)
		ellips <- tclvalue(ellipsoid) 
		dfNonpar <- tclvalue(dfNonparVariable)
		dfAdd <- tclvalue(dfAddVariable)
		bg <- tclvalue(bgVariable)
		identify <- tclvalue(identifyPoints)
		putDialog ("Scatter3D", list (initial.x = x, initial.y = y, initial.scales = scales, initial.grid = grid, 
						initial.resids = resids, initial.lin = lin, initial.quad = quad, initial.nonpar = nonpar, 
						initial.additive = additive, initial.ellips = ellips, initial.dfNonpar = dfNonpar, 
						initial.dfAdd = dfAdd, initial.bg = bg, initial.identify = identify))
		closeDialog()
		if (length(y) == 0) {
			errorCondition(recall = Scatter3D, message = gettextRcmdr("You must select a response variable."))
			return()
		}
		if (2 != length(x)) {
			errorCondition(recall = Scatter3D, message = gettextRcmdr("You must select 2 explanatory variables."))
			return()
		}
		if (is.element(y, x)) {
			errorCondition(recall = Scatter3D, message = gettextRcmdr("Response and explanatory variables must be different."))
			return()
		}
		scales <- if (tclvalue(axisScales) == 1) 
					"TRUE"
				else "FALSE"
		grid <- if (tclvalue(gridLines) == 1) 
					"TRUE"
				else "FALSE"
		resids <- if (tclvalue(squaredResiduals) == 1) 
					", residuals=\"squares\""
				else ", residuals=TRUE"
		lin <- if (tclvalue(linearLSSurface) == 1) 
			"\"linear\""
		quad <- if (tclvalue(quadLSSurface) == 1) 
			"\"quadratic\""
		nonpar <- if (tclvalue(nonparSurface) == 1) 
			"\"smooth\""
		additive <- if (tclvalue(additiveSurface) == 1) 
			"\"additive\""
		surfaces <- c(lin, quad, nonpar, additive)
		nsurfaces <- length(surfaces)
		if (nsurfaces > 1) 
			resids <- ""
		ellips <- if (tclvalue(ellipsoid) == 1) 
					"TRUE"
				else "FALSE"
		opts <- options(warn = -1)
		dfNonpar <- if (dfNonpar == gettextRcmdr("<auto>")) 
					""
				else paste(", df.smooth=", as.numeric(dfNonpar), sep = "")
		dfAdd <- if (dfAdd == gettextRcmdr("<auto>")) 
					""
				else paste(", df.additive=", as.numeric(dfAdd), sep = "")
		options(opts)
		fit <- if (nsurfaces == 0) 
					", surface=FALSE"
				else if (nsurfaces == 1) 
					paste(", fit=", surfaces, sep = "")
				else paste(", fit=c(", paste(surfaces, collapse = ","), 
							")", sep = "")
		.activeDataSet <- ActiveDataSet()
		if (.groups != FALSE) {
			groups <- paste(", groups=", .activeDataSet, "$", 
					.groups, sep = "")
			parallel <- paste(", parallel=", .linesByGroup, sep = "")
		}
		else groups <- parallel <- ""
		command <- paste("scatter3d(", .activeDataSet, "$", x[1], 
				", ", .activeDataSet, "$", y, ", ", .activeDataSet, 
				"$", x[2], fit, resids, dfNonpar, dfAdd, groups, 
				parallel, ", bg=\"", bg, "\", axis.scales=", scales, 
				", grid=", grid, ", ellipsoid=", ellips, ", xlab=\"", 
				x[1], "\", ylab=\"", y, "\", zlab=\"", x[2], "\")", 
				sep = "")
		doItAndPrint(command)
		putRcmdr("rgl", TRUE)
		command <- paste("identify3d(", .activeDataSet, "$", 
				x[1], ", ", .activeDataSet, "$", y, ", ", .activeDataSet, 
				"$", x[2], groups, ", axis.scales=", scales, ", labels=row.names(", 
				.activeDataSet, "))", sep = "")
		putRcmdr("Identify3d", command)
		.Tcl("update")
		if (tclvalue(identifyPoints) == 1) {
			RcmdrTkmessageBox(title = "Identify Points", message = paste(gettextRcmdr("Use left mouse button to identify points,\n"), 
							gettextRcmdr(if (MacOSXP()) 
												"esc key to exit."
											else "right button to exit."), sep = ""), icon = "info", 
					type = "ok")
			doItAndPrint(command)
		}
		activateMenus()
		tkfocus(CommanderWindow())
		rgl.bringtotop()
	}
	groupsBox(Scatter3D, plotLinesByGroup = TRUE, plotLinesByGroupsText = gettextRcmdr("Parallel regression surfaces"))
	OKCancelHelp(helpSubject = "Scatter3DDialog", reset = "Scatter3D")
	tkgrid(getFrame(yBox), labelRcmdr(variablesFrame, text = "  "), 
			getFrame(xBox), sticky = "nw")
	tkgrid(variablesFrame, sticky = "nw")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Identify observations\nwith mouse")), 
			identifyPointsCheckBox, sticky = "w")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Show axis scales")), 
			axisScalesCheckBox, sticky = "w")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Show surface grid lines")), 
			gridLinesCheckBox, sticky = "w")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Show squared residuals")), 
			squaredResidualsCheckBox, sticky = "w")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Surfaces to Fit"), 
					fg = "blue"), sticky = "w")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Linear least-squares")), 
			linearLSCheckBox, sticky = "w")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Quadratic least-squares")), 
			quadLSCheckBox, sticky = "w")
	dfLabel <- labelRcmdr(surfacesFrame, text = gettextRcmdr("df = "))
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Smooth regression")), 
			nonparCheckBox, dfLabel, dfNonparField, sticky = "w")
	tkgrid.configure(dfLabel, sticky = "e")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Additive regression")), 
			additiveCheckBox, labelRcmdr(surfacesFrame, text = gettextRcmdr("df(each term) = ")), 
			dfAddField, sticky = "w")
	tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Plot 50% concentration ellipsoid")), 
			ellipsoidCheckBox, sticky = "w")
	tkgrid(surfacesFrame, sticky = "w")
	tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("Background Color"), 
					fg = "blue"), sticky = "w", columnspan = 2)
	tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("Black")), 
			blackButton, sticky = "w")
	tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("White")), 
			whiteButton, sticky = "w")
	tkgrid(bgFrame, sticky = "w")
	tkgrid(groupsFrame, sticky = "w")
	tkgrid(buttonsFrame, stick = "w")
	dialogSuffix(rows = 5, columns = 1)
}

Identify3D <- function(){
    if (0 == rgl.cur()) {
        Message(message=gettextRcmdr("There is no current RGL graphics device."),
            type="error")
        return()
        }
    RcmdrTkmessageBox(title="Identify Points",
        message=gettextRcmdr("Drag right mouse button to identify points,\nclick right button to exit."),
        icon="info", type="ok")
    command <- getRcmdr("Identify3d")
    doItAndPrint(command)
    }

#saveBitmap <- function(){
#    if (1 == dev.cur()) {
#        Message(gettextRcmdr("There is no current graphics device to save."), type="error")
#        return()
#        }
#    initializeDialog(title=gettextRcmdr("Save Graph as Bitmap"))
#    radioButtons(name="filetype", buttons=c("png", "jpeg"), labels=c("PNG", "JPEG"), title=gettextRcmdr("Graphics File Type"))
#    sliderFrame <- tkframe(top)
#    widthVariable <- tclVar("500")
#    widthSlider <- tkscale(sliderFrame, from=200, to=1000, showvalue=TRUE, variable=widthVariable,
#        resolution=25, orient="horizontal")
#    heightVariable <- tclVar("500")
#    heightSlider <- tkscale(sliderFrame, from=200, to=1000, showvalue=TRUE, variable=heightVariable,
#        resolution=25, orient="horizontal")
#    onOK <- function(){
#        closeDialog()
#        width <- tclvalue(widthVariable)
#        height <- tclvalue(heightVariable)
#        type <- tclvalue(filetypeVariable)
#        if (type == "png"){
#            ext <- "png"
#            filetypes <- gettextRcmdr('{"All Files" {"*"}} {"PNG Files" {".png" ".PNG"}}')
#            initial <- "RGraph.png"
#            }
#        else{
#            ext <- "jpg"
#            filetypes <- gettextRcmdr('{"All Files" {"*"}} {"JPEG Files" {".jpg" ".JPG" ".jpeg" ".JPEG"}}')
#            initial <- "RGraph.jpg"
#            }
#        filename <- tclvalue(tkgetSaveFile(filetypes=filetypes,
#                                           defaultextension=ext,
#                                           initialfile=initial,
#                                           parent=CommanderWindow()))
#        if (filename == "") return()
#        command <- paste('dev.print(', type, ', filename="', filename, '", width=', width, ', height=', height, ')', sep="")
#        doItAndPrint(command)
#        Message(paste(gettextRcmdr("Graph saved to file"), filename), type="note")
#        }
#    OKCancelHelp(helpSubject="png")
#    tkgrid(filetypeFrame, sticky="w")
#    tkgrid(labelRcmdr(sliderFrame, text=gettextRcmdr("Width (pixels)")), widthSlider, sticky="sw")
#    tkgrid(labelRcmdr(sliderFrame, text=gettextRcmdr("Height (pixels)")), heightSlider, sticky="sw")
#    tkgrid(sliderFrame, sticky="w")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=3, columns=1)
#    }

saveBitmap <- function () {
	if (1 == dev.cur()) {
		Message(gettextRcmdr("There is no current graphics device to save."), 
				type = "error")
		return()
	}
	defaults <- list (initial.width = 500, initial.height = 500, initial.type = "png")
	dialog.values <- getDialog ("saveBitmap", defaults)
	initializeDialog(title = gettextRcmdr("Save Graph as Bitmap"))
	radioButtons(name = "filetype", buttons = c("png", "jpeg"), 
			labels = c("PNG", "JPEG"), title = gettextRcmdr("Graphics File Type"),
			initialValue = dialog.values$initial.type)
	sliderFrame <- tkframe(top)
	widthVariable <- tclVar(dialog.values$initial.width)
	widthSlider <- tkscale(sliderFrame, from = 200, to = 1000, 
			showvalue = TRUE, variable = widthVariable, resolution = 25, 
			orient = "horizontal")
	heightVariable <- tclVar(dialog.values$initial.height)
	heightSlider <- tkscale(sliderFrame, from = 200, to = 1000, 
			showvalue = TRUE, variable = heightVariable, resolution = 25, 
			orient = "horizontal")
	onOK <- function() {
		closeDialog()
		width <- tclvalue(widthVariable)
		height <- tclvalue(heightVariable)
		type <- tclvalue(filetypeVariable)
		putDialog ("saveBitmap", list (initial.width = width, initial.height = height, initial.type = type))
		if (type == "png") {
			ext <- "png"
			filetypes <- gettextRcmdr("{\"All Files\" {\"*\"}} {\"PNG Files\" {\".png\" \".PNG\"}}")
			initial <- "RGraph.png"
		}
		else {
			ext <- "jpg"
			filetypes <- gettextRcmdr("{\"All Files\" {\"*\"}} {\"JPEG Files\" {\".jpg\" \".JPG\" \".jpeg\" \".JPEG\"}}")
			initial <- "RGraph.jpg"
		}
		filename <- tclvalue(tkgetSaveFile(filetypes = filetypes, 
						defaultextension = ext, initialfile = initial, parent = CommanderWindow()))
		if (filename == "") 
			return()
		command <- paste("dev.print(", type, ", filename=\"", 
				filename, "\", width=", width, ", height=", height, 
				")", sep = "")
		doItAndPrint(command)
		Message(paste(gettextRcmdr("Graph saved to file"), filename), 
				type = "note")
	}
	OKCancelHelp(helpSubject = "png", reset = "saveBitmap")
	tkgrid(filetypeFrame, sticky = "w")
	tkgrid(labelRcmdr(sliderFrame, text = gettextRcmdr("Width (pixels)")), 
			widthSlider, sticky = "sw")
	tkgrid(labelRcmdr(sliderFrame, text = gettextRcmdr("Height (pixels)")), 
			heightSlider, sticky = "sw")
	tkgrid(sliderFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

#savePDF <- function(){
#    if (1 == dev.cur()) {
#        Message(gettextRcmdr("There is no current graphics device to save."), type="error")
#        return()
#        }
#    initializeDialog(title=gettextRcmdr("Save Graph as PDF/Postscript"))
#    radioButtons(name="filetype", buttons=c("pdf", "postscript", "eps"),
#        labels=gettextRcmdr(c("PDF", "Postscript", "Encapsulated Postscript")), title=gettextRcmdr("Graphics File Type"))
#    sliderFrame <- tkframe(top)
#    widthVariable <- tclVar("5")
#    widthSlider <- tkscale(sliderFrame, from=3, to=10, showvalue=TRUE, variable=widthVariable,
#        resolution=0.1, orient="horizontal")
#    heightVariable <- tclVar("5")
#    heightSlider <- tkscale(sliderFrame, from=3, to=10, showvalue=TRUE, variable=heightVariable,
#        resolution=0.1, orient="horizontal")
#    pointSizeVariable <- tclVar("10")
#    pointSizeSlider <- tkscale(sliderFrame, from=6, to=14, showvalue=TRUE, variable=pointSizeVariable,
#        resolution=1, orient="horizontal")
#    onOK <- function(){
#        closeDialog()
#        width <- tclvalue(widthVariable)
#        height <- tclvalue(heightVariable)
#        type <- tclvalue(filetypeVariable)
#        pointsize <- tclvalue(pointSizeVariable)
#        if (type == "pdf"){
#            ext <- "pdf"
#            filetypes <- gettextRcmdr('{"All Files" {"*"}} {"PDF Files" {".pdf" ".PDF"}}')
#            initial <- "RGraph.pdf"
#            }
#        else if (type == "postscript") {
#            ext <- "ps"
#            filetypes <- gettextRcmdr('{"All Files" {"*"}} {"Postscript Files" {".ps" ".PS"}}')
#            initial <- "RGraph.ps"
#            }
#        else {
#            ext <- "eps"
#            filetypes <- gettextRcmdr('{"All Files" {"*"}} {"Encapsulated Postscript Files" {".eps" ".EPS"}}')
#            initial <- "RGraph.eps"
#            }
#        filename <- tclvalue(tkgetSaveFile(filetypes=filetypes,
#                                           defaultextension=ext,
#                                           initialfile=initial,
#                                           parent=CommanderWindow()))
#        if (filename == "") return()
#        command <- if (type == "eps") paste('dev.copy2eps(file="', filename, '", width=', width, ', height=', height,
#                ', pointsize=', pointsize, ')', sep="")
#            else paste('dev.print(', type, ', file="', filename, '", width=', width, ', height=', height,
#                ', pointsize=', pointsize, ')', sep="")
#        doItAndPrint(command)
#        Message(paste(gettextRcmdr("Graph saved to file"), filename), type="note")
#        }
#    OKCancelHelp(helpSubject="pdf")
#    tkgrid(filetypeFrame, sticky="w")
#    tkgrid(labelRcmdr(sliderFrame, text=gettextRcmdr("Width (inches)")), widthSlider, sticky="sw")
#    tkgrid(labelRcmdr(sliderFrame, text=gettextRcmdr("Height (inches)")), heightSlider, sticky="sw")
#    tkgrid(labelRcmdr(sliderFrame, text=gettextRcmdr("Text size (points)")), pointSizeSlider, sticky="sw")
#    tkgrid(sliderFrame, sticky="w")
#    tkgrid(buttonsFrame, sticky="w")
#    dialogSuffix(rows=3, columns=1)
#    }

savePDF <- function () {
	if (1 == dev.cur()) {
		Message(gettextRcmdr("There is no current graphics device to save."), 
				type = "error")
		return()
	}
	defaults <- list (initial.width = 5.0, initial.height = 5.0, initial.type = "pdf", initial.pointsize = 10)
	dialog.values <- getDialog ("savePDF", defaults)
	initializeDialog(title = gettextRcmdr("Save Graph as PDF/Postscript"))
	radioButtons(name = "filetype", buttons = c("pdf", "postscript", 
					"eps"), labels = gettextRcmdr(c("PDF", "Postscript", 
							"Encapsulated Postscript")), title = gettextRcmdr("Graphics File Type"), 
			initialValue = dialog.values$initial.type)
	sliderFrame <- tkframe(top)
	widthVariable <- tclVar(dialog.values$initial.width)
	widthSlider <- tkscale(sliderFrame, from = 3, to = 10, showvalue = TRUE, 
			variable = widthVariable, resolution = 0.1, orient = "horizontal")
	heightVariable <- tclVar(dialog.values$initial.height)
	heightSlider <- tkscale(sliderFrame, from = 3, to = 10, showvalue = TRUE, 
			variable = heightVariable, resolution = 0.1, orient = "horizontal")
	pointSizeVariable <- tclVar(dialog.values$initial.pointsize)
	pointSizeSlider <- tkscale(sliderFrame, from = 6, to = 14, 
			showvalue = TRUE, variable = pointSizeVariable, resolution = 1, 
			orient = "horizontal")
	onOK <- function() {
		closeDialog()
		width <- tclvalue(widthVariable)
		height <- tclvalue(heightVariable)
		type <- tclvalue(filetypeVariable)
		pointsize <- tclvalue(pointSizeVariable)
		putDialog ("savePDF", list (initial.width = width, initial.height = height, initial.type = type,
						initial.pointsize = pointsize))
		if (type == "pdf") {
			ext <- "pdf"
			filetypes <- gettextRcmdr("{\"All Files\" {\"*\"}} {\"PDF Files\" {\".pdf\" \".PDF\"}}")
			initial <- "RGraph.pdf"
		}
		else if (type == "postscript") {
			ext <- "ps"
			filetypes <- gettextRcmdr("{\"All Files\" {\"*\"}} {\"Postscript Files\" {\".ps\" \".PS\"}}")
			initial <- "RGraph.ps"
		}
		else {
			ext <- "eps"
			filetypes <- gettextRcmdr("{\"All Files\" {\"*\"}} {\"Encapsulated Postscript Files\" {\".eps\" \".EPS\"}}")
			initial <- "RGraph.eps"
		}
		filename <- tclvalue(tkgetSaveFile(filetypes = filetypes, 
						defaultextension = ext, initialfile = initial, parent = CommanderWindow()))
		if (filename == "") 
			return()
		command <- if (type == "eps") 
					paste("dev.copy2eps(file=\"", filename, "\", width=", 
							width, ", height=", height, ", pointsize=", pointsize, 
							")", sep = "")
				else paste("dev.print(", type, ", file=\"", filename, 
							"\", width=", width, ", height=", height, ", pointsize=", 
							pointsize, ")", sep = "")
		doItAndPrint(command)
		Message(paste(gettextRcmdr("Graph saved to file"), filename), 
				type = "note")
	}
	OKCancelHelp(helpSubject = "pdf", reset = "savePDF")
	tkgrid(filetypeFrame, sticky = "w")
	tkgrid(labelRcmdr(sliderFrame, text = gettextRcmdr("Width (inches)")), 
			widthSlider, sticky = "sw")
	tkgrid(labelRcmdr(sliderFrame, text = gettextRcmdr("Height (inches)")), 
			heightSlider, sticky = "sw")
	tkgrid(labelRcmdr(sliderFrame, text = gettextRcmdr("Text size (points)")), 
			pointSizeSlider, sticky = "sw")
	tkgrid(sliderFrame, sticky = "w")
	tkgrid(buttonsFrame, sticky = "w")
	dialogSuffix(rows = 3, columns = 1)
}

saveRglGraph <- function(){
    if (0 == rgl.cur()) {
        Message(message=gettextRcmdr("There is no current RGL graphics device to save."),
            type="error")
        return()
        }
    ext <- "png"
    filetypes <- gettextRcmdr('{"All Files" {"*"}} {"PNG Files" {".png" ".PNG"}}')
    initial <- "RGLGraph.png"
    filename <- tclvalue(tkgetSaveFile(filetypes=filetypes,
                                       defaultextension=ext,
                                       initialfile=initial,
                                       parent=CommanderWindow()))
    if (filename == "") return()
    command <- paste('rgl.snapshot("', filename, '")', sep="")
    doItAndPrint(command)
    Message(paste(gettextRcmdr("Graph saved to file"), filename), type="note")
    }

# The following function by Richard Heiberger, with small modifications by J. Fox

## The following function by Richard Heiberger, with small modifications by J. Fox
## with more modifications by Richard Heiberger.
## 2008-01-03 added conditions, layout, and multiple colors
#Xyplot <- function() {
#	Library("lattice")
#	initializeDialog(title=gettextRcmdr("XY Conditioning Plot"))
#	predictorFrame <- tkframe(top)
#	predictorBox <- variableListBox(predictorFrame, Numeric(), title=gettextRcmdr("Explanatory variables (pick one or more)"), selectmode="multiple")
#	responseBox <- variableListBox(predictorFrame, Numeric(), title=gettextRcmdr("Response variables (pick one or more)"), selectmode="multiple")
#	cgFrame <- tkframe(top)
#	conditionsBox <- variableListBox(cgFrame, Factors(), title=gettextRcmdr("Conditions '|' (pick zero or more)"), selectmode="multiple", initialSelection=FALSE)
#	groupsBox <- variableListBox(cgFrame, Factors(), title=gettextRcmdr("Groups 'groups=' (pick zero or more)"), selectmode="multiple", initialSelection=FALSE)
#	checkBoxes(frame="optionsFrame",
#			boxes=c("auto.key", "outer"),
#			initialValues=c(1,0),
#			labels=gettextRcmdr(c("Automatically draw key", 
#							"Different panels for different y~x combinations")))
#	relationFrame <- tkframe(top)
#	radioButtons(window=relationFrame,
#			name="x.relation",
#			buttons=c("same", "free", "sliced"),
#			labels=gettextRcmdr(c("Identical", "Free", "Same range")),
#			title=gettextRcmdr("X-Axis Scales in Different Panels"))
#	radioButtons(window=relationFrame,
#			name="y.relation",
#			buttons=c("same", "free", "sliced"),
#			labels=gettextRcmdr(c("Identical", "Free", "Same range")),
#			title=gettextRcmdr("Y-Axis Scales in Different Panels"))
#	
#	scalarsFrame <- tkframe(top)
#	
#	layoutColumnsVar <- tclVar("")
#	layoutColumnsEntry <- tkentry(scalarsFrame, width="6", textvariable=layoutColumnsVar)
#	layoutRowsVar <- tclVar("")
#	layoutRowsEntry <- tkentry(scalarsFrame, width="6", textvariable=layoutRowsVar)
#	
#	onOK <- function() {
#		predictor <- getSelection(predictorBox)
#		response <- getSelection(responseBox)
#		conditions <- getSelection(conditionsBox)
#		groups <- getSelection(groupsBox)
#		closeDialog()
#		if (0 == length(response)) {
#			errorCondition(recall=Xyplot.HH, message=gettextRcmdr("At least one response variable must be selected."))
#			return()
#		}
#		if (0 == length(predictor)) {
#			errorCondition(recall=Xyplot.HH, message=gettextRcmdr("At least one explanatory variable must be selected."))
#			return()
#		}
#		auto.key <- ("1" == tclvalue(auto.keyVariable))
#		outer    <- ("1" == tclvalue(outerVariable))
#		x.relation <- as.character(tclvalue(x.relationVariable))
#		y.relation <- as.character(tclvalue(y.relationVariable))
#		
#		layoutColumns  <- as.numeric(tclvalue(layoutColumnsVar))
#		layoutRows     <- as.numeric(tclvalue(layoutRowsVar))
#		layout.command <- ""
#		number.na <- is.na(layoutColumns) + is.na(layoutRows)
#		
#		if (number.na==1) {
#			errorCondition(recall=Xyplot.HH,
#					message=gettextRcmdr("Both or neither layout values must be numbers."))
#			return()
#		}
#		if (number.na==0) layout.command <- deparse(c(layoutColumns, layoutRows))
#		
#		.activeDataSet <- ActiveDataSet()
#		
#		
#		
#		condtions.command <-
#				if (length(conditions)==0) {
#					if (outer) {
#						if (layout.command=="")
#							paste(", layout=c(",
#									length(predictor),
#									",",
#									length(response),
#									")")
#						else
#							paste(", layout=", layout.command, sep="")
#					}
#				}
#				else {  ## (length(conditions)>0)
#					if (outer) {
#						condition.levels <- prod(sapply(conditions, d.f=get(.activeDataSet),
#										function(g, d.f) length(levels(d.f[[g]]))))
#						paste(", layout=c(",
#								condition.levels,
#								"*",
#								length(predictor),
#								",",
#								length(response),
#								")",
#								## ", between=list(x=c(0,0, 1, 0,0), y=1)",
#								", between=list(x=c(",
#								paste(rep(c(rep(0, condition.levels-1), 1),
#												length=condition.levels*length(predictor)-1),
#										collapse=","),
#								"), y=1)")
#					}
#				}
#		
#		groups.command <-
#				if (length(groups)==1) paste(", groups=", groups, sep="")
#				else ""
#		
#		xyplot.command <- paste("xyplot(",
#				paste(response, collapse=' + '),
#				" ~ ",
#				paste(predictor, collapse=' + '),
#				if (length(conditions) > 0)
#							paste(" | ",
#									paste(conditions, collapse=' + ')
#							) else "",
#				if (outer) ",\n outer=TRUE",
#				condtions.command,
#				groups.command,
#				", pch=16",
#				if (auto.key) ",\n auto.key=list(border=TRUE), par.settings = simpleTheme(pch=16)" else "",
#				paste(", scales=list(x=list(relation='",
#						x.relation,
#						"'), y=list(relation='",
#						y.relation,
#						"'))", sep=""),
#				",\n data=", .activeDataSet, ')', sep="")
#		doItAndPrint(xyplot.command)
#		activateMenus()
#		tkfocus(CommanderWindow())
#	}
#	OKCancelHelp(helpSubject="xyplot")
#	tkgrid(getFrame(predictorBox), getFrame(responseBox),
#			columnspan=1, sticky="w")
#	tkgrid(predictorFrame, sticky="w")
#	tkgrid(getFrame(conditionsBox),
#			tklabel(cgFrame, text=gettextRcmdr("           ")),
#			getFrame(groupsBox),
#			columnspan=1, sticky="w")
#	tkgrid(cgFrame, sticky="w")
#	tkgrid(tklabel(top, text=gettextRcmdr("Options"), fg="blue"), sticky="w")
#	tkgrid(optionsFrame, sticky="w")
#	tkgrid(x.relationFrame, y.relationFrame, columnspan=2, sticky="w")
#	tkgrid(relationFrame, sticky="w")
#	tkgrid(tklabel(top, text=gettextRcmdr("Layout"), fg="blue"), sticky="w")
#	tkgrid(tklabel(scalarsFrame, text=gettextRcmdr("number of columns:")), layoutColumnsEntry, sticky="w")
#	tkgrid(tklabel(scalarsFrame, text=gettextRcmdr("number of rows:")), layoutRowsEntry, sticky="w")
#	tkgrid(scalarsFrame, sticky="w")
#	tkgrid(buttonsFrame, columnspan=2, sticky="w")
#	dialogSuffix(rows=6, columns=2)
#}

Xyplot <- function () {
	Library("lattice")
	defaults <- list(initial.predictor = NULL, initial.response = NULL, initial.auto.key = 1, 
			initial.outer = 0, initial.x.relation = "same", initial.y.relation = "same",
			initial.layoutColumns = "", initial.layoutRows = "", initial.conditions = FALSE,
			initial.groups = FALSE) 
	dialog.values <- getDialog("Xyplot", defaults)
	initializeDialog(title = gettextRcmdr("XY Conditioning Plot"))
	predictorFrame <- tkframe(top)
	predictorBox <- variableListBox(predictorFrame, Numeric(), 
			title = gettextRcmdr("Explanatory variables (pick one or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.predictor, "numeric"))
	responseBox <- variableListBox(predictorFrame, Numeric(), 
			title = gettextRcmdr("Response variables (pick one or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.response, "numeric"))
	cgFrame <- tkframe(top)
	conditionsBox <- variableListBox(cgFrame, Factors(), title = gettextRcmdr("Conditions '|' (pick zero or more)"), 
			selectmode = "multiple", 
			initialSelection = if (dialog.values$initial.conditions == FALSE) FALSE else varPosn (dialog.values$initial.conditions, "factor"))
	groupsBox <- variableListBox(cgFrame, Factors(), title = gettextRcmdr("Groups 'groups=' (pick zero or more)"), 
			selectmode = "multiple", 
			initialSelection = if (dialog.values$initial.groups == FALSE) FALSE else varPosn (dialog.values$initial.groups, "factor"))
	checkBoxes(frame = "optionsFrame", boxes = c("auto.key", 
					"outer"), initialValues = c(dialog.values$initial.auto.key, dialog.values$initial.outer), 
			labels = gettextRcmdr(c("Automatically draw key", "Different panels for different y~x combinations")))
	relationFrame <- tkframe(top)
	radioButtons(window = relationFrame, name = "x.relation", 
			buttons = c("same", "free", "sliced"), labels = gettextRcmdr(c("Identical", 
							"Free", "Same range")), title = gettextRcmdr("X-Axis Scales in Different Panels"), 
			initialValue = dialog.values$initial.x.relation)
	radioButtons(window = relationFrame, name = "y.relation", 
			buttons = c("same", "free", "sliced"), labels = gettextRcmdr(c("Identical", 
							"Free", "Same range")), title = gettextRcmdr("Y-Axis Scales in Different Panels"), 
			initialValue = dialog.values$initial.y.relation)
	scalarsFrame <- tkframe(top)
	layoutColumnsVar <- tclVar(dialog.values$initial.layoutColumns)
	layoutColumnsEntry <- tkentry(scalarsFrame, width = "6", 
			textvariable = layoutColumnsVar)
	layoutRowsVar <- tclVar(dialog.values$initial.layoutRows)
	layoutRowsEntry <- tkentry(scalarsFrame, width = "6", textvariable = layoutRowsVar)
	onOK <- function() {
		predictor <- getSelection(predictorBox)
		response <- getSelection(responseBox)
		conditions <- getSelection(conditionsBox)
		groups <- getSelection(groupsBox)
		closeDialog()
		if (0 == length(response)) {
			errorCondition(recall = Xyplot.HH, message = gettextRcmdr("At least one response variable must be selected."))
			return()
		}
		if (0 == length(predictor)) {
			errorCondition(recall = Xyplot.HH, message = gettextRcmdr("At least one explanatory variable must be selected."))
			return()
		}
		auto.key <- ("1" == tclvalue(auto.keyVariable))
		outer <- ("1" == tclvalue(outerVariable))
		x.relation <- as.character(tclvalue(x.relationVariable))
		y.relation <- as.character(tclvalue(y.relationVariable))
		layoutColumns <- as.numeric(tclvalue(layoutColumnsVar))
		layoutRows <- as.numeric(tclvalue(layoutRowsVar))
		putDialog ("Xyplot", list(initial.predictor = predictor, initial.response = response, 
						initial.auto.key = auto.key, initial.outer = outer, initial.x.relation = x.relation, 
						initial.y.relation = y.relation, initial.layoutColumns = tclvalue(layoutColumnsVar), 
						initial.layoutRows = tclvalue(layoutRowsVar), initial.conditions = if (length(conditions) != 0) conditions else FALSE, 
						initial.groups = if (length(groups) != 0) groups else FALSE))
		layout.command <- ""
		number.na <- is.na(layoutColumns) + is.na(layoutRows)
		if (number.na == 1) {
			errorCondition(recall = Xyplot.HH, message = gettextRcmdr("Both or neither layout values must be numbers."))
			return()
		}
		if (number.na == 0) 
			layout.command <- deparse(c(layoutColumns, layoutRows))
		.activeDataSet <- ActiveDataSet()
		condtions.command <- if (length(conditions) == 0) {
					if (outer) {
						if (layout.command == "") 
							paste(", layout=c(", length(predictor), ",", 
									length(response), ")")
						else paste(", layout=", layout.command, sep = "")
					}
				}
				else {
					if (outer) {
						condition.levels <- prod(sapply(conditions, d.f = get(.activeDataSet), 
										function(g, d.f) length(levels(d.f[[g]]))))
						paste(", layout=c(", condition.levels, "*", length(predictor), 
								",", length(response), ")", ", between=list(x=c(", 
								paste(rep(c(rep(0, condition.levels - 1), 1), 
												length = condition.levels * length(predictor) - 
														1), collapse = ","), "), y=1)")
					}
				}
		groups.command <- if (length(groups) == 1) 
					paste(", groups=", groups, sep = "")
				else ""
		xyplot.command <- paste("xyplot(", paste(response, collapse = " + "), 
				" ~ ", paste(predictor, collapse = " + "), if (length(conditions) > 
								0) 
							paste(" | ", paste(conditions, collapse = " + "))
						else "", if (outer) 
					",\n outer=TRUE", condtions.command, groups.command, 
				", pch=16", if (auto.key) 
							",\n auto.key=list(border=TRUE), par.settings = simpleTheme(pch=16)"
						else "", paste(", scales=list(x=list(relation='", 
						x.relation, "'), y=list(relation='", y.relation, 
						"'))", sep = ""), ",\n data=", .activeDataSet, 
				")", sep = "")
		doItAndPrint(xyplot.command)
		activateMenus()
		tkfocus(CommanderWindow())
	}
	OKCancelHelp(helpSubject = "xyplot", reset = "Xyplot")
	tkgrid(getFrame(predictorBox), getFrame(responseBox), columnspan = 1, 
			sticky = "w")
	tkgrid(predictorFrame, sticky = "w")
	tkgrid(getFrame(conditionsBox), tklabel(cgFrame, text = gettextRcmdr("           ")), 
			getFrame(groupsBox), columnspan = 1, sticky = "w")
	tkgrid(cgFrame, sticky = "w")
	tkgrid(tklabel(top, text = gettextRcmdr("Options"), fg = "blue"), 
			sticky = "w")
	tkgrid(optionsFrame, sticky = "w")
	tkgrid(x.relationFrame, y.relationFrame, columnspan = 2, 
			sticky = "w")
	tkgrid(relationFrame, sticky = "w")
	tkgrid(tklabel(top, text = gettextRcmdr("Layout"), fg = "blue"), 
			sticky = "w")
	tkgrid(tklabel(scalarsFrame, text = gettextRcmdr("number of columns:")), 
			layoutColumnsEntry, sticky = "w")
	tkgrid(tklabel(scalarsFrame, text = gettextRcmdr("number of rows:")), 
			layoutRowsEntry, sticky = "w")
	tkgrid(scalarsFrame, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 6, columns = 2)
}


# set the colour palette

setPalette <- function() {
    cval <- function(x,y) -sum((x-y)^2)
    contrasting <- function(x)
        optim(rep(127, 3),cval,lower=0,upper=255,method="L-BFGS-B",y=x)$par
    # the following local function from Thomas Lumley via r-help
    convert <- function (color){
        rgb <- col2rgb(color)/255
        L <- c(0.2, 0.6, 0) %*% rgb
        ifelse(L >= 0.2, "#000060", "#FFFFA0")
        }
    env <- environment()
    pal <- palette()
    pickColor <- function(initialcolor, parent){
        tclvalue(.Tcl(paste("tk_chooseColor", .Tcl.args(title = "Select a Color",
            initialcolor=initialcolor, parent=parent))))
        }
    initializeDialog(title=gettextRcmdr("Set Color Palette"))
    hexcolor <- colorConverter(toXYZ = function(hex,...) {
        rgb <- t(col2rgb(hex))/255
        colorspaces$sRGB$toXYZ(rgb,...) },
        fromXYZ = function(xyz,...) {
            rgb <- colorspaces$sRGB$fromXYZ(xyz,..)
            rgb <- round(rgb,5)
            if (min(rgb) < 0 || max(rgb) > 1) as.character(NA)
            else rgb(rgb[1],rgb[2],rgb[3])},
            white = "D65", name = "#rrggbb")
    cols <- t(col2rgb(pal))
    hex <- convertColor(cols, from="sRGB", to=hexcolor, scale.in=255, scale.out=NULL)
    for (i in 1:8) assign(paste("hex", i, sep="."), hex[i], envir=env)
    paletteFrame <- tkframe(top)
    button1 <- tkbutton(paletteFrame, text=hex[1], bg = hex[1],
        fg=convert(hex[1]),
        command=function() {
            color <- pickColor(hex[1], parent=button1)
            fg <- convert(color)
            tkconfigure(button1, bg=color, fg=fg)
            assign("hex.1", color, envir=env)
            }
        )
    button2 <- tkbutton(paletteFrame, text=hex[2], bg = hex[2],
        fg=convert(hex[2]),
        command=function() {
            color <- pickColor(hex[2], parent=button2)
            fg <- convert(color)
            tkconfigure(button2, bg=color, fg=fg)
            assign("hex.2", color, envir=env)
            }
        )
     button3 <- tkbutton(paletteFrame, text=hex[3], bg = hex[3],
        fg=convert(hex[3]),
        command=function() {
            color <- pickColor(hex[3], parent=button3)
            fg <- convert(color)
            tkconfigure(button3, bg=color, fg=fg)
            assign("hex.3", color, envir=env)
            }
        )
     button4 <- tkbutton(paletteFrame, text=hex[4], bg = hex[4],
        fg=convert(hex[4]),
        command=function() {
            color <- pickColor(hex[4], parent=button4)
            fg <- convert(color)
            tkconfigure(button4, bg=color, fg=fg)
            assign("hex.4", color, envir=env)
            }
        )
     button5 <- tkbutton(paletteFrame, text=hex[5], bg = hex[5],
        fg=convert(hex[5]),
        command=function() {
            color <- pickColor(hex[5], parent=button5)
            fg <- convert(color)
            tkconfigure(button5, bg=color, fg=fg)
            assign("hex.5", color, envir=env)
            }
        )
     button6 <- tkbutton(paletteFrame, text=hex[6], bg = hex[6],
        fg=convert(hex[6]),
        command=function() {
            color <- pickColor(hex[6], parent=button6)
            fg <- convert(color)
            tkconfigure(button6, bg=color, fg=fg)
            assign("hex.6", color, envir=env)
            }
        )
     button7 <- tkbutton(paletteFrame, text=hex[7], bg = hex[7],
        fg=convert(hex[7]),
        command=function() {
            color <- pickColor(hex[7], parent=button7)
            fg <- convert(color)
            tkconfigure(button7, bg=color, fg=fg)
            assign("hex.7", color, envir=env)
            }
        )
     button8 <- tkbutton(paletteFrame, text=hex[8], bg = hex[8],
        fg=convert(hex[8]),
        command=function() {
            color <- pickColor(hex[8], parent=button8)
            fg <- convert(color)
            tkconfigure(button8, bg=color, fg=fg)
            assign("hex.8", color, envir=env)
            }
        )
     onOK <- function(){
        closeDialog(top)
        palette(c(hex.1, hex.2, hex.3, hex.4, hex.5, hex.6, hex.7, hex.8))
        Message(gettextRcmdr("Color palette reset.", type="note"))
        }
    OKCancelHelp(helpSubject="palette")
    tkgrid(button1, button2, button3, button4, button5, button6, button7, button8)
    tkgrid(paletteFrame)
    tkgrid(buttonsFrame, sticky="w")
    dialogSuffix(rows=2)
    }
	
#stripChart <- function(){
#	initializeDialog(title=gettextRcmdr("Strip Chart"))
#	groupBox <- variableListBox(top, Factors(), title=gettextRcmdr("Factors (pick zero or more)"), selectmode="multiple")
#	responseBox <- variableListBox(top, Numeric(), title=gettextRcmdr("Response Variable (pick one)"))
#	onOK <- function(){
#		groups <- getSelection(groupBox)
#		response <- getSelection(responseBox)
#		closeDialog()
#		if (0 == length(response)) {
#			errorCondition(recall=stripChart, message=gettextRcmdr("No response variable selected."))
#			return()
#		}
#		.activeDataSet <- ActiveDataSet()
#		plotType <- tclvalue(plotTypeVariable)
#		method <- paste(', method="', plotType, '"', sep="")
#		if (length(groups) == 0) doItAndPrint(paste("stripchart(", .activeDataSet, "$", response,
#							method, ', xlab="', response, '")', sep=""))
#		else {
#			groupNames <- paste(groups, collapse="*")
#			doItAndPrint(paste('stripchart(', response, ' ~ ', groupNames,
#							', vertical=TRUE', method, ', xlab="', groupNames, '", ylab="', response,
#							'", data=', .activeDataSet, ')', sep=""))
#		}
#		activateMenus()
#		tkfocus(CommanderWindow())
#	}
#	radioButtons(name="plotType", buttons=c("stack", "jitter"), labels=gettextRcmdr(c("Stack", "Jitter")), title=gettextRcmdr("Duplicate Values"))
#	buttonsFrame <- tkframe(top)
#	OKCancelHelp(helpSubject="stripchart")
#	tkgrid(getFrame(groupBox), getFrame(responseBox), sticky="nw")
#	tkgrid(plotTypeFrame, sticky="w")
#	tkgrid(buttonsFrame, columnspan=2, sticky="w")
#	dialogSuffix(rows=3, columns=2)
#}

stripChart <- function () {
	defaults <- list (initial.group = NULL, initial.response = NULL, initial.plotType = "stack")
	dialog.values <- getDialog("stripChart", defaults)
	initializeDialog(title = gettextRcmdr("Strip Chart"))
	groupBox <- variableListBox(top, Factors(), title = gettextRcmdr("Factors (pick zero or more)"), 
			selectmode = "multiple", initialSelection = varPosn (dialog.values$initial.group, "factor"))
	responseBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Response Variable (pick one)"), 
			initialSelection = varPosn (dialog.values$initial.response, "numeric"))
	onOK <- function() {
		groups <- getSelection(groupBox)
		response <- getSelection(responseBox)
		closeDialog()
		if (0 == length(response)) {
			errorCondition(recall = stripChart, message = gettextRcmdr("No response variable selected."))
			return()
		}
		.activeDataSet <- ActiveDataSet()
		plotType <- tclvalue(plotTypeVariable)
		putDialog ("stripChart", list (initial.group = groups, initial.response = response, 
						initial.plotType = plotType))
		method <- paste(", method=\"", plotType, "\"", sep = "")
		if (length(groups) == 0) 
			doItAndPrint(paste("stripchart(", .activeDataSet, 
							"$", response, method, ", xlab=\"", response, 
							"\")", sep = ""))
		else {
			groupNames <- paste(groups, collapse = "*")
			doItAndPrint(paste("stripchart(", response, " ~ ", 
							groupNames, ", vertical=TRUE", method, ", xlab=\"", 
							groupNames, "\", ylab=\"", response, "\", data=", 
							.activeDataSet, ")", sep = ""))
		}
		activateMenus()
		tkfocus(CommanderWindow())
	}
	radioButtons(name = "plotType", buttons = c("stack", "jitter"), 
			labels = gettextRcmdr(c("Stack", "Jitter")), title = gettextRcmdr("Duplicate Values"), 
			initialValue = dialog.values$initial.plotType)
	buttonsFrame <- tkframe(top)
	OKCancelHelp(helpSubject = "stripchart", reset = "stripChart")
	tkgrid(getFrame(groupBox), getFrame(responseBox), sticky = "nw")
	tkgrid(plotTypeFrame, sticky = "w")
	tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
	dialogSuffix(rows = 3, columns = 2)
}


