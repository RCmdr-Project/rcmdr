# Distributions menu dialogs

# last modified 17 September 2008 by J. Fox

#   many distributions added (and some other changes) by Miroslav Ristic (20 July 06)

normalQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Normal Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    muVar <- tclVar("0")
    muEntry <- ttkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- ttkentry(top, width="6", textvariable=sigmaVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=normalQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
		warn <- options(warn=-1)
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
		options(warn)
		if (is.na(mu)) {
			errorCondition(recall=normalQuantiles, message=gettextRcmdr("Mean not specified."))
			return()
			}
        if (is.na(sigma) || sigma <= 0) {
            errorCondition(recall=normalQuantiles, message=gettextRcmdr("Standard deviation must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qnorm(c(", quantiles, "), mean=", mu,
            ", sd=", sigma, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qnorm")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("mu (mean)")), muEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("sigma (standard deviation)")), sigmaEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

normalProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Normal Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    muVar <- tclVar("0")
    muEntry <- ttkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- ttkentry(top, width="6", textvariable=sigmaVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=normalProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
		options(warn)
		if (is.na(mu)) {
			errorCondition(recall=normalQuantiles, message=gettextRcmdr("Mean not specified."))
			return()
		}
        if (is.na(sigma) || sigma <= 0) {
            errorCondition(recall=normalProbabilities, message=gettextRcmdr("Standard deviation must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pnorm(c(", probabilities, "), mean=", mu, 
            ", sd=", sigma, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pnorm")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("mu (mean)")), muEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("sigma (standard deviation)")), sigmaEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }
    
tQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("t Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    dfVar <- tclVar("")
    dfEntry <- ttkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=tQuantiles, message=gettextRcmdr("No probabilities specified.")) 
            return()
            }
		warn <- options(warn=-1)
        df <- as.numeric(tclvalue(dfVar))
		options(warn)
        if (is.na(df)) {
            errorCondition(recall=tQuantiles, message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df <= 0) {
            errorCondition(recall=tQuantiles, message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qt(c(", quantiles, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qt")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=quantilesEntry)
    }
    
tProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("t Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    dfVar <- tclVar("")
    dfEntry <- ttkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
		warn <- options(warn=-1)
        df <- as.numeric(tclvalue(dfVar))
		options(warn)
        if ("" == probabilities) {
            errorCondition(recall=tProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
        if (is.na(df)) {
            errorCondition(recall=tProbabilities, message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df <= 0) {
            errorCondition(recall=tProbabilities, message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pt(c(", probabilities, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pt")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=probabilitiesEntry)
    }

chisqQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Chi-Squared Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    dfVar <- tclVar("")
    dfEntry <- ttkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=chisqQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
		warn <- options(warn=-1)
        df <- as.numeric(tclvalue(dfVar))
		options(warn)
        if (is.na(df)) {
            errorCondition(recall=chisqQuantiles, message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df <= 0) {
            errorCondition(recall=chisqQuantiles, message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qchisq(c(", quantiles, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qchisq")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=quantilesEntry)
    }
    
chisqProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Chi-Squared Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    dfVar <- tclVar("")
    dfEntry <- ttkentry(top, width="6", textvariable=dfVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=chisqProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        df <- as.numeric(tclvalue(dfVar))
		options(warn)
        if (is.na(df)) {
            errorCondition(recall=chisqProbabilities, message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df <= 0) {
            errorCondition(recall=chisqProbabilities, message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pchisq(c(", probabilities, "), df=", df, 
            ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pchisq")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(OKbutton, cancelButton, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=probabilitiesEntry)
    }

FQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("F Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    df1Var <- tclVar("")
    df1Entry <- ttkentry(top, width="6", textvariable=df1Var)
    df2Var <- tclVar("")
    df2Entry <- ttkentry(top, width="6", textvariable=df2Var)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=FQuantiles, message=gettextRcmdr("Probabilities not specified"))
            return()
            }
		warn <- options(warn=-1)
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
		options(warn)
        if (is.na(df1) || is.na(df2)) {
            errorCondition(recall=FQuantiles, message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df1 <= 0 || df2 <= 0) {
            errorCondition(recall=FQuantiles, message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qf(c(", quantiles, "), df1=", df1, 
            ", df2=", df2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qf")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Numerator degrees of freedom")), df1Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Denominator degrees of freedom")), df2Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }
    
FProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("F Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    df1Var <- tclVar("")
    df1Entry <- ttkentry(top, width="6", textvariable=df1Var)
    df2Var <- tclVar("")
    df2Entry <- ttkentry(top, width="6", textvariable=df2Var)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=FProbabilities, message=gettextRcmdr("Values not specified."))
            return()
            }
		warn <- options(warn=-1)
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
		options(warn=-1)
        if (is.na(df1) || is.na(df2)) {
            errorCondition(recall=FProbabilities, message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df1 <= 0 || df2 <= 0) {
            errorCondition(recall=FProbabilities, message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pf(c(", probabilities, "), df1=", df1, 
            ", df2=", df2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pf")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Numerator degrees of freedom")), df1Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Denominator degrees of freedom")), df2Entry, sticky="e")    
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=probabilitiesEntry)
    }
    
exponentialQuantiles<-function() { 
    initializeDialog(title=gettextRcmdr("Exponential Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    rateVar <- tclVar("1")
    rateEntry <- ttkentry(top, width="6", textvariable=rateVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=exponentialQuantiles, message=gettextRcmdr("Probabilities not specified."))
            return()
            }
		warn <- options(warn=-1)
        rate <- as.numeric(tclvalue(rateVar))
		options(warn)
        if (is.na(rate) || rate <= 0) {
            errorCondition(recall=exponentialQuantiles, message=gettextRcmdr("Rate must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qexp(c(", quantiles, "), rate=", rate, ", lower.tail=", tail == "lower", ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qexp")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Rate")), rateEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(rateEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=quantilesEntry)
    }
    
exponentialProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Exponential Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    rateVar <- tclVar("1")
    rateEntry <- ttkentry(top, width="6", textvariable=rateVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=exponentialProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        rate <- as.numeric(tclvalue(rateVar))
		options(warn)
        if (is.na(rate) || rate <= 0) {
            errorCondition(recall=exponentialProbabilities, message=gettextRcmdr("Rate must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pexp(c(", probabilities, "), rate=", rate, ", lower.tail=", tail == "lower", ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pexp")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Rate")), rateEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(rateEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }
    
uniformQuantiles<-function() { 
    initializeDialog(title=gettextRcmdr("Uniform Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    minVar <- tclVar("0")
    maxVar <- tclVar("1")
    minEntry <- ttkentry(top, width="6", textvariable=minVar)
    maxEntry <- ttkentry(top, width="6", textvariable=maxVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=uniformQuantiles, message=gettextRcmdr("Probabilities not specified."))
            return()
            }
		warn <- options(warn=-1)
        min <- as.numeric(tclvalue(minVar))
        max <- as.numeric(tclvalue(maxVar))
		options(warn)
        if (is.na(min) || is.na(max) || min >= max) {
            errorCondition(recall=uniformQuantiles, message=gettextRcmdr("Lower limit must be less than upper limit."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qunif(c(", quantiles, "), min=", min, ", max=", max, ", lower.tail=", tail == "lower", ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qunif")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Minimum")), minEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Maximum")), maxEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(minEntry, sticky="w")
    tkgrid.configure(maxEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=quantilesEntry)
    }
    
uniformProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Uniform Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    minVar <- tclVar("0")
    maxVar <- tclVar("1")
    minEntry <- ttkentry(top, width="6", textvariable=minVar)
    maxEntry <- ttkentry(top, width="6", textvariable=maxVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
		warn <- options(warn=-1)
        min <- as.numeric(tclvalue(minVar))
        max <- as.numeric(tclvalue(maxVar))
		options(warn)
        tail <- tclvalue(tailVar)
        if ("" == probabilities) {
            errorCondition(recall=uniformProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
        if (is.na(min) || is.na(max) || min >= max) {
            errorCondition(recall=uniformProbabilities, message=gettextRcmdr("Lower limit must be less than upper limit."))
            return()
            }
        doItAndPrint(paste("punif(c(", probabilities, "), min=", min, ", max=", max, ", lower.tail=", tail == "lower", ")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="punif")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Minimum")), minEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Maximum")), maxEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(minEntry, sticky="w")
    tkgrid.configure(maxEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }
    
betaQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Beta Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    shape1Var <- tclVar("")
    shape1Entry <- ttkentry(top, width="6", textvariable=shape1Var)
    shape2Var <- tclVar("")
    shape2Entry <- ttkentry(top, width="6", textvariable=shape2Var)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=betaQuantiles, message=gettextRcmdr("Probabilities not specified"))
            return()
            }
		warn <- options(warn=-1)
        shape1 <- as.numeric(tclvalue(shape1Var))
        shape2 <- as.numeric(tclvalue(shape2Var))
		options(warn)
        if (is.na(shape1) || is.na(shape2)) {
            errorCondition(recall=betaQuantiles, message=gettextRcmdr("Shapes not specified."))
            return()
            }
        if (shape1 <= 0 || shape2 <= 0) {
            errorCondition(recall=betaQuantiles, message=gettextRcmdr("Shapes must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qbeta(c(", quantiles, "), shape1=", shape1, 
            ", shape2=", shape2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qbeta")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=paste(gettextRcmdr("Shape"), "1")), shape1Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=paste(gettextRcmdr("Shape"), "2")), shape2Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(shape1Entry, sticky="w")
    tkgrid.configure(shape2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }
    
betaProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Beta Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    shape1Var <- tclVar("")
    shape1Entry <- ttkentry(top, width="6", textvariable=shape1Var)
    shape2Var <- tclVar("")
    shape2Entry <- ttkentry(top, width="6", textvariable=shape2Var)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=betaProbabilities, message=gettextRcmdr("Values not specified."))
            return()
            }
		warn <- options(warn=-1)
        shape1 <- as.numeric(tclvalue(shape1Var))
        shape2 <- as.numeric(tclvalue(shape2Var))
		options(warn)
        if (is.na(shape1) || is.na(shape2)) {
            errorCondition(recall=betaProbabilities, message=gettextRcmdr("Shapes not specified."))
            return()
            }
        if (shape1 <= 0 || shape2 <= 0) {
            errorCondition(recall=betaProbabilities, message=gettextRcmdr("Shapes must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pbeta(c(", probabilities, "), shape1=", shape1, 
            ", shape2=", shape2, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pbeta")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=paste(gettextRcmdr("Shape"), "1")), shape1Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=paste(gettextRcmdr("Shape"), "2")), shape2Entry, sticky="e")    
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(shape1Entry, sticky="w")
    tkgrid.configure(shape2Entry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=probabilitiesEntry)
    }
    
CauchyQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Cauchy Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    locationVar <- tclVar("0")
    locationEntry <- ttkentry(top, width="6", textvariable=locationVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=CauchyQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
		warn <- options(warn=-1)
        location <- as.numeric(tclvalue(locationVar))
        s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(s) || s <= 0) {
            errorCondition(recall=CauchyQuantiles, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qcauchy(c(", quantiles, "), location=", location,
            ", scale=", s, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qcauchy")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Location")), locationEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(locationEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

CauchyProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Cauchy Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    locationVar <- tclVar("0")
    locationEntry <- ttkentry(top, width="6", textvariable=locationVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=CauchyProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        location <- as.numeric(tclvalue(locationVar))
        s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(s) || s <= 0) {
            errorCondition(recall=CauchyProbabilities, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pcauchy(c(", probabilities, "), location=", location, 
            ", scale=", s, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pcauchy")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Location")), locationEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(locationEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }
    
logisticQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Logistic Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    locationVar <- tclVar("0")
    locationEntry <- ttkentry(top, width="6", textvariable=locationVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=logisticQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
		warn <- options(warn=-1)
        location <- as.numeric(tclvalue(locationVar))
        s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(s) || s <= 0) {
            errorCondition(recall=logisticQuantiles, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qlogis(c(", quantiles, "), location=", location,
            ", scale=", s, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qlogis")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Location")), locationEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(locationEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

logisticProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Logistic Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    locationVar <- tclVar("0")
    locationEntry <- ttkentry(top, width="6", textvariable=locationVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=logisticProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        location <- as.numeric(tclvalue(locationVar))
        s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(s) || s <= 0) {
            errorCondition(recall=logisticProbabilities, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("plogis(c(", probabilities, "), location=", location, 
            ", scale=", s, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="plogis")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Location")), locationEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(locationEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }
    
lognormalQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Lognormal Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    meanlogVar <- tclVar("0")
    meanlogEntry <- ttkentry(top, width="6", textvariable=meanlogVar)
    sdlogVar <- tclVar("1")
    sdlogEntry <- ttkentry(top, width="6", textvariable=sdlogVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=lognormalQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
		warn <- options(warn=-1)
        meanlog <- as.numeric(tclvalue(meanlogVar))
        sdlog <- as.numeric(tclvalue(sdlogVar))
		options(warn)
		if (is.na(meanlog)){
			errorCondition(recall=lognormalQuantiles, message=gettextRcmdr("Mean not specified."))
			return()
			}
        if (is.na(sdlog) || sdlog <= 0) {
            errorCondition(recall=lognormalQuantiles, message=gettextRcmdr("Standard deviation must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qlnorm(c(", quantiles, "), meanlog=", meanlog,
            ", sdlog=", sdlog, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qlnorm")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Mean (log scale)")), meanlogEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Standard deviation (log scale)")), sdlogEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(meanlogEntry, sticky="w")
    tkgrid.configure(sdlogEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

lognormalProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Lognormal Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    meanlogVar <- tclVar("0")
    meanlogEntry <- ttkentry(top, width="6", textvariable=meanlogVar)
    sdlogVar <- tclVar("1")
    sdlogEntry <- ttkentry(top, width="6", textvariable=sdlogVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=lognormalProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        meanlog <- as.numeric(tclvalue(meanlogVar))
        sdlog <- as.numeric(tclvalue(sdlogVar))
		options(warn)
		if (is.na(meanlog)) {
			errorCondition(recall=lognormalProbabilities, message=gettextRcmdr("Mean not specified."))
			return()
			}
	    if (is.na(sdlog) || sdlog <= 0) {
	        errorCondition(recall=lognormalProbabilities, message=gettextRcmdr("Scale must be positive."))
	        return()
	        }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("plnorm(c(", probabilities, "), meanlog=", meanlog, 
            ", sdlog=", sdlog, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="plnorm")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Mean (log scale)")), meanlogEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Standard deviation (log scale)")), sdlogEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(meanlogEntry, sticky="w")
    tkgrid.configure(sdlogEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }

gammaQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Gamma Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=gammaQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=gammaQuantiles, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=gammaQuantiles, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=gammaQuantiles, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qgamma(c(", quantiles, "), shape=", shape,
            ", scale=", s, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qgamma")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale (inverse rate)")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

gammaProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Gamma Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=gammaProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=gammaProbabilities, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=gammaProbabilities, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=gammaProbabilities, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pgamma(c(", probabilities, "), shape=", shape, 
            ", scale=", s, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pgamma")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale (inverse rate)")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }

WeibullQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Weibull Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=WeibullQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=WeibullQuantiles, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=WeibullQuantiles, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=WeibullQuantiles, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qweibull(c(", quantiles, "), shape=", shape,
            ", scale=", s, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qweibull")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

WeibullProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Weibull Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=WeibullProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=WeibullProbabilities, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=WeibullProbabilities, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=WeibullProbabilities, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pweibull(c(", probabilities, "), shape=", shape, 
            ", scale=", s, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pweibull")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }

GumbelQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Gumbel Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=GumbelQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=GumbelQuantiles, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=GumbelQuantiles, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=GumbelQuantiles, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("log(qweibull(c(", quantiles, "), shape=", shape,
            ", scale=", s, ", lower.tail=", tail == "lower",")) # Gumbel distribution", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qweibull")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape (log scale)")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale (log scale)")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

GumbelProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Gumbel Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=GumbelProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=GumbelProbabilities, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=GumbelProbabilities, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=GumbelProbabilities, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pweibull(exp(c(", probabilities, ")), shape=", shape, 
            ", scale=", s, ", lower.tail=", tail == "lower",") # Gumbel distribution", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pweibull")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape (log scale)")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale (log scale)")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }
    
    
binomialQuantiles <- function(){
    initializeDialog(title=gettextRcmdr("Binomial Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    trialsVar <- tclVar("")
    trialsEntry <- ttkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
		warn <- options(warn=-1)
        trials <- round(as.numeric(tclvalue(trialsVar)))
        prob <- as.numeric(tclvalue(probVar))
		options(warn)
        if ("" == quantiles) {
            errorCondition(recall=binomialQuantiles, message=gettextRcmdr("Probabilities not specified."))
            return()
            }
        if (is.na(trials)) {
            errorCondition(recall=binomialQuantiles, message=gettextRcmdr("Binomial trials not specified."))
            return()
            }
        if (is.na(prob)) {
            errorCondition(recall=binomialQuantiles, message=gettextRcmdr("Probability of success not specified."))
            return()
            }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=binomialQuantiles, message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qbinom(c(", quantiles, "), size=", trials, 
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qbinom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Binomial trials")), trialsEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame,columnspan=2, sticky="w")
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }
    
binomialProbabilities <- function(){
    initializeDialog(title=gettextRcmdr("Cumulative Binomial Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    trialsVar <- tclVar("")
    trialsEntry <- ttkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
		warn <- options(warn=-1)
        trials <- round(as.numeric(tclvalue(trialsVar)))
        prob <- as.numeric(tclvalue(probVar))
		options(warn)
        if ("" == probabilities) {
            errorCondition(recall=binomialProbabilities, message=gettextRcmdr("Values not specified.")) 
            return()
            }
        if (is.na(trials)) {
            errorCondition(recall=binomialProbabilities, message=gettextRcmdr("Binomial trials not specified."))
            return()
            }
        if (is.na(prob)) {
            errorCondition(recall=binomialProbabilities, message=gettextRcmdr("Probability of success not specified."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pbinom(c(", probabilities, "), size=", trials, 
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pbinom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Binomial trials")), trialsEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")    
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=probabilitiesEntry)
    }

binomialMass <- function(){
    checkTrials <- function(trials){
        RcmdrTkmessageBox(message=sprintf(gettextRcmdr("Number of trials, %d, is large.\nCreate long output?"), trials),
            icon="warning", type="yesno", default="no")
        }
    initializeDialog(title=gettextRcmdr("Binomial Probabilities"))
    trialsVar <- tclVar("")
    trialsEntry <- ttkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        trials <- as.numeric(tclvalue(trialsVar))
		prob <- as.numeric(tclvalue(probVar))
		options(warn)
        if (is.na(trials)) {
            errorCondition(recall=binomialMass, message=gettextRcmdr("Binomial trials not specified."))
            return()
            }
        if (trials > 50){
            if ("no" == tclvalue(checkTrials(trials))){
                if (getRcmdr("grab.focus")) tkgrab.release(top)
                tkdestroy(top)
                binomialMass()
                return()
                }
            }
        if (is.na(prob)) {
            errorCondition(recall=binomialMass, message=gettextRcmdr("Probability of success not specified."))
            return()
            }
        command <- paste("data.frame(Pr=dbinom(0:", trials, ", size=", trials, 
            ", prob=", prob, "))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- 0:", trials, sep=""))
        justDoIt(paste("rownames(.Table) <- 0:", trials, sep=""))
        doItAndPrint(".Table")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)       
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dbinom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Binomial trials")), trialsEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    dialogSuffix(rows=3, columns=2, focus=trialsEntry)
    }

PoissonMass <- function(){
    checkRange <- function(range){
        RcmdrTkmessageBox(message=sprintf(gettextRcmdr("Range of values over which to plot, %d, is large.\nCreate long output?"), range),
            icon="warning", type="yesno", default="no")
        }
    initializeDialog(title=gettextRcmdr("Poisson Probabilities"))
    meanVar <- tclVar("")
    meanEntry <- ttkentry(top, width="6", textvariable=meanVar)
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        mean <- as.numeric(tclvalue(meanVar))
		options(warn)
        if (is.na(mean)) {
            errorCondition(recall=PoissonMass, message=gettextRcmdr("Poisson mean not specified."))
            return()
            }
        if (mean < 0) {
            errorCondition(recall=PoissonMass, message=gettextRcmdr("Poisson mean cannot be negative."))
            return()
            }
        min <- qpois(.00005, lambda=mean)
        max <- qpois(.99995, lambda=mean)
        range <- max - min
        if (range > 50){
            if ("no" == tclvalue(checkRange(range))){
                if (getRcmdr("grab.focus")) tkgrab.release(top)
                tkdestroy(top)
                PoissonMass()
                return()
                }
            }
        command <- paste("data.frame(Pr=round(dpois(", min, ":", max, ", lambda=", mean, "), 4))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- ", min, ":", max, sep=""))
        justDoIt(paste("rownames(.Table) <- ", min, ":", max, sep=""))
        doItAndPrint(".Table")
        logger("remove(.Table)") 
        remove(.Table, envir=.GlobalEnv)       
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dpois")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Mean")), meanEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    dialogSuffix(rows=2, columns=2, focus=meanEntry)
    }

# the following functions were contributed by G. Jay Kerns, Andy Chang, and  Theophilius Boye
#  modified by J. Fox

PoissonQuantiles  <- function(){
    initializeDialog(title=gettextRcmdr("Poisson Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    lambdaVar <- tclVar("1")
    lambdaEntry <- ttkentry(top, width="6", textvariable=lambdaVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
            errorCondition(recall=PoissonQuantiles, message=gettextRcmdr("No probabilities specified."))
            return()
            }
        lambda <- tclvalue(lambdaVar)
        tail <- tclvalue(tailVar)
        if (is.na(lambda)) {
            errorCondition(recall=PoissonQuantiles, message=gettextRcmdr("Poisson mean not specified."))
            return()
            }
        if (lambda < 0) {
            errorCondition(recall=PoissonQuantiles, message=gettextRcmdr("Poisson mean cannot be negative."))
            return()
            }
        doItAndPrint(paste("qpois(c(", quantiles, "), lambda=", lambda,
                     ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qpois")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Mean")),lambdaEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(lambdaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }
    
PoissonProbabilities  <- function(){
    initializeDialog(title=gettextRcmdr("Poisson Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    lambdaVar <- tclVar("1")
    lambdaEntry <- ttkentry(top, width="6", textvariable=lambdaVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
            errorCondition(recall=PoissonProbabilities, message=gettextRcmdr("No values specified."))
            return()
            }
        lambda <- tclvalue(lambdaVar)
        tail <- tclvalue(tailVar)
        if (is.na(lambda)) {
            errorCondition(recall=PoissonProbabilities, message=gettextRcmdr("Poisson mean not specified."))
            return()
            }
        if (lambda < 0) {
            errorCondition(recall=PoissonProbabilities, message=gettextRcmdr("Poisson mean cannot be negative."))
            return()
            }
        doItAndPrint(paste("ppois(c(", probabilities, "), lambda=", lambda,
                           ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="ppois")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Mean")), lambdaEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(lambdaEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }

geomQuantiles  <- function(){
    initializeDialog(title=gettextRcmdr("Geometric Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    probVar <- tclVar("0.5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
              errorCondition(recall=geomQuantiles, message=gettextRcmdr("No probabilities specified."))
              return()
        }
        prob <- tclvalue(probVar)
        tail <- tclvalue(tailVar)
        if ( is.na(prob) ){
              errorCondition(recall=geomQuantiles, message=gettextRcmdr("Probability of success not specified."))
              return()
        }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=geomQuantiles, message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        doItAndPrint(paste("qgeom(c(", quantiles, "), prob=", prob,
                     ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qgeom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

geomProbabilities  <- function(){
    initializeDialog(title=gettextRcmdr("Geometric Probabilities"))
    probabilitiesVar <- tclVar("")
    probabilitiesEntry <- ttkentry(top, width="30", textvariable=probabilitiesVar)
    probVar <- tclVar("0.5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(probabilitiesVar))
        if ("" == probabilities) {
              errorCondition(recall=geomProbabilities, message=gettextRcmdr("No values specified."))
              return()
        }
        prob <- tclvalue(probVar)
        tail <- tclvalue(tailVar)
        if ( is.na(prob) ){
              errorCondition(recall=geomProbabilities, message=gettextRcmdr("Probability of success was not specified."))
              return()
        }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=geomProbabilities, message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        doItAndPrint(paste("pgeom(c(", probabilities, "), prob=", prob,
                           ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pgeom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), probabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(probabilitiesEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=1, focus=probabilitiesEntry)
    }

geomMass  <- function(){
    checkRange <- function(range){
        RcmdrTkmessageBox(message=sprintf(gettextRcmdr("Range of values over which to plot, %d, is large.\nCreate long output?"), range),
            icon="warning", type="yesno", default="no")
        }
    initializeDialog(title=gettextRcmdr("Geometric Probabilities"))
    probVar <- tclVar("0.5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        prob <- as.numeric(tclvalue(probVar))
		options(warn)
        if (is.na(prob) ) {
              errorCondition(recall=geomMass, message=gettextRcmdr("Probability of success was not specified."))
              return()
        }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=geomMass, message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        xmin <- qgeom(.0005, prob=prob)
        xmax <- qgeom(.9995, prob=prob)
        range <- xmax - xmin
        if (range > 50){
            if ("no" == tclvalue(checkRange(range))){
                if (getRcmdr("grab.focus")) tkgrab.release(top)
                tkdestroy(top)
                geomMass()
                return()
                }
            }
        command <- paste("data.frame(Pr=dgeom(", xmin, ":", xmax, ", prob=", prob, "))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- ", xmin, ":", xmax, sep=""))
        justDoIt(paste("rownames(.Table) <- ", xmin, ":", xmax, sep=""))
        doItAndPrint(".Table")
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dgeom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    dialogSuffix(rows=2, columns=2, focus=probEntry)
    }

hyperQuantiles  <- function(){
    initializeDialog(title=gettextRcmdr("Hypergeometric Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    mVar <- tclVar("1")
    mEntry <- ttkentry(top, width="6", textvariable=mVar)
    nVar <- tclVar("1")
    nEntry <- ttkentry(top, width="6", textvariable=nVar)
    kVar <- tclVar("1")
    kEntry <- ttkentry(top, width="6", textvariable=kVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
              errorCondition(recall=hyperQuantiles, message=gettextRcmdr("No probabilities specified."))
              return()
        }
		warn <- options(warn=-1)
        m <- as.numeric(tclvalue(mVar))
        n <- as.numeric(tclvalue(nVar))
        k <- as.numeric(tclvalue(kVar))
		options(warn)
        # Do some error checking
        if ( is.na(m) ){
              errorCondition(recall=hyperQuantiles, message=gettextRcmdr("The m parameter was not specified."))
              return()
        }
        if ( m < 0 ){
              errorCondition(recall=hyperQuantiles, message=gettextRcmdr("The m parameter cannot be negative."))
              return()
        }
        m <- round(m)
        if ( is.na(n) ){
              errorCondition(recall=hyperQuantiles, message=gettextRcmdr("The n parameter was not specified."))
              return()
        }
        if ( n < 0 ){
              errorCondition(recall=hyperQuantiles, message=gettextRcmdr("The n parameter cannot be negative."))
              return()
        }
        n <- round(n)
        if ( is.na(k) ){
              errorCondition(recall=hyperQuantiles, message=gettextRcmdr("The k parameter was not specified."))
              return()
        }
        k <- round(k)
        if ( k > (m + n) ){
                errorCondition(recall=hyperQuantiles,
                message=gettextRcmdr("The k parameter cannot be greater than m + n."))
                        return()
                    }
        if ( k < 0 ){
                errorCondition(recall=hyperQuantiles,
                message=gettextRcmdr("The k parameter cannot be negative."))
                        return()
                    }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qhyper(c(", quantiles, "), m=", m,
            ", n=", n, ", k=", k,", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qhyper")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("m (number of white balls in the urn)")), mEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("n (number of black balls in the urn)")), nEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("k (number of balls drawn from the urn)")), kEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(mEntry, sticky="w")
    tkgrid.configure(nEntry, sticky="w")
    tkgrid.configure(kEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=7, columns=2, focus=quantilesEntry)
    }

hyperProbabilities  <- function(){
    initializeDialog(title=gettextRcmdr("Hypergeometric Probabilities"))
    ProbabilitiesVar <- tclVar("")
    ProbabilitiesEntry <- ttkentry(top, width="30", textvariable=ProbabilitiesVar)
    mVar <- tclVar("1")
    mEntry <- ttkentry(top, width="6", textvariable=mVar)
    nVar <- tclVar("1")
    nEntry <- ttkentry(top, width="6", textvariable=nVar)
    kVar <- tclVar("1")
    kEntry <- ttkentry(top, width="6", textvariable=kVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        probabilities <- gsub(" ", ",", tclvalue(ProbabilitiesVar))
        if ("" == probabilities) {
              errorCondition(recall=hyperProbabilities.ipsr, message=gettextRcmdr("No values specified."))
              return()
        }
		warn <- options(warn=-1)
        m <- as.numeric(tclvalue(mVar))
        n <- as.numeric(tclvalue(nVar))
        k <- as.numeric(tclvalue(kVar))
		options(warn)
        # Do some error checking
        if ( is.na(m) ){
              errorCondition(recall=hyperProbabilities, message=gettextRcmdr("The m parameter was not specified."))
              return()
        }
        if ( m < 0 ){
              errorCondition(recall=hyperProbabilities, message=gettextRcmdr("The m parameter cannot be negative."))
              return()
        }
        m <- round(m)
        if ( is.na(n) ){
              errorCondition(recall=hyperProbabilities, message=gettextRcmdr("The n parameter was not specified."))
              return()
        }
        if ( n < 0 ){
              errorCondition(recall=hyperProbabilities, message=gettextRcmdr("The n parameter cannot be negative."))
              return()
        }
        n <- round(n)
        if ( is.na(k) ){
              errorCondition(recall=hyperProbabilities, message=gettextRcmdr("The k parameter was not specified."))
              return()
        }
        k <- round(k)
        if ( k > (m + n) ){
                errorCondition(recall=hyperProbabilities,
                message=gettextRcmdr("The k parameter cannot be greater than m + n."))
                        return()
                    }
        if ( k < 0 ){
                errorCondition(recall=hyperProbabilities,
                message=gettextRcmdr("The k parameter cannot be negative."))
                        return()
                    }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("phyper(c(", probabilities, "), m=", m,
            ", n=", n, ", k=", k,", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }

    OKCancelHelp(helpSubject="phyper")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), ProbabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("m (number of white balls in the urn)")), mEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("n (number of black balls in the urn)")), nEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("k (number of balls drawn from the urn)")), kEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(ProbabilitiesEntry, sticky="w")
    tkgrid.configure(mEntry, sticky="w")
    tkgrid.configure(nEntry, sticky="w")
    tkgrid.configure(kEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=7, columns=2, focus=ProbabilitiesEntry)
    }

hyperMass  <- function(){
    checkRange <- function(range){
        RcmdrTkmessageBox(message=sprintf(gettextRcmdr("Range of values over which to plot, %d, is large.\nCreate long output?"), range),
            icon="warning", type="yesno", default="no")
        }
    initializeDialog(title=gettextRcmdr("Hypergeometric  Probabilities"))
    mVar <- tclVar("1")
    mEntry <- ttkentry(top, width="6", textvariable=mVar)
    nVar <- tclVar("1")
    nEntry <- ttkentry(top, width="6", textvariable=nVar)
    kVar <- tclVar("1")
    kEntry <- ttkentry(top, width="6", textvariable=kVar)
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        m <- as.numeric(tclvalue(mVar))
        n <- as.numeric(tclvalue(nVar))
        k <- as.numeric(tclvalue(kVar))
		options(warn)
        # Do some error checking
        if ( is.na(m) ){
              errorCondition(recall=hyperMass, message=gettextRcmdr("The m parameter was not specified."))
              return()
        }
        if ( m < 0 ){
              errorCondition(recall=hyperMass, message=gettextRcmdr("The m parameter cannot be negative."))
              return()
        }
        m <- round(m)
        if ( is.na(n) ){
              errorCondition(recall=hyperMass, message=gettextRcmdr("The n parameter was not specified."))
              return()
        }
        if ( n < 0 ){
              errorCondition(recall=hyperMass, message=gettextRcmdr("The n parameter cannot be negative."))
              return()
        }
        n <- round(n)
        if ( is.na(k) ){
              errorCondition(recall=hyperMass, message=gettextRcmdr("The k parameter was not specified."))
              return()
        }
        k <- round(k)
        if ( k > (m + n) ){
                errorCondition(recall=hyperMass,
                message=gettextRcmdr("The k parameter cannot be greater than m + n."))
                        return()
                    }
        if ( k < 0 ){
                errorCondition(recall=hyperMass,
                message=gettextRcmdr("The k parameter cannot be negative."))
                        return()
                    }
        xmin <- qhyper(.0005, m=m, n=n, k=k)
        xmax <- qhyper(.9995, m=m, n=n, k=k)
        if (xmax - xmin > 50){
            if ("no" == tclvalue(checkRange(range))){
                if (getRcmdr("grab.focus")) tkgrab.release(top)
                tkdestroy(top)
                hyperMass()
                return()
                }
            }
        command <- paste("data.frame(Pr=dhyper(", xmin, ":", xmax, ", m=", m, ", n=", n, ", k=", k, "))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- ", xmin, ":", xmax, sep=""))
        justDoIt(paste("rownames(.Table) <- ", xmin, ":", xmax, sep=""))
        doItAndPrint(".Table")
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dhyper")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("m (number of white balls in the urn)")), mEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("n (number of black balls in the urn)")), nEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("k (number of balls drawn from the urn)")), kEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(mEntry, sticky="w")
    tkgrid.configure(nEntry, sticky="w")
    tkgrid.configure(kEntry, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=mEntry)
    }

    negbinomialQuantiles  <- function(){
    initializeDialog(title=gettextRcmdr("Negative Binomial Quantiles"))
    quantilesVar <- tclVar("")
    quantilesEntry <- ttkentry(top, width="30", textvariable=quantilesVar)
    sizeVar <- tclVar("1")
    sizeEntry <- ttkentry(top, width="6", textvariable=sizeVar)
    probVar <- tclVar("0.5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(quantilesVar))
        if ("" == quantiles) {
              errorCondition(recall=negbinomialQuantiles, 
                message=gettextRcmdr("No probabilities specified."))
              return()
          }
	  	warn <- options(warn=-1)
        size <- as.numeric(tclvalue(sizeVar))
        prob <- as.numeric(tclvalue(probVar))
		options(warn)
        # Do some error checking
        if ( is.na(size) ){
              errorCondition(recall=negbinomialQuantiles, 
                message=gettextRcmdr("Target number of successes not specified."))
              return()
          }
        if ( size < 0){
              errorCondition(recall=negbinomialQuantiles, 
                message=gettextRcmdr("Target number of successes cannot be negative."))
              return()
          }
        size <- round(size) 
        if ( is.na(prob) ){
              errorCondition(recall=negbinomialQuantiles, 
                message=gettextRcmdr("Probability of success not specified."))
              return()
          }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=negbinomialQuantiles, 
              message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("qnbinom(c(", quantiles, "), size=", size,
            ", prob=", prob, ", lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="qnbinom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probabilities")), quantilesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Target number of successes")), sizeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(quantilesEntry, sticky="w")
    tkgrid.configure(sizeEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=quantilesEntry)
    }

negbinomialProbabilities  <- function(){
    initializeDialog(title=gettextRcmdr("Negative Binomial Probabilities"))
    ProbabilitiesVar <- tclVar("")
    ProbabilitiesEntry <- ttkentry(top, width="30", textvariable=ProbabilitiesVar)
    sizeVar <- tclVar("1")
    sizeEntry <- ttkentry(top, width="6", textvariable=sizeVar)
    probVar <- tclVar("0.5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    tailVar <- tclVar("lower")
    lowerTailButton <- ttkradiobutton(top, variable=tailVar, value="lower")
    upperTailButton <- ttkradiobutton(top, variable=tailVar, value="upper")
    onOK <- function(){
        closeDialog()
        quantiles <- gsub(" ", ",", tclvalue(ProbabilitiesVar))
        if ("" == quantiles) {
              errorCondition(recall=negbinomialProbabilities, 
                message=gettextRcmdr("No values specified."))
              return()
        }
		warn <- options(warn=-1)
        size <- as.numeric(tclvalue(sizeVar))
        prob <- as.numeric(tclvalue(probVar))
		options(warn)
        # Do some error checking
        if ( is.na(size) ){
              errorCondition(recall=negbinomialProbabilities, 
                message=gettextRcmdr("Target number of successes not specified."))
              return()
          }
        if ( size < 0){
              errorCondition(recall=negbinomialProbabilities, 
                message=gettextRcmdr("Target number of successes cannot be negative."))
              return()
          }
        size <- round(size) 
        if ( is.na(prob) ){
              errorCondition(recall=negbinomialProbabilities, 
                message=gettextRcmdr("Probability of success not specified."))
              return()
          }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=negbinomialProbabilities, 
              message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        tail <- tclvalue(tailVar)
        doItAndPrint(paste("pnbinom(c(", quantiles, "), size=", size,
            ", prob=", prob, ",  lower.tail=", tail == "lower",")", sep=""))
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="pnbinom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Variable value(s)")), ProbabilitiesEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Target number of successes")), sizeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Lower tail")), lowerTailButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Upper tail")), upperTailButton, sticky="e")
    tkgrid(buttonsFrame, sticky="w", columnspan=2)
    tkgrid.configure(ProbabilitiesEntry, sticky="w")
    tkgrid.configure(sizeEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(lowerTailButton, sticky="w")
    tkgrid.configure(upperTailButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=ProbabilitiesEntry)
    }

negbinomialMass  <- function(){
    checkRange <- function(range){
        RcmdrTkmessageBox(message=sprintf(gettextRcmdr("Range of values over which to plot, %d, is large.\nCreate long output?"), range),
            icon="warning", type="yesno", default="no")
        }
    initializeDialog(title=gettextRcmdr("Negative Binomial Probabilities"))
    trialsVar <- tclVar("1")
    trialsEntry <- ttkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar("0.5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        trials <- as.numeric(tclvalue(trialsVar))
		prob <- as.numeric(tclvalue(probVar))
		options(warn)
        if ( is.na(trials) ){
              errorCondition(recall=negbinomialMass, 
                message=gettextRcmdr("Target number of successes not specified."))
              return()
          }
        if ( trials < 0){
              errorCondition(recall=negbinomialMass, 
                message=gettextRcmdr("Target number of successes cannot be negative."))
              return()
          }
        trials <- round(trials)
        if ( is.na(prob) ){
              errorCondition(recall=negbinomialMass, 
                message=gettextRcmdr("Probability of success not specified."))
              return()
          }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=negbinomialMass, 
              message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        xmin <- qnbinom(.0005, size=trials, prob=prob)
        xmax <- qnbinom(.9995, size=trials, prob=prob) 
        range <- xmax - xmin
        if (range > 50){
            if ("no" == tclvalue(checkRange(range))){
                if (getRcmdr("grab.focus")) tkgrab.release(top)
                tkdestroy(top)
                negbinomialMass()
                return()
                }
            }
        command <- paste("data.frame(Pr=dnbinom(", xmin, ":", xmax, ", size=", trials,", prob=", prob, "))", sep="")
        logger(paste(".Table <- ", command, sep=""))
        assign(".Table", justDoIt(command), envir=.GlobalEnv)
        logger(paste("rownames(.Table) <- ", xmin, ":", xmax, sep=""))
        justDoIt(paste("rownames(.Table) <- ", xmin, ":", xmax, sep=""))
        doItAndPrint(".Table")
        logger("remove(.Table)")
        remove(.Table, envir=.GlobalEnv)
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dnbinom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Target number of successes")), trialsEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    dialogSuffix(rows=3, columns=2, focus=trialsEntry)
    }
