# Distributions menu dialogs for plots

# last modified 30 August 2009 by J. Fox

#   many distributions added (and some other changes) by Miroslav Ristic  (20 July 06)

normalDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Normal Distribution"))
    muVar <- tclVar("0")
    muEntry <- ttkentry(top, width="6", textvariable=muVar)
    sigmaVar <- tclVar("1")
    sigmaEntry <- ttkentry(top, width="6", textvariable=sigmaVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        mu <- as.numeric(tclvalue(muVar))
        sigma <- as.numeric(tclvalue(sigmaVar))
		options(warn)
		if (is.na(mu)) {
			errorCondition(recall=normalDistributionPlot, message=gettextRcmdr("Mean not specified."))
			return()
		}
        if (is.na(sigma) || sigma <= 0) {
            errorCondition(recall=normalDistributionPlot, message=gettextRcmdr("Standard deviation must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dnorm" else "pnorm"
        min <- round(qnorm(.0005, mean=mu, sd=sigma), 3)
        max <- round(qnorm(.9995, mean=mu, sd=sigma), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, mean=", mu, 
            ", sd=", sigma, '), xlab="x", ylab="', fun, 
            '", main=expression(paste("Normal Distribution: ", mu, " = ',
            mu, ', ", sigma, " = ', sigma, '")), type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dnorm")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("mu (mean)")), muEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("sigma (standard deviation)")), sigmaEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(muEntry, sticky="w")
    tkgrid.configure(sigmaEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=muEntry)
    }

tDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("t Distribution"))
    dfVar <- tclVar("")
    dfEntry <- ttkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        df <- as.numeric(tclvalue(dfVar))
		options(warn)
        if (is.na(df)) {
            errorCondition(recall=tDistributionPlot, message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df<=0) {
            errorCondition(recall=tDistributionPlot, message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dt" else "pt"
        min <- round(qt(.0005, df=df), 3)
        max <- round(qt(.9995, df=df), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df=", df, 
            '), xlab="t", ylab="', fun, 
            '", main="t Distribution: df = ', df, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dt")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=dfEntry)
    }

chisquareDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Chi-squared Distribution"))
    dfVar <- tclVar("")
    dfEntry <- ttkentry(top, width="6", textvariable=dfVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        df <- as.numeric(tclvalue(dfVar))
		options(warn)
        if (is.na(df)) {
            errorCondition(recall=chisquareDistributionPlot,message=gettextRcmdr("Degrees of freedom not specified."))
            return()
            }
        if (df<=0) {
            errorCondition(recall=chisquareDistributionPlot, message=gettextRcmdr("Degrees of freedom must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dchisq" else "pchisq"
        min <- round(qchisq(.0005, df=df), 3)
        max <- round(qchisq(.9995, df=df), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df=", df, 
            '), xlab=expression(chi^2), ylab="', fun, 
            '", main="Chi-Squared Distribution: df = ', df, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dchisq")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Degrees of freedom")), dfEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(dfEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=dfEntry)
    }

FDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("F Distribution"))
    df1Var <- tclVar("")
    df2Var <- tclVar("")
    df1Entry <- ttkentry(top, width="6", textvariable=df1Var)
    df2Entry <- ttkentry(top, width="6", textvariable=df2Var)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        df1 <- as.numeric(tclvalue(df1Var))
        df2 <- as.numeric(tclvalue(df2Var))
		options(warn)
        if (is.na(df1)) {
            errorCondition(recall=FDistributionPlot, message=gettextRcmdr("Numerator degrees of freedom not specified."))
            return()
            }
        if (is.na(df2)) {
             errorCondition(recall=FDistributionPlot, message=gettextRcmdr("Denominator degrees of freedom not specified."))
            return()
            }
        if (df1 <= 0) {
            errorCondition(recall=FDistributionPlot, message=gettextRcmdr("Numerator degrees of freedom must be positive."))
            return()
            }
        if (df2 <= 0) {
            errorCondition(recall=FDistributionPlot, message=gettextRcmdr("Denominator degrees of freedom must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "df" else "pf"
        min <- round(qf(.0005, df1=df1, df2=df2), 3)
        max <- round(qf(.9995, df1=df1, df2=df2), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, df1=", df1, ", df2=", df2,
            '), xlab="f", ylab="', fun, 
            '", main="F Distribution: Numerator df = ', df1, ', Denominator df = ', df2, 
            '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="df")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Numerator degrees of freedom")), df1Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Denominator degrees of freedom")), df2Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(df1Entry, sticky="w")
    tkgrid.configure(df2Entry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=df1Entry)
    }

exponentialDistributionPlot <- function() { 
    initializeDialog(title=gettextRcmdr("Exponential Distribution"))
    rateVar <- tclVar("1")
    rateEntry <- ttkentry(top, width="6", textvariable=rateVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        rate <- as.numeric(tclvalue(rateVar))
		options(warn)
        if (is.na(rate) || rate <= 0) {
            errorCondition(recall=exponentialDistributionPlot, message=gettextRcmdr("Rate must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dexp" else "pexp"
        min <- round(qexp(.0005, rate=rate), 3)
        max <- round(qexp(.9995, rate=rate), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, rate=", rate, 
            '), xlab="x", ylab="', fun, 
            '", main="Exponential Distribution: rate = ', rate, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dexp")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Rate")), rateEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(rateEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=rateEntry)
    }
    
uniformDistributionPlot <- function() { 
    initializeDialog(title=gettextRcmdr("Uniform Distribution"))
    minVar <- tclVar("0")
    maxVar <- tclVar("1")
    minEntry <- ttkentry(top, width="6", textvariable=minVar)
    maxEntry <- ttkentry(top, width="6", textvariable=maxVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        minValue <- as.numeric(tclvalue(minVar))
        maxValue <- as.numeric(tclvalue(maxVar))
		options(warn)
        if (is.na(minValue) || is.na(maxValue) || minValue >= maxValue) {
            errorCondition(recall=uniformDistributionPlot, message=gettextRcmdr("Lower limit must be less than upper limit."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dunif" else "punif"
        min <- round(qunif(.0005, min=minValue, max=maxValue), 3)
        max <- round(qunif(.9995, min=minValue, max=maxValue), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, min=", minValue, ", max=", maxValue,
            '), xlab="x", ylab="', fun, 
            '", main="Uniform Distribution: min=', minValue, ', max=', maxValue, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dunif")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Minimum")), minEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Maximum")), maxEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(minEntry, sticky="w")
    tkgrid.configure(maxEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=minEntry)
    }
    
betaDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Beta Distribution"))
    shape1Var <- tclVar("")
    shape2Var <- tclVar("")
    shape1Entry <- ttkentry(top, width="6", textvariable=shape1Var)
    shape2Entry <- ttkentry(top, width="6", textvariable=shape2Var)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        shape1 <- as.numeric(tclvalue(shape1Var))
        shape2 <- as.numeric(tclvalue(shape2Var))
		options(warn)
        if (is.na(shape1) || is.na(shape2)) {
            errorCondition(recall=betaDistributionPlot, message=gettextRcmdr("Shapes not specified."))
            return()
            }
        if (shape1 <= 0 || shape2 <= 0) {
            errorCondition(recall=betaDistributionPlot, message=gettextRcmdr("Shapes must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dbeta" else "pbeta"
        min <- round(qbeta(.0005, shape1=shape1, shape2=shape2), 3)
        max <- round(qbeta(.9995, shape1=shape1, shape2=shape2), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, shape1=", shape1, ", shape2=", shape2,
            '), xlab="x", ylab="', fun, 
            '", main="Beta Distribution: Shapes a = ', shape1, ', b = ', shape2, 
            '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dbeta")
    tkgrid(labelRcmdr(top, text=paste(gettextRcmdr("Shape"), "1")), shape1Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=paste(gettextRcmdr("Shape"), 2)), shape2Entry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(shape1Entry, sticky="w")
    tkgrid.configure(shape2Entry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=shape1Entry)
    }
    
CauchyDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Cauchy Distribution"))
    locationVar <- tclVar("0")
    locationEntry <- ttkentry(top, width="6", textvariable=locationVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        location <- as.numeric(tclvalue(locationVar))
        s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(s) || s <= 0) {
            errorCondition(recall=CauchyDistributionPlot, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dcauchy" else "pcauchy"
        min <- round(qcauchy(.01, location=location, scale=s), 3)
        max <- round(qcauchy(.99, location=location, scale=s), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, location=", location, 
            ", scale=", s, '), xlab="x", ylab="', fun, 
            '", main="Cauchy Distribution: location = ',
            location, ', scale = ', s, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dcauchy")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Location")), locationEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(locationEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=locationEntry)
    }
    
logisticDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Logistic Distribution"))
    locationVar <- tclVar("0")
    locationEntry <- ttkentry(top, width="6", textvariable=locationVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        location <- as.numeric(tclvalue(locationVar))
        s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(s) || s <= 0) {
            errorCondition(recall=logisticDistributionPlot, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dlogis" else "plogis"
        min <- round(qlogis(.0005, location=location, scale=s), 3)
        max <- round(qlogis(.9995, location=location, scale=s), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, location=", location, 
            ", scale=", s, '), xlab="x", ylab="', fun, 
            '", main="Logistic Distribution: location = ',
            location, ', scale = ', s, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dlogis")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Location")), locationEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(locationEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=locationEntry)
    }
    
lognormalDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Lognormal Distribution"))
    meanlogVar <- tclVar("0")
    meanlogEntry <- ttkentry(top, width="6", textvariable=meanlogVar)
    sdlogVar <- tclVar("1")
    sdlogEntry <- ttkentry(top, width="6", textvariable=sdlogVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        meanlog <- as.numeric(tclvalue(meanlogVar))
        sdlog <- as.numeric(tclvalue(sdlogVar))
		options(warn)
        if (is.na(sdlog) || sdlog <= 0) {
            errorCondition(recall=lognormalDistributionPlot, message=gettextRcmdr("Standard deviation must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dlnorm" else "plnorm"
        min <- round(qlnorm(.0005, meanlog=meanlog, sdlog=sdlog), 3)
        max <- round(qlnorm(.9995, meanlog=meanlog, sdlog=sdlog), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, meanlog=", meanlog, 
            ", sdlog=", sdlog, '), xlab="x", ylab="', fun, 
            '", main="Lognormal Distribution: Mean (log scale) = ',
            meanlog, ', SD (log scale) = ', sdlog, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dlnorm")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Mean (log scale)")), meanlogEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Standard deviation (log scale)")), sdlogEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(meanlogEntry, sticky="w")
    tkgrid.configure(sdlogEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=meanlogEntry)
    }
    
gammaDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Gamma Distribution"))
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=gammaDistributionPlot, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=gammaDistributionPlot, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=gammaDistributionPlot, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dgamma" else "pgamma"
        min <- round(qgamma(.0005, shape=shape, scale=s), 3)
        max <- round(qgamma(.9995, shape=shape, scale=s), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, shape=", shape, 
            ", scale=", s, '), xlab="x", ylab="', fun, 
            '", main="Gamma Distribution: shape = ',
            shape, ', scale = ', s, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dgamma")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale (inverse rate)")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=shapeEntry)
    }

WeibullDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Weibull Distribution"))
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=WeibullDistributionPlot, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=WeibullDistributionPlot, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=WeibullDistributionPlot, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dweibull" else "pweibull"
        min <- round(qweibull(.0005, shape=shape, scale=s), 3)
        max <- round(qweibull(.9995, shape=shape, scale=s), 3)
        command <- paste("seq(", min, ", ", max, ", length.out=100)", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(.x, ", fn, "(.x, shape=", shape, 
            ", scale=", s, '), xlab="x", ylab="', fun, 
            '", main="Weibull Distribution: shape = ',
            shape, ', scale = ', s, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dweibull")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=shapeEntry)
    }

GumbelDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Gumbel Distribution"))
    shapeVar <- tclVar("")
    shapeEntry <- ttkentry(top, width="6", textvariable=shapeVar)
    sVar <- tclVar("1")
    sEntry <- ttkentry(top, width="6", textvariable=sVar)
    functionVar <- tclVar("Density")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        shape <- as.numeric(tclvalue(shapeVar))
		s <- as.numeric(tclvalue(sVar))
		options(warn)
        if (is.na(shape)) {
            errorCondition(recall=GumbelDistributionPlot, message=gettextRcmdr("Shape not specified."))
            return()
            }
        if (shape <= 0) {
            errorCondition(recall=GumbelDistributionPlot, message=gettextRcmdr("Shape must be positive."))
            return()
            }
        if (is.na(s) || s <= 0) {
            errorCondition(recall=GumbelDistributionPlot, message=gettextRcmdr("Scale must be positive."))
            return()
            }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") "dweibull" else "pweibull"
        min <- round(log(qweibull(.0005, shape=shape, scale=s)), 3)
        max <- round(log(qweibull(.9995, shape=shape, scale=s)), 3)
        command <- paste("exp(seq(", min, ", ", max, ", length.out=100))", sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        doItAndPrint(paste("plot(log(.x), ", fn, "(.x, shape=", shape, 
            ", scale=", s, '), xlab="x", ylab="', fun, 
            '", main="Gumbel Distribution: shape (log scale) = ',
            shape, ', scale (log scale) = ', s, '", type="l")', sep=""))
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dweibull")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Shape (log shape)")), shapeEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Scale (log scale)")), sEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot density function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(shapeEntry, sticky="w")
    tkgrid.configure(sEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=shapeEntry)
    }
    

binomialDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Binomial Distribution"))
    trialsVar <- tclVar("")
    trialsEntry <- ttkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar(".5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    functionVar <- tclVar("Probability")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        trials <- round(as.numeric(tclvalue(trialsVar)))
		prob <- as.numeric(tclvalue(probVar))
		options(warn)
        if (is.na(trials)) {
            errorCondition(recall=binomialDistributionPlot, message=gettextRcmdr("Binomial trials not specified."))
            return()
            } 
        if (is.na(prob)) {
            errorCondition(recall=binomialDistributionPlot, message=gettextRcmdr("Probability of success not specified."))
            return()
            }
        fun <- tclvalue(functionVar)
        min <- qbinom(.0005, size=trials, prob=prob)
        max <- qbinom(.9995, size=trials, prob=prob)
        command <- paste(min, ":", max, sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dbinom(.x, size=", trials, ", prob=", prob,
                '), xlab="Number of Successes", ylab="Probability Mass", main="Binomial Distribution: Trials = ', 
                trials, ', Probability of success = ', prob, '", type="h")', sep=""))
            doItAndPrint(paste("points(.x, dbinom(.x, size=", trials, ", prob=", prob,
                '), pch=16)', sep=""))
            }
        else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], pbinom(.x, size=", trials, ", prob=", prob,
                ')[-length(.x)], xlab="Number of Successes", ylab="Cumulative Probability", main="Binomial Distribution: Trials = ', 
                trials, ', Probability of success = ', prob, '", type="l")', sep=""))
            }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dbinom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Binomial trials")), trialsEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot probability mass function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=trialsEntry)
    }

PoissonDistributionPlot <- function(){
    initializeDialog(title=gettextRcmdr("Poisson Distribution"))
    meanVar <- tclVar("")
    meanEntry <- ttkentry(top, width="6", textvariable=meanVar)
    functionVar <- tclVar("Probability")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        mean <- as.numeric(tclvalue(meanVar))
		options(warn)
        if (is.na(mean)) {
            errorCondition(recall=PoissonDistributionPlot, message=gettextRcmdr("Mean not specified."))
            return()
            }
        if (mean < 0) {
            errorCondition(recall=PoissonDistributionPlot, message=gettextRcmdr("Poisson mean cannot be negative."))
            return()
            }
        fun <- tclvalue(functionVar)
        min <- qpois(.0005, lambda=mean)
        max <- qpois(.9995, lambda=mean)
        command <- paste(min, ":", max, sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dpois(.x, lambda=", mean,
                '), xlab="x", ylab="Probability Mass", main="Poisson Distribution: Mean = ', 
                mean, '", type="h")', sep=""))
            doItAndPrint(paste("points(.x, dpois(.x, lambda=", mean,
                '), pch=16)', sep=""))
            }
        else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], ppois(.x, lambda=", mean,
                ')[-length(.x)], xlab="x", ylab="Probability Mass", main="Poisson Distribution: Mean = ', 
                mean, '", type="l")', sep=""))
            }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dpois")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Mean")), meanEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot probability mass function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(meanEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=4, columns=2, focus=meanEntry)
    }
    
# the following functions were contributed by G. Jay Kerns, Andy Chang, and  Theophilius Boye
#  last modified 26 July 06 by J. Fox

geomDistributionPlot  <- function(){
    initializeDialog(title=gettextRcmdr("Geometric Distribution"))
    probVar <- tclVar("0.5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    functionVar <- tclVar("Probability")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        prob <- as.numeric(tclvalue(probVar))
		options(warn)
        if ( is.na(prob) ) {
              errorCondition(recall=geomDistributionPlot, message=gettextRcmdr("Probability of success was not specified."))
              return()
        }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=geomDistributionPlot, message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        fun <- tclvalue(functionVar)
        xmin <- qgeom(.0005, prob=prob)
        xmax <- qgeom(.9995, prob=prob)
        command <- paste(xmin, ":", xmax, sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dgeom(.x, prob=", prob,
                '), xlab="Number of Failures until Success", ylab="Probability Mass", main="Geometric Distribution: Prob of success = ', prob, '", type="h")', sep=""))
            doItAndPrint(paste("points(.x, dgeom(.x, prob=", prob,
                '), pch=16)', sep=""))
        } else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], pgeom(.x, prob=", prob,
                ')[-length(.x)], xlab="Number of Failures until Success", ylab="Cumulative Probability", main="Geometric Distribution: Probability of success = ', prob, '", type="l")', sep=""))
        }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dgeom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot probability mass function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=probEntry)
    }
    
hyperDistributionPlot  <- function(){
    initializeDialog(title=gettextRcmdr("Hypergeometric Distribution"))
    mVar <- tclVar("1")
    mEntry <- ttkentry(top, width="6", textvariable=mVar)
    nVar <- tclVar("1")
    nEntry <- ttkentry(top, width="6", textvariable=nVar)
    kVar <- tclVar("1")
    kEntry <- ttkentry(top, width="6", textvariable=kVar)
    functionVar <- tclVar("Probability")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        m <- as.numeric(tclvalue(mVar))
        n <- as.numeric(tclvalue(nVar))
        k <- as.numeric(tclvalue(kVar))
		options(warn)
        fun <- tclvalue(functionVar)
        # Do some error checking
        if ( is.na(m) ){
              errorCondition(recall=hyperDistributionPlot, message=gettextRcmdr("The m parameter was not specified."))
              return()
        }
        if ( m < 0 ){
              errorCondition(recall=hyperDistributionPlot, message=gettextRcmdr("The m parameter cannot be negative."))
              return()
        }
        m <- round(m)
        if ( is.na(n) ){
              errorCondition(recall=hyperDistributionPlot, message=gettextRcmdr("The n parameter was not specified."))
              return()
        }
        if ( n < 0 ){
              errorCondition(recall=hyperDistributionPlot, message=gettextRcmdr("The n parameter cannot be negative."))
              return()
        }
        n <- round(n)
        if ( is.na(k) ){
              errorCondition(recall=hyperDistributionPlot, message=gettextRcmdr("The k parameter was not specified."))
              return()
        }
        k <- round(k)
        if ( k > (m + n) ){
                errorCondition(recall=hyperDistributionPlot,
                message=gettextRcmdr("The k parameter cannot be greater than m + n."))
                        return()
                    }
        if ( k < 0 ){
                errorCondition(recall=hyperDistributionPlot,
                message=gettextRcmdr("The k parameter cannot be negative."))
                        return()
                    }

        xmin <- qhyper(.0005, m=m, n=n, k=k)
        xmax <- qhyper(.9995, m=m, n=n, k=k)
        command <- paste(xmin, ":", xmax, sep="")
        logger(paste(".x <- ", command, sep=""))
        assign(".x", justDoIt(command), envir=.GlobalEnv)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dhyper(.x, m=", m, ", n=", n, ", k=", k,
                '), xlab="Number of White Balls in Sample", ylab="Probability Mass", main="Hypergeometric Distribution: m=',
                m, ", n=", n, ", k=", k, '", type="h")', sep=""))
            doItAndPrint(paste("points(.x, dhyper(.x, m=", m, ", n=", n, ", k=", k,
                '), pch=16)', sep=""))
            }
        else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], phyper(.x, m=", m, ", n=", n, ", k=", k,
                ')[-length(.x)], xlab="Number of White Balls in Sample", ylab="Cumulative Probability", main="Hypergeometric Distribution: m=',
                m, ", n=", n, ", k=", k, '", type="l")', sep=""))
            }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dhyper")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("m (number of white balls in the urn)")), mEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("n (number of black balls in the urn)")), nEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("k (number of balls drawn from the urn)")), kEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot probability mass function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(mEntry, sticky="w")
    tkgrid.configure(nEntry, sticky="w")
    tkgrid.configure(kEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=6, columns=2, focus=mEntry)
    }

negbinomialDistributionPlot  <- function(){
    initializeDialog(title=gettextRcmdr("Negative Binomial Distribution"))
    trialsVar <- tclVar("1")
    trialsEntry <- ttkentry(top, width="6", textvariable=trialsVar)
    probVar <- tclVar("0.5")
    probEntry <- ttkentry(top, width="6", textvariable=probVar)
    functionVar <- tclVar("Probability")
    densityButton <- ttkradiobutton(top, variable=functionVar, value="Probability")
    distributionButton <- ttkradiobutton(top, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        closeDialog()
		warn <- options(warn=-1)
        trials <- as.numeric(tclvalue(trialsVar))
		prob <- as.numeric(tclvalue(probVar))
		options(warn)
        if ( is.na(trials) ){
              errorCondition(recall=negbinomialDistributionPlot, 
                message=gettextRcmdr("Target number of successes not specified."))
              return()
          }
        if ( trials < 0){
              errorCondition(recall=negbinomialDistributionPlot, 
                message=gettextRcmdr("Target number of successes cannot be negative."))
              return()
          }
        trials <- round(trials)
        if ( is.na(prob) ){
              errorCondition(recall=negbinomialDistributionPlot, 
                message=gettextRcmdr("Probability of success not specified."))
              return()
          }
        if (prob < 0 || prob > 1) {
            errorCondition(recall=negbinomialDistributionPlot, 
              message=gettextRcmdr("Probability of success must be between 0 and 1."))
            return()
            }
        xmin <- qnbinom(.0005, size=trials, prob=prob)
        xmax <- qnbinom(.9995, size=trials, prob=prob) 
        logger(paste(".x <- ", xmin, ":", xmax, sep=""))
        assign(".x", justDoIt(paste(".x <- ", xmin, ":", xmax, sep="")), envir=.GlobalEnv)
        fun <- tclvalue(functionVar)
        if (fun == "Probability"){
            doItAndPrint(paste("plot(.x, dnbinom(.x, size=", trials, ", prob=", prob,
              '), xlab="Number of Failures Until Target Successes", ylab="Probability Mass", main=',
              paste('"Negative Binomial Distribution:\\nTarget successes = ', trials, ', Probability of success = ', prob, '"', sep=""), 
              ', type="h")', sep=""))
            doItAndPrint(paste("points(.x, dnbinom(.x, size=", trials, ", prob=", prob,
              '), pch=16)', sep=""))
            } 
        else {
            command <- "rep(.x, rep(2, length(.x)))"
            logger(paste(".x <- ", command, sep=""))
            assign(".x", justDoIt(command), envir=.GlobalEnv)
            doItAndPrint(paste("plot(.x[-1], pnbinom(.x, size=", trials, ", prob=", prob,
                ')[-length(.x)], xlab="Number of Failures Until Target Successes", ylab="Cumulative Probability", main=',
                paste('"Negative Binomial Distribution:\\nTarget successes = ',trials, ', Probability of success = ', prob, '"', sep=""), 
                ', type="l")', sep=""))
            }
        doItAndPrint('abline(h=0, col="gray")')
        remove(.x, envir=.GlobalEnv)
        logger("remove(.x)")
        tkfocus(CommanderWindow())
        }
    OKCancelHelp(helpSubject="dnbinom")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Target number of successes")), trialsEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Probability of success")), probEntry, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot probability mass function")), densityButton, sticky="e")
    tkgrid(labelRcmdr(top, text=gettextRcmdr("Plot distribution function")), distributionButton, sticky="e")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    tkgrid.configure(trialsEntry, sticky="w")
    tkgrid.configure(probEntry, sticky="w")
    tkgrid.configure(densityButton, sticky="w")
    tkgrid.configure(distributionButton, sticky="w")
    dialogSuffix(rows=5, columns=2, focus=trialsEntry)
    }