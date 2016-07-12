# Distributions menu dialogs for plots

# last modified 2016-07-12 by J. Fox

#   many distributions added (and some other changes) by Miroslav Ristic  (20 July 06)
# modified by Miroslav M. Ristic (15 January 11)

normalDistributionPlot <- function() {distributionPlot("normal")}
tDistributionPlot <- function() {distributionPlot("t")}
chisqDistributionPlot <- function() {distributionPlot("chisq")}
FDistributionPlot <- function() {distributionPlot("F")}
exponentialDistributionPlot <- function() {distributionPlot("exponential")}
uniformDistributionPlot <- function() {distributionPlot("uniform")}
betaDistributionPlot <- function(){distributionPlot("beta")}
CauchyDistributionPlot <- function(){distributionPlot("Cauchy")}
logisticDistributionPlot <- function(){distributionPlot("logistic")}
lognormalDistributionPlot <- function(){distributionPlot("lognormal")}
gammaDistributionPlot <- function(){distributionPlot("gamma")}
WeibullDistributionPlot <- function(){distributionPlot("Weibull")}
GumbelDistributionPlot <- function(){distributionPlot("Gumbel")}
binomialDistributionPlot <- function(){discreteDistributionPlot("binomial")}
PoissonDistributionPlot <- function(){discreteDistributionPlot("Poisson")}

# the following functions were contributed by G. Jay Kerns, Andy Chang, and  Theophilius Boye
#  last modified 26 July 06 by J. Fox

geomDistributionPlot  <- function(){discreteDistributionPlot("geom")}
hyperDistributionPlot  <- function(){discreteDistributionPlot("hyper")}
negbinomialDistributionPlot  <- function(){discreteDistributionPlot("negbinomial")}

distributionPlot <- function(nameVar){
    fVar<-get(paste(nameVar,"Distribution",sep=""))
    nnVar<-length(fVar$params)
    dialogName <- paste(nameVar,"DistributionPlot", sep="")
    defaults <- list(initialValues=fVar$initialValues, type="Density", showRegions="0", valuesOrQuantiles="values",
                     from1="", from2="", to1="", to2="")
    initial <- getDialog(dialogName, defaults=defaults)
    initializeDialog(title=gettextRcmdr(paste(fVar$titleName,"Distribution",sep=" ")))
    entriesFrame <- tkframe(top)
    paramsVar<-paste(fVar$params,"Var",sep="")
    paramsEntry<-paste(fVar$params,"Entry",sep="")
    for (i in 1:nnVar) {
        eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
        eval(parse(text=paste(paramsEntry[i],"<-ttkentry(entriesFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
    functionVar <- tclVar(initial$type)
    buttonFrame <- tkframe(top)
    densityButton <- ttkradiobutton(buttonFrame, variable=functionVar, value="Density")
    distributionButton <- ttkradiobutton(buttonFrame, variable=functionVar, value="Cumulative Probability")
    regionsFrame <- tkframe(top)
    showRegionsVariable <- tclVar(initial$showRegions)
    regionsCheckBoxFrame <- tkframe(regionsFrame)
    regionsCheckBox <- ttkcheckbutton(regionsCheckBoxFrame, variable = showRegionsVariable)
    radioButtons(regionsFrame, "valuesOrQuantiles", buttons=c("values", "quantiles"), 
                 labels=gettextRcmdr(c("x-values", "quantiles")), title=gettextRcmdr("Specify regions by"),
                 initialValue = initial$valuesOrQuantiles)
    from1variable <- tclVar(initial$from1)
    from2variable <- tclVar(initial$from2)
    to1variable <- tclVar(initial$to1)
    to2variable <- tclVar(initial$to2)
    region1Frame <- tkframe(regionsFrame)
    region2Frame <- tkframe(regionsFrame)
    from1box <- ttkentry(regionsFrame, width="10", textvariable=from1variable)
    from2box <- ttkentry(regionsFrame, width="10", textvariable=from2variable)
    to1box   <- ttkentry(regionsFrame, width="10", textvariable=to1variable)
    to2box   <- ttkentry(regionsFrame, width="10", textvariable=to2variable)
    onOK <- function(){
        nameVarF<-get(paste(nameVar,"DistributionPlot",sep=""),mode="function")
        closeDialog()
        warn <- options(warn=-1)
        vars<-numeric(nnVar)
        for (i in 1:nnVar) {
            vars[i]<-as.numeric(tclvalue(get(paramsVar[i])))
        }
        if (length(fVar$paramsRound)>0) {
            for (j in fVar$paramsRound) {
                vars[j]<-round(vars[j])
            }
        }
        options(warn)
        for (i in 1:length(fVar$errorConds)) {
            if (eval(parse(text=fVar$errorConds[i]))) {
                errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
                return()
            }
        }
        fun <- tclvalue(functionVar)
        fn <- if (fun == "Density") paste("d",fVar$funName,sep="") else paste("p",fVar$funName,sep="")
        dist.arg <- if (fun == "Density") "FALSE" else "TRUE"
        pasteVar<-""
        for (i in 1:nnVar) {
            pasteVar<-paste(pasteVar,", ",fVar$params[i],"=",vars[i],sep="")
        }
        mainVar<-""
        for (i in 1:nnVar) {
            mainVar<-paste(mainVar,", ",fVar$paramsLabels[i],"=",vars[i],sep="")
        }
        if (nameVar=="Gumbel") {
            min <- eval(parse(text=paste("round(log(q",fVar$funName,"(.0005",pasteVar,")),3)",sep="")))
            max <- eval(parse(text=paste("round(log(q",fVar$funName,"(.9995",pasteVar,")),3)",sep="")))
        } else {
            min <- eval(parse(text=paste("round(q",fVar$funName,"(.0005",pasteVar,"),3)",sep="")))
            max <- eval(parse(text=paste("round(q",fVar$funName,"(.9995",pasteVar,"),3)",sep="")))
        }
        if (nameVar=="Gumbel") {
            command <- paste("exp(seq(", min, ", ", max, ", length.out=1000))", sep="")
        } else {
            command <- paste("seq(", min, ", ", max, ", length.out=1000)", sep="")
        }
        command <- paste("local({\n  .x <- ", command, sep="")
        doVar<-"\n  plotDistr(.x, "
        if (nameVar=="Gumbel") {doVar<-"\n  plotDistr(log(.x), "}
        if (nameVar=="F") {mainVar<-paste(", Numerator df = ",vars[1],", Denominator df = ",vars[2],sep="")}
        
        showRegions <- tclvalue(showRegionsVariable)
        valuesOrQuantiles <- tclvalue(valuesOrQuantilesVariable)
        command <- if (showRegions == 0 || fun != "Density"){
            paste(command, "  ", doVar, fn, "(.x", pasteVar,'), cdf=', dist.arg, ', xlab="x", ylab="', fun, 
                  '", main=paste("',fVar$titleName,' Distribution: ',substr(mainVar,2,nchar(mainVar)),'"))\n})', sep="")
            save.from1 <- save.to1 <- save.from2 <- save.to2 <- ""
        }
        else {
            from1 <- tclvalue(from1variable)
            if (from1 == ""){
                errorCondition(recall=nameVarF, message=paste(gettextRcmdr("You must specify 'from' and 'to' for at least one range.")))
                return()
            }
            if (!is.valid.number(from1)){
                errorCondition(recall=nameVarF, message=paste(from1, gettextRcmdr("is not a valid number.")))
                return()
            }
            from2 <- tclvalue(from2variable)
            if (from2 != "" && !is.valid.number(from2)){
                errorCondition(recall=nameVarF, message=paste(from2, gettextRcmdr("is not a valid number.")))
                return()
            }
            to1 <-  tclvalue(to1variable)
            if (to1 == ""){
                errorCondition(recall=nameVarF, message=paste(gettextRcmdr("You must specify 'from' and 'to' for at least one range.")))
                return()
            }
            if (!is.valid.number(to1)){
                errorCondition(recall=nameVarF, message=paste(to1, gettextRcmdr("is not a valid number.")))
                return()
            }
            to2 <-  tclvalue(to2variable)
            if (to2 != "" && !is.valid.number(to2)){
                errorCondition(recall=nameVarF, message=paste(to2, gettextRcmdr("is not a valid number.")))
                return()
            }
            if (as.numeric(to1) <= as.numeric(from1)){
                errorCondition(recall=nameVarF, message=gettextRcmdr("In specifying a range, 'to' must be greater than 'from'."))
                return()
            }
            if (to2 != "" && as.numeric(to2) <= as.numeric(from2)){
                errorCondition(recall=nameVarF, message=gettextRcmdr("In specifying a range, 'to' must be greater than 'from'."))
                return()
            }
            save.from1 <- from1
            save.from2 <- from2
            save.to1 <- to1
            save.to2 <- to2
            if (valuesOrQuantiles == "quantiles"){
                if (as.numeric(from1) < 0 || as.numeric(from1) >= 1 || as.numeric(to1) <= 0 || as.numeric(to1) > 1 ){
                    errorCondition(recall=nameVarF, message=gettextRcmdr("quantiles must be between 0 and 1"))
                    return()
                }
                from1 <- eval(parse(text=paste("q",fVar$funName,"(", from1, pasteVar, ")", sep="")))
                to1 <- eval(parse(text=paste("q",fVar$funName,"(", to1, pasteVar, ")", sep="")))
                if (from2 != "") {
                    if (as.numeric(from2) < 0 || as.numeric(from2) >= 1 || as.numeric(to2) <= 0 || as.numeric(to2) > 1 ){
                        errorCondition(recall=nameVarF, message=gettextRcmdr("quantiles must be between 0 and 1"))
                        return()
                    }
                    from2 <- eval(parse(text=paste("q",fVar$funName,"(", from2 ,")",sep="")))
                    to2 <- eval(parse(text=paste("q",fVar$funName,"(", to2 ,")",sep="")))
                }
            }
            paste(command, "  ", doVar, fn, "(.x", pasteVar,'), cdf=', dist.arg, ', xlab="x", ylab="', fun, 
                  '", main=paste("',fVar$titleName,' Distribution: ',substr(mainVar,2,nchar(mainVar)), '"), ',
                  "regions=list(c(", from1, ", ", to1, ")", if (from2 != "") paste(", c(", from2, ",", to2, ")", sep=""), ")",
                  ')\n})', sep="")
        }
        doItAndPrint(command)
        tkfocus(CommanderWindow())
        putDialog(dialogName, list(initialValues=vars, type=fun, showRegions=showRegions, valuesOrQuantiles=valuesOrQuantiles,
                                   from1=save.from1, from2=save.from2, to1=save.to1, to2=save.to2), resettable=FALSE)
    }
    OKCancelHelp(helpSubject=paste("d",fVar$funName,sep=""), reset=dialogName, apply=dialogName)
    for (i in 1:nnVar) {
        tkgrid(labelRcmdr(entriesFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    }
    tkgrid(entriesFrame, sticky="w")
    tkgrid(buttonFrame, sticky="w")
    tkgrid(densityButton, labelRcmdr(buttonFrame, text=gettextRcmdr("Plot density function")), sticky="w")
    tkgrid(distributionButton, labelRcmdr(buttonFrame, text=gettextRcmdr("Plot distribution function")), sticky="w")
    for (i in 1:nnVar) {
        tkgrid.configure(get(paramsEntry[i]), sticky="w")
    }
    tkgrid(labelRcmdr(regionsFrame, text=gettextRcmdr("Fill Regions Under the Density Function"), 
                      fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
    tkgrid(labelRcmdr(regionsCheckBoxFrame, text=gettextRcmdr("Fill regions ")), regionsCheckBox, sticky="w")
    tkgrid(regionsCheckBoxFrame, sticky="w")
    tkgrid(valuesOrQuantilesFrame, sticky="w")
    tkgrid(labelRcmdr(regionsFrame, text=gettextRcmdr("Regions to Fill (specify one or two"), 
                      fg=getRcmdr("title.color"), font="RcmdrTitleFont"), sticky="w")
    tkgrid(labelRcmdr(region1Frame, text=gettextRcmdr("Region 1: from ")), from1box, 
           labelRcmdr(region1Frame, text=gettextRcmdr("to ")), to1box, sticky="w")
    tkgrid(labelRcmdr(region2Frame, text=gettextRcmdr("Region 2: from ")), from2box, 
           labelRcmdr(region2Frame, text=gettextRcmdr("to ")), to2box, sticky="w")
    tkgrid(region1Frame, sticky="w")
    tkgrid(region2Frame, sticky="w")
    tkgrid(regionsFrame, sticky="w")
    tkgrid(buttonsFrame, sticky="ew")
    dialogSuffix(focus=get(paramsEntry[1]))
}

discreteDistributionPlot <- function(nameVar){
    fVar<-get(paste(nameVar,"Distribution",sep=""))
    nnVar<-length(fVar$params)
    dialogName <- paste(nameVar,"DistributionPlot", sep="")
    defaults <- list(initialValues=fVar$initialValues, type="Probability")
    initial <- getDialog(dialogName, defaults=defaults)
    initializeDialog(title=gettextRcmdr(paste(fVar$titleName,"Distribution",sep=" ")))
    entriesFrame <- tkframe(top)
    paramsVar<-paste(fVar$params,"Var",sep="")
    paramsEntry<-paste(fVar$params,"Entry",sep="")
    for (i in 1:nnVar) {
        eval(parse(text=paste(paramsVar[i],"<-tclVar('",initial$initialValues[i],"')",sep="")))
        eval(parse(text=paste(paramsEntry[i],"<-ttkentry(entriesFrame, width='6', textvariable=",paramsVar[i],")",sep="")))
    }
    functionVar <- tclVar(initial$type)
    buttonFrame <- tkframe(top)
    densityButton <- ttkradiobutton(buttonFrame, variable=functionVar, value="Probability")
    distributionButton <- ttkradiobutton(buttonFrame, variable=functionVar, value="Cumulative Probability")
    onOK <- function(){
        nameVarF<-get(paste(nameVar,"DistributionPlot",sep=""),mode="function")
        closeDialog()
        warn <- options(warn=-1)
        vars<-numeric(nnVar)
        for (i in 1:nnVar) {
            vars[i]<-as.numeric(tclvalue(get(paramsVar[i])))
        }
        if (length(fVar$paramsRound)>0) {
            for (j in fVar$paramsRound) {
                vars[j]<-round(vars[j])
            }
        }
        options(warn)
        for (i in 1:length(fVar$errorConds)) {
            if (eval(parse(text=fVar$errorConds[i]))) {
                errorCondition(recall=nameVarF, message=gettextRcmdr(fVar$errorTexts[i]))
                return()
            }
        }
        fun <- tclvalue(functionVar)
        pasteVar<-""
        for (i in 1:nnVar) {
            pasteVar<-paste(pasteVar,", ",fVar$params[i],"=",vars[i],sep="")
        }
        min <- eval(parse(text=paste("q",fVar$funName,"(.0005",pasteVar,")",sep="")))
        max <- eval(parse(text=paste("q",fVar$funName,"(.9995",pasteVar,")",sep="")))
        command <- paste(min, ":", max, sep="")
        command <- paste("local({\n  .x <- ", command, sep="")
        switch(nameVar,
            "binomial" = xlabVar<-"Number of Successes",
            "Poisson" = xlabVar<-"x",
            "geom" = xlabVar<-"Number of Failures until Success",
            "hyper" = xlabVar<-"Number of White Balls in Sample",
            "negbinomial" = xlabVar <-"Number of Failures Until Target Successes"
        )
        mainVar<-""
        if (nameVar=="negbinomial") {
            mainVar<-paste(", Trials=",vars[1],", Prob=",vars[2],sep="")
        } else if (nameVar=="hyper") {
            mainVar<-paste(", m=",vars[1],", n=",vars[2],", k=",vars[3],sep="")
        } else {
            for (i in 1:nnVar) {
                mainVar<-paste(mainVar,", ", fVar$paramsLabels[i],"=",vars[i],sep="")
            }   
        }
        command <- if (fun == "Probability"){
           paste(command, "\n  plotDistr(.x, d",fVar$funName,"(.x", pasteVar,
                '), xlab="',xlabVar,'", ylab="Probability Mass", main="',fVar$titleName,
                ' Distribution: ',substr(mainVar,2,nchar(mainVar)),'", discrete=TRUE)', sep="")
        }
        else {
            paste(command, "\n  plotDistr(.x, p",fVar$funName,"(.x",
                pasteVar,'), xlab="',xlabVar,
                '",ylab="Cumulative Probability", main="',
                fVar$titleName,' Distribution: ',substr(mainVar,2,nchar(mainVar)),'", discrete=TRUE, cdf=TRUE)', 
                sep="")
        }
        command <- paste(command, "\n})", sep="")
        doItAndPrint(command)
        tkfocus(CommanderWindow())
        putDialog(dialogName, list(initialValues=vars, type=fun), resettable=FALSE)
    }
    OKCancelHelp(helpSubject=paste("d",fVar$funName,sep=""), reset=dialogName, apply=dialogName)
    for (i in 1:nnVar) {
        tkgrid(labelRcmdr(entriesFrame, text=gettextRcmdr(fVar$paramsLabels[i])), get(paramsEntry[i]), sticky="w", padx=6)
    }
    tkgrid(densityButton, labelRcmdr(buttonFrame, text=gettextRcmdr("Plot probability mass function")), sticky="w")
    tkgrid(distributionButton, labelRcmdr(buttonFrame, text=gettextRcmdr("Plot distribution function")), sticky="w")
    tkgrid(entriesFrame, sticky="w")
    tkgrid(buttonFrame, sticky="w")
    tkgrid(buttonsFrame, columnspan=2, sticky="w")
    for (i in 1:nnVar) {
        tkgrid.configure(get(paramsEntry[i]), sticky="w")
    }
    dialogSuffix(focus=get(paramsEntry[1]))
}
