# Statistics Menu dialogs

# last modified 2023-08-29 by J. Fox

# Means menu

independentSamplesTTest <- function () {
    defaults <- list(initial.group = NULL, initial.response = NULL, initial.alternative = "two.sided", 
                     initial.confidenceLevel = ".95", initial.variances = "FALSE", initial.label=NULL,
                     initial.tab=0)
    dialog.values <- getDialog("independentSamplesTTest", defaults)
    initializeDialog(title = gettextRcmdr("Independent Samples t-Test"), use.tabs=TRUE)
    variablesFrame <- tkframe(dataTab)
    groupBox <- variableListBox(variablesFrame, TwoLevelFactors(), 
                                title = gettextRcmdr("Groups (pick one)"), 
                                initialSelection = varPosn(dialog.values$initial.group, "twoLevelFactor"))
    responseBox <- variableListBox(variablesFrame, Numeric(), 
                                   title = gettextRcmdr("Response Variable (pick one)"),
                                   initialSelection = varPosn(dialog.values$initial.response, "numeric"))
    onOK <- function() {
        tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
        group <- getSelection(groupBox)
        if (length(group) == 0) {
            errorCondition(recall = independentSamplesTTest, 
                           message = gettextRcmdr("You must select a groups variable."))
            return()
        }
        response <- getSelection(responseBox)
        if (length(response) == 0) {
            errorCondition(recall = independentSamplesTTest, 
                           message = gettextRcmdr("You must select a response variable."))
            return()
        }
        alternative <- as.character(tclvalue(alternativeVariable))
        level <- tclvalue(confidenceLevel)
        variances <- as.character(tclvalue(variancesVariable))
        putDialog ("independentSamplesTTest", list (initial.group = group, initial.response = response, initial.alternative = alternative, 
                                                    initial.confidenceLevel = level, initial.variances = variances, 
                                                    initial.label=.groupsLabel, initial.tab=tab))        
        closeDialog()
        # doItAndPrint(paste("t.test(", response, "~", group, ", alternative='", 
        #                    alternative, "', conf.level=", level, ", var.equal=", 
        #                    variances, ", data=", ActiveDataSet(), ")", sep = ""))
        command <- Command("t.test", paste(response, "~", group), alternative=Q(alternative),
                           var.equal=variances, data=ActiveDataSet())
        doItAndPrint(command)
        tkfocus(CommanderWindow())
    }
    OKCancelHelp(helpSubject = "t.test", reset = "independentSamplesTTest", apply = "independentSamplesTTest")
    optionsFrame <- tkframe(optionsTab)
    radioButtons(optionsFrame, name = "alternative", buttons = c("twosided", 
                                                                 "less", "greater"), values = c("two.sided", "less", "greater"), 
                 labels = gettextRcmdr(c("Two-sided", "Difference < 0", 
                                         "Difference > 0")), title = gettextRcmdr("Alternative Hypothesis"),
                 initialValue = dialog.values$initial.alternative)
    confidenceFrame <- tkframe(optionsFrame)
    confidenceLevel <- tclVar(dialog.values$initial.confidenceLevel)
    confidenceField <- ttkentry(confidenceFrame, width = "6", 
                                textvariable = confidenceLevel)
    radioButtons(optionsFrame, name = "variances", buttons = c("yes", 
                                                               "no"), values = c("TRUE", "FALSE"),  
                 labels = gettextRcmdr(c("Yes", "No")), title = gettextRcmdr("Assume equal variances?"),
                 initialValue = dialog.values$initial.variances)
    tkgrid(getFrame(groupBox), labelRcmdr(variablesFrame, text = "    "), 
           getFrame(responseBox), sticky = "nw")
    tkgrid(variablesFrame, sticky = "nw")
    tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level"), 
                      fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w")
    tkgrid(confidenceField, sticky = "w")
    groupsLabel(optionsTab, groupsBox = groupBox, initialText=dialog.values$initial.label)
    tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text = "    "), 
           confidenceFrame, labelRcmdr(optionsFrame, text = "    "), 
           variancesFrame, sticky = "nw")
    tkgrid(optionsFrame, sticky = "nw")
    dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE)
}

pairedTTest <- function () {
  defaults <- list(initial.x = NULL, initial.y = NULL, initial.alternative = "two.sided", 
                   initial.confidenceLevel = ".95", initial.tab=0)
  dialog.values <- getDialog("pairedTTest", defaults)
  initializeDialog(title = gettextRcmdr("Paired t-Test"), use.tabs=TRUE)
  .numeric <- Numeric()
  dataFrame <- tkframe(dataTab)
  xBox <- variableListBox(dataFrame, .numeric, title = gettextRcmdr("First variable (pick one)"),
                          initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  yBox <- variableListBox(dataFrame, .numeric, title = gettextRcmdr("Second variable (pick one)"),
                          initialSelection = varPosn(dialog.values$initial.y, "numeric"))
  onOK <- function() {
    tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    x <- getSelection(xBox)
    y <- getSelection(yBox)
    if (length(x) == 0 | length(y) == 0) {
      errorCondition(recall = pairedTTest, message = gettextRcmdr("You must select two variables."))
      return()
    }
    if (x == y) {
      errorCondition(recall = pairedTTest, message = gettextRcmdr("Variables must be different."))
      return()
    }
    alternative <- as.character(tclvalue(alternativeVariable))
    level <- tclvalue(confidenceLevel)
    putDialog ("pairedTTest", list (initial.x = x, initial.y = y, initial.alternative = alternative, 
                                    initial.confidenceLevel = level, initial.tab=tab))
    closeDialog()
    .activeDataSet <- ActiveDataSet()
#    doItAndPrint(paste("with(", ActiveDataSet (), ", (t.test(", x, 
#                       ", ", y, ", alternative='", 
#                       alternative, "', conf.level=", level, ", paired=TRUE)))", 
#                       sep = ""))
    command <- Command("with", ActiveDataSet(), paste0("(t.test(", x,", ", y), alternative=Q(alternative),
                       conf.level=level, "paired=TRUE))")
    doItAndPrint(command)
    insertRmdSection(paste0(gettextRmdHeader("Paired t-Test: "), x, ", ", y))
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "t.test", reset = "pairedTTest", apply = "pairedTTest")
  optionsFrame <- tkframe(optionsTab)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided", 
                                                               "less", "greater"), values = c("two.sided", "less", "greater"), 
               labels = gettextRcmdr(c("Two-sided", "Difference < 0", 
                                       "Difference > 0")), title = gettextRcmdr("Alternative Hypothesis"), 
               initialValue = dialog.values$initial.alternative)
  confidenceFrame <- tkframe(optionsFrame)
  confidenceLevel <- tclVar(dialog.values$initial.confidenceLevel)
  confidenceField <- ttkentry(confidenceFrame, width = "6", 
                              textvariable = confidenceLevel)
  tkgrid(getFrame(xBox), labelRcmdr(dataFrame, text="  "), getFrame(yBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level"), 
                    fg = getRcmdr("title.color"), font="RcmdrTitleFont"))
  tkgrid(confidenceField, sticky = "w")
  tkgrid(alternativeFrame, labelRcmdr(optionsFrame, text="  "), confidenceFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky="w")
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE)
}

singleSampleTTest <- function () {
  defaults <- list (initial.x = NULL, initial.alternative = "two.sided", initial.level = ".95", 
                    initial.mu = "0.0")
  dialog.values <- getDialog ("singleSampleTTest", defaults)  
  initializeDialog(title = gettextRcmdr("Single-Sample t-Test"))
  xBox <- variableListBox(top, Numeric(), title = gettextRcmdr("Variable (pick one)"),
                          initialSelection = varPosn(dialog.values$initial.x, "numeric"))
  onOK <- function() {
    x <- getSelection(xBox)
    if (length(x) == 0) {
      errorCondition(recall = singleSampleTTest, message = gettextRcmdr("You must select a variable."))
      return()
    }
    alternative <- as.character(tclvalue(alternativeVariable))
    level <- tclvalue(confidenceLevel)
    mu <- tclvalue(muVariable)
    putDialog ("singleSampleTTest", list (initial.x = x, initial.alternative = alternative, 
                                          initial.level = level, initial.mu = mu))
    closeDialog()
#    doItAndPrint(paste("with(", ActiveDataSet (), ", (t.test(", x, 
#                       ", alternative='", alternative, "', mu=", mu, ", conf.level=", 
#                       level, ")))", sep = ""))
    command <- Command("with",ActiveDataSet(), 
                       paste0("(t.test(", x), 
                       alternative=Q(alternative), mu=mu, conf.level=paste0(level, "))"))
    doItAndPrint(command)
    insertRmdSection(paste0(gettextRmdHeader("Single-Sample t-Test: "), x))
    tkdestroy(top)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "t.test", reset = "singleSampleTTest", apply = "singleSampleTTest")
  optionsFrame <- tkframe(top)
  radioButtons(optionsFrame, name = "alternative", buttons = c("twosided", 
                                                               "less", "greater"), values = c("two.sided", "less", "greater"), 
               labels = gettextRcmdr(c("Population mean != mu0", "Population mean < mu0", 
                                       "Population mean > mu0")), title = gettextRcmdr("Alternative Hypothesis"),
               initialValue = dialog.values$initial.alternative)
  rightFrame <- tkframe(optionsFrame)
  confidenceFrame <- tkframe(rightFrame)
  confidenceLevel <- tclVar(dialog.values$initial.level)
  confidenceField <- ttkentry(confidenceFrame, width = "6", 
                              textvariable = confidenceLevel)
  muFrame <- tkframe(rightFrame)
  muVariable <- tclVar(dialog.values$initial.mu)
  muField <- ttkentry(muFrame, width = "8", textvariable = muVariable)
  tkgrid(getFrame(xBox), sticky = "nw")
  tkgrid(labelRcmdr(rightFrame, text = ""), sticky = "w")
  tkgrid(labelRcmdr(muFrame, text = gettextRcmdr("Null hypothesis: mu = ")), 
         muField, sticky = "w", padx=c(10, 0))
  tkgrid(muFrame, sticky = "w")
  tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level: ")), 
         confidenceField, sticky = "w", padx=c(10, 0))
  tkgrid(confidenceFrame, sticky = "w")
  tkgrid(alternativeFrame, rightFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  tkgrid.configure(confidenceField, sticky = "e")
  dialogSuffix()
}

oneWayAnova <- function () {
  Library("multcomp")
  Library("abind")
  defaults <- list(initial.group = NULL, initial.response = NULL, initial.pairwise = 0, 
                   initial.welch=0, initial.level=0.95)
  dialog.values <- getDialog("oneWayAnova", defaults)
  initializeDialog(title = gettextRcmdr("One-Way Analysis of Variance"))
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"), 
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), title = gettextRcmdr("Groups (pick one)"), 
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"),
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))
  optionsFrame <- tkframe(top)
  pairwiseVariable <- tclVar(dialog.values$initial.pairwise)
  pairwiseCheckBox <- ttkcheckbutton(optionsFrame, variable = pairwiseVariable)
  
  confidenceFrame <- tkframe(optionsFrame)
  confidenceLevel <- tclVar(dialog.values$initial.level)
  confidenceField <- ttkentry(confidenceFrame, width = "6", 
                              textvariable = confidenceLevel)
  
  welchVariable <- tclVar(dialog.values$initial.welch)
  welchCheckBox <- ttkcheckbutton(optionsFrame, variable = welchVariable)
  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)) {
      UpdateModelNumber(-1)
      errorCondition(recall = oneWayAnova, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                                                             modelValue))
      return()
    }
    if (is.element(modelValue, listAOVModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        tkdestroy(top)
        if (getRcmdr("onApplyCalled")){
            putRcmdr("onApplyCalled", FALSE)
            return()
        }
        oneWayAnova()
        return()
      }
    }
    group <- getSelection(groupBox)
    response <- getSelection(responseBox)
    level <- tclvalue(confidenceLevel)
    alpha <- 1 - as.numeric(level)
    closeDialog()
    if (length(group) == 0) {
      errorCondition(recall = oneWayAnova, message = gettextRcmdr("You must select a groups factor."))
      return()
    }
    if (length(response) == 0) {
      errorCondition(recall = oneWayAnova, message = gettextRcmdr("You must select a response variable."))
      return()
    }
    .activeDataSet <- ActiveDataSet()
#    command <- paste(modelValue, " <- aov(", response, " ~ ", 
#                     group, ", data=", .activeDataSet, ")", sep = "")
#    command <- Command("lm", formula, data=ActiveDataSet(), subset=subset, weights=weights,
#                       subset=remove.cases, to=modelValue)
#    doItAndPrint(command)
#    doItAndPrint(paste("summary(", modelValue, ")", sep=""))
#   logger(command)
    command <- Command ("aov", paste(response,"~", group), data=ActiveDataSet(), to=modelValue)
    doItAndPrint(command)
    command <- Command ("summary", modelValue)
    #doItAndPrint(paste("summary(", modelValue, ")", sep = ""))
    doItAndPrint(command)
    # doItAndPrint(paste("with(", .activeDataSet, ", numSummary(",
    #                    response, ", groups=", group, 
    #                    ", statistics=c(\"mean\", \"sd\")))", sep = ""))
    command <- Command ("with", .activeDataSet, paste0("numSummary(", response), groups=group,
                        "statistics=c('mean', 'sd'))")
    doItAndPrint(command)
    activeModel(modelValue)
    putRcmdr("modelWithSubset", FALSE)
    pairwise <- tclvalue(pairwiseVariable)
    welch <- tclvalue(welchVariable)
    putDialog ("oneWayAnova", list (initial.group = group, initial.response = response, initial.pairwise = pairwise,
                                    initial.welch=welch, initial.level=level))
    if (pairwise == 1) {
      if (eval(parse(text = paste("length(levels(", .activeDataSet, 
                                  "$", group, ")) < 3")))) 
        Message(message = gettextRcmdr("Factor has fewer than 3 levels; pairwise comparisons omitted."), 
                type = "warning")
      else {
        commands <- character(7)
        commands[1] <- paste("local({\n  .Pairs <- glht(", modelValue, 
                             ", linfct = mcp(", group, " = \"Tukey\"))", 
                             sep = "")
        commands[2] <- "  print(summary(.Pairs)) # pairwise tests"
        commands[3] <- paste0("  print(confint(.Pairs, level=", level, ")) # confidence intervals")
        commands[4] <- paste0("  print(cld(.Pairs, level=", alpha, ")) # compact letter display")
        commands[5] <- "  old.oma <- par(oma=c(0, 5, 0, 0))"
        commands[6] <- "  plot(confint(.Pairs))"
        commands[7] <- "  par(old.oma)\n})"
        doItAndPrint(paste(commands, collapse="\n"))
      }
    }
    if (welch == 1){
#        command <- paste("oneway.test(", response, " ~ ", 
#                         group, ", data=", .activeDataSet, ") # Welch test", sep = "")
        command <- Command("oneway.test", paste(response, "~", group), data=.activeDataSet)
        doItAndPrint(paste(command, " # Welch test", sep = ""))
    }
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "anova", model = TRUE, reset = "oneWayAnova", apply = "oneWayAnova")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w", columnspan = 2)
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(pairwiseCheckBox, labelRcmdr(optionsFrame, text = gettextRcmdr("Pairwise comparisons of means")), 
         sticky = "w")
  tkgrid(labelRcmdr(confidenceFrame, text = gettextRcmdr("Confidence Level: ")), 
         confidenceField, sticky = "w", padx=c(10, 0))
  tkgrid(confidenceFrame, sticky = "w", columnspan=2)
  tkgrid(welchCheckBox, labelRcmdr(optionsFrame, text = gettextRcmdr("Welch F-test not assuming equal variances")), 
         sticky = "w")
  tkgrid(optionsFrame, sticky = "w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}

multiWayAnova <- function () {
  defaults <- list(initial.group = NULL, initial.response = NULL)
  dialog.values <- getDialog("multiWayAnova", defaults)
  initializeDialog(title = gettextRcmdr("Multi-Way Analysis of Variance"))
  UpdateModelNumber()
  modelName <- tclVar(paste("AnovaModel.", getRcmdr("modelNumber"), 
                            sep = ""))
  modelFrame <- tkframe(top)
  model <- ttkentry(modelFrame, width = "20", textvariable = modelName)
  dataFrame <- tkframe(top)
  groupBox <- variableListBox(dataFrame, Factors(), selectmode = "multiple", 
                              title = gettextRcmdr("Factors (pick one or more)"), 
                              initialSelection = varPosn(dialog.values$initial.group, "factor"))
  responseBox <- variableListBox(dataFrame, Numeric(), title = gettextRcmdr("Response Variable (pick one)"), 
                                 initialSelection = varPosn(dialog.values$initial.response, "numeric"))
  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))
    if (!is.valid.name(modelValue)) {
      UpdateModelNumber(-1)
      errorCondition(recall = multiWayAnova, message = sprintf(gettextRcmdr("\"%s\" is not a valid name."), 
                                                               modelValue))
      return()
    }
    if (is.element(modelValue, listAOVModels())) {
      if ("no" == tclvalue(checkReplace(modelValue, type = gettextRcmdr("Model")))) {
        UpdateModelNumber(-1)
        tkdestroy(top)
        if (getRcmdr("onApplyCalled")){
            putRcmdr("onApplyCalled", FALSE)
            return()
        }
        multiWayAnova()
        return()
      }
    }
    groups <- getSelection(groupBox)
    response <- getSelection(responseBox)
    putDialog ("multiWayAnova", list (initial.group = groups, initial.response = response))
    closeDialog()
    if (length(groups) == 0) {
      errorCondition(recall = multiWayAnova, message = gettextRcmdr("You must select at least one factor."))
      return()
    }
    if (length(response) == 0) {
      errorCondition(recall = multiWayAnova, message = gettextRcmdr("You must select a response variable."))
      return()
    }
    .activeDataSet <- ActiveDataSet()
    groups.list <- paste0(groups, collapse = " + ")
#    doItAndPrint(paste(modelValue, " <- lm(", response, 
#                       " ~ ", paste(groups, collapse = "*"), ", data=", 
#                       .activeDataSet, ", contrasts=list(", paste(paste(groups, '="contr.Sum"'), 
#                       collapse=", "), "))", sep = ""))
    command <- Command ("lm", paste(response, "~", paste(groups, collapse = "*")), 
                       data=.activeDataSet, to=modelValue,
                       paste0("contrasts=list(", groups, "=", "contr.Sum", ")"))
    doItAndPrint(command)
    doItAndPrint(paste("Anova(", modelValue, ")", sep = ""))
    command <- Command ("Tapply", paste0(response, " ~ ", groups.list), "mean, na.action = na.omit",
                        data=.activeDataSet)
    doItAndPrint(paste(command, " # means", sep = ""))
#    doItAndPrint(paste0("Tapply(", response, " ~ ", groups.list, ", mean, na.action=na.omit, data=", 
#                        .activeDataSet, ") # means")) 
#    doItAndPrint(paste0("Tapply(", response, " ~ ", groups.list, ", sd, na.action=na.omit, data=", 
#                        .activeDataSet, ") # std. deviations"))
    command <- Command ("Tapply", paste0(response, " ~ ", groups.list), "sd, na.action = na.omit",
                        data=.activeDataSet)
    doItAndPrint(paste(command, " # std. deviations", sep = ""))
#   doItAndPrint(paste("xtabs(~ ",groups.list, ", data=", .activeDataSet, ") # counts", sep=""))
    command <- Command ("xtabs", paste0("~", groups), data=.activeDataSet)
    doItAndPrint(paste(command, " # counts", sep = ""))
    activeModel(modelValue)
    putRcmdr("modelWithSubset", FALSE)
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "Anova", model = TRUE, reset = "multiWayAnova", apply = "multiWayAnova")
  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Enter name for model: ")), 
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(getFrame(groupBox), labelRcmdr(dataFrame, text="  "), getFrame(responseBox), sticky = "nw")
  tkgrid(dataFrame, sticky="w")
  tkgrid(buttonsFrame, sticky = "w")
  dialogSuffix()
}

oneWayRepeatedMeasures <- function () {
  defaults <- list(initial.rm=rep("", 8), initial.level=paste0("Level-", 1:8),
                   initial.rhs="", initial.testStatistic="Pillai", 
                   initial.test="multivariate", initial.plot=0, initial.print=0, initial.bsfactors=NULL,
                   initial.tab=0, initial.wsfactorName="Trials",
                   initial.trace=gettextRcmdr("<auto>"), initial.xvar=gettextRcmdr("<auto>"), initial.response="score")
  dialog.values <- getDialog ("oneWayRepeatedMeasures", defaults)
  currentFields <- list(rhs=dialog.values$initial.rhs)
  initializeDialog(title = gettextRcmdr("One Repeated Measures Factor ANOVA/ANCOVA"),
                   use.tabs=TRUE, tabs=c("designTab", "optionsTab"))
  onOK <- function() {
    rm1 <- getSelection(rm1ComboxBox)  
    rm2 <- getSelection(rm2ComboxBox)  
    rm3 <- getSelection(rm3ComboxBox)  
    rm4 <- getSelection(rm4ComboxBox)  
    rm5 <- getSelection(rm5ComboxBox)  
    rm6 <- getSelection(rm6ComboxBox) 
    rm7 <- getSelection(rm7ComboxBox)  
    rm8 <- getSelection(rm8ComboxBox)  
    responses <- list(rm1, rm2, rm3, rm4, rm5, rm6, rm7, rm8)
    selected <- !(responses %in% c("", "<no variable selected>"))
    if (all(!selected)){
      errorCondition(recall=oneWayRepeatedMeasures, message=gettextRcmdr("no responses specified"))
      return()
    }
    if (max(which(selected)) > sum(selected)){
      Message(gettextRcmdr("the specified responses are not contiguous\n missing responses are removed"), type="warning")
    }
    responses <- lapply(responses, function(x) if (x %in% c("", gettextRcmdr("<no selection>"))) NULL else x)
    level1value <- tclvalue(level1variable)
    level2value <- tclvalue(level2variable)
    level3value <- tclvalue(level3variable)
    level4value <- tclvalue(level4variable)
    level5value <- tclvalue(level5variable)
    level6value <- tclvalue(level6variable)
    level7value <- tclvalue(level7variable)
    level8value <- tclvalue(level8variable)
    levels <- c(level1value, level2value, level3value, level4value, 
                level5value, level6value, level7value, level8value)
    levels <- levels[selected]
    nlevels <- length(levels)
    if(nlevels < 8) levels <- c(levels, paste0("Level-", as.character((nlevels + 1):8)))
    duplicated.levels <- duplicated(levels)
    if (any(duplicated(levels))){
      errorCondition(recall=oneWayRepeatedMeasures, 
                     message=paste0(gettextRcmdr("there are duplicated level names"), ":\n ", 
                                    paste(unique(levels[duplicated.levels]), collapse=", ")))
      return()
    }
    rhs <- tclvalue(rhsVariable)
    if (trim.blanks(rhs) == "") rhs <- "1"
    testStatistic <- tclvalue(testStatisticVariable)
    test <- tclvalue(testVariable)
    wsfactorName <- tclvalue(wsfactorNameVariable)
    if (!is.valid.name(wsfactorName)){
      errorCondition(recall=oneWayRepeatedMeasures, message=paste(wsfactorName, gettextRcmdr("is not a valid name")))
      return()
    }
    plot <- tclvalue(plotVariable)
    print <- tclvalue(printVariable)
    bsfactors <- getSelection(betweenSubjectsFactors)
    trace <- tclvalue(traceVariable)
    if (trace == "") trace <- gettextRcmdr("<auto>")
    if (!(trace == gettextRcmdr("<auto>") || is.valid.name(trace))){
      errorCondition(recall=oneWayRepeatedMeasures, message=paste(trace, gettextRcmdr("is not a valid name")))
      return()
    }
    xvar <- tclvalue(xvarVariable)
    if (xvar == "") xvar <- gettextRcmdr("<auto>")
    if (!(xvar == gettextRcmdr("<auto>") || is.valid.name(xvar))){
      errorCondition(recall=oneWayRepeatedMeasures, message=paste(xvar, gettextRcmdr("is not a valid name")))
      return()
    }
    response <- tclvalue(responseVariable)
    if (!is.valid.name(response)){
      errorCondition(recall=oneWayRepeatedMeasures, message=paste(response, gettextRcmdr("is not a valid name")))
      return()
    }
    tab <- if (as.character(tkselect(notebook)) == designTab$ID) 0 else 1
    responses <- c(responses[selected], responses[!selected])
    putDialog ("oneWayRepeatedMeasures", list(initial.rm=responses, initial.level=levels, initial.rhs=rhs, 
                                              initial.testStatistic=testStatistic, initial.test=test, initial.plot=plot,
                                              initial.print=print,
                                              initial.bsfactors=bsfactors, initial.tab=tab, initial.wsfactorName=wsfactorName,
                                              initial.trace=trace, initial.xvar=xvar, initial.response=response))
    responses <- unlist(responses)
    duplicates <- duplicated(responses)
    if (any(duplicates)){
      errorCondition(recall=oneWayRepeatedMeasures, message=paste(gettextRcmdr("the following responses appear more than once:"),
                                                                  paste(responses[duplicates], collapse=", ")))
      return()
    }
    closeDialog()
    lhs <- paste0("cbind(", paste(responses, collapse=", "), ")")
    formula <- paste(lhs, "~", rhs)
    m <- length(unlist(responses))
    if (m < 2){
      errorCondition(recall=oneWayRepeatedMeasures, message=gettextRcmdr("at least 2 responses must be specified"))
      return()
    }
    idata = paste0("data.frame(", wsfactorName, "=factor(c(", paste(paste0("'", levels[1:m], "'"), collapse=", "), ")))")
    # command <- if (test == "multivariate"){
    #   paste0("Anova(lm(", formula, ", data=", ActiveDataSet(), ")",
    #          ", idata=", idata, ", idesign = ~", wsfactorName,
    #          ', test.statistic="', testStatistic, '")', sep = "")
    # } else {
    #   paste0("summary(Anova(lm(", formula, ", data=", ActiveDataSet(), ")",
    #          ", idata=", idata, ", idesign = ~", wsfactorName,
    #          '), univariate=TRUE, multivariate=FALSE)', sep = "")
    # }
    command <- if (test == "multivariate"){
      Command("Anova(lm", formula, paste0("data=", ActiveDataSet(), ")"), idata=idata, 
              paste0("idesign = ~", wsfactorName),  
              paste0('test.statistic="',testStatistic,'"'))
    } else {
      Command("summary(Anova(lm", formula, paste0("data=", ActiveDataSet(), ")"), 
              idata=idata, paste0("idesign = ~", wsfactorName),
              "univariate=TRUE, multivariate=FALSE)")
    }
    doItAndPrint(command)
    insertRmdSection(paste0(gettextRmdHeader("Repeated-Measures ANOVA: between = "),
                            rhs, gettextRmdHeader(", within = "), wsfactorName))
    if (plot == "1" || print == "1"){
      within <- paste0("c(", paste(paste0('"', responses, '"'), collapse=", "), ")")
      #within <- paste0("c(", paste(paste0(responses), collapse=", "), ")")
      between <- if (length (bsfactors > 0)){
        paste0(", between.names=c(", paste(paste0('"', bsfactors, '"'), collapse=", "), ")")
      } else {
        ""
      }
      tracefactor <- if (trace == gettextRcmdr("<auto>")) "" else paste0(', trace="', trace, '"')
      xvarfactor <- if (xvar == gettextRcmdr("<auto>")) "" else paste0(', xvar="', xvar, '"')
      
       command <- Command("repeatedMeasuresPlot", ActiveDataSet(), within= within,
                           within.names=Q(wsfactorName), 
                           paste("within.levels=list(", wsfactorName, '=c(', paste(Q(levels)[1:m], collapse=", "),"))"), 
                           print.tables=if (print == "1") TRUE else FALSE,
                           plot.means= if (plot == "1") TRUE else FALSE,
                           between, tracefactor, xvarfactor, paste("response.name=", Q(response), ")"))
       # command <- paste0("repeatedMeasuresPlot(", ActiveDataSet(), ", within=", within,
       #                   ', within.names="', wsfactorName, 
       #                   '", within.levels=list(', wsfactorName, '=c(', paste(paste0('"', levels[1:m], '"'), collapse=", "),
       #                   ')), print.tables=', if (print == "1") "TRUE" else "FALSE",
       #                   ', plot.means=', if (plot == "1") "TRUE" else "FALSE",
       #                   between, tracefactor, xvarfactor, ', response.name="', response, '")')
            doItAndPrint(command)
    }
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "RepeatedMeasuresDialogs", reset = "oneWayRepeatedMeasures", apply = "oneWayRepeatedMeasures")
  withinSubjectsFrame <- ttklabelframe(designTab, labelwidget=tklabel(designTab, text = gettextRcmdr("Within-Subject Factor"),
                                                                      font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  variableNameFrame <- tkframe(withinSubjectsFrame)
  wsfactorNameVariable <- tclVar(dialog.values$initial.wsfactorName)
  wsfactorNameBox <-ttkentry(variableNameFrame, width="20", textvariable=wsfactorNameVariable)
  rm1ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm[[1]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm2ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm[[2]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm3ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm[[3]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm4ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm[[4]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm5ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm[[5]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm6ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm[[6]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm7ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm[[7]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  rm8ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                   initialSelection=dialog.values$initial.rm[[8]], adjustWidth=TRUE,
                                   nullSelection=gettextRcmdr("<no selection>"))
  level1variable <- tclVar(dialog.values$initial.level[[1]])
  level1 <-ttkentry(withinSubjectsFrame, width="10", textvariable=level1variable, foreground=getRcmdr("title.color"))
  level2variable <- tclVar(dialog.values$initial.level[[2]])
  level2 <-ttkentry(withinSubjectsFrame, width="10", textvariable=level2variable, foreground=getRcmdr("title.color"))
  level3variable <- tclVar(dialog.values$initial.level[[3]])
  level3 <-ttkentry(withinSubjectsFrame, width="10", textvariable=level3variable, foreground=getRcmdr("title.color"))
  level4variable <- tclVar(dialog.values$initial.level[[4]])
  level4 <-ttkentry(withinSubjectsFrame, width="10", textvariable=level4variable, foreground=getRcmdr("title.color"))
  level5variable <- tclVar(dialog.values$initial.level[[5]])
  level5 <-ttkentry(withinSubjectsFrame, width="10", textvariable=level5variable, foreground=getRcmdr("title.color"))
  level6variable <- tclVar(dialog.values$initial.level[[6]])
  level6 <-ttkentry(withinSubjectsFrame, width="10", textvariable=level6variable, foreground=getRcmdr("title.color"))
  level7variable <- tclVar(dialog.values$initial.level[[7]])
  level7 <-ttkentry(withinSubjectsFrame, width="10", textvariable=level7variable, foreground=getRcmdr("title.color"))
  level8variable <- tclVar(dialog.values$initial.level[[8]])
  level8 <-ttkentry(withinSubjectsFrame, width="10", textvariable=level8variable, foreground=getRcmdr("title.color"))
  tkgrid(labelRcmdr(variableNameFrame, text=gettextRcmdr("Name for the within-subjects factor: ")), wsfactorNameBox, sticky="w")
  tkgrid(variableNameFrame, sticky="w", columnspan=4)
  tkgrid(labelRcmdr(withinSubjectsFrame, text=gettextRcmdr("Specify up to 8 levels (responses) and level names for the within-subjects factor")), 
         sticky="w", columnspan=4)
  tkgrid(level1, level2, level3, level4, sticky="w")
  tkgrid(getFrame(rm1ComboxBox), getFrame(rm2ComboxBox), getFrame(rm3ComboxBox), getFrame(rm4ComboxBox), sticky="w")
  tkgrid(level5, level6, level7, level8, sticky="w")
  tkgrid(getFrame(rm5ComboxBox), getFrame(rm6ComboxBox), getFrame(rm7ComboxBox), getFrame(rm8ComboxBox), sticky="w")
  tkgrid(withinSubjectsFrame, sticky = "w")
  betweenSubjectsFrame <- ttklabelframe(designTab, labelwidget=tklabel(designTab, text = gettextRcmdr("Between-Subjects Model Formula"),
                                                                       font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  currentModel <- TRUE
  modelFormula(betweenSubjectsFrame, hasLhs = FALSE, rhsExtras=TRUE, formulaLabel="")
  tkgrid(getFrame(xBox), sticky = "w")
  tkgrid(outerOperatorsFrame)
  tkgrid(formulaFrame, sticky = "w")
  tkgrid(labelRcmdr(designTab, text=""), sticky="w")
  tkgrid(betweenSubjectsFrame, sticky = "w")
  optionsFrame <- ttklabelframe(optionsTab, labelwidget=tklabel(optionsTab, text = gettextRcmdr("Hypothesis Tests"),
                                                                font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  radioButtons(optionsFrame, name = "testStatistic", buttons = c("Pillai", "Wilks", "Hotelling", "Roy"), 
               labels = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"), 
               title = gettextRcmdr("Multivariate Test Statistic"), initialValue = dialog.values$initial.testStatistic)
  
  radioButtons(optionsFrame, name = "test", buttons = c("multivariate", "univariate"), 
               labels = c(gettextRcmdr("Multivariate"), gettextRcmdr("Univariate")), 
               initialValue = dialog.values$initial.test,
               title=gettextRcmdr("Tests to Perform"))
  plotFrame <- ttklabelframe(optionsTab, labelwidget=tklabel(optionsTab, text = gettextRcmdr("Repeated-Measures Means"),
                                                             font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  plotVariable <- tclVar(dialog.values$initial.plot)
  plotCheckBoxFrame <- tkframe(plotFrame)
  plotCheckBox <- ttkcheckbutton(plotCheckBoxFrame, variable = plotVariable)
  printVariable <- tclVar(dialog.values$initial.print)
  printCheckBox <- ttkcheckbutton(plotCheckBoxFrame, variable = printVariable)
  betweenSubjectsFactors <- variableListBox(plotFrame, Factors(), selectmode="multiple",
                                            title = gettextRcmdr("Between-Subjects Factors (pick zero or more)"), 
                                            initialSelection = varPosn(dialog.values$initial.bsfactors, "factor"))
  maxchar <- max(15, 2 + max(nchar(c(tclvalue(wsfactorNameVariable), Factors()))))
  traceXvarFrame <- tkframe(plotFrame)
  traceVariable <- tclVar(dialog.values$initial.trace)
  traceBox <-ttkentry(traceXvarFrame, width=maxchar, textvariable=traceVariable)
  xvarVariable <- tclVar(dialog.values$initial.xvar)
  xvarBox <-ttkentry(traceXvarFrame, width=maxchar, textvariable=xvarVariable)
  responseVariable <- tclVar(dialog.values$initial.response)
  responseBox <-ttkentry(traceXvarFrame, width=maxchar, textvariable=responseVariable)
  tkgrid(testFrame, tklabel(optionsFrame, text="   "), testStatisticFrame, sticky="nw")
  tkgrid(testStatisticFrame, sticky="nw")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(labelRcmdr(optionsTab, text=""))
  tkgrid(plotCheckBox, labelRcmdr(plotCheckBoxFrame, text = gettextRcmdr("Plot response means by factors")), 
         sticky = "w")
  tkgrid(printCheckBox, labelRcmdr(plotCheckBoxFrame, text = gettextRcmdr("Print means and standard deviations by factors")), 
         sticky = "w")
  tkgrid(plotCheckBoxFrame, sticky="w")
  tkgrid(getFrame(betweenSubjectsFactors), sticky="w")
  tkgrid(labelRcmdr(traceXvarFrame, text=""))
  tkgrid(labelRcmdr(traceXvarFrame, text=gettextRcmdr("X-axis factor: ")), xvarBox, sticky="w")
  tkgrid(labelRcmdr(traceXvarFrame, text=gettextRcmdr("Trace factor: ")), traceBox, sticky="w")
  tkgrid(labelRcmdr(traceXvarFrame, text=gettextRcmdr("Response name: ")), responseBox, sticky="w")
  tkgrid(traceXvarFrame, sticky="w")
  tkgrid(plotFrame, sticky="w")
  dialogSuffix(use.tabs=TRUE, tabs=c("designTab", "optionsTab"),
               tab.names=c("Design", "Options"), grid.buttons=TRUE)
}

twoWayRepeatedMeasures <- function () {
  checkResponses <- function(x){
    rows <- any(apply(x, 1, 
                      function(r){
                        if (!any(r)) FALSE else max(which(r)) > sum(r)
                      }
    ))
    cols <- any(apply(x, 2, 
                      function(c) {
                        if (!any(c)) FALSE else max(which(c)) > sum(c)
                      }
    ))
    test1 <- !(rows || cols)
    r <- sum(x[, 1])
    c <- sum(x[1, ])
    test2 <- all(x[r, c])
    test1 && test2
  }
  defaults <- list(initial.rm=matrix("", 5, 5), initial.rhs="", initial.testStatistic="Pillai", 
                   initial.test="multivariate",  
                   initial.plot=0, initial.print=0, initial.bsfactors=NULL, 
                   initial.tab=0, 
                   initial.colLevel=paste0("Column-", 1:5),
                   initial.rowLevel=paste0("Row-", 1:5),
                   initial.wsrowfactorName="RowFactor",
                   initial.wscolfactorName="ColumnFactor",
                   initial.trace=gettextRcmdr("<auto>"), initial.xvar=gettextRcmdr("<auto>"), initial.response="score")
  dialog.values <- getDialog ("twoWayRepeatedMeasures", defaults)
  currentFields <- list(rhs=dialog.values$initial.rhs)
  initializeDialog(title = gettextRcmdr("Two Repeated Measures Factors ANOVA/ANCOVA"),
                   use.tabs=TRUE, tabs=c("designTab", "optionsTab"))
  onOK <- function() {
    rm11 <- getSelection(rm11ComboxBox)  
    rm12 <- getSelection(rm12ComboxBox)  
    rm13 <- getSelection(rm13ComboxBox)  
    rm14 <- getSelection(rm14ComboxBox)  
    rm15 <- getSelection(rm15ComboxBox)  
    rm21 <- getSelection(rm21ComboxBox) 
    rm22 <- getSelection(rm22ComboxBox)  
    rm23 <- getSelection(rm23ComboxBox)  
    rm24 <- getSelection(rm24ComboxBox)  
    rm25 <- getSelection(rm25ComboxBox)  
    rm31 <- getSelection(rm31ComboxBox)  
    rm32 <- getSelection(rm32ComboxBox)  
    rm33 <- getSelection(rm33ComboxBox)  
    rm34 <- getSelection(rm34ComboxBox) 
    rm35 <- getSelection(rm35ComboxBox)  
    rm41 <- getSelection(rm41ComboxBox)  
    rm42 <- getSelection(rm42ComboxBox)  
    rm43 <- getSelection(rm43ComboxBox)  
    rm44 <- getSelection(rm44ComboxBox)  
    rm45 <- getSelection(rm45ComboxBox)  
    rm51 <- getSelection(rm51ComboxBox)  
    rm52 <- getSelection(rm52ComboxBox) 
    rm53 <- getSelection(rm53ComboxBox)  
    rm54 <- getSelection(rm54ComboxBox)
    rm55 <- getSelection(rm55ComboxBox)
    responses <- list(rm11, rm12, rm13, rm14, rm15, 
                      rm21, rm22, rm23, rm24, rm25,
                      rm31, rm32, rm33, rm34, rm35,
                      rm41, rm42, rm43, rm44, rm45,
                      rm51, rm52, rm53, rm54, rm55)
    responses <- matrix(responses, nrow=5, ncol=5, byrow=TRUE)
    selected <- !(responses %in% c("", gettextRcmdr("<no selection>")))
    selected <- matrix(selected, nrow=5, ncol=5)
    if (!checkResponses(selected)){
      errorCondition(recall=twoWayRepeatedMeasures, message=gettextRcmdr("the specified responses are not rectangular"))
      return()
    }
    row1 <- selected[1, ]
    ncol <- if (any(row1)) max(which(row1)) else 0
    col1 <- selected[, 1]
    nrow <- if (any(col1)) max(which(col1)) else 0
    if (ncol == 0){
      errorCondition(recall=twoWayRepeatedMeasures, message=gettextRcmdr("no responses specified"))
      return()
    }
    if (ncol < 2 || nrow < 2){
      errorCondition(recall=twoWayRepeatedMeasures, message=gettextRcmdr("at least 2 rows and 2 columns of responses must be specified"))
      return()
    }
    responses <- as.vector(responses)
    save.responses <- unlist(responses)
    save.responses <- matrix(responses, 5, 5)
    responses <- responses[selected]
    rhs <- tclvalue(rhsVariable)
    if (trim.blanks(rhs) == "") rhs <- "1"
    testStatistic <- tclvalue(testStatisticVariable)
    test <- tclvalue(testVariable)
    wsrowfactorName <- tclvalue(wsrowfactorNameVariable)
    wscolfactorName <- tclvalue(wscolfactorNameVariable)
    if (!is.valid.name(wsrowfactorName)){
      errorCondition(recall=twoWayRepeatedMeasures, message=paste(wsrowfactorName, gettextRcmdr("is not a valid name")))
      return()
    }
    if (!is.valid.name(wscolfactorName)){
      errorCondition(recall=twoWayRepeatedMeasures, message=paste(wscolfactorName, gettextRcmdr("is not a valid name")))
      return()
    }
    colLevel1 <- tclvalue(colLevel1variable)
    colLevel2 <- tclvalue(colLevel2variable)
    colLevel3 <- tclvalue(colLevel3variable)
    colLevel4 <- tclvalue(colLevel4variable)
    colLevel5 <- tclvalue(colLevel5variable)
    colLevels <- c(colLevel1, colLevel2, colLevel3, colLevel4, colLevel5)
    rowLevel1 <- tclvalue(rowLevel1variable)
    rowLevel2 <- tclvalue(rowLevel2variable)
    rowLevel3 <- tclvalue(rowLevel3variable)
    rowLevel4 <- tclvalue(rowLevel4variable)
    rowLevel5 <- tclvalue(rowLevel5variable)
    rowLevels <- c(rowLevel1, rowLevel2, rowLevel3, rowLevel4, rowLevel5)
    duplicated.levels <- duplicated(rowLevels)
    if (any(duplicated(rowLevels))){
      errorCondition(recall=twoWayRepeatedMeasures, 
                     message=paste0(gettextRcmdr("there are duplicated row level names"), ":\n ", 
                                    paste(unique(rowLevels[duplicated.levels]), collapse=", ")))
      return()
    }    
    duplicated.levels <- duplicated(colLevels)
    if (any(duplicated(colLevels))){
      errorCondition(recall=twoWayRepeatedMeasures, 
                     message=paste0(gettextRcmdr("there are duplicated column level names"), ":\n ", 
                                    paste(unique(colLevels[duplicated.levels]), collapse=", ")))
      return()
    }    
    plot <- tclvalue(plotVariable)
    print <- tclvalue(printVariable)
    bsfactors <- getSelection(betweenSubjectsFactors)
    trace <- tclvalue(traceVariable)
    if (trace == "") trace <- gettextRcmdr("<auto>")
    if (!(trace == gettextRcmdr("<auto>") || is.valid.name(trace))){
      errorCondition(recall=twoWayRepeatedMeasures, message=paste(trace, gettextRcmdr("is not a valid name")))
      return()
    }
    xvar <- tclvalue(xvarVariable)
    if (xvar == "") xvar <- gettextRcmdr("<auto>")
    if (!(xvar == gettextRcmdr("<auto>") || is.valid.name(xvar))){
      errorCondition(recall=twoWayRepeatedMeasures, message=paste(xvar, gettextRcmdr("is not a valid name")))
      return()
    }
    response <- tclvalue(responseVariable)
    if (!is.valid.name(response)){
      errorCondition(recall=twoWayRepeatedMeasures, message=paste(response, gettextRcmdr("is not a valid name")))
      return()
    }
    tab <- if (as.character(tkselect(notebook)) == designTab$ID) 0 else 1
    putDialog ("twoWayRepeatedMeasures", list(initial.rm=save.responses, initial.rhs=rhs, initial.testStatistic=testStatistic,
                                              initial.test=test, 
                                              initial.plot=plot,
                                              initial.print=print,
                                              initial.bsfactors=bsfactors,
                                              initial.tab=tab, 
                                              initial.colLevel=colLevels,
                                              initial.rowLevel=rowLevels,
                                              initial.wsrowfactorName=wsrowfactorName,
                                              initial.wscolfactorName=wscolfactorName,
                                              initial.trace=trace, initial.xvar=xvar, initial.response=response))
    responses <- unlist(responses)
    duplicates <- duplicated(responses)
    if (any(duplicates)){
      errorCondition(recall=twoWayRepeatedMeasures, message=paste(gettextRcmdr("the following responses appear more than once:"),
                                                                  paste(responses[duplicates], collapse=", ")))
      return()
    }
    closeDialog()
    lhs <- paste0("cbind(", paste(responses, collapse=", "), ")")
    formula <- paste(lhs, "~", rhs)
    m <- length(unlist(responses))
    idata = paste0("data.frame(", wsrowfactorName, " = factor(rep(c(", paste(paste0("'", rowLevels[1:nrow], "'"), collapse=", "), "),", ncol, ")), ",
                   wscolfactorName, " = factor(rep(c(", paste(paste0("'", colLevels[1:ncol], "'"), collapse=", "), "), each=", nrow, ")))")
    # command <- if (test == "multivariate"){
    #   paste0("Anova(lm(", formula, ", data=", ActiveDataSet(), ")",
    #          ", idata=", idata, ", idesign = ~", wsrowfactorName, "*", wscolfactorName,
    #          ', test.statistic="', testStatistic, '")', sep = "")
    # } else {
    #   paste0("summary(Anova(lm(", formula, ", data=", ActiveDataSet(), ")",
    #          ", idata=", idata, ", idesign = ~", wsrowfactorName, "*", wscolfactorName,
    #          '), univariate=TRUE, multivariate=FALSE)', sep = "")
    # }
    command <- if (test == "multivariate"){
      Command("Anova(lm", formula, paste0("data =", ActiveDataSet(), ")"), idata=idata, 
                paste("idesign = ~", wsrowfactorName, "*", wscolfactorName),
                test.statistic=Q(testStatistic))
    } else {
      Command("summary(Anova(lm", formula, 
              paste0(data=ActiveDataSet(), ")"),
              idata= idata, 
              paste0("idesign = ~", wsrowfactorName, "*", wscolfactorName, ")"),
              univariate=TRUE, multivariate=FALSE)
    }
    doItAndPrint(command)
    insertRmdSection(paste0(gettextRmdHeader("Repeated-Measures ANOVA: between = "),
                            rhs, gettextRmdHeader(", within = "), wsrowfactorName, "*", wscolfactorName))
    if (plot == "1" || print == "1"){
      within <- paste0("c(", paste(paste0('"', responses, '"'), collapse=", "), ")")
      between <- if (length (bsfactors > 0)){
        paste0(", between.names=c(", paste(paste0('"', bsfactors, '"'), collapse=", "), ")")
      } else {
        ""
      }
      tracefactor <- if (trace == gettextRcmdr("<auto>")) "" else paste0(', trace="', trace, '"')
      xvarfactor <- if (xvar == gettextRcmdr("<auto>")) "" else paste0(', xvar="', xvar, '"')
      
      command <- paste0("repeatedMeasuresPlot(", ActiveDataSet(), ", within=", within,
                        ', within.names=c("', wsrowfactorName, '", "', wscolfactorName,
                        '"), within.levels=list(', wsrowfactorName, '=c(', paste(paste0('"', rowLevels[1:nrow], '"'), collapse=", "), '), ',
                        wscolfactorName, '=c(', paste(paste0('"', colLevels[1:ncol], '"'), collapse=", "),
                        ')), print.tables=', if (print == "1") "TRUE" else "FALSE",
                        ', plot.means=', if (plot == "1") "TRUE" else "FALSE",
                        between, tracefactor, xvarfactor, ', response.name="', response, '")')
      doItAndPrint(command)
    }
    tkfocus(CommanderWindow())
  }
  OKCancelHelp(helpSubject = "RepeatedMeasuresDialogs", reset = "twoWayRepeatedMeasures", apply = "twoWayRepeatedMeasures")
  withinSubjectsFrame <- ttklabelframe(designTab, labelwidget=tklabel(designTab, text = gettextRcmdr("Within-Subject Factors"),
                                                                      font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  variableNameFrame <- tkframe(withinSubjectsFrame)
  wsrowfactorNameVariable <- tclVar(dialog.values$initial.wsrowfactorName)
  wsrowfactorNameBox <-ttkentry(variableNameFrame, width="20", textvariable=wsrowfactorNameVariable)
  wscolfactorNameVariable <- tclVar(dialog.values$initial.wscolfactorName)
  wscolfactorNameBox <-ttkentry(variableNameFrame, width="20", textvariable=wscolfactorNameVariable)
  rm11ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 1]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm12ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 2]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm13ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 3]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm14ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 4]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm15ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), 
                                    initialSelection=dialog.values$initial.rm[[1, 5]], adjustWidth=TRUE,
                                    nullSelection=gettextRcmdr("<no selection>"))
  rm21ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 1]], nullSelection=gettextRcmdr("<no selection>"))
  rm22ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 2]], nullSelection=gettextRcmdr("<no selection>"))
  rm23ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 3]], nullSelection=gettextRcmdr("<no selection>"))
  rm24ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 4]], nullSelection=gettextRcmdr("<no selection>"))
  rm25ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[2, 5]], nullSelection=gettextRcmdr("<no selection>"))
  rm31ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 1]], nullSelection=gettextRcmdr("<no selection>"))
  rm32ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 2]], nullSelection=gettextRcmdr("<no selection>"))
  rm33ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 3]], nullSelection=gettextRcmdr("<no selection>"))
  rm34ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 4]], nullSelection=gettextRcmdr("<no selection>"))
  rm35ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[3, 5]], nullSelection=gettextRcmdr("<no selection>"))
  rm41ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 1]], nullSelection=gettextRcmdr("<no selection>"))
  rm42ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 2]], nullSelection=gettextRcmdr("<no selection>"))
  rm43ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 3]], nullSelection=gettextRcmdr("<no selection>"))
  rm44ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 4]], nullSelection=gettextRcmdr("<no selection>"))
  rm45ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[4, 5]], nullSelection=gettextRcmdr("<no selection>"))
  rm51ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 1]], nullSelection=gettextRcmdr("<no selection>"))
  rm52ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 2]], nullSelection=gettextRcmdr("<no selection>"))
  rm53ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 3]], nullSelection=gettextRcmdr("<no selection>"))
  rm54ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 4]], nullSelection=gettextRcmdr("<no selection>"))
  rm55ComboxBox <- variableComboBox(withinSubjectsFrame, variableList=Numeric(), adjustWidth=TRUE,
                                    initialSelection=dialog.values$initial.rm[[5, 5]], nullSelection=gettextRcmdr("<no selection>"))
  colLevel1variable <- tclVar(dialog.values$initial.colLevel[[1]])
  colLevel1 <-ttkentry(withinSubjectsFrame, width="10", textvariable=colLevel1variable, foreground=getRcmdr("title.color"))
  colLevel2variable <- tclVar(dialog.values$initial.colLevel[[2]])
  colLevel2 <-ttkentry(withinSubjectsFrame, width="10", textvariable=colLevel2variable, foreground=getRcmdr("title.color"))
  colLevel3variable <- tclVar(dialog.values$initial.colLevel[[3]])
  colLevel3 <-ttkentry(withinSubjectsFrame, width="10", textvariable=colLevel3variable, foreground=getRcmdr("title.color"))
  colLevel4variable <- tclVar(dialog.values$initial.colLevel[[4]])
  colLevel4 <-ttkentry(withinSubjectsFrame, width="10", textvariable=colLevel4variable, foreground=getRcmdr("title.color"))
  colLevel5variable <- tclVar(dialog.values$initial.colLevel[[5]])
  colLevel5 <-ttkentry(withinSubjectsFrame, width="10", textvariable=colLevel5variable, foreground=getRcmdr("title.color"))
  rowLevel1variable <- tclVar(dialog.values$initial.rowLevel[[1]])
  rowLevel1 <-ttkentry(withinSubjectsFrame, width="10", textvariable=rowLevel1variable, foreground=getRcmdr("title.color"))
  rowLevel2variable <- tclVar(dialog.values$initial.rowLevel[[2]])
  rowLevel2 <-ttkentry(withinSubjectsFrame, width="10", textvariable=rowLevel2variable, foreground=getRcmdr("title.color"))
  rowLevel3variable <- tclVar(dialog.values$initial.rowLevel[[3]])
  rowLevel3 <-ttkentry(withinSubjectsFrame, width="10", textvariable=rowLevel3variable, foreground=getRcmdr("title.color"))
  rowLevel4variable <- tclVar(dialog.values$initial.rowLevel[[4]])
  rowLevel4 <-ttkentry(withinSubjectsFrame, width="10", textvariable=rowLevel4variable, foreground=getRcmdr("title.color"))
  rowLevel5variable <- tclVar(dialog.values$initial.rowLevel[[5]])
  rowLevel5 <-ttkentry(withinSubjectsFrame, width="10", textvariable=rowLevel5variable, foreground=getRcmdr("title.color"))
  tkgrid(labelRcmdr(variableNameFrame, text=gettextRcmdr("Name for the within-subjects row factor: ")), wsrowfactorNameBox, sticky="w")
  tkgrid(labelRcmdr(variableNameFrame, text=gettextRcmdr("Name for the within-subjects column factor: ")), wscolfactorNameBox, sticky="w")
  tkgrid(variableNameFrame, sticky="w", columnspan=4)
  tkgrid(labelRcmdr(withinSubjectsFrame, text=gettextRcmdr("Select up to 5 levels and level-names for each within-subjects factor")), 
         sticky="w", columnspan=4)
  tkgrid(labelRcmdr(withinSubjectsFrame, text=""), colLevel1, colLevel2, colLevel3, colLevel4, colLevel5, sticky="w")
  tkgrid(rowLevel1,
         getFrame(rm11ComboxBox), getFrame(rm12ComboxBox), getFrame(rm13ComboxBox), getFrame(rm14ComboxBox),  getFrame(rm15ComboxBox), sticky="sw")
  tkgrid(rowLevel2,
         getFrame(rm21ComboxBox), getFrame(rm22ComboxBox), getFrame(rm23ComboxBox), getFrame(rm24ComboxBox),  getFrame(rm25ComboxBox), sticky="sw")
  tkgrid(rowLevel3,
         getFrame(rm31ComboxBox), getFrame(rm32ComboxBox), getFrame(rm33ComboxBox), getFrame(rm34ComboxBox),  getFrame(rm35ComboxBox), sticky="sw")
  tkgrid(rowLevel4,
         getFrame(rm41ComboxBox), getFrame(rm42ComboxBox), getFrame(rm43ComboxBox), getFrame(rm44ComboxBox),  getFrame(rm45ComboxBox), sticky="sw")
  tkgrid(rowLevel5,
         getFrame(rm51ComboxBox), getFrame(rm52ComboxBox), getFrame(rm53ComboxBox), getFrame(rm54ComboxBox),  getFrame(rm55ComboxBox), sticky="sw")
  tkgrid(withinSubjectsFrame, sticky = "w")
  betweenSubjectsFrame <- ttklabelframe(designTab, labelwidget=tklabel(designTab, text = gettextRcmdr("Between-Subjects Model Formula"),
                                                                       font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  currentModel <- TRUE
  modelFormula(betweenSubjectsFrame, hasLhs = FALSE, rhsExtras=TRUE, formulaLabel="")
  tkgrid(getFrame(xBox), sticky = "w")
  tkgrid(outerOperatorsFrame)
  tkgrid(formulaFrame, sticky = "w")
  tkgrid(labelRcmdr(designTab, text=""), sticky="w")
  tkgrid(betweenSubjectsFrame, sticky = "w")
  optionsFrame <- ttklabelframe(optionsTab, labelwidget=tklabel(optionsTab, text = ""))
  radioButtons(optionsFrame, name = "testStatistic", buttons = c("Pillai", "Wilks", "Hotelling", "Roy"), 
               labels = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy"), 
               title = gettextRcmdr("Multivariate Test Statistic"), initialValue = dialog.values$initial.testStatistic)
  radioButtons(optionsFrame, name = "test", buttons = c("multivariate", "univariate"), 
               labels = c(gettextRcmdr("Multivariate"), gettextRcmdr("Univariate")), 
               initialValue = dialog.values$initial.test,
               title=gettextRcmdr("Tests to Perform"))
  plotFrame <- ttklabelframe(optionsTab, labelwidget=tklabel(optionsTab, text = gettextRcmdr("Repeated-Measures Means"),
                                                             font="RcmdrTitleFont", foreground=getRcmdr("title.color")))
  plotVariable <- tclVar(dialog.values$initial.plot)
  plotCheckBoxFrame <- tkframe(plotFrame)
  plotCheckBox <- ttkcheckbutton(plotCheckBoxFrame, variable = plotVariable)
  printVariable <- tclVar(dialog.values$initial.print)
  printCheckBox <- ttkcheckbutton(plotCheckBoxFrame, variable = printVariable)
  
  betweenSubjectsFactors <- variableListBox(plotFrame, Factors(), selectmode="multiple",
                                            title = gettextRcmdr("Between-Subjects Factors (pick zero or more)"), 
                                            initialSelection = varPosn(dialog.values$initial.bsfactors, "factor"))
  maxchar <- max(15, 2 + max(nchar(c(tclvalue(wsrowfactorNameVariable), tclvalue(wscolfactorNameVariable), Factors()))))
  traceXvarFrame <- tkframe(plotFrame)
  traceVariable <- tclVar(dialog.values$initial.trace)
  traceBox <-ttkentry(traceXvarFrame, width=maxchar, textvariable=traceVariable)
  xvarVariable <- tclVar(dialog.values$initial.xvar)
  xvarBox <-ttkentry(traceXvarFrame, width=maxchar, textvariable=xvarVariable)
  responseVariable <- tclVar(dialog.values$initial.response)
  responseBox <-ttkentry(traceXvarFrame, width=maxchar, textvariable=responseVariable)
  tkgrid(testFrame, tklabel(optionsFrame, text="   "), testStatisticFrame, sticky="nw")
  tkgrid(optionsFrame, sticky="w")
  tkgrid(labelRcmdr(optionsTab, text=""))
  tkgrid(plotCheckBox, labelRcmdr(plotCheckBoxFrame, text = gettextRcmdr("Plot response means by factors")), 
         sticky = "w")
  tkgrid(printCheckBox, labelRcmdr(plotCheckBoxFrame, text = gettextRcmdr("Print means and standard deviations by factors")), 
         sticky = "w")
  tkgrid(plotCheckBoxFrame, sticky="w")
  tkgrid(getFrame(betweenSubjectsFactors), sticky="w")
  tkgrid(labelRcmdr(traceXvarFrame, text=""))
  tkgrid(labelRcmdr(traceXvarFrame, text=gettextRcmdr("X-axis factor: ")), xvarBox, sticky="w")
  tkgrid(labelRcmdr(traceXvarFrame, text=gettextRcmdr("Trace factor: ")), traceBox, sticky="w")
  tkgrid(labelRcmdr(traceXvarFrame, text=gettextRcmdr("Response name: ")), responseBox, sticky="w")
  tkgrid(traceXvarFrame, sticky="w")
  tkgrid(plotFrame, sticky="w")
  dialogSuffix(use.tabs=TRUE, tabs=c("designTab", "optionsTab"),
               tab.names=c("Design", "Options"), grid.buttons=TRUE)
}
