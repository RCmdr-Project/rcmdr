
CRPlot3D <- function () {
  use.rgl <- getOption("Rcmdr")$use.rgl
  if (is.null(use.rgl) || use.rgl) {
    Library("rgl")
    Library("mgcv")
  }
  putRcmdr("rgl.command", TRUE)
  defaults <- list (initial.x = NULL, initial.scales = 1, initial.grid = 1, 
                    initial.ellips = 0, initial.dfNonpar = gettextRcmdr("<auto>"), 
                    initial.bg = "white",
                    initial.identify="not", initial.id.n="2",
                    initial.rotations=0, initial.tab=0)
  dialog.values <- getDialog ("CRPlot3D", defaults)
  initializeDialog(title = gettextRcmdr("3D Component+Residual Plot"), use.tabs=TRUE)
  variablesFrame <- tkframe(dataTab)
  predictors <- Predictors("numeric")
  initial.selection <- if (length(predictors) == 2) 0:1
    else if (is.null(dialog.values$initial.x)) NULL 
    else apply(outer(dialog.values$initial.x, predictors, "=="), 1, which) - 1
  xBox <- variableListBox(variablesFrame, predictors, title = gettextRcmdr("Explanatory variables (pick two)"), 
                          selectmode = "multiple", initialSelection = initial.selection)
  surfacesFrame <- tkframe(optionsTab)
  axisScales <- tclVar(dialog.values$initial.scales)
  axisScalesCheckBox <- ttkcheckbutton(surfacesFrame, variable = axisScales)
  gridLines <- tclVar(dialog.values$initial.grid)
  gridLinesCheckBox <- ttkcheckbutton(surfacesFrame, variable = gridLines)
  dfNonparVariable <- tclVar(dialog.values$initial.dfNonpar)
  dfNonparField <- ttkentry(surfacesFrame, width = "6", textvariable = dfNonparVariable)
  ellipsoid <- tclVar(dialog.values$initial.ellips)
  ellipsoidCheckBox <- ttkcheckbutton(surfacesFrame, variable = ellipsoid)
  bgFrame <- tkframe(optionsTab)
  bgVariable <- tclVar(dialog.values$initial.bg)
  whiteButton <- ttkradiobutton(bgFrame, variable = bgVariable, value = "white")
  blackButton <- ttkradiobutton(bgFrame, variable = bgVariable, value = "black")
  idFrame <- tkframe(optionsTab)
  radioButtons(window=idFrame, name = "identify",
               buttons = c("auto", "mouse", "not"), 
               labels = gettextRcmdr(c("Automatically", "Interactively with mouse", "Do not identify")), 
               title = gettextRcmdr("Identify Points"), 
               initialValue = dialog.values$initial.identify)
  id.n.Var <- tclVar(dialog.values$initial.id.n) 
  npointsSpinner <- tkspinbox(idFrame, from=1, to=10, width=2, textvariable=id.n.Var) 
  rotationsVar <- tclVar(dialog.values$initial.rotations)
  rotationsSpinner <- tkspinbox(surfacesFrame, from=0, to=10, width=2, textvariable=rotationsVar) 
  onOK <- function() {
    tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    x <- getSelection(xBox)
    scales <- tclvalue(axisScales)
    grid <- tclvalue(gridLines)
    dfNonpar <- tclvalue(dfNonparVariable)
    df <- if (dfNonpar == gettextRcmdr("<auto>")) "NULL" else dfNonpar
    dfText <- paste0(", df.mgcv = ", df) 
    ellips <- tclvalue(ellipsoid) 
    bg <- tclvalue(bgVariable)
    identify <- tclvalue(identifyVariable)
    id.n <- tclvalue(id.n.Var)
    identify.text <- switch(identify,
                            auto = paste0(", id=list(method='mahal', n =", id.n, ")"),
                            mouse = ", id=list(method='identify')",
                            not = "")
    rotations <- tclvalue(rotationsVar)
    closeDialog()
    if (is.na(suppressWarnings(as.numeric(id.n))) || round(as.numeric(id.n)) != as.numeric(id.n)){
      errorCondition(recall = CRPlot3D, message = gettextRcmdr("number of points to identify must be an integer"))
      return()
    }
    if (2 != length(x)) {
      errorCondition(recall = CRPlot3D, message = gettextRcmdr("You must select 2 explanatory variables."))
      return()
    }
    putDialog ("CRPlot3D", list(initial.x = x, initial.scales = scales, initial.grid = grid, 
                                 initial.ellips = ellips, initial.dfNonpar = dfNonpar, 
                                 initial.bg = bg,
                                 initial.identify=identify, initial.id.n=id.n,
                                 initial.rotations=rotations, initial.tab=tab))
    scales <- if (tclvalue(axisScales) == 1)  "TRUE" else "FALSE"
    grid <- if (tclvalue(gridLines) == 1) "TRUE" else "FALSE"
    ellips <- if (tclvalue(ellipsoid) == 1) "TRUE"else "FALSE"
    revolutions <- if (rotations != "0") paste(", revolutions =", rotations) else ""
    if (identify == "mouse"){
      RcmdrTkmessageBox(title="Identify Points",
                        message=gettextRcmdr("Drag right mouse button to identify points,\nclick right button to exit."),
                        icon="info", type="ok")
    }
    command <- paste("crPlot3d(", activeModel(), ", \"", x[1], "\", \"", x[2], 
                     "\", bg=\"", bg, "\", axis.scales=", scales, 
                     ", grid=", grid, ", ellipsoid=", ellips, dfText, identify.text, revolutions,
                     ")", sep = "")
    if (identify == "mouse") command <- suppressMarkdown(command)
    doItAndPrint(command)
    putRcmdr("rgl", TRUE)
    .Tcl("update")
    activateMenus()
    tkfocus(CommanderWindow())
    rgl.bringtotop()
  }
  OKCancelHelp(helpSubject = "crPlor3d", reset = "CRPlot3D", apply = "CRPlot3D")
  tkgrid(getFrame(xBox), labelRcmdr(variablesFrame, text = "  "), sticky = "nw")
  tkgrid(variablesFrame, sticky = "nw")
  tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Number of automatic rotations  ")), 
         rotationsSpinner, sticky="w")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Show axis scales")), 
         axisScalesCheckBox, sticky = "w")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Show surface grid lines")), 
         gridLinesCheckBox, sticky = "w")
  dfLabel <- labelRcmdr(surfacesFrame, text = gettextRcmdr("df = "))
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Smooth regression")), 
         dfLabel, dfNonparField, sticky = "w")
  tkgrid.configure(dfLabel, sticky = "e")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Plot 50% concentration ellipsoid")), 
         ellipsoidCheckBox, sticky = "w")
  tkgrid(surfacesFrame, sticky = "w")
  tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("Background Color"), 
                    fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w", columnspan = 2)
  tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("Black")), 
         blackButton, sticky = "w")
  tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("White")), 
         whiteButton, sticky = "w")
  tkgrid(bgFrame, sticky = "w")
  tkgrid(identifyFrame, sticky="w")
  tkgrid(labelRcmdr(idFrame, text=gettextRcmdr("Number of points to identify  ")), npointsSpinner, sticky="w")
  tkgrid(idFrame, sticky="w")
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE)
}

AVPlot3D <- function () {
  use.rgl <- getOption("Rcmdr")$use.rgl
  if (is.null(use.rgl) || use.rgl) {
    Library("rgl")
  }
  putRcmdr("rgl.command", TRUE)
  defaults <- list (initial.x = NULL, initial.scales = 1, initial.grid = 1, 
                    initial.lin=1, initial.robust=0,
                    initial.ellips = 0, 
                    initial.bg = "white",
                    initial.identify="auto", initial.id.n="2",
                    initial.rotations=0, initial.tab=0)
  dialog.values <- getDialog ("AVPlot3D", defaults)
  initializeDialog(title = gettextRcmdr("3D Added-Variable Plot"), use.tabs=TRUE)
  variablesFrame <- tkframe(dataTab)
  coefs <- Coefficients()
  initial.selection <- if (is.null(dialog.values$initial.x)) NULL 
  else apply(outer(dialog.values$initial.x, coefs, "=="), 1, which) - 1
  xBox <- variableListBox(variablesFrame, coefs, title = gettextRcmdr("Coefficients (pick two)"), 
                          selectmode = "multiple", initialSelection = initial.selection)
  surfacesFrame <- tkframe(optionsTab)
  axisScales <- tclVar(dialog.values$initial.scales)
  axisScalesCheckBox <- ttkcheckbutton(surfacesFrame, variable = axisScales)
  gridLines <- tclVar(dialog.values$initial.grid)
  gridLinesCheckBox <- ttkcheckbutton(surfacesFrame, variable = gridLines)
  ellipsoid <- tclVar(dialog.values$initial.ellips)
  ellipsoidCheckBox <- ttkcheckbutton(surfacesFrame, variable = ellipsoid)
  linearLSSurface <- tclVar(dialog.values$initial.lin)
  linearLSCheckBox <- ttkcheckbutton(surfacesFrame, variable = linearLSSurface)
  robustSurface <- tclVar(dialog.values$initial.robust)
  robustCheckBox <- ttkcheckbutton(surfacesFrame, variable = robustSurface)
  bgFrame <- tkframe(optionsTab)
  bgVariable <- tclVar(dialog.values$initial.bg)
  whiteButton <- ttkradiobutton(bgFrame, variable = bgVariable, value = "white")
  blackButton <- ttkradiobutton(bgFrame, variable = bgVariable, value = "black")
  idFrame <- tkframe(optionsTab)
  radioButtons(window=idFrame, name = "identify",
               buttons = c("auto", "mouse", "not"), 
               labels = gettextRcmdr(c("Automatically", "Interactively with mouse", "Do not identify")), 
               title = gettextRcmdr("Identify Points"), 
               initialValue = dialog.values$initial.identify)
  id.n.Var <- tclVar(dialog.values$initial.id.n) 
  npointsSpinner <- tkspinbox(idFrame, from=1, to=10, width=2, textvariable=id.n.Var) 
  rotationsVar <- tclVar(dialog.values$initial.rotations)
  rotationsSpinner <- tkspinbox(surfacesFrame, from=0, to=10, width=2, textvariable=rotationsVar) 
  onOK <- function() {
    tab <- if (as.character(tkselect(notebook)) == dataTab$ID) 0 else 1
    x <- getSelection(xBox)
    scales <- tclvalue(axisScales)
    grid <- tclvalue(gridLines)
    ellips <- tclvalue(ellipsoid) 
    lin <- tclvalue(linearLSSurface)
    robust <- tclvalue(robustSurface)
    bg <- tclvalue(bgVariable)
    identify <- tclvalue(identifyVariable)
    id.n <- tclvalue(id.n.Var)
    identify.text <- switch(identify,
                            auto = paste0(", id=list(method='mahal', n =", id.n, ")"),
                            mouse = ", id=list(method='identify')",
                            not = ", id=FALSE")
    rotations <- tclvalue(rotationsVar)
    closeDialog()
    if (is.na(suppressWarnings(as.numeric(id.n))) || round(as.numeric(id.n)) != as.numeric(id.n)){
      errorCondition(recall = AVPlot3D, message = gettextRcmdr("number of points to identify must be an integer"))
      return()
    }
    if (2 != length(x)) {
      errorCondition(recall = AVPlot3D, message = gettextRcmdr("You must select 2 coefficients."))
      return()
    }
    putDialog ("AVPlot3D", list(initial.x = x, initial.scales = scales, initial.grid = grid, 
                                initial.ellips = ellips, 
                                initial.lin=lin, initial.robust=robust,
                                initial.bg = bg,
                                initial.identify=identify, initial.id.n=id.n,
                                initial.rotations=rotations, initial.tab=tab))
    scales <- if (tclvalue(axisScales) == 1)  "TRUE" else "FALSE"
    grid <- if (tclvalue(gridLines) == 1) "TRUE" else "FALSE"
    ellips <- if (tclvalue(ellipsoid) == 1) "TRUE"else "FALSE"
    lin <- if (tclvalue(linearLSSurface) == 1)  "\"linear\""
    robust <- if (tclvalue(robustSurface) == 1)  "\"robust\""
    surfaces <- c(lin, robust)
    nsurfaces <- length(surfaces)
    revolutions <- if (rotations != "0") paste(", revolutions =", rotations) else ""
    fit <- if (nsurfaces == 0) 
      ", surface=FALSE"
    else if (nsurfaces == 1) 
      paste(", fit=", surfaces, sep = "")
    else paste(", fit=c(", paste(surfaces, collapse = ","), 
               ")", sep = "")
    if (identify == "mouse"){
      RcmdrTkmessageBox(title="Identify Points",
                        message=gettextRcmdr("Drag right mouse button to identify points,\nclick right button to exit."),
                        icon="info", type="ok")
    }
    command <- paste("avPlot3d(", activeModel(), ", \"", x[1], "\", \"", x[2], 
                     "\", bg=\"", bg, "\", axis.scales=", scales, 
                     ", grid=", grid, ", ellipsoid=", ellips, identify.text, revolutions, fit,
                     ")", sep = "")
    if (identify == "mouse") command <- suppressMarkdown(command)
    doItAndPrint(command)
    putRcmdr("rgl", TRUE)
    .Tcl("update")
    activateMenus()
    tkfocus(CommanderWindow())
    rgl.bringtotop()
  }
  OKCancelHelp(helpSubject = "avPlor3d", reset = "AVPlot3D", apply = "AVPlot3D")
  tkgrid(getFrame(xBox), labelRcmdr(variablesFrame, text = "  "), sticky = "nw")
  tkgrid(variablesFrame, sticky = "nw")
  tkgrid(labelRcmdr(surfacesFrame, text=gettextRcmdr("Number of automatic rotations  ")), 
         rotationsSpinner, sticky="w")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Show axis scales")), 
         axisScalesCheckBox, sticky = "w")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Show surface grid lines")), 
         gridLinesCheckBox, sticky = "w")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Plot 50% concentration ellipsoid")), 
         ellipsoidCheckBox, sticky = "w")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Surfaces to Fit"), 
                    fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Linear least-squares")), 
         linearLSCheckBox, sticky = "w")
  tkgrid(labelRcmdr(surfacesFrame, text = gettextRcmdr("Robust linear regression")), 
         robustCheckBox, sticky = "w")
  tkgrid(surfacesFrame, sticky = "w")
  tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("Background Color"), 
                    fg = getRcmdr("title.color"), font="RcmdrTitleFont"), sticky = "w", columnspan = 2)
  tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("Black")), 
         blackButton, sticky = "w")
  tkgrid(labelRcmdr(bgFrame, text = gettextRcmdr("White")), 
         whiteButton, sticky = "w")
  tkgrid(bgFrame, sticky = "w")
  tkgrid(identifyFrame, sticky="w")
  tkgrid(labelRcmdr(idFrame, text=gettextRcmdr("Number of points to identify  ")), npointsSpinner, sticky="w")
  tkgrid(idFrame, sticky="w")
  dialogSuffix(use.tabs=TRUE, grid.buttons=TRUE)
}

