# SciViews specific R Commander code

# Supplied by Ph. Grosjean
# last modified 18 August 2009 by J. Fox
#  small fix to call to list.files() by J. Fox 17 Jan 05
#  modifications 18 Feb 06 by J. Fox
#  small change 20 Mar 08 by J. Fox

is.SciViews <- function() {
    # SciViews defines the option "SciViews.version".
    # So, we test if we are in SciViews this way:
    res <- !is.null(getOption("SciViews.version"))
    res
    }
    
is.SciViews.TclTk <- function() {
    # Determine if a TclTk-communicating SciViews client is currently running
    res <- (!is.null(getOption("SciViews.TclTk")) && getOption("SciViews.TclTk") == TRUE)
    res
    }
    
tkfocus <- function(...){
    # A call to tcltk:::tkfocus() causes a GPF in SciViews
    # => replaced by this version that check if we are in SciViews or not
    if (!is.SciViews()) tcltk:::tkfocus(...)
    }

svCommander <- function(Version = "1.1-0"){
    # The SciViews specific Commander() function
    if (is.SciViews()) {
        # TO DO: automatically generate the menu from "Rcmdr-menus.txt"
        # Display the R commander menu
        #...
        setOption <- function(option, default, global=TRUE) {
            opt <- if (is.null(current[[option]])) default else current[[option]]
            if (global) putRcmdr(option, opt)
            else opt
            }
        etc <- file.path(.path.package(package="Rcmdr")[1], "etc")
        # Do NOT sink error messages!
        #assign(".messages.connection", textConnection(".messages", open = "w"), envir=.GlobalEnv)
        #sink(.messages.connection, type="message")
        messageTag(reset=TRUE)
		putRcmdr("RcmdrVersion", Version)
		#putRcmdr("length.messages", 0)
        putRcmdr(".activeDataSet", NULL)
        putRcmdr(".activeModel", NULL)
        putRcmdr("logFileName", NULL)
        putRcmdr("outputFileName", NULL)
        putRcmdr("saveFileName", NULL)
        putRcmdr("modelNumber", 0)
        putRcmdr("rgl", FALSE)
        current <- options("Rcmdr")[[1]]
        setOption("log.font.size", if (.Platform$OS.type == "windows") 10 else 12)
        putRcmdr("logFont", tkfont.create(family="courier", size=getRcmdr("log.font.size")))
#    	putRcmdr("operatorFont", tkfont.create(family="courier", size=getRcmdr("log.font.size")))
		scale.factor <- current$scale.factor
        if (!is.null(scale.factor)) .Tcl(paste("tk scaling ", scale.factor, sep=""))
        if (packageAvailable("car")) {
            require("car")
            setOption("contrasts", c("contr.Treatment", "contr.poly"))
            }
        else setOption("contrasts", c("contr.treatment", "contr.poly"))
        setOption("log.commands", TRUE)
        #assign("logCommands", if (log.commands) tclVar("1") else tclVar("0"))
        setOption("console.output", TRUE) # Must be set to TRUE for SciViews app!
        log.height <- as.character(setOption("log.height", if (!getRcmdr("log.commands")) 0 else 10, global=FALSE))
        log.width <- as.character(setOption("log.width", 80, global=FALSE))
    	output.height <- as.character(setOption("output.height",
        	if (getRcmdr("console.output")) 0
        	else if ((as.numeric(log.height) != 0) || (!getRcmdr("log.commands"))) 2*as.numeric(log.height)
        	else 20, global=FALSE))
        setOption("output.height", output.height)
        putRcmdr("saveOptions", options(warn=1, contrasts=getRcmdr("contrasts"), width=as.numeric(log.width),
            na.action="na.exclude", graphics.record=TRUE))
        setOption("double.click", FALSE)
        setOption("sort.names", TRUE)
        setOption("grab.focus", TRUE)
        setOption("attach.data.set", FALSE)
        setOption("log.text.color", "black")
        setOption("command.text.color", "red")
        setOption("output.text.color", "darkblue")
        setOption("error.text.color", "red")
        setOption("warning.text.color", "darkgreen")
        setOption("multiple.select.mode", "extended")
        setOption("suppress.X11.warnings", .Platform$GUI == "X11") # to address problem in Linux
        setOption("showData.threshold", 100)
    	setOption("retain.messages", FALSE)
        setOption("crisp.dialogs",  (.Platform$OS.type == "windows") && (getRversion() >= "2.1.1"))
		if (.Platform$OS.type != "windows") {
        	putRcmdr("oldPager", options(pager=RcmdrPager))
        	default.font.size <- as.character(setOption("default.font.size", 12, global=FALSE))
        	default.font <- setOption("default.font",
            	paste("*helvetica-medium-r-normal-*-", default.font.size, "*", sep=""), global=FALSE)
        	.Tcl(paste("option add *font ", default.font, sep=""))
        	}
    	#if (getRcmdr("crisp.dialogs")) tclServiceMode(on=FALSE)
    	#if (getRcmdr("suppress.X11.warnings")) {
        #	putRcmdr("messages.connection", textConnection(".messages", open = "w", local=FALSE))
        #	sink(getRcmdr("messages.connection"), type="message")
        #	putRcmdr("length.messages", 0)
        #	}
        putRcmdr("commanderWindow", NULL)
        .commander <- NULL
		placement <- setOption("placement", "-40+20", global=FALSE)
#        source.files <- list.files(etc, pattern="\\.R$")  # duplicate line commented out by J. Fox
#        .commander.done <<- tclVar("0")
   #     source.files <- list.files(etc, pattern="\\.[Rr]$")
   #     for (file in source.files) {
   #          source(file.path(etc, file))
   #          cat(paste("Sourced:", file, "\n"))
   #          }
   #     Menus <- read.table(file.path(etc, "Rcmdr-menus.txt"), as.is=TRUE)
	    # TO DO: we need another treatment for this!
        #for (m in 1:nrow(Menus)){
        #    if (Menus[m, 1] == "menu") assign(Menus[m, 2], tkmenu(eval(parse(text=Menus[m, 3])), tearoff=FALSE))
        #    else if (Menus[m, 1] == "item") {
        #         if (Menus[m, 3] == "command")
        #             tkadd(eval(parse(text=Menus[m, 2])),"command", label=Menus[m, 4], command=eval(parse(text=Menus[m, 5])))
        #         else if (Menus[m, 3] == "cascade")
        #             tkadd(eval(parse(text=Menus[m, 2])),"cascade", label=Menus[m, 4], menu=eval(parse(text=Menus[m, 5])))
        #         else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
        #         }
        #    else stop(paste("menu defintion error:", Menus[m, ], collapse=" "))
        #    }
        putRcmdr("Menus", list())
		exceptions <- scan(file.path(etc, "log-exceptions.txt"), what="", quiet=TRUE, comment.char="#")
        putRcmdr("modelClasses", scan(file.path(etc, "model-classes.txt"), what="", quiet=TRUE, comment.char="#"))
		putRcmdr("dataSetName", tclVar("<No active dataset>"))
		putRcmdr("dataSetLabel", NULL)
        putRcmdr("logWindow", NULL)
        putRcmdr("outputWindow", NULL)
		putRcmdr("messagesWindow", NULL)
		putRcmdr("modelName", tclVar("<No active model>"))
        putRcmdr("modelLabel", NULL)
		show.edit.button <- options("Rcmdr")[[1]]$show.edit.button
        show.edit.button <- if (is.null(show.edit.button)) TRUE else show.edit.button
        if (!packageAvailable("rgl")) Message(gettextRcmdr("The rgl package is absent; 3D plots are unavailable."), type="warning")
    	Message(paste(gettextRcmdr("R Commander Version "), getRcmdr("RcmdrVersion"), ": ", date(), sep=""))
		}
    }

svlogger <- function(command){
     # the SciViews specific logger() function
     if (is.SciViews()) {
         # Behaviour is different if it is a TclTk communicating client,
         # or a plug
         if (is.SciViews.TclTk()) { # TclTk SciViews client
             getTemp <- function(x, default = NULL, mode="any") {
                 if  (exists(x, envir = TempEnv(), mode = mode,
					inherits = FALSE)) {
                     return(get(x, envir = TempEnv(), mode = mode,
						inherits = FALSE))
                 } else { # Variable not found, return the default value
                     return(default)
                 }
             }
             if (getRcmdr("log.commands")) {
                 CmdFun <- getTemp(".guiCmd", mode = "function")
                 if (!is.null(CmdFun)) CmdFun(paste("<<<<Log\n", command))
             }
             lines <- strsplit(command, "\n")[[1]]
             for (line in lines) cat(paste("\n> Rcmdr >", line, "\n"))
             command
         } else {    # plug SciViews client
             lines <- strsplit(command, "\n")[[1]]
             for (line in lines) {
                 cat(paste("<<<<History", line, "<<<<\n>>>> ", line,
					"    #[R-cmdr]\n", sep=""))
                 if (getRcmdr("log.commands")) cat(paste("<<<<Script",
					command, "\n<<<<", sep=""))
                 }
             command
             }
         }
     }
    
svMessage <- function(message, type = c("note", "error", "warning")) {
	# the SciViews specific Message function
    if (sink.number() > 0) sink(type="output")   # Make sure output is not diverted!
	type <- match.arg(type)
    type <- type[1]
	if (type == "note") return()    # Currently we display nothing if it is a note!
	if (type != "note") tkbell()
    prefix <- switch(type, error="Rcmdr Error", warning="Rcmdr Warning", note="Rcmdr Note", "Rcmdr")
    if (missing(message)) return()
    message <- paste(prefix, ": ", message, sep="")
    lines <- strsplit(message, "\n")[[1]]
    for (line in lines) cat(line, "\n")
    return(NULL)
	}

activeDataSetEdit <- function() {
    # This is SciViews equivalent to onEdit function of Commander()
    if (activeDataSet() == FALSE)
        return()
    .activeDataSet <- ActiveDataSet()
    command <- paste("fix(", .activeDataSet, ")", sep="")
    justDoIt(command)
    svlogger(command)
    activeDataSet(.activeDataSet)
    invisible()
}

activeDataSetView <- function() {
    # This is SciViews equivalent to onView function of Commander()
    if (activeDataSet() == FALSE) {
        return()
        }
    view.height <- 30 #max(as.numeric(output.height) + as.numeric(log.height), 10)
    ncols <- eval(parse(text=paste("ncol(", ActiveDataSet(), ")")))
    command <- if (packageAvailable("relimp") && ncols <= getRcmdr("showData.threshold")){
        library("relimp", pos=4)
        paste("showData(", ActiveDataSet(), ", placement='-20+200', font=getRcmdr('logFont'), maxwidth=",
        80, ", maxheight=", view.height, ")", sep="")
        }
    else paste("invisible(edit(", ActiveDataSet(), "))", sep="")
    justDoIt(command)
    invisible(svlogger(command))
	}

optionLogCommand <- function() {
    # Change log option in SciViews
    response <- tclvalue(tkmessageBox(message="Log R-cmdr commands in a script?",
        icon="question", type="yesno", default="yes"))
    val <- if (response == "yes") TRUE else FALSE
    putRcmdr("log.commands", val)
    Opts <- options("Rcmdr")[[1]]
    Opts$log.commands <- val
    options(Rcmdr=Opts)
    refreshStatus()
}

optionAttachDataSet <- function() {
    # Change attach option in SciViews
    response <- tclvalue(tkmessageBox(message="Attach active data set?",
        icon="question", type="yesno", default="yes"))
    val <- if (response == "yes") TRUE else FALSE
    putRcmdr("attach.data.set", val)
    Opts <- options("Rcmdr")[[1]]
    Opts$attach.data.set <- val
    options(Rcmdr=Opts)
    refreshStatus()
}

optionSortVariables <- function() {
    # Change sort variable names option
    response <- tclvalue(tkmessageBox(message="Sort variable names alphabetically?",
        icon="question", type="yesno", default="yes"))
    val <- if (response == "yes") TRUE else FALSE
    putRcmdr("sort.names", val)
    Opts <- options("Rcmdr")[[1]]
    Opts$sort.names <- val
    options(Rcmdr=Opts)
    refreshStatus()
}

refreshStatus <- function() {
    # Refresh dataset and model indication in the status bar of SciViews Client
    DataSet <- ActiveDataSet()
    if (is.null(DataSet) || length(DataSet) == 0) DataSet <- "<no data>"
    Model <- ActiveModel()
    if (is.null(Model) || length(Model) == 0) Model <- "<no model>"
    if (getRcmdr("log.commands")) Opts <- " [log]" else Opts <- " "
    if (getRcmdr("attach.data.set")) Opts <- paste(Opts, "[attach]", sep="")
    if (getRcmdr("sort.names")) Opts <- paste(Opts, "[sort]", sep="")
    # If it is a "SciViews TclTk GUI" client, use it...
    if (is.SciViews.TclTk()) {
        cat(paste("Data: ", DataSet, ", Model: ", Model, Opts, sep=""), file = file.path(tempdir(), "svStatus.txt"))
        # getTemp from SciViews[svMisc] is redefined here to avoid a Depends: svMisc!
        getTemp <- function(x, default = NULL, mode="any") {
            if  (exists(x, envir = TempEnv(), mode = mode, inherits = FALSE)) {
                return(get(x, envir = TempEnv(), mode = mode, inherits = FALSE))
            } else { # Variable not found, return the default value
                return(default)
            }
        }
        CmdFun <- getTemp(".guiCmd", mode = "function")
        if (!is.null(CmdFun)) CmdFun("<<<<Status")
        # Possibly update data in the object browser
        if (exists(".guiObjCallback", envir = TempEnv(), inherits = FALSE)) getTemp(".guiObjCallback")()
    } else {    # This should be SciViews Insider, or other similar client
        cat("<<<<StatusTextData: ", DataSet, ", Model: ", Model, Opts, "<<<<", sep="")
    }
}

helpSciViews <- function() {
    if (as.numeric(R.Version()$major) >= 2) print(help("Rcmdr.sciviews-specific"))
    else help("Rcmdr.sciviews-specific")
    }
