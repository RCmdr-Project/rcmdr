\name{Commander}
\alias{Commander}

\title{R Commander}
\description{
  Start the R Commander GUI (graphical user interface)
}
\usage{
Commander()
}

\details{

\bold{Getting Started}

For more detailed information about getting started, see \emph{Help -> Introduction to the R Commander}
from the R Commander menus or Fox (2017).

The default R Commander interface consists of (from top to bottom) a menu bar, a toolbar,
a code window with script and R Markdown tabs, an output window, and a messages window.

Commands to read, write, transform, and analyze data are entered using the menus in the
menu bar at the top of the \emph{Commander} window. Most menu items lead to dialog boxes
requesting further specification. I suggest that you explore the menus to see
what is available.

Below the menu bar is a toolbar with (from left to right) an information field
displaying the name of the active data set; buttons for editing and displaying the active
data set; and an information field showing the active statistical model. There is also
a \emph{Submit} button for re-executing commands in the Script tab.
The information fields for the active data set and active model are actually 
buttons that can be used to select the active data set and model from among, respectively,
data frames or suitable model objects in memory.

Almost all commands require an active data set. When the Commander starts, there is no active
data set, as indicated in the data set information field. A data set becomes the active data set
when it is read into memory from an R package or imported from a text file, SPSS data set,
Minitab data set,  STATA data set, SAS XPORT data set; or an Excel spreadsheet. 
In addition, the active data set can be selected from among R data frames
resident in memory. You can therefore switch among data sets during a session.

By default, commands are logged to the Script tab (the
initially empty text window immediately below the toolbar), and 
commands and output appear in the Output window (the initially empty text window below the
Script tab). Commands that don't require direct user interaction (such as interactive
identification of points on a graph) are also used to create an R Markdown document in
the tab of the same name. When the R Markdown tab is in front, pressing the "Generate HTML report" button
compiles the document to create an html page with input and output, which opens in a web browser. To alter these and
other defaults, see the information below on configuration. Note, for example, that the \pkg{knitr} package
can be used to create a LaTeX document to be compiled to a PDF report, as an alternative to --- or in addition to ---
an R Markdown document (see the \code{use.knitr} option below).
 
Some \pkg{Rcmdr} dialogs (those in the \emph{Statistics -> Fit models} menu) 
produce linear, generalized
linear, or other models. When a model is fit, it becomes the active model, as indicated in the information
field in the R Commander toolbar.
Items in the \emph{Models} menu apply to the active model. Initially, there is no active model. 
If there are several models in memory, you can select the active model from among them.

If command logging in turned on, R commands that are generated from the menus and dialog boxes are
entered into the Script and R Markdown tabs in the Commander. You can edit these commands in the normal
manner and can also type new commands. You can also type explanatory text in the R Markdown tab. Individual commands
in the Script tab can be continued over more than one line, but the several lines of a multi-line command
must be submitted simultaneously. (It is not necessary, as in earlier versions of the
R Commander, to begin continuation lines with white space.) The contents of the Script and R Markdown
tabs can be saved during or at the end of the session, and a saved script or R Markdown document can be loaded into the
respective tabs. The contents of the Output window can also be edited or saved to a text file. Finally,
editing operations also work in the Messages window.

To re-execute a command or set of commands in the Script tab, select the lines to be executed using
the mouse and press the \emph{Submit} button at the right of the toolbar
(or \emph{Control-R}, for "run", or \emph{Control-Tab}). If no text is selected, 
the \emph{Submit} button (or \emph{Control-R} or \emph{Control-Tab}) 
submits the line containing the text-insertion cursor. Note that an error will be generated if the submitted
command or commands are incomplete.

Pressing \emph{Control-F} brings up a find-text dialog box (which can also be accessed via 
\emph{Edit -> Find}) to search for text in the Script tab, R Markdown tab, knitr tab, Output window, or Messages window. Edit functions such as
search are performed in the Script tab unless you first click in another tab or window to make it active.

Pressing \emph{Control-S} will save the Script tab, R Markdown tab, knitr tab, or Output window.

Pressing \emph{Control-A} selects all of the text in the Script tab, R Markdown tab, knitr tab, Output window, or Messages window.

In addition, the following Control-key combinations work in these tabs and windows: \emph{Control-X}, cut;
\emph{Control-C}, copy; \emph{Control-V}, insert; \emph{Control-Z} or \emph{Alt-Backspace}, undo; and \emph{Control-W}, redo.

Under Mac OS X, the \emph{command} key may be used in place of the \emph{Control} key, though the
latter works as well.

Right-clicking the mouse (clicking button 3 on a three-button mouse, or \emph{Control}-left-clicking) in the tabs or windows brings up
a "context" menu with the \emph{Edit}-menu items, plus (in the Script, R Markdown, and knitr tabs) a \emph{Submit} item.

You can open a larger editor window with the document in the Markdown or knitr
tab by making the corresponding selection from the \emph{Edit} menu, the right-click
context menu when the cursor is in the tab, or by pressing \emph{Control-E} when the
cursor is in the tab.

When you
execute commands from the \emph{Commander} window, you must ensure that the sequence of commands
is logical. For example, it makes no sense to fit a statistical model to a data set that has
not been read into memory.

Pressing a letter key (e.g., "a") in a list box will scroll the list box to bring the next entry 
starting with that letter to the top of the box. 

You can cancel an R Commander dialog box by pressing the \emph{Esc} key.

Most R Commander dialogs remember their state when this is appropriate, and can be restored to pristine state by pressing the Reset button.

Some R Commander dialogs have an Apply button that will execute the command generated by the dialog and then re-open the dialog in its previous state.

Exit from the Commander via the \emph{File -> Exit} menu or by closing the \emph{Commander} window.

\bold{Customization and Configuration}

The preferred way of customizing the R Commander is to write a plug-in package:
see \code{help("\link{Plugins}")}.

Alternatively, configuration files reside in the \code{etc} subdirectory of the package, 
or in the locations given by the \code{etc} and \code{etcMenus} options (see below).

The \pkg{Rcmdr} menus can be customized by editing the file \code{Rcmdr-menus.txt}.

You can add R code to the package, e.g., for creating additional dialogs, by placing files with
file type \code{.R} in the \code{etc} directory, also editing \code{Rcmdr-menus.txt} to provide
additional menus, sub-menus, or menu-items.  Alternatively, you can edit 
the source package and recompile it. 

To reiterate, however, the preferred procedure is to write an R Commander plug-in package.

A number of functions are provided to assist in writing dialogs, and \pkg{Rcmdr} state information
is stored in a separate environment. See \code{help("\link{Rcmdr.Utilities}")}
and the manual supplied in the \code{doc} directory of the \pkg{Rcmdr} package for more information.

In addition, several features are controlled by run-time options, set via the \code{options("Rcmdr")}
command. These options should be set before the package is loaded. If the options are unset, which is
the usual situation, defaults are used. Specify options as a list of \emph{name=value} pairs. You can
set none, one, several, or all options. The available options are as follows:

\describe{
	\item{\code{ask.to.exit}}{if \code{TRUE} (the default), then the user is asked whether he or she wants to exit the \pkg{Rcmdr};
	if this option is set to \code{FALSE}, then the subsequent option is also set to \code{FALSE}.}
	
    \item{\code{ask.on.exit}}{if \code{TRUE} (the default), then the user is asked whether to save
    the script file, R Markdown file, and output file when the \pkg{Rcmdr} exits.}

    \item{\code{attach.data.set}}{if \code{TRUE} (the default is \code{FALSE}), the active data set is attached to the
    search path.}

    \item{\code{check.packages}}{if \code{TRUE} (the default), on start-up, the presence of all of the \pkg{Rcmdr}
    recommended packages will be checked, and if any are absent, the \pkg{Rcmdr} will offer to install them.}

    \item{\code{command.text.color}}{Color for commands in the output window; the default is \code{"red"}.}
       
    \item{\code{console.output}}{If \code{TRUE}, output is directed to the \emph{R Console}, and the 
    \emph{R Commander} output window is not displayed. The default is \code{FALSE}, unless the R Commander is running under RStudio, 
    in which case the default is \code{TRUE}.}
    
    \item{\code{crisp.dialogs}}{If \code{TRUE}, dialogs should appear on the screen fully drawn, rather than built up
    widget by widget. Prior to R 2.6.1, this option only works on the Windows version of R, but should in any event be harmless.
    The default is \code{TRUE}. If you encounter stability problems, try setting this option to
    \code{FALSE}.}
    
    \item{\code{default.contrasts}}{Serves the same function as the general \code{contrasts} option; the default is\cr
    \code{c("contr.Treatment", "contr.poly")}. When the Commander exits, the \code{contrasts}
    option is returned to its pre-existing value. Note that \code{contr.Treatment} is from the \code{car} package.}
    
    \item{\code{default.font.family}}{The default font for GUI elements such as menus and text labels, in the form of a 
    Tk font family specification, given in a character string. 
    For example,  \code{"Helvetica"} specifies the sans-serif Helvetica font family.
    The default is taken from the \code{TkDefaultFont}. Normally a sans-serif font should be used.}

    \item{\code{default.font.size}}{The size, in points, of the default font. The default is 10 on non-Windows
    system and the size of the system font on Windows. 
    To set the font size for R input and output, see the \code{log.font.size} option.
    The \pkg{Rcmdr} \code{scale.factor} option may also be used to
    control font size.}
    
    \item{\code{double.click}}{Set to \code{TRUE} if you want a double-click of the left mouse
    button to press the default button in all dialogs. The default is \code{FALSE}.}
    
    \item{\code{editDataset.threshold}}{If the number of values in the current
    data set exceed this value (the default is 10000), then the standard R
    data editor is used in preference to the R Commander \code{editDataset} editor.}
    
    \item{\code{error.text.color}}{Color for error messages; the default is \code{"red"}.}
    
    \item{\code{etc}}{Set to the path of the directory containing the \pkg{Rcmdr}
    configuration files; defaults to the \code{etc} subdirectory of the installed \pkg{Rcmdr} package.}
    
    \item{\code{grab.focus}}{Set to \code{TRUE} for the current Tk window to 
    "grab" the focus --- that is, to prevent the focus from being changed to another Tk window.
    On some systems, grabbing the focus in this manner apparently causes problems. The default is \code{TRUE}. 
    If you experience focus problems, try setting this option to \code{FALSE}.}
    
    \item{\code{help_type}}{This Rcmdr option takes precedence over the global R \code{help_type}
    option (see \code{\link{options}} and \code{\link{help}}), and by default is set to \code{"html"}.}
    
    \item{\code{iconify.commander}}{If \code{TRUE}, the \emph{Commander} window is minimized on startup;
    the default is \code{FALSE}.}
    
    \item{\code{length.output.stack}}{The R Commander maintains a list of output objects, by default including the last
    several outputs; the default length of the output stack is 10. \code{popOutput()} ``pops'' (i.e., returns and removes) the first entry
    of the output stack. Note that, as a stack, the queue is LIFO (``last in, first out'').}
    
    \item{\code{length.command.stack}}{The R Commander also maintains a list of commands that is managed similarly; the default length
    of this stack is also 10.}
      
    \item{\code{log.commands}}{If \code{TRUE} (the default), commands are echoed to the script window;
    if \code{FALSE}, the script window is not displayed.}
    
    \item{\code{log.font.family}}{The font family to be used for text in the script window, output window, messages window,
    etc., specified as a character vector giving a Tk font family. This should normally be a monospaced font like \code{"Courier"}.
    The default is taken from the \code{TkFixedFont}.}

    \item{\code{log.font.size}}{The font size, in points, to be used in the script window,
    in the output window, messages window,
    in recode dialogs, and in compute expressions --- that is, where a monospaced font is used. The default is 10.
    Alternatively the \pkg{Rcmdr} \code{scale factor} option may also be used to
    control font size.}
    
    \item{\code{log.height}}{The height of the script window, in lines. The default is 10. Setting \code{log.height}
    to 0 has the same effect as setting \code{log.commands} to \code{FALSE}.}
        
    \item{\code{log.text.color}}{Color for text in the script window; the default is \code{"black"}.}
        
    \item{\code{log.width}}{The width of the script and output windows, in characters. The default is 80.}
    
    \item{\code{messages.height}}{The height of the messages window, in lines. The default is 3.} 
    
    \item{\code{multiple.select.mode}}{Affects the way multiple variables are selected in variable-list boxes.
    If set to \code{"extended"} (the default), left-clicking on a variable selects it and deselects any other
    variables that are selected; Control-left-click toggles
    the selection (and may be used to select additional variables); Shift-left-click extends the selection.
    This is the standard Windows convention. If set
    to \code{"multiple"}, left-clicking toggles the selection of a variable and may be used to select more than one
    variable. This is the behaviour in the \pkg{Rcmdr} prior to version 1.9-10.}
    
    \item{\code{number.messages}}{If \code{TRUE}, the default, messages in the messages window are numbered.}
    
    \item{\code{output.height}}{The height of the output window, in lines. The default is twice the height
    of the script window, or 20 if the script window is suppressed. Setting \code{output.height}
    to 0 has the same effect as setting \code{console.output} to \code{TRUE}.}
        
    \item{\code{output.text.color}}{Color for output in the output window; the default is \code{"blue"}.}
        
    \item{\code{placement}}{Placement of the \emph{R Commander} window, in pixels; the default is \code{""},
    which lets the Tk window manager decide where to place the window; for example,
    \code{"+20+20"} should put the window near the upper-left corner of the screen, \code{"-20+20"} near the
    upper-right corner, though this doesn't appear to work reliably on Windows systems.}
    
    \item{\code{plugins}}{A character vector giving the names of \pkg{Rcmdr} plug-in packages to load when the 
    Commander starts up. Plug-in packages can also be loaded from the \emph{Tools -> Load Rcmdr plug-in(s)} menu.
    See \link{Plugins}.}
    
    \item{\code{prefixes}}{A four-item character vector to specify the prefixes used when output is directed
    to the R console; the default is \code{c("Rcmdr> ", "Rcmdr+ ", "RcmdrMsg: ", "RcmdrMsg+ ")}.}
    
    \item{\code{quit.R.on.close}}{if \code{TRUE}, both the Commander and R are exited when the Commander
    window is closed. The default is \code{FALSE}, in which case only the Commander is exited (and can be restarted
    by the command \code{Commander()}).}
    
    \item{\code{RcmdrEnv.on.path}}{If \code{TRUE} (the default is \code{FALSE}), 
    the environment in which R Commander state information
    is stored is placed on the search path. Some plug-ins, at least until
    they are updated, may require this setting.}
    
    \item{\code{retain.messages}}{If \code{TRUE} (the default), the contents of the message window
    are not erased between messages. In any event, a "NOTE" message will not erase a preceding "WARNING" or "ERROR".}
    
    \item{\code{retain.selections}}{If \code{TRUE} (the default), dialogs remember their previous state, where appropriate,
     as long as the data set isn't changed; some dialogs, e.g., for probabilities, retain selections even when the data set chanages.}
    
    \item{\code{RExcelSupport}}{If \code{TRUE} (the default is \code{FALSE}), menus and output are handled by Excel.}
    
    \item{\code{rmd.output.format}}{The output file type for R Markdown documents if pandoc is installed; one of \code{"html"}
    (the default), \code{"pdf"} (requires LaTeX), \code{"docx"} (Word), or \code{"rtf"} (rich text file).}
    
    \item{\code{rmd.template}}{The quoted path to a \code{.Rmd} file to serve as a template for
    R code and output. The default is to use a template included with the package.}
    
    \item{\code{scale.factor}}{A scaling factor to be applied to all Tk elements, such as fonts. This works well
    only in Windows. The default is \code{NULL}.}
    
    \item{\code{scientific.notation}}{Higher numbers cause ordinary (decimal) notation to be increasingly preferred to scientific notation
    for representing very small and very large numbers; correspond to the \code{scipen} option in R: see \code{\link{options}}. The default
    is \code{5}, while the standard default in R is \code{0} (where 0 means that scientific notation is used whenever the resulting printed
    representation of a number is smaller in scientific than in standard notation).}

    \item{\code{showData.threshold}}{a vector with 2 entries, defaulting to \code{c(20000, 100)}. 
    If the number of cases in the active data set exceeds the first number (default, 20,000)
    or the number of variables exceeds the second number (default, 100),
    then \code{View()} rather than \code{showData()} is used to display the data set. The reason for the option is that \code{showData()} is very slow
    when the number of cases or variables is large; setting the threshold to \code{c(0, 0)} suppresses the use of \code{showData} altogether. 
    It's necessary to use \code{showData} however for the view of the active data set to be updated dynamically when, e.g., a variable is added.}
         
    \item{\code{show.edit.button}}{Set to \code{TRUE} (the default) if you want an \emph{Edit} button in the Commander
    window, permitting you to edit the active data set. Windows users may wish to set this option to \code{FALSE}
    to suppress the \emph{Edit} button because changing variable names in the data editor can cause R to crash
    (though I believe that this problem as been solved).}
    
    \item{\code{sort.names}}{Set to \code{TRUE} (the default) if you want variable names to be sorted
    alphabetically in variable lists.}
    
    \item{\code{suppress.icon.images}}{Set to \code{TRUE} to suppress the icon images in dialog OK, Cancel, Reset, and Help buttons;
    the default is \code{FALSE}.}
    
    \item{\code{suppress.menus}}{if \code{TRUE}, the Commander menu bar and tool bar are suppressed, allowing another
    program (such as Excel) to take over these functions. The default (of course) is \code{FALSE}.}
    
    \item{\code{suppress.X11.warnings}}{On (some?) Linux and Mac OS X systems, multiple X11 warnings are generated by \pkg{Rcmdr}
    commands after a graphics-device window has been opened. Set this option to \code{TRUE} (the default when running
    interactively under X11) to suppress reporting of these warnings. An undesirable side effect
    is that then \emph{all} warnings and error messages are intercepted by the \pkg{Rcmdr},
    even those for commands entered at the R command prompt. Messages produced by such commands will be printed
    in the Commander Messages window after the next \pkg{Rcmdr}-generated command.
    Some X11 warnings may be printed when you exit from the Commander.}
    
    \item{\code{theme}}{A ttk theme to control the overall style of the Commander GUI; should be one of the themes returned by \code{tcltk2::tk2theme.list()}.
    The default theme varies by operating system, and can be discovered by entering the command \code{tcltk2::tk2theme()} in a fresh R session.}
    
    \item{\code{title.color}}{Color for the titles of some widgets, such as variable-list boxes; can be given as a color name, such as
    \code{"blue"} or as an RGB value, such as \code{"#0000FF"}.
    The default is the standard color for ttk label frames, unless that is \code{"#000000"} or \code{"black"}, in which case \code{"blue"} is used instead.}
        
    \item{\code{tkwait.commander}}{This option addresses a problem that, to my knowledge, is rare, 
    and may occur on some non-Windows systems. If the Commander causes R to hang, 
    then set the \code{tkwait} option to
    \code{TRUE}; otherwise set the option to \code{FALSE} or ignore it. 
    An undesirable side effect of setting
    the \code{tkwait.commander} option to \code{TRUE} is that the R session command prompt is suppressed until the Commander exits.
    One can still enter commands via the script window, however. In particular, there is no reason to use this option
    under Windows, and it should not be used with the Windows R GUI with buffered output when output is directed to the
    R console.}
    
    \item{\code{tkwait.dialog}}{If \code{TRUE} (the default is \code{FALSE}), R will
    wait until an R Commander dialog is closed. This has the disadvantage of preventing
    help pages from being displayed until a dialog is closed in the Mac OS X R.app 
    and in RStudio. This was also the standard behavior of the R Commander in earlier
    versions and is provided for compatibility with previous behavior. If this option is \code{TRUE},
    then the R Commander data editor is disabled in favor of the standard R platform-specific
    data editor, and the new-data-set menu item is suppressed.}
    
    \item{\code{use.knitr}}{If \code{TRUE} (the default is \code{FALSE}), a knitr \code{.Rnw} LaTeX document is created in a tab of the main Commander window;
    this document can be compiled into \code{.tex} and \code{.pdf} reports via the \code{\link[knitr]{knit2pdf}}
    function in the \pkg{knitr} package.}
    
    \item{\code{use.markdown}}{If \code{TRUE} (the default is the negation of the \code{use.knitr} argument), an R Markdown document
    is created, which can be compiled into an HTML, PDF, Word, or rich text file report.}
    
    \item{\code{use.rgl}}{If \code{TRUE} (the default), the \code{rgl} package will be loaded if it is present in an
    accessible library; if \code{FALSE}, the \code{rgl} package will be ignored even if it is available. 
    The \code{rgl} package can sometimes cause problems when running R under X11.}
    
    \item{\code{"valid.classes"}}{The classes of variables that the R Commander recognizes, in addition to numeric data; other variables
    in a data set will be suppressed. The default is \code{"factor", "ordered", "character", 
    "logical", "POSIXct", "POSIXlt", "Date", "chron", "yearmon", "yearqtr", "zoo", "zooreg", "timeDate", "xts", 
    "its", "ti", "jul", "timeSeries", "fts")}.}
    
    \item{\code{variable.list.height}}{the number of items (typically variables) to display in list boxes; longer lists may be viewed by scrolling. 
    The default is 6.} 
    
    \item{\code{variable.list.width}}{a two-item vector controlling the width of list boxes, in characters, giving the minimum and maximum
    width to display; the default is \code{c(20, Inf)}. If the widest item name falls in this range, then its number of characters
    determines the width of the box. \emph{Note:} This specification works only approximately.}
    
    \item{\code{warning.text.color}}{Color for warning messages; the default is \code{"darkgreen"}.}
    }
    
Some options can also be set via the \emph{File -> Options} menu, which will restart the Commander after options are set.

If you want always to launch the R Commander when R starts up, you can include the following code in one of R's start-up 
files (e.g., in the \code{Rprofile.site} file in R's \code{etc} subdirectory):

\preformatted{
local({
old <- getOption("defaultPackages")
options(defaultPackages = c(old, "Rcmdr"))
})
}

R Commander options can also be permanently set in the same manner.
For more information about R initialization, see \code{?Startup}.
}

\section{Warning}{
    The R Commander Script window does not provide a true console to R, and may have certain limitations.
    I don't recommend using the R Commander for serious programming or for data analysis that relies
    primarily on scripts --- use a programming editor instead. If you encounter any problems with the
    Script tab, however, I'd appreciate it if you brought them to my attention.
    }

\note{
  On startup, the R Commander sets \code{options(na.action=na.exclude)}; this is done so that
  observation statistics such as residuals can be properly added to the active data set
  when there are missing values. The
  option is reset to its pre-existing value when the Commander exits. Some functions may not
  work properly when the default \code{na.action} is set to \code{na.exclude}.

  This version should be compatiable with the \pkg{RExcel} package, which can use the R Commander
  menus.
}

\section{Platform-Specific Issues}{
  Under Windows, the \pkg{Rcmdr} package can be run under the \emph{Rgui} in the SDI (single-document
  interface) mode, or under \code{rterm.exe}. You might experience problems running the \pkg{Rcmdr} under
  \code{ESS} with NTEmacs or XEmacs, or under other R consoles. The R Commander can be run under the \emph{Rgui} in 
  MDI (multiple-document interface) mode but it is relatively inconvenient to do so and isn't recommended.
  
  Occasionally, under Windows, after typing some text into a dialog box (e.g., a subsetting expression in the Subset Data Set dialog), 
  buttons in the dialog (e.g., the OK button) will have no effect when they are pressed. Clicking anywhere inside or outside of the dialog 
  box should restore the function of the buttons. As far as I have been able to ascertain, this is a problem with Tcl/Tk for Windows.
  I have not seen this behavior in some time and the problem may have been solved.
  
  Under Mac OS X Mavericks and later, the R Commander may appear to freeze or hesitate when run under \emph{R.app} if the \emph{R.app} window is hidden
  and "app nap" is turned on. It is recommended that app nap be turned off for \emph{R.app}, which can be most conveniently done via the
  R Commander \emph{Tools} menu. The app nap setting is permanent until changed and so the current setting will apply whether or not the
  R Commander is used. When R is first installed, app nap will be on for \emph{R.app}. The \pkg{tcltk} package requires that X Windows is
  installed under Mac OS X, and as a consequence the \pkg{Rcmdr} package, which depends on \pkg{tcltk}, will not load if X Windows is
  absent. X Windows for Mac OS X may be obtained from \url{http://xquartz.macosforge.org}.
}

\references{
Fox, J. (2017)
\emph{Using the R Commander: A Point-and-Click Interface for R.}
Chapman and Hall/CRC Press.

Fox, J. (2005) 
The R Commander: A Basic Statistics Graphical User Interface to R. 
\emph{Journal of Statistical Software}, \bold{14(9)}: 1--42.

Fox, J. (2007)
Extending the R Commander by "plug in" packages. 
\emph{R News}, \bold{7(3)}: 46--52.
}

\author{John Fox \email{jfox@mcmaster.ca}}

\seealso{\link{Plugins}, \link{Rcmdr.Utilities}, \code{\link[knitr]{knit}}, \code{\link[knitr]{knit2pdf}}}

\examples{
    options(Rcmdr=list(log.font.size=12, default.contrasts=c("contr.Sum", "contr.poly")))
    }

\keyword{misc}
