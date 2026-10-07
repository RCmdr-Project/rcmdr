# Rcmdr

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/Rcmdr)](https://cran.r-project.org/package=Rcmdr)
[![CRAN\_MonthlyDownloads](http://cranlogs.r-pkg.org/badges/Rcmdr)](https://cran.r-project.org/package=Rcmdr)

An [R](https://www.r-project.org/) package that provides a platform-independent basic-statistics GUI (graphical user interface) for R, based on the tcltk package.

## Installation

Install the released version of **Rcmdr** from CRAN using:

``` r
install.packages("Rcmdr")
```

or from GitHub using:

``` r
devtools::install_github("RCmdr-Project/rcmdr/pkg")
```

or the development version from GitHub using:

``` r
devtools::install_github("RCmdr-Project/rcmdr/pkg@devel")
```

Install the `devtools` package using 

``` r
install.packages("devtools")
```

if necessary, before installing from GitHub.

## History

From its initial release (version 0.8-2) on 2003-05-26, through version 2.9-5 on 2024-10-25, John Fox was the lead developer of the Rcmdr package. Following his passing in November 2025, version 2.12.2 (released on 2026-04-11) marks the transition to a new maintainer and development team.

For a complete list of previous released versions, see [https://cran.r-project.org/src/contrib/Archive/Rcmdr/](https://cran.r-project.org/src/contrib/Archive/Rcmdr/).

## Language

R typically detects the system's language automatically and, if a translation is available, uses it by default. If the automatic detection fails, or if you wish to change the language for any other reason, you can do so by following the steps below.

Starting with R-Commander version 2.12.2 (2026-04-11), you can change the language by selecting `Tools` -> `Options...` from the menu. 
A dialog box will appear showing the available languages. 
After selecting your preferred language and clicking the `Restart R Commander` button, the interface language will update.

To make this change permanent, select `Tools` -> `Save Rcmdr options...` from the menu. 
In the dialog box uncomment the following lines (remove the # characters) to start the R Commander automatically whenever R starts:

\# local({

\#    old <- getOption('defaultPackages')

\#    options(defaultPackages = c(old, 'Rcmdr'))

\# })

You may also make additional changes to the startup options at this time. 
Then, click the `Save` button and choose your user’s home directory to save the file. 
Note that if you modify the filename, it will not be loaded automatically upon startup.

## Translations 

### Available Translations
Currently, translations are available in the following languages:

**[ca]**
: Catal&agrave; (Catalan by Manel Salamero 2016-09-03)

**[de]**
: Deutsch (German by Friedrich Leisch 2017-10-13)

**[el]**
: Ελληνικά (Elliniká) (Greek, Modern by Anastasios Vikatos, Andreas Vikatos and Vasileios Dimitropoulos 2015-09-06)

**[es]**
: Espa&ntilde;ol (Spanish by M. Munoz-Marquez always synchronized)

**[eu]**
: Euskara (Basque by José Ramón Rueda 2020-08-27)

**[fr]**
: Fran&ccedil;ais (French by Milan Bouchet-Valat 2020-04-17)

**[gl]**
: Galego (Galician by Antón Meixome 2015-09-17)

**[hu]**
: Magyar (Hungarian by Tamás Ferenci 2020-08-26)

**[id]**
: Bahasa Indonesia (Indonesian by I Made Tirta 2015-09-11)

**[it]**
: Italiano (Italian by Stefano Calza 2015-08-24)

**[ja]**
: &#26085;&#26412;&#35486; (Japanese by Rcmdr Japanese Translation 2022-08-11)

**[ko]**
: &#54620;&#44397;&#50612; (Korean by Jong-Hwa Shin 2022-08-07)

**[pl]**
: Polski (Polish by Łukasz Daniel 2026-06-20)

**[pt_BR]**
: Portugu&ecirc;s do Brasil(Portuguese by Marilia Sá Carvalho 2015-09-25)

**[ro]**
: Rom&acirc;n&#259; (Romanian by Adrian Dusa 2022-07-11)

**[ru]**
: &#1056;&#1091;&#1089;&#1089;&#1082;&#1080;&#1081; (Russian by Alexey Shipunov 2018-08-20)

**[sl]**
: Slovenščina (Slovenian by Jaro Lajovic 2026-09-20)

**[zh]**
: &#32321;&#39636;&#20013;&#25991; (Traditional Chinese by Li Cheng Hsun 2022-07-11)

**[zh_CN]**
: &#31616;&#20307;&#20013;&#25991; (Simplified Chinese by Shulin Yang 2017-04-17)

Please note that translation coverage varies depending on the language, as not all translations are updated to the same extent. 
Translations into new languages and contributions from new translators are very welcome. 

### How to Create or Update a Translation Using Poedit

If you want to contribute a new translation or update an existing one, we recommend using **Poedit**, a free and easy-to-use translation editor.

Follow these steps to translate the files:

1. Download and Install Poedit
Get the latest version of Poedit from their official website [poedit.net](http://poedit.net) and install it on your computer.

2. Open the Translation File
* **To update an existing translation:** Open the current `.po` file for your language (for example, `R-ca.po`).
* **To start a new translation:** Open the `.pot` template file. 
Poedit will ask you to set the new language for your translation and will create a new `.po` file for you.

3. Update from the Latest Template (Optional but recommended)
If the software has been updated recently, you should update your `.po` file to include the latest strings. 
Go to the top menu and select **Catalog > Update from POT file...**, then choose the most recent `.pot` template.

4. Translate the Strings
* Click on any line in the list. The original English text (`msgid`) will appear in the **Source text** box.
* Type your translation into the **Translation** box (`msgstr`) at the bottom.
* *Note: You only need to type in the Translation box. Never edit the Source text.*

5. Preserve Special Characters
Pay close attention to special programming characters or variables, such as `%s`, `%d`, or `\n`. 
You must include these exact same characters in your translated text, as the software needs them to function correctly.

6. Save and Compile Your Work
When you are done translating (or want to take a break), go to **File > Save**. 
Poedit will automatically save your `.po` file and generate a compiled `.mo` file. 
If the `.mo` file is not created automatically, you can generate it manually by going to **File > Compile to MO...** in the top menu. 
Make sure both files are saved in the same folder.

7. Submit Your Translation
Once both your `.po` and `.mo` files are complete and saved, you need to share them with the development team. 
**Please make sure to submit both files**, as the software requires the compiled `.mo` file to run, and the development team needs the `.po` file to make future updates. 
You can submit them in one of the following ways:
* **Via Email:** Send both the `.po` and `.mo` files as attachments directly to the maintainer's email address.
* **Via GitHub:** If you are familiar with Git, fork the repository, commit both files to the development branch (`devel`), and open a Pull Request.

## Release plan

The current devel version is available in the devel branch for testing purposes.

New versions are expected to be released at least every six months. 
A development version will be made available for testing one month prior to each official release.
