#' @name Commander-es
#' 
#' @aliases Commander-es
#'
#' @title R Commander (Español)
#' 
#' @author John Fox (de la versión inglesa)
#'
#' @author Manuel Muñoz-Márquez (traductor--mantenedor) \email{manuel.munoz@uca.es}
#' 
#' Véase \url{https://knuth.uca.es/R/doku.php?id=equipotraduccion}
#' 
#' @keywords package
#' 
#' @seealso \link{Commander}, \link{Plugins}, \link{Rcmdr.Utilities}, \link[knitr]{knit}, \link[knitr]{knit2pdf}

#' @description
#' Inicia la GUI (Interfaz Gráfica de Usuario) de R Commander
#'
#' @section Empezando:
#' 
#' La interfaz por defecto de R Commander consiste en (de arriba a abajo) una barra de menús, una barra de herramientas, una ventana de instrucciones, una ventana de salida y una ventana de mensajes. 
#' 
#' Las instrucciones para leer, escribir, transformar y analizar datos se ejecutan usando la barra de menú de la parte superior de la ventana de \emph{R Commander}.
#' La mayor parte de los items de este menú le guiarán mediante ventanas de diálogo, preguntando más allá de la especificación.
#' Es aconsejable explorar el menú para ver las opciones disponibles. 
#' 
#' Bajo la barra de menú se encuentra la barra de herramientas con (de izquierda a derecha) un campo de información que muestra el nombre del conjunto de datos activos, botones para editar y mostrar el conjunto de datos activos y un campo de información mostrando el modelo estadístico activo.
#' Bajo la ventana de instrucciones hay un botón \code{Ejecutar} para realizar las órdenes indicadas en la ventana de instrucciones.
#' Los campos de información para los datos y el modelo activo son botones que pueden ser usados para seleccionar éstos entre, respectivamente, conjuntos de datos o modelos disponibles en memoria. 
#' 
#' La mayor parte de las órdenes requiere un conjunto de datos activos.
#' Cuando se ejecuta R Commander no hay conjunto de datos activos, como está indicado en el campo de información del conjunto de datos activos.
#' Un conjunto de datos llega a ser un conjunto de datos activos cuando éste es leído en la memoria desde un paquete R o importado desde un archivo de texto, conjunto de datos SPSS, conjunto de datos Minitab, conjunto de datos STATA, Excel, Access o dBase.
#' Además el conjunto de datos activos puede ser seleccionado desde conjuntos de datos R residentes en memoria.
#' Los datos pueden ser elegidos de entre todos los conjuntos para cada sesión. 
#' 
#' Por defecto, las órdenes son registradas en la ventana de instrucciones (la ventana de texto vacía inmediatamente después de la barra de herramientas); las órdenes y las salidas aparecen en la ventana de resultados (la ventana de texto vacía después de la ventana de instrucciones) y el conjunto de datos activos es adjuntado a la ruta de búsqueda.
#' Para alterar éstos y otros parámetros por defecto, puede consultar la información pertinente en configuración. 
#' 
#' Algunos diálogos de \code{Rcmdr} (éstos en \code{Estadisticos -> Ajuste de modelos}) generan el modelo lineal, modelo lineal generalizado y otros modelos.
#' Cuando se construye un modelo, se convierte en el modelo activo, indicado en el campo de información de la barra de herramientas de R Commander.
#' Los items del menú \code{Modelos} se aplican al modelo activo.
#' Inicialmente, no hay modelo activo.
#' Si hay varios modelos en memoria, puede elegir el modelo activo de entre ellos. 
#' 
#' Si el registro de instrucciones está activo, las órdenes de R generadas desde los menús y los cuadros de diálogos, son introducidas en la ventana de instrucciones de R Comander.
#' Se pueden editar estas órdenes de manera normal y se pueden escribir otras nuevas en la ventana de instrucciones.
#' Las órdenes individuales pueden ser continuadas en más de una línea, pero cada línea después de la primera debe ser identada con uno o más espacios o tabuladores.
#' El contenido de la ventana de instrucciones puede ser almacenado durante o al final de la sesión y un conjunto de instrucciones guardado puede ser cargado en la ventana de instrucciones.
#' El contenido de la ventana de resultados puede ser editado o guardado en un archivo de texto. 
#' 
#' Para volver a ejecutar una orden o un conjunto de ellas, se seleccionan las líneas que se desean ejecutar usando el ratón y se presiona el botón \code{Ejecutar}, situado a la derecha de la barra de herramientas (o \code{Control-R}, para ejecutarlos).
#' Si no hay texto seleccionado el botón \code{Ejecutar} (o \code{Control-R}) envía el contenido de la línea que contiene el cursor de insercción.
#' Observar que se generará un error si la orden o las órdenes enviadas son incompletas. 
#' 
#' Presionando \code{Control-F} se abre un cuadro de diálogo de búsqueda de texto (también es accesible vía \code{Editar -> Buscar}) para buscar el texto en la ventana de instrucciones o la ventana de resultados.
#' Las búsquedas son realizadas en la ventana de instrucciones a menos que primero pulse en la ventana de resultados para activarla. 
#' 
#' Presionando \code{Control-S} se guardará el conjunto de instrucciones o la ventana de resultados. 
#' 
#' Presionando \code{Control-A} se selecciona todo el texto del conjunto de instrucciones o de la ventana de resultados. 
#' 
#' Pulsando el botón derecho del ratón (el tercer botón en un ratón de tres botones) en el conjunto de instrucciones o en la ventana de resultados se abre el menú contextual con los items del menú \code{Editar}, más un item \code{Ejecutar} (en la ventana de instrucciones). 
#' 
#' Cuando ejecute órdenes en la ventana de R Commander, debe asegurarse que la sentencia sea lógica. Por ejemplo, no tiene sentido ajustar un modelo estadístico de un conjunto de datos que no ha sido leído en memoria. 
#' 
#' Presionando una letra (ej. "a") en un cuadro con una lista se recorrerá ésta hasta la siguiente entrada que comience con esa letra desde el principio del cuadro. 
#' 
#' Salir de R Commander se realiza mediante \code{Fichero -> Salir} o cerrando la ventana de R Commander. 
#' 
#' @section Personalización y configuración:
#' 
#' Los archivos de configuración están en el subdirectorio \code{etc} de cada paquete o en la localización dada por \code{etc} y en las opciones de \code{etcMenus} (véase abajo). 
#' 
#' Los menús de \code{Rcmdr} pueden ser personalizados editando el archivo \code{Rcmdr-menus.txt}. 
#' 
#' Algunas funciones (ej. \code{histograma}) que normalmente no crean salida visible cuando se ejecutan desde la consola sí lo harán - a menos que se evite - cuando se ejecuten desde la ventana de instrucciones de R Commander.
#' Tal salida puede ser suprimida listando los nombres de estas funciones en el archivo \code{log-exceptions.txt}. 
#' 
#' Puede añadir código R al paquete, ej., para crear diálogos adicionales, colocando archivos con extensión \code{.R} en el directorio \code{etc}, además puede editar \code{Rcmdr-menus.txt} para proporcionar menús adicionales, submenús o items.
#' Una demostración de esto se proporciona mediante el archivo \code{BoxCox.demo}.
#' Para activar la demo, renombre el archivo a \code{BoxCox.R} y descomente la correspondiente línea del menú en \code{Rcmdr-menus.txt}.
#' Alternativamente, puede editar el código del paquete y recompilarlo. 
#' 
#' Algunas funciones son proporcionadas para ayudar a escribir diálogos y la información del estado de \code{Rcmdr} en un emplazamiento separado.
#' Véase \code{help("\link{Rcmdr.Utilities}")} y el manual suministrado en el directorio \code{doc} del paquete de \code{Rcmdr} para mayor información. 
#' 
#' Además, varias características son controladas mediante opciones, en tiempos de ejecución, establecidas por la orden \code{options("Rcmdr")}.
#' Estas opciones deben ser establecidas antes de cargar el paquete.
#' Si las opciones no están establecidas, que es la situación normal, serán usados los parámetros por defecto.
#' Las opciones se especifican como una lista de pares \code{name$=$values}.
#' Puede no establecer, establecer una, varias, o todas las opciones.
#' Las opciones disponibles son las dadas a continuación: 
#' 
#' \describe{
#' \item{\code{attach.data.set}}{Si es \code{TRUE} (por defecto \code{FALSE}), el conjunto de datos activo es fijado como la ruta de búsqueda.}
#' 
#' \item{\code{check.packages}}{Si es \code{TRUE} (por defecto), al arranque, la presencia de todos los paquetes recomendados de Rcmdr serán comprobados y si alguno no está instalado, Rcmdr preguntará si deben instalarse.} 
#' 
#' \item{\code{command.text.color}}{El color de las órdenes en la ventana de resultados es, por defecto, \code{"red"}.} 
#' 
#' \item{\code{console.output}}{Si es \code{TRUE} la salida será dirigida a la consola de R y la ventana de salida de R Commander no se mostrará. Por defecto es \code{FALSE}.} 
#' 
#' \item{\code{contrasts}}{Ofrece la misma función que la opción general \code{contrasts}; por defecto es \code{c("contr.Treatment", "contr.poly")}.
#'       Cuando se sale de Commander la opción \code{contrasts} vuelve a su valor preexistente.
#'       Observe que \code{contr.Treatment} es del paquete \code{car}.} 
#' 
#' \item{\code{crisp.dialogs}}{Si es \code{TRUE}, los diálogos deben aparecer en la pantalla dibujada completamente, más que acumular dispositivo a dispositivo.
#'       Esta opción debería afectar sólo a versiones Windows de R, pero debe en cualquier caso ser inofensivo.
#'       Por defecto es \code{TRUE} bajo versiones Windows de R 2.1.1 y superiores y \code{FALSE} si no.
#'       Si está trabajando en Windows y encuentra que se incrementan los problemas de estabilidad, pruebe establecer esta opción a \code{FALSE}.}
#' 
#' \item{\code{default.font}}{La fuente por defecto, como la especificación de la fuente de X11, dada en cadena de caracteres.
#'       Si está especificado, este valor toma precedencia sobre el tamaño de la fuente por defecto (abajo).
#'       Esta opción es sólo para sistemas no-Windows.} 
#' 
#' \item{\code{default.font.size}}{Tamaño, en puntos, por defecto de la fuente.
#'       Por defecto es 10 para sistemas Windows y 12 para otro sistemas, salvo especificación de lo contrario (véase el item anterior).
#'       La fuente por defecto es \code{"*helvetica-medium-r-normal-*-xx*"}, donde \code{xx} es por defecto el tamaño de la fuente.
#'       Esta opción es sólo para sistemas no-Windows.} 
#' 
#' \item{\code{double.click}}{Establecer a \code{TRUE} si quiere que un doble click con el botón izquierdo del ratón sirva para pulsar el botón por defecto en todos los diálogos.
#'       Por defecto es \code{FALSE}.}
#' 
#' \item{\code{error.text.color}}{Color de los mensajes de error; por defecto es \code{"red"}.} 
#' 
#' \item{\code{etc}}{Establece la ruta del directorio que contiene los archivos de configuración de Rcmdr; por defecto el subdirectorio \code{etc} del paquete Rcmdr instalado.} 
#' 
#' \item{\code{grab.focus}}{Establecer a \code{TRUE} para "mantener" el enfoque en la ventana actual de Tk, esto es, para prevenir que el enfoque sea cambiado a otra ventana Tk.
#'       En algunos sistemas, mantener el enfoque de esta forma, puede causar problemas.
#'       Por defecto es TRUE. Si experimenta problemas de enfoque, intente establecer esta opción a \code{FALSE}.} 
#' 
#' \item{\code{load.at.startup}}{Vector de caracteres de nombres de los paquetes que deben ser cargados cuando el paquete \code{Rcmdr} es cargado; por defecto se carga sólo el paquete \code{car}.
#'       Otros paquetes requeridos serán cargados cuando se necesiten.
#'       Si esto está disponible, el paquete \code{car} será cargado cuando Commander se inicie en cualquier caso.} 
#' 
#' \item{\code{log.commands}}{Si es \code{TRUE} (por defecto), los comandos son repetidos en la ventana de instrucciones; si es \code{FALSE}, la ventana de instrucciones no se muestra.} 
#' 
#' \item{\code{log.font.size}}{Tamaño de la fuente, en puntos, que es usado en la ventana de instrucciones, en la ventana de resultados, en diálogos recodificados y en expresiones de cáculo, esto es, donde es usada una fuente monoespacio. Por defecto es 10 para sistemas Windows y 12 para otros sistemas.} 
#' 
#' \item{\code{log.height}}{La altura de la ventana de instrucciones, en líneas.
#'       Por defecto es 10. Estableciendo log.height a 0 tiene el mismo efecto que establer \code{log.commands} a \code{FALSE}.} 
#' 
#' \item{\code{log.text.color}}{Color del texto de la ventana de instrucciones; por defecto es \code{"black"}.} 
#' 
#' \item{\code{log.width} }{La anchura de la ventana de instrucciones y la de salida, en caracteres. Por defecto es 80.} 
#' 
#' \item{\code{multiple.select.mode}}{Afecta a la forma en la que múltiples variables son seleccionadas en una caja de listas de variables.
#'       Si se establece a \code{"extended"} (por defecto), el botón izquierdo en una variable selecciona ésta y deselecciona cualquier otra variable que estuviera seleccionada; \code{Control+botón izquierdo} acciona la selección (y puede ser usado para seleccionar variables adicionales); \code{Mayúsculas+botón izquierdo} extiende la selección.
#'       éste es el convenio estándar de Windows. Si lo establece a \code{"multiple"}, el botón izquierdo acciona la selección de una variable y puede ser usado para seleccionar más de una variable. éste es el comportamiento de Rcmdr antes de la versión 1.9-10.} 
#' 
#' \item{\code{output.height}}{Altura de la ventana de resultados, en líneas.
#'       Por defecto es dos veces la altura de la ventana de instrucciones o 20 si la ventana de instrucciones es suprimida.
#'       Establecer \code{output.height} a 0 tiene el mismo efecto que \code{console.output} a \code{TRUE}.} 
#' 
#' \item{\code{output.text.color}}{Color de la salida en la ventana de resultados, por defecto es \code{"blue"}.} 
#' 
#' \item{\code{placement}}{Emplazamiento de la ventana de R Commander, en píxeles; por defecto es \code{"$-40+20$"}, lo que pone la ventana cerca de la esquina superior derecha de la pantalla.}
#' 
#' \item{\code{plugins}}{Vector de caracteres con los nombres de paquetes de plugins de Rcmdr a cargar cuando Commander arranque.
#'       Los paquetes plugins también pueden ser cargados desde el menú \code{Herramientas -> Cargar paquete(s)}.} 
#' 
#' \item{\code{suppress.menus}}{Si es \code{TRUE}, la barra de menús y de herramientas de R Commander son suprimidas, permitiendo que otro programa (como Excel) asuma esas funciones.
#'       Por defecto (por supuesto) es \code{FALSE}.} 
#' 
#' \item{\code{suppress.X11.warnings}}{En (algunos) sistemas Linux X11 se generan múltiples advertencias por las órdenes de Rcmdr, después de abrir la ventana del dispositivo gráfico. 
#'       Establecer esta opción a \code{TRUE} (por defecto cuando arranca interactivamente bajo X11 antes de la versión de R 2.4.0) suprime la aparición de estas advertencias.
#'       Un efecto secundario indeseable es que entonces todas las advertencias y mensajes de error son interceptados por Rcmdr, incluso para las instrucciones introducidas en los avisos de R.
#'       Los mensajes producidos por tales órdenes serán impresos en la ventana de mensajes de R Commander después de la siguiente orden generada en Rcmdr.
#'       Algunas advertencias de X11 puede ser impresas al salir de R Commander.
#'       Este problema sólo se aplica a versiones de R anteriores a 2.4.0 y el valor por defecto de la opción es establecido por consiguiente.} 
#' 
#' \item{\code{retain.messages}}{Si es \code{TRUE} (por defecto \code{FALSE}), el contenido de la ventana de mensajes no es borrado entre mensajes.
#'       En cualquier caso, un mensaje "NOTE" no borrará un anterior "WARNING" o "ERROR".} 
#' 
#' \item{\code{RExcelSupport}}{Establecido como \code{TRUE} (por defecto es \code{FALSE}), los menús y salidas son dirigidas a Excel.} 
#' 
#' \item{\code{scale.factor}}{Factor de escala aplicado a todos los elementos Tk, como las fuentes.
#'       Esto funciona bien sólo en Windows.
#'       Por defecto es \code{NULL}.} 
#' 
#' \item{\code{showData.threshold}}{Si el número de variables en el conjunto de datos activos excede este valor (por defecto, 100), entonces \code{edit()}, más que \code{showData()}, es utilizado para exhibir el conjunto de datos.
#'       Un inconveniente es que el control no se devuelve a Commander hasta que la ventana de edición sea cerrada. 
#'       La razón de esta opción es que \code{showData()} es muy lento cuando el número de variables es grande; fijando el umbral a 0 suprime el uso en conjunto de showData.} 
#' 
#' \item{\code{show.edit.button}}{Fijar a \code{TRUE} (por defecto) si quiere un botón \code{Editar} en la ventana de Commander, que permita editar el conjunto activo de datos.
#'      Los usuarios de Windows pueden desear establecer esta opción a \code{FALSE} para suprimir el botón \code{Editar} porque cambiando los nombres de las variables en el editor de datos se puede causar que R falle (aunque este problema se cree solucionado).} 
#' 
#' \item{\code{sort.names}}{Fijar a \code{TRUE} (por defecto) si se quiere ordenar alfabéticamente el nombre de las variables en una lista de variables.} 
#' 
#' \item{\code{tkwait}}{Esta opción trata un problema que, en mi conocimiento, es raro y puede ocurrir en algunos sistemas no Windows.
#'       Si R Commander causa que se cuelgue R, entonces establezca la opción \code{tkwait} a \code{TRUE}; o conserve la opción en \code{FALSE} e ignórelo.
#'       Un indeseable efecto secundario de establecer la opción \code{tkwait} a \code{TRUE} es que el aviso de órdenes de la sesión de R es suprimido hasta salir de R Commander.
#'       Uno sin embargo todavía puede introducir órdenes por la ventana de instrucciones.
#'       En particular, no hay razón para usar esta opción bajo Windows y no se debería usar con la GUI de R en Windows con salida protegida cuando la salida esté dirigida a la consola de R.} 
#' 
#' \item{\code{use.rgl}}{Si es \code{TRUE} (por defecto), el paquete \code{rgl} será cargado si está presente en una librería accesible, si es \code{FALSE}, el paquete \code{rgl} será ignorado aunque esté disponible. El paquete rgl puede a veces causar problemas cuando se arranca R bajo X11.} 
#' 
#' \item{\code{warning.text.color}}{Color de los mensajes de advertencia; por defecto es \code{"darkgreen"}.}
#' }
#' 
#' Muchas opciones pueden también ser establecidas mediante el menú \emph{Archivo -> Opciones}, que reiniciará R Commander después de que las opciones sean establecidas.
#' 
#' Si quiere lanzar R Commander cuando inicie R, puede incluir la siguiente instrucción en uno de los ficheros de inicio de R (por ejemplo, en el fichero \code{Rprofile.site} de la carpeta \code{etc} de R):
#' 
#' \preformatted{
#' local({
#'    old <- getOption("defaultPackages")
#'    options(defaultPackages = c(old, "Rcmdr"))
#' })
#' }
#' 
#' Las opciones de R Commander puede ser establecidas de forma permanente de la misma forma. Para más información sobre el inicio de R, véase \code{?Startup}.
#' 
#' @section Avisos:
#' 
#' La ventana de instrucciones de R Commander no proporciona una verdadera consola a R y tiene ciertas limitaciones.
#' No se recomienda usar R Commander para la programación sería o el análisis de datos que confíe primordialmente en instrucciones - usar un editor de programación en su lugar. 
#' Por ejemplo, para declaraciones de composiciones de R incluidas entre llaves \code{"\{ \}"}, incluyendo definición de funciones, no serían analizadas ni ejecutadas correctamente, aunque si las líneas después de las primeras que estén identandas.
#' Puede ejecutar declaraciones de composiciones desde la ventana de instrucciones separando los comandos dentro de las llaves por puntos y comas. 
#' 
#' @section Problemas Conocidos:
#'
#' Ocasionalmente, bajo Windows, después de teclear algún texto en un cuadro de diálogo (ej. subconjunto de expresiones en el diálogo de subconjunto de conjunto de datos), algunos botones en el diálogo (ej. el botón Aceptar) pueden no tener efecto cuando sean presionados.
#' Pulsando en cualquier sitio, dentro o fuera del cuadro de diálogo, debería restaurarse las funciones de los botones.
#' Por lo que se ha podido comprobar, éste es un problema con Tcl/Tk de Windows.
#' 
#' @note
#' Bajo Windows, el paquete \code{Rcmdr} puede también funcionar bajo de Rgui en modo SDI (interfaz de único documento) o bajo \code{rterm.exe}; puede ser que experimente problemas ejecutando \code{Rcmdr} bajo \code{ESS} con NTEmacs o XEmacs. 
#' 
#'

NULL
