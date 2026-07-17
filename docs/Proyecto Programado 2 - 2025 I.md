

Instituto  Tecnológico  de Costa  Rica
Ingeniería en Computación
Lenguajes  de Programación
## Semestre I, 2025
## Profesor: Allan Rodríguez  Dávila

## Proyecto Programado #2
Gestión  de Finca Agrícola
## Introducción
El objetivo de este proyecto es desarrollar un sistema para gestionar las operaciones agrícolas
dentro de una finca productora de vegetales. El sistema debe permitir registrar y consultar las
parcelas de cultivo, gestionar las herramientas utilizadas, registrar cosechas, y consultar la
disponibilidad de parcelas según fechas. También permitirá llevar un control de los trabajadores y
generar reportes estadísticos sobre la producción.

Se espera que el proyecto siga los principios del paradigma funcional, como la inmutabilidad de
datos y el uso de funciones puras.

Además, el proyecto busca someter al estudiante a entornos y realidades lo más cercano a la
realidad, con el objetivo de generar un aprendizaje efectivo.
Proyecto a desarrollar
Su trabajo consiste en implementar una aplicación de escritorio para administrar la gestión de una
finca agrícola utilizando el lenguaje Haskell y la Programación Funcional.

El programa deberá desplegar un menú al usuario con dos submenús o grupo de funciones (el
menú debe mostrarse recurrentemente después de cada opción realizada -a excepción del salir- y
tener la opción de subir al menú principal desde los menús secundarios) y salir:
## Opciones Operativas
Al acceder a estas funciones, el usuario debe ingresar su cédula, que debe existir en el registro de
trabajadores de la finca (ver sección Información de Trabajadores).


Al indicar el usuario se habilitará un menú donde se habilitarán las siguientes funcionalidades:
- Información de trabajadores (no va en menú)
- Cargar y Mostrar Herramientas de Campo
- Registrar y Mostrar Parcelas de Cultivo
- Informe de Cosechas
## • Volver

Información de Trabajadores
El sistema debe tener predefinido el registro de al menos 5 trabajadores, cada uno con: cédula,
nombre completo y rol (Ej. Agrónomo, Supervisor, Operario).

Cargar y Mostrar Herramientas de Campo
El sistema debe permitir cargar un archivo con las herramientas usadas en las labores agrícolas
(tipo append). Por cada herramienta se debe indicar: código, nombre, descripción y tipo (manual,
motorizada, automatizada). La información se cargará desde un archivo, que el usuario indicará la
ruta, donde cada línea tendrá información de una herramienta y los datos estarán separados por
coma. Al finalizar se mostrarán todos los ítems del sistema. La unicidad estará dada por el código.
Ejemplo línea: HR001,Desmalezadora,Elimina maleza entre cultivos,motorizada

Al finalizar, se deben resaltar las nuevas herramientas registradas.

Registrar y Mostrar Parcelas de Cultivo
El usuario puede registrar una nueva parcela indicando: nombre, zona, área en metros cuadrados,
tipos de vegetal sembrado, precio por kilo por vegetal y herramientas asignadas (seleccionadas
de las existentes).
Se genera un identificador único de parcela.

Para consultar una parcela, se ingresa su código, y se muestra la información general y las
herramientas asociadas.

Informe de cosechas
Muestra la información de todas las cosechas registradas, con detalles como: identificador,
parcela, tipo de vegetal, fecha de recolección, cantidad (en kilogramos) y trabajador encargado.
Además, genera cinco estadísticas:
- Parcela con mayor volumen de cosecha.
- Top 3 de parcelas con mayor venta.
- Trabajador con más cosechas realizadas.
- Mes-Año con mayor recolección acumulada.
- Cosechas con subproducción y sobreproducción.




## Opciones Generales
Para acceder a estas funcionalidades el usuario deberá ingresar por medio del Menú Principal y se
deben habilitar las siguientes funcionalidades (Menú General):
o Gestión de cosechas
o Cierre de cosecha
o Consultar de cosecha
o Cancelación o modificación cosecha
o Consulta de disponibilidad de parcela
o Volver
Gestión de cosecha
Se permite al usuario registrar una cosecha para una parcela en un rango de fechas específico.
El usuario indica Identificador de trabajador, Identificador de parcela, Fecha inicio y fecha fin de
cosecha, tipo de vegetal y Cantidad por recolectar (kg).
El sistema debe comprobar que la parcela esté disponible en las fechas solicitadas antes de
confirmar la cosecha y que el tipo de vegetal sea permitido.
Se debe informar al usuario si fue posible o no la creación. Al generar la cosecha se indica el
identificador.

Cierre de cosecha
Mediante el código de una cosecha, el usuario indicará la cantidad de kilos recolectados en una
cosecha.

Consulta de cosecha
Mediante el código de una cosecha, el sistema mostrará toda la información correspondiente.

Cancelación de cosecha
El usuario puede realizar la anulación de una cosecha indicando el código de cosecha, el sistema
procederá a eliminar la cosecha del sistema, siempre y cuando existe el id indicado. La cosecha no
debe estar “cerrada”.

Modificación de cosecha
El usuario puede realizar la modificación de una cosecha, primero indicará el código de cosecha a
modificar, luego podrá realizar la modificación de al menos uno de los siguientes datos:
Parcela, fechas y tipo de vegetal.
El sistema debe comprobar que con los nuevos datos se pueda realizar la modificación, ver
validaciones de creación, antes de confirmar la modificación de la cosecha.
Se debe informar al usuario si fue posible o no la modificación. La cosecha no debe estar
## “cerrada”.




Consulta disponibilidad de parcela
El usuario dispone de dos opciones de consulta:
- El usuario indica un rango de fechas y el sistema indica las parcelas disponibles.
- El usuario indica un rango de fechas y el sistema muestra por cada día del rango el estado
de las parcelas (utilizada o disponible). Agrupado por parcela.

## Salir
Los datos del sistema no son persistentes, toda la información indicada anteriormente se libera, se
hace un manejo de estructuras en memoria. Al ser información volátil, se valorará la usabilidad
del sistema, libre de fallas.

## Puntos Extra
Se darán 2.5 puntos adicionales al entregar a más tardar el miércoles 23 de abril a las 11:55:55  PM
el Documento de Requerimientos, ver plantilla suministrada en el Tec Digital. Debe subirse en la
documentación llamada “Proyecto Programado II (archivos adicionales)” debajo de la carpeta de
“Proyectos”.

Se darán 7.5 puntos adicionales si se maneja persistencia de datos, es decir que al salir del Menú
Principal y volver a entrar al Menú Principal se mantengan los datos. La persistencia debe
realizarse por medio de un medio físico (archivo o base de datos). Con la carga de datos, si se
carga por segunda vez una información, se anexará lo nuevo y se mantendrá lo anterior, sin
permitir repetidos.
Aspectos técnicos
El proyecto deberá estar escrito en el lenguaje de programación Haskell. En caso de requerir
librerías adicionales para compilar y ejecutar el programa, deberán especificarlo en la
documentación, ya que de lo contrario se descontarán puntos en la evaluación.

Deberán utilizar el sistema de control de versiones GitHub, el repositorio deberá ser público o
incluir al profesor en el control de acceso de este.

Se valorará el aporte generado por cada estudiante, considerando, entre otras cosas, los commit
generados por cada uno. Por lo que el puntaje obtenido por cada uno de los estudiantes puede ser
diferenciado.
## Documentación
La documentación es un aspecto de gran importancia en el desarrollo de programas,
especialmente en tareas relacionadas con el mantenimiento de estos.

Para la documentación interna, deberán incluir comentarios descriptivos para cada función, con
sus entradas, salidas, restricciones y objetivo.

La documentación externa deberá incluir:
## 1. Portada.
- Manual de usuario: instrucciones de compilación, ejecución y uso.
- Pruebas de funcionalidad: incluir screenshots.
- Descripción del problema.
- Diseño del programa: decisiones de diseño, algoritmos usados.
- Librerías usadas: manejo entradas-salidas, archivos, etc.
- Análisis de resultados: objetivos alcanzados, objetivos no alcanzados, y razones por las
cuales no se alcanzaron los objetivos (en caso de haberlos).
- Bitácora (autogenerada en git, commit por usuario incluyendo comentario)
Forma de trabajo
El trabajo se debe realizar en parejas.
## Evaluación
La evaluación se va a centrar en dos elementos: programación y documentación.

El proyecto programado tiene un valor de 10% de la nota final, en el rubro de Proyectos.

Desglose de la evaluación del proyecto programado:
- Documentación interna 2 ptos.
- Documentación externa 8 ptos.
- Funcionalidad 80 ptos (ver detalle en Proyecto a Desarrollar)
- Revisión del proyecto (según completitud del proyecto y gestión del tiempo) 5 ptos.
- Hora de Entrega 5 ptos.

Aspectos administrativos
Debe crear un archivo .zip (“PP2_Integrante1_Integrante2.zip”) que contenga únicamente un
archivo info.txt y 2 carpetas llamadas documentacion y programa, en la primera deberá incluir el
documento de word o pdf solicitado y en la segunda los archivos y carpetas necesarias para la
implementación de este proyecto programado, y/o link en git del repositorio. El archivo info.txt
debe contener la siguiente información (cualidades):

a. Nombre del curso
b. Número de semestre y año lectivo
c. Nombre de los Estudiantes
d. Número de carnet de los estudiantes
e. Número de proyecto programado
f. Fecha de entrega
g. Estatus de la entrega (debe ser CONGRUENTE con la solución entregada):
[Deplorable|Regular|Buena|MuyBuena|Excelente|Superior]
## Entrega
Deberá subir el archivo antes mencionado al TEC Digital en el curso de LENGUAJES DE
PROGRAMACIÓN GR 60, en la asignación llamada “P2” debajo del rubro de “Proyectos”.  En la
evaluación del Proyecto el rubro de “Hora de Entrega” valdrá por 5 puntos de la nota total del
proyecto, según la siguiente escala:
a. Si se entrega antes de las 11:55:55 PM del miércoles 07 de mayo de 2025, 5 puntos.
b. Si se entrega antes de las 11:55:55 AM del jueves 08 de mayo de 2025 2.5 puntos.
c. Si se entrega antes de las 11:55:55 PM del jueves 08 de mayo de 2025, 0 puntos.
NO SE ACEPTARÁN trabajos que contengan “commits” posterior a esta fecha.

Todo el contenido de cada proyecto debe ser 100%  original y en caso de conducta fraudulenta se
procederá según el artículo 75 del RREA.
Todos los miembros del grupo deberán participar de la revisión, donde se demuestre la
funcionalidad y la autoría del proyecto.

