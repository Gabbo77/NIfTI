# NIfTI
Uso de libreria(oro.nifti)

### Codigo

#### numero 1: Preliminaries
Para poder aplicar las funciones necesarias, se necesita instalar y cargar las siguientes librerias. 

library("bitops") 

library("XML") 

library("splines") 

library("oro.nifti") 

Podermos cambiar como el se muestra el prompt  en nuestra Consola. Para ello utilizamos "options"
esta opcion modifica la forma en la que R realiza los calculos y los muestra en la consola. 
En el caso del prompt el argumento que coloquemos debe de finalizar con un espacio. 

options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

options(niftiAuditTrail = TRUE)

#### numero 2: mniLR_nifti

Primero vamos a cargar un archivo que ya se encuentra en dentro de la paqueteria oro.nifti. 
Para esto utilizamos declaramos el nombre que tendra nuestro archivo y utilizamos las funciones "system.file" para ubicar 
nuestro archivo dentro de nuestro sistema y en la paqueteria (recordar que solo se debe declarar una sola paqueteria sino arrojara un error)
y con "file.path" la ruta donde se encentra nuestro archivo almacenado, indicando la carpeta en la que se ubica y el nombre del archivo.

fname <- system.file(file.path("nifti", "mniLR.nii.gz"), package="oro.nifti")

Ahora leemos el archivo NIfTI que hemos declarado previamente con la funcion "readNIfTI". 
El uso de los  parentesis permite que se muestre el resultado de la operacion realizada en R.

(mniLR <- readNIfTI(fname))

Podemos ver nuestra imagen de la sigueinte manera:

image(mniLR)

Ahora podemos observar algunas características de nuestro archivo NIfTI. Si realizamos un "str(mniLR)" observaremos todas los slots (etiquetas o extensiones en el header) y su respectivos valores, de nuestro archivo, 
de la misma manera podemos ver los slots con la función "slotNames(archivo)".

slotNames(mniLR)

De forma individual, para saber que valor tiene cada slots y de que clase es, podemos poner el nombre del slot y en paréntesis el nombre de nuestro objeto con el cual se declaro la imgen que cargamos. 

pixdim(mniLR)

descrip(mniLR)

aux.file(mniLR)

### numero 3: mniLR-png

Para trasformar y guardar nuestra imagen en formato JPEG utilizamos la funcion homonima "jpeg", para ello declaramos en nombre de la imegen de salida, el tamaño de la misma, la calidad de la imagen y el fondo que tendra dicha imagen. 

jpeg(filename="mniLR.jpeg", width=480, height=480, quality=95, bg="black")

#### numero 4: mniLR-image

Con la funcion "image" podemos proyectar la imagen de nuestro archivo NIfTI de forma "ligthbox vie" como se vería en un visualizador como FSLview y que sera la forma en la cual se guardará nuestra imagen.

image(mniLR)

#### numero 5: mniLR-dev.off

Finalmente, con "dev.off()" finalizamos la trasformacion y el almacenamiento de nuestra imagen en JPEG

dev.off()

#### numero 6: mniRL-ortho-png

Tambien podemos guardar nuestros archivos en de forma ortogonal. Realizamos los siguientes pasos que en el caso anterior

jpeg(filename="mniRL_orthographic.jpeg", width=480, height=480, quality=95, bg="black")

#### numero 7: mniRL-orthographic

Aqui utilizamos la funcion "orthographic", que permite visualizar la imagen de forma ortogonalizada, es decir muestra un corte axial, uno sagital y uno coronal.

orthographic(mniRL)

#### numero 8: mniRL-ortho-dev.off

dev.off()

#### numero 9: NIfTI-slots

Los slots de nuestro header los podemos manipular, agrupandolos, tranformandolos, obteniendo intervalos o simeplmente para saber su valor.

c(cal.min(mniRL), cal.max(mniRL))

range(mniRL)

mniRL@"datatype"

convert.datatype(mniRL@"datatype")

#### numero 10: NIfTI-constructor
Con el uso de esta libreria poemos crear obejtos NIfTI con la función homonima "nifti", incluyendo un header a partir de la generacion de un array.

n <- 100

(random.image <- nifti(array(runif(n * n), c(n,n,1))))

random.image@"dim_"

dim(random.image)

#### numero 11: NIfTI-write

Mientras que con la función "writeNIfTI" podemos guardar nuestro objeto en formato NIfTI. Y que permite que ya sea visualizado en alguno de los visualizadores convencionales como FSLview.
Los archivos pueden ser almacenado compresos o no (gnuzip)

writeNIfTI(random.image, "random")

list.files(pattern= "random")

#### number 12: niftiAuditTrail (eval = FALSE)
Podemos incrementar el formato del encabezado de nuestros datos NIfTI. Es util para:
Primero, las extensiones de nuestro formato pueden ser apropiadamente manipuladas para leer como para escribir archivos NIfTi.
Segundo, se permite a los usuarios agregar extensiones a los objetos NIfTI recién creados considerandolos como objetos "niftiExtension" y agregando objetos "niftiExtensionSection" al slot "extensions".
Tercero, todas las operaciones que se realicen en un objeto NIfTI generarán lo que se denomina como "AuditTrail" que consiste en un registro de acciones basado en formato XML, que contendra no solo información de las operaciones realizadas sino tambien de infromación a respecto al sistema y las paqueterías.
Por lo tanto podemos controlar el seguimiento en la manipulacion de los objetos NIfTI

options(niftiAuditTrail=TRUE)

#### number 13: NIfTI audit.trail 01

De la siguiente manera podemos observar esa trazabilidad en nuestro obejtos y que se almacena en el header el mismo, en el partado de 2extensions"

audit.trail(mniLR)

#### numero 14: EBImage01 (eval = FALSE)

Podemos realizar una visualizacion interactiva de nuestro objeto utilizando la paquetería "EBImage" (No disponible para versiones de R 3.3.3)
Permite visualización y animación y demanda el declarar escala de grises [0, 1]. 

mniLR.range <- range(mniLR)
EBImage::display((mniLR - min(mniLR)) / diff(mniLR.range))

#### numero 15: ffd
A continuación se realiza la visualización de una imagen 4D, de fMRI. Obtenida de FSL (Analysis Group, FMRIB, Oxford 2008) y que se encuentra en nuestra paqueteria oro.nifti
Por lo tanto cargamos nuestro archivo como se realizo previamente con la imagen 3D

filtered.func.data <- system.file(file.path("nifti", "filtered_func_data.nii.gz"), package="oro.nifti")

(ffd <- readNIfTI(filtered.func.data))

#### numero 16: ffd-png
La trafromamos y guardamos en formato JPEG en las dos versiones ligthbox u ortogonal

jpeg(filename="ffd.jpeg", width=480, height=480, quality=95, bg="black")

image(ffd, zlim=range(ffd) * 0.95) 

dev.off()

#### numero 17: ffd-ortho-png

jpeg(filename="ffd_orthographic.jpeg", width=480, height=480, quality=95, bg="black")

orthographic(ffd, xyz=c(34,29,10), zlim=range(ffd) * 0.9)

dev.off()

# numero 18: ffd-glm-design
El entorno de programación en R ofrece una amplia variedad de metodología estadística para el análisis
cuantitativo de los datos. Por ejemplo, los datos fMRI se analizan típicamente Aplicando un modelo de regresión lineal múltiple, 
conocido como un modelo lineal general (GLM), el cual se utiliza nuestro modelo experimantal basado en estimulo para contruir la matriz de diseño.
La estimación de los coeficientes de regresión en el GLM produce una imagen estadística en valores z-score 
para una prueba de hipótesis voxelwise en relacion a la activación que se presenta durante el experimento de fMRI.

Para este caso la imagen "filtered_func_data.nii.gz" se ubtuvo de un experimento con un valor de TR=3, en el cual se utilizó una tarea visual y una auditiva.
El estimulo auditivo fue aplicado con un patrón on/off con una duración de 60 s.
El estimulo visual fue aplicado con un patrón on/off con una duración de 90 s.
La funcion de respuesta hemodinamica (HRF) parametrica consistio de una media = 6 y una desviación estándar = 3, 

Para ello se contruye el diseño para cada una de las tareas.

visual <- rep(c(-0.5,0.5), each=30, times=9)

auditory <- rep(c(-0.5,0.5), each=45, times=6)

Se realiza el diseño de la respuesta hemodinamica para cada una de las condiciones . Utilizando una distribucion gamma

hrf <- c(dgamma(1:15, 4, scale=1.5))

hrf0 <- c(hrf, rep(0, length(visual)-length(hrf)))

hrf0 <- c(hrf, rep(0, length(auditory)-length(hrf)))


Se realiza la convulucion del diseño de la tarea y al respuesta hemodinamica  para cada una de las condiciones utilizamos la funcion "convolve" la cual realiza la convolucion aplicando una FFT

visual.hrf <- convolve(hrf0, visual)

auditory.hrf <- convolve(hrf0, auditory)

Finalmente se realizo un submuestreo de la señal de cada unas de las condiciones, por un factor de 3 para obtener las columnas del diseño de la matriz y que coinciden con la adquisición de los datos de MRI. 

index <- seq(3, 540, by=3)

visual.hrf <- visual.hrf[index]

auditory.hrf <- auditory.hrf[index]


#### numero 19: ffd-design.png

Acontinuacion de trasforman y guardan las convoluciones de ambas condiciones  en formato JPEG.

jpeg("ffd_design.jpeg", width=3 * 480, height=1.5 * 480, quality=95)

par(mfrow=c(1,2), mar=c(5,4,4,2) + 1, mex=0.85, cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)

plot(index, visual.hrf, type="l", lwd=2, xlab="Acquisition Index", ylab="Visual Stimulus")

plot(index, auditory.hrf, type="l", lwd=2, xlab="Acquisition Index", ylab="Auditory Stimulus") 

dev.off()



#### numero 20: ffd-glm

## Reducir tamaño

Se suguiere reducir el tamaño a la mitad de las matrices por ue el almacenamiento de los datos es limitado. 

lenght(visual.hrf)

lenght(auditory.hrf )

visual.hrf <-visual.hrf[1:64]

auditory.hrf <-auditory.hrf[1:64]

## Umbralizado: 10% max de intensidad 

voxel.lsfit <- function(x, thresh) { # general linear model

## check against background threshold

  if (max(x) < thresh) {
    return(rep(NA, 5))
  }

## Glm
La funcion lsfit estima los parámetros en la regresión lineal.

  output <- lsfit(cbind(visual.hrf, auditory.hrf), x)

## extraer los valores 
Se extraen los valores t-statistic, p-values para cada voxel (voxelwise). Se calculan sus valores p asociados de acuerdo a la prueba de hipótesis de ausencia de efecto para cada estímulo individual, 
junto con una estadística F para la prueba de hipótesis de ningún efecto de cualquier estímulo usando la función ls.print.

  output.t <- ls.print(output, print.it=FALSE)$coef.table[[1]][2:3,3:4]
  
  output.f <- ls.print(output, print.it=FALSE)$summary[3]
  
  c(output.t, as.numeric(output.f))
}

Finalmente, se aplica el GLM a cada voxel. 

ffd.glm <- apply(ffd, 1:3, voxel.lsfit, thresh=0.1 * max(ffd))

#### numero 21: zstat1

Los valores t se trasforman a valores z en cada una de las condiciones. Se extrae el valor maximo de las dimensiones del objeto NIfTI usando al función "ntim".
Se crean las imagenes nifti de los valore z de cada condicion. 

dof <- ntim(ffd) - 1

Z.visual <- nifti(qnorm(pt(ffd.glm[1,,,], dof, log.p=TRUE), log.p=TRUE), datatype=16)

Z.auditory <- nifti(qnorm(pt(ffd.glm[2,,,], dof, log.p=TRUE), log.p=TRUE), datatype=16)

#### numero 22: zstat1-png
Se guardan las imagenes en formato JPEG

jpeg("ffd_zstat1.jpeg", width=480, height=480, quality=95, bg="black")

#### number 23: zstat1-overlay
#### Visual
Se sobrelapa en la imagen original el mapa z-score de cada condición. 

yrange <- c(5, max(Z.visual, na.rm=TRUE))

overlay(ffd, ifelse(Z.visual > 5, Z.visual, NA), zlim.x=range(ffd) * 0.95, zlim.y = yrange)

dev.off()

#### Auditiva

jpeg("ffd_zstat2.jpeg", width=480, height=480, quality=95, bg="black")

yrange <- c(5, max(Z.auditory, na.rm=TRUE))

overlay(ffd, ifelse(Z.auditory > 5, Z.auditory, NA), zlim.x=range(ffd)*0.95, zlim.y=yrange)

dev.off()


### cargar archivos nifti propios de tu carpeta de trabajo 
Primero saber donde estamos trabajando y se suguiere que el archivo NIfTI que vayamos a leer se encuentre en la misma carpeta y sino es asi obtener la ruta donde se encuentra dicho archivo.

getwd()

nim <- "~/Desktop"

nimdir <- file.path(nim , "R", "Curso_R_psicologia")

funcional_2 <- file.path(nimdir, "filtered_func_data")

(funcional_imagen <- readNIfTI(funcional_2, reorient = FALSE))

image(funcional_imagen)

orthographic(funcional_imagen, col = 'orange')

# para guardar el archivo de R en la carpeta de tabajo 
Una vez cargada nuestra imagen la podemos manipular para realizar diferentes operaciones, y finalmente se guarda de la forma convencional utilizando "writeNIfTI", asignandole el nombre del archivo deseado. 
writeNIfTI(funcional_imagen, "funcional_final")
