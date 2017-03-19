# NIfTI
Uso de libreria(oro.nifti)
### Codigo

# number 1: preliminaries

library("bitops") # para operaciones Bitwise
library("XML") # para leer y crear archivos XML y HTML
library("splines") # para suavizado
library("oro.nifti") # Para procesar las imagenes nifti
options(prompt = "Gabbo> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)
options(niftiAuditTrail = TRUE)

###################################################
### cnumero 2: mniLR_nifti

fname <- system.file(file.path("nifti", "mniLR.nii.gz"), package="oro.nifti")
(mniLR <- readNIfTI(fname))
pixdim(mniLR)
descrip(mniLR)
aux.file(mniLR)
slotNames(mniLR)

###################################################
###number 3: mniLR-png

jpeg(filename="mniLR.jpeg", width=480, height=480, quality=95, bg="black")

###################################################
# number 4: mniLR-image
image(mniLR)

### number 5: mniLR-dev.off
dev.off()

###################################################
# number 6: mniRL-read
fname <- system.file(file.path("nifti", "mniRL.nii.gz"), package="oro.nifti")
(mniRL <- readNIfTI(fname))

###################################################
# number 7: mniRL-png
jpeg(filename="mniRL.jpeg", width=480, height=480, quality=95, bg="black")

###################################################
# number 8: mniRL-image
image(mniRL)

###################################################
# number 9: mniRL-dev.off
dev.off()

###################################################
# number 10: mniRL-ortho-png
jpeg(filename="mniRL_orthographic.jpeg", width=480, height=480, quality=95, bg="black")

###################################################
# number 11: mniRL-orthographic
orthographic(mniRL)

###################################################
# number 12: mniRL-ortho-dev.off
dev.off()

###################################################
# number 13: NIfTI-slots
slotNames(mniRL)
c(cal.min(mniRL), cal.max(mniRL))
range(mniRL)
mniRL@"datatype"
convert.datatype(mniRL@"datatype")

###################################################
# number 14: NIfTI-constructor
n <- 100
(random.image <- nifti(array(runif(n*n), c(n,n,1))))
random.image@"dim_"
dim(random.image)

###################################################
#number 15: NIfTI-write
writeNIfTI(random.image, "random")
list.files(pattern= "random")

###################################################
# number 16: niftiAuditTrail (eval = FALSE)
# options(niftiAuditTrail=TRUE)

###################################################
# number 17: NIfTI audit.trail 01
audit.trail(mniLR)


###################################################
### code chunk number 18: EBImage01 (eval = FALSE)
###################################################
## mniLR.range <- range(mniLR)
## EBImage::display((mniLR - min(mniLR)) / diff(mniLR.range))

###################################################
# number 19: ffd
filtered.func.data <- 
  system.file(file.path("nifti", "filtered_func_data.nii.gz"), 
              package="oro.nifti")
(ffd <- readNIfTI(filtered.func.data))


###################################################
# number 20: ffd-png
jpeg(filename="ffd.jpeg", width=480, height=480, quality=95, bg="black")

###################################################
# number 21: ffd-image
image(ffd, zlim=range(ffd)*0.95) 

###################################################
# number 22: ffd-dev.off
dev.off()

###################################################
# number 23: ffd-ortho-png
jpeg(filename="ffd_orthographic.jpeg", width=480, height=480, quality=95, bg="black")

###################################################
# number 24: ffd-orthographic
orthographic(ffd, xyz=c(34,29,10), zlim=range(ffd)*0.9)

###################################################
# number 25: ffd-ortho-dev.off
dev.off()


###################################################
# number 26: ffd-glm-design
visual <- rep(c(-0.5,0.5), each=30, times=9)
auditory <- rep(c(-0.5,0.5), each=45, times=6)
hrf <- c(dgamma(1:15, 4, scale=1.5))
hrf0 <- c(hrf, rep(0, length(visual)-length(hrf)))
visual.hrf <- convolve(hrf0, visual)
hrf0 <- c(hrf, rep(0, length(auditory)-length(hrf)))
auditory.hrf <- convolve(hrf0, auditory)
index <- seq(3, 540, by=3)
visual.hrf <- visual.hrf[index]
auditory.hrf <- auditory.hrf[index]


###################################################
# number 27: ffd-design.png
jpeg("ffd_design.jpeg", width=3*480, height=1.5*480, quality=95)
par(mfrow=c(1,2), mar=c(5,4,4,2) + 1, mex=0.85, 
    cex=1.5, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
plot(index, visual.hrf, type="l", lwd=2,
     xlab="Acquisition Index", ylab="Visual Stimulus")
plot(index, auditory.hrf, type="l", lwd=2,
     xlab="Acquisition Index", ylab="Auditory Stimulus") 
dev.off()

###################################################
# number 28: ffd-glm
# reduced length due to R package storage limitations
visual.hrf <-visual.hrf[1:64]
auditory.hrf <-auditory.hrf[1:64]
## background threshold: 10% max intensity
voxel.lsfit <- function(x, thresh) { # general linear model
  ## check against background threshold
  if (max(x) < thresh) {
    return(rep(NA, 5))
  }
  ## glm
  output <- lsfit(cbind(visual.hrf, auditory.hrf), x)
  ## extract t-statistic, p-values
  output.t <- ls.print(output, print.it=FALSE)$coef.table[[1]][2:3,3:4]
  output.f <- ls.print(output, print.it=FALSE)$summary[3]
  c(output.t, as.numeric(output.f))
}

## apply local glm to each voxel
ffd.glm <- apply(ffd, 1:3, voxel.lsfit, thresh=0.1 * max(ffd))

###################################################
# number 29: zstat1
dof <- ntim(ffd) - 1
Z.visual <- nifti(qnorm(pt(ffd.glm[1,,,], dof, log.p=TRUE), log.p=TRUE),
                  datatype=16)
Z.auditory <- nifti(qnorm(pt(ffd.glm[2,,,], dof, log.p=TRUE), log.p=TRUE),
                    datatype=16)

###################################################
# number 30: zstat1-png
jpeg("ffd_zstat1.jpeg", width=480, height=480, quality=95, bg="black")

###################################################
# number 31: zstat1-overlay
yrange <- c(5, max(Z.visual, na.rm=TRUE))
overlay(ffd, ifelse(Z.visual > 5, Z.visual, NA), 
        zlim.x=range(ffd)*0.95, zlim.y=yrange)

###################################################
#number 32: zstat1-dev.off
dev.off()

###################################################
# number 33: zstat2-png
jpeg("ffd_zstat2.jpeg", width=480, height=480, quality=95, bg="black")

###################################################
# number 34: zstat2-overlay
yrange <- c(5, max(Z.auditory, na.rm=TRUE))
overlay(ffd, ifelse(Z.auditory > 5, Z.auditory, NA), 
        zlim.x=range(ffd)*0.95, zlim.y=yrange)

###################################################
# number 35: zstat2-dev.off
dev.off()


#cargar archivos nifti
getwd()
nim <- "~/Desktop"
nimdir <- file.path(nim , "R", "Curso_R_psicologia")
funcional_2 <- file.path(nimdir, "filtered_func_data")
(funcional_imagen <- readNIfTI(funcional_2, reorient = FALSE))
image(funcional_imagen)
orthographic(funcional_imagen, col = 'orange')

# para guardar el archivo de R en la carpeta de tabajo 
writeNIfTI(funcional_imagen, "funcional_final")
