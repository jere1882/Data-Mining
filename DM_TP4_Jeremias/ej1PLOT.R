espErr  <- read.table("espErr")
diagErr <- read.table("diagErr")

errd  <- read.table("errd")
erre  <- read.table("erre")


# Generate the weak lerner complexity plots

pdf("espComplexity.pdf") 

espPoints <- cbind(1:20,rowMeans(espErr))
plot(espPoints,
     main = "Weak Lerner complexity vs Error",
     xlab = "Tree depth",
     ylab = "Test error",
     col  = "black",
     lwd = 1,
     ylim = c(0.35,0.44),
     cex = 0.7)
suavizado.datos <- smooth.spline(espPoints,spar=0.45)
lines(suavizado.datos$y,col="red",lwd=1)

dev.off()


pdf("diagComplexity.pdf") 
diagPoints <- cbind(1:20,rowMeans(diagErr))
plot(diagPoints,
     main = "Weak Lerner complexity vs Error",
     xlab = "Tree depth",
     ylab = "Test error",
     col  = "black",
     lwd = 1,
     ylim = c(0.11,0.14),
     cex = 0.7)
suavizado.datos <- smooth.spline(diagPoints,spar=0.45)
lines(suavizado.datos$y,col="blue",lwd=1)

dev.off()
# Generate de number of weak lerners plots

x <- c(10*(1:20),c(225, 250, 275, 300, 350))
yesp  <- rowMeans(erre)
ydiag <- c(rowMeans(errd),0.1295,0.1285,0.1355,0.1315,0.1265)

espPoints <- cbind(x,yesp)
diagPoints <- cbind(x,ydiag)
pdf("esperr.pdf") 
plot(espPoints,
     main = "Number of weak lerners vs Error",
     xlab = "Number of trees",
     ylab = "Test error",
     col  = "black",
     lwd = 1,
     ylim = c(0.35,0.44),
     cex = 0.7)
suavizado.datos <- smooth.spline(espPoints,spar=0.45)
lines(x,suavizado.datos$y,col="red",lwd=1)
dev.off()




pdf("diagerr.pdf") 
plot(diagPoints,
     main = "Number of weak lerners vs Error",
     xlab = "Number of trees",
     ylab = "Test error",
     col  = "black",
     lwd = 1,
     ylim = c(0.12,0.14),
     cex = 0.7)
suavizado.datos <- smooth.spline(diagPoints,spar=0.45)
lines(10*1:45,suavizado.datos$y,col="blue",lwd=1)
dev.off()




