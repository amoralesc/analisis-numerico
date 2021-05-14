################################################################################
# INTERPOLACIÓN MEDIANTE SPLINES
################################################################################

library("stats")
library("graphics")
rm(list=ls())

# DATOS INICIALES
x=c(6.7,6.0,5.0,4.3,3.5,2.8,2.3,1.7,1.8,2.5,3.0,3.6,4.2,4.5,5.2,5.6,5.3,5.2,5.1,
	4.9,4.7,4.8,5.2,5.6,6.0,6.3,7.2,7.3,7.4,7.3,7.8,8.4,8.8,8.9,9.0,9.1,9.2,9.6,
	10.1,10.8,11.5,11.7,11.4,10.7,11.4,11.8,12.6,13.0,13.6,13.9,14.0,13.7,13.2,
	12.9,12.8,12.7,12.5,12.2,12.1,12.0,11.9,11.8,11.6,11.5)
y=c(3.0,3.6,4.9,6.3,8.0,8.6,9.7,10.7,11.3,11.5,11.2,10.7,10.0,9.6,8.6,9.6,11.0,
	12.2,13.3,14.0,15.2,16.2,16.6,16.5,15.5,14.0,11.7,13.8,14.7,16.0,17.4,17.6,
	17.3,16.3,15.4,14.0,12.3,14.5,16.1,16.9,16.5,15.7,14.4,11.2,12.0,12.4,13.5,
	14.2,14.4,14.1,13.7,13.0,12.2,11.6,11.2,10.9,10.5,9.5,8.3,7.6,6.7,6.0,5.0,4.5)
particiones = c(1, 4, 8, 9, 14, 16, 19, 20, 21, 22, 24, 27, 29, 30, 37, 41, 42, 44, 51, 53, 58, 64)

plot.new()
plot(NULL, xlim=c(0,15), ylim=c(0,20), main="Interpolación Splines Reducida", 
	 xlab="x", ylab="y")

graficarPuntosNumeros <- function(x, y, color="red") {
	for (i in 1:length(x)) {
		text(x[i], y[i], label=i, col=color, cex=0.8, add=TRUE)
	}
}

graficarPuntos <- function(x, y, color="red") {
	for (i in 1:length(x)) {
		points(x[i], y[i], pch=20, col=color, cex=1, add=TRUE)
	}
}

# Graficar los puntos con números, para conocer a cuáles se refieren
# graficarPuntosNumeros(x,y)

splines <- function(x, y, particiones, color="red") {
	for (i in 1:(length(particiones)-1)) {
		desde <- particiones[i]
		hasta <- particiones[i+1]
		funcion_spline <- splinefun(x[desde:hasta], y[desde:hasta], method="monoH.FC")
		curve(funcion_spline(x), add=TRUE, from=x[desde], to=x[hasta], col=color)
	}
}

graficarPuntos(x, y)
splines(x, y, particiones)

###############################################################################
# REDUCCIÓN DE PUNTOS
###############################################################################

eliminarPunto <- function(i, datos) {
	x <- datos[[1]]
	y <- datos[[2]]
	p <- datos[[3]]
	indice <- 0
	for (j in 1:length(p)) {
		if (p[j] > i) {
			indice <- j
			break
		}
	}
	x <- x[-i]
	y <- y[-i]
	for (j in indice:length(p)) {
		p[j] = p[j] - 1
	}
	return(list(x, y, p))
}

datos <- list(x, y, particiones)
datos <- eliminarPunto(2, datos)
datos <- eliminarPunto(3, datos)
datos <- eliminarPunto(10, datos)
datos <- eliminarPunto(10, datos)
datos <- eliminarPunto(14, datos)
datos[[3]] <- datos[[3]][-8]
datos <- eliminarPunto(15, datos)
datos <- eliminarPunto(22, datos)
datos <- eliminarPunto(28, datos)
datos <- eliminarPunto(28, datos)
datos <- eliminarPunto(29, datos)
datos <- eliminarPunto(33, datos)
datos <- eliminarPunto(34, datos)
datos <- eliminarPunto(34, datos)
datos <- eliminarPunto(39, datos)
datos[[3]][19] <- 40
datos <- eliminarPunto(39, datos)
datos <- eliminarPunto(41, datos)
datos <- eliminarPunto(41, datos)
datos <- eliminarPunto(42, datos)
datos <- eliminarPunto(43, datos)
datos <- eliminarPunto(44, datos)
datos <- eliminarPunto(43, datos)

print(datos)
x_reduccion <- datos[[1]]
y_reduccion <- datos[[2]]
p_reduccion <- datos[[3]]

graficarPuntos(datos[[1]], datos[[2]], "blue")
splines(datos[[1]], datos[[2]], datos[[3]], "blue")

###############################################################################
# CALCULAR ERROR
###############################################################################

splinesError <- function(x, y, particiones, frac = 0.01) {
	x_medida <- c()
	y_medida <- c()
	for (i in 1:(length(particiones)-1)) {
		desde <- particiones[i]
		hasta <- particiones[i+1]
		funcion_spline <- splinefun(x[desde:hasta], y[desde:hasta], method="monoH.FC")
		if (x[hasta] > x[desde]) {
			medida <- seq(x[desde], x[hasta], frac)
		} else {
			medida <- seq(x[desde], x[hasta], frac * -1)
		}
		x_medida <- c(x_medida, medida)
		y_medida <- c(y_medida, funcion_spline(medida))
	}
	return (list(x_medida, y_medida))
}

#medidas_inicial <- splinesError(x, y, particiones, 0.000001)
#medidas_final <- splinesError(datos[[1]], datos[[2]], datos[[3]], 0.000001)

calcularError <- function(m1, m2) {
	y_error <- c()
	for (i in 1:length(m1)) {
		for (j in 1:length(m2)) {
			if (m1[[1]][i] == m2[[1]][j]) {
				y_error <- c(y_error, abs(m1[[2]][i] - m2[[2]][j]))
				break	
			}
		}
	}
	return(y_error)
}

#y_error <- calcularError(medidas_inicial, medidas_final)

#summary(y_error)
# error medio: < 1.3x10^-7
