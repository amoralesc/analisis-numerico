################################################################################
# HERMITE DIFERENCIAS DIVIDIDAS
################################################################################
# Con puntos
################################################################################

difDiv <- function(f1, f0, z1, z0) {
	return ((f1-f0)/(z1-z0))
}

hermitePuntos <- function(x, y, d) {
	A <- matrix(nrow=length(x)*4-1,ncol=length(x)*2+1)
	rows = length(x)*4-1
	cols = length(x)*2+1
	cont = cols
	spacex = -1
	for (i in 1:cols) {
		rowx = 1
		times = 0
		primera = FALSE
		rowA = -1
		for (j in 1:rows) {
			if (i == 1) {			# Columna de las z
				if (j %% 2 != 0) {
					A[j,i] = x[rowx]
					times = times + 1
				}
				if (times == 2) {
					rowx = rowx + 1
					times = 0
				}
			} else if (i == 2) {	# Columna de las f[z]
				if (j %% 2 != 0) {
					A[j,i] <- y[rowx]
					times = times + 1
				}
				if (times == 2) {
					rowx = rowx + 1
					times = 0
				}
			} else if (i == 3) {	# Columna de la primera diferencia dividida
				if (j %% 2 == 0) {
					if (j %% 4 != 0) {
						A[j,i] = d[rowx]
						rowx = rowx + 1
					} else {
						A[j,i] <- difDiv(A[j+1,2], A[j-1,2], A[j+1,1], A[j-1,1])
					}
				}
			} else {				# Subsecuentes columnas de diferencias divididas
				if (j %% (i-1) == 0 && primera == FALSE) {
					A[j,i] = difDiv(A[j+1,i-1], A[j-1,i-1], A[j+(i-2),1], A[j-(i-2),1])
					primera = TRUE
					rowA = j+2
					times = times + 1
				} else {
					if (j == rowA && times != cont) {
						A[j,i] = difDiv(A[j+1,i-1], A[j-1,i-1], A[j+(i-2),1], A[j-(i-2),1])
						rowA = j+2
						times = times + 1
					}
				}
			}
		}
		cont = cont - 1
	}
	return (A)
}

x <- c(1, 2)
y <- c(0, 0.69)
d <- c(1, 0.5)

A <- hermitePuntos(x, y, d)
print(A)

################################################################################
# Con una función derivable en un intervalo
################################################################################

hermiteIntervalo <- function(f, a, b, n, redondeo = -1) {
	if (n < 2) {
		return (NULL);
	}
	
	Fx <- function(x) { eval(f[[1]]) }
	
	# Calcular los puntos
	# x
	x <- c()
	if (redondeo == -1) {
		d <- ((b - a) / (n-1))
	} else {
		d <- round(((b - a) / (n-1)), redondeo)
	}
	
	ultimo <- a - d
	for (i in 1:n) {
		actual <- ultimo + d
		x <- append(x, ultimo + d)
		ultimo <- actual
	}
	
	# y
	y <- c()
	for (i in x) {
		if (redondeo == -1) {
			y <- append(y, Fx(i))
		} else {
			y <- append(y, round(Fx(i), redondeo))
		}
	}
	
	# d
	dx <- D(f, "x")
	Dx <- function(x) { eval(dx) }
	
	d <- c()
	for (i in x) {
		if (redondeo == -1) {
			d <- append(d, Dx(i))
		} else {
			d <- append(d, round(Dx(i), redondeo))
		}
	}
	
	print(x)
	print(y)
	print(d)
	# Hacer hermite para los puntos calculados
	return(hermitePuntos(x, y, d))
}

f <- expression(log(x))
a <- 1
b <- 2
n <- 4
r <- 2
B <- hermiteIntervalo(f, a, b, n)
print(B)
