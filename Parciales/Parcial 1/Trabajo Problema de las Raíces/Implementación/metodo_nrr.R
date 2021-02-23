# Remueve todos los objetos creados
rm(list=ls())

library("Rmpfr")

obtenerPrecision <- function(tol) {
	n = log(tol, 10) * -1
	return (n)
}

fmt <- function(n, precision) {
	return (formatC(n, format = "e", digits=precision))
}

mpfrStr <- function(n) {
	return (sub("\'mpfr1\' ", "", capture.output(n@.Data[[1]])))
}

newtonRelajado <- function(f, x, tol, max_i = 1000) {
	cat("-------------------------------------------------------------\n")
	cat("F(x)= ", deparse(f), "\ttol= ", tol, "\tx0= ", x, "\n")
	
	# Obtener F(x) y F'(x)
	d <- D(f, "x")
	Fx <- function(x) { eval(f[[1]]) }
	Dx <- function(x) { eval(d) }
	
	prec <- obtenerPrecision(tol)
	
	# Obtener F'(x0)
	x0 = x
	Dx0 <-  Dx(x0)
	
	if (Fx(x) == 0) {
		return(c(x, 0))
	} else if (Dx0 == 0) {
		print("Error: F'(x0) == 0")
		break
	}
	
	i <- 0
	error <- 1
	
	while (error >= tol) {
		cat("i=", i, "\tx=", fmt(x, prec), "\te=", fmt(error, prec), "\n")
		
		x1 <- x - (Fx(x) / Dx0)
		error <- abs(x1 - x)
		
		x <- x1
		i <- i + 1
		
		if (Fx(x) == 0) {
			break
		} else if (i == max_i) {
			print("Maxima iteracion alcanzada")
			break
		}
	}
	cat("i=", i, "\tx=", fmt(x, prec), "\te=", fmt(error, prec), "\n")	# Última x
	return (c(x, i))
}

newtonRelajadoAltaPrecision <- function(f, x, tol, bits, max_i = 10000) {
	cat("-------------------------------------------------------------\n")
	cat("F(x)= ", deparse(f), "\ttol= ", tol, "\tx0= ", x, "\n")
	
	# Obtener F(x) y F'(x)
	d <- D(f, "x")
	Fx <- function(x) { eval(f[[1]]) }
	Dx <- function(x) { eval(d) }
	
	# Obtener F'(x0)
	x0 <- x
	Dx0 <-  Dx(x0)
	
	if (Dx0 == 0) {
		print("Error: F'(x0) == 0")
		break
	}
	
	i <- 0
	error <- mpfr(1, bits)
	
	while (error >= mpfr(tol, bits)) {
		cat("i=", i, "\tx=", mpfrStr(x), "\te=", mpfrStr(error), "\n")
		
		x1 <- mpfr(x, bits) - mpfr(Fx(x), bits) / mpfr(Dx0, bits)
		error <- mpfr(abs(mpfr(x1, bits) - mpfr(x, bits)), bits)
		
		x <- mpfr(x1, bits)
		i <- i + 1
		
		if (Fx(x) == 0) {
			break
		} else if (max_i == i) {
			print("Maxima iteracion alcanzada")
			break
		}
	}
	cat("i=", i, "\tx=", mpfrStr(x), "\te=", mpfrStr(error), "\n")	# Última x
	return (c(x, i))
}

newtonRelajadoGrafico <- function(f, x, tol, a, b, max_i = 1000) {
	cat("-------------------------------------------------------------\n")
    cat("f= ", deparse(f), "\tx= ", x, "\ttol= ", tol, "\n")
	
	tiempo = 0.25	# Tiempo de impresión (Step-by-Step)
	
	# Get F(x) and F'(x)
	d <- D(f, "x")
	Fx <- function(x) { eval(f[[1]]) }
	Dx <- function(x) { eval(d) }
    
    prec <- obtenerPrecision(tol)
    
    # Plot the curve
	curve(Fx, a, b, n=100)
	abline(0, 0, col="black")
	abline(v = 0, col="black")
	Sys.sleep(tiempo)
	
	# Get F'(x0)
	x0 <- x
	Dx0 <-  Dx(x0)
	
	if (Dx0 == 0) {
		print("Error: F'(x0) == 0")
		break
	}
	
	i <- 0
	error <- 1
	
	while (error >= tol) {
		Sys.sleep(tiempo)
		
		x1 <- x - (Fx(x) / Dx0)
		error <- abs(x1 - x)
		
		cat("i=", i, "\tx=", fmt(x, prec), "\te=", fmt(error, prec), "\n")
		
		points(x, 0, col="blue")
		segments(x, 0, x, Fx(x))
		segments(x, Fx(x), x1, 0, col="red")
		
		x <- x1
		i <- i + 1
		
		if(Fx(x) == 0) {
			break
		} else if (i == max_i) {
			print("Maxima iteracion alcanzada")
			break
		}
    }
	cat("i=", i, "\tx=", fmt(x, prec), "\te=", fmt(error, prec), "\n")
	return (c(x, i))
}

##################################################################################
# INPUT: No tocar nada
##################################################################################

f <- expression(cos(x)^2 - x^2)
f_x0 <- 1

g <- expression(x*sin(x) - 1)
g_x0 <- 1

h <- expression(x^3 - 2*x^2 + (4/3)*x - (8/27))
h_x0 <- 0.6

i <- expression((668.061/x) * (1 - exp(-0.146843 * x)) - 40)
i_x0 <- 10

j <- expression(x^3 - 2*x - 5)
j_x0 <- 2

functions = c(f, g, h, i, j)
points_x0 = c(f_x0, g_x0, i_x0, j_x0)
tols = c(10^-8, 10^-16, 10^-32, 10^-56)

##################################################################################
# PROCESOS: Hacer los llamados a las funciones a su gusto
##################################################################################

##################################################################################
# 1. NEWTON RELAJADO 
# No se recomienda utilizar con tols[3], tols[4]
# Se recomienda establecer una máxima iteracion (max_i) con la función h

#newtonRelajado(f, f_x0, tols[1])

##################################################################################
# 2. NEWTON RELAJADO - ALTA PRECISION 
# Utilizar con cualquier tols[i]
# Se recomienda establecer una máxima iteracion (max_i) con la función h
# Se recomienda establecer bits = 180 para tols[4]

# newtonRelajadoAltaPrecision(g, g_x0, tols[4], 180)

##################################################################################
# 3. NEWTON RELAJADO - GRAFICO 
# No se recomienda utilizar con una tol > 10^-5

# newtonRelajadoGrafico(expression(x^2 - 1), 4, 10^-5, 0, 4)
