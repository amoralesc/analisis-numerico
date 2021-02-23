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

aitken <- function(g, p0, tol) {
	cat("-------------------------------------------------------------\n")
	cat("G(x)= ", deparse(g), "\ttol= ", tol, "\tp0= ", p0, "\n")
	
	# Obtener F(x)
	Gx <- function(x) { eval(g[[1]]) }
	
	prec <- obtenerPrecision(tol)
	
	i <- 0
	error <- 1
	p <- p0
	
	while (error >= tol) {
		cat("i=", i, "\tx=", fmt(p, prec), "\te=", fmt(error, prec), "\n")
		
		p1 <- Gx(p0)
		p2 <- Gx(p1)
		
		p <- p0 - ((p1 - p0)^2) / (p2 - 2*p1 + p0)
		error <- abs(p - p0)
		
		i <- i + 1
		p0 <- p
		
		if (is.nan(p)) {
			return (0)
		} else if (p == Inf || p == -Inf) {
			return (0)
		} else if (Gx(p) == 0) {
			break
		}
	}
	cat("i=", i, "\tx=", fmt(p, prec), "\te=", fmt(error, prec), "\n")
	return (p)
}

aitkenAltaPrecision <- function(g, p0, tol, bits) {
	cat("-------------------------------------------------------------\n")
	cat("G(x)= ", deparse(g), "\ttol= ", tol, "\tp0= ", p0, "\n")
	
	# Obtener F(x)
	Gx <- function(x) { eval(g[[1]]) }
	
	i <- 0
	error <- mpfr(1, bits)
	p <- mpfr(p0, bits)
	
	while (error >= tol) {
		cat("i=", i, "\tx=", mpfrStr(p), "\te=", mpfrStr(error), "\n")
		
		p1 <- mpfr(Gx(p0), bits)
		p2 <- mpfr(Gx(p1), bits)
		
		p <- mpfr(p0, bits) - ((mpfr(p1, bits) - mpfr(p0, bits))^2) / (mpfr(p2, bits) - 2*mpfr(p1, bits) + mpfr(p0, bits))
		error <- abs(mpfr(p, bits) - mpfr(p0, bits))
		
		i <- i + 1
		p0 <- p
		
		if (is.nan(p)) {
			return (0)
		} else if (p == Inf || p == -Inf) {
			return (0)
		} else if (Gx(p) == 0) {
			break
		}
	}
	cat("i=", i, "\tx=", mpfrStr(p), "\te=", mpfrStr(error), "\n")
	return (p)
}

##################################################################################
# INPUT: No tocar nada
##################################################################################

f <- expression(cos(x)^2 - x^2)
f_despeje <- expression(cos(x))
f_x0 <- 1

g <- expression(x*sin(x) - 1)
g_despeje <- expression(1/sin(x))
g_x0 <- 1

h <- expression(x^3 - 2*x^2 + (4/3)*x - (8/27))
h_despeje <- expression((8/27)*(x^2 - 2*x + (4/3))^-1)
h_x0 <- 0.6

i <- expression((668.061/x) * (1 - exp(-0.146843 * x)) - 40)
i_despeje <- expression((668.061/40) * (1 - exp(-0.146843 * x)))
i_x0 <- 10

j <- expression(x^3 - 2*x - 5)
j_despeje <- expression(5*(x^2 - 2)^-1)
j_x0 <- 2

functions = c(f_despeje, g_despeje, h_despeje, i_despeje, j_despeje)
points_x0 = c(f_x0, g_x0, i_x0, j_x0)
tols = c(10^-8, 10^-16, 10^-32, 10^-56)

##################################################################################
# PROCESOS: Hacer los llamados a las funciones a su gusto
##################################################################################

##################################################################################
# 1. AITKEN 
# No se recomienda utilizar con tols[3], tols[4]

# aitken(f, f_x0, tols[1])

##################################################################################
# 2. AITKEN - ALTA PRECISION 
# Utilizar con cualquier tols[i]
# Se recomienda establecer bits = 180 para tols[4]

# aitkenAltaPrecision(g_despeje, g_x0, tols[4], 180)