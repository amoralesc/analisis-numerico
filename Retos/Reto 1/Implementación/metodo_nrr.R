library("Rmpfr")

mpfrStr <- function(n) {
	return (sub("\'mpfr1\' ", "", capture.output(n@.Data[[1]])))
}

nrr <- function(f, x, tol, bits, max_i = 10000) {
	startTime <- Sys.time()
	
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
	
	endTime <- Sys.time()
	elapsedTime <- endTime - startTime 
	print(elapsedTime)
	
	cat("i=", i, "\tx=", mpfrStr(x), "\te=", mpfrStr(error), "\n")	# Última x
	return (c(x, i))
}

###########################################################################
# INPUT: No tocar nada
###########################################################################

f <- expression(10/x - x)

h1 <- expression(10/x - x + ((1 + sqrt(1 + 684*x))/(6*x)))
h1_x0 <- 4

h2 <- expression(10/x - x + ((1 - sqrt(1 + 684*x))/(6*x)))
h2_x0 <- 1.5

Fx <- function(x) { mpfr(eval(f[[1]]), 180) }

tols <- c(10^-8, 10^-16, 10^-32, 10^-56)
bits <- 180

###########################################################################
# PROCESOS: Hacer los llamados a las funciones a su gusto
###########################################################################

# nrr(h1, h1_x0, tols[4], bits)
# nrr(h2, h2_x0, tols[4], bits)
