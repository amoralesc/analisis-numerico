library("Rmpfr")

mpfrStr <- function(n) {
	return (sub("\'mpfr1\' ", "", capture.output(n@.Data[[1]])))
}

brent <- function(g, a, b, tol, bits) {
	startTime <- Sys.time()
	
	f <- function(x) { mpfr(eval(g[[1]]), bits) }
	
	a <- mpfr(a, bits)
	b <- mpfr(b, bits)
	c <- mpfr(a, bits)
	d <- mpfr(0, bits)
	
	s <- mpfr(a, bits)
	
	if (f(a)*f(b) >= 0) {
		print("f(a)*f(b) >= 0")
		break
	}
	
	flag <- TRUE
	
	i <- 0
	
	while (f(b) != 0 | f(s) != 0 | mpfr(abs(b - a), bits) >= mpfr(tol, bits)) {
		if (f(a) != f(c) & f(b) != f(c)) {	# Interpolación inversa cuadrática
			s <- ((a * f(b) * f(c)) / (f(a) - f(b)) * (f(a) - f(c))) + ((b * f(a) * f(c))/(f(b) - f(a)) * (f(b) - f(c))) + ((c * f(a) * f(b)) / (f(c) - f(a)) * (f(c) - f(b)))
			
		} else {	# Método secante
			s <- b - f(b) * ((b-a) / (f(b) - f(a)))
			
		}
		
		if (!((3*a + b)/4 < s & s < b) |
			(flag == TRUE & abs(s-b) >= abs(b-c)/2) |
			(flag == FALSE & abs(s-b) >= (c-d)/2) |
			(flag == TRUE & abs(b-c) < abs(tol)) |
			(flag == FALSE & abs(c-d) < abs(tol))) {
			s = (a+b)/2
			flag = TRUE
			
		} else {
			flag = FALSE
		}
		
		d = c
		c = b
		
		if (f(a)*f(s) < 0) {
			b = s
		} else {
			a = s
		}
		
		if (abs(f(a)) < abs(f(b))) {
			a_copia = a
			a = b
			b = a_copia
		}
		# print(i)
		# print(s)
		i <- i + 1
	}
	
	endTime <- Sys.time()
	elapsedTime <- endTime - startTime 
	# print(elapsedTime)
	print(i)
	return (c(b, s))
}

###########################################################################
# INPUT: No tocar nada
###########################################################################

fx = expression(x^3 - 2*x^2 + (4/3)*x - (8/27))
hx <- expression(10/x - x + ((1 + sqrt(1 + 684*x))/(6*x)))
tol = mpfr(10^-56, 180)

###########################################################################
# PROCESOS: Hacer los llamados a las funciones a su gusto
###########################################################################

# brent(fx, 0, 1, tol, 180)
