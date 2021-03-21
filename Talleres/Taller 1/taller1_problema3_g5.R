###################################################################################
# Suponga que en el siguiente modelo f(x) describe la cantidad de personas que son infectadas por un
# virus, en donde t es el tiempo en d´ias
# f(t) = k1t + k2t^2 + k3e^0.15t
# 
# Se conocen los siguientes datos:
# f(10) = 25; f(15) = 190; f(20) = 950 
# Determine de forma aproximada, el día más cercano donde la
# cantidad de personas infectadas estará entre [1500 ??? 1600]
###################################################################################

library("pracma")

fmt <- function(n, precision) {
	return (formatC(n, format = "e", digits=precision - 1))
}

hallarT <- function(f, valores) {
	t <- 0
	sol <- 0
	while (sol < 1500 || sol > 1600) {
		t <- t + 1
		sol <- f(t, valores)	
	}
	
	cat("\nRESULTADOS")
	cat("\nDias para alcanzar el intervalo: ", t)
	cat("\nPersonas infectadas: ", sol)
	
	return(c(t, sol))
}

imprimirSol <- function(valores_solve, valores_iterativo, prec) {
	cat("SOLUCIONES")
	cat("\nObtenidos mediante el metodo iterativo\n")
	cat("k1 = ", fmt(valores_iterativo[1], prec), "\n")
	cat("k2 = ", fmt(valores_iterativo[2], prec), "\n")
	cat("k3 = ", fmt(valores_iterativo[3], prec), "\n")
	
	cat("Comparacion con la funcion solve\n")
	cat("k1 = ", fmt(valores_solve[1], prec), "\n")
	cat("k2 = ", fmt(valores_solve[2], prec), "\n")
	cat("k3 = ", fmt(valores_solve[3], prec), "\n")
}

# Ecuaciones
# 1) 10 * k1 + 100 * k2 + e^1.5 * k3 = 25
# 2) 15 * k1 + 225 * k2 + e^2.25 * k3 = 190
# 3) 20 * k1 + 400 * k2 + e^3 * k3 = 950

A <- matrix(c(10, 100, exp(1.5), 
			  15, 225, exp(2.25), 
			  20, 400, exp(3)), nrow = 3, ncol = 3, byrow = TRUE)

B <- matrix(c(25, 190, 950), nrow=3, ncol=1, byrow = TRUE)

#################################################################################
# ITERATIVOS
#################################################################################
# 1) Jacobi
sol_j1 = pracma::itersolve(A, B, nmax = 1000, tol = 10^-8, method = "Jacobi")
sol_j2 = pracma::itersolve(A, B, nmax = 1000, tol = 10^-16, method = "Jacobi")

# DIVERGE

#################################################################################
# 2) Richardson
sol_r1 = pracma::itersolve(A, B, nmax = 110, tol = 10^-8, method = "Richardson")
sol_r2 = pracma::itersolve(A, B, nmax = 110, tol = 10^-16, method = "Richardson")

# Toca ajustar nmax
# Sin embargo, no da el valor exacto

#################################################################################
# 3) Gauss-Seidel
sol_gs1 = pracma::itersolve(A, B, nmax = 1000, tol = 10^-8, method = "Gauss-Seidel")
sol_gs2 = pracma::itersolve(A, B, nmax = 1000, tol = 10^-16, method = "Gauss-Seidel")

# CONVERGE

#################################################################################

# SOLUCION MEDIANTE GUASS-SEIDEL
# TOLERANCIA: 10^-16
# TOMA 256 ITERACIONES

prec <- 16
valores_solve <- solve(A, B)
valores_iterativo <- sol_gs2["x"][[1]]

imprimirSol(valores_solve, valores_iterativo, prec)

f <- function(t, valores) { valores[1] * t + valores[2] * t^2 + valores[3] * exp(0.15*t) }

solucion <- hallarT(f, valores_iterativo)



