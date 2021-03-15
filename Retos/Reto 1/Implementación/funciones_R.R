# Importar librerías
library("pracma")
library("Rmpfr")
library("rootSolve")

###############################################################################
# 1. polyroot
###############################################################################

values_pr = polyroot(c(-8/27, 4/3, -2, 1))
print(values_pr[1], digits=20)
print(values_pr[2], digits=20)
print(values_pr[3], digits=20)

###############################################################################
# 2. uniroot
###############################################################################

# 1.
fx = expression(x^3 - 2*x^2 + (4/3)*x - (8/27))
Fx <- function(x) {{ eval(fx[[1]]) }}
values_ur = uniroot(Fx, c(0, 1), tol = 10^-16, maxiter=100)

print(values_ur["root"], digits=20)
print(values_ur["f.root"])
print(values_ur["iter"])
print(values_ur["estim.prec"], digits=20)

# 2.
hx = expression(10/x - x + ((1 + sqrt(1 + 684*x))/(6*x)))
Hx <- function(x) {{ eval(hx[[1]]) }}
values_ur2 = uniroot(Hx, c(4, 5), tol = 10^-16, maxiter=100)

print(values_ur2["root"], digits=20)
print(values_ur2["f.root"])
print(values_ur2["iter"])
print(values_ur2["estim.prec"], digits=20)

###############################################################################
# 3. brentDekker
###############################################################################

# 1.
fx = expression(x^3 - 2*x^2 + (4/3)*x - (8/27))
Fx <- function(x) {{ mpfr(eval(fx[[1]]), 180) }}
values_bd = brentDekker(Fx, 0, 1, maxiter = 100, tol = mpfr(10^-50, 180))

print(values_bd["root"])
print(values_bd["f.root"])
print(values_bd["f.calls"])
print(values_bd["estim.prec"])

# 2.
hx = expression(10/x - x + ((1 + sqrt(1 + 684*x))/(6*x)))
Hx <- function(x) {{ mpfr(eval(hx[[1]]), 180) }}
values_bd2 = brentDekker(Hx, 4, 5, maxiter = 100, tol = mpfr(10^-50, 180))

print(values_bd2["root"])
print(values_bd2["f.root"])
print(values_bd2["f.calls"])
print(values_bd2["estim.prec"])

###############################################################################
# 4. uniroot.all
###############################################################################

# 1.
fx = expression(x^3 - 2*x^2 + (4/3)*x - (8/27))
Fx <- function(x) {{ eval(fx[[1]]) }}
values_ura = uniroot.all(Fx, c(0, 1), tol = 10^-16, maxiter=100)

print(values_ura, digits=20)

# 2. 
hx = expression(10/x - x + ((1 + sqrt(1 + 684*x))/(6*x)))
Hx <- function(x) {{ eval(hx[[1]]) }}
values_ura2 = uniroot.all(Hx, c(4, 5), tol = 10^-16, maxiter=100)

print(values_ura2, digits=20)
