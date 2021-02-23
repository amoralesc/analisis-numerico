# Método Newton-Raphson Relajado
```
metodo_nrr.R
```

## Uso
Editar únicamente el final del archivo, bajo el enunciado PROCESOS.

## Documentación

### Newton Relajado
```
newtonRelajado(f, x, tol, max_i = 1000)
```
##### Parámetros:
| Parámetro | Tipo       | Descripción  |
|:--------- |:----------:|:----- |
| f         | expression | función a evaluar |
| x         | numeric    | punto inicial x0 a evaluar |
| tol       | numeric    | tolerancia |
| max_i*    | numeric    | máxima cantidad de iteraciones a evaluar |
##### Descripción:
Desarolla el método Newton-Raphson Relajado para la función f, con el punto inicial x0, hasta la tolerancia tol. La implementación sufre pérdida de precisión para tol > 10^-16

### Newton Relajado Alta Precisión
```
newtonRelajadoAltaPrecision(f, x, tol, bits, max_i = 10000)
```
##### Parámetros:
| Parámetro | Tipo       | Descripción  |
|:--------- |:----------:|:----- |
| f         | expression | función a evaluar |
| x         | numeric    | punto inicial x0 a evaluar |
| tol       | numeric    | tolerancia |
| bits      | numeric    | bits de precisión |
| max_i*    | numeric    | máxima cantidad de iteraciones a evaluar |
##### Descripción:
Desarolla el método Newton-Raphson Relajado para la función f, con el punto inicial x0, hasta la tolerancia tol. La implementación alcanza una alta precisión, correspondiente
a la cantidad de bits proporcionada (bits: 180 -> Tol 10^-56).

### Newton Relajado Grafico
```
newtonRelajadoGrafico(f, x, tol, a, b, max_i = 1000)
```
##### Parámetros:
| Parámetro | Tipo       | Descripción  |
|:--------- |:----------:|:----- |
| f         | expression | función a evaluar |
| x         | numeric    | punto inicial x0 a evaluar |
| tol       | numeric    | tolerancia |
| a         | numeric    | Rango de graficación x >= a |
| b         | numeric    | Rango de graficación x <= b |
| max_i*    | numeric    | máxima cantidad de iteraciones a evaluar |
##### Descripción:
Desarolla y grafica geométricamente el método Newton-Raphson Relajado para la función f, con el punto inicial x0, hasta la tolerancia tol. Se recomienda utilizar una tol <= 10^-5

# Método Aitken
```
metodo_aitken.R
```

## Uso
Editar únicamente el final del archivo, bajo el enunciado PROCESOS.

## Documentación

### Aitken
```
aitken(g, p0, tol)
```
##### Parámetros:
| Parámetro | Tipo       | Descripción  |
|:--------- |:----------:|:----- |
| g         | expression | función x=g(x) despejada |
| p0        | numeric    | punto inicial p0 a evaluar |
| tol       | numeric    | tolerancia |
##### Descripción:
Desarolla el método de Aitken para la función x=g(x), con el punto inicial x0, hasta la tolerancia tol. La implementación sufre pérdida de precisión para tol > 10^-16

### Aitken Alta Precision
```
aitkenAltaPrecision(g, p0, tol, bits)
```
##### Parámetros:
| Parámetro | Tipo       | Descripción  |
|:--------- |:----------:|:----- |
| g         | expression | función x=g(x) despejada |
| p0        | numeric    | punto inicial p0 a evaluar |
| tol       | numeric    | tolerancia |
| bits      | numeric    | bits de precisión |
##### Descripción:
Desarolla el método de Aitken para la función x=g(x), con el punto inicial x0, hasta la tolerancia tol. La implementación alcanza una alta precisión, correspondiente
a la cantidad de bits proporcionada (bits: 180 -> Tol 10^-56).
