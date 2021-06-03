################################################################################
# ANALISIS NUMERICO - RETO 3
# Simulacion del Covid-19 mediante modelo epidemiologico SI y modelo SIR
################################################################################

################################################################################
# 1.) Instalacion de librerias
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("deSolve")
################################################################################

################################################################################
# 2.) Importar librerias
library(shiny)
library(shinythemes)
library(deSolve)
################################################################################

################################################################################
# 3.) Cargar base de datos Covid-19
# Fuente: ECDC (European Centre for Disease Prevention and Control)
datos <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
				  na.strings = "", fileEncoding = "UTF-8-BOM")
################################################################################

################################################################################
# 4.) Funciones de negocio

obtenerPaises <- function(datos) {
	paises <- c()
	for (i in 1:length(datos$countriesAndTerritories)) {
		if (!is.element(datos$countriesAndTerritories[i], paises)) {
			paises <- c(paises, datos$countriesAndTerritories[i])
		}
	}
	return(paises)
}

paises <- obtenerPaises(datos)

obtenerPais <- function(datos, pais) {
	pos <- -1
	cant <- 0
	for (i in 1:length(datos$countriesAndTerritories)) {
		if (datos$countriesAndTerritories[i] == pais) {
			if (pos == -1) {
				pos <- i
			} else {
				cant <- cant + 1
			}
		}
	}
	pais <- datos[(pos+cant):pos,]
	pais[["dateRep"]] <- as.Date(pais[["dateRep"]], format="%d/%m/%Y")
	return(pais)
}

colombia <- obtenerPais(datos, "Colombia")

rangoFechasPais <- function(datos_pais) {
	#fecha <- strsplit(datos_pais$dateRep[1], "/")
	#fechaMin <- paste(fecha[[1]][3], fecha[[1]][2], fecha[[1]][1], sep="-")
	
	#fecha <- strsplit(datos_pais$dateRep[length(datos_pais$dateRep)], "/")
	#fechaMax <- paste(fecha[[1]][3], fecha[[1]][2], fecha[[1]][1], sep="-")
	
	fechaMin <- datos_pais$dateRep[1]
	fechaMax <- datos_pais$dateRep[length(datos_pais$dateRep)]
	
	return(c(fechaMin, fechaMax))
}

obtenerDatosSI <- function(pais, pos_fecha) {
	i <- sum(pais$cases[pos_fecha:(pos_fecha - 13)]) / 1
	s <- (pais$popData2019[1]/1) - i
	return(list(S = s, I = i))
}

obtenerBSI <- function(pais, pos_fecha) {
	i <- sum(pais$cases[pos_fecha:(pos_fecha - 13)]) / 1
	s <- (pais$popData2019[1]/1) - i
	di <- pais$cases[pos_fecha]
	b <- di / (s)
	b <- b/i
	return(b)
}

obtenerDatosPaisSI <- function(pais, fecha) {
	pos_fecha = -1
	for (i in 1:length(pais$dateRep)) {
		if (pais$dateRep[i] == fecha) {
			pos_fecha = i
			break
		}
	}
	if (pos_fecha == -1) {
		return (c(S = 0, I = 0, beta = 0))
	}
	
	SI <- obtenerDatosSI(pais, pos_fecha)
	S <- SI["S"][[1]]
	I <- SI["I"][[1]]
	
	ultimos14 <- c()
	for (i in pos_fecha:(pos_fecha-13)) {
		ultimos14 <- c(ultimos14, obtenerBSI(pais, pos_fecha))
	}
	beta <- mean(ultimos14)
	
	return (c(S = S, I = I, beta = beta))
}

obtenerGamma <- function(pais, pos_fecha) {
	i <- sum(pais$cases[pos_fecha:(pos_fecha - 13)]) / 1
	dr <- pais$cases[pos_fecha - 13]
	
	return(dr/i)
}

obtenerBSIR <- function(pais, pos_fecha) {
	gamma <- obtenerGamma(pais, pos_fecha)
	i <- sum(pais$cases[pos_fecha:(pos_fecha - 13)]) / 1
	s <- (pais$popData2019[1]/1) - i
	di <- pais$cases[pos_fecha]
	
	b <- di + gamma*i
	b <- b / s
	b <- b / i
	return(b)
}

obtenerDatosPaisSIR <- function(pais, fecha) {
	pos_fecha = -1
	for (i in 1:length(pais$dateRep)) {
		if (pais$dateRep[i] == fecha) {
			pos_fecha = i
			break
		}
	}
	if (pos_fecha == -1) {
		return (c(S = 0, I = 0, R = 0, beta = 0, gamma = 0))
	}
	
	SI <- obtenerDatosSI(pais, pos_fecha)
	S <- SI["S"][[1]]
	I <- SI["I"][[1]]
	R <- 0
	for (i in 1:(pos_fecha - 13)) {
		R <- R + pais$cases[i]
	}
	
	ultimos14_b <- c()
	for (i in pos_fecha:(pos_fecha-13)) {
		ultimos14_b <- c(ultimos14_b, obtenerBSIR(pais, pos_fecha))
	}
	beta <- mean(ultimos14_b)
	
	ultimos14_g <- c()
	for (i in pos_fecha:(pos_fecha-13)) {
		ultimos14_g <- c(ultimos14_g, obtenerGamma(pais, pos_fecha))
	}
	gamma <- mean(ultimos14_g)
	
	return (c(S = S, I = I, R = R, beta = beta, gamma = gamma))
}

obtenerDatosPaisSIR(colombia, fecha)

################################################################################

################################################################################
# 5.) Modelo EDO epidemiológico

modeloSI <- function(t, state, parms) {
	with(as.list(c(state, parms)), {
		dS <- - beta * S * I
		dI <- beta * S * I
		
		return(list(c(dS, dI)))
	})
}

modeloSIR <- function(t, state, parms) {
	with(as.list(c(state, parms)), {
		dS <- - beta * S * I
		dI <- beta * S * I - gamma * I
		dR <- gamma * I
		
		return(list(c(dS, dI, dR)))
	})
}

################################################################################

################################################################################
# 6.) WebApp

ui <- fluidPage(
	titlePanel("Simulación del Covid-19 mediante Modelo Epidemiológico SI / SIR"),
	sidebarLayout(
		sidebarPanel(
			h3("Datos de entrada"),
			selectInput("pais", h4("País"), choices = paises, selected = paises[1]),
			dateInput("fecha_inicio", h4("Fecha de inicio")),
			sliderInput("tiempo", h4("Tiempo (días)"), min = 0, max = 300, value = 100),
			selectInput("modelo", h4("Modelo"), choices = list("SI" = 1, "SIR" = 2),
						selected = 1),
			selectInput("metodo", h4("Método numérico"), choices = list("Euler" = 1,
																		"Runge-Kutta" = 2),
						selected = 1),
			div(style = "text-align:center", 
				actionButton("simular", "Simular")
			),
			br(),
			p("Datos tomados de: ECDC (European Centre for Disease Prevention and Control)"),
			a("https://opendata.ecdc.europa.eu/covid19/casedistribution")
		),
		mainPanel(
			h2(textOutput("titulo_sim")),
			plotOutput("plot_sim1", width = "90%")
		)
	)
)

# Define server logic ----
server <- function(input, output, session) {
	v <- reactiveValues(titulo_sim = NULL, solucion_edo1 = NULL)
	
	parms_plot <- reactiveValues(unidad_tiempo = NULL, tiempo = NULL,
								 paso = NULL, lineas = NULL)
	
	datosPais <- reactive({
		obtenerPais(datos, input$pais)
	})
	
	datosPaisSI <- reactive({
		obtenerDatosPaisSI(datosPais(), input$fecha_inicio)
	})
	
	datosPaisSIR <- reactive({
		obtenerDatosPaisSIR(datosPais(), input$fecha_inicio)
	})
	
	datosInicialesSI <- reactive({
		c(S = datosPaisSI()["S"][[1]], I = datosPaisSI()["I"][[1]])
	})
	
	parmsSI <- reactive({
		c(beta = datosPaisSI()["beta"][[1]])
	})
	
	datosInicialesSIR <- reactive({
		c(S = datosPaisSIR()["S"][[1]], I = datosPaisSIR()["I"][[1]], r = datosPaisSIR()["R"][[1]])
	})
	
	parmsSIR <- reactive({
		c(beta = datosPaisSIR()["beta"][[1]], gamma = datosPaisSIR()["gamma"][[1]])
	})
	
	tiempo <- reactive({
		seq(0, input$tiempo, by=0.1)
	})
	
	observeEvent(input$pais, {
		rango_fechas <- rangoFechasPais(datosPais())
		updateDateInput(session, "fecha_inicio", min = rango_fechas[1] + 30,
						max = rango_fechas[2], value = rango_fechas[1] + 30)
	})
	
	observeEvent(input$simular, {
		if (input$modelo == 1 && input$metodo == 1) {
			# Solución EDO mediante Euler
			v$solucion_edo1 <- as.data.frame(ode(func = modeloSI, y = datosInicialesSI(), 
												parms = parmsSI(), times = tiempo(),
												method = "euler"))
			
			parms_plot$unidad_tiempo = "días"
			parms_plot$tiempo = input$tiempo
			parms_plot$paso = 0.1
			parms_plot$lineas = 2
			
			v$titulo_sim <- "Simulación SI mediante el Método de Euler"
		} else if (input$modelo == 1 && input$metodo == 2) {
			# Solución EDO mediante Runge-Kutta
			v$solucion_edo1 <- as.data.frame(ode(func = modeloSI, y = datosInicialesSI(), 
												parms = parmsSI(), times = tiempo(),
												method = "rk4"))
			
			parms_plot$unidad_tiempo = "días"
			parms_plot$tiempo = input$tiempo
			parms_plot$paso = 0.1
			parms_plot$lineas = 2
			
			v$titulo_sim = "Simulación SI mediante el Método de Runge-Kutta orden 4"
		} else if (input$modelo == 2 && input$metodo == 1) {
			# Solución EDO mediante Euler
			v$solucion_edo1 <- as.data.frame(ode(func = modeloSIR, y = datosInicialesSIR(), 
												parms = parmsSIR(), times = tiempo(),
												method = "euler"))
			
			parms_plot$unidad_tiempo = "días"
			parms_plot$tiempo = input$tiempo
			parms_plot$paso = 0.1
			parms_plot$lineas = 3
			
			v$titulo_sim <- "Simulación SIR mediante el Método de Euler"
		} else if (input$modelo == 2 && input$metodo == 2) {
			# Solución EDO mediante Runge-Kutta
			v$solucion_edo1 <- as.data.frame(ode(func = modeloSIR, y = datosInicialesSIR(), 
												parms = parmsSIR(), times = tiempo(),
												method = "rk4"))
			
			parms_plot$unidad_tiempo = "días"
			parms_plot$tiempo = input$tiempo
			parms_plot$paso = 0.1
			parms_plot$lineas = 3
			
			v$titulo_sim = "Simulación SIR mediante el Método de Runge-Kutta orden 4"
		}
	})
	
	output$titulo_sim <- renderText({
		if (is.null(v$titulo_sim)) return()
		v$titulo_sim
	})
	
	output$plot_sim1 <- renderPlot({
		if (is.null(v$solucion_edo1)) return()
		matplot(v$solucion_edo1[,-1], type = "l", 
				xlab = paste("Tiempo (", parms_plot$unidad_tiempo, ")", sep=""), 
				ylab = "Población", col = 2:(parms_plot$lineas + 1), xaxt='n')
		axis(side=1, at=seq(0, parms_plot$tiempo * (1 / parms_plot$paso), 
							(1 / parms_plot$paso) * 10), 
			 labels=seq(0, parms_plot$tiempo, 10))
		axis(2)
		#legend("topleft", c("Presa", "Depredador"),
		#	   lty = c(1,2), col = c(2,3), box.lwd = 0)
	})
}

# Run the app ----
options(shiny.port = 8100)
shinyApp(ui = ui, server = server)
################################################################################
