################################################################################
# ANALISIS NUMERICO - RETO 3
# Simulacion de modelo Depredador-Presa de Lotka-Volterra
################################################################################

################################################################################
# 1.) Instalacion de librerias
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("deSolve")
# install.packages("phaseR")
################################################################################

################################################################################
# 2.) Importar librerias
library(shiny)
library(shinythemes)
library(deSolve)
library(phaseR)
################################################################################

################################################################################
# 3.) Cargar datos de conejos - linces en Canadá 1900 a 1921

cl_canada_parms = c(alpha = 0.4, beta = 0.018, gamma = 0.8, delta = 0.023)
cl_canada_iniciales = c(x = 30, y = 4)

################################################################################

################################################################################
# 4.) Modelo EDO de Depredadores - Presas

modeloDP <- function(t, state, parms) {
	with(as.list(c(state, parms)), {
		dx <- alpha * x - beta * x * y
		dy <- - gamma * y + delta * x * y
		
		return(list(c(dx, dy)))
	})
}

################################################################################

################################################################################
# 5.) WebApp

ui <- fluidPage(
	titlePanel(h1("Simulación del Modelo Depredador-Presa de Lotka-Volterra")),
	sidebarLayout(
		sidebarPanel(
			h3("Datos de entrada"),
			selectInput("dp", h4("Depredadores y Presas"), 
						choices = list("Conejos y Linces en 1900 Canadá" = 1,
									   "Especies propias" = 2), selected = 1),
			fluidRow(
				column(6, 
					numericInput("presas_iniciales", 
								 h4("Presas iniciales"), value = 30)
				),
				column(6, 
					numericInput("depredadores_iniciales", 
								 h4("Depredadores iniciales"), value = 4)
				)
			),
			h4("Coeficientes de las presas"),
			fluidRow(
				column(6,
					numericInput("alpha", h5("Alpha"), value = 0.4)
				),
				column(6,
					numericInput("beta", h5("Beta"), value = 0.018)
				)
			),
			h4("Coeficientes de los depredadores"),
			fluidRow(
				column(6,
					numericInput("gamma", h5("Gamma"), value = 0.08)
				),
				column(6,
					numericInput("delta", h5("Delta"), value = 0.023)
				)
			),
			fluidRow(
				column(9,
					sliderInput("tiempo", h4("Tiempo de simulación"),
					   			min = 0, max = 100, value = 50),
				),
				column(3,
					selectInput("unidad_tiempo", h4("Unidad de tiempo"), 
					   			choices = list("horas" = 1, "días" = 2, 
					   						   "meses" = 3, "años" = 4), 
					   			selected = 4),
				)
			),
			fluidRow(
				column(6,
					selectInput("metodo", h4("Método numérico"), choices = list("Euler" = 1,
																				"Runge-Kutta" = 2),
								selected = 1)
				),
				column(6,
					   sliderInput("paso", h4("Tamaño del paso"),
					   			min = 0, max = 1, value = 0.1),
				)
			),
			div(style = "text-align:center", 
				actionButton("simular", "Simular")
			)
		),
		mainPanel(
			h2(textOutput("titulo_sim")),
			plotOutput("plot_sim", width = "90%")
		)
	)
)

server <- function(input, output, session) {
	v <- reactiveValues(titulo_sim = NULL, solucion_edo = NULL)
	
	parms_plot <- reactiveValues(unidad_tiempo = NULL, tiempo = NULL,
								 paso = NULL)
	
	datosIniciales <- reactive({
		c(x = input$presas_iniciales, y = input$depredadores_iniciales)
	})
	
	parms <- reactive({
		c(alpha = input$alpha, beta = input$beta, 
		  gamma = input$gamma, delta = input$delta)
	})
	
	tiempo <- reactive({
		seq(0, input$tiempo, by=input$paso)
	})
	
	observeEvent(input$dp, {
		if (input$dp == 1) {
			updateNumericInput(session, "presas_iniciales", value =
							   	getElement(cl_canada_iniciales, "x"))
			updateNumericInput(session, "depredadores_iniciales", value =
							   	getElement(cl_canada_iniciales, "y"))
			updateNumericInput(session, "alpha", value =
							   	getElement(cl_canada_parms, "alpha"))
			updateNumericInput(session, "beta", value =
							   	getElement(cl_canada_parms, "beta"))
			updateNumericInput(session, "gamma", value =
							   	getElement(cl_canada_parms, "gamma"))
			updateNumericInput(session, "delta", value =
							   	getElement(cl_canada_parms, "delta"))
			updateSliderInput(session, "tiempo", value = 50)
			updateSelectInput(session, "unidad_tiempo", selected = 4)
			updateSliderInput(session, "paso", value = 0.1)
		}
	})
	
	observeEvent(input$simular, {
		if (input$metodo == 1) {
			# Solución EDO mediante Euler
			v$solucion_edo <- as.data.frame(ode(func = modeloDP, y = datosIniciales(), 
											 parms = parms(), times = tiempo(),
											 method = "euler"))
			
			if (input$unidad_tiempo == 1) {
				parms_plot$unidad_tiempo <- "horas"
			} else if (input$unidad_tiempo == 2) {
				parms_plot$unidad_tiempo <- "días"
			} else if (input$unidad_tiempo == 3) {
				parms_plot$unidad_tiempo <- "meses"
			} else if (input$unidad_tiempo == 4) {
				parms_plot$unidad_tiempo <- "años"
			}
			parms_plot$tiempo = input$tiempo
			parms_plot$paso = input$paso
			
			v$titulo_sim <- "Simulación mediante el Método de Euler"
		} else if (input$metodo == 2) {
			# Solución EDO mediante Runge-Kutta
			v$solucion_edo <- as.data.frame(ode(func = modeloDP, y = datosIniciales(), 
											 parms = parms(), times = tiempo(),
											 method = "rk4"))
			
			if (input$unidad_tiempo == 1) {
				parms_plot$unidad_tiempo <- "horas"
			} else if (input$unidad_tiempo == 2) {
				parms_plot$unidad_tiempo <- "días"
			} else if (input$unidad_tiempo == 3) {
				parms_plot$unidad_tiempo <- "meses"
			} else if (input$unidad_tiempo == 4) {
				parms_plot$unidad_tiempo <- "años"
			}
			parms_plot$tiempo = input$tiempo
			parms_plot$paso = input$paso
			
			v$titulo_sim = "Simulación mediante el Método de Runge-Kutta orden 4"
		}
	})
	
	output$titulo_sim <- renderText({
		if (is.null(v$titulo_sim)) return()
		v$titulo_sim
	})
	
	output$plot_sim <- renderPlot({
		if (is.null(v$solucion_edo)) return()
		#matplot(v$solucion_edo[,-1], type = "l", 
		#	xlab = paste("Tiempo (", unidadTiempo(), ")", sep=""), 
		#	ylab = "Población", col = c(2,3), xaxt='n')
		#axis(side=1, at=seq(0, input$tiempo * (1 / paso()), (1 / paso()) * 10), 
		#	 labels=seq(0, input$tiempo, 10))
		#axis(2)
		#legend("topleft", c("Presa", "Depredador"),
		#	   lty = c(1,2), col = c(2,3), box.lwd = 0)
		
		matplot(v$solucion_edo[,-1], type = "l", 
				xlab = paste("Tiempo (", parms_plot$unidad_tiempo, ")", sep=""), 
				ylab = "Población", col = c(2,3), xaxt='n')
		axis(side=1, at=seq(0, parms_plot$tiempo * (1 / parms_plot$paso), 
							(1 / parms_plot$paso) * 10), 
			 labels=seq(0, parms_plot$tiempo, 10))
		axis(2)
		legend("topleft", c("Presa", "Depredador"),
			   lty = c(1,2), col = c(2,3), box.lwd = 0)
	})
}

# Run the app ----
options(shiny.port = 8100)
shinyApp(ui = ui, server = server)
################################################################################
