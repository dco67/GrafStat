if (!require("librarian")) install.packages("librarian")
librarian::shelf(
  shiny,
  vioplot,
  ggplot2,
  ggdist,
  LaplacesDemon,
  agrmt,
  sn,
  psych,
  rstatix,
  modeest,
  DT,
  shinyWidgets,
  qcc
)
  
#library(shiny)
#library(vioplot)
#library(ggplot2)
#library(ggdist)
#library(LaplacesDemon)
#library(agrmt)
#library(sn)
#library(psych)
#library(rstatix)
#library(modeest)
#library(DT)
#library(shinyWidgets)
#library(qcc)

source("global.R")

ui <- fluidPage(
  titlePanel("Description de données univariées"),
  withMathJax(),
  tags$div(HTML("<script type='text/x-mathjax-config' >
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script >
                ")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("typedat",
        "Niveau de mesure:",
        choices = c("Nominale", "Ordinale", "Intervalle", "Rapport"),
        selected = "Rapport",
        inline = TRUE
      ),
      conditionalPanel(
        condition = "input.typedat == 'Ordinale' | input.typedat == 'Nominale'",
        numericInput("k",
          "Nombre de catégories:",
          min = 2,
          max = 15,
          value = 5,
          width = "100px"
        ),
        fluidPage(
          column(
            6,
            radioButtons("graftypeQ",
              "Type de graphique:",
              choices = c("Secteurs", "Barres", "Pareto"),
              selected = "Barres"
            )
          ),
          column(
            6,
            conditionalPanel(
              condition = "input.graftypeQ == 'Secteurs'",
              radioButtons("piet",
                "Étiquettes:",
                choices = c("Noms", "Fréquences", "Pourcentages"),
                selected = "Noms"
              )
            ),
            conditionalPanel(
              condition = "input.graftypeQ == 'Barres'",
              radioButtons("baraxe",
                "Axe vertical:",
                choices = c("Fréquences", "Pourcentages"),
                selected = "Fréquences"
              )
            ),
            conditionalPanel(
              condition = "input.graftypeQ == 'Pareto'",
              radioButtons("baraxe2",
                           "Axe vertical:",
                           choices = c("Fréquences", "Pourcentages"),
                           selected = "Fréquences"
              )
            )
            
          )
        )
      ),
      conditionalPanel(
        condition = "input.typedat == 'Intervalle'",
        sliderInput("m",
          "Centre:",
          min = -100,
          max = 100,
          step = 0.1,
          value = 50
        )
      ),
      conditionalPanel(
        condition = "input.typedat == 'Rapport'",
        sliderInput("mr",
          "Centre:",
          min = 0,
          max = 100,
          step = 0.1,
          value = 50
        )
      ),
      conditionalPanel(
        condition = "input.typedat == 'Intervalle' | input.typedat == 'Rapport'",
        sliderInput("s",
          "Dispersion:",
          min = 0,
          max = 100,
          step = 0.1,
          value = 8
        ),
        fluidPage(
          column(
            4,
            radioButtons("graftypeN",
              "Type de graphique:",
              choices = c("Histogramme", "Densité", "Boîte", "H+D+B", "Violon", "Pluie"),
              selected = "Histogramme"
            )
          ),
          column(
            8,
            conditionalPanel(
              condition = "input.graftypeN == 'Histogramme'",
              sliderInput("bin",
                "Nombre de classes:",
                min = 4,
                max = 50,
                value = 15
              ),
              radioButtons("frpr",
                "Axe vertical:",
                choices = c("Fréquences", "Densités"),
                selected = "Fréquences",
                inline = TRUE
              ),
              checkboxInput("fcum",
                "Distribution Cumulative",
                value = FALSE
              ),
              radioButtons("fract",
                           "Fractiles:",
                           choices = c("Aucun", "Quartiles", "Déciles"),
                           selected = "Aucun",
                           inline = TRUE)
            ),
            conditionalPanel(
              condition = "input.graftypeN == 'Densité'",
              sliderInput("bw",
                "Bande Passante:",
                min = 0.01,
                max = 10,
                value = 2
              )
            ),
            conditionalPanel(
              condition = "input.graftypeN == 'Boîte'",
              checkboxInput("dpt",
                "Afficher les données:",
                value = FALSE
              )
            ),
            conditionalPanel(
              condition = "input.graftypeN == 'H+D+B'",
              sliderInput("bin2",
                "Nombre de classes:",
                min = 4,
                max = 50,
                value = 15
              ),
              sliderInput("bw2",
                "Bande Passante:",
                min = 0.01,
                max = 10,
                value = 2
              )
            ),
            conditionalPanel(
              condition = "input.graftypeN == 'Violon'",
              checkboxInput("dpt2",
                "Afficher les données:",
                value = FALSE
              )
            )
          )
        )
      ),

    sliderInput("n",
      "Taille de l'échantillon:",
      min = 10,
      max = 1000,
      value = 200
    ),
    actionBttn("go",
               "Générer les données",
               color = "success",
               style = "stretch",
               icon = icon("sliders"),
               block = TRUE
    ),
    
    p(),
    p(),
    
    wellPanel(
      style = "background: lightblue",
      fluidRow(
        column(
          6,
          a(h4("Par Daniel Coulombe, Ph.D.")),
          p("Institut des Sciences, des Technologies et des Études Avancées d'Haiti"),
          p("2023")
        ),
        column(
          6,
          tags$a(
            href = "https://isteah.org",
            tags$img(
              src = "ISTEAH_LOGO.png",
              title = "ISTEAH",
              width = "160",
              height = "140"
            )
          )
        )
      )
    )
    
    
  ),
  
  mainPanel(
    plotOutput("distPlot"),
    conditionalPanel(
      condition = "input.typedat == 'Nominale' | input.typedat == 'Ordinale'",
      fluidPage(
      column(
        4,
        uiOutput("prtdat1")
      ),
      column(
        4,
        uiOutput("prtdat2")
      ),
      column(
        4,
        uiOutput("prtdat3")
      )
    )
    ),
    
    conditionalPanel(
      condition = "input.typedat == 'Intervalle' | input.typedat == 'Rapport'",
      dataTableOutput("DescStat")
    )
  )
    )
  )

server <- function(input, output, session) {
  dat <- eventReactive(input$go, {
    if (input$typedat == "Nominale" | input$typedat == "Ordinale") {
      sample(1:input$k,
        size = input$n,
        prob = round(runif(input$k, 1, 100)),
        replace = TRUE
      )
    } else {
      ralaplace(
        n = input$n,
        location = input$m,
        scale = input$s,
        kappa = runif(1, 0.01, 1.5)
      )
    }
  })

  output$distPlot <- renderPlot({
    if (input$typedat == "Nominale" | input$typedat == "Ordinale") {
      data <- table(dat())
      if (input$baraxe == "Pourcentages") {
        data <- round(100 * data / sum(data), 2)
        barlbl2 <- paste0(data, "%")
      } else {
        barlbl2 <- data
      }
      if (input$graftypeQ == "Barres") {
        if (input$typedat == "Nominale") {
          barlbl <- paste0("Catégorie ", 1:input$k)
        } else {
          barlbl <- c(paste0(2:input$k - 1, " < "), as.character(input$k))
        }
        yaxe <- ifelse(input$baraxe == "Fréquences", "Fréquences", "Pourcentages")
        ylim <- c(0, 1.1 * max(data))

        gr <- barplot(data,
          xaxt = "n",
          xlab = "",
          width = 0.85,
          ylim = ylim,
          main = paste0("Diagramme en barres \n Catégories ", input$typedat, "s"),
          ylab = input$baraxe,
          cex.main = 1.5,
          cex.names = 1.5,
          cex.lab = 1.5,
          col = "lightblue"
        )
        text(x = gr, y = data, label = barlbl2, pos = 3, cex = 1.2, col = "red")
        axis(1, at = gr, labels = barlbl, tick = FALSE, las = 1, line = -0.5, cex.axis = 1.2)
      }
      else if (input$graftypeQ == "Pareto") {
        data <- table(dat())
        if (input$baraxe2 == "Pourcentages") {
          data <- round(100 * data / sum(data), 2)
          barlbl2 <- paste0(data, "%")
        } else {
          barlbl2 <- data
        }
        if (input$typedat == "Nominale") {
          barlbl <- paste0("Catégorie ", 1:input$k)
        } else {
          barlbl <- c(paste0(2:input$k - 1, " < "), as.character(input$k))
        }
        yaxe <- ifelse(input$baraxe2 == "Fréquences", "Fréquences", "Pourcentages")
        ylim <- c(0, 1.1 * max(data))
        
        
        pareto.chart(data,
                     main = "Diagramme de Pareto (Données simulées)",
                     col = heat.colors(nrow(data)),
                     xlab = "Catégories",
                     ylab = ifelse(input$baraxe2 == "Fréquences", "Fréquences", "Pourcentages"),
                     ylab2 = "Pourcentages cumulatifs",
                     cex.axis = 1.2,
                     cex.lab = 1.2)
        }
      
      else {
        pielbl <- if (input$piet == "Noms") {
          paste0("Catégorie ", 1:input$k)
        } else if (input$piet == "Fréquences") {
          paste0(data)
        } else {
          paste0(round(100 * data / sum(data), 2), "%")
        }

        pie(data,
          main = "Diagramme en Secteurs",
          labels = pielbl,
          col = 1:input$k,
          radius = 1,
          cex.main = 1.5
        )
        legend("topright",
          legend = paste0("Catégorie ", 1:input$k),
          fill = c(1:input$k)
        )
      }
    } else {
      data <- dat()
      if (input$graftypeN == "Histogramme") {
        gr <- hist(data,
          breaks = input$bin
        )
        if (input$fcum) {
          gr$counts <- cumsum(gr$counts)
        }
        plot(gr,
          main = "Histogramme de la variable X",
          freq = ifelse(input$frpr == "Fréquences", TRUE, FALSE),
          ylab = ifelse(input$frpr == "Fréquences", "Fréquences", "Densités")
        )
        if(input$fract == "Quartiles"){
          qtl <- quantile(data, c(0.25, 0.5, 0.75))
          abline(v = qtl, col = "red", lty = 2, lwd = 2)
          axis(side = 1, 
               at = qtl, 
               labels = c("Q1", "Q2", "Q3"), 
               tick = FALSE, 
               padj = -1.25)
        } else if(input$fract == "Déciles"){
          qtl <- quantile(data, seq(0.1, 0.9, by = 0.1))
          abline(v = qtl, col = "green", lty = 2, lwd = 2)
          axis(side = 1, 
               at = qtl, 
               labels = paste0("D", 1:9), 
               tick = FALSE, 
               padj = -1.25)
        }
      } else if (input$graftypeN == "Densité") {
        plot(density(data, bw = input$bw),
          main = "Diagramme de Densité",
          xlab = "Variable X",
          ylab = "Densité"
        )
#        md <- mlv(dat(), method = "meanshift")
#        abline(v=md,
#               lty = 2,
#               lwd = 2,
#               col = "red")
        
      } else if (input$graftypeN == "Boîte") {
        boxplot(data,
          horizontal = TRUE,
          xlab = "Variable X",
          col = "lightblue"
        )
        if (input$dpt) {
          stripchart(data, # Data
            method = "jitter", # Random noise
            pch = 19, # Pch symbols
            col = "red", # Color of the symbol
            vertical = FALSE, # Vertical mode
            add = TRUE
          )
        }
      } else if (input$graftypeN == "H+D+B") {
        par(
          las = 1,
          mar = c(5.5, 4.5, 4.5, 2.5)
        )
        hist_boxplot(data,
          bw = input$bw2,
          breaks = input$bin2,
          freq = FALSE,
          ylab = "Densités",
          density = TRUE,
          main = "Distribution de la variable X",
          xlab = "Variable X"
        )
      } else if (input$graftypeN == "Violon") {
        vioplot(data,
          horizontal = TRUE,
          col = "lightblue",
          rectCol = "yellow",
          lineCol = "yellow",
          pchMed = 19,
          colMed = "red"
        )
        if (input$dpt2) {
          stripchart(data, # Data
            method = "jitter", # Random noise
            pch = 19, # Pch symbols
            col = "red", # Color of the symbol
            vertical = FALSE, # Vertical mode
            add = TRUE
          )
        }
      } else {
        data <- data.frame(data, rep(1, input$n))
        colnames(data) <- c("Y", "Groupe")

        plt2 <- data %>%
          ggplot(mapping = aes(x = factor(Groupe), y = Y)) +
          ggdist::stat_halfeye(
            adjust = .5,
            #            width = .4,
            justification = -.3,
            width = 1,
            fill = "lightblue",
            point_colour = "darkgreen"
          ) +
          geom_boxplot(
            width = .15,
            fill = "lightblue",
            outlier.color = "red" ## `outlier.shape = NA` works as well
          ) +
          coord_cartesian(xlim = c(1.2, NA)) +
          labs(
            fill = "#69b3a2",
            y = "Variable X",
            x = ""
          ) +
          stat_summary(fun = "mean", shape = 19, col = "darkblue", geom = "point", size = 4) +
          theme(
            legend.position = "none",
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "bold"),
            aspect.ratio = 3 / 10
          ) +
          stat_dots(
            side = "bottom",
            dotsize = 0.8,
            justification = 1.1,
            binwidth = 0.5,
            colour = "blue"
          )

        plt2 + coord_flip()
      }
    }
  })

  output$prtdat1 <- renderUI({
    if (input$typedat == "Nominale" | input$typedat == "Ordinale") {
      data <- table(dat())
      md <- c(1:input$k)[which.max(data)]
      if (input$typedat == "Ordinale") {
        med <- median(dat())
        qtl <- quantile(dat(), c(0.25, 0.75))
      }

      helpText(
        h4("Position:"), br(),
        if (input$typedat == "Nominale") {
          h5(paste("Mode = Catégorie ", md, " :  Freq = ", data[md]))
        } else if (input$typedat == "Ordinale") {
          h5(
            paste("Médiane (Q2) = ", med), br(),
            paste("Q1           = ", qtl[1]), br(),
            paste("Q3           = ", qtl[2])
          )
        }
      )
    } 
  })

  output$prtdat2 <- renderUI({
    data <- table(dat())
    md <- c(1:input$k)[which.max(data)]
    fmd <- data[md]
    N <- sum(data)
    rv <- 1 - fmd / sum(data)
    dmm <- 1 - sum(fmd - data) / (N * (input$k - 1))
    if (input$typedat == "Ordinale") {
      cnx <- round(consensus(data), 2)
    }

    if (input$typedat == "Nominale") {
      withMathJax(
        helpText(
          h4("Dispersion"), br(),
          strong("Rapport de Variation"), ": ", br(),
          "RV = $1-\\frac{f_{mode}}{N} = 1 - \\frac{", fmd, "}{", N, "}= ", round(rv, 3), " $", p(),
          "RV = probabilité qu’en choisissant une observation au hasard, cette dernière n’appartienne pas à la catégorie modale;
                plus sa valeur est élevée, plus la dispersion est grande..."
        ), p(),
        strong("Déviation Modale Moyenne"), ": ", br(),
        "DMM = $1- \\sum_{i = 1}^{k}\\frac{{f_{mode}-f_i}}{N(k-1)} $", p(),
        "$DMM = 1- \\left [ \\frac{", fmd, " - ", data[1], "}{", N, "(", input$k, " - 1)} + \\frac{", fmd, " - ", data[2], "}{", N, "(", input$k, " - 1)}+...\\right ]=", round(dmm, 3), "$", p(),
        "Plus la valeur de DMM est élevée, plus la répartition des données dans les catégories tend vers l'égalité. DMM prend une valeur de 0 lorsque toutes les ",
        "observations se trouvent dans une seule catégorie, et une valeur de 1 lorsque la répartition dans les $k$ catégories est égale."
      )
    } else if (input$typedat == "Ordinale") {
      dx <- input$k - 1
      mux <- sum(data / N * 1:input$k)
      withMathJax(
        helpText(
          h4("Dispersion"), br(),
          strong("Consensus"), ": ", br(),
          "$Cns_x=1+\\sum\\limits_{i=1}^{k}p_i log_2 \\left(1-\\frac{\\left| f_i-\\mu_x\\right|}{d_x}\\right) = ", round(cnx, 3), "$", p(),
          "où ", br(),
          "$d_x=X_{max}-X_{min}=",dx, "$", br(),
          "$\\mu_x=\\sum\\limits_{i=1}^{k}p_iX_i$",
          "$\\mu_x= ", paste0(round(data[1] / input$n, 3), "\\times", 1, " + ... + ", round(data[input$k] / input$n, 3), "\\times ", input$k, " = ", round(mux, 3), "$"), p(),
          "$Cns_x$ prend une valeur comprise entre 0, lorsque les données se répartissent également dans les deux extrémités [", strong("dissension"), "], ",
          "et 1, lorsque toutes les réponses se trouvent dans une catégorie particulière [", strong("consensus"), "]. ",
          "Si les données se répartissent également dans les différentes catégories, $Cns_x$ est approximativement égal à 0.5."
        )
      )
    } 
  })

  output$prtdat3 <- renderUI({
    data <- table(dat())
    md <- c(1:input$k)[which.max(data)]
    if (input$typedat == "Ordinale") {
      med <- median(dat())
      qtl <- quantile(dat(), c(0.25, 0.75))
    }

    helpText(
      h4("Forme:"), br(),
      if (input$typedat == "Nominale") {
        h5(paste("La distribution peut être uni-, bi-, ou multi-modale"))
      } else if (input$typedat == "Ordinale") {
        h5(
          "Il n'y a pas d'indice numérique autre que le mode, permettant de caractériser la forme de la distribution. ",
          " On peut parfois déceler visuellement une croissance ou une décroissance au niveau des catégories ordonnées."
        )
      } 
#      else {
#        dstat <- round(describe(dat()), 3)
#        outl <- is_outlier(dat(), coef = 1.5)
#        helpText(
#          h5("Symétrie = ", dstat$skew), br(),
#          h5("Voussure = ", dstat$kurtosis),
#          h5("Cas Extrêmes = ", ifelse(length(outl) > 0, "Présents", "Absents"))
#        )
#      }
    )
  })
  
  output$DescStat <- renderDataTable({
    dstat <- round(describe(dat()), 3)
    md <- round(max(Modes(dat())$modes), 3)
    cent <- c(md, dstat$median, dstat$mean, dstat$trimmed, NA)
    moydev <- round(mean(abs(dat() - dstat$mean) / input$n), 3)
    meddev <- round(sum(abs(dat() - dstat$median) / input$n), 3)
    qtl <- quantile(dat(), c(0.25, 0.75))
    tbl <- data.frame(c("Mode", "Médiane", "Moyenne", "Moyenne tronquée", NA, NA),
                      c(md, dstat$median, dstat$mean, dstat$trimmed, NA, NA),
                      c("Étendue", "EIQ", "Déviation Moyenne", "Déviation Médiane", "Variance", "Écart-type"),
                      c(dstat$range, round(qtl[2] - qtl[1], 3), moydev, meddev, round(var(dat()), 3), round(sd(dat()), 3)),
                      c("Symétrie", "Voussure", NA, NA, NA, NA),
                      c(dstat$skew, dstat$kurtosis, NA, NA, NA, NA))
#    colnames(tbl) <- c("Position", "A ", "Dispersion", "B ", "Forme", "C ")
    
    DT::datatable(tbl, 
                  colnames=c("Position", " ", "Dispersion", " ", "Forme", " "),
                  caption = htmltools::tags$caption(
                    style = 'caption-side: bottom; text-align: center;',
                    htmltools::em(h4("Statistiques Descriptives"))),
                  class = "cell-border stripe",
                  rownames= FALSE,
                  options = list(
                    info = FALSE,
                    paging = FALSE,
                    searching = FALSE,
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '100px', targets = "_all"))
                  )
    ) 
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)

# Run in browser
# runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))

# Run in a dialog within R Studio
#runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1200, height = 600))

# Run in Viewer pane
# runGadget(ui, server, viewer = paneViewer(minHeight = 500))