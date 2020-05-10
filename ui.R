#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

shinyUI(fluidPage(
    tags$head(tags$style(
        HTML('
         #sidebar {
         }

        body, label, input, button, select { 
          font-family: "Arial";
        }')
    )),

    titlePanel("Normality"),

    fluidRow(
        sidebarPanel(id="sidebar",
            sliderInput("df",
                        "Degree of Freedom:",
                        min = 1,
                        max = 1000,
                        value = 100),
            sliderInput("samplesize",
                        "Sample Size:",
                        min = 1,
                        max = 2000,
                        value = 1000),
            
            
            numericInput("cutoff", "Cut Off:", 0.05, min = 0, max = 1),
            
            selectInput("distribution", "Distribution:",
                        c("T Distribution" = "rt",
                          "Cauchy Distribution" = "rcauchy",
                          "Normal Distribution" = "rnorm",
                          "Continuous Uniform Distribution" = "runif")),
            
        
           fluidRow( column(5,checkboxInput("right_skewed", "Right Skewed", value = FALSE, width = NULL)),
            column(5,checkboxInput("left_skewed", "Left Skewed", value = FALSE, width = NULL)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(position = "below",
                        tabPanel("P-values of Shapiro-Wilk test", plotlyOutput("swT")),
                        tabPanel("QQ Plot", plotOutput("qqplot")),
                        tabPanel("Histogram", plotOutput("histogram"))
            )
        )
    )
))
