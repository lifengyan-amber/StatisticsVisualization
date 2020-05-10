#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('gifski')
#install.packages('png')
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    m <- list(
        l = 0,
        r = 0,
        b = 0,
        t = 70,
        pad = 0
    )
    

    output$swT <- renderPlotly({
        
        new_pvals <- NULL
        dfs_ind <- NULL
        skew <- NULL
        

        #Simulation for df selected
        for (i in 1:input$samplesize) {
            if(input$distribution == "rcauchy"){
                ex <- rcauchy(100) + ifelse(input$right_skewed,rexp(100),0)+ifelse(input$left_skewed,-rexp(100),0)
            } else if (input$distribution == "rt"){
                ex <- rt(100, df = input$df)+ ifelse(input$right_skewed,rexp(100),0)+ifelse(input$left_skewed,-rexp(100),0)
            } else if (input$distribution == "rnorm") {
                ex <- rnorm(100) + ifelse(input$right_skewed,rexp(100),0)+ifelse(input$left_skewed,-rexp(100),0)
            } else if (input$distribution == "runif") {
                ex <- runif(100) + ifelse(input$right_skewed,rexp(100),0)+ifelse(input$left_skewed,-rexp(100),0)
            } else {
                ex <- rt(100, df = input$df) + ifelse(input$right_skewed,rexp(100),0)+ifelse(input$left_skewed,-rexp(100),0)
            }
            
            test <- shapiro.test(ex)
            dfs_ind <- c(dfs_ind, input$df)
            new_pvals <- c(new_pvals, test$p.value)
        }
        
        
        new_pvals = data.frame(ID = 1:input$samplesize,p_values = new_pvals, reject = if_else(new_pvals < input$cutoff, "Do not Reject", "Reject"))
        
        fig <- plot_ly(new_pvals, x = ~ID)
        fig <- fig %>% add_trace(y = ~p_values,color=~reject,mode = 'markers')
        fig <- fig %>% add_trace(y = input$cutoff, name = "Cut Off", mode = 'lines',line=list(color='red'))
        fig <- fig %>% layout(title=paste("Reject Probability-",mean(new_pvals$p_values < input$cutoff)),margin = m,yaxis = list(range = c(0,1)))
        

    })
    
    
    output$qqplot <- renderPlot({
        skew = 0
        set.seed(1337)
        add = rexp(input$samplesize)
        if(input$right_skewed){
            
            skew = skew + add
            
        } 
        if (input$left_skewed){
            skew = skew - add
        }
        
        if(input$distribution == "rcauchy"){
            ex <- rcauchy(input$samplesize) + skew
        } else if (input$distribution == "rt"){
            ex <- rt(input$samplesize, df = input$df)+ skew
        } else if (input$distribution == "rnorm") {
            ex <- rnorm(input$samplesize)+ skew
        } else if (input$distribution == "runif") {
            ex <- runif(input$samplesize)+ skew
        } else {
            ex <- rt(input$samplesize, df = input$df)+ skew
        }
        distributions = data.frame(ex)
        rs <- ggplot(data = distributions) +
            aes(sample = ex) +
            theme_minimal() +
            geom_qq(color = "#cc6699") +
            geom_qq_line(color = "#009999")+theme(plot.margin = margin(2, 2, 2, 2, "cm"),axis.title.x = element_text(size = 16),
                                                  axis.title.y = element_text(size = 16))
                
            
        rs
        
        
    })
    
    output$histogram <- renderPlot({
        skew = 0
        set.seed(1338)
        add = rexp(input$samplesize)
        if(input$right_skewed){
            
            skew = skew + add
            
        } 
        if (input$left_skewed){
            skew = skew - add
        }
        
        if(input$distribution == "rcauchy"){
            ex <- rcauchy(input$samplesize) + skew
        } else if (input$distribution == "rt"){
            ex <- rt(input$samplesize, df = input$df)+ skew
        } else if (input$distribution == "rnorm") {
            ex <- rnorm(input$samplesize)+ skew
        } else if (input$distribution == "runif") {
            ex <- runif(input$samplesize)+ skew
        } else {
            ex <- rt(input$samplesize, df = input$df)+ skew
        }
        distributions = data.frame(ex)
        
        rs <- ggplot(data = distributions) +
            aes(x = ex) +
            theme_minimal() + labs (x = "Distribution")+
            geom_histogram(bins = 50, color="darkblue", fill="lightblue") +
            geom_vline(xintercept = mean(distributions$ex), size = 0.5, color = "#ff6600") +
            geom_vline(xintercept = median(distributions$ex), size = 0.5, color = "#339966")+theme(plot.margin = margin(2, 2, 2, 2, "cm"),axis.title.x = element_text(size = 16),
                                                                                                axis.title.y = element_text(size = 16))
        rs
        
        
    })
    
    
    

})
