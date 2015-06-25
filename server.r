# server.R

library(maps)
library(mapproj)
counties <- readRDS("data/counties.rds")


source("calcSampleSize.R")

shinyServer(
        function(input, output) {
                

                
                output$sample_size_per_variation <- renderText ({ 
                        
                        vol <- round(new.power.prop.test(p1 = input$cvr, 
                                                   p2 = (input$cvr * (1 + (input$mde/100))), 
                                                   power = input$beta/100, 
                                                   alternative = "two.sided", 
                                                   sig.level = ((100-input$alpha)/100)),0
                        )

                        vol
                })
                
                output$sample_size_total <- renderText ({
                        vol <- round(new.power.prop.test(p1 = input$cvr, 
                                                         p2 = (input$cvr * (1 + (input$mde/100))), 
                                                         power = input$beta/100, 
                                                         alternative = "two.sided", 
                                                         sig.level = ((100-input$alpha)/100)),0
                        )
                        
                        vol * 2
                        
                })
                
##                output$cows <- renderText ({
##                        vol <- round(new.power.prop.test(p1 = input$cvr, 
##                                                         p2 = (input$cvr * (1 + (input$mde/100))), 
##                                                         power = input$beta/100, 
##                                                         alternative = "two.sided", 
##                                                         sig.level = ((100-input$alpha)/100)),0
##                        )
##                        c("That's this many cows: ", rep("cows ",vol))
##                })
                
        }
)