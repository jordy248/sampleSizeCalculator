# ui.R

shinyUI(fluidPage(
        titlePanel("Jordy's A/B Test Sample Size COW-culator"),
        
        sidebarLayout(
                sidebarPanel(
                        helpText("Use the proper sample size for your A/B test
                                 and you just might find yourself as happy
                                 as this cow:"
                        ),
                        img(src="http://i.imgur.com/nsFUwJ1.gif", style=
                                    "width:75%; height:75%; align:center;"
                        ),
                        
                        numericInput("cvr", label = h3("Enter the current 
                                conversion rate as a decimal:"), 
                                value = 0.1
                        ), 

                        sliderInput("mde", label = h3("Select the desired minimum detectable effect:"),
                                min = 0, max = 100, value = 10
                        ),
                        
                        sliderInput("alpha", label = h3("Enter the desired confidence interval:"),
                                    min = 50, max = 100, value = 95
                        ),
                        
                        sliderInput("beta", label = h3("Enter the desired statistical power:"),
                                    min = 50, max = 100, value = 80
                        )
                ),
                
                mainPanel(
                        p("Sample size required per variation: "),
                                h1(strong(textOutput("sample_size_per_variation"), style="color:blue;")),
                        br(),
                        br(),
                        p("TOTAL SAMPLE SIZE REQUIRED: ",style="font-weight:bold; font-size:2em;"),
                                h1(strong(textOutput("sample_size_total"), style="color:blue;"))                        
                        
##                        textOutput("cows")
                        
                )
        )
))