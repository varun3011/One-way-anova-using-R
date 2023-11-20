#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)


# Define UI for application that draws a histogram

ui <- fluidPage(
    shinythemes::themeSelector(),
    
    
    # Application title
    titlePanel("One Way Anova"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("dataset", "Choose CSV file", multiple = FALSE,
                      accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),
                      width = NULL, buttonLabel = "Browse...",
                      placeholder = "No file selected"),
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("About",
                         h2("One Way Anova"),
                         p("The one-way analysis of variance (ANOVA), also known as one-factor ANOVA,is an extension of independent two-sample t-test for comparing means in a
situation where there are more than two groups. In one-way ANOVA, the data
is organized into several groups base on one single grouping variable (also
called factor variable)."),
                         h4("ANOVA test hypotheses:"),
                         p("-> Null hypothesis: the means of the different groups are the same"),
                         p("-> Alternative hypothesis: At least one sample mean is not equal to the
others."),
                         h4("Assumptions of ANOVA test:"),
                         p("Here we describe the requirement for ANOVA test."),
                         p("-> The observations are obtained independently and randomly from the
population defined by the factor levels."),
                         p("-> The data of each factor level are normally distributed."),
                         p("-> These normal populations have a common variance."),
                ),
                
                tabPanel("Data",
                         h3("Data from CSV File :"),
                         dataTableOutput("content")
                ),
                tabPanel("Testing Homogenity & Variance",
                    verbatimTextOutput("bartlett")
                ),
                tabPanel("One Way Anova",
                         verbatimTextOutput("anova")
                ),
                tabPanel("Tukey Test",
                         verbatimTextOutput("tukey")
                ),
                tabPanel("Contact Us",
                         h3("Viraj D. Patel - 19162121033"),
                         h4("virajdpatel19@gnu.ac.in"),
                         h4("https://www.linkedin.com/in/viraj-patel-3a60731a8"),
                         br(),
                         h3("Varun K. Patel - 19162121032"),
                         h4("varunpatel19@gnu.ac.in"),
                         h4("https://www.linkedin.com/in/varun-patel-a958a21a5"),
                         br(),
                         h3("Priyal R. Thakkar - 19162121045"),
                         h4("priyalthakkar19@gnu.ac.in"),
                         h4("https://www.linkedin.com/in/priyal-thakkar-b048941a1")
                )
            )
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    var1 <- reactive({
        validate(need(input$dataset,""))
        inFile <- input$dataset
        if (is.null(inFile))
            return(NULL)
        df <- read.csv(inFile$datapath,na.strings = c("", "NA", "#N/A"))
        df2 <- df
        return(df2)    
    })
    
    output$content <- renderDataTable({
        var1()
    })
    
    mydata <- reactive(as.data.frame(var1()))
    stack1 <- reactive(as.data.frame(stack(mydata())))
     
    output$bartlett <- renderPrint({
        
            bartlett.test(values~ind,data=stack1())
    })  
    
    output$anova <- renderPrint({
        
        d1 <- lm(values~ind,data=stack1())
        anova(d1)
    })
    
    output$tukey <- renderPrint({
        d2 <- aov(values~ind,data=stack1())
        TukeyHSD(d2)
    })
            
}


# Run the application 
shinyApp(ui = ui, server = server)
