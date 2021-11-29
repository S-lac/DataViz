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
library(shinyBS)
library(cowplot)
library(plotly)

# An app to use the Phymea photosynthesis model included in the QPAR & QPAR Pro

ui <- fluidPage(

  bsAlert("alert"),
  
  title=HTML("Phymea | QPAR Model"), 

  # titlePanel(title=div(" ", img(src="logo_phymea.png"))),
  titlePanel(div(img(src = "logo.png", style="width:15%"),tags$blockquote(tags$span(style="font-family: 'Courier';color:#00AA00;font-size: 90%;","QPAR : grow smarter, grow better ! "))
            )),
  #tags$hr(),
  
  sidebarPanel(width = 4,
      # Sidebarpanel : 
      tags$style(type = "text/css", "
              .irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
                .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
                .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
                .irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
                .irs-grid-pol {display: none;}
                .irs-max {font-family: 'arial'; color: black;}
                .irs-min {font-family: 'arial'; color: black;}
                .irs-single {color:phymea; background:#00AA00;}
                .irs-slider {width: 30px; height: 30px; top: 22px; background:#00AA00;}
                
                .irs-bar1 {width: 50%; height: 25px; background: red; border-top: 1px solid black; border-bottom: 1px solid black;}
                .irs-bar-edge1 {background: black; border: 1px solid red; height: 25px; border-radius: 0px; width: 20px;}
                .irs-line1 {border: 1px solid red; height: 25px; border-radius: 0px;}
                .irs-grid-text1 {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
                .irs-grid-pol1 {display: none;}
                .irs-max1 {font-family: 'arial'; color: red;}
                .irs-min1 {font-family: 'arial'; color: red;}
                .irs-single1 {color:black; background:#6666ff;}
                .irs-slider1 {width: 30px; height: 30px; top: 22px;}
                
                "),
      # Tabsetpanel :  
      tags$style(HTML("
                .tabbable > .nav > li > a                  {background-color: black;  color:white; border-color:grey}
                .tabbable > .nav > li > a[data-value='t1'] {background-color: red;   color:white}
                .tabbable > .nav > li > a[data-value='t2'] {background-color: blue;  color:white}
                .tabbable > .nav > li > a[data-value='t3'] {background-color: green; color:white}
                .tabbable > .nav > li > a[data-value='QPAR - Model'] {background-color: white; color:#00AA00}
                .tabbable > .nav > li[class=active]    > a {background-color: #00AA00; color:black}
                ")),
      
      tags$style(HTML(".selectize-input {background-color: black;border: 2px solid #00AA00;color:#00AA00;}")),
      
      tags$head(tags$link(rel="shortcut icon", href="Favicon.ico")),
      
      selectInput("chosen_specie", 
                  label = NULL,
                  choices = c('TOMATO','HEMP','SUNFLOWER','WINTER WHEAT'), 
                  selected = 'HEMP', 
                  multiple = FALSE,
                  selectize = TRUE),
      
      sliderInput(inputId="choose_temperature", label='Temperature', min = 0, max = 50, value = 24, step = 0.5, width='100%'),
   
      sliderInput(inputId="choose_concentration", label='Carbon dioxide concentration', min=100, max=2000, value=415, step = 5, width='100%'),

      sliderInput(inputId="choose_light", label='Light intensity', min=50, max=2000, value=700, step = 50, width='100%'),
     
      hr(),
    
      plotOutput(outputId = "PhotosynthesisGauge",width = "100%",height='80px')
    
     ),
  
   mainPanel(
     
     tabsetPanel(selected = 'PAR effect',
      
       # tabPanel("Gauge",plotOutput(outputId = "PhotosynthesisGauge",width = "15%", height = "300px")),
       tabPanel("PAR effect", 
                br(),
                mainPanel(
                    div(
                    style = "position:relative",
                    plotOutput(outputId = "PhotosynthesisPlotWithLight",width = "95%", height = "420px",
                               hover=hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
                    uiOutput("hover_info_light")
                    )
                ),
                sidebarPanel(width = 4,
                  numericInput(inputId='steps_PAR', label= 'Discretisation :', value=39, min = 10, max = 2500, step = 10,
                               width = NULL),
                  numericInput(inputId='min_PAR', label= 'Minimum :', value=39, min = 0, max = 2500, step = 10,
                               width = NULL),
                  numericInput(inputId='max_PAR', label= 'Maximum :', value=2496, min = 0, max = 2500, step = 10,
                               width = NULL),
                  tags$style(type = "text/css","#steps_PAR.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type = "text/css","#min_PAR.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type = "text/css","#max_PAR.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type="text/css", "#Download_PvsPAR {border: 2px solid #00AA00;font-size: 125%;}"),
                  downloadButton('Download_PvsPAR','Download data')
                )
                ),
       
       tabPanel("Temperature effect",
                br(),
                mainPanel(
                  br(),
                  div(
                    style = "position:relative",
                    plotOutput(outputId = "PhotosynthesisPlotWithTemp",width = "95%", height = "420px",
                             hover=hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
                    uiOutput("hover_info_temp")
                  )
                ),
                sidebarPanel(width = 4,
                  numericInput(inputId='steps_TEMP', label= 'Discretisation :', value=1, min = 0, max = 40, step = 1,
                               width = NULL),
                  numericInput(inputId='min_TEMP', label= 'Minimum :', value=0, min = 0, max = 40, step = 1,
                               width = NULL),
                  numericInput(inputId='max_TEMP', label= 'Maximum :', value=40, min = 0, max = 40, step = 1,
                               width = NULL),
                  tags$style(type = "text/css","#max_TEMP.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type = "text/css","#min_TEMP.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type = "text/css","#steps_TEMP.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type="text/css", "#Download_PvsTEMP {border: 2px solid #00AA00;font-size: 125%;}"),
                  downloadButton('Download_PvsTEMP','Download data')
                )
                ),
       
       tabPanel("CO2 effect",
                br(),
                mainPanel(
                  div(
                    style="position:relative",
                    plotOutput(outputId = "PhotosynthesisPlotWithCO2",width = "95%", height = "420px",
                               hover=hoverOpts(id= "plot_hover", delay = 100, delayType = 'debounce')),
                    uiOutput("hover_info_co2")
                  )
                ),
                sidebarPanel(width=4,
                  numericInput(inputId='steps_CO2', label= 'Discretisation :', value=10, min = 0, max = 1000, step = 10,
                               width = NULL),
                  numericInput(inputId='min_CO2', label= 'Minimum :', value=0, min = 0, max = 1000, step = 10,
                               width = NULL),
                  numericInput(inputId='max_CO2', label= 'Maximum :', value=1000, min = 0, max = 1000, step = 10,
                               width = NULL),
                  tags$style(type = "text/css","#steps_CO2.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type = "text/css","#min_CO2.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type = "text/css","#max_CO2.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                  tags$style(type="text/css", "#Download_PvsCO2 {border: 2px solid #00AA00;font-size: 125%;}"),
                  downloadButton('Download_PvsCO2','Download data')
                )
       ),

       tabPanel("Gain", br(),plotOutput(outputId = "Gains",width = "80%", height = "400px")),
       
       tabPanel("QPAR Model - Synthesis for multiple species",     
                
                br(),
                mainPanel(
                  div(
                    style="position:relative",
                    plotOutput(outputId = "All_Species_PAR",width = "95%", height = "420px")
                    
                  )
                ),
                sidebarPanel(width=4,
                             numericInput(inputId='steps_PARall', label= 'Discretisation :', value=39, min = 10, max = 2500, step = 10,
                                          width = NULL),
                             numericInput(inputId='min_PARall', label= 'Minimum :', value=39, min = 0, max = 2500, step = 10,
                                          width = NULL),
                             numericInput(inputId='max_PARall', label= 'Maximum :', value=2496, min = 0, max = 2500, step = 10,
                                          width = NULL),
                             tags$style(type = "text/css","#steps_PARall.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                             tags$style(type = "text/css","#min_PARall.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                             tags$style(type = "text/css","#max_PARall.form-control.shiny-bound-input {background-color: white;border: 2px solid #00AA00;color:#00AA00;}"),
                             tags$style(type="text/css", "#Download_PvsPARall {border: 2px solid #00AA00;font-size: 125%;}"),
                             downloadButton('Download_PvsPARall','Download data')
                )       
                
       )
       
       
     )# End tabsetPanel
     
   ) # end mainPanel
  
)# End fluidpage


# Define server logic required :
server <- function(input, output) {
  

  # Load parameters value ----
  Parameters <- read.table('./data/Parameters.csv', 
                          sep=";",                                           
                          header=TRUE,dec=".",
                          row.names=NULL,
                          as.is=FALSE, 
                          fill=TRUE)
   
  # Load loess final model and Data : 
  load('./data/Species_Model.RData')
  
  
  
  output$All_Species_PAR <- renderPlot(
    {
    ggplot(data=All_Species_Effect_Of_Temperature,
           aes(x=PPFD,y=value,color=variable))+
      geom_line(size=0.9)+
      labs(x='PPFD (umol/m².s)',y='Normalised photosyntheis (%)')+
      theme_light()+
      scale_x_continuous(limits=c(0,2500),breaks=seq(0,3000,250))+
      geom_hline(yintercept = 1,color='black')+
      scale_y_continuous(limits=c(0,1.1),breaks=seq(0,1,0.1))+
      scale_colour_discrete("Species :")+
      geom_line(data=All_Species_Effect_Of_Temperature,
                aes(x=PPFD,y=Final_Loess_Photosynthesis),
                color='black',size=1.5,se=F)+
      theme(panel.background = element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            axis.text = element_text(size=10),
            axis.title = element_text(size=14),
            title = element_text(size=14),
            legend.position = 'bot'
      )+
      
      #  LOW - GOOD - OPTIMAL - HIGH - CAUTION 
      
      geom_text(aes(x=100,y=1.07,label='LOW'),color='red')+
      geom_text(aes(x=400,y=0,label='@300'),color='black')+
      geom_vline(xintercept = 300,color='black',size=0.5)+
      
      geom_text(aes(x=600,y=1.07,label='GOOD'),color='orange')+
      geom_text(aes(x=1000,y=0,label='@900'),color='black')+
      geom_vline(xintercept = 900,color='black',size=0.5)+
      
      geom_text(aes(x=1200,y=1.07,label='OPTIMAL'),color='green')+
      geom_text(aes(x=1600,y=0,label='@1600'),color='black')+
      geom_vline(xintercept = 1500,color='black',size=0.5)+
      
      geom_text(aes(x=1800,y=1.07,label='HIGH'),color='orange')+
      geom_text(aes(x=2200,y=0,label='@2100'),color='black')+
      geom_vline(xintercept = 2100,color='black',size=0.5)+
      
      geom_text(aes(x=2300,y=1.07,label='CAUTION'),color='red')
    }
  )
  
  
  observe({
    
    
    # Simulate photosynthesis ----
    Photosynthesis <- function(Spe,PPFD,CO2,Temp,Parameters) 
    {
      if (Spe!='HEMP')
      {
        
        if (Spe=='SUNFLOWER')
        {
          alpha <- 26.5
          gamma <- 201
          kappa <- 0.0012
          tem_zero <- 30
        }
        else if (Spe=='WINTER WHEAT'){
          alpha <- 25.1
          gamma <- 306
          kappa <- 0.0015
          tem_zero <- 24
        }
        else {
          alpha <- Parameters$alpha_tom 
          gamma <- Parameters$gamma_tom
          kappa <- Parameters$kappa_tom
          tem_zero <- Parameters$tem_zero_tom
        }
        
        PPFDeffect <- ((alpha*PPFD) / (gamma + PPFD))
        Tempeffect <- ( 1 - (kappa *  ((Temp -tem_zero)^2) ) )
        Respi <- (Parameters$a_respi_tom + Parameters$b_respi_tom * Temp + Parameters$c_respi_tom * Temp^2)
        CO2effect <- Parameters$a_CO2_tom *  (Parameters$b_CO2_tom)^(CO2/Parameters$d_CO2_tom) + Parameters$c_CO2_tom
        Pn <- (PPFDeffect * Tempeffect - Respi)*CO2effect
        
      }
      
      if (Spe=='HEMP')
      {
        
        CO2effect <- (Parameters$a_CO2 * exp( Parameters$b_CO2 * exp( Parameters$c_CO2 * CO2))) - exp (Parameters$d_CO2 + Parameters$e_CO2 * CO2)
        Pmax <- Parameters$Pmax_a + (Parameters$Pmax_b * Temp) + (Parameters$Pmax_c * Temp^2)
        PI <- Parameters$PI_a + (Parameters$PI_b * Temp) + Parameters$PI_c * Temp^2 
        Pn <- ((Pmax * (1 - exp(-PPFD/PI))* exp(-PPFD/PI))-Parameters$R) * CO2effect    
        
      }
      
      return(Pn)
    }
    
    # Max values : 
    Names <- c('HEMP','TOMATO','SUNFLOWER','WINTER WHEAT')
    Maxes <- c(57,36,42,36)
    MAX <-Maxes[which(Names==input$chosen_specie)]
    
    ## Light effect : 
    PPFD <<- seq(0,2500,1)
    PnWithLight <<- Photosynthesis(input$chosen_specie,PPFD,input$choose_concentration,input$choose_temperature,Parameters) 
    ToPlotLight <<- data.frame('PPFD'=PPFD,'Pn'=PnWithLight)
    ToPlotLight$factor <<- ToPlotLight$Pn/max(ToPlotLight$Pn)
    ToPlotLight$GainLight <- c(diff(ToPlotLight$factor),0)*100
    ## CO2 effect : 
    CO2bis <<- seq(0,2000,1)
    PnWithCO2 <<- Photosynthesis(input$chosen_specie,input$choose_light,CO2bis,input$choose_temperature,Parameters) 
    PnWithCO2[which(PnWithCO2<0)] <<- 0
    ToPlotCO2 <<- data.frame('CO2'=CO2bis,'Pn'=PnWithCO2)
    ToPlotCO2$factor <<- ToPlotCO2$Pn/max(ToPlotCO2$Pn)
    ToPlotCO2$GainCO2 <<- c(diff(ToPlotCO2$factor),0)*100
    ## Temp effect : 
    Temp <<- seq(0,50,0.1)
    PnWithTemp <<- Photosynthesis(input$chosen_specie,input$choose_light,input$choose_concentration,Temp,Parameters) 
    PnWithTemp[which(PnWithTemp<0)] <<- 0
    ToPlotTemp <<- data.frame('Temp'=Temp,'Pn'=PnWithTemp)    
    ToPlotTemp$factor <<- ToPlotTemp$Pn/max(ToPlotTemp$Pn)
    ToPlotTemp$GainTemp <<- c(diff(ToPlotTemp$factor),0)*100
    # Pn Gauge : 
    PnToPlot <<- Photosynthesis(input$chosen_specie,input$choose_light,input$choose_concentration,input$choose_temperature,Parameters)
    PnToPlot[which(PnToPlot<0)] <<- 0.1
    ToPlotPn <<- data.frame('Pn'= PnToPlot) 
    ValueOfPn <<- as.character(round(ToPlotPn/MAX*100,digits=0))
    xmax <<- seq(-0.5,ToPlotPn$Pn,0.1)
    Rects <<- data.frame(ymin = 0.05, ymax= 0.95 , xmin = (xmax-0.1)/MAX, xmax = (xmax)/MAX)
    
    # Data to show : 
    if ( (is.na(input$min_TEMP)!=1) && (is.na(input$max_TEMP)!=1) && (is.na(input$steps_TEMP)!=1) )
    {
      Disc_TEMP <<- seq(input$min_TEMP,input$max_TEMP,input$steps_TEMP)
    } else {
      Disc_TEMP <<- seq(0,30,0.5)
    }
    
    if ( (is.na(input$min_PAR)!=1) && (is.na(input$max_PAR)!=1) && (is.na(input$steps_PAR)!=1) )
    {
      Disc_PAR <<- seq(input$min_PAR,input$max_PAR,input$steps_PAR)
    } else {
      Disc_PAR <<- seq(0,2500,100)
    }
    
    if ( (is.na(input$min_CO2)!=1) && (is.na(input$max_CO2)!=1) && (is.na(input$steps_CO2)!=1) )
    {
      Disc_CO2 <<- seq(input$min_CO2,input$max_CO2,input$steps_CO2)
    } else {
      Disc_CO2 <<- seq(0,1000,100)
    }
    
    # Data to show : 
    if ( (is.na(input$min_PARall)!=1) && (is.na(input$max_PARall)!=1) && (is.na(input$steps_PARall)!=1) )
    {
      Disc_PARall <<- seq(input$min_PARall,input$max_PARall,input$steps_PARall)
    } else {
      Disc_PARall <<- seq(0,2500,100)
    }
    
    Disc_C02 <<- seq(input$min_CO2,input$max_CO2,input$steps_CO2)
    Disc_PAR <<- seq(input$min_PAR,input$max_PAR,input$steps_PAR)
    Disc_PARall <<- seq(input$min_PARall,input$max_PARall,input$steps_PARall)
    ToDownloadTemp <<- Photosynthesis(input$chosen_specie,input$choose_light,input$choose_concentration,Disc_TEMP,Parameters)
    ToDownloadPAR <<- Photosynthesis(input$chosen_specie,Disc_PAR,input$choose_concentration,input$choose_temperature,Parameters)
    ToDownloadPARall <<- predict(Loess_Photosynthesis,Disc_PARall)
    ToDownloadCO2 <<- Photosynthesis(input$chosen_specie,input$choose_light,Disc_C02,input$choose_temperature,Parameters)
    ToDownloadTemp[which(ToDownloadTemp<0)] <<- 0
    ToDownloadPAR[which(ToDownloadPAR<0)] <<- 0
    ToDownloadPARall[which(ToDownloadPARall<0)] <<- 0
    ToDownloadCO2[which(ToDownloadCO2<0)] <<- 0
    df_DL_Temp <<- data.frame('Temp'=Disc_TEMP,'Pn'=ToDownloadTemp)    
    df_DL_PAR <<- data.frame('PAR'=Disc_PAR,'Pn'=ToDownloadPAR)    
    df_DL_CO2 <<- data.frame('CO2'=Disc_C02,'Pn'=ToDownloadCO2)    
    df_DL_PARall <<- data.frame('PAR'=Disc_PARall,'Pn'=ToDownloadPARall)    
    
    # Max values : 
    Names <- c('HEMP','TOMATO','SUNFLOWER','WINTER WHEAT')
    Maxes <- c(57,36,42,36)
    MAX <-Maxes[which(Names==input$chosen_specie)]
    
    output$PhotosynthesisPlotWithLight <- renderPlot(
    {
      
      ggplot(data=ToPlotLight,aes(x=PPFD,y=Pn))+
        geom_line(size=1.5,color="#00aa00")+
        scale_y_continuous(expand = c(0, 0),limits=c(-3,55))+
        scale_x_continuous(expand = c(0, 10),limits=c(0,2500))+
        theme_light()+
        theme(
          panel.border = element_rect(colour = "black", fill=NA, size=1),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size=12),
          axis.title=element_text(size=15)
        )+
        labs(x=bquote('Light ('*mu~ 'mol' ~ m^-2~s^-1*')'),y=bquote('Photosynthesis ('*mu~ 'mol'~CO[2] ~ m^-2~s^-1*')'))
      
    })

    output$PhotosynthesisPlotWithCO2 <- renderPlot(
    {
        
        ggplot(data=ToPlotCO2,aes(x=CO2,y=Pn))+
          geom_line(size=1.5,color="#00aa00")+
          scale_y_continuous(expand = c(0, 0),limits=c(-3,55))+
          scale_x_continuous(expand = c(0, 10),limits=c(0,2000))+
          theme_light()+
          theme(
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size=12),
            axis.title=element_text(size=15)
          )+
          labs(x=bquote('Carbon dioxide ('*mu~ 'mol' ~ m^-2~s^-1*')'),y=bquote('Photosynthesis ('*mu~ 'mol'~CO[2] ~ m^-2~s^-1*')'))
        
      })

    output$PhotosynthesisPlotWithTemp <- renderPlot(
    {
        
        ggplot(data=ToPlotTemp,aes(x=Temp,y=Pn))+
          geom_line(size=1.5,color="#00aa00")+
          scale_y_continuous(expand = c(0, 0),limits=c(-3,55))+
          scale_x_continuous(expand = c(0, 0),limits=c(0,50))+
          theme_light()+
          theme(
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size=12),
            axis.title=element_text(size=15)
          )+
          labs(x=bquote('Temperature ('*degree ~'C )'),y=bquote('Photosynthesis ('*mu~ 'mol'~CO[2] ~ m^-2~s^-1*')'))
        
      })
    
    output$PhotosynthesisGauge <- renderPlot(
    {
        
      p <- ggplot(Rects, aes(ymax = ymax, ymin = ymin, xmax = xmax, xmin = xmin,fill=xmax)) +
                  geom_rect()+
                  geom_rect(aes(ymax=0.95, ymin=0.05, xmax=1, xmin=0),alpha=0.2,fill=NA,color='black',size=1) +
                  scale_x_continuous(limits=c(0,1))+
                  geom_text(aes(x = max(Rects$xmax)-0.05, y = 0.5, label = ValueOfPn), colour="black", size=6.5) +
                  theme_void()+
                  scale_fill_gradient2(limits=c(0,1),midpoint=0.5,low='red',mid='#00AA00',high='#00AA00')+
                  guides(fill=FALSE) +
                  guides(colour=FALSE)+
                  labs(title="Potential photosynthesis (%)")+
                  theme(plot.title = element_text(hjust = 0.5, margin=margin(0,0,20,0)))

      print(p)
    
      })
   
    output$Gains <- renderPlot(
    {
        
        Seq <- seq(0,2000,100)
        ToPlotHist <- ToPlotCO2[ToPlotCO2$CO2 %in% Seq,]
        GainCO2 <- ggplot(data=ToPlotHist,aes(x=CO2,y=GainCO2))+
          geom_histogram(size=2.5,stat='identity')+
          geom_line(color="#00aa00",size=0.5)+
          geom_point(color="#00aa00",size=3)+
          scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,250))+
          geom_vline(xintercept=input$choose_concentration,color="#00aa00",size=1.5)+
          theme(
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size=12),
            axis.title=element_text(size=15)
          )+
          expand_limits(x=0,y=0)+
          labs(x=bquote('Carbon dioxide ('*mu~ 'mol' ~ m^-2~s^-1*')'),y='Gain (%)')
          
        Seq <- seq(50,2000,100)
        ToPlotHist <- ToPlotLight[ToPlotLight$PPFD %in% Seq,]
        GainLight <- ggplot(data=ToPlotHist,aes(x=PPFD,y=GainLight))+
          geom_histogram(size=2.5,stat='identity')+
          geom_line(color="#00aa00")+
          geom_point(color="#00aa00",size=2.5)+
          scale_x_continuous(limits=c(50,2000),breaks=seq(0,2000,200))+
          geom_vline(xintercept=input$choose_light,color="#00aa00",size=1.5)+
          theme(
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size=12),
            axis.title=element_text(size=15)
          )+
          expand_limits(x=0,y=0)+
          labs(x=bquote('Light ('*mu~ 'mol' ~ m^-2~s^-1*')'),y='Gain (%)')          

        Seq <- seq(0,50,2.5)
        ToPlotHist <- ToPlotTemp[ToPlotTemp$Temp %in% Seq,]
        GainTemp <- ggplot(data=ToPlotHist,aes(x=Temp,y=GainTemp))+
          geom_histogram(size=2.5,stat='identity')+
          geom_line(color="#00aa00")+
          geom_point(color="#00aa00",size=2.5)+
          scale_x_continuous(limits=c(0,50),breaks=seq(0,50,5))+
          geom_vline(xintercept=input$choose_temperature,color="#00aa00",size=1.5)+
          theme(
            panel.border = element_rect(colour = "black", fill=NA, size=1),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size=12),
            axis.title=element_text(size=15)
          )+
          expand_limits(x=0,y=0)+
          labs(x=bquote('Temperature ('*degree ~'C )'),y='Gain (%)')  
        
        Grid <- plot_grid(nrow=3,GainTemp,GainCO2,GainLight)
        
        suppressWarnings(print(Grid))
        
        
      })
    
    
    output$hover_info_temp <- renderUI({
      hover <- input$plot_hover
      point <- nearPoints(ToPlotTemp, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Temperature :</b>", round(point$Temp,digits=2), "<br/>",
                      "<b> Photosynthesis : </b>", round(point$Pn,digits=2), "<br/>")))
      )
    })
    
    output$hover_info_co2 <- renderUI({
      hover <- input$plot_hover
      point <- nearPoints(ToPlotCO2, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> CO2 :</b>", round(point$CO2,digits=2), "<br/>",
                      "<b> Photosynthesis : </b>", round(point$Pn,digits=2), "<br/>")))
      )
    })
    
    output$hover_info_light <- renderUI({
      hover <- input$plot_hover
      point <- nearPoints(ToPlotLight, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
      if (nrow(point) == 0) return(NULL)
      
      # calculate point position INSIDE the image as percent of total dimensions
      # from left (horizontal) and from top (vertical)
      left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
      top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
      
      # calculate distance from left and bottom side of the picture in pixels
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
      
      # create style property fot tooltip
      # background color is set so tooltip is a bit transparent
      # z-index is set so we are sure are tooltip will be on top
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", left_px + 2, "px; top:", top_px + 2, "px;")
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        p(HTML(paste0("<b> Light :</b>", round(point$PPFD,digits=2), "<br/>",
                      "<b> Photosynthesis : </b>", round(point$Pn,digits=2), "<br/>")))
      )
    })
    
    output$Download_PvsTEMP <- downloadHandler(
      filename = function(){
        filename = "Photosynthesis_with_TEMP.csv"
        },
      content = function(file){
        write.csv2(df_DL_Temp,file,row.names=FALSE,sep=';')
      }
    )
      
    output$Download_PvsPAR <- downloadHandler(
      filename = function(){
        filename = "Photosynthesis_with_PAR.csv"
      },
      content = function(file){
        write.csv2(df_DL_PAR,file,row.names=FALSE)
      }
    )
    
    output$Download_PvsCO2 <- downloadHandler(
      filename = function(){
        filename = "Photosynthesis_with_CO2.csv"
      },
      content = function(file){
        write.csv2(df_DL_CO2,file,row.names=FALSE)
      }
    )
      
    output$Download_PvsPARall <- downloadHandler(
      filename = function(){
        filename = "Photosynthesis_with_PAR_GlobalModel.csv"
      },
      content = function(file){
        write.csv2(df_DL_PARall,file,row.names=FALSE)
      }
    )      
  })
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)


