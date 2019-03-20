library(shiny)
library(shinythemes)
library(radarchart)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(plotly)

# Inicialization -----------------
comorbilidades <- c("infarct", "HeartFailure", "VascularDisease", "CerebroDisease", "Dementia", "PulmonaryDisease", "ConnectiveDisease", "Liver_grouped",
                    "Diabetes", "RenalDisease")

comorbilidades_labels <- c("Myocardial infarct", "Congestive heart failure", "Peripheral vascular disease", "Cerebrovascular disease",
                           "Dementia", "Chronic obstructive pulmonary disease", "Rheumatologic disease", "Liver disease",
                           "Diabetes", "Renal disease")

comorbilidades_romanos <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "XI")

top5 <- c("Diabetes", "PulmonaryDisease", "HeartFailure",
          "VascularDisease", "ConnectiveDisease")

top5_labels <- c("Diabetes", "Chronic obstructive pulmonary disease", "Congestive heart failure", 
                 "Peripheral vascular disease", "Rheumatologic disease")

top5_romanos <- c("IX", "VI", "II", "III", "VII")

url <- "https://twitter.com/intent/tweet?url=http://watzilei.com/shiny/CoMCoR/&via=WATZILEI"

# ui ------------------------------
ui <- fluidPage(theme = shinytheme("cosmo"),
      titlePanel(tags$b("Pattern of Comorbidities and Multimorbidity among Colorectal Cancer Patients in Spain: CoMCoR study."),
                 windowTitle = "CoMCoR"),
      
      tabsetPanel(
        # Tab 1 - Introduction --------------------
        tabPanel(h4(tags$b("Abstract")),
                 
                 br(),
                 fluidRow(
                     column(7, wellPanel(tags$b("Cite as:"),br(),
                           "Miguel Angel Luque-Fernandez, Daniel Redondo Sánchez et al., 2018.", br(),
                           tags$i("Pattern of Comorbidities and Multimorbidity among Colorectal Cancer Patients in Spain: CoMCoR study."), br(),
                           "Biomedical Research Institute of Granada, Non‐Communicable and Cancer Epidemiology Group (ibs.Granada), University of Granada.")),
                     column(5, wellPanel(tags$b("E-mail us"),  "with your comments:", br(),
                                         tags$i("miguel.luque.easp at juntadeandalucia.es")
                                         ),
                            tags$div(actionButton("twitter",
                                                  label = "Share on Twitter",
                                                  icon = icon("twitter"),
                                                  style="color: #fff; background-color: #00ACED; border-color: #00ACED",
                                                  onclick = sprintf("window.open('%s')", url)
                            ), style = "text-align:center"))
                 ),
                 
                 br(),
                 
                 fluidRow(
                     column(7, 
                            h3(tags$b("Background:")),
                            "Cancer accounted for 8.8 million deaths globally in 2015 and was the second most common cause of death in the world.[1] Colorectal (CRC) cancer is the
                            second most frequently diagnosed cancer in Spain with 44,937 estimated new CRC cases in 2019.[2] Despite the high prevalence
                            of colorectal cancer in the elderly population, the inclusion of this cohort in clinical trials is disproportionately low. Besides clinical
                            and pathological characteristics of the tumour, also general health status and comorbidities can influence cancer treatment and outcomes.
                            Comorbidity and multimorbidity are increasingly seen as a problem of the elderly.[3, 4] A number of studies have been performed analysing the influence of age and comorbidity on cancer outcomes,
                            but little evidence is available regarding the frequency and distribuion of comorbidities and multimorbidity at a population level among colorectal cancer patients in Spain.",
                            
                            h3(tags$b("Methods:")),
                            "We developed a population-based high-resolution cohort study, including all CRC incident cases diagnosed in 2011 (n= 1,061). Data were drawn from 
                            two population cancer registries and patient’s medical records. We defined comorbidity as the existence of a long-term health condition or disorder 
                            in the presence of cancer, whereas multimorbidity refers to the existence of two or more comorbid conditions [5, 6]. We described the frequency and distribution
                            of comorbidities and multimorbidity by patient, tumor and healthcare factors using radar-plots and heatmaps. Then, we used generalized linear models 
                            to characterize the factors associated with the presence of the most prevalent comorbidities plus dementia and multimorbidity. We used forest plots 
                            to display the results.",
                            
                            h3(tags$b("Results:")),
                            "The most common comorbidities were diabetes (23.6%), chronic obstructive pulmonary disease (17.2%) and congestive heart failure (14.5%). Dementia 
                            was the most common comorbidity among older patients (75+ years) showing a higher proportion (30%) of late cancer diagnosis (stage IV) and hospital 
                            emergency admission (33%). CRC with dementia had nearly three times higher prevalence of not receiving surgery treatment (RR: 2.8, 95%CI: 1.6, 5.0). 
                            Older (+75 years) obese male and current smoker, late surgery after cancer diagnosis (more than 60 days) and not being offered surgical treatment after 
                            were associated with a higher prevalence of multimorbidity. Patients with multimorbidity (2 or more comorbidities) aged 75+ years showed a higher prevalence 
                            of surgery the same day or the day after hospital emergency admission (37%).",
                            
                            h3(tags$b("Conclusions:")),
                            "CoMCoR found a relevant pattern in the distribution and frequency of comorbidities and multimorbidity among CRC patients in Spain. CRC frequency of late diagnosis (stage IV) among patients with dementia and the high proportion 
                            of older patients not being offered surgical treatment are important findings that require policy actions. All the results from CoMCoR are made open-source 
                            available in a web application which is meant to serve as a scientific tool supporting evidence-based policymaking aiming to improve comorbid CRC patients' outcomes.",
                            
                            hr(),
                            
                            h4(tags$b("References:")),
                            "1.	World Health Organization. 2017. Cancer [Online]. Available: http://www.who.int/cancer/en/ [Accessed 30 October 2017].",
                            br(),
                            "2.	Galceran, J., et al., Cancer incidence in Spain, 2015. Clin Transl Oncol, 2017. 19(7): p. 799-825.",
                            br(),
                            "3.	Macleod, U. and E. Mitchell, Comorbidity in general practice. Practitioner, 2005. 249(1669): p. 282-4.",
                            br(),
                            "4.	Macleod, U., et al., Comorbidity and socioeconomic deprivation: an observational study of the prevalence of comorbidity in general practice.
                            Eur J Gen Pract, 2004. 10(1): p. 24-6.",
                            br(),
                            "5.	Porta, M.S., et al., A dictionary of epidemiology. Sixth edition / ed. 2014, Oxford: Oxford University Press. xxxii, 343 pages.",
                            br(),
                            "6.	Lujic, S., et al., Multimorbidity in Australia: Comparing estimates derived using administrative data sources and survey data. PLoS One, 2017. 12(8):
                            p. e0183817."
                            
                            , style = "text-align: justify"),
                 
                 column(5,
                        tags$div(tags$img(src = "logo.png", width = "65%"), style = "text-align:center")
                  )
                 )
                 
        ),
                 
        # Tab 2 - Radar plots --------------------
        tabPanel(h4(tags$b("Pattern of comorbidities: radar plots")),
                 h3(tags$b("Pattern of comorbidities: radar plots")),
                 sidebarLayout(
                   sidebarPanel(width = 4,
                                radioButtons(inputId = "eleccion_com",
                                             label = h3("Comorbidities:"),
                                             choices = c( "Top 10" = FALSE, "Top 5" = TRUE),
                                             selected = TRUE
                                             ),
                                selectInput(inputId = "by",
                                            label = h3("Results by:"),
                                            choices = c("Patient's factors" = "Patient",
                                                        "Tumor factors" = "Tumor",
                                                        "Healthcare factors" = "Healthcare"),
                                            selected = "Patient"
                                            ),
                                conditionalPanel("input.by == 'Patient'",{
                                  radioButtons(inputId = "patient",
                                               label =  NULL,
                                               choices = c("Sex" = "e_sexo",
                                                         "Age" = "e_edad",
                                                         "Region" = "e_region",
                                                         "Performance" = "e_performance",
                                                         "Smoking" = "e_smoking",
                                                         "BMI" = "e_bmi"),
                                              selected = "e_sexo"
                                              )
                                }),
                                conditionalPanel("input.by == 'Tumor'",{
                                  radioButtons(inputId = "tumor",
                                               label =  NULL,
                                               choices = c("Location" = "e_site",
                                                           "Grade" = "e_grade",
                                                           "Stage" = "e_stage"),
                                               selected = "e_site"
                                  )
                                }),
                                conditionalPanel("input.by == 'Healthcare'",{
                                  radioButtons(inputId = "healthcare",
                                               label =  NULL,
                                               choices = c("Type of hospital admission" = "e_hospital",
                                                           "Surgery" = "e_surgery",
                                                           "Type of surgery" = "e_typesurgery",
                                                           "Time to surgery" = "e_timesurgery"),
                                               selected = "e_hospital"
                                  )
                                })
                              
                   ),
                   mainPanel(
                     tags$div(tags$h4("Distribution (percentage) of comorbidities prevalence by factors selected in the sidebar menu"),
                              style = "text-align:center;"),
                     conditionalPanel("input.by == 'Patient'", chartJSRadarOutput("radarplot_patient", width = "450", height = "250")),
                     conditionalPanel("input.by == 'Tumor'", chartJSRadarOutput("radarplot_tumor", width = "450", height = "250")),
                     conditionalPanel("input.by == 'Healthcare'", chartJSRadarOutput("radarplot_healthcare", width = "450", height = "250"))

                     )
                   )
                 ),
        
        # Tab 3 - Heatmaps --------------------
        tabPanel(h4(tags$b("Cluster of comorbidities: heatmaps")),
                 h3(tags$b("Cluster of comorbidities: heatmaps")),
                 sidebarLayout(
                   sidebarPanel(width = 4,
                                radioButtons(inputId = "h_eleccion_com",
                                             label = h3("Comorbidities:"),
                                             choices = c( "Top 10" = FALSE, "Top 5" = TRUE),
                                             selected = TRUE
                                ),
                                selectInput(inputId = "h_by",
                                            label = h3("Results by:"),
                                            choices = c("Patient's factors" = "Patient",
                                                        "Tumor factors" = "Tumor",
                                                        "Healthcare factors" = "Healthcare"),
                                            selected = "Patient"
                                ),
                                
                                conditionalPanel("input.h_by == 'Patient'",{
                                  radioButtons(inputId = "h_patient",
                                               label =  NULL,
                                               choices = c("Sex" = "e_sexo",
                                                           "Age" = "e_edad",
                                                           "Region" = "e_region",
                                                           "Performance" = "e_performance",
                                                           "Smoking" = "e_smoking",
                                                           "BMI" = "e_bmi"),
                                               selected = "e_sexo"
                                  )
                                }),
                                conditionalPanel("input.h_by == 'Tumor'",{
                                  radioButtons(inputId = "h_tumor",
                                               label =  NULL,
                                               choices = c("Location" = "e_site",
                                                           "Grade" = "e_grade",
                                                           "Stage" = "e_stage"),
                                               selected = "e_site"
                                  )
                                }),
                                conditionalPanel("input.h_by == 'Healthcare'",{
                                  radioButtons(inputId = "h_healthcare",
                                               label =  NULL,
                                               choices = c("Type of hospital admission" = "e_hospital",
                                                           "Surgery" = "e_surgery",
                                                           "Type of surgery" = "e_typesurgery",
                                                           "Time to surgery" = "e_timesurgery"),
                                               selected = "e_hospital"
                                  )
                                }),
                                
                                # Legend
                                wellPanel(h4(tags$b("Legend")),
                                          "I = Myocardial infarct", br(), 
                                          "II = Congestive heart failure", br(), 
                                          "III = Peripheral vascular disease", br(), 
                                          "IV = Cerebrovascular disease", br(), 
                                          "V = Dementia", br(), 
                                          "VI = Chronic obstructive pulmonary disease", br(), 
                                          "VII = Rheumatologic disease", br(), 
                                          "VIII = Liver disease", br(), 
                                          "IX = Diabetes", br(), 
                                          "X =  Hemiplegia/Paraplegia", tags$i("(excluded from the analysis)"), br(),
                                          "XI = Renal disease", br(),
                                          "XII =  AIDS/HIV", tags$i("(excluded from the analysis)"), br()
                                          )

                   ),
                   mainPanel(
                     tags$div(tags$h4("Prevalence of comorbidities by factors selected in the sidebar menu:"),
                              style = "text-align:center;"),
                     conditionalPanel("input.h_by == 'Patient'", plotOutput("heatmap_patient", width = "100%")),
                     conditionalPanel("input.h_by == 'Tumor'", plotOutput("heatmap_tumor", width = "100%")),
                     conditionalPanel("input.h_by == 'Healthcare'", plotOutput("heatmap_healthcare", width = "100%"))
                     
                   )
                 )
        ),
        
#####                  # Tab 4 - Time to surgery --------------------
#####                  tabPanel(h4(tags$b("Comorbidities and time to surgery")),
#####                           h3(tags$b("Comorbidities and time from cancer diagnosis to surgical treatment")),
#####
#####                           # Both sexes
#####                           fluidRow(
#####                             column(7, plotlyOutput("graph_by_stage", width = "100%", height = "550px")),
#####                             column(5, 
#####                                    checkboxInput("heatselection", tags$b("Show heatmaps instead of tables"), value = FALSE),
#####                                    
#####                                    conditionalPanel("input.heatselection == false",
#####                                                     h4(tags$b("Stage I:  Median of days (IQR)")),
#####                                                     tableOutput("table_bothsexes_1"),
#####                                                     h4(tags$b("Stage II:  Median of days (IQR)")),
#####                                                     tableOutput("table_bothsexes_2"),
#####                                                     h4(tags$b("Stage III:  Median of days (IQR)")),
#####                                                     tableOutput("table_bothsexes_3"),
#####                                                     h4(tags$b("Stage IV:  Median of days (IQR)")),
#####                                                     tableOutput("table_bothsexes_4")
#####                                    ),
#####                                    
#####                                    conditionalPanel("input.heatselection == true",
#####                                                     h4(tags$b("Stage I:  Median of days")),
#####                                                     plotOutput("heatmap_bothsexes_1", width = "100%", height = "140px"),
#####                                                     h4(tags$b("Stage II:  Median of days")),
#####                                                     plotOutput("heatmap_bothsexes_2", width = "100%", height = "140px"),
#####                                                     h4(tags$b("Stage III:  Median of days")),
#####                                                     plotOutput("heatmap_bothsexes_3", width = "100%", height = "140px"),
#####                                                     h4(tags$b("Stage IV:  Median of days")),
#####                                                     plotOutput("heatmap_bothsexes_4", width = "100%", height = "140px")
#####                                    )
#####                                    
#####                                    )
#####                           ),
#####                          
#####                           hr()
#####                  ),
        
        # Tab 4 - Forest plot ---------------------------
        tabPanel(h4(tags$b("Comorbidities' Prevalence Ratios: forest plots")),
                 h3(tags$b("Comorbidities' Prevalence Ratios: forest plots")),
                 sidebarLayout(
                   sidebarPanel(width = 4,
                     selectInput(inputId = "forest.com",
                                 label = h3("Comorbidities (top 5 + dementia) and multimorbidity:"),
                                 choices = c("Rheumatic disease" = "ConnectiveDisease",
                                             "Dementia",
                                             "Diabetes",
                                             "Congestive heart failure" = "HeartFailure",
                                             "Chronic pulmonary disease" = "PulmonaryDisease",
                                             "Peripherical vascular disease" = "VascularDisease",
                                             "Multimorbidity" = "MM"),
                                 selected = "MM"
                                 ),
                     selectInput(inputId = "forest.fac",
                                 label = h3("Factors:"),
                                 choices = c("Patient's factors" = "pf",
                                             "Tumor factors" = "tf",
                                             "Healthcare factors" = "hf"),
                                 selected = "pf"
                                 ),
                     checkboxInput("adjusted", "Adjusted Prevalence Ratio by sex and age", value = FALSE)
                   ),
                   mainPanel(imageOutput("forestplot", height = "75%"))
                 )
        ),

        # Tab 5 - Authorship ----------------------
        tabPanel(h4(tags$b("Authorship & acknowledgments")),
                 
                 br(),
                 fluidRow(
                     column(7, wellPanel(tags$b("Cite as:"),br(),
                                         "Miguel Angel Luque-Fernandez, Daniel Redondo Sánchez et al., 2018.", br(),
                                         tags$i("Pattern of Comorbidities and Multimorbidity among Colorectal Cancer Patients in Spain: CoMCoR study."), br(),
                                         "Biomedical Research Institute of Granada, Non‐Communicable and Cancer Epidemiology Group (ibs.Granada), University of Granada.")),
                     column(5, wellPanel(tags$b("E-mail us"),  "with your comments:", br(),
                                         tags$i("miguel.luque.easp at juntadeandalucia.es")
                     ),
                     tags$div(actionButton("twitter",
                                           label = "Share on Twitter",
                                           icon = icon("twitter"),
                                           style="color: #fff; background-color: #00ACED; border-color: #00ACED",
                                           onclick = sprintf("window.open('%s')", url)
                     ), style = "text-align:center"))
                 ),
                 
                 br(),
                 
                 
                 h3(tags$b("Authorship")),
                 fluidRow(column(2, img(src = "logo_MALF.png", width = "100px")),
                          column(10, h4(tags$b("Miguel Angel Luque-Fernandez")),
                                 h4("Biomedical Research Institute of Granada", br(),
                                    "Non‐Communicable and Cancer Epidemiology Group (ibs.Granada)", br(),
                                    "University of Granada, Granada, Spain."),
                                 h4("Biomedical Network Research Centers of Epidemiology and Public Health (CIBERESP), ISCIII, Madrid, Spain"),
                                 h4("London School of Hygiene and Tropical Medicine, London, UK"),
                                 tags$i(h5("miguel.luque.easp at juntadeandalucia.es"))
                          )
                 ),
                 
                 hr(),
                 
                 fluidRow(column(2, img(src = "logo_DRS.png", width = "100px")),
                          column(10, h4(tags$b("Daniel Redondo Sánchez")),
                                 h4("Biomedical Research Institute of Granada", br(),
                                    "Non‐Communicable and Cancer Epidemiology Group (ibs.Granada)", br(),
                                    "University of Granada."),
                                 h4("Granada Cancer Registry, Andalusian School of Public Health"),
                                 h4("Biomedical Network Research Centers of Epidemiology and Public Health (CIBERESP), ISCIII, Madrid, Spain"),
                                 tags$i(h5("daniel.redondo.easp at juntadeandalucia.es"))
                          )
                 ),
                 
                 hr(),

                 fluidRow(column(2, img(src = "logo_MRB.jpg", width = "100px")),
                          column(10, h4(tags$b("Miguel Rodríguez Barranco")),
                                 h4("Biomedical Research Institute of Granada", br(),
                                    "Non‐Communicable and Cancer Epidemiology Group (ibs.Granada)", br(),
                                    "University of Granada, Granada, Spain."),
                                 h4("Granada Cancer Registry, Andalusian School of Public Health"),
                                 h4("Biomedical Network Research Centers of Epidemiology and Public Health (CIBERESP), ISCIII, Madrid, Spain"),
                                 tags$i(h5("miguel.rodriguez.barranco.easp at juntadeandalucia.es"))
                          )
                 ),
                 
                 hr(),
                 
                 fluidRow(column(2, img(src = "logo_MCG.jpg", width = "100px")),
                          column(10, h4(tags$b("Mª Carmen Carmona-García")),
                                 h4("Catalan Institute of Oncology"), 
                                 h4("University Hospital Dr Josep Trueta of Girona", br(),
                                    "Descriptive Epidemiology, Genetics and Cancer Prevention of the Biomedical Research Institute of Girona"),
                                 h4("University of Girona"),
                                 tags$i(h5("ccarmona at iconcologia.net & ccarmonag.girona.ics at gencat.cat"))
                          )
                 ),
                 
                 hr(),

                 fluidRow(column(2, img(src = "logo_RMG.jpg", width = "100px")),
                          column(10, h4(tags$b("Rafael Marcos Gragera")),
                                 h4("Catalan Institute of Oncology", br(),
                                    "Descriptive Epidemiology, Genetics and Cancer Prevention of the Biomedical Research Institute of Girona"),
                                 h4("University of Girona, Girona, Spain"),
                                 tags$i(h5("rmarcos at iconcologia.net"))
                          )
                 ),
                 
                 hr(),

                 fluidRow(column(2, img(src = "logo_MJSP.jpg", width = "100px")),
                          column(10, h4(tags$b("María José Sánchez Pérez")),
                                 h4("Biomedical Research Institute of Granada", br(),
                                    "Non‐Communicable and Cancer Epidemiology Group (ibs.Granada)", br(),
                                    "University of Granada, Granada, Spain."),
                                 h4("Granada Cancer Registry, Andalusian School of Public Health, Granada, Spain"),
                                 h4("Biomedical Network Research Centers of Epidemiology and Public Health (CIBERESP), ISCIII, Madrid, Spain"),
                                 tags$i(h5("mariajose.sanchez.easp at juntadeandalucia.es"))
                          )
                 ),
                 
                 hr(),

                 # Acknowledgment
                 h3(tags$b("Acknowledgment")),
                 tags$b("Funding information"), br(),
                 "Carlos III Institute of Health, Grant/Award Number: CP17/00206 and the Andalusian Department of Health, Grant Number: PI-0152/2017.", br(), br(),
                 fluidRow(column(5, img(src = "logofeder.png", width = "75%")),
                          column(5, img(src = "logoibs.png", width = "75%"),
                                 img(src = "logo_ciber.png", width = "50%"))
                          )
                 )
      )
)

server <- function(input, output) {

  load("www/shiny_database.RData")
  
  com_per <- function(var, categoria = "*", topfive = FALSE){
    salida <- c()
    
    if(topfive == FALSE){
      for(i in 1:10){
       datos <- subset(CR, CR[comorbilidades[i]] == "Yes")
        attach(datos, warn.conflicts = FALSE)
        salida <- cbind(salida, 100 * prop.table(table(eval(as.name(var)), eval(as.name(comorbilidades[i]))))[, 1])
        detach(datos)
      }
    } 
    
    if(topfive == TRUE){
      for(i in 1:5){
        datos <- subset(CR, CR[top5[i]] == "Yes")
        attach(datos, warn.conflicts = FALSE)
        salida <- cbind(salida, 100 * prop.table(table(eval(as.name(var)), eval(as.name(top5[i]))))[, 1])
        detach(datos)
      }
    } 

    if(categoria != "*") salida <- salida[categoria, ]
    return(round(salida,1))
  }
  
  # Ejemplos:
  #com_per("sex")
  #com_per("sex", 1)
  #com_per("estadio", 1, topfive = TRUE)
  
  # Radar plots -------------
  output$radarplot_patient<-renderChartJSRadar({
        scores <- switch(input$patient,
                       e_sexo = list(
                         "Men"   = com_per("sex", 1, topfive = input$eleccion_com),
                         "Women" = com_per("sex", 2, topfive = input$eleccion_com)
                       ),
                       e_edad = list(
                         "<55"   = com_per("edad4c", 1, topfive = input$eleccion_com),
                         "55-64" = com_per("edad4c", 2, topfive = input$eleccion_com),
                         "65-74" = com_per("edad4c", 3, topfive = input$eleccion_com),
                         ">75"   = com_per("edad4c", 4, topfive = input$eleccion_com)
                          ),
                       e_region = list(
                         "Granada" = com_per("REG", 1, topfive = input$eleccion_com),
                         "Girona"  = com_per("REG", 2, topfive = input$eleccion_com)
                         ),
                       e_performance = list(
                         "0 - Fully active" = com_per("ECOG", 1, topfive = input$eleccion_com),
                         "1 - Restricted in physically strenuous activity but ambulatory"  = com_per("ECOG", 2, topfive = input$eleccion_com),
                         "2 - Ambulatory and capable of all self-care but unable to carry out any work activities" = com_per("ECOG", 3, topfive = input$eleccion_com),
                         "3 - Capable of only limited self-care; confined to bed or chair more than 50% of waking hours"  = com_per("ECOG", 4, topfive = input$eleccion_com),
                         "4 - Completely disabled. Cannot carry on any self-care. Totally confined to bed or chair"  = com_per("ECOG", 5, topfive = input$eleccion_com)
                       ),
                       e_smoking = list(
                         "Yes, currently"  = com_per("Smoker", 1, topfive = input$eleccion_com),
                         "Yes, previously" = com_per("Smoker", 2, topfive = input$eleccion_com),
                         "No, never"       = com_per("Smoker", 3, topfive = input$eleccion_com)
                       ),
                       e_bmi = list(
                         "<24.9"   = com_per("BMIcat", 1, topfive = input$eleccion_com),
                         "25-29.9" = com_per("BMIcat", 2, topfive = input$eleccion_com),
                         ">=30"    = com_per("BMIcat", 3, topfive = input$eleccion_com)
                       )
                       
                       )
        
        if(input$eleccion_com == FALSE) labs <- comorbilidades_labels
        if(input$eleccion_com == TRUE) labs <- top5_labels
        
        chartJSRadar(scores = scores, labs = labs, labelSize = 18, showLegend = TRUE, maxScale = 100, colMatrix = grDevices::col2rgb(c("royalblue", "brown1", "chartreuse3", "darkgoldenrod1", "mediumorchid2")))
  
  })

  output$radarplot_tumor<-renderChartJSRadar({
    scores <- switch(input$tumor,
                     e_stage = list(
                       "I"   = com_per("estadio", 1, topfive = input$eleccion_com),
                       "II"  = com_per("estadio", 2, topfive = input$eleccion_com),
                       "III" = com_per("estadio", 3, topfive = input$eleccion_com),
                       "IV"  = com_per("estadio", 4, topfive = input$eleccion_com)
                       ),
                     e_grade = list(
                       "I"   = com_per("GICDO3", 1, topfive = input$eleccion_com),
                       "II"  = com_per("GICDO3", 2, topfive = input$eleccion_com),
                       "III" = com_per("GICDO3", 3, topfive = input$eleccion_com),
                       "IV"  = com_per("GICDO3", 4, topfive = input$eleccion_com)
                       ),
                     e_site = list(
                       "Right colon"        = com_per("site", 1, topfive = input$eleccion_com),
                       "Left colon"         = com_per("site", 2, topfive = input$eleccion_com),
                       "Rectal"             = com_per("site", 3, topfive = input$eleccion_com),
                       "Unspecified colon"  = com_per("site", 4, topfive = input$eleccion_com)
                     )
    )
    
    if(input$eleccion_com == FALSE) labs <- comorbilidades_labels
    if(input$eleccion_com == TRUE) labs <- top5_labels
    
    chartJSRadar(scores = scores, labs = labs, labelSize = 18, showLegend = TRUE, maxScale = 100, colMatrix = grDevices::col2rgb(c("royalblue", "brown1", "chartreuse3", "darkgoldenrod1", "mediumorchid2")))
    
  })
  
  output$radarplot_healthcare<-renderChartJSRadar({
    scores <- switch(input$healthcare,
                     e_hospital = list(
                       "Planned"   = com_per("Hospital", 1, topfive = input$eleccion_com),
                       "Emergency"  = com_per("Hospital", 2, topfive = input$eleccion_com)
                     ),
                     e_surgery = list(
                       "Yes"   = com_per("cirugia", 1, topfive = input$eleccion_com),
                       "No"  = com_per("cirugia", 2, topfive = input$eleccion_com)
                     ),
                     e_typesurgery = list(
                       "Major" = com_per("tipo_cirugia_radarplot", 1, topfive = input$eleccion_com),
                       "Minor"  = com_per("tipo_cirugia_radarplot", 2, topfive = input$eleccion_com)
                     ),
                     e_timesurgery = list(
                       "0 days (emergency)" = com_per("tiempo_cirugia_cat", 1, topfive = input$eleccion_com),
                       "1-13 days" = com_per("tiempo_cirugia_cat", 2, topfive = input$eleccion_com),
                       "14-30 days" = com_per("tiempo_cirugia_cat", 3, topfive = input$eleccion_com),
                       "31-59 days" = com_per("tiempo_cirugia_cat", 4, topfive = input$eleccion_com),
                       ">59 days" = com_per("tiempo_cirugia_cat", 5, topfive = input$eleccion_com)
                     )
    )
    
    if(input$eleccion_com == FALSE) labs <- comorbilidades_labels
    if(input$eleccion_com == TRUE) labs <- top5_labels
    
    chartJSRadar(scores = scores, labs = labs, labelSize = 18, showLegend = TRUE, maxScale = 100, colMatrix = grDevices::col2rgb(c("royalblue", "brown1", "chartreuse3", "darkgoldenrod1", "mediumorchid2")))
    
  })
  
  # Heatmaps ------------------
  output$heatmap_patient<-renderPlot({
    datos <- switch(input$h_patient,
                     e_sexo = com_per("sex", topfive = input$h_eleccion_com),
                     e_edad = com_per("edad4c", topfive = input$h_eleccion_com),
                     e_region = com_per("REG", topfive = input$h_eleccion_com),
                     e_performance = com_per("ECOG", topfive = input$h_eleccion_com),
                     e_smoking = com_per("Smoker", topfive = input$h_eleccion_com),
                     e_bmi = com_per("BMIcat", topfive = input$h_eleccion_com)
    )
    
    ifelse(input$h_eleccion_com == FALSE, colnames(datos) <- comorbilidades_romanos, colnames(datos) <- top5_romanos)
    datos <- melt(datos)
    
    ggplot(data = datos, aes(x = Var2, y = Var1)) +
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low = "white", high = "purple") +
      geom_text(size = 5, label = paste0(datos$value, "%")) +
      theme_minimal() + 
      xlab("Comorbidities") +
      ylab(NULL) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 18, face = "bold"))
  })
  
  
  output$heatmap_tumor <- renderPlot({
    datos <- switch(input$h_tumor,
                    e_site = com_per("site", topfive = input$h_eleccion_com),
                    e_grade = com_per("GICDO3", topfive = input$h_eleccion_com),
                    e_stage = com_per("estadio", topfive = input$h_eleccion_com)
    )
    
    ifelse(input$h_eleccion_com == FALSE, colnames(datos) <- comorbilidades_romanos, colnames(datos) <- top5_romanos)
    datos <- melt(datos)
    
    ggplot(data = datos, aes(x = Var2, y = Var1)) +
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low = "white", high = "purple") +
      geom_text(size = 5, label = paste0(datos$value, "%")) +
      theme_minimal() + 
      xlab("Comorbidities") +
      ylab(NULL) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 18, face = "bold")) 
  })
  
  output$heatmap_healthcare <- renderPlot({
    datos <- switch(input$h_healthcare,
                    e_hospital = com_per("Hospital", topfive = input$h_eleccion_com),
                    e_surgery = com_per("cirugia", topfive = input$h_eleccion_com),
                    e_typesurgery = com_per("tipo_cirugia_radarplot", topfive = input$h_eleccion_com),
                    e_timesurgery = com_per("tiempo_cirugia_cat", topfive = input$h_eleccion_com)
    )
    
    ifelse(input$h_eleccion_com == FALSE, colnames(datos) <- comorbilidades_romanos, colnames(datos) <- top5_romanos)
    datos <- melt(datos)
    
    ggplot(data = datos, aes(x = Var2, y = Var1)) +
      geom_tile(aes(fill = value)) +
      scale_fill_gradient(low = "white", high = "purple") +
      geom_text(size = 5, label = paste0(datos$value, "%")) +
      theme_minimal() + 
      xlab("Comorbidities") +
      ylab(NULL) +
      theme(axis.text = element_text(size = 16),
            axis.title = element_text(size = 18, face = "bold")) 
  })

  # Tables time to surgery ----------
  
#  crear.tabla <- function(stage, bysex = FALSE, sexo, showIQR = TRUE){
#    
#    ifelse(bysex == FALSE, data <- read.csv("www/NPgraphGG_bothsexes.csv"), data <- read.csv("www\\NPgraphGG_bygender.csv"))
#    data$edad4c <- factor(CR$edad4c)
#
#    data <- subset(data, data$restadio == stage & is.na(data$tsurg) == FALSE)
#    
#    if(bysex == TRUE) data <- subset(data, data$sex == sexo)
#    tabla <- matrix(0, nrow = 3, ncol = 4) %>% as.data.frame
#    data$catCom <- factor(data$catCom)
#    colnames(tabla) <- levels(data$edad4c)
#    rownames(tabla) <- c("No comorbidity", "One comorbidity", "Two or more comorbidities")
#    tabla
#    
#    for(i in 1:nrow(tabla)){
#      for(j in 1:ncol(tabla)){
#        aux <- subset(data, data$catCom == levels(data$catCom)[i] & data$edad4c == levels(data$edad4c)[j])
#        if(showIQR == TRUE) tabla[i, j] = paste0(round(quantile(aux$tsurg, 0.5, type = 2), 0), "(", round(IQR(aux$tsurg, type = 2), 0), ")")
#        if(showIQR == FALSE) tabla[i, j] = round(quantile(aux$tsurg, 0.5, type = 2), 0)
#      }
#    }
#    
#    tabla
#  }
#  
#  # Examples
#  #crear.tabla(stage = 1, bysex = FALSE)
#  #crear.tabla(stage = 4, bysex = TRUE, sex = "Male")
#  #crear.tabla(stage = 1, bysex = TRUE, sex = "Female")
#  
#  output$table_bothsexes_1 <- renderTable({
#    crear.tabla(1, FALSE)
#  }, rownames = TRUE)
#  
#  output$table_bothsexes_2 <- renderTable({
#    crear.tabla(2, FALSE)
#  }, rownames = TRUE)
#  
#  output$table_bothsexes_3 <- renderTable({
#    crear.tabla(3, FALSE)
#  }, rownames = TRUE)
#  
#  output$table_bothsexes_4 <- renderTable({
#    crear.tabla(4, FALSE)
#  }, rownames = TRUE)
#  
#  output$heatmap_bothsexes_1 <- renderPlot({
#    datos <- crear.tabla(1, showIQR = FALSE) %>% as.matrix %>% melt
#    
#    ggplot(data = datos, aes(x = Var2, y = Var1)) +
#      geom_tile(aes(fill = value)) +
#      scale_fill_gradient(low = "white", high = "purple") +
#      geom_text(size = 4, label = datos$value) +
#      theme_minimal() + 
#      xlab("Comorbidities") +
#      ylab(NULL) +
#      theme(axis.text = element_text(size = 12),
#            axis.title = element_text(size = 14, face = "bold")) 
#  })
#  
#  output$heatmap_bothsexes_2 <- renderPlot({
#    datos <- crear.tabla(2, showIQR = FALSE) %>% as.matrix %>% melt
#    
#    ggplot(data = datos, aes(x = Var2, y = Var1)) +
#      geom_tile(aes(fill = value)) +
#      scale_fill_gradient(low = "white", high = "purple") +
#      geom_text(size = 4, label = datos$value) +
#      theme_minimal() + 
#      xlab("Comorbidities") +
#      ylab(NULL) +
#      theme(axis.text = element_text(size = 12),
#            axis.title = element_text(size = 14, face = "bold")) 
#  })
#  
#  output$heatmap_bothsexes_3 <- renderPlot({
#    datos <- crear.tabla(3, showIQR = FALSE) %>% as.matrix %>% melt
#    
#    ggplot(data = datos, aes(x = Var2, y = Var1)) +
#      geom_tile(aes(fill = value)) +
#      scale_fill_gradient(low = "white", high = "purple") +
#      geom_text(size = 4, label = datos$value) +
#      theme_minimal() + 
#      xlab("Comorbidities") +
#      ylab(NULL) +
#      theme(axis.text = element_text(size = 12),
#            axis.title = element_text(size = 14, face = "bold")) 
#  })
#  
#  output$heatmap_bothsexes_4 <- renderPlot({
#    datos <- crear.tabla(4, showIQR = FALSE) %>% as.matrix %>% melt
#    
#    ggplot(data = datos, aes(x = Var2, y = Var1)) +
#      geom_tile(aes(fill = value)) +
#      scale_fill_gradient(low = "white", high = "purple") +
#      geom_text(size = 4, label = datos$value) +
#      theme_minimal() + 
#      xlab("Comorbidities") +
#      ylab(NULL) +
#      theme(axis.text = element_text(size = 12),
#            axis.title = element_text(size = 14, face = "bold")) 
#  })
#  
#  output$graph_by_stage <- renderPlotly({
#    # Customising display
#    gp <- ggplotly(g) %>% layout(margin = list(r = 210, l = 65, b = -10)) %>%
#       config(displaylogo = FALSE,
#              collaborate = FALSE,
#              toggleSpikesLines = FALSE,
#                 modeBarButtonsToRemove = list(
#                   'sendDataToCloud',
#                   'toImage',
#                   'autoScale2d',
#                   'resetScale2d',
#                   'hoverClosestCartesian',
#                   'hoverCompareCartesian',
#                   'zoom2d',
#                   'pan2d',
#                   'select2d',
#                   'lasso2d',
#                   'zoomIn2d',
#                   'zoomOut2d',
#                   'toggleSpikelines'
#                 ))
#    # Moving the title of the axis to show them
#    gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.05
#    gp[['y']][['layout']][['annotations']][[2]][['y']] <- -0.05
#    gp
#})
#
  
output$forestplot <- renderImage({
  ajustado <- ifelse(input$adjusted == TRUE, "_adj", "")
  list(src = paste0(getwd(), "/www/forest_plot/", input$forest.com, "_", input$forest.fac, ajustado, ".svg"),
       contentType = 'image/svg+xml')
  }, deleteFile = FALSE)
}


shinyApp(ui = ui, server = server)
