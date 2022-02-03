#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# 

library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(broom)
library(kableExtra)
library(plotly)
library(dplyr)
library(leaflet)
library(osrm)
library(waiter)
library(rvest)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(tm)
library(rintrojs)

# Data Source 
df.salary.2020 <- read.csv("df.salary.2020.csv")
df.salary.final <- read.csv("df.salary.final.csv")
NUS.final <- read.csv("NUS_WC.csv")
NTU.final <- read.csv("NTU_WC.csv")
SMU.final <- read.csv("SMU_WC.csv")
SUTD.final <- read.csv("SUTD_WC.csv")
SUSS.final <- read.csv("SUSS_WC.csv")

# Remove first column (redundant index)
df.salary.2020 <- df.salary.2020[-c(1,2)]
df.salary.final <- df.salary.final[-c(1)]
NUS.final <- NUS.final[-c(1)]
NTU.final <- NTU.final[-c(1)]
SMU.final <- SMU.final[-c(1)]
SUTD.final <- SUTD.final[-c(1)]
SUSS.final <- SUSS.final[-c(1)]


# Postal Code Files 
df.postal <- read.delim("SG.txt")
df.postal <- df.postal %>% select(2,10,11)
colnames(df.postal) <- c("PostalCode","lat","lng")

# Filtering unique degree names 
degree.names <- gsub("[(,)]", "", df.salary.2020$degree)
unique.degree.names <- unique(unlist(strsplit(degree.names, split = " ")))
stopwords <- c("and", "of", "with", "Hons", "Bachelor", "in", "Work", "Cum", "above","Laude")
unique.degree.names <- unique.degree.names[!unique.degree.names %in% stopwords]

# Tab 3 Files 
job.vacancy.ind <- read.csv("job.vacancy.ind.csv")
wage_increment <- read.csv("wage_increment.csv")
job.vacancy.ind$industry2 <- str_to_title(job.vacancy.ind$industry2)
wage_increment$industry2 <- str_to_title(wage_increment$industry2)

# Define UI for application
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "SG Degree Screener"),
  
  # Dashboard Side Bar
  dashboardSidebar(
    introjsUI(),
    
    sidebarMenu(menuItem("Home Page", tabName = "Home", icon = icon("home"))),
    
    introBox(
    sidebarMenu(
      menuItem("Explore Degree", tabName = "Degree", icon = icon("book")),
      menuItem("Explore University", tabName = "School", icon = icon("school")),
      menuItem("Salary Projections", tabName = "Career", icon = icon("chart-line"))),
      data.step = 1,
      data.intro = "In this section, you will navigate across the different tabs to explore the screener."),
    
    introBox(
    sidebarMenu(
      menuItem("MBTI Profiling Test", tabName = "MBTI", icon = icon("user"))),
    data.step = 3,
    data.intro = "If you do not know about your MBTI, click the tab here to do your profiling test.")
    
),
  # Dashboard Body 
  dashboardBody(
    tabItems(
      
      tabItem("Home", tags$img(src = "homepage.png", height = "100%", width = "100%"),
      h3(strong("In this web app you will be able to:")),
      p("1. Explore and find your desire degree that suit your expectation"),
      p("2. Choose a university that meet your requirement"),
      p("3. Learn about the future prospect of your salary"),
      p(em("*The salary information on this app is based on fresh graduate employment survey from 
           NUS, NTU, SMU, SUTD & SUSS")),
      
      
      introBox(
        actionButton("btn",strong("Click here for a website tour!")),
        data.step = 11,
        data.intro = wellPanel(p("Enjoy your self discovery!"))
      )),

      # Tab 1: Degree 
      tabItem("Degree",
              
              fluidRow(box(width = 3, height = 280,
                           
                           introBox(
                           selectInput("mbti",label = "Select MBTI", choices = df.salary.2020$mbti_types),
                           tags$head(tags$style(HTML(".selectize-input {height: 50px;}"))),
                           sliderInput("starting.salary", dragRange = TRUE ,
                                       label = "Enter desired starting salary (Gross Monthly Median):", 
                                       min = 0, max = 8000, value = c(2800,3000), step = 50),
                           data.step = 2,
                           data.intro = "In the input here, you will key in your MBTI and your preferred 
                           or expected salary ranges and this will show you the degrees that suit your expectation.")),
                       
                       box(width = 9, height = 480, plotlyOutput("chart2"))
              ),
              
              fluidRow(box(width = 3,height = 130,
                           
                           introBox(
                           selectInput("chosen.degree1",label = "Select Desired Degree", choices = df.salary.2020$degree),
                           tags$head(tags$style(HTML(".selectize-input {height: 50px;}"))),
                           data.step = 4,
                           data.intro = "In this input, you can select different degrees to see their 
                           salary changes over the years.")),
                           
                           box(width = 9, height = 480, plotlyOutput("chart3"))),
              
              
              fluidRow(box(title = "Summary", width = 12, height = 500, 
                           style = "font-size:100%", status = "primary",
                           solidHeader = T, 
                           
                           introBox(
                           DTOutput("chart1"),
                           data.step = 5,
                           data.intro = "This table here shows the entire list of degrees, 
                           universities and their respective salaries in Singapore.")),
              )),
      
      # Tab 2: School 
      tabItem("School",
              
              fluidRow(box(width = 2, height = 125,
                           
                           introBox(
                           numericInput("postal",label = "Home Postal Code", min = 0, max = 999999, value = 752350),
                           data.step = 6,
                           data.intro = "Key in your home postal code to view the distance between your 
                           home and the different universities.")),
                           
                           
                       useWaitress(),
                       box(width = 10, leafletOutput("chart4"))),
              
              fluidRow(box(width = 2, height = 150,
                           
                           introBox(
                           selectizeInput('MultiSelect', 'Select Degree Keywords', choices = unique.degree.names, multiple = TRUE),
                           data.step = 7,
                           data.intro = "This is a multi select input. You can key in the keywords of your desired degrees to 
                           see which university offers them and how their starting salaries differs.")),
                           
                       box(width = 10, plotlyOutput("chart5"))),
              
              fluidRow(box(width = 2, height = 300,
                           
                           introBox(
                           radioButtons("radio_buttons", label = "University", choices = c("NUS", "NTU", "SMU", "SUTD", "SUSS")),
                           sliderInput("size",
                                       "Zoom Bar",
                                       min = 1,  max = 10,  value = 1),
                           data.step = 8,
                           data.intro = "In this input here, you can select the different universities to see a word cloud of 
                           reviews given by the current and ex students. The zoom bar allows you to adjust the frequency of the 
                           words appeared in the reviews.")),
              
                       box(width = 10, title = "University Reviews", wordcloud2Output("word_cloud"))) 
              
      ),
      
      # Tab 3: Career
      tabItem("Career",
              
              fluidRow(box(width = 3,height = 430,
                           
                           introBox(
                           selectInput("chosen.school.tab4",label = "Select Desired University", choices = df.salary.2020$university),
                           tags$head(tags$style(HTML(".selectize-input {height: 50px;}"))),
                           
                           selectInput("chosen.degree.tab4",label = "Select Desired Degree", choices = NULL),
                           tags$head(tags$style(HTML(".selectize-input {height: 50px;}"))),
                           data.step = 9,
                           data.intro = "In the input here, you can choose your desired university and 
                           degree to see the potential salary projections for the next 5 years as well as its employability.<br/><br/><b> 
                           Please select your university first followed by your degree.</b>"),
                           
                           introBox(
                           selectInput("chosen.industry.tab4",label = "Select Desired Industry", choices = sort(wage_increment$industry2)),
                           tags$head(tags$style(HTML(".selectize-input {height: 50px;}"))),
                           data.step = 10,
                           data.intro = "In the input here, you can choose your preferred industry that you think you might work in. 
                           By choosing a different industry, it will change your salary projections due to the different 
                           wage growth in different industry. <br/><br/>This input will also show your the job vacancy chart over the past 10 years."
                           )),
                       
                       box(width = 9, height = 430, plotlyOutput("chart6"))),
              
              fluidRow(box(plotlyOutput("chart7")),
                       box(plotlyOutput("chart8")))

              ),
      
      #Tab 4: MBTI test
      tabItem("MBTI",
              
              box(style = "font-size:300%",width = 950, height = 600, 
                  tags$div(class="header", checked=NA,
                           tags$p("Unsure about your MBTI?"),
                           tags$a(href="https://www.16personalities.com/free-personality-test", "Click Here!"),align = "center"),
                  tags$img(src = "mbtitest.png", height = 300, width = 950), align = "center"))
    ),
  )
)

# Define server logic required
server <- function(session, input, output) {
  
  observeEvent(input$btn, introjs(session, 
                                    events = list(onbeforechange = readCallback("switchTabs"))))
  
  
  observe({
    updateSelectInput(session, inputId = "chosen.degree.tab4", label = "Select Desired Degree", 
                      choices = df.salary.2020[df.salary.2020$university==input$chosen.school.tab4,]$degree)
  })
  
  waitress <- Waitress$new(theme = "overlay-percent", min = 0, max = 10)
  
  output$chart1 <- renderDT(df.salary.2020, 
                            
                            options = list(pageLength = 5, 
                                           lengthMenu = c(5, 10, 15, 20), 
                                           scrollX = T, autoWidth = TRUE,
                                           columnDefs = list(list(width = '150px', 
                                                                  targets = c(1,2,3))
                                           )))
  
  output$chart2 <- renderPlotly({
    df.salary.2020 %>% filter(gross_monthly_median >= input$starting.salary[1] & 
                                gross_monthly_median <= input$starting.salary[2] &
                                mbti_types == input$mbti) %>% 
      arrange(desc(gross_monthly_median)) %>% 
      
      plot_ly(x = ~gross_monthly_median,
              y = ~employment_rate_overall, size = ~10, 
              marker = list(size = 15, opacity = 0.7, color = ~SGP_Fees, 
                            colorbar = list(title='<b>Legend: Cost of Degree (SGD$)</b>', title=list(font=12),titleside= "right"),
                            colorscale = list(c(0,0.5,1), c('#00FF00', '#FFFF00','#FF0000'))),
              text = ~paste('Degree:', degree, '</br>University:', university),
              hovertemplate = paste("<b>%{text}</b><br><br>",
                                    "%{yaxis.title.text}: %{y:.1f}<br>",
                                    "%{xaxis.title.text}: %{x:.0f}<br>",
                                    "Cost of Degree: %{marker.color:,} <extra></extra>"),
              height = 450) %>% 
      layout(title = 'Degrees Salary, Employability and Cost',
             title=list(font=15),
             xaxis = list(title = 'Median Gross Monthly Starting Salary (SGD$)', 
                          title=list(font=15)), 
             yaxis = list(title = 'Employment Rate (%)',
                          title=list(font=15)))
  })
  
  output$chart3 <- renderPlotly({
    
    df.chosen <- df.salary.final %>% group_by(year, degree) %>% 
      filter(degree == input$chosen.degree1) %>% 
      summarise(employment_rate_overall = mean(employment_rate_overall),
                gross_monthly_mean = mean(gross_monthly_mean),
                gross_monthly_median = mean(gross_monthly_median),
                gross_mthly_25_percentile = mean(gross_mthly_25_percentile),
                gross_mthly_75_percentile = mean(gross_mthly_75_percentile)) %>% 
      ungroup()
    
    year <- df.chosen$year
    gross_mean <- df.chosen$gross_monthly_mean
    gross_25 <- df.chosen$gross_mthly_25_percentile
    gross_50 <- df.chosen$gross_monthly_median
    gross_75 <- df.chosen$gross_mthly_75_percentile
    employment <- df.chosen$employment_rate_overall
    
    ay <- list(overlaying = "y", side = "right", title = "Employment Rate(%)", title=list(font=12), automargin = T)
    
    plot_ly(df.chosen, x = ~year, mode = 'lines', height = 450) %>% 
      
      add_trace(y = ~employment, name = "Employment Rate (%)", 
                yaxis = "y2", type = 'bar',opacity = 0.3, 
                color = I("sky blue"), dash = 'dash') %>% 
      add_trace(y = ~gross_mean, 
                name = 'Mean', 
                line = list(color = 'rgb(22, 96, 167)', width = 4), 
                marker = list(color = "rgb(0,0,0)", size = 10)) %>% 
      add_trace(y = ~gross_50, 
                name = '50th Perc', 
                line = list(color = 'rgb(205, 12, 24)', width = 4), 
                marker = list(color = "rgb(0,0,0)", size = 10)) %>% 
      add_trace(y = ~gross_25, 
                name = '25th Perc', 
                line = list(color = 'rgb(0,0,0)', width = 4, dash = 'dot'), 
                marker = list(color = "rgb(0,0,0)", size = 10)) %>% 
      add_trace(y = ~gross_75, 
                name = '75th Perc', 
                line = list(color = 'rgb(0,0,0)', width = 4, dash = 'dot'), 
                marker = list(color = "rgb(0,0,0)", size = 10)) %>% 
      layout(title = paste(df.chosen$degree[1],'Salary over the years'),
             title=list(font=15),
             yaxis2 = ay,
             xaxis = list(title = 'Year',title=list(font=15), tick0 = 2016, dtick= 1),
             yaxis = list(title = 'Gross Monthly Starting Salary (SGD$)',title=list(font=15), tick0 = 0, dtick= 500),
             legend = list(orientation = "h",
                           xanchor = "center", 
                           x = 0.5,
                           y = -0.3))
  })
  
  output$chart4 <- renderLeaflet({
    
    validate(
      need(nchar(input$postal) == 6, 'Invalid Postal Code!'),
      need(input$postal %in% df.postal$PostalCode, 'Invalid Postal Code!'))
    
    waitress$notify(background_color = "transparent", position = "tr")
    
    user.lat <- df.postal[df.postal$PostalCode == input$postal,][[2]]
    user.lng <- df.postal[df.postal$PostalCode == input$postal,][[3]]
    
    df1 <- data.frame(school = c("Home", "NUS"),
                      lng = c(user.lng,103.77641003396995),
                      lat = c(user.lat,1.2967079330870517))
    
    df2 <- data.frame(school = c("Home", "SMU"),
                      lng = c(user.lng,103.85014253239788),
                      lat = c(user.lat,1.2963671519425972))
    
    df3 <- data.frame(school = c("Home", "NTU"),
                      lng = c(user.lng,103.68298651325388),
                      lat = c(user.lat,1.3494212039232791))
    
    df4 <- data.frame(school = c("Home", "SUTD"),
                      lng = c(user.lng,103.96331653901565),
                      lat = c(user.lat,1.3416006687263378))
    
    df5 <- data.frame(school = c("Home", "SUSS"),
                      lng = c(user.lng,103.77618596785106),
                      lat = c(user.lat,1.3292955180176584))
    
    uni.list <- c("NUS", "SMU", "NTU", "SUTD", "SUSS")
    
    for(i in 1:5){
      waitress$inc(1) # increase by 10%
      Sys.sleep(.2)
    }
    
    trip1 <- osrmRoute(loc = df1, returnclass="sf", osrm.profile = "car")
    travel.info <- osrmRoute(loc = df1, returnclass="sf",overview = FALSE, osrm.profile = "car")
    duration1 <- travel.info[[1]]
    distance1 <- travel.info[[2]]
    
    trip2 <- osrmRoute(loc = df2, returnclass="sf", osrm.profile = "car")
    travel.info <- osrmRoute(loc = df2, returnclass="sf",overview = FALSE, osrm.profile = "car")
    duration2 <- travel.info[[1]]
    distance2 <- travel.info[[2]]
    
    trip3 <- osrmRoute(loc = df3, returnclass="sf", osrm.profile = "car")
    travel.info <- osrmRoute(loc = df3, returnclass="sf",overview = FALSE, osrm.profile = "car")
    duration3 <- travel.info[[1]]
    distance3 <- travel.info[[2]]
    
    for(i in 1:4){
      waitress$inc(1) # increase by 10%
      Sys.sleep(.1)
    }
    
    trip4 <- osrmRoute(loc = df4, returnclass="sf", osrm.profile = "car")
    travel.info <- osrmRoute(loc = df4, returnclass="sf",overview = FALSE, osrm.profile = "car")
    duration4 <- travel.info[[1]]
    distance4 <- travel.info[[2]]
    
    trip5 <- osrmRoute(loc = df5, returnclass="sf", osrm.profile = "car")
    travel.info <- osrmRoute(loc = df5, returnclass="sf",overview = FALSE, osrm.profile = "car")
    duration5 <- travel.info[[1]]
    distance5 <- travel.info[[2]]
    
    waitress$close()
    
  
    
    leaflet() %>% 
      addProviderTiles("OneMapSG.Original", group = "Day") %>% 
      addProviderTiles("OneMapSG.Night", group = "Night") %>% 
      addProviderTiles("OneMapSG.Grey", group = "Grey") %>% 
      
      addPolylines(data = trip1, weight = 8, label = paste0("Distance: ",distance1,"km", " & Duration (Car): ",duration1,"mins"), color = "navy", opacity = 1, group = uni.list[1]) %>%
      addCircleMarkers(lat = df1$lat[2],
                       lng = df1$lng[2],
                       label = df1$school[2],
                       labelOptions = labelOptions(noHide = T, direction = "top"),
                       # popup = df1$school,
                       # popupOptions = popupOptions(closeButton=F, closeOnClick=F),
                       color = "blue",
                       stroke = FALSE,
                       radius = 8,
                       fillOpacity = 0.8,
                       group = uni.list[1]) %>% 
      
      addPolylines(data = trip2, weight = 8, label = paste0("Distance: ",distance2,"km", " & Duration (Car): ",duration2,"mins"),color = "black",opacity = 1, group = uni.list[2]) %>%
      addCircleMarkers(lat = df2$lat[2],
                       lng = df2$lng[2],
                       label = df2$school[2],
                       labelOptions = labelOptions(noHide = T,direction = "top"),
                       #popup = df2$school,
                       #popupOptions = popupOptions(closeButton=F, closeOnClick=F),
                       color = "blue",
                       stroke = FALSE,
                       radius = 8,
                       fillOpacity = 0.8,
                       group = uni.list[2]) %>%  
      
      addPolylines(data = trip3, weight = 8, label = paste0("Distance: ",distance3,"km", " & Duration (Car): ",duration3,"mins"), color = "#CC0066",opacity = 1, group = uni.list[3]) %>%
      addCircleMarkers(lat = df3$lat[2],
                       lng = df3$lng[2],
                       label = df3$school[2],
                       labelOptions = labelOptions(noHide = T,direction = "top"),
                       #popup = df3$school,
                       #popupOptions = popupOptions(closeButton=F, closeOnClick=F),
                       color = "blue",
                       stroke = FALSE,
                       radius = 8,
                       fillOpacity = 0.8,
                       group = uni.list[3]) %>% 
      
      addPolylines(data = trip4, weight = 8, label = paste0("Distance: ",distance4,"km", " & Duration (Car): ",duration4,"mins"), color = "#FF6666",opacity = 1, group = uni.list[4]) %>%
      addCircleMarkers(lat = df4$lat[2],
                       lng = df4$lng[2],
                       label = df4$school[2],
                       labelOptions = labelOptions(noHide = T,direction = "top"),
                       #popup = df4$school,
                       #popupOptions = popupOptions(closeButton=F, closeOnClick=F),
                       color = "blue",
                       stroke = FALSE,
                       radius = 8,
                       fillOpacity = 0.8,
                       group = uni.list[4]) %>% 
      
      addPolylines(data = trip5, weight = 8, label = paste0("Distance: ",distance5,"km", " & Duration (Car): ",duration5,"mins"), color = "#FFCC00",opacity = 1, group = uni.list[5]) %>%
      addCircleMarkers(lat = df5$lat[2],
                       lng = df5$lng[2],
                       label = df5$school[2],
                       labelOptions = labelOptions(noHide = T, direction = "top"),
                       #popup = df5$school,
                       #popupOptions = popupOptions(closeButton=F, closeOnClick=F),
                       color = "blue",
                       stroke = FALSE,
                       radius = 8,
                       fillOpacity = 0.8,
                       group = uni.list[5]) %>% 
      addCircleMarkers(lat = user.lat,
                       lng = user.lng,
                       color = "red",
                       stroke = FALSE,
                       radius = 9,
                       fillOpacity = 1,
                       label = "Home",
                       labelOptions = labelOptions(noHide = T, direction = "top")) %>%
      
      addLayersControl(baseGroups = c("Day","Night","Grey"), 
                       overlayGroups = uni.list)
  
    
    
  })
  
  output$chart5 <- renderPlotly({
    
    allpatterns <- function(fnames, patterns) {
      i <- sapply(fnames, function(fn) all(sapply(patterns, grepl, fn)) )
      fnames[i]
    }
    
    degree_choice <- input$MultiSelect
    
    a <- df.salary.2020 %>% filter(degree %in% unique(allpatterns(df.salary.2020$degree, degree_choice)))
    relevant_degrees <- a[!duplicated(a[,c('university','degree')]),]
    
    fig1 <- relevant_degrees %>% plot_ly(x=~degree, color=~university,
                                         colors= "Dark2", type = "box", 
                                         q1 =~gross_mthly_25_percentile, 
                                         median=~gross_monthly_median, 
                                         q3=~gross_mthly_75_percentile)
    
    fig1 %>% layout(title = "Degree Comparison Across Universities (hover for details)",
                    title=list(font=15),
                    xaxis=list(title="Degree", size=3, showticklabels=F), 
                    yaxis=list(title="Gross Monthly Starting Salary (SGD$)"), 
                    showlegend = TRUE, 
                    legend=list(title=list(text='University')))
    
  })
  
  output$word_cloud = renderWordcloud2(
    if (input$radio_buttons == "NUS"){
      nus.colours <- c("#ea7c08", "#053d7c", "#55626f")
      wordcloud2(NUS.final, color= rep_len(nus.colours, nrow(NUS.final)), size=input$size, fontFamily = "DM Sans", rotateRatio = 0)
    }else if (input$radio_buttons == "NTU"){
      ntu.colours <- c("#de2635", "#ddb265", "#061d62","#2e2024")
      wordcloud2(NTU.final, color= rep_len(ntu.colours, nrow(NTU.final)), size=input$size, fontFamily = "DM Sans", rotateRatio = 0)
    }else if (input$radio_buttons == "SMU"){
      smu.colours <- c("#997c4e", "#162e72", "#4c6490")
      wordcloud2(SMU.final, color= rep_len(smu.colours, nrow(SMU.final)), size=input$size, fontFamily = "DM Sans", rotateRatio = 0)
    }else if (input$radio_buttons == "SUTD"){
      sutd.colours <- c("#ba1225", "#4a4a4a", "#040404")
      wordcloud2(SUTD.final, color= rep_len(sutd.colours, nrow(SUTD.final)), size=input$size, fontFamily = "DM Sans", rotateRatio = 0)
    }else if (input$radio_buttons == "SUSS"){
      suss.colours <- c("#05395c", "#f90f08", "#7ba4b8", "#39668c")
      wordcloud2(SUSS.final, color= rep_len(suss.colours, nrow(SUSS.final)), size=input$size, fontFamily = "DM Sans", rotateRatio = 0)
    }
  )
  
  output$chart6 <- renderPlotly({
    
    starting.salary <- df.salary.2020 %>%
      filter(degree == input$chosen.degree.tab4 & university == input$chosen.school.tab4) %>%
      summarise(avg.25 = mean(gross_mthly_25_percentile),
                avg.gross = mean(gross_monthly_median),
                avg.75 = mean(gross_mthly_75_percentile))
    
    # Select Industry 
    # unique(wage_increment$industry2)
    selected.ind <- wage_increment %>% filter(industry2 == input$chosen.industry.tab4 & year > 2020)
    
    # Forecasted salary from 2021 to 2025 for 25th, median and 75th 
    new.salary.25 <- starting.salary[[1]]
    current.salary.25 <- starting.salary[[1]]
    
    for (num in 1:5){
      current.salary.25 <- ((selected.ind$Wage.Increment[num]/100) * current.salary.25) + current.salary.25
      new.salary.25 <- c(new.salary.25, current.salary.25)
    }
    
    new.salary.med <- starting.salary[[2]]
    current.salary.med <- starting.salary[[2]]
    
    for (num in 1:5){
      current.salary.med <- ((selected.ind$Wage.Increment[num]/100) * current.salary.med) + current.salary.med
      new.salary.med <- c(new.salary.med, current.salary.med)
    }
    
    new.salary.75 <- starting.salary[[3]]
    current.salary.75 <- starting.salary[[3]]
    
    for (num in 1:5){
      current.salary.75 <- ((selected.ind$Wage.Increment[num]/100) * current.salary.75) + current.salary.75
      new.salary.75 <- c(new.salary.75, current.salary.75)
    }
    
    # Combine them into a df 
    year <- c(2020,selected.ind$year)
    salary.projection <- data.frame(year, new.salary.25, new.salary.med, new.salary.75)
    salary.projection <- salary.projection %>% mutate_at(vars(new.salary.25, new.salary.med, new.salary.75), funs(round(., 0)))
    
    
    plot_ly(salary.projection, x = ~year, mode = 'lines') %>% 
      
      add_trace(y = ~new.salary.med, name = 'Median', 
                line = list(color = 'rgb(22, 96, 167)', width = 4), 
                marker = list(color = "rgb(0,0,0)", size = 10)) %>% 
      
      add_trace(y = ~new.salary.25, name = '25th Perc', 
                line = list(color = 'rgb(0,0,0)', width = 4, dash = 'dot'), 
                marker = list(color = "rgb(0,0,0)", size = 10)) %>% 
      
      add_trace(y = ~new.salary.75, name = '75th Perc', 
                line = list(color = 'rgb(0,0,0)', width = 4, dash = 'dot'), 
                marker = list(color = "rgb(0,0,0)", size = 10)) %>% 
      
      layout(title = paste(input$chosen.degree.tab4,"<br>",'Salary Projection from 2021 to 2025'),
             title=list(font=13),
             xaxis = list(title = 'Year',tick0 = 2016, dtick= 1),
             yaxis = list(title = 'Gross Monthly Starting Salary (SGD$)', tick0 = 0, dtick= 500))
  })
  
  output$chart7 <- renderPlotly({
    
    rate <- df.salary.2020 %>% 
      filter(degree == input$chosen.degree.tab4 & university == input$chosen.school.tab4) %>% 
      summarise(rate = mean(employment_rate_overall))
    
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = rate[[1]],
      gauge = list(axis = list(range = list(NULL,100),tickwidth = 1, tickcolor = "black")),
      number = list(suffix = "%"), 
      title = list(text = paste("Employability for","<br>",input$chosen.degree.tab4)),
      title=list(font=15),
      type = "indicator",
      mode = "gauge+number") %>%
      
      layout(margin = list(l=30,r=60))
  })
  
  output$chart8 <- renderPlotly({
    
    trend <- job.vacancy.ind %>% filter(industry2 == input$chosen.industry.tab4)
    
    trend %>% 
      plot_ly(x = ~year, y = ~job_vacancy, name = "Job Vacancy",
              type = 'bar', mode = 'lines+markers', textposition = 'auto') %>% 
      
      layout(title = paste('Job Vacancy for',"<br>",input$chosen.industry.tab4,"industry"), 
             title=list(font=13),
             xaxis = list(title = 'Year',tick0 = 2016, dtick= 1),
             yaxis = list(title = 'Job Vacancy'))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
