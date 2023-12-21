library(shiny)
library(shinydashboard)
library(openxlsx)
library(DT)
library(dplyr)
library(ggplot2)
library(tidyr)



ui <- dashboardPage(
  dashboardHeader(title = "Электронный журнал",titleWidth = 500),
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem("Загрузка журнала", tabName = "upload", icon = icon("file-import")),
                     menuItem("Журнал", tabName = "journal", icon = icon("book-journal-whills")),
                     menuItem("Статистика", tabName = "table_stats", icon = icon("chart-simple")),
                     menuItem("Графики", tabName = "plot_stats", icon = icon("chart-area")),
                     menuItem("Помощь", tabName = "help", icon = icon("circle-question")),
                     menuItem("О сайте", tabName = "about", icon = icon("circle-info"))
                   )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css")
    ),
    tabItems(
      tabItem(
        tabName = "upload",
        fileInput("upload","Выберете файл для загрузки", 
                  buttonLabel = "Выберете файл",
                  width = 500,
                  placeholder = "Файл не найден",
                  accept = c(".csv",".txt",".xmlx")),
        box( title = "Просмотр содержимого", 
             status = "info", 
             height = "545",
             width = "14",
             solidHeader = T, 
             column(width = 12,
                    tableOutput("upload"),
                    style = "height:485px; overflow-y: scroll;overflow-x: scroll;")
        )
      ),
      
      tabItem(
        tabName = "journal",
        box( title = "Журнал", 
             status = "info", 
             height = "545",
             width = "14",
             solidHeader = T, 
             column(width = 12,
                    dataTableOutput("journal"),
                    style = "height:485px; overflow-y: scroll;overflow-x: scroll;")
        ),
        actionButton("createusr","Добавить ученика"),
        actionButton("deleteusr","Удалить ученика")
      ),
      
      tabItem(
        tabName = "table_stats",
        tabsetPanel(
          tabPanel("Статистика по классам",
                   DTOutput("class_subject_stats")),
          tabPanel("Статистика по предметам",
                   DTOutput("overall_subject_stats"))
        )                   
      ),
      
      tabItem(
        tabName = "plot_stats",
        tabsetPanel(
          tabPanel("Статистика по классам",
                   plotOutput("plot_class_subject"), plotOutput("histogram_class_subject")),
          tabPanel("Статистика по предметам",
                   plotOutput("plot_overall_subject"), plotOutput("histogram_overall_subject"))
        )     
      ),
      
      tabItem(
        tabName = "help",
        box(title = "Помощь",
            status = "info",
            width = 15,
            HTML("<p>Добро пожаловать в электронный журнал оценок,</p>
            <p>Здесь вы можете загрузить и отредактировать журнал, а также посмотреть статистику учеников в удобной для вас форме</p>
            <p>1. Для начала загрузите журнал во вкладке 'Загрузка журнала'</p>
            <p>1.1. Для загрузки журнала, нажмите на кнопку 'Выберете файл', он должен иметь одно из таких расширений: #.xlsx #.csv #.txt</p>
            <p>2. Для редактирования журнала зайдите во вкладку 'Журнал'</p>
            <p>2.1. Для удаления ученика, нажмите на кнопку 'Удалить ученика'</p>
            <p>2.2. Для добавления ученика, нажмите на кпоку 'Добавить ученика'</p>
            <p>4. Для просмотра статистики в табличной форме перейдите во вкладку 'Статистика'</p>
            <p>4.1. Для просмотра статистики по классам выберите вкладку 'Статистика по классам'</p>
            <p>4.2. Для просмотра статистики по предметам выберите вкладку 'Статистика по предметам'</p>
            <p>5. Для просмотра статистики в виде графика перейдите во вкладку 'График'</p>
            <p>5.1. Для просмотра графика по классам выберите вкладку 'Статистика по классам'</p>
            <p>5.2. Для просмотра графика по предметам выберите вкладку 'Статистика по предметам'</p>
            <p>Во вкладке о сайте, содеражатся контакты автора приложения Shiny </p>")
        )
      ),
      
      tabItem(
        tabName = "about",
        box(title = "О разработчике",
            status = "info",
            width = 15,
            HTML("<p>Разработчик приложения Shiny</p>
            <p>Малинин Матвей Эдуардович, студент СибГУТИ, по направлению 'Информационная безопасность', группа: АБ-110</p>
            <p> Контактные данные:</p>
            <p>1. VK: <a href='https://vk.com/malinin3009'>Вконтакте</a> </p>
            <p>2. Telegram: <a href='https://postimages.org/' target='_blank'><img src='https://i.postimg.cc/cHSfRk7r/mj8-KU-rq-Xyg.jpg' border='0' alt='mj8-KU-rq-Xyg'/></a> </p>
            <p>3. Номер телефона 8-999-450-**-**-.</p>")
        )
      )
    )
  )
)



server <- function(input, output){
  
  loadedData <- reactiveVal(NULL)
  values <- reactiveValues(dfWorking = loadedData)
  
  observeEvent(input$upload,{
    req(input$upload)
    file <- input$upload
    ext <- tools::file_ext(file$datapath)
    if (ext == "xlsx"){
      data <- read.xlsx(file$datapath) 
      loadedData(data)
    }
    else if(ext == "csv"){
      data <- read.csv2(file$datapath)
      loadedData(data)
    }
    else if(ext == "txt"){
      data <- read.delim(file$datapath,sep = "\t")
      loadedData(data)
    }
    else{
      loadedData("Загрузите файл .csv .txt .xlsx")
    }
  })
  
  output$upload <- renderTable({
    loadedData()
  })
  
  output$journal <- renderDataTable({
    datatable(loadedData(), 
              editable = list(target = 'cell', 
                              disable = list(columns = c(0))),
              selection = "single",
              options = list(language = list(search = "Поиск"))
    )
  })
  
  observeEvent(input$createusr,{
    if(input$createusr){
      loadedData(add_row(loadedData()))
    }
  })
  
  observeEvent(input$deleteusr, {
    t = loadedData()
    if (!is.null(input$journal_rows_selected)) {
      t <- t[-as.numeric(input$journal_rows_selected),]
    }
    loadedData(t)
  })
  
  observe({
    if (!is.null(loadedData())) {
      data_stats <- loadedData() %>%
        gather(key = "subject", value = "mark", -c(name, class)) %>%
        group_by(class, subject) %>%
        summarize(
          Среднее.значение = mean(mark),
          Медиана = median(mark),
          `2` = sum(mark == 2),
          `3` = sum(mark == 3),
          `4` = sum(mark == 4),
          `5` = sum(mark == 5),
          Всего = n()
        ) %>%
        mutate(across(`2`:`5`, ~sprintf("%d (%.1f%%)", ., .*100/Всего)))
      
      output$class_subject_stats <- 
        renderDT(data_stats,
                 editable = FALSE, 
                 selection = "single", 
                 escape = FALSE, 
                 options = list(language = list(search = "Предмет")))
      
      plot_class_subject <- ggplot(data_stats, aes(x = subject, y = Среднее.значение, fill = factor(class))) +
        geom_bar(stat = "identity", position = "dodge",colour="black",width = 0.5) +
        labs(title = "Статистика по классам",
             x = "Предмет",
             y = "Средняя оценка") +
        theme_grey()
      
      output$plot_class_subject <- renderPlot({
        print(plot_class_subject)
      })
    }
  })
  
  observe({
    if (!is.null(loadedData())) {
      data_stats <- loadedData() %>%
        gather(key = "subject", value = "mark", -c(name, class))%>%
        group_by(subject) %>%
        summarize(
          Среднее.значение = mean(mark),
          Медиана = median(mark),
          `2` = sum(mark == 2),
          `3` = sum(mark == 3),
          `4` = sum(mark == 4),
          `5` = sum(mark == 5),
          Всего = n()
        ) %>%
        mutate(across(`2`:`5`, ~sprintf("%d (%.1f%%)", ., .*100/Всего)))
      
      output$overall_subject_stats <- 
        renderDT(data_stats,
                 editable = FALSE, 
                 selection = "single", 
                 escape = FALSE, 
                 options = list(language = list(search = "Предмет")))
      
      plot_overall_subject <- ggplot(data_stats, aes(x = subject, y = Среднее.значение)) +
        geom_bar(stat = "identity", fill = "#BA55D3",colour="black",width = 0.1) +
        labs(title = "Статистика по предметам",
             x = "Предмет",
             y = "Средняя оценка") +
        theme_grey()
      
      output$plot_overall_subject <- renderPlot({
        print(plot_overall_subject)
      })
    }
  })
}



shinyApp(ui = ui, server = server)