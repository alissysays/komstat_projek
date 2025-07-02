library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(shinydashboard)

# batas maksimal upload file (max 30 MB)
options(shiny.maxRequestSize = 30*1024^2) 

# code bantuan variabel
categorical_vars <- c("gender", "EKG_results", "region", "dietary_habits")
binary_vars <- c("hypertension", "diabetes", "obesity", "family_history", 
                 "previous_heart_disease", "participated_in_free_screening")
predictor_vars <- c(categorical_vars, binary_vars)

# UI
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = span("Analisis Data Kategorik", 
                               style = "font-family: 'Georgia'; font-weight: bold; font-size: 20px")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Step 1", tabName = "step1", icon = icon("dot-circle")),
      menuItem("Step 2", tabName = "step2", icon = icon("dot-circle")),
      menuItem("Step 3", tabName = "step3", icon = icon("dot-circle")),
      menuItem("Step 4", tabName = "step4", icon = icon("dot-circle")),
      menuItem("Quiz!", tabName = "quiz", icon = icon("brain")),
      menuItem("FAQ", tabName = "FAQ", icon = icon("question-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        body, .content-wrapper {
          background-image: url('background.png');
          background-size: cover;
          background-position: center;
          background-repeat: no-repeat;
          background-attachment: fixed;
        }
        h1, h2, h3, p {
          font-family: 'Georgia';
        }
      "))
    ),
    
    fluidRow(
      column(12, align = "center",
             tags$img(src = "logo.png", height = "100px"),
             tags$h1("ANALISIS LOGISTIK BINER", style = "font-weight: bold;")
      )
    ),
    
    tabItems(
      tabItem(tabName = "home",
              tags$div(
                h2("Hallo !!", style = "text-align: center; font-weight: bold;"),
                h2("Selamat Datang di Dashboard Logistik Biner", 
                   style = "text-align: center;"),
                br(),
                h3("Apa itu Fungsi Binomial?", style = "font-weight: bold;"),
                p("Tiga link function yang umum digunakan adalah logit, probit, dan complementary log-log (cloglog). Fungsi logit mengubah menjadi log-odds dengan rumus, dan merupakan yang paling umum digunakan karena interpretasinya yang jelas terhadap odds. Fungsi probit menggunakan invers dari distribusi normal standar kumulatif, cocok untuk data yang diasumsikan mengikuti distribusi normal laten. Sedangkan cloglog menggunakan, lebih sensitif untuk kejadian jarang dan sering digunakan dalam analisis survival. Pemilihan link function tergantung pada asumsi distribusi laten dan konteks aplikasinya."),
                br(),
                p("Silahkan Input Datamu!"),
                p("Pastikan datamu dalam format CSV"),
                fileInput("file1", "Pilih File CSV", accept = c(".csv", ".txt")),
                textOutput("file_warning"),
                tableOutput("file1_contents"),
                tableOutput("tipe_variabel"),
                br(),
                uiOutput("select_var_respon")
              )
      ),
      
      tabItem(tabName = "step1", 
              fluidRow(
                box(title = tagList(icon("table"), "Tabel Missing Value"), 
                    width = 6, status = "primary", solidHeader = TRUE,
                    DT::dataTableOutput("missing_table")),
                
                box(title = tagList(icon("check-circle"), "Tabel Setelah Preprocessing Data"), 
                    width = 6, status = "success", solidHeader = TRUE,
                    DT::dataTableOutput("processed_table"))
              )
      ),
      
      tabItem(tabName = "step2",
              h2("Eksplorasi Data")
      ),
      
      tabItem(tabName = "step3",
              h2("Uji Asumsi dan Pemodelan")
      ),
      
      tabItem(tabName = "step4",
              h2("Kecocokan Model")
      ),
      
      tabItem(tabName = "quiz", 
              h2("Quiz Time!", style = "text-align:center; font-weight:bold;"),
              p("Uji pemahamanmu tentang regresi logistik biner.", style = "text-align:center;"),
              hr(),
              
              fluidRow(
                column(12,
                       h4("Soal 1", style="font-weight:bold;"),
                       tags$img(src = "quiz_1.png", width = "100%"),
                       p("Berikut adalah hasil dari Generalized Linear Model untuk mengetahui apakah suatu kepiting betina memiliki asosiasi dengan kepiting jantan lain berdasarkan lebar cangkangnya. Berdasarkan hasil tersebut, persamaan prediksinya adalah..."),
                       radioButtons("quiz1", "Pilihan:", 
                                    choices = c("A. Logit (P(Y=1))= -12,35 +0,49x", 
                                                "B. (P(Y=1))= -12,35 +0,49x",
                                                "C. Y= -12,35 +0,49x",
                                                "D. Y=  0,49-12,35x",
                                                "E. Logit (P(Y=1))= 0,49-12,35x"), 
                                    selected = character(0)),
                       # Tempat untuk menampilkan feedback jawaban 1
                       uiOutput("quiz1_feedback")),
              ),
              hr(),
              
              fluidRow(
                column(12,
                       h4("Soal 2", style="font-weight:bold;"),
                       p("Berdasarkan hasil nomor 1, tentukan probabilitas kepiting betina memiliki asosiasi dengan kepiting jantan lain jika lebar cangkangnya sebesar 21 cm!"),
                       radioButtons("quiz2", "Pilihan:", 
                                    choices = c("A. 0,812", 
                                                "B. 0.126",
                                                "C. 0,129",
                                                "D. 0,876",
                                                "E. 0,821"), 
                                    selected = character(0)),
                       # Tempat untuk menampilkan feedback jawaban 2
                       uiOutput("quiz2_feedback")),
              ),
              hr(),
              
              # --- Tombol untuk Cek Jawaban ---
              fluidRow(
                column(12, align="center",
                       actionButton("check_quiz", "Cek Jawaban!", icon = icon("check"), class = "btn-primary")
                )
              )
      ),
      
      
      tabItem(tabName = "FAQ",
              h2("Frequently Asked Questions")
      )
    )
  )
)

# Server
server <- function(input, output) {
  # Validasi tipe file dan load data
  data <- reactive({
    req(input$file1)
    
    # Cek apakah .csv
    ext <- tools::file_ext(input$file1$name)
    if (tolower(ext) != "csv") {
      return(NULL)
    }
    
    read.csv(input$file1$datapath)
  })
  
  # Pesan kesalahan jika bukan CSV
  output$file_warning <- renderText({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    
    if (tolower(ext) != "csv") {
      return("File dataset tidak sesuai. Harap unggah file dengan format .csv")
    } else {
      return("")
    }
  })
  
  # Preview data
  output$file1_contents <- renderTable({
    req(data())
    head(data())
  })
  
  # Tipe data setiap kolom
  output$tipe_variabel <- renderTable({
    req(data())
    
    df <- data()
    tipe <- sapply(df, function(col) {
      if (is.numeric(col)) {
        return("Numerik")
      } else if (is.factor(col) || is.character(col)) {
        return("Kategorik")
      } else {
        return(class(col))
      }
    })
    
    data.frame(
      Variabel = names(tipe),
      Tipe = unname(tipe),
      row.names = NULL
    )
  })
  
  # Pilih variabel respon
  output$select_var_respon <- renderUI({
    req(data())
    selectInput("response_var", "Pilih variabel respon (Y):",
                choices = names(data()))
  })
  
  # Preprocessing: ubah NA jadi modus dan ubah ke faktor
  df_reactive <- reactive({
    req(data())
    df <- data()
    
    # Ubah ke faktor
    for (col in c(categorical_vars, binary_vars)) {
      if (col %in% names(df)) {
        df[[col]] <- as.factor(df[[col]])
      }
    }
    
    # Mengganti NA dengan modus
    fill_na_mode <- function(x) {
      if (is.factor(x)) {
        mode_val <- names(sort(table(x), decreasing = TRUE))[1]
        x[is.na(x)] <- mode_val
        x <- factor(x)
      }
      return(x)
    }
    
    valid_vars <- predictor_vars[predictor_vars %in% names(df)]
    df[valid_vars] <- lapply(df[valid_vars], fill_na_mode)
    
    df
  })
  
  # Tabel Missing Value
  output$missing_table <- DT::renderDataTable({
    req(data())
    df <- data()
    sum_na <- data.frame()
    for (col in predictor_vars) {
      if (col %in% names(df)) {
        na_count <- sum(is.na(df[[col]]))
        sum_na <- rbind(sum_na, data.frame(var = col, n_na = na_count))
      }
    }
    DT::datatable(sum_na, options = list(scrollX = TRUE))
  })
  
  # Tabel setelah preprocessing
  output$processed_table <- DT::renderDataTable({
    req(df_reactive())
    DT::datatable(df_reactive(), options = list(scrollX = TRUE))
  })

  #Menampilkan Jawaban QUiz
  observeEvent(input$check_quiz, {
    # Feedback untuk Soal 1
    output$quiz1_feedback <- renderUI({
      req(input$quiz1) # Pastikan pengguna sudah menjawab
      if (input$quiz1 == "A. Logit (P(Y=1))= -12,35 +0,49x") {
        tags$p("Benar!", style = "color: green; font-weight: bold;")
      } else {
        tags$p("Salah. Jawaban yang benar adalah A. Logit (P(Y=1))= -12,35 +0,49x", style = "color: red; font-weight: bold;")
      }
    })
    
    #Feedback untuk soal 2
    output$quiz2_feedback <- renderUI({
      req(input$quiz2) # Pastikan pengguna sudah menjawab
      if (input$quiz2 == "C. 0,129") {
        tags$p("Benar!", style = "color: green; font-weight: bold;")
      } else {
        tags$p("Salah. Jawaban yang benar adalah C. 0,129", style = "color: red; font-weight: bold;")
      }
    })
    
  })
}




# Run app
shinyApp(ui, server)
