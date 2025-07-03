library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(shinydashboard)

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
        h1, h2, h3, h4, p {
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
                h4("Silahkan Input Datamu!"),
                p("Pastikan datamu dalam format CSV (max 30MB"),
                fileInput("file1", "Choose a File"),
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
              h2("Quiz Section")
      ),
      
      tabItem(tabName = "FAQ",
              h2("Frequently Asked Questions")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
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
  
  # Pemilihan Variabel Respon (DIPERBARUI)
  # Hanya variabel yang memenuhi salah satu dari dua kriteria berikut yang akan ditampilkan:
  # 1) Variabel kategorik dengan tepat 2 kategori unik (binary categorical),
  # 2) Variabel numerik dengan nilai hanya 0 dan 1 (binary numeric).
  output$select_var_respon <- renderUI({
    req(data())
    df <- data()
    
    # Inisialisasi vektor kosong untuk menampung nama variabel yang valid
    valid_vars <- c()
    
    # Loop melalui setiap kolom di dataset
    for (var_name in names(df)) {
      col <- df[[var_name]]
      # Hilangkan nilai NA untuk pengecekan
      col_non_na <- na.omit(col)
      
      # Cek Kondisi 1: Apakah variabel kategorik dengan 2 level unik?
      is_categorical_2_level <- (is.factor(col) || is.character(col)) && (length(unique(col_non_na)) == 2)
      
      # Cek Kondisi 2: Apakah variabel numerik yang hanya berisi 0 dan 1?
      is_numeric_01 <- is.numeric(col) && all(col_non_na %in% c(0, 1)) && length(unique(col_non_na)) > 0
      
      # Jika salah satu kondisi terpenuhi, tambahkan nama variabel ke daftar valid
      if (is_categorical_2_level || is_numeric_01) {
        valid_vars <- c(valid_vars, var_name)
      }
    }
    
    # Buat selectInput hanya dengan variabel yang telah divalidasi
    selectInput("response_var", "Pilih variabel respon (Y):", choices = valid_vars)
  })
  
  # Declare variabel prediktor
  predictor_vars <- reactive({
    req(data(), input$response_var)
    setdiff(names(data()), input$response_var)
  })
  
  output$var_to_plot_ui <- renderUI({
    req(df_reactive(), predictor_vars())
    selectInput("var_to_plot", "Pilih Variabel Kategorik:",
                choices = predictor_vars(), selected = predictor_vars()[1])
  })
    
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
}

# Run app
shinyApp(ui, server)
