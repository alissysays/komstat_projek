library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(shinydashboard)

# batas maksimal upload file (max 30 MB)
options(shiny.maxRequestSize = 30*1024^2) 

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
        position: relative;
        z-index: 1;
      }

      .content-wrapper::before {
        content: '';
        position: absolute;
        top: 0; left: 0; right: 0; bottom: 0;
        background: rgba(255, 255, 255, 0.6); 
        z-index: -1; 
      }

      body, .content-wrapper {
        background-image: url('background_new.png');
        background-size: cover;
        background-position: center;
        background-repeat: no-repeat;
        background-attachment: fixed;
      }

      h1, h2, h3, h4, p, label, span, div {
        font-family: 'Georgia', serif;
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
                 p("Aplikasi ini dirancang untuk membantu Anda melakukan analisis data menggunakan model regresi logistik biner", 
                   style = "text-align: center;"),
                br(),
                h3("Apa itu Fungsi Binomial?", style = "font-weight: bold;"),
                p("Tiga link function yang umum digunakan adalah logit, probit, dan complementary log-log (cloglog). Fungsi logit mengubah menjadi log-odds dengan rumus, dan merupakan yang paling umum digunakan karena interpretasinya yang jelas terhadap odds. Fungsi probit menggunakan invers dari distribusi normal standar kumulatif, cocok untuk data yang diasumsikan mengikuti distribusi normal laten. Sedangkan cloglog menggunakan, lebih sensitif untuk kejadian jarang dan sering digunakan dalam analisis survival. Pemilihan link function tergantung pada asumsi distribusi laten dan konteks aplikasinya."),
                br(),
                h4("Silahkan Input Datamu!", style = "font-weight: bold;"),
                p("Keterangan:", style = "font-weight: bold;"),
                p("   1. Pastikan datamu dalam format CSV"),
                p("   2. Maksimal file berukuran 30MB"),
                p("   3. Pastikan terdapat minimal 1 variabel biner pada dataset"),
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
                p("Step 1 akan memandu Anda melalui tahapan awal analisis data, dimulai dengan pemeriksaan kualitas data dan mempersiapkan data untuk pemodelan", style="text-align: center;"),
                box(title = tagList(
                  icon("table"), "Tabel Missing Value"), 
                  width = 6, status = "primary", solidHeader = TRUE,
                  DT::dataTableOutput("missing_table"),
                p(actionLink("help_missing_value", 
                             "Klik di sini untuk detail lebih lanjut tentang tabel ini." ),
                style = "text-align: left; margin-top: 5px; font-size: 0.9em;")
                    ),
                
                box(title = tagList(icon("check-circle"), "Tabel Setelah Preprocessing Data"), 
                    width = 6, status = "success", solidHeader = TRUE,
                    DT::dataTableOutput("processed_table"))
              )
      ),
      
      tabItem(tabName = "step2",
              p("Step 2 akan memandu Anda untuk mengeksplorasi struktur dan karakteristik dasar dataset Anda.", style="text-align: center;"),
              fluidRow(
                column(6,
                       tags$h3(icon("chart-bar"), "Data Summary",style= "font-weight: bold;"),
                       div(class = "box-custom",
                           verbatimTextOutput("summaryOutputUpload"),
                           p(actionLink("help_summary", 
                                        "Klik di sini untuk detail lebih lanjut tentang tabel ini." ),
                             style = "text-align: left; margin-top: 5px; font-size: 0.9em;")
                       )
                ),
                
                column(6,
                       tags$h3(icon("chart-bar"), "Visualisasi Data",style= "font-weight: bold;"),
                       div(class = "box-custom",
                           uiOutput("var_to_plot_ui"),
                           plotOutput("plotOutputUpload")
                            p(actionLink("help_viz", 
                                        "Klik di sini untuk detail lebih lanjut tentang grafik ini." ),
                             style = "text-align: left; margin-top: 5px; font-size: 0.9em;"),
                       )
                )
              )
      ),
      
    tabItem(tabName = "step3",
              fluidRow(
                p("Pada step 3 ini, Anda akan memulai proses pembangunan model regresi logistik biner. 
           ", style="text-align: center;"),
                box(title = "Pilih Variabel Prediktor", status = "warning", solidHeader = TRUE, width = 12,
                    uiOutput("predictor_selector_ui")
                )
              ),
              fluidRow(
                column(6,
                       tags$h3(icon("chart-bar"), "Uji Multikolinearitas",style= "font-weight: bold;"),
                       div(class = "box-custom", dataTableOutput("vif_table_dt")),
                       p(actionLink("help_multiko", 
                                    "Klik di sini untuk detail lebih lanjut tentang tabel ini." ),
                         style = "text-align: left; margin-top: 5px; font-size: 0.9em;")
                       
                  
                       
                      
                       ),
                column(6,
                       tags$h3(icon("chart-bar"), "Summary Logit",style= "font-weight: bold;"),
                       div(class = "box-custom", verbatimTextOutput("summarylogitOutputUpload"))
                       ),
                column(6,
                       tags$h3(icon("chart-bar"), "Summary Probit",style= "font-weight: bold;"),
                       div(class = "box-custom", verbatimTextOutput("summaryprobitOutputUpload"))
                ),
                column(6,
                       tags$h3(icon("chart-bar"), "Summary Cloglog",style= "font-weight: bold;"),
                       div(class = "box-custom", verbatimTextOutput("summarycloglogOutputUpload"))
                       )
              )
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
    
    #cek apakah .csv
    ext <- tools::file_ext(input$file1$name)
    if (tolower(ext) != "csv") {
      # Tampilkan popup peringatan jika format file salah
      sendSweetAlert(
        session = session,
        title = "Format File Salah!",
        text = "Mohon unggah file dengan format .csv",
        type = "error",
        btn_labels = "Oke",
        btn_colors = "#FFA500"
      )
    } else {
      # Lanjutkan proses jika format file benar
      output$file_info <- renderPrint({
        cat("File berhasil diunggah:\n")
        print(input$upload_file)
        # Di sini Anda bisa menambahkan kode untuk membaca dan memproses file
      })
      
      # Opsional: Tampilkan popup sukses
      sendSweetAlert(
        session = session,
        title = "Sukses!",
        text = paste0("File '", input$upload_file$name, "' berhasil diunggah."),
        type = "success",
        btn_labels = "Oke",
        btn_colors = "#FFA500"
      )
    }
    read.csv(input$file1$datapath)
  })
  
  #tampilkan pesan kesalahan jika bukan CSV
  output$file_warning <- renderText({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    if (tolower(ext) != "csv") {
      return("File dataset tidak sesuai. Harap unggah file dengan format .csv")
    } else {
      return("")
    }
  })
  
  #Preview data
  output$file1_contents <- renderTable({
    req(data())
    head(data())
  })
  
  #Tipe data setiap kolom
  output$tipe_variabel <- renderTable({
    req(data())
    df <- data()
    tipe <- sapply(df, function(col) {
      if (is.numeric(col)) return("Numerik")
      else if (is.factor(col) || is.character(col)) return("Kategorik")
      else return(class(col))
    })
    data.frame(Variabel = names(tipe), Tipe = unname(tipe), row.names = NULL)
  })
  
  # Pemilihan Variabel Respon (DIPERBARUI) ---
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
    selectInput("var_to_plot", "Pilih Variabel Prediktor:",
                choices = predictor_vars(), selected = predictor_vars()[1])
  })
  
  #ubah NA jadi modus dan median
  df_reactive <- reactive({
    req(data(), input$response_var)
    df <- data()
    
    if (is.numeric(df[[input$response_var]])) {
      df[[input$response_var]] <- as.factor(df[[input$response_var]])
    }
    
    df[] <- lapply(df, function(x) {
      # Jika kolom numerik, imputasi dengan median. Jika kategorik, dengan modus.
      if(is.numeric(x)) {
        x[is.na(x)] <- median(x, na.rm = TRUE)
      } else {
        x <- as.factor(x)
        x[is.na(x)] <- names(which.max(table(x)))
      }
      x
    })
    
    df
  })
  
  # Tabel missing value
  output$missing_table <- DT::renderDataTable({
    req(data())
    df <- data()
    sum_na <- data.frame()
    for (col in predictor_vars()) {
      if (col %in% names(df)) {
        na_count <- sum(is.na(df[[col]]))
        sum_na <- rbind(sum_na, data.frame(var = col, n_na = na_count))
      }
    }
    DT::datatable(sum_na, options = list(scrollX = TRUE))
  })
  
  observeEvent(input$help_missing_value, {
    sendSweetAlert(
      session = session,
      title = "Tabel Missing Value",
      text = paste0(
        "Tabel ini menunjukkan status kelengkapan data Anda:\n\n",
        "• Kolom 'var': Nama dari setiap variabel (kolom) dalam dataset.\n",
        "• Kolom 'n_na': Jumlah nilai yang hilang (missing values) pada variabel tersebut.\n\n",
        "Jika 'n_na' adalah 0, berarti variabel tersebut tidak memiliki nilai yang hilang. Jika 'n_na' lebih dari 0, Anda perlu memperhatikan variabel tersebut karena data yang hilang dapat memengaruhi analisis Anda."
      ),
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
      
    )
  })
  
  observeEvent(input$help_prep_data, {
    sendSweetAlert(
      session = session,
      title = "Tabel Preprocessing Data",
      text =" tabel ini menampilkan dataset setelah melalui proses pembersihan dan transformasi data. 
      Proses ini memastikan data siap dan berkualitas tinggi untuk analisis logistik biner.",
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
      # html = TRUE dihapus
    )
  })
  #tabel setelah preprocessing
  output$processed_table <- DT::renderDataTable({
    req(df_reactive())
    DT::datatable(df_reactive(), options = list(scrollX = TRUE))
  })
  
  # Data Summary
  output$summaryOutputUpload <- renderPrint({
    req(df_reactive())
    summary(df_reactive())
  })
  
  observeEvent(input$help_summary, {
    sendSweetAlert(
      session = session,
      title = "Data Summary",
      text = "Bagian 'Data Summary' ini menyajikan ringkasan statistik deskriptif untuk setiap variabel yang ada dalam dataset Anda.
      Ringkasan ini mencakup informasi penting seperti nilai minimum (Min.), kuartil pertama (1st Qu.), median, nilai rata-rata (Mean), kuartil ketiga (3rd Qu.), dan nilai maksimum (Max.).
      Untuk variabel kategorik, Anda akan melihat frekuensi kemunculan setiap kategori. Informasi ini sangat berguna untuk memahami distribusi, rentang nilai, dan potensi anomali dalam data sebelum dilakukan analisis lebih lanjut.",
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
      
    )
  })
  
  #Visualisasi berdasarkan variabel yang dipilih
  output$plotOutputUpload <- renderPlot({
    df <- df_reactive()
    var_to_plot <- input$var_to_plot
    
    if (!is.null(var_to_plot) && var_to_plot %in% names(df)) {
      ggplot(df, aes_string(x = var_to_plot)) +
        geom_bar(fill = "#FFB6C1") +
        labs(title = paste("Distribusi", var_to_plot),
             x = var_to_plot, y = "Jumlah") +
        theme_minimal(base_family = "Georgia") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  observeEvent(input$help_viz, {
    sendSweetAlert(
      session = session,
      title = "Visualisasi Data",
      text = "Panel 'Visualisasi Data' ini memungkinkan Anda untuk mengeksplorasi distribusi data berdasarkan variabel prediktor yang Anda pilih. Pilih salah satu variabel dari daftar di atas, dan grafik di bawahnya akan menampilkan distribusinya.
                         Visualisasi ini cukup penting untuk mengidentifikasi pola, sebaran, dan potensi hubungan antara variabel prediktor dengan variabel respon Anda.",
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
      
    )
  })
  
  
  #Pilih Prediktor
  # 1. Reactive untuk semua prediktor yang TERSEDIA
  available_predictors <- reactive({
    req(data(), input$response_var)
    setdiff(names(data()), input$response_var)
  })
  
  # 2. UI untuk memilih prediktor (menggunakan shinyWidgets::pickerInput)
  output$predictor_selector_ui <- renderUI({
    req(available_predictors())
    pickerInput(
      inputId = "selected_predictors_input",
      label = "Pilih satu atau lebih variabel prediktor untuk dianalisis:",
      choices = available_predictors(),
      selected = available_predictors(), # Defaultnya semua terpilih
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE)
    )
  })
  
  # 3. Reactive untuk prediktor yang DIPILIH oleh pengguna
  selected_predictors <- reactive({
    req(input$selected_predictors_input)
    input$selected_predictors_input
  })
  
  #Uji Multikolinearitas
  output$vif_table_dt <- DT::renderDataTable({
    req(model_logit())
    
    # Menangani error jika VIF tidak dapat dihitung
    vif_results <- tryCatch({
      # Pengecekan jumlah prediktor, vif() butuh > 1 prediktor
      if (length(attr(model_logit()$terms, "term.labels")) < 2) {
        stop("VIF tidak dapat dihitung. Dibutuhkan minimal 2 variabel prediktor.")
      }
      
      vif_vals <- car::vif(model_logit())
      
      # Jika vif_vals matriks (untuk faktor dengan > 2 level)
      if (is.matrix(vif_vals)) {
        vif_df <- data.frame(
          Variabel_Prediktor = rownames(vif_vals),
          GVIF = round(vif_vals[, "GVIF"], 4),
          Df = vif_vals[, "Df"],
          `GVIF_pangkat_1/2df` = round(vif_vals[, "GVIF^(1/(2*Df))"], 4)
        )
      } else {
        # Jika vektor biasa
        vif_df <- data.frame(
          Variabel_Prediktor = names(vif_vals),
          VIF = round(as.numeric(vif_vals), 4)
        )
      }
      vif_df
    }, error = function(e) {
      data.frame(Pesan = as.character(e$message))
    })
    
    DT::datatable(vif_results, rownames = FALSE, options = list(dom = 't'))
  })
  
  observeEvent(input$help_multiko, {
    sendSweetAlert(
      session = session,
      title = "Tabel Uji Multikolinearitas",
      text = "Tabel 'Uji Multikolinearitas' menampilkan hasil Variance Inflation Factor (VIF) 
      untuk setiap variabel prediktor yang dipilih. VIF adalah metrik yang digunakan untuk mendeteksi multikolinearitas. 
      Nilai VIF yang tinggi (umumnya > 5 atau > 10) menunjukkan adanya multikolinearitas yang signifikan, 
      yang dapat membuat koefisien model menjadi tidak stabil dan sulit diinterpretasikan.
      'GVIF' adalah nilai VIF yang disesuaikan atau metrik terkait untuk penilaian yang lebih tepat. 
      Pertimbangkan untuk menghapus atau menggabungkan variabel dengan VIF tinggi.",
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
      
    )
  })
  
  #Output GLM dengan 3 link function
  model_logit <- reactive({
    req(df_reactive(), input$response_var, selected_predictors())
    df <- df_reactive()
    formula <- as.formula(paste(input$response_var, "~", paste(selected_predictors(), collapse="+" )))
    glm(formula, data = df, family = binomial(link="logit"))
  })
  
  model_probit <- reactive({
    req(df_reactive(), input$response_var, selected_predictors())
    df <- df_reactive()
    formula <- as.formula(paste(input$response_var, "~", paste(selected_predictors(), collapse="+" )))
    glm(formula, data = df, family = binomial(link="probit"))
  })
  
  model_cloglog <- reactive({
    req(df_reactive(), input$response_var, selected_predictors())
    df <- df_reactive()
    formula <- as.formula(paste(input$response_var, "~", paste(selected_predictors(), collapse="+" )))
    glm(formula, data = df, family = binomial(link="cloglog"))
  })
  
  output$summarylogitOutputUpload <- renderPrint({
    req(model_logit())
    summary(model_logit())
  })
  
  output$summaryprobitOutputUpload <- renderPrint({
    req(model_probit())
    summary(model_probit())
  })
  
  output$summarycloglogOutputUpload <- renderPrint({
    req(model_cloglog())
    summary(model_cloglog())
  })
}

# Run app
shinyApp(ui, server)
