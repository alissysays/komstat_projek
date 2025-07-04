library(shiny)
library(ggplot2)
library(DT)
library(shinydashboard)
library(dplyr)
library(ggcorrplot)
library(shinyWidgets)
library(car)
library(pROC)

# batas maksimal upload file (max 30 MB)
options(shiny.maxRequestSize = 30*1024^2)

# UI
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = span("Tahap Analisis", 
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
             tags$h1("ANALISIS LOGISTIK BINER", style = "font-weight: bold; color:#9F99D1;text-shadow: 1px 1px 2px black;")
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
                p("Tiga link function yang umum digunakan adalah logit, probit, dan complementary log-log (cloglog). 
                  Fungsi logit mengubah menjadi log-odds dengan rumus, dan merupakan yang paling umum digunakan 
                  karena interpretasinya yang jelas terhadap odds. Fungsi probit menggunakan invers dari distribusi
                  normal standar kumulatif, cocok untuk data yang diasumsikan mengikuti distribusi normal laten. 
                  Sedangkan cloglog menggunakan, lebih sensitif untuk kejadian jarang dan sering digunakan 
                  dalam analisis survival. Pemilihan link function tergantung pada asumsi distribusi laten dan konteks 
                  aplikasinya."),
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
                    DT::dataTableOutput("processed_table"),
                    p(actionLink("help_prep_data","Klik di sini untuk detail lebih lanjut tentang tabel ini." ),
                      style = "text-align: left; margin-top: 5px; font-size: 0.9em;"))
          
                
              )
      ),
      
      tabItem(tabName = "step2",
              
              fluidRow(
                p("Step 2 akan memandu Anda untuk mengeksplorasi struktur dan karakteristik dasar dataset Anda.", style="text-align: center;"),
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
                           plotOutput("plotOutputUpload"),
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
                    
              h2("Kecocokan Model", style="text-align: center; font-weight: bold;"),
              br(),
              fluidRow(
                p("Pada step 4 ini, Anda akan mengevaluasi kecocokan dan kinerja model regresi logistik biner yang telah dibangun", style="text-align: center;"),
                
                column(6,
                       tags$h3(icon("chart-bar"), "Koefisien Plot Logit",
                               actionLink("help_coef_logit", icon("question-circle")), # Tambahkan di sini
                               style= "font-weight: bold;"),
                       div(class = "box-custom",plotOutput("coef_plot_logit"))
                ),
                
                column(6,
                       tags$h3(icon("chart-bar"), "Koefisien Plot Probit",
                               actionLink("help_coef_probit", icon("question-circle")), # Tambahkan di sini
                               style= "font-weight: bold;"),
                       div(class = "box-custom",plotOutput("coef_plot_probit"))
                ),
                
                column(6,
                       tags$h3(icon("chart-bar"), "Koefisien Plot Cloglog",
                               actionLink("help_coef_cloglog", icon("question-circle")), # Tambahkan di sini
                               style= "font-weight: bold;"),
                       div(class = "box-custom",plotOutput("coef_plot_cloglog"))
                ),
                
                
                column(6,
                       tags$h3(icon("chart-bar"), "ROC Plot", actionLink("help_roc_plot", icon("question-circle")),
                               style= "font-weight: bold;"),
                       div(class = "box-custom",plotOutput("roc_plot"))
                )
              )
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
              
              fluidRow(
                column(12,
                       h4("Soal 3", style="font-weight:bold;"),
                       p("Jika dilakukan uji Wald untuk menganalisis pengaruh memiliki kebiasaan minum kopi dengan hasil tes kebugaran, didapatkan hasil sebesar -3,77 dan P-Value mendekati 0. Apa yang dapat diinterpretasikan?"),
                       radioButtons("quiz3", "Pilihan:", 
                                    choices = c("A. Tolak H0 yang memiliki arti bahwa memiliki kebiasaan minum kopi tidak memiliki pengaruh signifikan terhadap hasil tes kebugaran", 
                                                "B. Tolak H0 yang memiliki arti bahwa memiliki kebiasaan minum kopi memiliki pengaruh signifikan terhadap hasil tes kebugaran",
                                                "C. Terima H0 yang memiliki arti bahwa memiliki kebiasaan minum kopi memiliki pengaruh signifikan terhadap hasil tes kebugaran",
                                                "D. Terima H0 yang memiliki arti bahwa memiliki kebiasaan minum kopi tidak dapat disimpulkan",
                                                "E. Hasil tersebut tidak dapat dibuat interpretasi"), 
                                    selected = character(0)),
                       # Tempat untuk menampilkan feedback jawaban 3
                       uiOutput("quiz3_feedback")),
              ),
              hr(),
              
              fluidRow(
                column(12,
                       h4("Soal 4", style="font-weight:bold;"),
                       tags$img(src = "quiz_4.png", width = "100%"),
                       p("Berikut adalah hasil dari Generalized Linear Model untuk mengetahui apakah ideologi manusia benar berasal dari hewan atau tidak. Berdasarkan output Generalized Linear Model tersebut, interval kepercayaan Wald 95% untuk kebenaran ideologi tersebut adalah..."),
                       radioButtons("quiz4", "Pilihan:", 
                                    choices = c("A. (0,4 ; 0,8)", 
                                                "B. (0,5 ; 0,7)",
                                                "C. (0,1 ; 0,4)",
                                                "D. (0,12 ; 0,23)",
                                                "E. (0,4 ; 0,6)"), 
                                    selected = character(0)),
                       # Tempat untuk menampilkan feedback jawaban 4
                       uiOutput("quiz4_feedback")),
              ),
              hr(),
              
              fluidRow(
                column(12,
                       h4("Soal 5", style="font-weight:bold;"),
                       p("Selain uji Chi-Square, ukuran asosiasi apa yang sering digunakan untuk mengevaluasi hubungan antara dua variabel kategorik?"),
                       radioButtons("quiz5", "Pilihan:", 
                                    choices = c("A. Mean Absolute Error", 
                                                "B. R-Squared",
                                                "C. Koefisien korelasi Pearson",
                                                "D. Risiko relatif dan odds rasio",
                                                "E. Mean Absolute Percentage Error"), 
                                    selected = character(0)),
                       # Tempat untuk menampilkan feedback jawaban 5
                       uiOutput("quiz5_feedback")),
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
              h2("Frequently Asked Questions (FAQ)", style = "text-align:center; font-weight:bold;"),
              p("Punya pertanyaan? Mungkin jawabannya ada di bawah ini.", style = "text-align:center;"),
              hr(),
              
              h3("Apa itu Regresi Logistik Biner?", style="font-weight:bold;"),
              p("Regresi logistik biner adalah metode statistik yang digunakan untuk memodelkan hubungan antara satu atau lebih variabel prediktor (independen) dengan sebuah variabel respon (dependen) yang bersifat biner atau dikotomus. Artinya, variabel respon hanya memiliki dua kemungkinan nilai, seperti 'Ya/Tidak', 'Sukses/Gagal', atau 'Sakit/Sehat'."),
              
              h3("Kapan saya harus menggunakan Regresi Logistik?", style="font-weight:bold;"),
              p("Gunakan regresi logistik ketika Anda ingin memprediksi hasil dari variabel dependen yang hanya memiliki dua kategori. Contohnya, memprediksi apakah seorang nasabah akan gagal bayar pinjaman (ya/tidak) berdasarkan pendapatan dan riwayat kreditnya."),
              
              h3("Apa perbedaan antara link function logit, probit, dan cloglog?", style="font-weight:bold;"),
              p("Ketiganya digunakan untuk mengubah probabilitas (yang nilainya antara 0 dan 1) menjadi skala kontinu. Perbedaannya terletak pada asumsi distribusi yang mendasarinya:",
                tags$ul(
                  tags$li(tags$b("Logit:"), "Paling umum digunakan. Mengasumsikan distribusi logistik. Koefisiennya dapat diinterpretasikan sebagai log-odds."),
                  tags$li(tags$b("Probit:"), "Mengasumsikan distribusi normal standar kumulatif. Sering digunakan dalam ekonometrika."),
                  tags$li(tags$b("Cloglog (Complementary log-log):"), "Bersifat asimetris. Cocok digunakan ketika probabilitas salah satu kategori sangat kecil atau sangat besar (kejadian langka).")
                )
              ),
              
              h3("Bagaimana cara menginterpretasikan koefisien dalam model logit?", style="font-weight:bold;"),
              p("Koefisien (β) dalam model logit menunjukkan perubahan dalam log-odds untuk setiap kenaikan satu unit pada variabel prediktor. Untuk interpretasi yang lebih mudah, koefisien sering diubah menjadi odds ratio dengan menghitung eksponensialnya (exp(β)). Odds ratio > 1 berarti variabel meningkatkan peluang kejadian, sedangkan odds ratio < 1 berarti menurunkannya."),
              
              hr(),
              
              h2("Kirim Komentar dan Masukan", style = "text-align:center; font-weight:bold;"),
              p("Kami sangat menghargai masukan Anda untuk pengembangan dashboard ini. Silakan isi form di bawah ini.", style = "text-align:center;"),
              
              fluidRow(
                column(6, textInput("first_name", "Nama Depan:", placeholder = "Contoh: Budi")),
                column(6, textInput("last_name", "Nama Belakang:", placeholder = "Contoh: Santoso"))
              ),
              fluidRow(
                column(12, textInput("email", "Email:", placeholder = "Contoh: budi.santoso@example.com"))
              ),
              fluidRow(
                column(12, textAreaInput("comment", "Komentar Anda:", placeholder = "Tulis komentar atau pertanyaan Anda di sini...", height = "120px"))
              ),
              fluidRow(
                column(12, align="center",
                       actionButton("submit_faq", "Kirim Masukan", icon = icon("paper-plane"), class = "btn-success")
                )
              )
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
  
  #Coef Plot
  create_coef_plot <- function(model, title) {
    if (is.null(model)) return(NULL)
    
    coef_df <- as.data.frame(summary(model)$coef)
    coef_df$Variable <- rownames(coef_df)
    colnames(coef_df) <- c("Estimate", "Std.Error", "z.value", "Pr...z..", "Variable")
    
    coef_df <- coef_df %>% filter(Variable != "(Intercept)")
    
    coef_df$lower_ci <- coef_df$Estimate - 1.96 * coef_df$Std.Error
    coef_df$upper_ci <- coef_df$Estimate + 1.96 * coef_df$Std.Error
    
    ggplot(coef_df, aes(x = Variable, y = Estimate, ymin = lower_ci, ymax = upper_ci)) +
      geom_pointrange(color = "#FFA500", size = 0.8) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#00BFFF") +
      coord_flip() + # Flip coordinates 
      labs(title = title,
           x = "Variabel Prediktor",
           y = "Estimasi Koefisien") +
      theme_minimal(base_family = "Georgia") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  }
  
  output$coef_plot_logit <- renderPlot({
    create_coef_plot(model_logit(), "Koefisien Model Logit")
  })
  
  output$coef_plot_probit <- renderPlot({
    create_coef_plot(model_probit(), "Koefisien Model Probit")
  })
  
  output$coef_plot_cloglog <- renderPlot({
    create_coef_plot(model_cloglog(), "Koefisien Model Cloglog")
  })
  

  observeEvent(input$help_coef_logit, {
    sendSweetAlert(
      session = session,
      title = "Cara Membaca Koefisien Plot Logit",
      text = paste0("Plot ini menunjukkan pengaruh setiap variabel prediktor terhadap log-odds variabel target Anda:\n\n",
                    "• Titik (Dot): Menunjukkan nilai estimasi koefisien (pengaruh) dari variabel prediktor tersebut.\n",
                    "• Garis Horizontal: Interval kepercayaan (confidence interval) untuk estimasi koefisien. Jika garis ini melewati garis nol vertikal, berarti variabel tersebut tidak signifikan secara statistik pada tingkat kepercayaan tertentu.\n",
                    "• Garis Vertikal Putus-putus (di tengah): Ini adalah garis nol. Jika titik koefisien berada di kanan garis ini, variabel memiliki pengaruh positif. Jika di kiri, pengaruhnya negatif.\n\n",
                    "Semakin jauh titik dari garis nol (baik ke kanan atau kiri), semakin kuat pengaruh variabel tersebut."),
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
    )
  })
  
  observeEvent(input$help_coef_probit, {
    sendSweetAlert(
      session = session,
      title = "Cara Membaca Koefisien Plot Probit",
      text = paste0("Plot ini serupa dengan Koefisien Plot Logit, namun menggunakan fungsi *link* 'probit' (berdasarkan distribusi normal kumulatif). Interpretasi dasarnya sama:\n\n" ,
        "• Titik (Dot): Nilai estimasi koefisien.\n" ,
        "• Garis Horizontal: Interval kepercayaan. Jika melewati garis nol, tidak signifikan.\n" ,
        "• Garis Vertikal Putus-putus (di tengah): Garis nol. Kanan berarti pengaruh positif, kiri berarti pengaruh negatif.\n\n" ,
        "Model Probit seringkali menghasilkan interpretasi yang serupa dengan Logit, tetapi dengan asumsi distribusi yang berbeda. Perbedaan koefisien antara kedua model biasanya kecil."),
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
    )
  })
  
  observeEvent(input$help_coef_cloglog, {
    sendSweetAlert(
      session = session,
      title = "Cara Membaca Koefisien Plot Cloglog",
      text = paste0("Plot ini menampilkan estimasi koefisien untuk model regresi logistik yang menggunakan fungsi link 'cloglog' (complementary log-log). Fungsi cloglog sering digunakan ketika kejadian yang diminati memiliki probabilitas yang sangat kecil. Interpretasinya serupa dengan logit dan probit:\n\n" ,
        "• Titik (Dot): Nilai estimasi koefisien.\n" ,
        "• Garis Horizontal: Interval kepercayaan. Jika melewati garis nol, tidak signifikan.\n" ,
        "• Garis Vertikal Putus-putus (di tengah): Garis nol. Kanan berarti pengaruh positif, kiri berarti pengaruh negatif.\n\n" ,
        "Perhatikan bahwa besar koefisien antar fungsi link (logit, probit, cloglog) tidak bisa langsung dibandingkan karena skala yang berbeda, tetapi arah pengaruh (positif/negatif) dan signifikansi statistik umumnya konsisten."),
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
    )
  })
  
  #AUC dan ROC Plot
  output$roc_plot <- renderPlot({
    df <- df_reactive()
    
    roc_logit <- roc(df[[input$response_var]], predict(model_logit(), type = "response"))
    roc_probit <- roc(df[[input$response_var]], predict(model_probit(), type = "response"))
    roc_cloglog <- roc(df[[input$response_var]], predict(model_cloglog(), type = "response"))
    
    auc_logit_val <- auc(roc_logit)
    auc_probit_val <- auc(roc_probit)
    auc_cloglog_val <- auc(roc_cloglog)
    
    plot(roc_logit, col = "red", lwd = 2, main = "ROC Curve Ketiga Model")
    plot(roc_probit, col = "purple", lwd = 2, add = TRUE)
    plot(roc_cloglog, col = "#00BFFF", lwd = 2, add = TRUE)
    
    legend("bottomright", legend = c("Logit", "Probit", "Cloglog"),
           col = c("red", "purple", "#00BFFF"), lwd = 2)
    
    # AUC
    text(0, 0.35, paste("AUC Logit:", round(auc_logit_val, 3)), col = "red", cex = 0.9)
    text(0, 0.30, paste("AUC Probit:", round(auc_probit_val, 3)), col = "purple", cex = 0.9)
    text(0, 0.25, paste("AUC Cloglog:", round(auc_cloglog_val, 3)), col = "#00BFFF", cex = 0.9)
  })
  
  observeEvent(input$help_roc_plot, {
    sendSweetAlert(
      session = session,
      title = "Cara Membaca Kurva ROC dan AUC",
      text = paste0("Kurva ROC membantu Anda menilai kinerja model klasifikasi:\n\n" ,
        "• Sumbu X (False Positive Rate / 1 - Spesifisitas): Proporsi kasus negatif yang salah diklasifikasikan sebagai positif.\n" ,
        "• Sumbu Y (True Positive Rate / Sensitivitas): Proporsi kasus positif yang berhasil diklasifikasikan sebagai positif.\n\n" ,
        "• Garis Diagonal (dari (0,0) ke (1,1)): Menunjukkan model yang tidak lebih baik dari menebak secara acak.\n" ,
        "• Kurva Model: Semakin dekat kurva model ke pojok kiri atas, semakin baik kinerja model dalam membedakan kelas.\n\n" ,
        "• AUC (Area Under the Curve): Angka ini (0 hingga 1) adalah ringkasan kinerja keseluruhan model. Semakin dekat nilai AUC ke 1, semakin baik model Anda. AUC 0.5 berarti model tidak lebih baik dari tebakan acak, sementara AUC 1 berarti model sempurna."),
      type = "question",
      btn_labels = "Oke",
      btn_colors = "#FFA500"
    )
  })
  
  #Menampilkan Jawaban QUiz
  observeEvent(input$check_quiz, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm_check",
      title = "Konfirmasi Jawaban",
      text = "Apakah Anda sudah yakin dengan semua jawaban Anda?",
      type = "question", 
      btn_labels = c("Tidak", "Ya"), 
      btn_colors = c("#D33", "#FFA500") 
    )
  })
  
  observeEvent(input$confirm_check, {
    req(input$confirm_check)
    
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
    
    #Feedback untuk soal 3
    output$quiz3_feedback <- renderUI({
      req(input$quiz3) # Pastikan pengguna sudah menjawab
      if (input$quiz3 == "B. Tolak H0 yang memiliki arti bahwa memiliki kebiasaan minum kopi memiliki pengaruh signifikan terhadap hasil tes kebugaran") {
        tags$p("Benar!", style = "color: green; font-weight: bold;")
      } else {
        tags$p("Salah. Jawaban yang benar adalah B. Tolak H0 yang memiliki arti bahwa memiliki kebiasaan minum kopi memiliki pengaruh signifikan terhadap hasil tes kebugaran", style = "color: red; font-weight: bold;")
      }
    })
    
    #Feedback untuk soal 4
    output$quiz4_feedback <- renderUI({
      req(input$quiz4) # Pastikan pengguna sudah menjawab
      if (input$quiz4 == "E. (0,4 ; 0,6)") {
        tags$p("Benar!", style = "color: green; font-weight: bold;")
      } else {
        tags$p("Salah. Jawaban yang benar adalah E. (0,4 ; 0,6)", style = "color: red; font-weight: bold;")
      }
    })
    
    #Feedback untuk soal 5
    output$quiz5_feedback <- renderUI({
      req(input$quiz5) # Pastikan pengguna sudah menjawab
      if (input$quiz5 == "D. Risiko relatif dan odds rasio") {
        tags$p("Benar!", style = "color: green; font-weight: bold;")
      } else {
        tags$p("Salah. Jawaban yang benar adalah D. Risiko relatif dan odds rasio", style = "color: red; font-weight: bold;")
      }
    })
    
  })
  
  # FAQs - SUBMIT KOMENTAR
  observeEvent(input$submit_faq, {
    req(input$first_name, input$comment) 
    
    feedback_data <- data.frame(
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      FirstName = input$first_name,
      LastName = input$last_name,
      Email = input$email,
      Comment = input$comment
    )
    
    # Gunakan tryCatch untuk menangani error penulisan file
    tryCatch({
      # Tulis data ke file CSV
      write.table(
        feedback_data,
        "feedback_log.csv",
        sep = ",",
        append = TRUE,
        row.names = FALSE,
        col.names = !file.exists("feedback_log.csv")
      )
      
      # Jika berhasil, tampilkan notifikasi sukses dan kosongkan form
      showNotification("Terima kasih! Masukan Anda telah kami terima.", type = "message", duration = 5)
      
      updateTextInput(session, "first_name", value = "")
      updateTextInput(session, "last_name", value = "")
      updateTextInput(session, "email", value = "")
      updateTextAreaInput(session, "comment", value = "")
      
    }, error = function(e) {
      # Jika gagal, tampilkan notifikasi error
      showNotification(
        paste("Gagal menyimpan masukan. Pastikan server memiliki izin tulis. Error:", e$message), 
        type = "error", 
        duration = 10
      )
    })
  })
}

# Run app
shinyApp(ui, server)
