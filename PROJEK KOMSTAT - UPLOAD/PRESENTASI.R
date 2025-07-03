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
              h2("Quiz Section")
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
}
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
# Run app
shinyApp(ui, server)
