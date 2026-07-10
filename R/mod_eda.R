# ==============================================================================
# MODUL: EKSPLORASI DATA ANALISIS (EDA) - INTERACTIVE & STATISTICAL EDITION
# ==============================================================================
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(reshape2)
library(DT)
library(skimr)
library(ggplot2)
library(plotly) 
library(car)

# 1. ANTARMUKA PENGGUNA (UI)
mod_eda_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("eda_content"))
}

# 2. LOGIKA SERVER
mod_eda_server <- function(id, dataset_reaktif) {
  moduleServer(id, function(input, output, session) {
    
    # ---------------------------------------------------------
    # PENANGANAN MISSING VALUE (OTOMATIS)
    # ---------------------------------------------------------
    df_clean <- reactive({
      df <- dataset_reaktif()
      req(df)
      total_na <- sum(is.na(df))
      
      if(total_na > 0) {
        showNotification(paste("Ditemukan", total_na, "missing values. Baris dengan data kosong dihapus otomatis."), type = "warning")
        df <- na.omit(df) 
      }
      return(df)
    })
    
    # ---------------------------------------------------------
    # RENDER UI UTAMA
    # ---------------------------------------------------------
    output$eda_content <- renderUI({
      df <- df_clean()
      req(df) 
      semua_variabel <- names(df)
      
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          
          # CARD: DATA SUMMARY
          card(
            card_header(
              class = "bg-primary text-white d-flex justify-content-between align-items-center", 
              span(bs_icon("clipboard-data"), " Data Summary"),
              actionButton(session$ns("help_summary"), "", icon = icon("question"), class = "btn-warning btn-sm rounded-circle")
            ),
            card_body(
              p(em("Klik baris pada tabel untuk melihat detail penjelasan statistik secara sederhana.")),
              div(style = "overflow-x: auto; margin-bottom: 10px;", DT::dataTableOutput(session$ns("summary_table")))
            )
          ),
          
          # CARD: VISUALISASI DATA (INTERAKTIF PLOTLY)
          card(
            card_header(
              class = "bg-primary text-white d-flex justify-content-between align-items-center", 
              span(bs_icon("bar-chart"), " Visualisasi Distribusi"),
              actionButton(session$ns("help_visual"), "", icon = icon("question"), class = "btn-warning btn-sm rounded-circle")
            ),
            wellPanel(style = "background-color: #f8f9fa;",
                      selectInput(session$ns("var_prediktor"), "Pilih Variabel Prediktor:", choices = semua_variabel)
            ),
            plotly::plotlyOutput(session$ns("plot_distribusi_utama")),
            hr(),
            uiOutput(session$ns("interpretasi_plot_utama"))
          )
        ),
        
        br(), hr(), br(),
        
        # CARD: MATRIKS KORELASI (INTERAKTIF PLOTLY) & INTERPRETASI
        card(
          card_header(class = "bg-info text-white", bs_icon("grid-3x3"), " Matriks Korelasi Numerik (Heatmap)"),
          plotly::plotlyOutput(session$ns("plot_korelasi_plotly"), height = "450px"),
          hr(),
          uiOutput(session$ns("interpretasi_korelasi"))
        ),
        
        br(),
        
        # CARD: UJI MULTIKOLINEARITAS (VIF)
        card(
          card_header(
            class = "bg-info text-white d-flex justify-content-between align-items-center", 
            span(bs_icon("exclamation-triangle"), " Uji Multikolinearitas (VIF)"),
            actionButton(session$ns("help_vif"), "", icon = icon("question"), class = "btn-warning btn-sm rounded-circle")
          ),
          card_body(
            wellPanel(style = "background-color: #e1f5fe; border-color: #b3e5fc;",
                      h6(strong("Setup Variabel Respon")),
                      p("Sistem perlu menjalankan regresi di balik layar untuk menghitung nilai VIF. Silakan pilih variabel target (Y) Anda"),
                      selectInput(session$ns("var_respon"), "Variabel Respon (Y)", choices = semua_variabel, selected = tail(semua_variabel, 1))
            ),
            div(style = "overflow-x: auto; margin-bottom: 20px;", DT::dataTableOutput(session$ns("tabel_vif"))),
            hr(), 
            uiOutput(session$ns("interpretasi_vif"))
          )
        )
      )
    })
    
    # ---------------------------------------------------------
    # TOMBOL BANTUAN 
    # ---------------------------------------------------------
    observeEvent(input$help_summary, {
      showModal(modalDialog(
        title = "Data Summary",
        p("Bagian ini menyajikan ringkasan awal data Anda. Sangat berguna untuk mengintip rentang nilai data sebelum dianalisis lebih jauh."),
        easyClose = TRUE, footer = modalButton("Oke")
      ))
    })
    
    observeEvent(input$help_visual, {
      showModal(modalDialog(
        title = "Visualisasi Data",
        p("Gunakan panel ini untuk melihat distribusi empiris dari variabel Anda. Grafik bersifat interaktif, silakan arahkan kursor (hover) ke atas grafik untuk melihat nilai eksaknya."),
        easyClose = TRUE, footer = modalButton("Oke")
      ))
    })
    
    observeEvent(input$help_vif, {
      showModal(modalDialog(
        title = "Tabel Uji Multikolinearitas",
        p("VIF (Variance Inflation Factor) mendeteksi apakah ada variabel prediktor yang berkorelasi sangat kuat satu sama lain. Nilai VIF yang tinggi menandakan variabel tersebut sebaiknya dibuang agar model stabil."),
        easyClose = TRUE, footer = modalButton("Oke")
      ))
    })
    
    # ---------------------------------------------------------
    # SUMMARY TABLE & POP-UP INTERPRETASI
    # ---------------------------------------------------------
    skim_data <- reactive({ skimr::skim(df_clean()) })
    var_terpilih <- reactiveVal(NULL)
    
    output$summary_table <- DT::renderDataTable({
      req(skim_data())
      DT::datatable(skim_data(), selection = 'single', rownames = FALSE,
                    class = "display nowrap compact hover", options = list(pageLength = 5, scrollX = TRUE))
    })
    
    observeEvent(input$summary_table_rows_selected, {
      req(input$summary_table_rows_selected)
      nama_var <- skim_data()$skim_variable[input$summary_table_rows_selected]
      var_terpilih(nama_var)
      
      showModal(modalDialog(
        title = paste("Rincian Statistik:", nama_var), size = "m", easyClose = TRUE,
        uiOutput(session$ns("modal_interpretasi")),
        footer = modalButton("Tutup")
      ))
    })
    
    output$modal_interpretasi <- renderUI({
      req(var_terpilih())
      data_var <- df_clean()[[var_terpilih()]]
      
      if(is.numeric(data_var)) {
        res <- summary(data_var)
        sd_val <- sd(data_var, na.rm = TRUE)
        
        tagList(
          h5(strong("Angka Ringkasan")),
          tags$ul(
            tags$li(strong("Minimum: "), round(res[["Min."]], 2)),
            tags$li(strong("Kuartil Bawah (Q1): "), round(res[["1st Qu."]], 2)),
            tags$li(strong("Median: "), round(res[["Median"]], 2)),
            tags$li(strong("Rata-rata (Mean): "), round(res[["Mean"]], 2)),
            tags$li(strong("Kuartil Atas (Q3): "), round(res[["3rd Qu."]], 2)),
            tags$li(strong("Maksimum: "), round(res[["Max."]], 2)),
            tags$li(strong("Standar Deviasi: "), round(sd_val, 2))
          ),
          hr(),
          h5(strong("Analisis Karakteristik Distribusi:")),
          wellPanel(style = "background-color: #f8f9fa;",
                    p("Berdasarkan data observasi, ukuran tendensi sentral untuk variabel ", strong(var_terpilih()), " menunjukkan nilai mean sebesar ", strong(round(res[["Mean"]], 2)), " dengan nilai median berada pada ", strong(round(res[["Median"]], 2)), "."),
                    p("Dispersi data bergerak dari nilai minimum ", strong(round(res[["Min."]], 2)), " hingga mencapai nilai maksimum sebesar ", strong(round(res[["Max."]], 2)), ".")
          )
        )
      } else {
        freq_tbl <- as.data.frame(table(data_var))
        colnames(freq_tbl) <- c("Kategori", "Frekuensi")
        kat_top <- names(which.max(table(data_var)))
        
        tagList(
          h5(strong("Frekuensi Kategori")),
          renderTable(freq_tbl, width = "100%", striped = TRUE),
          hr(),
          h5(strong("Analisis Karakteristik Kategori:")),
          wellPanel(style = "background-color: #f8f9fa;",
                    p("Variabel ", strong(var_terpilih()), " merupakan data kategorik. Distribusi frekuensi mutlak untuk setiap kategori tersaji pada tabel di atas."),
                    p("Modus dari variabel ini diwakili oleh kategori ", strong(kat_top), " sebagai kelompok dengan frekuensi tertinggi dalam sampel data.")
          )
        )
      }
    })
    
    # ---------------------------------------------------------
    # VISUALISASI UTAMA INTERAKTIF (PLOTLY)
    # ---------------------------------------------------------
    output$plot_distribusi_utama <- plotly::renderPlotly({
      df <- df_clean()
      req(df, input$var_prediktor)
      var_to_plot <- input$var_prediktor
      
      if(is.numeric(df[[var_to_plot]])) {
        p <- ggplot(df, aes_string(x = var_to_plot)) + 
          geom_histogram(fill = "#3498db", color = "white", bins = 30) +
          labs(title = paste("Distribusi Numerik:", var_to_plot), x = var_to_plot, y = "Frekuensi") +
          theme_minimal(base_family = "sans") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        p <- ggplot(df, aes_string(x = var_to_plot)) + 
          geom_bar(fill = "#3498db", color = "white") +
          labs(title = paste("Frekuensi Kategori:", var_to_plot), x = var_to_plot, y = "Jumlah Observasi") +
          theme_minimal(base_family = "sans") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      }
      
      plotly::ggplotly(p) %>% plotly::config(displayModeBar = FALSE)
    })
    
    output$interpretasi_plot_utama <- renderUI({
      df <- df_clean()
      req(df, input$var_prediktor)
      data_var <- df[[input$var_prediktor]]
      
      if(is.numeric(data_var)) {
        mean_val <- mean(data_var, na.rm = TRUE)
        median_val <- median(data_var, na.rm = TRUE)
        
        skew_text <- if(mean_val > median_val * 1.05) {
          "cenderung positif (right-skewed)"
        } else if (mean_val < median_val * 0.95) {
          "cenderung negatif (left-skewed)"
        } else {
          "relatif simetris"
        }
        
        tagList(
          h6(strong("Interpretasi Statistik:")),
          p("Berdasarkan histogram di atas, variabel numerik ", strong(input$var_prediktor), " menunjukkan ukuran tendensi sentral dengan rata-rata ", strong(round(mean_val, 2)), " dan median ", strong(round(median_val, 2)), "."),
          p("Perbandingan antara rata-rata dan median mengindikasikan bahwa distribusi data ini ", strong(skew_text), ".")
        )
      } else {
        tbl <- table(data_var)
        kat_max <- names(which.max(tbl))
        prop_max <- round(max(tbl) / sum(tbl) * 100, 2)
        
        tagList(
          h6(strong("Interpretasi Statistik:")),
          p("Berdasarkan bar chart di atas, variabel kategorik ", strong(input$var_prediktor), " memiliki modus (frekuensi terbanyak) pada kelas/kategori ", strong(kat_max), "."),
          p("Kategori tersebut mendominasi dengan proporsi frekuensi relatif sebesar ", strong(paste0(prop_max, "%")), " dari total observasi data.")
        )
      }
    })
    
    # ---------------------------------------------------------
    # MATRIKS KORELASI (INTERAKTIF PLOTLY) & INTERPRETASI
    # ---------------------------------------------------------
    matriks_kor_reaktif <- reactive({
      df <- df_clean()
      num_df <- df[sapply(df, is.numeric)]
      if(ncol(num_df) < 2) return(NULL)
      cor(num_df, use = "complete.obs")
    })
    
    output$plot_korelasi_plotly <- plotly::renderPlotly({
      matriks_kor <- matriks_kor_reaktif()
      req(matriks_kor)
      
      mat_melt <- reshape2::melt(matriks_kor)
      
      p <- ggplot(mat_melt, aes(x = Var1, y = Var2, fill = value, 
                                text = paste("Variabel X:", Var1, "<br>Variabel Y:", Var2, "<br>Korelasi:", round(value, 2)))) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(value, 2)), color = ifelse(abs(mat_melt$value) > 0.6, "white", "black"), size = 4) +
        scale_fill_gradient2(low = "#ca0020", mid = "#f7f7f7", high = "#0571b0", midpoint = 0, limit = c(-1,1), name = "Korelasi") +
        theme_minimal(base_family = "sans") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title.x = element_blank(), axis.title.y = element_blank())
      
      plotly::ggplotly(p, tooltip = "text") %>% 
        plotly::layout(showlegend = FALSE) %>% 
        plotly::config(displayModeBar = FALSE)
    })
    
    output$interpretasi_korelasi <- renderUI({
      matriks_kor <- matriks_kor_reaktif()
      req(matriks_kor)
      
      mat_df <- as.data.frame(matriks_kor) %>% 
        tibble::rownames_to_column(var = "V1") %>% 
        pivot_longer(cols = -V1, names_to = "V2", values_to = "Korelasi") %>% filter(V1 != V2)
      
      high_corr <- mat_df %>% filter(abs(Korelasi) > 0.75) %>% group_by(V1) %>% 
        summarise(Vars = paste(unique(V2), collapse = ", "))
      
      if (nrow(high_corr) > 0) {
        tagList(
          p(strong("Peringatan Korelasi Tinggi", style="color:#ca0020;")),
          p("Pasangan variabel berikut memiliki korelasi linear yang kuat (|r| > 0.75):"),
          tags$ul(apply(high_corr, 1, function(row) tags$li(paste0(row["V1"], " ↔ ", row["Vars"])))),
          p(em("Sangat disarankan untuk mengecek hasil Uji VIF di tabel bawah."))
        )
      } else {
        tagList(
          p(strong("Korelasi Terkendali", style="color:#0571b0;")),
          p("Tidak ditemukan indikasi multikolinearitas yang terlampau kuat (|r| > 0.75) antar prediktor numerik.")
        )
      }
    })
    
    # ---------------------------------------------------------
    # UJI MULTIKOLINEARITAS (VIF) - PROTECTED VERSION
    # ---------------------------------------------------------
    model_logit_reaktif <- reactive({
      df <- df_clean()
      req(input$var_respon)
      
      target_levels <- length(unique(na.omit(df[[input$var_respon]])))
      
      # Menolak eksekusi jika variabel Y tidak memiliki tepat 2 kategori
      if(target_levels != 2) {
        return(list(error_msg = paste("Untuk menjalankan uji multikolinearitas logistik, variabel respon harus bertipe biner (tepat 2 kategori). Saat ini variabel '", input$var_respon, "' memiliki ", target_levels, " kategori.")))
      }
      
      df[[input$var_respon]] <- as.factor(df[[input$var_respon]])
      form <- as.formula(paste("`", input$var_respon, "` ~ .", sep = ""))
      
      mod <- tryCatch({
        glm(formula = form, data = df, family = binomial(link = "logit"))
      }, error = function(e) { return(list(error_msg = e$message)) })
      
      return(mod)
    })
    
    output$tabel_vif <- DT::renderDataTable({
      mod <- model_logit_reaktif()
      
      if(is.list(mod) && !is.null(mod$error_msg)) {
        return(DT::datatable(data.frame(Peringatan = mod$error_msg), options = list(dom = 't', scrollX = TRUE), rownames = FALSE))
      }
      if(is.null(mod)) return(NULL)
      
      # Proteksi struktural terhadap perfect multicollinearity (Aliased Coefficients)
      if(any(is.na(coef(mod)))) {
        return(DT::datatable(data.frame(Pesan = "Gagal menghitung VIF. Terdeteksi adanya multikolinearitas sempurna (Aliased Coefficients) pada variabel prediktor."), options = list(dom = 't', scrollX = TRUE), rownames = FALSE))
      }
      
      vif_vals <- tryCatch(car::vif(mod), error = function(e) NULL)
      if(is.null(vif_vals)) return(DT::datatable(data.frame(Pesan = "Gagal memproses perhitungan nilai VIF."), options = list(dom = 't', scrollX = TRUE), rownames = FALSE))
      
      if(is.matrix(vif_vals) || is.data.frame(vif_vals)) {
        vif_df <- data.frame(
          Variabel_Prediktor = rownames(vif_vals),
          GVIF = round(vif_vals[, 1], 4),
          Df = vif_vals[, 2],
          GVIF_pangkat_1_2df = round(vif_vals[, 3], 4)
        )
      } else {
        vif_df <- data.frame(
          Variabel_Prediktor = names(vif_vals),
          GVIF = round(vif_vals, 4),
          Df = 1,
          GVIF_pangkat_1_2df = round(sqrt(vif_vals), 4)
        )
      }
      
      DT::datatable(vif_df, options = list(dom = 't', scrollX = TRUE), rownames = FALSE) %>%
        formatStyle('GVIF_pangkat_1_2df', backgroundColor = styleInterval(c(2.236), c('#d4edda', '#f8d7da')))
    })
    
    output$interpretasi_vif <- renderUI({
      mod <- model_logit_reaktif()
      if(is.list(mod) && !is.null(mod$error_msg)) return(NULL)
      if(is.null(mod) || any(is.na(coef(mod)))) return(NULL)
      
      vif_vals <- tryCatch(car::vif(mod), error = function(e) NULL)
      req(vif_vals)
      
      nilai_cek <- if(is.matrix(vif_vals)) vif_vals[, 3] else sqrt(vif_vals)
      max_vif <- max(nilai_cek)
      
      if(max_vif < 2.236) {
        div(class = "alert alert-success", strong("Aman:"), " Tidak ditemukan masalah multikolinearitas antar variabel prediktor.")
      } else if (max_vif < 3.162) {
        div(class = "alert alert-warning", strong("Waspada:"), " Terdeteksi indikasi multikolinearitas moderat/sedang pada struktur data.")
      } else {
        div(class = "alert alert-danger", strong("Bahaya:"), " Multikolinearitas tinggi terdeteksi. Beberapa variabel prediktor memiliki tingkat korelasi linear ganda yang tinggi dan berpotensi merusak kestabilan estimasi model.")
      }
    })
    
  })
}