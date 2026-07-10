# ==============================================================================
# MODUL: PREDIKSI DATA BARU (BATCH SCORING)
# Fungsi: Menerima data baru, memprediksi probabilitas & kelas, lalu export
# ==============================================================================

# 1. ANTARMUKA PENGGUNA (UI)
mod_prediksi_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_utama"))
}

# 2. LOGIKA SERVER
mod_prediksi_server <- function(id, model_reaktif) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # RENDER UI SECARA DINAMIS (Hanya muncul jika model sudah ada)
    output$ui_utama <- renderUI({
      res <- model_reaktif()
      
      if (is.null(res)) {
        return(h4(class="text-muted", style="text-align:center; padding: 50px;", 
                  "Silakan bangun model di menu 'Pemodelan' terlebih dahulu sebelum melakukan prediksi."))
      }
      
      fluidRow(
        column(4,
               wellPanel(style = "background-color: #f8f9fa; border-top: 3px solid #003366;",
                         h4(bs_icon("magic"), " Input Data Baru"),
                         hr(),
                         p("Unggah file .csv berisi data pasien baru. Pastikan file ini memiliki nama kolom prediktor yang sama persis dengan data awal."),
                         
                         fileInput(ns("file_baru"), "Pilih File (.csv)", accept = ".csv"),
                         radioButtons(ns("separator"), "Pemisah Kolom:", 
                                      choices = c("Koma (,)" = ",", "Titik Koma (;)" = ";"), inline = TRUE),
                         hr(),
                         actionButton(ns("btn_prediksi"), "Jalankan Prediksi", class = "btn-primary w-100", icon = icon("play")),
                         br(), br(),
                         # Tombol unduh hanya muncul jika hasil sudah ada
                         uiOutput(ns("ui_download"))
               ),
               
               br(),
               uiOutput(ns("ui_ringkasan"))
        ),
        column(8,
               wellPanel(style = "background-color: #ffffff; border-top: 3px solid #00A8CC;",
                         h4(bs_icon("table"), " Hasil Klasifikasi Data Baru"),
                         hr(),
                         DT::dataTableOutput(ns("tabel_prediksi"))
               )
        )
      )
    })
    
    # Reaktif untuk menyimpan hasil prediksi
    data_prediksi <- reactiveVal(NULL)
    
    # Eksekusi Prediksi
    observeEvent(input$btn_prediksi, {
      res <- model_reaktif()
      req(res)
      
      if (is.null(input$file_baru)) {
        showNotification("Silakan unggah file data baru terlebih dahulu!", type = "warning")
        return()
      }
      
      # Membaca data baru
      df_baru <- tryCatch({
        read.csv(input$file_baru$datapath, sep = input$separator, stringsAsFactors = TRUE)
      }, error = function(e) {
        showNotification("Gagal membaca file. Pastikan format CSV benar.", type = "error")
        return(NULL)
      })
      
      req(df_baru)
      
      model_glm <- res$model
      var_target <- all.vars(res$formula)[1]
      
      # Menjalankan fungsi predict()
      prob_pred <- tryCatch({
        predict(model_glm, newdata = df_baru, type = "response")
      }, error = function(e) {
        showNotification("Gagal! Pastikan nama kolom di data baru SAMA PERSIS dengan variabel prediktor model.", type = "error")
        return(NULL)
      })
      
      req(prob_pred)
      
      # Ekstrak nama label asli langsung dari data latih (res$train)
      level_kelas <- levels(res$train[[var_target]])
      if (is.null(level_kelas)) level_kelas <- c("0", "1")
      
      kelas_pred <- ifelse(prob_pred > 0.5, level_kelas[2], level_kelas[1])
      
      # Menggabungkan hasil ke tabel
      df_hasil <- df_baru
      df_hasil$Probabilitas <- round(prob_pred, 4)
      df_hasil$Prediksi_Kelas <- kelas_pred
      
      # Pindahkan kolom hasil ke paling depan agar mudah dibaca
      cols <- c("Prediksi_Kelas", "Probabilitas", setdiff(names(df_hasil), c("Prediksi_Kelas", "Probabilitas")))
      df_hasil <- df_hasil[, cols]
      
      data_prediksi(df_hasil)
      showNotification("Scoring data baru sukses!", type = "message")
    })
    
    # Merender Tabel Hasil
    output$tabel_prediksi <- DT::renderDataTable({
      df <- data_prediksi()
      req(df)
      
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE) %>%
        DT::formatStyle('Probabilitas', backgroundColor = DT::styleColorBar(c(0,1), '#00A8CC')) %>%
        DT::formatStyle('Prediksi_Kelas', fontWeight = 'bold')
    })
    
    # Merender Tombol Download Dinamis
    output$ui_download <- renderUI({
      req(data_prediksi())
      downloadButton(ns("btn_download"), "Unduh Hasil ke CSV", class = "btn-primary w-100")
    })
    
    # Fungsi Download
    output$btn_download <- downloadHandler(
      filename = function() { paste("Hasil_Prediksi_LogisStat_", Sys.Date(), ".csv", sep = "") },
      content = function(file) {
        write.csv(data_prediksi(), file, row.names = FALSE)
      })
    
    # Ringkasan hasil prediksi hanya muncul setelah prediksi berhasil
    output$ui_ringkasan <- renderUI({
      req(data_prediksi())
      wellPanel(
        style = "background-color:#ffffff; border-top:3px solid #00A8CC;",
        h4(bs_icon("bar-chart"), " Ringkasan Hasil Prediksi"),
        hr(),
        h5(textOutput(ns("jumlah_data"))),
        plotOutput(
          ns("plot_distribusi"),
          height = "220px"
        ),
        hr(),
        h5(bs_icon("lightbulb"), " Interpretasi"),
        htmlOutput(ns("interpretasi_prediksi"))
      )
    })
    
    # Menampilkan jumlah data
    output$jumlah_data <- renderText({
      df <- data_prediksi()
      req(df)
      paste("Jumlah Data :", nrow(df))
    })
    
    # Diagram batang distribusi hasil prediksi
    output$plot_distribusi <- renderPlot({
      df <- data_prediksi()
      req(df)
      
      distribusi <- as.data.frame(table(df$Prediksi_Kelas))
      names(distribusi) <- c("Kelas", "Jumlah")
      
      ggplot(
        distribusi,
        aes(
          x = Jumlah,
          y = reorder(Kelas, Jumlah),
          fill = Kelas
        )
      ) +
        geom_col(width = 0.7) +
        geom_text(
          aes(label = Jumlah),
          hjust = -0.3,
          size = 5,
        ) +
        scale_x_continuous(
          expand = expansion(mult = c(0, 0.15))
        ) + 
        labs(x = "Jumlah Observasi", y = NULL) +
        scale_fill_manual(values = c("#00A8CC", "#003366")) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.title = element_text(face = "bold")
        )
    })
    
    # Interpretasi hasil prediksi
    output$interpretasi_prediksi <- renderUI({
      df <- data_prediksi()
      req(df)
      
      total_data <- nrow(df)
      distribusi <- table(df$Prediksi_Kelas)
      
      bold_style <- "font-weight: bold !important; font-family: system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif !important;"
      
      teks <- ""
      for(i in seq_along(distribusi)) {
        persen <- round(distribusi[i] / total_data * 100, 2)
        teks <- paste0(
          teks,
          "<div style='margin-bottom: 2px;'>",
          "<span style='color: #00A8CC; font-weight: bold; margin-right: 8px;'>→</span>",
          "Sebanyak <span style=\"", bold_style, "\">", persen, "%</span> ",
          "diklasifikasikan ke dalam kelas <span style=\"", bold_style, "\">", names(distribusi)[i], "</span>.",
          "</div>"
        )
      }
      
      HTML(
        paste0(
          "<div style='margin-bottom: 4px;'>Berdasarkan proses prediksi terhadap <span style=\"", bold_style, "\">", total_data, " data baru</span>, diperoleh distribusi hasil klasifikasi sebagai berikut:</div>",
          teks
        )
      )
    })
  })
}