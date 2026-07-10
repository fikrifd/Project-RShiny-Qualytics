# ==============================================================================
# MODUL: EVALUASI MODEL & DIAGNOSTIK
# Fungsi: Confusion Matrix, Metrik, ROC, VIF, dan Hosmer-Lemeshow
# ==============================================================================

# 1. ANTARMUKA PENGGUNA (UI)
mod_evaluasi_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("ui_utama"))
}

# 2. LOGIKA SERVER
mod_evaluasi_server <- function(id, model_reaktif) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # RENDER UI SECARA DINAMIS
    output$ui_utama <- renderUI({
      res <- model_reaktif()
      
      if (is.null(res)) {
        return(h4(class="text-muted", style="text-align:center; padding: 50px;", 
                  "Belum ada model yang dilatih. Silakan bangun model di menu 'Pemodelan' terlebih dahulu."))
      }
      
      fluidRow(
        # Panel Kiri: Metrik ML Standar
        column(5,
               wellPanel(style = "background-color: #ffffff; border-top: 3px solid #003366;",
                         h4(bs_icon("grid-3x3"), " Confusion Matrix (Data Uji)"),
                         hr(),
                         plotly::plotlyOutput(ns("plot_conf_matrix"), height = "350px"),
                         br(),
                         h4(bs_icon("bar-chart-steps"), " Metrik Performa"),
                         hr(),
                         tableOutput(ns("tabel_metrik"))
               )
        ),
        
        # Panel Kanan: Kurva ROC & Diagnostik Statistik Murni
        column(7,
               fluidRow(
                 # Kurva ROC di sebelah kiri
                 column(7,
                        wellPanel(style = "background-color: #ffffff; border-top: 3px solid #00A8CC;",
                                  h4(bs_icon("graph-up"), " Kurva ROC"),
                                  hr(),
                                  plotOutput(ns("plot_roc"), height = "300px")
                        )
                 ),
                 # Interpretasi Adaptif di sebelah kanan ROC
                 column(5,
                        wellPanel(style = "background-color: #f4f8fb; border-top: 3px solid #003366; height: 415px; overflow-y: auto;",
                                  h4(bs_icon("lightbulb"), " Interpretasi"),
                                  hr(),
                                  htmlOutput(ns("teks_interpretasi_adaptif"))
                        )
                 )
               ),
               
               br(), 
               
               # DIAGNOSTIK MODEL
               wellPanel(style = "background-color: #f8f9fa; border-top: 3px solid #003366;",
                         h4(bs_icon("check2-all"), " Diagnostik & Uji Asumsi Model"),
                         hr(),
                         fluidRow(
                           column(6,
                                  p(strong("Multikolinearitas (VIF)")),
                                  tableOutput(ns("tabel_vif"))
                           ),
                           column(6,
                                  p(strong("Hosmer-Lemeshow Goodness-of-Fit")),
                                  verbatimTextOutput(ns("text_hoslem"))
                           )
                         )
               )
        )
      )
    })
    
    # PERHITUNGAN PREDIKSI & METRIK MANUAL
    hasil_evaluasi <- reactive({
      res <- model_reaktif()
      req(res)
      
      # Menggunakan data uji jika tersedia, jika tidak maka fallback ke data latih
      data_test <- if(!is.null(res$test)) res$test else res$train
      model_glm <- res$model
      var_target <- all.vars(res$formula)[1]
      
      prob_pred <- predict(model_glm, newdata = data_test, type = "response")
      kelas_aktual <- data_test[[var_target]]
      level_kelas <- levels(kelas_aktual)
      if (is.null(level_kelas)) {
        level_kelas <- c("0", "1")
        kelas_aktual <- factor(kelas_aktual, levels = level_kelas)
      }
      
      kelas_pred <- ifelse(prob_pred > 0.5, level_kelas[2], level_kelas[1])
      kelas_pred <- factor(kelas_pred, levels = level_kelas)
      
      cm <- table(Aktual = factor(kelas_aktual, levels = level_kelas), 
                  Prediksi = factor(kelas_pred, levels = level_kelas))
      
      if(length(level_kelas) < 2) return(NULL)
      
      TN <- cm[1, 1]; FP <- cm[1, 2]
      FN <- cm[2, 1]; TP <- cm[2, 2]
      
      Akurasi <- (TP + TN) / sum(cm)
      Presisi <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
      Recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
      Spesifisitas <- ifelse((TN + FP) == 0, 0, TN / (TN + FP))
      F1_Score <- ifelse((Presisi + Recall) == 0, 0, 2 * (Presisi * Recall) / (Presisi + Recall))
      
      roc_obj <- pROC::roc(kelas_aktual, prob_pred, levels = level_kelas, direction = "<", quiet = TRUE)
      
      list(
        cm = cm,
        metrik = data.frame(
          Metrik = c("Akurasi (Accuracy)", "Presisi (Precision)", "Sensitivitas (Recall)", "Spesifisitas (Specificity)", "F1-Score"),
          Nilai = c(Akurasi, Presisi, Recall, Spesifisitas, F1_Score)
        ),
        roc_obj = roc_obj
      )
    })
    
    output$tabel_metrik <- renderTable({
      eval <- hasil_evaluasi()
      req(eval)
      eval$metrik$Nilai <- sprintf("%.2f %%", eval$metrik$Nilai * 100)
      return(eval$metrik)
    }, striped = TRUE, hover = TRUE, width = "100%")
    
    output$plot_conf_matrix <- plotly::renderPlotly({
      eval <- hasil_evaluasi()
      req(eval)
      
      cm_df <- as.data.frame(eval$cm)
      
      cm_df$Hover_Text <- ifelse(
        cm_df$Prediksi == cm_df$Aktual,
        paste0(
          "Klasifikasi Benar!<br>",
          "Prediksi Model: ", cm_df$Prediksi,
          "<br>Kenyataan Aktual: ", cm_df$Aktual,
          "<br>Jumlah Observasi: ", cm_df$Freq
        ),
        paste0(
          "Klasifikasi Salah!<br>",
          "Prediksi Model: ", cm_df$Prediksi,
          "<br>Kenyataan Aktual: ", cm_df$Aktual,
          "<br>Jumlah Observasi: ", cm_df$Freq
        )
      )
      
      p <- ggplot(
        data = cm_df,
        aes(x = Prediksi,
            y = Aktual,
            text = Hover_Text)
      ) +
        geom_tile(aes(fill = Freq), color = "white") +
        geom_text(aes(label = Freq),
                  size = 8,
                  fontface = "bold") +
        scale_fill_gradient(low = "#e0f7fa", high = "#00A8CC") +
        labs(
          x = "Kelas Prediksi (Model)",
          y = "Kelas Aktual (Kenyataan)"
        ) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.title = element_text(size = 12, face = "bold")
        )
      
      plotly::ggplotly(p, tooltip = "text", height = 270) %>%
        plotly::layout(
          margin = list(l = 100, r = 20, b = 60, t = 90)
        ) %>%
        plotly::config(displayModeBar = FALSE)
    })
    
    output$plot_roc <- renderPlot({
      eval <- hasil_evaluasi()
      req(eval)
      roc_obj <- eval$roc_obj
      auc_val <- round(pROC::auc(roc_obj), 4)
      plot(roc_obj, col = "#003366", lwd = 3, main = "Kurva ROC (Data Uji)", print.auc = FALSE, legacy.axes = TRUE)
      polygon(c(1, roc_obj$specificities, 0), c(0, roc_obj$sensitivities, 0), 
              col = rgb(0, 168, 204, maxColorValue=255, alpha=50), border = NA)
      legend("bottomright", legend = paste("AUC =", auc_val), bty = "n", cex = 1.5, text.col = "#003366", text.font = 2)
    })
    
    # OUTPUT BARU: DIAGNOSTIK STATISTIKA 
    # 1. Output Tabel VIF 
    output$tabel_vif <- renderTable({
      res <- model_reaktif()
      req(res)
      
      #VIF butuh minimall 2 variabel prediktor 
      if(length(res$model$coefficients) <= 2) {
        return(data.frame(Pesan = "Pilih minimal 2 variabel prediktor untuk menghitung VIF."))
      }
      
      # Menghitung VIF menggunakan tryCatch agar tidak crash jika matriks singular
      vif_vals <- tryCatch({
        car::vif(res$model)
      }, error = function(e) {
        return(NULL)
      })
      
      if(is.null(vif_vals)) return(data.frame(Pesan = "VIF Gagal Dihitung (Terjadi Perfect Multicollinearity)."))
      
      if(is.matrix(vif_vals) || is.data.frame(vif_vals)) {
        data.frame(Variabel = rownames(vif_vals), Nilai_VIF = round(vif_vals[, 1], 3))
      } else {
        data.frame(Variabel = names(vif_vals), Nilai_VIF = round(vif_vals, 3))
      }
    }, striped = TRUE, hover = TRUE)
    
    # 2. Output Hosmer-Lemeshow Test
    output$text_hoslem <- renderPrint({
      res <- model_reaktif()
      req(res)
      
      # Fungsi hoslem.test bawaan package ResourceSelection
      tryCatch({
        hl_test <- ResourceSelection::hoslem.test(res$model$y, fitted(res$model), g = 10)
        
        # Cetak output summary R 
        cat("Hosmer and Lemeshow goodness of fit (GOF) test\n\n")
        cat(sprintf("X-squared = %.4f, df = %d, p-value = %.4f\n\n", 
                    hl_test$statistic, hl_test$parameter, hl_test$p.value))
        
        if(hl_test$p.value > 0.05) {
          cat("Kesimpulan: Model FIT (Cocok dengan data observasi)")
        } else {
          cat("Kesimpulan: Model TIDAK FIT (Beda signifikan dengan data observasi)")
        }
        
      }, error = function(e) {
        cat("Peringatan: Uji tidak dapat dilakukan pada data ini.\nPastikan data mencukupi untuk pembagian 10 grup.")
      })
    })
    
    # 3. Output Interpretasi Adaptif 
    output$teks_interpretasi_adaptif <- renderUI({
      res <- model_reaktif()
      eval <- hasil_evaluasi()
      req(res, eval)
      
      bold_style <- "font-weight: bold !important; font-family: system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif !important;"
      
      #Adaptasi Akurasi 
      akurasi_num <- eval$metrik$Nilai[1] 
      teks_akurasi <- if(akurasi_num >= 0.85) {
        paste0("<span style=\"", bold_style, "\">memiliki tingkat keandalan yang sangat tinggi</span>")
      } else if(akurasi_num >= 0.70) {
        paste0("<span style=\"", bold_style, "\">menunjukkan performa yang memadai namun masih dapat dioptimalkan</span>")
      } else {
        paste0("<span style=\"", bold_style, "\">menunjukkan tingkat ketepatan yang belum optimal (rentan terhadap kesalahan klasifikasi)</span>")
      }
      
      #Adaptasi VIF 
      teks_vif <- ""
      multi_text <- paste0("<span style=\"", bold_style, "\">multikolinearitas</span>")
      
      if(length(res$model$coefficients) > 2) {
        vif_vals <- tryCatch(car::vif(res$model), error = function(e) NULL)
        nilai_cek <- if(is.matrix(vif_vals)) vif_vals[, 1] else vif_vals
        if(is.null(vif_vals) || any(nilai_cek > 5)) {
          teks_vif <- paste0("Terdeteksi adanya indikasi ", multi_text, ", yakni kondisi di mana terdapat korelasi atau kemiripan informasi antar variabel prediktor yang digunakan. Hal ini mengakibatkan sistem kesulitan untuk mengisolasi kontribusi murni dari masing-masing faktor secara individual terhadap model.")
        } else {
          teks_vif <- paste0("Pemilihan variabel prediktor dinilai efisien dan terbebas dari indikasi ", multi_text, ". Tidak terdapat tumpang tindih informasi, sehingga setiap faktor memberikan kontribusi penjelasan yang independen terhadap model.")
        }
      } else {
        teks_vif <- paste0("Mengingat model hanya menggunakan satu variabel prediktor, model dipastikan terbebas dari risiko ", multi_text, " (tumpang tindih informasi antar variabel).")
      }
      
      # Adaptasi Hosmer-Lemeshow
      teks_hoslem <- ""
      hl_test <- tryCatch(ResourceSelection::hoslem.test(res$model$y, fitted(res$model), g = 10), error = function(e) NULL)
      if(!is.null(hl_test)) {
        if(hl_test$p.value > 0.05) {
          teks_hoslem <- paste0("Berdasarkan pengujian, probabilitas prediksi model telah selaras dengan distribusi kejadian aslinya. Model ini dinyatakan <span style=\"", bold_style, "\">layak (fit)</span> dan aman untuk diimplementasikan pada data sesungguhnya.")
        } else {
          teks_hoslem <- paste0("Catatan: persentase probabilitas yang dihasilkan belum sepenuhnya merepresentasikan pola observasi aktual. <span style=\"", bold_style, "\">Penggunaan model ini sebagai basis pengambilan keputusan tunggal perlu dikaji lebih lanjut</span>.")
        }
      }
      
      HTML(paste0(
        "<p style='font-size: 14px;'>Secara garis besar, model ini ", teks_akurasi, " dalam mengklasifikasikan data (akurasi: ", round(akurasi_num * 100, 2), "%).</p>",
        "<p style='font-size: 14px;'>", teks_vif, "</p>",
        "<p style='font-size: 14px;'>", teks_hoslem, "</p>"
      ))
    })
    
  })
}
