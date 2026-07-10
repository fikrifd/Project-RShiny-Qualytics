# =====================================
# MODUL: PEMODELAN LOGISTIC REGRESSION 
# =====================================
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(plotly)
library(car)

mod_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      
      # 1. PANEL KONFIGURASI
      card(
        card_header(class = "bg-primary text-white", bs_icon("gear-fill"), " Konfigurasi Model"),
        accordion(
          accordion_panel("Variabel Respon (Y)", icon = bs_icon("bullseye"),
                          radioButtons(ns("var_target"), NULL, choices = "Unggah data...")),
          accordion_panel("Variabel Prediktor (X)", icon = bs_icon("input-cursor-text"),
                          checkboxGroupInput(ns("var_prediktor"), NULL, choices = "Unggah data...")),
          accordion_panel("Parameter Lanjut", icon = bs_icon("sliders"),
                          selectInput(ns("link_func"), "Link Function:", choices = c("logit", "probit", "cloglog")),
                          numericInput(ns("alpha_val"), "Alpha (Signifikansi):", value = 0.05, step = 0.01),
                          sliderInput(ns("split_ratio"), "Split Train/Test:", 0.5, 0.9, 0.8, 0.05))
        ),
        card_footer(actionButton(ns("btn_train"), "Bangun Model", class = "btn-primary w-100 fw-bold"))
      ),
      
      # 2. PANEL HASIL ANALISIS
      navset_card_pill(
        title = "Analisis Model",
        
        nav_panel("Estimasi & Koefisien", icon = bs_icon("table"),
                  uiOutput(ns("status_model")),
                  div(style = "overflow-x: auto; margin-bottom: 20px;", 
                      DT::dataTableOutput(ns("tabel_koefisien"))),
                  uiOutput(ns("auto_interpret"))
        ),
        
        nav_panel("Visualisasi Koefisien", icon = bs_icon("graph-up"),
                  plotlyOutput(ns("plot_impact"), height = "450px"),
                  hr(),
                  div(class = "alert alert-info", 
                      strong("Cara Membaca Koefisien: "), 
                      "Titik menunjukkan nilai estimasi koefisien. Garis adalah interval kepercayaan. Jika garis melewati titik nol vertikal, variabel tersebut tidak signifikan.")
        ),
        
        nav_panel("Uji Multikolinearitas (VIF)", icon = bs_icon("shield-check"),
                  div(style = "overflow-x: auto; margin-bottom: 20px;", 
                      DT::dataTableOutput(ns("tabel_vif"))),
                  uiOutput(ns("vif_summary"))
        )
      )
    )
  )
}

mod_model_server <- function(id, dataset_reaktif) {
  moduleServer(id, function(input, output, session) {
    
    # Sinkronisasi Pilihan: Tampilkan variabel yang sesuai
    observeEvent(dataset_reaktif(), {
      df <- dataset_reaktif(); req(df)
      vars <- names(df)
      
      # Target: variabel kategorik (karakter/faktor) atau yang punya sedikit nilai unik
      kategorik_vars <- vars[sapply(df, function(x) is.factor(x) || is.character(x) || length(unique(na.omit(x))) == 2)]
      if(length(kategorik_vars) == 0) kategorik_vars <- "Tidak ada variabel cocok"
      
      updateRadioButtons(session, "var_target", choices = kategorik_vars)
      updateCheckboxGroupInput(session, "var_prediktor", choices = vars)
    })
    
    # Prediktor tidak bisa memilih variabel target
    observeEvent(input$var_target, {
      df <- dataset_reaktif(); req(df)
      updateCheckboxGroupInput(session, "var_prediktor", choices = setdiff(names(df), input$var_target))
    })
    
    hasil_model <- reactiveVal(NULL)
    
    # Eksekusi Model
    observeEvent(input$btn_train, {
      df <- dataset_reaktif(); req(df, input$var_target, input$var_prediktor)
      validate(need(length(input$var_prediktor) > 0, "Pilih minimal satu variabel prediktor."))
      
      # 1. Penanganan NA
      df_final <- na.omit(df[, c(input$var_target, input$var_prediktor), drop = FALSE])
      validate(need(nrow(df_final) > 10, "Terlalu banyak baris yang kosong (NA). Data tersisa tidak cukup untuk membuat model."))
      
      df_final[[input$var_target]] <- as.factor(df_final[[input$var_target]])
      
      # 2. Validasi 2 Kategori
      target_levels <- length(unique(df_final[[input$var_target]]))
      validate(need(target_levels == 2, paste("Model Logistik Biner mensyaratkan tepat 2 kategori. Variabel", input$var_target, "memiliki", target_levels, "kategori setelah NA dihapus.")))
      
      # 3. Split Data
      set.seed(42); idx <- sample(1:nrow(df_final), round(input$split_ratio * nrow(df_final)))
      train <- df_final[idx, ]
      
      # 4. Pengecekan Level Prediktor di Data Latih (Mencegah error contrasts)
      for(col in input$var_prediktor) {
        if(is.factor(train[[col]]) || is.character(train[[col]])) {
          unique_vals <- length(unique(train[[col]]))
          if(unique_vals < 2) {
            showNotification(paste("Variabel", col, "hanya memiliki 1 kategori di data latih. Model mungkin gagal. Coba tambah porsi Split Train/Test atau hapus variabel tersebut."), type = "warning", duration = 8)
          }
        }
      }
      
      form_str <- paste("`", input$var_target, "` ~ ", paste(paste0("`", input$var_prediktor, "`"), collapse = " + "), sep = "")
      form <- as.formula(form_str)
      
      # 5. Fitting dengan Warning Catcher (Perfect Separation)
      pesan_warning <- NULL
      mod <- withCallingHandlers(
        tryCatch(glm(form, data = train, family = binomial(link = input$link_func)), error = function(e) {
          showNotification(paste("Error Pemodelan:", e$message), type = "error")
          return(NULL)
        }),
        warning = function(w) {
          pesan_warning <<- w$message
          invokeRestart("muffleWarning")
        }
      )
      req(mod)
      
      hasil_model(list(model = mod, train = train, link = input$link_func, alpha = input$alpha_val, formula = form, peringatan = pesan_warning))
    })
    
    # OUTPUT: SUMMARY
    output$status_model <- renderUI({
      res <- hasil_model(); req(res); mod <- res$model
      null_dev <- mod$null.deviance; res_dev <- mod$deviance
      
      # Menampilkan warning separation jika ada
      alert_html <- NULL
      if(!is.null(res$peringatan) && grepl("fitted probabilities numerically 0 or 1", res$peringatan)) {
        alert_html <- div(class = "alert alert-warning", strong("Perfect Separation Terdeteksi: "), "Model memprediksi secara sempurna untuk observasi tertentu. Beberapa koefisien atau standar error mungkin bernilai sangat ekstrim (tidak stabil).")
      }
      
      form_text <- paste(deparse(res$formula), collapse = " ")
      
      tagList(
        alert_html,
        div(style = "background-color: #f8f9fa; color: #212529; font-family: 'Courier New', Courier, monospace; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6; margin-bottom: 20px; white-space: pre-wrap; font-size: 0.9em;",
            paste0("Call:\nglm(formula = ", form_text, ", family = binomial(link = \"", res$link, "\"))\n"),
            "\n(Dispersion parameter for binomial family taken to be 1)\n",
            paste0("\n    Null deviance: ", round(null_dev, 2), "  on ", mod$df.null, "  degrees of freedom"),
            paste0("\nResidual deviance: ", round(res_dev, 2), "  on ", mod$df.residual, "  degrees of freedom"),
            paste0("\nAIC: ", round(summary(mod)$aic, 2), "\n"),
            paste0("\nNumber of Fisher Scoring iterations: ", mod$iter)
        )
      )
    })
    
    # OUTPUT: TABEL KOEFISIEN
    output$tabel_koefisien <- DT::renderDataTable({
      res <- hasil_model(); req(res); s <- summary(res$model)$coefficients
      df_coef <- data.frame(
        Variabel = rownames(s), Estimate = round(s[,1], 4), Std_Error = round(s[,2], 4),
        Z_Value = round(s[,3], 4), Odds_Ratio = round(exp(s[,1]), 4), P_Value = round(s[,4], 4)
      )
      datatable(df_coef, options = list(dom = 't', scrollX = TRUE), rownames = FALSE) %>%
        formatStyle('P_Value', backgroundColor = styleInterval(c(0.01, res$alpha), c('#d4edda', '#fff3cd', 'white')))
    })
    
    # OUTPUT: INTERPRETASI
    output$auto_interpret <- renderUI({
      res <- hasil_model(); req(res); s <- summary(res$model)$coefficients
      sig_vars <- rownames(s)[s[,4] < res$alpha & rownames(s) != "(Intercept)"]
      
      if(any(is.na(coef(res$model)))) {
        return(div(class = "alert alert-danger", strong("Peringatan: "), "Terdeteksi multikolinearitas sempurna (Aliased Coefficients). Ada variabel prediktor yang redundan."))
      }
      
      if(length(sig_vars) == 0){
        div(class = "alert alert-warning", strong("Tidak ada variabel signifikan pada α = "), res$alpha)
      } else {
        div(class = "alert alert-success", strong("Variabel signifikan (P-Value < "), res$alpha, "): ", strong(paste(sig_vars, collapse = ", ")))
      }
    })
    
    # OUTPUT: PLOT KOEFISIEN
    output$plot_impact <- renderPlotly({
      res <- hasil_model(); req(res); mod <- res$model
      ci <- suppressMessages(confint.default(mod))
      s <- as.data.frame(summary(mod)$coefficients)
      s$lower_ci <- ci[, 1]; s$upper_ci <- ci[, 2]
      s$Variable <- rownames(s); s <- s[s$Variable != "(Intercept)", ]
      s <- na.omit(s)
      
      p <- ggplot(s, aes(x = reorder(Variable, Estimate), y = Estimate, ymin = lower_ci, ymax = upper_ci, 
                         text = paste("Variabel:", Variable, "<br>Koefisien:", round(Estimate, 2)))) +
        geom_pointrange(color = "#003366", size = 0.8) + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "#00BFFF") +
        coord_flip() + theme_minimal(base_family = "sans") + 
        labs(title="Estimasi Koefisien Model Logit", x=NULL, y="Estimasi Koefisien")
      ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    # OUTPUT: TABEL VIF
    output$tabel_vif <- DT::renderDataTable({
      res <- hasil_model(); req(res); mod <- res$model
      
      if(any(is.na(coef(mod)))) {
        return(datatable(data.frame(Status = "Gagal menghitung VIF akibat Multikolinearitas Sempurna (Aliased)."), options=list(dom='t')))
      }
      
      v <- tryCatch(car::vif(mod), error = function(e) e$message)
      if(is.character(v)) return(datatable(data.frame(Pesan = "VIF tidak dapat dihitung karena ada variabel yang berkorelasi sempurna atau faktor dengan 1 level."), options=list(dom='t')))
      
      if(is.matrix(v)){
        v_df <- data.frame(Variabel = rownames(v), GVIF = round(v[,1], 4), Df = v[,2], GVIF_pangkat_1_2df = round(v[,3], 4))
      } else {
        v_df <- data.frame(Variabel = names(v), GVIF = round(v, 4), Df = 1, GVIF_pangkat_1_2df = round(sqrt(v), 4))
      }
      
      datatable(v_df, options = list(dom = 't', scrollX = TRUE), rownames = FALSE) %>%
        formatStyle('GVIF_pangkat_1_2df', backgroundColor = styleInterval(c(2.236), c('#d4edda', '#f8d7da')))
    })
    
    output$vif_summary <- renderUI({
      res <- hasil_model(); req(res); mod <- res$model
      if(any(is.na(coef(mod)))) return(NULL)
      v <- tryCatch(car::vif(mod), error = function(e) NULL); req(v)
      
      nilai_vif <- if(is.matrix(v)) v[,3] else sqrt(v)
      max_vif <- max(nilai_vif)
      
      if(max_vif < 2.236){
        div(class = "alert alert-success", strong("Aman: "), "Tidak ditemukan masalah multikolinearitas.")
      } else if(max_vif < 3.162){
        div(class = "alert alert-warning", strong("Waspada: "), "Terdapat indikasi multikolinearitas sedang.")
      } else {
        div(class = "alert alert-danger", strong("Bahaya: "), "Terdapat multikolinearitas tinggi (cek Usia vs Lama_Bekerja).")
      }
    })
    return(hasil_model)
  })
}