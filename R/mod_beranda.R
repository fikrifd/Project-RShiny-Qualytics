# ==============================================================================
# MODUL: BERANDA (HOME)
# Fungsi: Halaman pengantar, penjelasan Regresi Logistik, dan panduan
# ==============================================================================

# 1. ANTARMUKA PENGGUNA (UI)
mod_beranda_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Banner Utama
    fluidRow(
      column(12,
             wellPanel(style = "background-color: #003366; color: white; text-align: center; padding: 40px; border-radius: 10px; margin-top: 10px;",
                       h1(strong("Aplikasi Analisis Regresi Logistik")),
                       h4("Dashboard Interaktif untuk Pemodelan dan Prediksi Klasifikasi Biner")
             )
      )
    ),
    
    br(),
    
    # Penjelasan dan Panduan
    fluidRow(
      column(6,
             card(
               card_header(class = "bg-primary text-white", bs_icon("book"), " Apa itu Regresi Logistik?"),
               p(style = "text-align: justify;", "Regresi Logistik adalah metode statistika yang digunakan untuk memodelkan hubungan antara satu atau lebih variabel prediktor (numerik atau kategorik) dengan variabel respon berskala biner (dikotomis)."),
               p(style = "text-align: justify;", "Aplikasi ini menggunakan pendekatan Maximum Likelihood Estimation (MLE) untuk menghitung probabilitas kejadian suatu kelas target. Model yang dihasilkan telah dilengkapi dengan uji asumsi VIF dan kecocokan model Hosmer-Lemeshow untuk memastikan keandalan hasil klasifikasi.")
             )
      ),
      column(6,
             card(
               card_header(class = "bg-info text-white", bs_icon("list-check"), " Panduan Penggunaan"),
               tags$ol(
                 tags$li(strong("Unggah Data:"), " Masukkan dataset dalam format .csv atau .xlsx."),
                 tags$li(strong("Eksplorasi:"), " Tinjau ringkasan statistik, deteksi ketidakseimbangan kelas (Class Imbalance), dan multikolinearitas antar prediktor numerik."),
                 tags$li(strong("Pemodelan:"), " Konfigurasikan variabel target dan prediktor untuk membangun model logistik."),
                 tags$li(strong("Evaluasi:"), " Tinjau performa model menggunakan Confusion Matrix, Kurva ROC, serta diagnostik uji asumsi."),
                 tags$li(strong("Prediksi:"), " Lakukan pemeringkatan skor (scoring) probabilitas pada observasi data baru.")
               )
             )
      )
    )
  )
}

# 2. LOGIKA SERVER
mod_beranda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Kosong: Halaman beranda hanya berisi informasi statis
  })
}