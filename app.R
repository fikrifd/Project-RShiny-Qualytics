source("global.R")

# Konfigurasi Tema UI
tema_logisstat <- bs_theme(
  version = 5,
  bootswatch = "zephyr",
  primary = "#003366",
  info = "#00A8CC",
  bg = "#F8F9FA",
  fg = "#2C3E50",
  base_font = font_google("Inter"),
  heading_font = font_google("Montserrat")
)

# Antarmuka Pengguna (UI)
ui <- page_navbar(
  theme = tema_logisstat,
  
  title = span(
    tags$img(src = "Qualytics.png", height = "70px", style = "margin-right: 3px; margin-bottom: 5px; vertical-align: middle;"),
    strong("Qualytics", style = "font-size: 30px; vertical-align: middle;")
  ),
  id = "main_nav",
  fillable = FALSE,
  
  header = tagList(
    useShinyjs()
  ),
  
  nav_spacer(),
  
  # Navigasi Modul
  nav_panel(
    title = "Beranda", 
    icon = bs_icon("house"),
    mod_beranda_ui("beranda_1") # <--- Memanggil UI Modul Beranda
  ),
  
  nav_panel(
    title = "Unggah Data", 
    icon = bs_icon("cloud-arrow-up"),
    mod_upload_ui("upload_1")
  ),
  
  nav_panel(
    title = "Eksplorasi", 
    icon = bs_icon("search"),
    mod_eda_ui("eda_1")
  ),
  
  nav_panel(
    title = "Pemodelan", 
    icon = bs_icon("diagram-3"),
    mod_model_ui("model_1")
  ),
  
  nav_panel(
    title = "Evaluasi", 
    icon = bs_icon("clipboard-data"),
    mod_evaluasi_ui("evaluasi_1")
  ),
  
  nav_panel(
    title = "Prediksi", 
    icon = bs_icon("magic"),
    mod_prediksi_ui("prediksi_1")
  )

)

# Logika Server Utama
server <- function(input, output, session) {
  mod_beranda_server("beranda_1")
  data_utama <- mod_upload_server("upload_1")
  mod_eda_server("eda_1", dataset_reaktif = data_utama)
  model_utama <- mod_model_server("model_1", dataset_reaktif = data_utama)
  mod_evaluasi_server("evaluasi_1", model_reaktif = model_utama)
  mod_prediksi_server("prediksi_1", model_reaktif = model_utama)
}

# Inisiasi Aplikasi
shinyApp(ui = ui, server = server)