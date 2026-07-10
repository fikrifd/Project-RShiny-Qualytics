# ==============================================================================
# MODUL: UNGGAH DATA
# Fungsi: Menangani input file (CSV/Excel), parsing data, dan preview tabel
# ==============================================================================

# 1. ANTARMUKA PENGGUNA (UI)
mod_upload_ui <- function(id) {
  ns <- NS(id) # Namespace untuk mengisolasi ID antar modul
  
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      
      # Panel Kiri: Kontrol Input
      card(
        card_header(
          class = "bg-primary text-white",
          bs_icon("cloud-upload"), " Panel Unggah"
        ),
        fileInput(ns("file_upload"), "Pilih File (.csv atau .xlsx)",
                  accept = c(".csv", ".xlsx")),
        
        radioButtons(ns("separator"), "Pemisah Kolom (Khusus CSV)",
                     choices = c("Koma (,)" = ",", "Titik Koma (;)" = ";"),
                     inline = TRUE),
        
        hr(),
        textOutput(ns("data_info"))
      ),
      
      # Panel Kanan: Preview Tabel
      card(
        card_header(
          class = "bg-info text-white",
          bs_icon("table"), " Pratinjau Dataset"
        ),
        DT::dataTableOutput(ns("data_preview"))
      )
    )
  )
}

# 2. LOGIKA SERVER
mod_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reaktif: Membaca dan memproses file saat diunggah
    dataset_reaktif <- reactive({
      req(input$file_upload) # Menunggu sampai ada file yang diunggah
      
      filepath <- input$file_upload$datapath
      ext <- tools::file_ext(input$file_upload$name)
      
      # Parsing berdasarkan ekstensi file
      if (ext == "csv") {
        df <- read.csv(filepath, sep = input$separator, stringsAsFactors = TRUE)
      } else if (ext == "xlsx") {
        df <- readxl::read_excel(filepath)
        # Mengubah kolom karakter menjadi faktor (wajib untuk data kategorik)
        df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
      } else {
        showNotification("Format file tidak didukung. Harap gunakan CSV atau XLSX.", type = "error")
        return(NULL)
      }
      
      return(df)
    })
    
    # Output: Informasi dimensi data
    output$data_info <- renderText({
      df <- dataset_reaktif()
      req(df)
      paste("Status: Sukses! Dataset memiliki", nrow(df), "baris dan", ncol(df), "kolom.")
    })
    
    # Output: Tabel interaktif menggunakan DT
    output$data_preview <- DT::renderDataTable({
      req(dataset_reaktif())
      DT::datatable(
        dataset_reaktif(),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip' # Format minimalis
        ),
        class = "display nowrap compact",
        rownames = FALSE
      )
    })
    
    # Mengembalikan objek reaktif agar bisa ditarik oleh modul lain
    return(dataset_reaktif)
  })
}