# ==============================================================================
# KONFIGURASI GLOBAL & MANAJEMEN PACKAGES (KELOMPOK 1)
# ==============================================================================

# Core Shiny & UI Framework
library(shiny)         # Framework utama untuk membangun aplikasi web interaktif
library(bslib)         # Mengatur tema modern menggunakan Bootstrap 5 (Zephyr)
library(bsicons)       # Menyediakan ikon navigasi dari Bootstrap
library(shinyjs)       # Menjalankan fungsi JavaScript (seperti reset/toggle tombol)

# Input & Output (Tabel Interaktif)
library(readxl)        # Membaca file dataset berformat Excel (.xlsx)
library(DT)            # Merender tabel interaktif (DataTables) yang bisa di-search

# Manipulasi Data & Eksplorasi (EDA)
library(dplyr)         # Agregasi data (seperti fungsi count dan pipa %>%)
library(skimr)         # Membuat ringkasan deskriptif data secara otomatis
library(echarts4r)     # Grafik batang interaktif modern untuk visualisasi biner
library(plotly)        # Heatmap interaktif untuk visualisasi matriks korelasi

# Evaluasi Model & Grafik
library(ggplot2)       # Menggambar visualisasi Confusion Matrix berbasis tile
library(pROC)          # Menghitung nilai AUC dan membuat visualisasi Kurva ROC
library(car)                # Untuk deteksi Multikolinearitas (VIF)
library(ResourceSelection)  # Untuk uji kecocokan model (Hosmer-Lemeshow Test)