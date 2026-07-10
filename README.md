PETUNJUK PENGGUNAAN APLIKASI DASHBOARD REGRESI LOGISTIK
Kelompok 1 - Mata Kuliah Komputasi Statistika
====================================================================

Aplikasi ini adalah dashboard interaktif berbasis R Shiny yang dibangun 
dengan arsitektur modular. Aplikasi mencakup alur komprehensif mulai 
dari Eksplorasi Data (EDA), Pemodelan Regresi Logistik Biner, 
Evaluasi (termasuk Uji Asumsi VIF & Hosmer-Lemeshow), hingga 
Scoring/Prediksi data baru.

## Anggota Kelompok 1
Berikut adalah daftar nama mahasiswa, NIM, dan branch kontribusi masing-masing di dalam proyek ini:

| No | Nama Anggota | NIM | Branch Kontribusi |
|:--:|:---|:---:|:---|
| 1 | Lintang Djenar Mahesa Ayu | 1314624005 | `fitur-pemodelan` |
| 2 | Mutiara Syaka | 1314624006 | `fitur-upload` |
| 3 | Nabilah Az-Zahrah | 1314624007 | `fitur-beranda` |
| 4 | Divani Cantika Leli | 1314624033 | `fitur-prediksi` |
| 5 | Glory Jovanca | 1314624036 | `fitur-evaluasi` |
| 6 | Fikri Fadila | 1314624064 | `core-app` |

--------------------------------------------------------------------
1. PERSYARATAN SISTEM (PREREQUISITES)
--------------------------------------------------------------------
Pastikan R dan RStudio telah terinstal di komputer Anda. Sebelum 
menjalankan aplikasi, pastikan seluruh library di bawah ini telah 
terinstal. Anda dapat menjalankan perintah berikut di R Console:

install.packages(c("shiny", "bslib", "bsicons", "shinyjs", 
                   "readxl", "DT", "dplyr", "skimr", "echarts4r", 
                   "plotly", "ggplot2", "pROC", "car", 
                   "ResourceSelection"))

--------------------------------------------------------------------
2. STRUKTUR DIREKTORI PROJECT
--------------------------------------------------------------------
Agar aplikasi berjalan tanpa error, pastikan struktur folder 
tidak diubah:

📂 Kelompok1_Project/
 ┣ 📂 R/                  --> Berisi modul logika (mod_upload.R, dll)
 ┣ 📂 www/                --> Direktori publik untuk aset visual
 ┣ 📄 app.R               --> File utama pusat kendali aplikasi
 ┣ 📄 global.R            --> Memuat library dan konfigurasi awal
 ┣ 📄 breastcancer.csv    --> Dataset sampel untuk uji coba
 ┗ 📄 README.txt          --> Petunjuk penggunaan aplikasi

--------------------------------------------------------------------
3. CARA MENJALANKAN APLIKASI
--------------------------------------------------------------------
1. Buka aplikasi RStudio.
2. Buka project ini dengan mengklik file .Rproj (jika ada), ATAU 
   jadikan folder utama ini sebagai Working Directory 
   (Session > Set Working Directory > To Source File Location).
3. Buka file "app.R".
4. Klik tombol "Run App" (ikon panah hijau) yang berada di pojok 
   kanan atas panel editor teks RStudio.
5. Aplikasi akan terbuka di jendela baru atau browser default Anda.

--------------------------------------------------------------------
4. PANDUAN UJI COBA CEPAT (QUICK START)
--------------------------------------------------------------------
- Gunakan dataset "breastcancer.csv" yang telah dilampirkan.
- Di menu Pemodelan, pilih "Class" sebagai variabel target (Y).
- Centang beberapa prediktor medis (X) untuk melihat output 
  P-Value dan Odds Ratio.
- Buka menu Evaluasi untuk melihat Uji VIF dan Hosmer-Lemeshow Test.
====================================================================
