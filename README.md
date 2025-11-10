# 📈Analisis Faktor-Faktor yang Mempengaruhi Tingkat Kemiskinan di Jawa Barat Menurut Kabupaten/Kota Tahun 2024

# Latar Belakang
Tingkat Pengangguran Terbuka (TPT) merupakan salah satu indikator utama yang mencerminkan kondisi ekonomi dan ketenagakerjaan di suatu daerah. Di Provinsi Jawa Barat, angka TPT cenderung lebih tinggi dibandingkan provinsi lain di Indonesia menurut data BPS. Kondisi ini menunjukkan adanya ketidakseimbangan antara pertumbuhan penduduk, ketersediaan lapangan kerja, serta kualitas tenaga kerja. Oleh karena itu, penting dilakukan analisis untuk mengetahui faktor-faktor yang memengaruhi tingkat pengangguran terbuka di provinsi ini.

Beberapa variabel yang diperkirakan berpengaruh terhadap TPT antara lain laju pertumbuhan penduduk, rata-rata lama sekolah, dan tingkat partisipasi angkatan kerja (TPAK). Pertumbuhan penduduk yang cepat dapat meningkatkan jumlah pencari kerja, sementara pendidikan (lama sekolah) diharapkan menurunkan pengangguran dengan meningkatkan kualitas tenaga kerja. Di sisi lain, TPAK yang tinggi menunjukkan semakin banyak penduduk usia kerja yang aktif di pasar tenaga kerja, yang bisa berdampak positif maupun negatif terhadap tingkat pengangguran tergantung pada kemampuan penyerapan kerja.

Untuk menganalisis hubungan antarvariabel tersebut, penelitian ini menggunakan pendekatan regresi linier dengan bantuan perangkat lunak R. Pendekatan ini didasarkan pada buku “Introduction to Machine Learning Using R: Konsep, Teori, dan Praktik” karya Lukmanul Hakim dan Asep Saefuddin (2022), yang menjelaskan penerapan konsep machine learning dasar dalam pemodelan regresi menggunakan R. Buku tersebut menekankan pentingnya pemrosesan data, validasi model, serta interpretasi hasil secara komputasional dan statistikal untuk mendukung analisis kuantitatif berbasis data dalam penelitian sosial ekonomi.


# Visualisasi Data

## 1. Barchart TPT
<p align="center">
  <img src="Image/Barchart%20TPT.png" width="500">
</p>
Grafik Tingkat Pengangguran Terbuka (TPT) menunjukkan bahwa Kota Cimahi, Kabupaten Bekasi, dan Kota Sukabumi mencatatkan TPT tertinggi  di atas 7,5%. Sebaliknya, kabupaten seperti Pangandaran dan Ciamis memiliki TPT terendah di bawah 4%. Pada kabupaten Pangandaran, TPT yang rendah ini sejalan dengan Tingkat Partisipasi Angkatan Kerja (TPAK) yang sangat tinggi di daerah tersebut, Hal ini mengindikasikan bahwa di derah ini  mayoritas penduduk usia kerja aktif dan terserap dengan baik.

## 2. Barchart TPAK
<p align="center">
  <img src="Image/Barchart%20TPAK.png" width="500">
</p>
Berdasarkan grafik Tingkat Partisipasi Angkatan Kerja (TPAK) di Kabupaten/Kota Jawa Barat, menunjukkan bahwa Pangandaran memimpin dengan TPAK tertinggi yaitu 80% diikuti oleh kabupaten seperti Cianjur dan Subang. Hal ini mengindikasikan tingginya keterlibatan penduduk usia kerja dalam kegiatan ekonomi di wilayah-wilayah yang cenderung merupakan kawasan agraris atau pedesaan. Di sisi lain, kota-kota besar dan metropolitan seperti Kota Depok dan Kota Sukabumi mencatatkan TPAK terendah dengan nilai di bawah 70%.

## 3. Barchart RLS
<p align="center">
  <img src="Image/Barchart%20RLS.png" width="500">
</p>
Grafik Rata-Rata Lama Sekolah (RLS) di berbagai Kabupaten/Kota di Jawa Barat, menunjukkan bahwa kota-kota besar dan metropolitan seperti Kota Bekasi, Kota Depok, dan Kota Cimahi menempati posisi teratas dengan RLS tertinggi rata-rata 11 tahun. Hal ini mengindikasikan bahwa rata-rata penduduk di daerah tersebut telah menempuh pendidikan setara SMA atau lebih. Sebaliknya, wilayah-wilayah seperti Kabupaten Subang, Sukabumi dan Indramayu berada di posisi terbawah dengan RLS lebih rendah yaitu di bawah 8.0 tahun yang mengindikasikan bahwa rata-rata penduduknya hanya memiliki pendidikan setara SMP atau di bawahnya.

## 4. Barchart Penduduk
<p align="center">
  <img src="Image/barchart%20penduduk.png" width="500">
</p>
Grafik rata-rata laju pertumbuhan penduduk di berbagai kabupaten/kota di Provinsi Jawa Barat, menunjukkan bahwa Kota Sukabumi, Kabupaten Bandung Barat, dan Kota Cimahi menempati posisi dengan laju pertumbuhan penduduk tertinggi dengan nilai pertumbuhan penduduk rata-rata 1,4%. Hal ini mungkin didorong oleh tingginya urbanisasi dan migrasi masuk seiring perkembangan wilayah tersebut menjadi pusat ekonomi. Sedangkan, daerah seperti Kabupaten Sumedang, Pangandaran, dan Ciamis berada pada posisi terbawah dengan laju pertumbuhan penduduk yang relatif rendah di bawah 1%.

# Uji Korelasi
## A. TPT vs Laju Pertumbuhan Penduduk

