# 📈Analisis Faktor-Faktor yang Mempengaruhi Tingkat Kemiskinan di Jawa Barat Menurut Kabupaten/Kota Tahun 2024

# Latar Belakang
Tingkat Pengangguran Terbuka (TPT) merupakan salah satu indikator utama yang mencerminkan kondisi ekonomi dan ketenagakerjaan di suatu daerah. Di Provinsi Jawa Barat, angka TPT cenderung lebih tinggi dibandingkan provinsi lain di Indonesia menurut data BPS. Kondisi ini menunjukkan adanya ketidakseimbangan antara pertumbuhan penduduk, ketersediaan lapangan kerja, serta kualitas tenaga kerja. Oleh karena itu, penting dilakukan analisis untuk mengetahui faktor-faktor yang memengaruhi tingkat pengangguran terbuka di provinsi ini.

Beberapa variabel yang diperkirakan berpengaruh terhadap TPT antara lain laju pertumbuhan penduduk, rata-rata lama sekolah, dan tingkat partisipasi angkatan kerja (TPAK). Pertumbuhan penduduk yang cepat dapat meningkatkan jumlah pencari kerja, sementara pendidikan (lama sekolah) diharapkan menurunkan pengangguran dengan meningkatkan kualitas tenaga kerja. Di sisi lain, TPAK yang tinggi menunjukkan semakin banyak penduduk usia kerja yang aktif di pasar tenaga kerja, yang bisa berdampak positif maupun negatif terhadap tingkat pengangguran tergantung pada kemampuan penyerapan kerja.

Untuk menganalisis hubungan antarvariabel tersebut, penelitian ini menggunakan pendekatan regresi linier dengan bantuan perangkat lunak R. Pendekatan ini didasarkan pada buku “Introduction to Machine Learning Using R: Konsep, Teori, dan Praktik” karya Lukmanul Hakim dan Asep Saefuddin (2022), yang menjelaskan penerapan konsep machine learning dasar dalam pemodelan regresi menggunakan R. Buku tersebut menekankan pentingnya pemrosesan data, validasi model, serta interpretasi hasil secara komputasional dan statistikal untuk mendukung analisis kuantitatif berbasis data dalam penelitian sosial ekonomi.

# 📋Data dan Variabel
<p align="center">
  <img src="Image/logo%20bps.png" width="250">
</p>

## Variabel Prediktor (X):
<ul>
  <li> Tingkat Partisipasi Angkatan Kerja (%) </li>
  <li> Laju Pertumbuhan Penduduk (%) </li>
  <li> Rata-Rata Lama Sekolah (%) </li> 
</ul>

## Variabel Respon (Y):
Tingkat Kemiskinan (%)


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
<p align="center">
  <img src="Image/korelasi%20tpt%20vs%20penduduk.png" width="300">
</p>
Koefisien korelasi (cor) = 0.6490519 (atau 0.649) (positif) Artinya ada hubungan searah antara Laju Pertumbuhan Penduduk dan TPT. Sifat hubungan positif menunjukkan bahwa jika nilai Penduduk meningkat, maka nilai TPT juga cenderung meningkat (begitu pula sebaliknya). Nilai korelasi sebesar 0.649 menunjukkan bahwa hubungan linear cukup kuat (atau sedang).

## B. TPT vs RLS
<p align="center">
  <img src="Image/korelasi%20tpt%20vs%20rls.png" width="300">
</p>
Koefisien korelasi (cor) = 0.4209774 (atau 0.421) (positif). Artinya ada hubungan searah antara RLS dan TPT. Sifat hubungan positif menunjukkan bahwa jika nilai RLS meningkat, maka nilai TPT juga cenderung meningkat (begitu pula sebaliknya). Nilai korelasi sebesar 0.421 menunjukkan bahwa hubungan linear cukup lemah.

## C. TPT vs TPAK
<p align="center">
  <img src="Image/korelasi%20tpt%20vs%20tpak.png" width="300">
</p>
Koefisien korelasi (cor) = -0.6517322 (atau -0.652) (negatif). Artinya ada hubungan berlawanan arah antara TPAK dan TPT. Sifat hubungan negatif menunjukkan bahwa jika nilai TPAK meningkat, maka nilai TPT juga cenderung menurun (begitu pula sebaliknya). Nilai korelasi sebesar 0.652 menunjukkan bahwa hubungan linear cukup kuat.

# Syarat Multikolinieritas
<p align="center">
  <img src="Image/syarat%20multiko.png" width="300">
</p>
Syarat multikolinearitas bertujuan untuk mengecek apakah dalam model regresi terdapat korelasi antar variabel independen. Salah satu cara yang digunakan untuk mendeteksi adanya multikolinearitas adalah mengecek nilai VIF. Hasil uji multikolinearitas diperoleh nilai VIF masing-masing variabel yaitu Penduduk (1,313), RLS (1,244), TPAK (1,587) < 5. Hal ini berarti tidak terjadi multikolineritas atau asumsi multikolinearitas terpenuhi.

# Hasil Analisis Regresi Berganda
Pengolahan dengan analisis Regreasi Berganda memperoleh hasil sebagai berikut.
<p align="center">
  <img src="Image/hasil%20analisis.png" width="300">
</p>
Berdasarkan hal tersebut, dapat diinterpretasikan hal-hal sebagai berikut.

## Uji Overall (Uji F)
Dari tabel diperoleh hasil uji signifikansi model secara keseluruhan:
<ul>
 <li><em>F-statistic</em>: 11.63</li>
 <li><em>p-value</em>: 7.709e-05</li> 
</ul>	

Nilai *p-value* yang sangat kecil (7.709e-05), jauh di bawah tingkat signifikansi umum α = 0.05 menunjukkan bahwa hipotesis nol (H0) yang menyatakan semua koefisien variabel independen (Penduduk, RLS, TPAK) secara bersama-sama sama dengan nol ditolak. 

Kesimpulannya: 
Model secara statistik menyatakan bahwa setidaknya ada satu variabel independen (Penduduk, RLS, atau TPAK) yang berpengaruh secara signifikan terhadap variabel dependen TPT.

## Uji Parsial (Uji t)
Ini menguji pengaruh masing-masing variabel independen terhadap variabel dependen secara individu, dengan asumsi variabel lain konstan, dan α = 0.05.

