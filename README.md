<p align="center">
  <img class="banner" src="Image/Banner%20Tingkat%20Pengangguran%20Terbuka.jpg"/>
</p>

---
# 🧩Latar Belakang
<p align="center">
  <img class="banner" src="Image/TPT%20Provinsi%20Pulau%20Jawa.jpg"/>
</p>
Tingkat Pengangguran Terbuka (TPT) merupakan indikator penting yang mencerminkan kondisi ketenagakerjaan suatu wilayah. Di Jawa Barat, TPT masih relatif tinggi dibanding rata-rata nasional yang sebesar 4,85%. Data BPS terbaru per Agustus 2025 menunjukkan bahwa TPT Jawa Barat berada pada angka 6,77%, sedikit meningkat dari Februari 2025 yang mencapai 6,74%. Meskipun terdapat tanda-tanda perbaikan sejak 2024, tingginya angka TPT mencerminkan bahwa pasar kerja di Jawa Barat belum mampu mengimbangi pertumbuhan angkatan kerja dan masih menghadapi masalah ketidaksesuaian keterampilan tenaga kerja dengan permintaan industri.

<br>Kondisi tersebut menegaskan perlunya analisis terhadap faktor-faktor yang berpengaruh terhadap TPT. Beberapa variabel yang diperkirakan memiliki peran penting antara lain laju pertumbuhan penduduk, rata-rata lama sekolah sebagai indikator kualitas pendidikan, serta tingkat partisipasi angkatan kerja (TPAK). Pertumbuhan penduduk dapat meningkatkan jumlah pencari kerja, sedangkan pendidikan yang lebih baik diharapkan mampu meningkatkan peluang bekerja. TPAK mencerminkan seberapa banyak penduduk usia kerja yang aktif di pasar tenaga kerja, yang dapat berdampak berbeda bergantung pada kapasitas penyerapan kerja.
</br>
<br>Untuk memahami hubungan antarvariabel tersebut, penelitian ini menggunakan analisis regresi linier dengan bantuan perangkat lunak R.

---
# 🎯Tujuan Penelitian

**Tujuan dari penelitian ini adalah sebagai berikut:**

1. Menganalisis pengaruh Laju Pertumbuhan Penduduk, Rata-Rata Lama Sekolah, dan Tingkat Partisipasi Angkatan Kerja terhadap Tingkat Pengangguran Terbuka.
  
2. Mengidentifikasi variabel yang memiliki pengaruh paling signifikan.

---
# 📋Sumber Data dan Variabel
<p align="center">
  <img src="Image/logo%20bps.png" width="200">
</p>

## Variabel Prediktor (X):
<ul>
  <li> Tingkat Partisipasi Angkatan Kerja (%) </li>
  <li> Laju Pertumbuhan Penduduk (%) </li>
  <li> Rata-Rata Lama Sekolah (Tahun) </li> 
</ul>

## Variabel Respon (Y):
Tingkat Pengangguran Terbuka (%)

---
# 🧭 Tahapan Analisis

Tahapan analisis yang dilakukan dalam penelitian ini adalah sebagai berikut:

### 1. Pengumpulan Data
Data diambil dari publikasi Badan Pusat Statistik (BPS) Provinsi Jawa Barat, mencakup variabel:
- Tingkat Pengangguran Terbuka (TPT)
- Tingkat Partisipasi Angkatan Kerja (TPAK)
- Laju Pertumbuhan Penduduk
- Rata-Rata Lama Sekolah (RLS)

### 2. Eksplorasi Data Awal
- Mengidentifikasi pola awal dan mengamati wilayah dengan nilai ekstrem (tertinggi/terendah) melalui grafik barchart TPT, TPAK, RLS, dan Laju Pertumbuhan Penduduk.

### 3. Analisis Korelasi
- Menghitung korelasi Pearson antar variabel
- Menginterpretasikan arah dan kekuatan hubungan
- Menentukan variabel dengan potensi pengaruh paling kuat terhadap TPT

### 4. Uji Asumsi Regresi
Sebelum membangun model regresi, dilakukan uji asumsi:
- **Normalitas residual** (<i>Shapiro–Wilk</i> dan QQ-Plot)
- **Multikolinearitas** (VIF/<i>Variance Inflation Factor</i>)
- **Heteroskedastisitas** (Uji <i>Breusch-Pagan</i>)

### 5. Pemodelan Regresi Linier Berganda
Menyusun model regresi dengan TPT sebagai variabel respon dan:
- TPAK  
- Laju Pertumbuhan Penduduk  
- RLS  
sebagai variabel prediktor.

### 6. Uji Signifikansi Model
- **Uji F** → menguji kelayakan model secara keseluruhan  
- **Uji t** → menguji pengaruh masing-masing variabel secara parsial  

### 7. Hasil Analisis
Menyimpulkan temuan utama dan evaluasi model (R²).

---
# 🗂️Struktur Proyek

```
PEMSTAT-KEL-2/
├── Data/
│   └── Data GitHub.xlsx
│
├── Image/
│   ├── Arah Faktor.jpg
│   ├── Barchart Laju Pertumbuhan.png
│   ├── Barchart RLS.png
│   ├── Barchart TPAK.png
│   ├── Barchart TPT.png
│   ├── Hasil Regresi.jpg
│   ├── Korelasi Penduduk dan TPT.jpg
│   ├── Korelasi RLS dan TPT.jpg
│   ├── Korelasi TPAK dan TPT.jpg
│   ├── Normal Plot.png
│   ├── TPT Provinsi Pulai Jawa.jpg
│   ├── Uji Heteroskedastisitas.jpg
│   ├── Uji Normalitas.jpg
│   ├── Uji Parsial.png
│   └── VIF.png
│
├── Syntax/
│   └── Pemro_Kelompok.R
│
└── README.md
```

---
# 📊Visualisasi Data

## 1. Barchart TPT
<p align="center">
  <img src="Image/Barchart%20TPT.png" width="500">
</p>
<p align="center"><i>Gambar 1. Barchart Tingkat Pengangguran Terbuka.</i></p>

Grafik Tingkat Pengangguran Terbuka (TPT) menunjukkan bahwa Kota Cimahi, Kabupaten Bekasi, dan Kota Sukabumi mencatatkan TPT tertinggi  di atas 7,5%. Sebaliknya, kabupaten seperti Pangandaran dan Ciamis memiliki TPT terendah di bawah 4%. Pada kabupaten Pangandaran, TPT yang rendah ini sejalan dengan Tingkat Partisipasi Angkatan Kerja (TPAK) yang sangat tinggi di daerah tersebut. Hal ini mengindikasikan bahwa di daerah ini  mayoritas penduduk usia kerja aktif dan terserap dengan baik.

## 2. Barchart TPAK
<p align="center">
  <img src="Image/Barchart%20TPAK.png" width="500">
</p>
<p align="center"><i>Gambar 2. Barchart Tingkat Partisipasi Angkatan Kerja. </i></p>

Berdasarkan grafik Tingkat Partisipasi Angkatan Kerja (TPAK) di Kabupaten/Kota Jawa Barat, menunjukkan bahwa Pangandaran memimpin dengan TPAK tertinggi yaitu 80% diikuti oleh kabupaten seperti Cianjur dan Subang. Hal ini mengindikasikan tingginya keterlibatan penduduk usia kerja dalam kegiatan ekonomi di wilayah-wilayah yang cenderung merupakan kawasan agraris atau pedesaan. Di sisi lain, kota-kota besar dan metropolitan seperti Kota Depok dan Kota Sukabumi mencatatkan TPAK terendah dengan nilai di bawah 70%.

## 3. Barchart RLS
<p align="center">
  <img src="Image/Barchart%20RLS.png" width="500">
</p>
<p align="center"><i>Gambar 3. Barchart Rata-Rata Lama Sekolah. </i></p>

Grafik Rata-Rata Lama Sekolah (RLS) di berbagai Kabupaten/Kota di Jawa Barat, menunjukkan bahwa kota-kota besar dan metropolitan seperti Kota Bekasi, Kota Depok, dan Kota Cimahi menempati posisi teratas dengan RLS tertinggi rata-rata 11 tahun. Hal ini mengindikasikan bahwa rata-rata penduduk di daerah tersebut telah menempuh pendidikan setara SMA atau lebih. Sebaliknya, wilayah-wilayah seperti Kabupaten Subang, Sukabumi dan Indramayu berada di posisi terbawah dengan RLS lebih rendah yaitu di bawah 8.0 tahun yang mengindikasikan bahwa rata-rata penduduknya hanya memiliki pendidikan setara SMP atau di bawahnya.

## 4. Barchart Laju Pertumbuhan Penduduk
<p align="center">
  <img src="Image/Barchart%20Laju%20Pertumbuhan.png" width="500">
</p>
<p align="center"><i>Gambar 4. Barchart Rata-Rata Laju Pertumbuhan Penduduk. </i></p>

Grafik rata-rata laju pertumbuhan penduduk di berbagai kabupaten/kota di Provinsi Jawa Barat, menunjukkan bahwa Kota Sukabumi, Kabupaten Bandung Barat, dan Kota Cimahi menempati posisi dengan laju pertumbuhan penduduk tertinggi dengan nilai pertumbuhan penduduk rata-rata 1,4%. Hal ini mungkin didorong oleh tingginya urbanisasi dan migrasi masuk seiring perkembangan wilayah tersebut menjadi pusat ekonomi. Adapun, daerah seperti Kabupaten Sumedang, Pangandaran, dan Ciamis berada pada posisi terbawah dengan laju pertumbuhan penduduk yang relatif rendah di bawah 1%.

---
# 🔗Hasil Uji Korelasi

## 1. TPT vs Laju Pertumbuhan Penduduk
<p align="center">
  <img src="Image/Korelasi%20Penduduk%20dan%20TPT.jpg" width="400">
</p>
<p align="center"><i>Gambar 5. Hasil Koefisien Korelasi TPT vs Laju Pertumbuhan Penduduk. </i></p>

<ul> <li><strong>Koefisien Korelasi (cor) = 0.649 (positif)</strong><br> Artinya terdapat hubungan searah antara Penduduk dan TPT. <div style="margin-left:20px;"> &gt; Jika Penduduk meningkat, maka TPT juga cenderung meningkat.<br> &gt; Nilai korelasi 0.649 menunjukkan hubungan linear cukup kuat (atau sedang). </div> </li> <li><strong>p-value = 0.0002497 &lt; 0.05</strong><br> Artinya H0 ditolak → terdapat hubungan linear yang signifikan antara Penduduk dan TPT. </li> <li><strong>Selang Kepercayaan (95%) = 0.3571215 sampai 0.8254657</strong><br> Rentang tidak mencakup 0 → hubungan signifikan. </li> </ul>

## 2. TPT vs RLS
<p align="center">
  <img src="Image/Korelasi%20RLS%20dan%20TPT.jpg" width="400">
</p>
<p align="center"><i>Gambar 6. Hasil Koefisien Korelasi TPT vs RLS. </i></p>

<ul> <li><strong>Koefisien Korelasi (cor) = 0.421 (positif)</strong><br> Artinya terdapat hubungan searah antara RLS dan TPT. <div style="margin-left:20px;"> &gt; Jika RLS meningkat, maka TPT cenderung meningkat.<br> &gt; Nilai korelasi 0.421 menunjukkan hubungan linear yang lemah. </div> </li> <li><strong>p-value = 0.028767 &lt; 0.05</strong><br> Artinya H0 ditolak → terdapat hubungan linear signifikan antara RLS dan TPT. </li> <li><strong>Selang Kepercayaan (95%) = 0.049 sampai 0.691</strong><br> Rentang tidak mencakup 0 → hubungan signifikan. </li> </ul>

## 3. TPT vs TPAK
<p align="center">
  <img src="Image/Korelasi%20TPAK%20dan%20TPT.jpg" width="400">
</p>
<p align="center"><i>Gambar 7. Hasil Koefisien Korelasi TPT vs TPAK. </i></p>

<ul> <li><strong>Koefisien Korelasi (cor) = -0.652 (negatif)</strong><br> Artinya terdapat hubungan berlawanan arah antara TPAK dan TPT. <div style="margin-left:20px;"> &gt; Jika TPAK meningkat, maka TPT cenderung menurun.<br> &gt; Nilai korelasi -0.652 menunjukkan hubungan linear cukup kuat. </div> </li> <li><strong>p-value = 0.0002307 &lt; 0.05</strong><br> Artinya H0 ditolak → terdapat hubungan linear signifikan dan kuat. </li> <li><strong>Selang Kepercayaan (95%) = -0.8269401 sampai -0.3611676</strong><br> Rentang tidak mencakup 0 → hubungan signifikan. </li> </ul>

---
# ⚙️Uji Asumsi
## 1. Uji Normalitas
<p align="center">
  <img src="Image/Normal%20Plot.png" width="350">
</p>
<p align="center"><i>Gambar 8. Plot Normal Q-Q . </i></p>

Uji normalitas berfungsi untuk menguji apakah nilai residual dalam model regresi memiliki distribusi normal atau tidak. Berdasarkan plot Normal Q-Q, dapat diketahui bahwa mayoritas sebaran data berada di sekitar garis lurus yang mengindikasikan bahwa data tersebut berdistribusi normal. Meskipun demikian analisis visual bersifat subjektif, maka diperlukan uji statistik seperti <i>Shapiro-Wilk</i> untuk memperoleh kesimpulan yang lebih objektif.

<p align="center">
  <img src="Image/Uji%20Normalitas.jpg" width="280">
</p>
<p align="center"><i>Gambar 9. Hasil Uji Normalitas. </i></p>

Pengujian normalitas dilakukan menggunakan <i>Shapiro – Wilk</i> karena jumlah data yang digunakan dalam penelitian ini adalah < 50. Berdasarkan hasil uji tersebut diperoleh nilai 𝑝 − 𝑣𝑎𝑙𝑢𝑒 (0.4613) > 𝛼 (0.05). Hal ini berarti bawa residual menyebar normal dan asumsi normalitas terpenuhi.

## 2. Uji Multikolinearitas

<p align="center">
  <img src="Image/VIF.jpg" width="225">
</p>
<p align="center"><i>Gambar 10. Hasil Uji multikolinearitas. </i></p>

Uji multikolinearitas bertujuan untuk menguji apakah dalam model regresi terdapat korelasi antar variabel independen. Salah satu cara yang digunakan untuk mendeteksi adanya multikolinearitas adalah mengecek nilai VIF. Hasil uji multikolinearitas diperoleh nilai VIF masing-masing variabel yaitu Penduduk (1.313), RLS (1.244), TPAK (1.587) < 5. Hal ini berarti tidak terjadi multikolineritas atau asumsi multikolinearitas terpenuhi.

## 3. Uji Heteroskedastisitas

<p align="center">
  <img src="Image/Uji%20Heteroskedastisitas.jpg" width="300">
</p>
<p align="center"><i>Gambar 11. Hasil Uji Heteroskedastisitas. </i></p>

Uji heteroskedastisitas bertujuan untuk melihat apakah dalam model regresi terjadi ketidaksamaan varians dari residual satu pengamatan ke pengamatan yang lain. Jika varians dari nilai residual antar pengamatan tetap maka disebut homokedastis. Akan tetapi jika berbeda, maka disebut heteroskedastisitas. Model regresi yang baik adalah model yang bersifat homokedastis. Berdasarkan uji <i>Breusch Pagan Godfrey</i> diperoleh nilai 𝑝 − 𝑣𝑎𝑙𝑢𝑒 (0.4251) > 𝛼 (0.05). Hal ini berarti bahwa tidak terdapat indikasi heteroskedastisitas atau asumsi uji heteroskedastisitas sudah terpenuhi.

---
# 📈Hasil Analisis Regresi Berganda
Pengolahan dengan analisis Regreasi Berganda memperoleh hasil sebagai berikut.
<p align="center">
  <img src="Image/Hasil%20Regresi.jpg" width="450">
</p>
<p align="center"><i>Gambar 12. Hasil Analisi Regresi Berganda. </i></p>
Berdasarkan hal tersebut, dapat diinterpretasikan hal-hal sebagai berikut.

## 1. Uji Overall (Uji F)
Dari tabel diperoleh hasil uji signifikansi model secara keseluruhan:
<ul>
 <li><em>F-statistic</em>: 11.63</li>
 <li><em><i>p-value</i></em>: 7.709e-05</li> 
</ul>	

Nilai *p-value* yang sangat kecil (7.709e-05), jauh di bawah tingkat signifikansi umum α = 0.05 menunjukkan bahwa hipotesis nol (H0) yang menyatakan semua koefisien variabel independen (Laju Pertumbuhan Penduduk, RLS, TPAK) secara bersama-sama sama dengan hipotesis nol ditolak. 

Kesimpulan: 
<br>Model secara statistik menyatakan bahwa setidaknya ada satu variabel independen (Laju Pertumbuhan Penduduk, RLS, atau TPAK) yang berpengaruh secara signifikan terhadap variabel dependen TPT.

## 2. Uji Parsial (Uji t)
Ini menguji pengaruh masing-masing variabel independen terhadap variabel dependen secara individu, dengan asumsi variabel lain konstan, dan α = 0.05.

<p align="center">
  <img src="Image/Uji%20Parsial.png" width="45%" />
  <img src="Image/Arah%20Faktor.jpg" width="45%" />
</p>
<p align="center"><i>Gambar 13. (a). Plot P-Value Uji Parsial (b). Kekuatan Pengaruh.</i></p>
</p>

### a.	Variabel: Penduduk
<i>p-value</i> (0.00643) < 0.05, ➜ variabel Penduduk berpengaruh positif dan signifikan terhadap TPT. Artinya, jika variabel RLS dan TPAK konstan, maka setiap kenaikan 1 persen pada Penduduk akan meningkatkan TPT sebesar 3.22%.

### b.	Variabel: RLS
<i>p-value</i> (0.18278) > 0.05 ➜ variabel RLS berpengaruh tidak signifikan terhadap TPT. Artinya, tidak ada bukti statistik yang cukup untuk menyatakan bahwa RLS memiliki pengaruh terhadap TPT dalam model ini.

### c.	Variabel: TPAK
<i>p-value</i> (0.04839) < 0.05 ➜ variabel TPAK berpengaruh negatif dan signifikan terhadap TPT. Artinya, jika variabel Penduduk dan RLS konstan, maka setiap kenaikan 1 persen pada TPAK akan menurunkan TPT sebesar 0.17%.

## 	3. Persamaan Model Regresi
Berdasarkan nilai Estimate tersebut, maka persamaan regresi yang terbentuk adalah:

$$
\text{TPT} = 12.228 + 3.224 \cdot \text{Penduduk} + 0.258 \cdot \text{RLS} - 0.171 \cdot \text{TPAK}
$$

## 	4. Koefisien Determinasi
Nilai koefisien determinasi (**R²**) yang diperoleh adalah **55.09%**.

**Artinya:**

- Model regresi mampu menjelaskan **55.09% variasi** pada **Tingkat Pengangguran Terbuka (TPT)** berdasarkan variabel **Laju Pertumbuhan Penduduk**, **Rata-Rata Lama Sekolah (RLS)**, dan **Tingkat Partisipasi Angkatan Kerja (TPAK)**.

- Sisanya, yaitu **44.91%**, dipengaruhi oleh faktor lain.

---
# 📝Kesimpulan
1. Variabel Penduduk, RLS, dan TPAK memiliki hubungan linear dengan TPT, di mana Penduduk berpengaruh positif dan signifikan, TPAK berpengaruh negatif dan signifikan, sedangkan RLS memiliki pengaruh lebih lemah.
2. Variabel yang paling signifikan memengaruhi TPT adalah Laju Pertumbuhan Penduduk dan TPAK. RLS tidak menjadi faktor dominan. Hal ini mengindikasikan bahwa kebijakan penurunan TPT dapat difokuskan pada peningkatan partisipasi tenaga kerja dan pengelolaan pertumbuhan penduduk.


<h2 align="center">👥 Anggota Kelompok 2</h2>

<table align="center">
<tr>

  <td align="center" width="30%" style="padding:15px;">

  ### 👤 Arifatun Nisak Nur Amanati  
  NIM: M0501251027

  </td>

  <td align="center" width="30%" style="padding:15px;">

  ### 👤 Haniyathul Husna  
  NIM: M0501251011

  </td>

  <td align="center" width="30%" style="padding:15px;">

  ### 👤 Lalu Muhammad Yahya  
  NIM: M0501251015

  </td>

</tr>

<tr>

  <td align="center" width="30%" style="padding:15px;">

  ### 👤 Ni Made Ray Diantari  
  NIM: M0501251033

  </td>

  <td align="center" width="30%" style="padding:15px;">

  ### 👤 Suci Rahmadani  
  NIM: M0501251057

  </td>

  <!-- Kolom kosong untuk keseimbangan -->
  <td align="center" width="30%" style="padding:15px; color:transparent;">
  .
  </td>

</tr>
</table>
