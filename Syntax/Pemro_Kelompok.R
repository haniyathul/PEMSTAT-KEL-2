#input data
pemro = read.delim("clipboard")
pemro

#uji korelasi
cor.test(pemro$Penduduk, pemro$TPT)
cor.test(pemro$RLS, pemro$TPT)
cor.test(pemro$TPAK, pemro$TPT)

#melakukan analisis regresi
regresi = lm(TPT~Penduduk+RLS+TPAK, data=pemro)

#uji asumsi klasik
#uji normalitas
library(nortest)
resid_std <- rstandard(regresi)
shapiro.test(resid_std)
qqnorm(resid_std); qqline(resid_std, col="blue", lwd=2)

model <- lm(log(TPT) ~ log(Penduduk) + log(RLS) + log(TPAK), data = pemro)
summary(model)
qqnorm(residual_std); qqline(residual_std, col="blue", lwd=2)

#uji multikolinieritas
library(car)
vif(regresi)

#uji heteroskedastisitas
library(lmtest)
bptest(regresi, studentize=TRUE, data=pemro)

summary(regresi)

library(ggplot2)
library(viridis)
library(forcats)

#Barchart TPT
ggplot(pemro, aes(x = TPT, y = fct_reorder(Kab.Kota, TPT, .desc = FALSE),, fill = TPT)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_c(option = "viridis", 
                       name = "TPT")+
  labs(title = "Tingkat Pengangguran Terbuka",
       x = "Average TPT",
       y = "Kab/Kota di Indonesia")
theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.position = "none"
  )

#Barchart RLS
ggplot(pemro, aes(x = RLS, y = fct_reorder(Kab.Kota, RLS, .desc = FALSE),, fill = RLS)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_c(option = "magma", 
                       name = "RLS")+
  labs(title = "Rata-Rata Lama Sekolah",
       x = "Average RLS",
       y = "Kab/Kota di Indonesia")
theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.position = "none"
  )

#Barchart TPAK
ggplot(pemro, aes(x = TPAK, y = fct_reorder(Kab.Kota, TPAK, .desc = FALSE),, fill = TPAK)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_c(option = "cividis", 
                       name = "TPAK")+
  labs(title = "Tingkat Partisipasi Angkatan Kerja",
       x = "Average TPAK",
       y = "Kab/Kota di Indonesia")
theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.position = "none"
  )

#Barchart Laju Pertumbuhan Penduduk
ggplot(pemro, aes(x = Penduduk, y = fct_reorder(Kab.Kota, Penduduk, .desc = FALSE),, fill = Penduduk)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_c(option = "plasma", 
                       name = "Penduduk")+
  labs(title = "Laju Pertumbuhan Penduduk",
       x = "Average Penduduk",
       y = "Kab/Kota di Indonesia")
theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.position = "none"
  )
