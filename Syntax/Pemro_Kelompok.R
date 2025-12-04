#input data
pemro = read.delim("clipboard")
pemro

#uji korelasi
cor.test(pemro$Penduduk, pemro$TPT)
cor.test(pemro$RLS, pemro$TPT)
cor.test(pemro$TPAK, pemro$TPT)

library(nortest)
#melakukan analisis regresi
regresi = lm(TPT~Penduduk+RLS+TPAK, data=pemro)

#uji asumsi klasik
#uji normalitas
resid_std <- rstandard(regresi)
shapiro.test(resid_std)
qqnorm(resid_std); qqline(resid_std, col="blue", lwd=2)

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

# Barchart TPT
ggplot(pemro, aes(x = TPT, 
                  y = fct_reorder(Kab.Kota, TPT, .desc = FALSE), 
                  fill = TPT)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "green", high = "red", name = "TPT") +
  labs(title = "Tingkat Pengangguran Terbuka",
       x = "Average TPT",
       y = "Kab/Kota di Indonesia") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    legend.position = "right"     # ← Legend muncul di kanan
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

# Barchart TPAK
ggplot(pemro, aes(x = TPAK, 
                  y = fct_reorder(Kab.Kota, TPAK, .desc = FALSE), 
                  fill = TPAK)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_c(option = "viridis", name = "TPAK") +
  labs(title = "Tingkat Partisipasi Angkatan Kerja",
       x = "Average TPAK",
       y = "Kab/Kota di Indonesia") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    axis.title.y = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold")
  ) +
  coord_cartesian(xlim = c(60, 80))

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


library(dplyr)
library(knitr)
# Membuat tabel koefisien secara manual
tabel_coef <- data.frame(
  Variable = c("(Intercept)", "Penduduk", "RLS", "TPAK"),
  Estimate = c(12.22880, 3.22454, 0.25801, -0.17110),
  Std_Error = c(7.09531, 1.07573, 0.18782, 0.08208),
  t_value = c(1.724, 2.998, 1.374, -2.085),
  Pr = c(0.09822, 0.00643, 0.18278, 0.04839)
)

kable(tabel_coef, caption = "Tabel Koefisien Regresi")

coef_table_filtered <- tabel_coef %>%
  mutate(Arah = ifelse(Estimate > 0,
                       "Positif (Meningkat)",
                       "Negatif (Menurun)"))

# Hapus Intercept
coef_table_filtered <- tabel_coef %>% 
  filter(Variable != "(Intercept)") %>% 
  mutate(
    Arah = ifelse(Estimate > 0,
                  "Positif (Meningkat)",
                  "Negatif (Menurun)"),
    hjust_label = ifelse(Estimate > 0, -0.2, 1.1)  # posisi teks
  )

# PlotKekuatan Estimate
ggplot(coef_table_filtered,
       aes(x = Variable, y = Estimate, fill = Arah)) +
  geom_col(alpha = 0.8, width = 0.7) +
  coord_flip() +
  geom_text(aes(label = round(Estimate, 3),
                hjust = hjust_label),
            size = 2.5, fontface = "bold") +
  scale_fill_manual(
    values = c("Negatif (Menurun)" = "red",
               "Positif (Meningkat)" = "darkgreen")
  ) +
  ylim(min(coef_table_filtered$Estimate) - 0.2,
       max(coef_table_filtered$Estimate) + 0.2) +
  labs(
    title = "Faktor Penentu Tingkat Pengangguran Terbuka",
    y     = "Kekuatan Pengaruh (Estimate)",
    x     = "Variable",
    fill  = "Arah Pengaruh"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 8, face = "bold")
  )




