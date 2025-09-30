#input data
pemro = read.delim("clipboard")
pemro

#melakukan analisis regresi
regresi = lm(TPT~UM+Penduduk+RLS, data=pemro)
summary(regresi)

#RLS tidak signifikan
regresi2 = lm(TPT~UM+Penduduk, data=pemro)
summary(regresi2)

#uji korelasi
cor.test(pemro$UM, pemro$TPT)
cor.test(pemro$Penduduk, pemro$TPT)

#uji asumsi klasik
#uji normalitas
library(nortest)
shapiro.test(regresi2$residuals)

#uji autokorelasi
library(lmtest)
dwtest(regresi2)

#uji multikolinieritas
library(car)
vif(regresi2)

#uji heteroskedastisitas
bptest(regresi2, studentize=TRUE, data=pemro)
