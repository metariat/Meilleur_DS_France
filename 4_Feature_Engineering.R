View(head(data))
data[, com.duration := date.amm.annee - date.declar.annee]
data[, libelle := NULL]
data[, substances := NULL]
str(data, len = ncol(data))

