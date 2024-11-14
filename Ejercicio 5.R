# Leer los datos
datos <- read.csv("Peso_hom_muj.csv", sep=";")

# Realizar test de normalidad Shapiro-Wilk para cada grupo
shapiro_hombres <- shapiro.test(datos$Peso_Hombres)
shapiro_mujeres <- shapiro.test(datos$Peso_Mujeres)

# Mostrar resultados de pruebas de normalidad
cat("Test de Shapiro-Wilk para Hombres:\n")
print(shapiro_hombres)
cat("\nTest de Shapiro-Wilk para Mujeres:\n")
print(shapiro_mujeres)

# Estadísticas descriptivas
cat("\nEstadísticas descriptivas:\n")
cat("Media Hombres:", mean(datos$Peso_Hombres), "\n")
cat("Media Mujeres:", mean(datos$Peso_Mujeres), "\n")
cat("DE Hombres:", sd(datos$Peso_Hombres), "\n")
cat("DE Mujeres:", sd(datos$Peso_Mujeres), "\n")

# Si los datos son normales, usar t.test
# Si no son normales, usar wilcox.test
if(shapiro_hombres$p.value > 0.05 && shapiro_mujeres$p.value > 0.05) {
  # Realizar prueba t de Student
  cat("\nRealizando prueba t de Student:\n")
  test_resultado <- t.test(datos$Peso_Hombres, datos$Peso_Mujeres)
} else {
  # Realizar prueba de Wilcoxon
  cat("\nRealizando prueba de Wilcoxon:\n")
  test_resultado <- wilcox.test(datos$Peso_Hombres, datos$Peso_Mujeres)
}

print(test_resultado)