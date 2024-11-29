# Charger les données
urlPath <- "https://hbiostat.org/data/repo/rhc.csv"
rawData <- read.csv(urlPath, header = TRUE)

# Afficher un aperçu des données
head(rawData)
str(rawData)


# Variables choisies
quant_vars <- c("age", "meanbp1", "resp1", "hrt1", "temp1", "wblc1", "pafi1", "alb1", 
                "hema1", "bili1", "crea1", "das2d3pc", "surv2md1", "ph1", "edu")
bin_vars <- c("sex", "death" , "malighx", "cardiohx", "chfhx")


# Verifier le nombre de valeurs manquantes par colonne
colSums(is.na(rawData))
# Sous-ensemble des variables sélectionnées
selected_vars <- c(quant_vars, bin_vars)

# Nombre de valeurs manquantes pour ces variables
colSums(is.na(rawData[selected_vars]))


# Conversion des variables binaires en facteurs
rawData$sex <- as.factor(rawData$sex)
rawData$death <- as.factor(rawData$death)
rawData$cardiohx <- as.factor(rawData$cardiohx)
rawData$chfhx <- as.factor(rawData$chfhx)

# Recoder sex
rawData$sex <- factor(rawData$sex, levels = c("Male", "Female"), labels = c(0, 1))

# Recoder death
rawData$death <- factor(rawData$death, levels = c("No", "Yes"), labels = c(0, 1))
# Convertir en facteur après recodage
rawData$sex <- as.factor(rawData$sex)
rawData$death <- as.factor(rawData$death)


# question2
# Liste des variables quantitatives et binaires
quant_vars <- c("age", "meanbp1", "resp1", "hrt1", "temp1", "wblc1", "pafi1", "alb1", 
                "hema1", "bili1", "crea1", "das2d3pc", "surv2md1", "ph1", "edu")
bin_vars <- c("sex", "death", "malighx", "cardiohx", "chfhx")

# Création de la colonne group (RHC vs No RHC)
rawData$group <- ifelse(rawData$swang1 == "RHC", "RHC", "No RHC")

# Fonction pour calculer le SMD pour les variables continues
smd_continuous <- function(var) {
  mean_rhc <- mean(rawData[[var]][rawData$group == "RHC"], na.rm = TRUE)
  mean_no_rhc <- mean(rawData[[var]][rawData$group == "No RHC"], na.rm = TRUE)
  sd_rhc <- sd(rawData[[var]][rawData$group == "RHC"], na.rm = TRUE)
  sd_no_rhc <- sd(rawData[[var]][rawData$group == "No RHC"], na.rm = TRUE)
  smd <- abs(mean_rhc - mean_no_rhc) / sqrt((sd_rhc^2 + sd_no_rhc^2) / 2)
  return(smd)
}

# Fonction pour calculer le SMD pour les variables binaires
smd_binary <- function(var) {
  count_rhc <- sum(rawData[[var]] == 1 & rawData$group == "RHC", na.rm = TRUE)
  count_no_rhc <- sum(rawData[[var]] == 1 & rawData$group == "No RHC", na.rm = TRUE)
  n_rhc <- sum(rawData$group == "RHC", na.rm = TRUE)
  n_no_rhc <- sum(rawData$group == "No RHC", na.rm = TRUE)
  
  if (n_rhc == 0 | n_no_rhc == 0) {
    return(NA)  # Retourner NA si l'un des groupes est vide
  }
  
  prop_rhc <- (count_rhc / n_rhc) * 100
  prop_no_rhc <- (count_no_rhc / n_no_rhc) * 100
  return(abs(prop_rhc - prop_no_rhc))
}

# Fonction pour calculer les statistiques (moyenne, SD, n, proportions) pour les variables
stats_table <- function(var, is_binary = FALSE) {
  if (is_binary) {
    # Effectifs et proportions pour les variables binaires
    n_rhc <- sum(rawData$group == "RHC", na.rm = TRUE)
    n_no_rhc <- sum(rawData$group == "No RHC", na.rm = TRUE)
    
    # Effectifs et proportions pour RHC
    count_rhc <- sum(rawData[[var]] == 1 & rawData$group == "RHC", na.rm = TRUE)
    count_no_rhc <- sum(rawData[[var]] == 1 & rawData$group == "No RHC", na.rm = TRUE)
    
    # Calcul des proportions (en %)
    prop_rhc <- (count_rhc / n_rhc) * 100
    prop_no_rhc <- (count_no_rhc / n_no_rhc) * 100
    
    # Retourner les statistiques pour binaires
    return(c(sprintf("%d (%0.1f)", count_rhc, prop_rhc), 
             sprintf("%d (%0.1f)", count_no_rhc, prop_no_rhc)))
  } else {
    # Moyenne et écart-type pour les variables continues
    mean_rhc <- mean(rawData[[var]][rawData$group == "RHC"], na.rm = TRUE)
    mean_no_rhc <- mean(rawData[[var]][rawData$group == "No RHC"], na.rm = TRUE)
    sd_rhc <- sd(rawData[[var]][rawData$group == "RHC"], na.rm = TRUE)
    sd_no_rhc <- sd(rawData[[var]][rawData$group == "No RHC"], na.rm = TRUE)
    return(c(sprintf("%0.2f (%0.2f)", mean_rhc, sd_rhc), 
             sprintf("%0.2f (%0.2f)", mean_no_rhc, sd_no_rhc)))
  }
}

# Calculer le tableau pour toutes les variables
table_data <- list()

# Pour les variables quantitatives
for (var in quant_vars) {
  stats_rhc_no_rhc <- stats_table(var, is_binary = FALSE)
  smd_value <- smd_continuous(var)
  table_data[[var]] <- c(var, stats_rhc_no_rhc, smd_value)
}

# Pour les variables binaires
for (var in bin_vars) {
  stats_rhc_no_rhc <- stats_table(var, is_binary = TRUE)
  smd_value <- smd_binary(var)
  table_data[[var]] <- c(var, stats_rhc_no_rhc, smd_value)
}

# Conversion de la liste en un tableau de données
result_table <- do.call(rbind, table_data)
colnames(result_table) <- c("Variable", "RHC", "No RHC", "SMD")

# Afficher le tableau final
print(result_table)

# Charger les bibliothèques
library(gridExtra)
library(knitr)

# Définir le nom du fichier PDF de sortie
output_file <- "tableau_resultat.pdf"

# Ouvrir le fichier PDF pour enregistrer le tableau
pdf(output_file, width = 8, height = 6) 

# Afficher le tableau dans le fichier PDF
grid.table(result_table)  

# Fermer le fichier PDF
dev.off()

# Message de confirmation
cat("Le tableau a été enregistré dans", output_file, "\n")

# question3
# Charger la librairie ggplot2
library(ggplot2)

# Trier les données par ordre décroissant de SMD
library(dplyr)
results_sorted <- results%>%arrange(desc(SMD))

# Créer un nuage de points
ggplot(results_sorted, aes(x = SMD, y = reorder(Variable, SMD))) +
  geom_point(shape = 1, color = "black", size = 2) +
  labs(title = "Absolute Standardized Difference", x = "SMD", y = "Variable") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5))

# question4
## Randomiser le traitement RHC
set.seed(123)  # Pour reproductibilité
randomized_treatment <- sample(c("RHC", "No RHC"), size = nrow(rawData), replace = TRUE)

# Ajouter les traitements randomisés à la base de données
rawData$randomized_treatment <- randomized_treatment


#Q5  Recalculer les SMD pour les données après avoir randomisé le traitement

# Recalcul des SMD pour les variables quantitatives après randomisation
smd_continuous_randomized <- function(var) {
  mean_rhc <- mean(rawData[[var]][rawData$randomized_treatment == "RHC"], na.rm = TRUE)
  mean_no_rhc <- mean(rawData[[var]][rawData$randomized_treatment == "No RHC"], na.rm = TRUE)
  sd_rhc <- sd(rawData[[var]][rawData$randomized_treatment == "RHC"], na.rm = TRUE)
  sd_no_rhc <- sd(rawData[[var]][rawData$randomized_treatment == "No RHC"], na.rm = TRUE)
  smd <- abs(mean_rhc - mean_no_rhc) / sqrt((sd_rhc^2 + sd_no_rhc^2) / 2)
  return(smd)
}

# Recalcul des SMD pour les variables binaires après randomisation
smd_binary_randomized <- function(var) {
  count_rhc <- sum(rawData[[var]] == 1 & rawData$randomized_treatment == "RHC", na.rm = TRUE)
  count_no_rhc <- sum(rawData[[var]] == 1 & rawData$randomized_treatment == "No RHC", na.rm = TRUE)
  n_rhc <- sum(rawData$randomized_treatment == "RHC", na.rm = TRUE)
  n_no_rhc <- sum(rawData$randomized_treatment == "No RHC", na.rm = TRUE)
  
  if (n_rhc == 0 | n_no_rhc == 0) {
    return(NA)  # Retourner NA si l'un des groupes est vide
  }
  
  prop_rhc <- (count_rhc / n_rhc) * 100
  prop_no_rhc <- (count_no_rhc / n_no_rhc) * 100
  return(abs(prop_rhc - prop_no_rhc))
}

# Calcul des nouveaux SMD pour toutes les variables
smd_randomized <- list()

# Pour les variables quantitatives
for (var in quant_vars) {
  smd_value <- smd_continuous_randomized(var)
  smd_randomized[[var]] <- smd_value
}

# Pour les variables binaires
for (var in bin_vars) {
  smd_value <- smd_binary_randomized(var)
  smd_randomized[[var]] <- smd_value
}

# Convertir en un dataframe
smd_randomized_df <- data.frame(
  Variable = names(smd_randomized),
  SMD_Randomized = unlist(smd_randomized)
)
# Fusionner les SMD avant et après randomisation
smd_combined <- results_sorted %>%
  left_join(smd_randomized_df, by = "Variable") %>%
  arrange(desc(SMD))

# Créer un graphique montrant les deux SMD
ggplot(smd_combined, aes(y = reorder(Variable, SMD))) +
  # Points pour les SMD observés
  geom_point(aes(x = SMD, color = "Observed"), size = 2, shape = 1) +
  # Points pour les SMD randomisés
  geom_point(aes(x = SMD_Randomized, color = "Randomized"), size = 2, shape = 15) +
  # Ajout d'une légende personnalisée
  scale_color_manual(
    name = NULL,
    values = c("Observed" = "black", "Randomized" = "blue")
  ) +
 
  # Ajouter le titre en bas
  labs(
    title = "",
    x = NULL,
    y = NULL,
    caption = "Absolute Standardized Difference"
  ) +
  # Encadrer la légende sur le graphique
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),  # Centrer le titre
    legend.position = c(0.85, 0.2),  # Position de la légende (coordonnées x, y dans [0, 1])
    legend.background = element_rect(color = "black", fill = "white"),  # Rectangle autour de la légende
    legend.key = element_rect(fill = "white"),  # Fond des clés dans la légende
    plot.caption = element_text(hjust = 0.5, size = 12)  # Centrer et ajuster le texte du titre
  )




