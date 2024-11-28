#Parti I question 1 importation fichier 

setwd("C:/Users/Acheteur/Desktop/CERTIFICAT/LOGC-STAT/TP2")
getwd()

#lecture fichier csv
actes_criminels <- read.csv("actes-criminels_in_.csv", sep = ",", header = TRUE)

# Installer le package xlsx
install.packages("readxl") 
library(readxl)
#lecture fichier xlsx
pdq_description <- read_excel("pdq_description.xlsx")

# question 2 Jointure entre les deux tables

actes_criminels_complet <- merge(actes_criminels, pdq_description, 
                                 by.x = "PDQ",  
                                 by.y = "code_PDQ",  
                                 all.x = TRUE)
# question 3 tester jointure 

# A- Filtrer les PDQ où la description est manquante (NA)
pdq_sans_description <- actes_criminels_complet[is.na(actes_criminels_complet$Description), ]

# B- Nombre de lignes où la description est manquante
nombre_lignes_sans_description <- nrow(pdq_sans_description)

# C- Supprimer les lignes où la description est NA
actes_criminels_nettoye <- actes_criminels_complet[!is.na(actes_criminels_complet$Description), ]

# D- LE PDQ enregistre le plus grand nombre de crimes Et le plus faible nombre
# Étape 1 : Compter le nombre de crimes par PDQ
pdq_counts <- summary(as.factor(actes_criminels_nettoye$PDQ))

# Étape 2 :Transformer le résultat en data.frame
pdq_crimes <- data.frame(
  PDQ = names(pdq_counts),          # Les numéros des PDQ
  Crimes = as.numeric(pdq_counts)   # Les nombres de crimes
)

# Étape 3 :Ordonner les données par ordre croissant
pdq_crimes_sorted <- pdq_crimes[order(pdq_crimes$Crimes), ]

# les PDQ avec le plus faible et le plus grand nombre de crimes
pdq_least_crimes <- pdq_crimes_sorted[1, ]  # PDQ avec le moins de crimes
pdq_most_crimes <- pdq_crimes_sorted[nrow(pdq_crimes_sorted), ]  # PDQ avec le plus de crimes
# pour affichage 
pdq_least_crimes 
pdq_most_crimes

# Question 4  Boucle de partage par description
# Initialiser une liste vide
crimes_by_PDQ <- list()

# Obtenir les valeurs uniques de la colonne Description
unique_descriptions <- unique(actes_criminels_nettoye$Description)

# Boucle for pour remplir la liste
for (desc in unique_descriptions) {
  
  crimes_by_PDQ[[desc]] <- actes_criminels_nettoye[actes_criminels_nettoye$Description == desc, ]
}
# pour affichage 
crimes_by_PDQ

# question 5 Généralisation de la fonction
# Vérifier si split_var est une colonne valide
split_df_to_list <- function(entry_df, split_var, selected_var = NULL) {
  if (!split_var %in% names(entry_df)) {
    stop("La variable de partitionnement (split_var) n'est pas dans le data.frame.")
  }
# Manipulation des colonnes selected_
  if (!is.null(selected_var)) {

    if (!all(selected_var %in% names(entry_df))) {
      stop("Certaines colonnes de selected_var ne sont pas présentes dans entry_df.")
    }
    entry_df <- entry_df[, c(split_var, selected_var), drop = FALSE]
  }

  unique_values <- unique(entry_df[[split_var]])

# Initialiser une liste vide
  result_list <- list()
  
# Boucle pour remplir la liste
  for (value in unique_values) {
    result_list[[as.character(value)]] <- entry_df[entry_df[[split_var]] == value, ]
  }
  
  return(result_list)
}

# question 6 Tester la fonction avec les paramètres donnés
resultat_test <- split_df_to_list(
  entry_df = actes_criminels_complet,       
  split_var = "CATEGORIE",                  
  selected_var = c("LONGITUDE", "LATITUDE", "DATE", "QUART")  
)

# partie II question 1- adaptation de code 

get_circle_points <- function(center_long,
                              center_lat,
                              radius_in_km,
                              nb_of_points){
# Convertir le rayon en degrés de longitude et latitude
  radiusLon = 1 / (111.319 * cos(center_lat * (pi / 180))) * radius_in_km
  radiusLat = 1 / 110.574 * radius_in_km
  
# Calcul de l'incrémentation angulaire
  dTheta = 2 * pi / nb_of_points
  theta = 0
  
# Un data.frame vide dans lequel on va stocker le résultat au fur et à mesure des itérations de la boucle for
  points_on_circle = data.frame(LONGITUDE = numeric(0), LATITUDE = numeric(0))
  
  for (i in 1:nb_of_points){
# Ajouter les points calculés au data.frame
    points_on_circle <- rbind(
      points_on_circle,
      data.frame(
        center_long + radiusLon * cos(theta),
        center_lat + radiusLat * sin(theta)
      )
    )
    theta = theta + dTheta
  }
  
  points_on_circle <- data.frame(points_on_circle)
  
# Ajustez le nom des colonnes du dataframe de sortie
  colnames(points_on_circle) <- c('LONGITUDE', 'LATITUDE')
  
  points_on_circle$geographical_quarter <- 'West - South'
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE >= center_lat & points_on_circle$LONGITUDE <= center_long] <- 'West - North'
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE >= center_lat & points_on_circle$LONGITUDE > center_long] <- 'East - North'
  
# En vous inspirant de ce qui est fait dans les 2 lignes au-dessus, écrivez la dernière condition
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE < center_lat & points_on_circle$LONGITUDE > center_long] <- 'East - South'  # Ajoutez le point central défini en paramètre d’entrée
  points_on_circle = rbind(points_on_circle, c(center_long, center_lat, 'Center'))
  
# Ajustez les formats à numérique
  points_on_circle$LONGITUDE <- as.numeric(points_on_circle$LONGITUDE)
  points_on_circle$LATITUDE <- as.numeric(points_on_circle$LATITUDE)
  
# On veut retourner ET AFFICHER AUTO le dataframe que l’on a construit dans la fonction
  print(points_on_circle)
  return(points_on_circle)
}

# Question 2 – Teste de  fonction
# Fonction pour générer les points d'un cercle
get_circle_points <- function(center_long, center_lat, radius_in_km, nb_of_points) {
# Convertir le rayon en degrés de longitude et latitude
  radiusLon = 1 / (111.319 * cos(center_lat * (pi / 180))) * radius_in_km
  radiusLat = 1 / 110.574 * radius_in_km
  
# Calcul de l'incrémentation angulaire
  dTheta = 2 * pi / nb_of_points
  theta = 0
  
# Un data.frame vide dans lequel on va stocker le résultat
  points_on_circle = data.frame(LONGITUDE = numeric(0), LATITUDE = numeric(0))
  
  for (i in 1:nb_of_points) {
    
    points_on_circle <- rbind(
      points_on_circle,
      data.frame(
        LONGITUDE = center_long + radiusLon * cos(theta),
        LATITUDE = center_lat + radiusLat * sin(theta)
      )
    )
    theta = theta + dTheta
  }
  
# Ajouter une colonne pour les quarts géographiques
  points_on_circle$geographical_quarter <- 'West - South'
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE >= center_lat & points_on_circle$LONGITUDE <= center_long] <- 'West - North'
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE >= center_lat & points_on_circle$LONGITUDE > center_long] <- 'East - North'
  points_on_circle$geographical_quarter[points_on_circle$LATITUDE < center_lat & points_on_circle$LONGITUDE > center_long] <- 'East - South'
  
# Ajouter le point central comme une ligne supplémentaire
  points_on_circle = rbind(points_on_circle, c(center_long, center_lat, 'Center'))
  
# Ajuster les formats à numérique
  points_on_circle$LONGITUDE <- as.numeric(points_on_circle$LONGITUDE)
  points_on_circle$LATITUDE <- as.numeric(points_on_circle$LATITUDE)
  
  # Retourner le dataframe
  return(points_on_circle)
}

# Tester la fonction avec les paramètres donnés
test1km <- get_circle_points(
  center_long = -73.6212999,  # Longitude du centre
  center_lat = 45.4983503,    # Latitude du centre
  radius_in_km = 1,           # Rayon de 1 km
  nb_of_points = 100          # 100 points pour caractériser le périmètre
)

# Visualiser les points
plot(
  test1km$LONGITUDE,          # Axe des X : Longitude
  test1km$LATITUDE,           # Axe des Y : Latitude
  col = as.factor(test1km$geographical_quarter), # Couleurs par quart géographique
  pch = 19,                   # Type de point
  main = "Points dans un rayon de 1 km",         # Titre
  xlab = "Longitude",         # Légende axe X
  ylab = "Latitude"           # Légende axe Y
)

# Ajouter une légende pour les quarts géographiques
legend("topright", 
       legend = unique(test1km$geographical_quarter), 
       col = as.factor(unique(test1km$geographical_quarter)), 
       pch = 19, 
       title = "Quart géographique")

# Question 3 -
# Déterminer les limites du cercle
min_lat <- min(test1km$LATITUDE)
max_lat <- max(test1km$LATITUDE)
min_lon <- min(test1km$LONGITUDE)
max_lon <- max(test1km$LONGITUDE)

# Filtrer la base de données actes_criminels_nettoye
# comme il ya des valeur manquant dans les variable long et latt on commence par Supprimer les NA dans LATITUDE et LONGITUDE
actes_criminels_nettoye <- actes_criminels_nettoye[!is.na(actes_criminels_nettoye$LATITUDE) & !is.na(actes_criminels_nettoye$LONGITUDE), ]

actes_criminels_filtered <- actes_criminels_nettoye[
  actes_criminels_nettoye$LATITUDE >= min_lat & actes_criminels_nettoye$LATITUDE <= max_lat &
    actes_criminels_nettoye$LONGITUDE >= min_lon & actes_criminels_nettoye$LONGITUDE <= max_lon,
]

# Question 4- Identifiez les points à l’intérieur du périmètre voulu.
in_the_cercle_v2 <- function(x, circle_point) {
  res <- all(c(
    # West - South
    any(
      circle_point[circle_point$geographical_quarter == 'West - South', 'LONGITUDE'] <= as.numeric(x[['LONGITUDE']]) &
        circle_point[circle_point$geographical_quarter == 'West - South', 'LATITUDE'] < as.numeric(x[['LATITUDE']])
    ),
    # West - North
    any(
      circle_point[circle_point$geographical_quarter == 'West - North', 'LONGITUDE'] <= as.numeric(x[['LONGITUDE']]) &
        circle_point[circle_point$geographical_quarter == 'West - North', 'LATITUDE'] > as.numeric(x[['LATITUDE']])
    ),
    # East - North
    any(
      circle_point[circle_point$geographical_quarter == 'East - North', 'LONGITUDE'] >= as.numeric(x[['LONGITUDE']]) &
        circle_point[circle_point$geographical_quarter == 'East - North', 'LATITUDE'] > as.numeric(x[['LATITUDE']])
    ),
    # East - South
    any(
      circle_point[circle_point$geographical_quarter == 'East - South', 'LONGITUDE'] >= as.numeric(x[['LONGITUDE']]) &
        circle_point[circle_point$geographical_quarter == 'East - South', 'LATITUDE'] < as.numeric(x[['LATITUDE']])
    )
  ))
  
  return(res)
}
# Exemple de test pour verification de la fonction
x <- actes_criminels_filtered[1, ]  
circle_point <- test1km  
# Tester si l'observation est dans le cercle
in_the_cercle_v2(x, circle_point)

# Question 5 – Rassemblez toutes les pièces
merge_all <- function(data_center_long, data_center_lat, entire_spatial_DB, radius_in_km, nb_radius_points = 100) {
# Étape 1 : Générer les points du cercle (Question 1)
  radius_points <- get_circle_points(
    center_long = data_center_long,
    center_lat = data_center_lat,
    radius_in_km = radius_in_km,
    nb_of_points = nb_radius_points
  )
  
# Étape 2 : Filtrer la base de données pour garder les observations dans le carré défini (Question 3)
  min_lat <- min(radius_points$LATITUDE)
  max_lat <- max(radius_points$LATITUDE)
  min_lon <- min(radius_points$LONGITUDE)
  max_lon <- max(radius_points$LONGITUDE)
  
  entire_spatial_DB_int <- entire_spatial_DB[
    entire_spatial_DB$LATITUDE >= min_lat & entire_spatial_DB$LATITUDE <= max_lat &
      entire_spatial_DB$LONGITUDE >= min_lon & entire_spatial_DB$LONGITUDE <= max_lon &
      !is.na(entire_spatial_DB$LATITUDE) & !is.na(entire_spatial_DB$LONGITUDE), 
  ]
  
# Étape 3 : Appliquer la fonction de la Question 4 sur chaque ligne (in_the_cercle_v2)
  res <- apply(entire_spatial_DB_int, 1, function(row) {
    in_the_cercle_v2(row, radius_points)
  })
  
# Ajouter une colonne indiquant si le crime est dans le rayon ou non
  entire_spatial_DB_int$is_in_circle <- res
  
# Retourner les résultats
  return(list(
    crimes_in_circle = entire_spatial_DB_int[entire_spatial_DB_int$is_in_circle == TRUE, ], # Crimes dans le rayon
    filtered_data = entire_spatial_DB_int, # Données filtrées dans le carré
    circle_points = radius_points          # Points définissant le cercle
  ))
}

# Question 6 - test de fonction 
# Tester la fonction merge_all avec les coordonnées de Côte-Sainte-Catherine
testRes <- merge_all(
  data_center_long = -73.6206714,  
  data_center_lat = 45.50333,      
  entire_spatial_DB = actes_criminels_nettoye, 
  radius_in_km = 1.5,              
  nb_radius_points = 100           
)

# Visualiser les données dans le périmètre
plot(
  testRes[[2]]$LONGITUDE, testRes[[2]]$LATITUDE, 
  col = as.factor(testRes[[1]][, 1]),           
  pch = 19,                                     
  main = "Analyse des crimes autour de Côte-Sainte-Catherine",
  xlab = "Longitude",
  ylab = "Latitude"
)

# Ajouter une légende pour les crimes
legend(
  "topright", 
  legend = unique(testRes[[1]][, 1]), 
  col = unique(as.factor(testRes[[1]][, 1])), 
  pch = 19, 
  title = "Crimes"
)




