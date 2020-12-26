library(tidyverse)
library(readxl)

FIFA18_Ultimate_Team_players <- read_excel("FIFA/FIFA18 - Ultimate Team players.xlsx")

# FIFA ULTIMATE TEAM ANALYSIS
fifa<-FIFA18_Ultimate_Team_players[FIFA18_Ultimate_Team_players$price_ps4 != "N/A",]

fifa$dribbling<-as.numeric(fifa$dribbling)
fifa$price_ps4<-as.numeric(fifa$price_ps4)

# Interessante Fakten ??ber den Datensatz: ####

# Durchschnittswerte:
mean(fifa$overall)    #69
sort(table(fifa$date_of_birth))                   # altester: 22. 10. 1929
sort(table(fifa$date_of_birth), decreasing = T)   # j??ngster:  2. 11. 2000

# Verteilungen innerhalb des Datensatzen:
table(fifa$league)
Farbverktor_Ligen<-c("gold", "black", "black", 
                     "black", "gold", "gold", 
                     "green", "green", "black", 
                     "black", "black", "black", 
                     "blue", "black", "black", 
                     "black", "black", "darkorange",
                     "black", "black", "black",
                     "floralwhite", "black", "red", 
                     "red", "black", "black", 
                     "black", "magenta4", "hotpink",
                     "navy", "black", "black",
                     "blue", "black", "black",
                     "black", "black", "black",
                     "black", "black", "black")

ggplot(mapping = aes(x = fifa$league)) +
  geom_bar(color = "black", fill = Farbverktor_Ligen) +
  theme_minimal()

# Player Verteilung nach Nationalit??t
sort_nationality<-sort(table(fifa$nationality))
barplot(sort_nationality[153:162], col = c("darkorange", "white", "forestgreen", "goldenrod1", "green", "darkslategray1", "hotpink", "red", "gold", "blue"))

# Preis in abh??ngigkeit von Nominalen Variablen ####
# Liga, Klub, Position, Gr????e

# Liga -> Price und Overall
#     mit special
mean(fifa$price_ps4[fifa$league == "Premier League"])     # Preis: 38496.97
mean(fifa$price_ps4[fifa$league == "Bundesliga"])         # Preis: 13043.79
mean(fifa$price_ps4[fifa$league == "LaLiga Santander"])   # Preis: 58235.69
mean(fifa$price_ps4[fifa$league == "Calcio A"])           # Preis: 16019.99
mean(fifa$price_ps4[fifa$league == "Ligue 1 Conforama"])  # Preis: 21086.35

mean(fifa$overall[fifa$league == "Premier League"])     # Preis: 77
mean(fifa$overall[fifa$league == "Bundesliga"])         # Preis: 76
mean(fifa$overall[fifa$league == "LaLiga Santander"])   # Preis: 78
mean(fifa$overall[fifa$league == "Calcio A"])           # Preis: 76
mean(fifa$overall[fifa$league == "Ligue 1 Conforama"])  # Preis: 75

#     ohne special
mean(fifa$price_ps4[fifa$league == "Premier League" & fifa$revision == "Normal" & fifa$origin == "N/A"])     # Preis: 2595.712
mean(fifa$price_ps4[fifa$league == "Bundesliga" & fifa$revision == "Normal" & fifa$origin == "N/A"])         # Preis: 2049.705
mean(fifa$price_ps4[fifa$league == "LaLiga Santander" & fifa$revision == "Normal" & fifa$origin == "N/A"])   # Preis: 4555.01
mean(fifa$price_ps4[fifa$league == "Calcio A" & fifa$revision == "Normal" & fifa$origin == "N/A"])           # Preis: 2368.426
mean(fifa$price_ps4[fifa$league == "Ligue 1 Conforama" & fifa$revision == "Normal" & fifa$origin == "N/A"])  # Preis: 2207.5

mean(fifa$overall[fifa$league == "Premier League" & fifa$revision == "Normal" & fifa$origin == "N/A"])     # Preis: 73.2
mean(fifa$overall[fifa$league == "Bundesliga" & fifa$revision == "Normal" & fifa$origin == "N/A"])         # Preis: 72.8
mean(fifa$overall[fifa$league == "LaLiga Santander" & fifa$revision == "Normal" & fifa$origin == "N/A"])   # Preis: 75.2
mean(fifa$overall[fifa$league == "Calcio A" & fifa$revision == "Normal" & fifa$origin == "N/A"])           # Preis: 73.4
mean(fifa$overall[fifa$league == "Ligue 1 Conforama" & fifa$revision == "Normal" & fifa$origin == "N/A"])  # Preis: 71.6



# Klub ->
sort(table(fifa$club))
mean(fifa$price_ps4[fifa$club == "Manchester City" & fifa$revision == "Normal" & fifa$origin == "N/A"])     # Preis: 5.977.94
mean(fifa$price_ps4[fifa$club == "FC Barcelona" & fifa$revision == "Normal" & fifa$origin == "N/A"])        # Preis: 20065.38
mean(fifa$price_ps4[fifa$club == "Chelsea" & fifa$revision == "Normal" & fifa$origin == "N/A"])             # Preis: 7009.375
mean(fifa$price_ps4[fifa$club == "Paris Saint-Germain" & fifa$revision == "Normal" & fifa$origin == "N/A"]) # Preis: 8837.5
mean(fifa$price_ps4[fifa$club == "Manchester United"  & fifa$revision == "Normal" & fifa$origin == "N/A"])  # Preis: 7179.68
mean(fifa$price_ps4[fifa$club == "Juventus" & fifa$revision == "Normal" & fifa$origin == "N/A"])            # Preis: 7619.231
mean(fifa$price_ps4[fifa$club == "Real Madrid" & fifa$revision == "Normal" & fifa$origin == "N/A"])         # Preis: 33648
mean(fifa$price_ps4[fifa$club == "FC Bayern M??nchen" & fifa$revision == "Normal" & fifa$origin == "N/A"])   # Preis: 9994
mean(fifa$price_ps4[fifa$club == "Lazio" & fifa$revision == "Normal" & fifa$origin == "N/A"])               # Preis: 1695.161
mean(fifa$price_ps4[fifa$club == "Borussia Dortmund" & fifa$revision == "Normal" & fifa$origin == "N/A"])   # Preis: 2993.548
mean(fifa$price_ps4[fifa$club == "AS Monaco Football Club SA" & fifa$revision == "Normal" & fifa$origin == "N/A"]) # Preis: 2195.312

mean(fifa$overall[fifa$club == "Manchester City" & fifa$revision == "Normal" & fifa$origin == "N/A"])     # OV: 73.8
mean(fifa$overall[fifa$club == "FC Barcelona" & fifa$revision == "Normal" & fifa$origin == "N/A"])        # OV: 82.5
mean(fifa$overall[fifa$club == "Chelsea" & fifa$revision == "Normal" & fifa$origin == "N/A"])             # OV: 77.1 
mean(fifa$overall[fifa$club == "Paris Saint-Germain" & fifa$revision == "Normal" & fifa$origin == "N/A"]) # OV: 78.5
mean(fifa$overall[fifa$club == "Manchester United"  & fifa$revision == "Normal" & fifa$origin == "N/A"])  # OV: 78.1
mean(fifa$overall[fifa$club == "Juventus" & fifa$revision == "Normal" & fifa$origin == "N/A"])            # OV: 81.7
mean(fifa$overall[fifa$club == "Real Madrid" & fifa$revision == "Normal" & fifa$origin == "N/A"])         # OV: 81.52
mean(fifa$overall[fifa$club == "FC Bayern M??nchen" & fifa$revision == "Normal" & fifa$origin == "N/A"])   # OV: 80.16
mean(fifa$overall[fifa$club == "Lazio" & fifa$revision == "Normal" & fifa$origin == "N/A"])               # OV: 74
mean(fifa$overall[fifa$club == "Borussia Dortmund" & fifa$revision == "Normal" & fifa$origin == "N/A"])   # OV: 76.12
mean(fifa$overall[fifa$club == "AS Monaco Football Club SA" & fifa$revision == "Normal" & fifa$origin == "N/A"]) # OV: 73.8

# Nach Position
mean(fifa$price_ps4[fifa$position == "ST" & fifa$revision == "Normal" & fifa$origin == "N/A"])     # OV: 1324.76
mean(fifa$price_ps4[fifa$position == "CB" & fifa$revision == "Normal" & fifa$origin == "N/A"])     # OV: 1260.07
mean(fifa$price_ps4[fifa$position == "GK" & fifa$revision == "Normal" & fifa$origin == "N/A"])     # OV: 790.3571
mean(fifa$price_ps4[fifa$position == "CM" & fifa$revision == "Normal" & fifa$origin == "N/A"])     # OV: 1372.29
mean(fifa$price_ps4[fifa$position == "CAM" & fifa$revision == "Normal" & fifa$origin == "N/A"])    # OV: 1430.728


# Regressionen:
# Preis Erkl??ren durch die Wertung 

player_field<-fifa[fifa$position != "GK",]

player_goalkeeper<-fifa[fifa$position == "GK",]
player_defense<-data.frame(rbind(filter(fifa, position == "CB"), 
                                 filter(fifa, position == "RB"), 
                                 filter(fifa, position == "LB"), 
                                 filter(fifa, position == "CDM")))

player_offense<-data.frame(rbind(filter(fifa, position == "CAM"),
                                 filter(fifa, position == "LW"), 
                                 filter(fifa, position == "RW"), 
                                 filter(fifa, position == "CF"), 
                                 filter(fifa, position == "ST")))

player_center<- data.frame(rbind(filter(fifa, position == "CM"),
                                 filter(fifa, position == "RM"), 
                                 filter(fifa, position == "LM")))

# Preis ~ Stats
regr_price_goalkeeper<-lm(data = fifa, price_ps4~gk_diving+gk_handling+gk_speed+gk_kicking+height+gk_reflexes)
summary(regr_price_goalkeeper)

regr_price_defense<-lm(data = player_defense, price_ps4 ~ pace + dribbling + shooting + passing + defending)
summary(regr_price_defense)

regr_price_offense<-lm(data = player_offense, price_ps4 ~ pace + shooting + passing + physicality + dribbling + defending)
summary(regr_price_offense)

regr_price_center<-lm(data = player_center, price_ps4 ~ pace + shooting + passing + defending + physicality + dribbling)
summary(regr_price_center)
  
# Darstellung unterteilt in Position

#   Goalkeeper ####
plot(1, 1, type = "n", ylim=c(0,50000), xlim=c(0,100), main = "Einfluss von Torwartwerten", xlab = "Wert", ylab = "Zuwachs an Marktwert")
abline(0, 521.40, col = "red")
text(10, 45000, c("Diving"), col = "red", cex = 1.2)

abline(0, 271.1, col ="yellow")
text(10, 42000, c("Speed"), col = "yellow", cex = 1.2)

abline(0, 363.36, col="green")
text(10, 39000, c("Kicking"), col = "green", cex = 1.2)

abline(0, 387.55, col="orange")
text(10, 36000, c("Gr????e"), col = "orange", cex = 1.2)

abline(0, 384, col = "blue")
text(10, 33000, c("Handling"), col = "blue", cex = 1.2)

text(10, 30000, c("Reflexes"), col = "purple", cex = 1, font = 3)

#   Defense ####
plot(1, 1, type = "n", ylim=c(0,50000), xlim=c(0,100), main = "Einfluss von Verteidigerwerten", xlab = "Wert", ylab = "Zuwachs an Marktwert")
abline(0, 501.33, col = "red")
text(10, 40000, c("Pace"), col = "red", cex = 1.2)

abline(60000, -579.81, col ="yellow")
text(10, 37000, c("Dribbling"), col = "yellow", cex = 1.2)

abline(0, 216.6, col = "green")
text(10, 34000, c("Shooting"), col = "green", cex = 1.2)

abline(0, 374.41, col = "orange")
text(10, 31000, c("Passing"), col = "orange", cex = 1.2)

abline(0, 1292.83, col = "blue")
text(10, 28000, c("Defending"), col = "blue", cex = 1.2)

text(10, 25000, c("Physicality"), col = "purple", cex = 1, font = 3)

#   Center ####
plot(1, 1, type = "n", ylim=c(0,50000), xlim=c(0,100), main = "Einfluss von Mittelfeldwerten", xlab = "Wert", ylab = "Zuwachs an Marktwert")
abline(0, 715.46, col = "red")
text(10, 40000, c("Pace"), col = "red", cex = 1.2)

#nicht signifikant
text(10, 37000, c("Dribbling"), col = "yellow", cex = 1, font = 3)

abline(0, 392.68, col = "green")
text(10, 34000, c("Shooting"), col = "green", cex = 1.2)

abline(0, 1008, col = "orange")
text(10, 31000, c("Passing"), col = "orange", cex = 1.2)

abline(0, 309.68, col = "blue")
text(10, 28000, c("Defending"), col = "blue", cex = 1.2)

abline(0, 369.16, col = "purple")
text(10, 25000, c("Physicality"), col = "purple", cex = 1.2)


#   Offense ####
plot(1, 1, type = "n", ylim=c(0,50000), xlim=c(0,100), main = "Einfluss von Offensivwerten", xlab = "Wert", ylab = "Zuwachs an Marktwert")

abline(0, 1874.9, col = "red")
text(90, 40000, c("Pace"), col = "red", cex = 1.2)

#nicht signifikant
text(90, 37000, c("Dribbling"), col = "yellow", cex = 1, font = 3)

abline(0, 3109.2, col = "green")
text(90, 34000, c("Shooting"), col = "green", cex = 1.2)

abline(0, 1594.7, col = "orange")
text(90, 31000, c("Passing"), col = "orange", cex = 1.2)

#abline(0, 309.68, col = "blue")
text(90, 28000, c("Defending"), col = "blue", cex = 1, font = 3)

abline(0, 516.5, col = "purple")
text(90, 25000, c("Physicality"), col = "purple", cex = 1.2)



# Ausrei??er von Overall~Preis Darstellung ####
#    Goalkeeper   ####
player_goalkeeper$sum_overall<-player_goalkeeper$gk_diving + player_goalkeeper$gk_handling + player_goalkeeper$gk_kicking + player_goalkeeper$gk_positoning + player_goalkeeper$gk_speed + player_goalkeeper$gk_reflexes

regr_gk<-lm(price_ps4~sum_overall+I(sum_overall^2)+I(sum_overall^3)+I(sum_overall^4)+I(sum_overall^5)+I(sum_overall^6)+I(sum_overall^7)+I(sum_overall^8), data = player_goalkeeper)
summary(regr_gk)
predict_gk<-sort(unique(predict(regr_gk, newdata = player_goalkeeper)))

plot(jitter(player_goalkeeper$sum_overall), player_goalkeeper$price_ps4, pch = 16, col = "purple", xlim = c(450, 570),
     main = "Torh??ter", ylab = "PS4 Preis", xlab = "Overall Wert")
points(sort(unique(player_goalkeeper$sum_overall)), predict_gk, type = "l")
 
#    Defense      ####
player_defense$sum_overall<-player_defense$shooting + player_defense$passing + player_defense$physicality + player_defense$defending + player_defense$pace + player_defense$dribbling
regr_defense<-lm(price_ps4~sum_overall+I(sum_overall^2)+I(sum_overall^3)+I(sum_overall^4)+I(sum_overall^5)+I(sum_overall^6), 
                 data = player_defense)
summary(regr_defense)
predict_defense<-sort(unique(predict(regr_defense, newdata = player_defense)))

plot(jitter(player_defense$sum_overall), player_defense$price_ps4, pch = 16, col = "blue", xlim = c(200, 550),
     main = "Defensivspieler", ylab = "PS4 Preis", xlab = "Overall Wert")
points(sort(unique(player_defense$sum_overall)), predict_defense, type = "l", col="red")

#    Offense      ####
player_offense$sum_overall<-player_offense$shooting + player_offense$passing + player_offense$physicality + player_offense$defending + player_offense$pace + player_offense$dribbling

regr_offense<-lm(price_ps4~sum_overall+I(sum_overall^2)+I(sum_overall^3)+I(sum_overall^4)+I(sum_overall^5)+I(sum_overall^6), 
                 data = player_offense)
summary(regr_offense)
predict_offense<-sort(unique(predict(regr_offense, newdata = player_offense)))

plot(jitter(player_offense$sum_overall), player_offense$price_ps4, pch = 16, col = "red", xlim = c(450, 570),
     main = "Offensivspieler", ylab = "PS4 Preis", xlab = "Overall Wert")
points(sort(unique(player_offense$sum_overall)), predict_offense, type = "l", col="blue")

#    center       ####
player_center$sum_overall<-player_center$shooting + player_center$passing + player_center$physicality + player_center$defending + player_center$pace + player_center$dribbling
regr_center<-lm(price_ps4~sum_overall+I(sum_overall^2)+I(sum_overall^3)+I(sum_overall^4)+I(sum_overall^5), 
                 data = player_center)
summary(regr_center)
predict_center<-sort(unique(predict(regr_center, newdata = player_center)))

plot(player_center$sum_overall, player_center$price_ps4, pch = 16, col = "green", 
     main = "Mittelfeldspieler", ylab = "PS4 Preis", xlab = "Overall Wert", xlim=c(450, 570))
points(sort(unique(player_center$sum_overall)), predict_center, type = "l")


# Residuals Ausrei??eranalyse ####
#   Center gr????ter Ausrei??er
which.max(regr_center$residuals[player_center$overall >= "85"])     # Patrick Vieria
which.min(regr_center$residuals[player_center$overall >= "90"])     # Milinkovic-Savic
which.min(regr_center$residuals[player_center$overall >= "85"])     # Milinkovic-Savic

#   Offense gr????ter Ausrei??er
which.max(regr_offense$residuals)                                   # Ronaldo Icon
which.min(regr_offense$residuals[player_offense$overall >= "90"])   # Mohamed Salah

#   Defense gr????ter Ausrei??er
which.max(regr_defense$residuals)                                   # Maldini
which.min(regr_defense$residuals[player_defense$overall >= "90"])   # Mohamed Salah

#   Goalkeeper gr????ter Ausrei??er
which.max(regr_gk$residuals)                                        # Yashin
which.min(regr_gk$residuals[player_goalkeeper$overall >= "90"])     # Lecomte


# Prediction von selbsgew??hlten Karten ####
kroos_krass<- -171045.65 + 727.61 * 90 + 402.03*95 + 1037.47 * 99 + 308.92 * 90 + 365.58*90 + -49.11*95 
kroos_normal<- -171045.65 + 727.61 * 67 + 402.03*92 + 1037.47 * 99 + 308.92 * 85 + 365.58*80 + -49.11*95

tobi_d<-     -456772.26 + 1754.52 * 39 + 2917.29 * 70 + 1288.53 * 49 + 552.95 * 99 + 602.79 * 44 + 59.35 * 35
sasa_p<-     -456772.26 + 1754.52 * 76 + 2917.29 * 71 + 1288.53 * 51 + 552.95 * 84 + 602.79 * 66 + 59.35 * 70
mitt_weida<- -456772.26 + 1754.52 * 20 + 2917.29 * 80 + 1288.53 * 70 + 552.95 * 69 + 602.79 * 63 + 59.35 * 12

