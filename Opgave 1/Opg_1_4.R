library(pROC)
library(caret)
#OPG 1.4 - 3 klassifikationsmodeller

#### 1. Multiple Logistisk Regression ####
glmdata <- randomforest_træning[, c("goal",
                                    "SHOTBODYPART", 
                                    "meters_to_goal", 
                                    "shot_angle_geom",
                                    "POSSESSIONTYPE1")]

#### 1.1 TRÆNING GLM ####
glm <- glm(goal ~ .,
           data = glmdata, 
           family = binomial())

summary(glm)

#### 1.2 GLM PREDICT ####
#Forudsiger sandsynligheden for, at et skud bliver til mål på testdata
testdata$predicted_prob <- predict(glm, newdata = testdata, type = "response")

#### 1.3 VALIDERING ####
#Test af optimal grænseværdi- CT og youden
roc_kurve <- roc(testdata$goal, testdata$predicted_prob)
ct_grænseværdi <- coords(roc_kurve, "best", ret = "closest.topleft")
print(ct_grænseværdi) #0.1291991

youden_grænseværdi <- coords(roc_kurve, "best", best.method="youden")
print(youden_grænseværdi) #0.1619234

##0.1291991 er den optimale grænseværdi ud fra cloests topleft

#plot af roc kurve
plot(roc_kurve, col = "blue", lwd = 2, main = "ROC-kurve for modelens præstation på testdata")
auc_value <- auc(roc_kurve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

#Visualisering af optimal grænseværdi på roc kurve
optimal_threshold <- 0.129
optimal_coords <- coords(roc_kurve, "best", ret = c("specificity", "sensitivity"), best.method="closest.topleft")
points(1 - optimal_coords[1], optimal_coords[2], col = "red", pch = 19)
legend("bottomleft", legend = paste("Optimal grænseværdi =", round(optimal_threshold, 3)), col = "red", pch = 19)

#Konfusionsmatrix 
#laver faktor ud fra grænseværdi på 0.129
testdata$predicted_goal <- ifelse(testdata$predicted_prob > 0.129, 1, 0)
testdata$predicted_goal <- as.factor(testdata$predicted_goal)
testdata$goal <- as.factor(testdata$goal) #faktor så konf matric virker

conf_matrix <- confusionMatrix(testdata$predicted_goal,testdata$goal, positive = "1")
print(conf_matrix)

#### 1.3 WYSCOUT SAMMENLIGNING #### 
testdata$xg_predicted <- ifelse(testdata$SHOTXG > 0.129, 1, 0)
testdata$xg_predicted <- as.factor(testdata$xg_predicted)

conf_matrix2 <- confusionMatrix(testdata$xg_predicted, testdata$goal)
print(conf_matrix2)


#### 2. Beslutningstræ ####
library(rpart)
library(rpart.plot)
library(pROC)

#### 2.1 TRÆNING BESLUTNINGSTRÆ ####
# Træn modellen på træningsdata med balanced class weights og cp = 0.005
dt_model <- rpart(
  goal ~ SHOTBODYPART + meters_to_goal + shot_angle_geom + POSSESSIONTYPE1,
  data = randomforest_træning,
  method = "class",
  control = rpart.control(cp = 0.005),
  parms = list(prior = c(false = 0.5, true = 0.5))  # Balancer mål og ikke-mål
)

# Visualiser det beskårne træ
rpart.plot(
  dt_model,
  main = "Beslutningstræ for forudsigelse af målchancer",  # ← din nye overskrift
  extra = 106,
  box.palette = "Greens"
)

#### 2.2 BESLUTNINGSTRÆ PREDICT ####
# Forudsig sandsynligheder for testdata
testdata$pred_prob <- predict(dt_model, newdata = testdata, type = "prob")[, 2]

#### 2.3 VALIDERING ####
# Beregn ROC-kurve
roc_curve <- roc(testdata$SHOTISGOAL, testdata$pred_prob)

# Find optimal grænseværdi med Youden's J
grænseværdi <- coords(roc_curve, "best", best.method = "youden")
print(grænseværdi)
#threshold 
# 0.3722785

#### Klassificér med threshold
testdata$predictedGoal <- ifelse(testdata$pred_prob > 0.3722, 1, 0)

# Konfusionsmatrix
conf_mat <- table(Predicted = testdata$predictedGoal, Actual = testdata$SHOTISGOAL)
print(conf_mat)
#Predicted false true
#   0       823   48
#   1       285   98

# Accuracy
accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
print(paste("Accuracy:", round(accuracy, 4)))
#Accuracy: 0.7344

# Udtræk værdier fra konfusionsmatrix
TN <- conf_mat[1,1]
FP <- conf_mat[2,1]
FN <- conf_mat[1,2]
TP <- conf_mat[2,2]

# Precision
precision <- TP / (TP + FP)
print(paste("Precision:", round(precision, 4)))
#Precision: 0.2559"

# Recall (sensitivitet)
recall <- TP / (TP + FN)
print(paste("Recall:", round(recall, 4)))
#"Recall: 0.6712"

#AUC
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 4)))
# AUC: 0.7715

#ROC-kurve
roc_curve <- roc(testdata$SHOTISGOAL, testdata$pred_prob)

# Plot med AUC som tekstboks (uden "1 - Specificity")
plot(
  roc_curve,
  col = "blue",
  main = "ROC-kurve for beslutningstræets præstation på testdata"
)

# Tilføj AUC som tekstboks (legend)
legend("bottomright",
       legend = paste("AUC =", round(auc(roc_curve), 3)),
       col = "blue",
       lwd = 2,
       box.lty = 1
)



#### 3. Multilayer Perceptron ####
library(fastDummies)
library(neuralnet)

#### 3.1 MLP TEST OG TRÆNING ####
perceptron_data2 <- træningsdata[, c("goal",
                                     "SHOTBODYPART", 
                                     "meters_to_goal", 
                                     "shot_angle_geom",
                                     "POSSESSIONTYPE1")]

test_perceptron_data2 <- testdata[, c("goal",
                                      "SHOTBODYPART", 
                                      "meters_to_goal", 
                                      "shot_angle_geom",
                                      "POSSESSIONTYPE1")]

#### 3.2 MLP DATA FORBEREDELSE ####
# Fjern alle NA-værdier i hele træningsdata
perceptron_data2 <- na.omit(perceptron_data2)
test_perceptron_data2 <- na.omit(test_perceptron_data2)

# One-hot encoding af kategoriske kolonner
perceptron_data2 <- dummy_cols(perceptron_data2, 
                               select_columns = c("SHOTBODYPART", "POSSESSIONTYPE1"), 
                               remove_first_dummy = TRUE,  # Undgå dummy trap (dropper én kategori)
                               remove_selected_columns = TRUE)  # Fjerner original variabel


test_perceptron_data2 <- dummy_cols(test_perceptron_data2, 
                                    select_columns = c("SHOTBODYPART", "POSSESSIONTYPE1"), 
                                    remove_first_dummy = TRUE, 
                                    remove_selected_columns = TRUE)


#skalering
perceptron_data2$meters_to_goal <- scale(perceptron_data2$meters_to_goal)
perceptron_data2$shot_angle_geom <- scale(perceptron_data2$shot_angle_geom)

test_perceptron_data2$meters_to_goal <- scale(test_perceptron_data2$meters_to_goal)
test_perceptron_data2$shot_angle_geom <- scale(test_perceptron_data2$shot_angle_geom)

#numeriske værdier og goal som faktor
perceptron_data2 <- data.frame(lapply(perceptron_data2, as.numeric))
perceptron_data2$goal <- as.factor(perceptron_data2$goal)

test_perceptron_data2 <- data.frame(lapply(test_perceptron_data2, as.numeric))
test_perceptron_data2$goal <- as.factor(test_perceptron_data2$goal)

# ! kun nedenstående hvis goal er lavet om til 1,2 i test_perceotron data
test_perceptron_data2$goal <- ifelse(test_perceptron_data2$goal == 2, 1, 0)

#### 3. TRÆNING AF MLP MODEL ####
formula <- goal ~.

#Træner model på træningsdata med 4 neuroner i 1 skjult lag
model <- neuralnet(goal ~., data = perceptron_data2, hidden = c(4), stepmax = 1e6, linear.output = FALSE)

model$weights
plot(model)

#### 3.4 MLP VALIDERING ####
#Forudsig sandsynlighederne for testdata
test_perceptron_data2$predictions <- predict(model, test_perceptron_data2, type = "response")[,2] #gemmer kun værdien for 1
test_perceptron_data2$predictions <- as.vector(test_perceptron_data2$predictions) #skal være vektor format

#ROC-kurve
roc_curve <- roc(test_perceptron_data2$goal,test_perceptron_data2$predictions)

plot(roc_curve, col = "blue", lwd = 2, main = "ROC-kurve for MLP modelens præstation på testdata")
auc_value <- auc(roc_curve)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3)), col = "blue", lwd = 2)

optimal_threshold <- coords(roc_curve, "best", ret = "threshold")
print(optimal_threshold) #0.1574294

#Binær variabel ud fra modellens sandsynligheder og optimal grænseværdi
test_perceptron_data2$predicted_classes <- ifelse(test_perceptron_data2$predictions > 0.157, 1, 0)

#Konf matrix
conf_matrix <- table(Predicted = test_perceptron_data2$predicted_classes, Actual = test_perceptron_data2$goal)
print(conf_matrix)

#præcision og nøjagtighed
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2,2] / sum(conf_matrix[2,])
recall <- conf_matrix[2,2] / sum(conf_matrix[,2])

cat("Accuracy:", accuracy, "\n") #0.7695375
cat("Precision:", precision, "\n") #0.255814
cat("Recall:", recall, "\n") #0.5422535

#### 3.5. MLP VISUALISERING ####
#Heatmap af vægte 
library(ggplot2)
library(reshape2)

#Matrix med vægtene
weights <- matrix(c(
  0.5196, -108.5014, -54.1020, -20.7262,
  -0.2045, -67.3016, -11.7573, -13.7029,
  -0.6155, -68.7297, -19.6103, -18.5581,
  -0.7123, -53.5397, -4.5777, -4.2483,
  -0.4365, 84.5149, -103.5831, 12.8817,
  -0.3842, -12.9126, -20.5932, -5.6742,
  0.1271, -4.3513, -27.5153, 3.8829
), nrow = 7, byrow = TRUE)

#Navngiver rækker og kolonner
rownames(weights) <- c("meters_to_goal", "shot_angle_geom", "SHOTBODYPART_left_foot", 
                       "SHOTBODYPART_right_foot", "POSSESSIONTYPE1_corner", 
                       "POSSESSIONTYPE1_counterattack", "POSSESSIONTYPE1_set_piece_attack")
colnames(weights) <- c("Neuron 1", "Neuron 2", "Neuron 3", "Neuron 4")

#Vægte til data frame til plot
weights_df <- melt(weights) #langt format
colnames(weights_df) <- c("Input Variable", "Neuron", "Weight")
weights_df$Weight <- round(weights_df$Weight, 3)  # Rund værdier til 3 decimaler

#Heatmap af vægte
heatmap_plot <- ggplot(weights_df, aes(x = Neuron, y = `Input Variable`, fill = Weight)) +
  geom_tile(color = "black") + # Tilføj kant for bedre synlighed
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  geom_text(aes(label = Weight), color = "black", size = 4) + # Tilføj værdier
  theme_minimal() +
  labs(title = "Heatmap af MLP-vægte", fill = "Vægt")

print(heatmap_plot)

#Plot af netværk
#install.packages("NeuralNetTools")
library(NeuralNetTools)

# Definer variabelnavne
mine_labels <- c("Meters to Goal", "Shot Angle", "Left Foot", "Right Foot", 
                 "Corner", "Counterattack", "Set Piece Attack")

# Plot netværket med egne labels
plotnet(model, 
        node.size = 12,            
        font.size = 14,            
        col.input = "lightgreen",  
        col.hidden = "lightblue", 
        col.output = "lightcoral", 
        edge.arrow.size = 0.7,     
        display.weights = TRUE,    
        display.bias = TRUE,      
        label = TRUE,            
        var.labs = c("Meters to Goal", "Shot Angle", "Left Foot", "Right Foot", 
                     "Corner", "Counterattack", "Set Piece Attack")  
)




