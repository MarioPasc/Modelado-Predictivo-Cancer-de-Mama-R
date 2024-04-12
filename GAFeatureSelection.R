library(GA)

calculate_fitness_value <- function(y_true, y_pred, threshold = 0.35) {
  y_pred <- ifelse(y_pred < 0.35, 0, 1)
  TP <- sum(y_pred == 1 & y_true == 1)
  TN <- sum(y_pred == 0 & y_true == 0)
  FP <- sum(y_pred == 1 & y_true == 0)
  FN <- sum(y_pred == 0 & y_true == 1)
  
  accuracy = (TP + TN) / (TP + TN + FP + FN)
  precision = ifelse(TP + FP == 0, 0, TP / (TP + FP))
  recall = ifelse(TP + FN == 0, 0, TP / (TP + FN))
  f1_score = ifelse(precision + recall == 0, 0, (2 * precision * recall) / (precision + recall))
  
  return(0.5 * accuracy + 0.5 * f1_score)
}

fitness_function <- function(binary_vector, df, target_name, train_indices, test_indices, threshold = 0.35) {
  binary_vector <- binary_vector[1:(ncol(df)-1)]  # Asegura la longitud correcta del vector binario
  variables <- names(df)[-which(names(df) == target_name)][binary_vector == 1]
  
  if (length(variables) <= 1) return(0)  # Evita modelos sin variables
  
  formula_glm <- as.formula(paste(target_name, "~", paste(variables, collapse = "+")))
  train_set <- df[train_indices, c(target_name, variables), drop = FALSE]
  test_set <- df[test_indices, c(target_name, variables), drop = FALSE]
  model <- glm(formula_glm, data = train_set, family = binomial())
  
  predicciones <- predict(model, newdata = test_set, type = "response")
  fitness_value <- calculate_fitness_value(test_set[[target_name]], predicciones, threshold)
  return(fitness_value)
}


ga_featureSelection <- function(df, target_name, train_indices, test_indices, nBits, threshold = 0.35) {
  accuracy_history <- data.frame(iteration = numeric(), mean_accuracy = numeric(), max_accuracy = numeric())
  # Inicializar feature_selection_frequency con un número específico de columnas y filas
  feature_selection_frequency <- matrix(0, nrow = nBits, ncol = 1)
  
  ga_results <- ga(type = "binary", 
                   fitness = function(chromosome) fitness_function(chromosome, df, target_name, train_indices, test_indices, threshold),
                   nBits = nBits, popSize = 25, maxiter = 100,
                   monitor = function(obj) {
                     accuracy_history <<- rbind(accuracy_history, data.frame(iteration = obj@iter,
                                                                             mean_accuracy = mean(obj@fitnessValue),
                                                                             max_accuracy = max(obj@fitnessValue)))
                     # Asegurarse de que el número de columnas sea consistente
                     current_frequency <- colSums(matrix(obj@population, nrow = nBits, ncol = obj@popSize))
                     if (ncol(feature_selection_frequency) == 0) {
                       feature_selection_frequency <<- current_frequency
                     } else {
                       feature_selection_frequency <<- cbind(feature_selection_frequency, current_frequency)
                     }
                   })
  
  return(ga_results)
}


