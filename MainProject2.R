setwd("D:/Projetos/BigDataAnalyticsComReMAzureLearning/Cap21/Projeto2")
getwd()

install.packages("readxl")
install.packages("dplyr")
install.packages("sqldf")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(sqldf)
library(ggplot2)

# Pacotes para visualizar a análise de correlação
install.packages('corrgram')
install.packages('corrplot')
library(corrplot)
library(corrgram)


# carregar excel:
path_file_excel = "Acoustic_Extinguisher_Fire_Dataset/Acoustic_Extinguisher_Fire_Dataset.xlsx"

worksheets = excel_sheets(path_file_excel)
worksheets

df_extinguisher = read_excel(path_file_excel)
View(df_extinguisher)
#



# verificar valores nulos:
sum(is.na(df_extinguisher))
colSums(is.na(df_extinguisher))
#



## verificar tipos categóricos e numéricos e fazer os seus tratamentos:
str(df_extinguisher)

# modificar a coluna FUEL para numérica:
unique(df_extinguisher$FUEL)
df_extinguisher$FUEL[which(df_extinguisher$FUEL == "gasoline")] <- 1
df_extinguisher$FUEL[which(df_extinguisher$FUEL == "thinner")] <- 2
df_extinguisher$FUEL[which(df_extinguisher$FUEL == "kerosene")] <- 3
df_extinguisher$FUEL[which(df_extinguisher$FUEL == "lpg")] <- 4

str(df_extinguisher)

fuel_colum_as_numeric <- as.numeric(df_extinguisher$FUEL)
df_extinguisher$FUEL <- fuel_colum_as_numeric
str(df_extinguisher)
View(df_extinguisher)
#
##



## verificação valores únicos
unique(df_extinguisher)

#SIZE:
unique(df_extinguisher$SIZE)
#

#FUEL:
unique(df_extinguisher$FUEL)
#

# DISTANCE:
unique(df_extinguisher$DISTANCE)
#

# DESIBEL:
unique(df_extinguisher$DESIBEL)
#

# AIRFLOW:
unique(df_extinguisher$AIRFLOW)
#

# FREQUENCY
unique(df_extinguisher$FREQUENCY)
#

##


# alterar linhas de forma randomica para tirar a ordenação das colunas SIZE e FUEL:
df_extinguisher <- df_extinguisher[sample(1:nrow(df_extinguisher)), ]
View(df_extinguisher)
#





### VERIFICAÇÃO DAS CORRELAÇÕES E RELEVANCIA PARA AS COLUNAS EM RELAÇÃO AO STATUS DE FOGO EXTINTO OU NÃO:


str(df_extinguisher)

# correlação entre o tamanho do extintor com o status do fogo apagado ou não - SIZE:
correlation_status_and_size = cor(df_extinguisher$STATUS, df_extinguisher$SIZE)
correlation_status_and_size # correlação negativa próxima de 0 : -0.0969

regression_status_and_size <- lm(df_extinguisher$STATUS ~ df_extinguisher$SIZE)
summary(regression_status_and_size) # p-valor: 2e-16 ***

plot_x_y <- plot(
  x=df_extinguisher$SIZE, main = "SIZE x STATUS",
  y=df_extinguisher$STATUS,
  xlab="SIZE", ylab = "STATUS", pch=1,
)
grid(plot_x_y)
abline(regression_status_and_size)


# correlação entre a tipo de combustível usado e o status do fogo apagado ou não - FUEL:
correlation_status_and_FUEL = cor(df_extinguisher$STATUS, df_extinguisher$FUEL)
correlation_status_and_FUEL # correlação negativa próxima de 0 : -0.02278606

regression_status_and_FUEL <- lm(df_extinguisher$STATUS ~ df_extinguisher$FUEL)
summary(regression_status_and_FUEL) # p-valor: 0.00262 **

plot_x_y <- plot(
  x=df_extinguisher$FUEL, main = "FUEL x STATUS",
  y=df_extinguisher$STATUS,
  xlab="FUEL", ylab = "STATUS", pch=1,
)
grid(plot_x_y)
abline(regression_status_and_FUEL)


# correlação da distancia do extintor para o fogo com o status do fogo apagado - DISTANCE:
correlation_status_and_DISTANCE = cor(df_extinguisher$STATUS, df_extinguisher$DISTANCE)
correlation_status_and_DISTANCE # correlação negativa muito próxima de 0 : -0.6440506

regression_status_and_DISTANCE <- lm(df_extinguisher$STATUS ~ df_extinguisher$DISTANCE)
summary(regression_status_and_DISTANCE) # p-valor: 2e-16 ***

plot_x_y <- plot(
  x=df_extinguisher$DISTANCE, main = "DISTANCE x STATUS",
  y=df_extinguisher$STATUS,
  xlab="DISTANCE", ylab = "STATUS", pch=1,
)
grid(plot_x_y)
abline(regression_status_and_DISTANCE)


# correlação do nível de decibeis do extintor com o status do fogo apagado - DESIBEL:
correlation_status_and_DESIBEL = cor(df_extinguisher$STATUS, df_extinguisher$DESIBEL)
correlation_status_and_DESIBEL # correlação próxima de 0 : 0.2039698

regression_status_and_DESIBEL <- lm(df_extinguisher$STATUS ~ df_extinguisher$DESIBEL)
summary(regression_status_and_DESIBEL) # p-valor: 2e-16 ***

plot_x_y <- plot(
  x=df_extinguisher$DESIBEL, main = "DESIBEL x STATUS",
  y=df_extinguisher$STATUS,
  xlab="DESIBEL", ylab = "STATUS", pch=1,
)
grid(plot_x_y)
abline(regression_status_and_DESIBEL)


# correlação do fluxo do ar com o status do fogo apagado - AIRFLOW:
correlation_status_and_AIRFLOW = cor(df_extinguisher$STATUS, df_extinguisher$AIRFLOW)
correlation_status_and_AIRFLOW # correlação próxima de 0,8 : 0.7606279

regression_status_and_AIRFLOW <- lm(df_extinguisher$STATUS ~ df_extinguisher$AIRFLOW)
summary(regression_status_and_AIRFLOW) # p-valor: 2e-16 ***

plot_x_y <- plot(
  x=df_extinguisher$AIRFLOW, main = "AIRFLOW x STATUS",
  y=df_extinguisher$STATUS,
  xlab="AIRFLOW", ylab = "STATUS", pch=1,
)
grid(plot_x_y)
abline(regression_status_and_AIRFLOW)


# correlação entre a frequencia do som do extintor com o status do fogo apagado - FREQUENCY:
correlation_status_and_FREQUENCY = cor(df_extinguisher$STATUS, df_extinguisher$FREQUENCY)
correlation_status_and_FREQUENCY # correlação negativa próxima de 0 : -0.244203

regression_status_and_FREQUENCY <- lm(df_extinguisher$STATUS ~ df_extinguisher$FREQUENCY)
summary(regression_status_and_FREQUENCY) # p-valor: 2e-16 ***

plot_x_y <- plot(
  x=df_extinguisher$FREQUENCY, main = "FREQUENCY x STATUS",
  y=df_extinguisher$STATUS,
  xlab="FREQUENCY", ylab = "STATUS", pch=1,
)
grid(plot_x_y)
abline(regression_status_and_FREQUENCY)


# Matriz de correlação entre as colunas:
corr_simple <- function(data=df,sig=0.1){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}

corr_simple(df_extinguisher)


##

### seleção dos campos para o algoritmo de previsão:
# resolvi remover o DISTANCE, pois apesar de ter uma relação ruim com o STATUS, também tem uma relação moderada ou ruim com o restante dos outros campos.

str(df_extinguisher)
# campos ideais: SIZE, FUEL, DESIBEL, AIRFLOW, FREQUENCY. 
#                (esses campos podem ter uma relação ruim com o status, exceto o AIRFLOW. Mas os mesmos tem uma boa relação entre si e com o AIRFLOW que tem uma boa relação com o STAtUS)

##



## Divisão do DataSet para Treino e Teste:

# make reproducible
set.seed(1)

# create ID column
df_extinguisher$id <- 1:nrow(df_extinguisher)
View(df_extinguisher)


# 80% para treino:
train_df_extinguisher <- df_extinguisher %>% dplyr::sample_frac(0.80)
View(train_df_extinguisher)

# 20% para teste:
test_df_extinguisher <- dplyr::anti_join(df_extinguisher, train_df_extinguisher, by = "id") 
View(test_df_extinguisher)
##



## Algoritmo de Regressão Linear
model_learn_extnguisher <- lm(STATUS ~ SIZE + FUEL + DESIBEL + AIRFLOW + FREQUENCY, data = train_df_extinguisher)
summary(model_learn_extnguisher)
#




## Interpretação/Explicação do modelo:

# Obter os resíduos:
residuals <- resid(model_learn_extnguisher)
residuals
#

# Gráfico de Resíduos vs Valores Ajustados
ggplot(train_df_extinguisher, aes(x = predict(model_learn_extnguisher), y = residuals)) +
  geom_point() +
  geom_smooth(se = FALSE, method = 'loess') +
  ggtitle("Resíduos x Valores Ajustados") +
  xlab("Valores Ajustados") +
  ylab("Resíduos") # Não estão bem espalhados, porque o STATUS é somente 0 ou 1. Isso não parece ser um bom sinal.

# Histograma dos resíduos:
ggplot(train_df_extinguisher, aes(x = residuals)) +
  geom_histogram(binwidth = 1, fill = 'blue', alpha = 0.7) +
  ggtitle("Histograma dos Resíduos") +
  xlab("Resíduos") # Para o Status 0 ou 1, podemos dizer que está parecido com uma distribuição normal, porém muito concentrado nos resultados 0. Informação importante para poder melhorar o DataSet.

# QQ-Plot
ggplot(train_df_extinguisher, aes(sample = residuals)) +
  geom_qq() +
  geom_qq_line() +
  ggtitle("QQ-Plot dos Resíduos") +
  xlab("Quantis Teóricos") +
  ylab("Quantis Amostrais") # muitos pontos muito próximos da linha diagonal, mas tem uma certa quantidade um pouco longe, o que mostra que podemos diminuir a taxa de erros.


# Salvar modelo localmente:
save(model_learn_extnguisher, file = "model_efficiency_extinguisher.RDATA")


# carregar modelo e testar com dataset de teste com 20% dos dados:
?load
model_loaded = load("model_efficiency_extinguisher.RDATA.RDATA")
summary(model_loaded)
View(test_df_cars)


predictions = predict(model_learn_extnguisher, newdata = test_df_extinguisher)

cat("Esperamos esses STATUS previstos: ", as.integer(predictions))




## Contagem de acertos de acertos:
test_df_extinguisher$STATUS
predictions

# colocar limiar threshold para converter para exatamente inteiros conforme a coluna STATUS

limiar = 0.5

previsoes_inteiras <- ifelse(predictions > limiar, 1, 0)
predictions <- previsoes_inteiras


test_df_extinguisher$STATUS
predictions

# fazer contagem de acertos:
count_acertos = 0
for (index in 1:length(predictions)) {
  value_test = test_df_extinguisher$STATUS[index]
  value_prediction = predictions[index]
  print(value_test)
  print(value_prediction)
  acerto <- ifelse(value_test == value_prediction, 1, 0)
  count_acertos = count_acertos + acerto
}

cat("Total de acerto: ", as.integer(count_acertos))
percent = (count_acertos / length(predictions)) * 100
cat("Porcentagem de acertos no DataSet de teste: ", as.character(percent))






## Testando algoritmo Random Forest para comprar os resultados:


library(randomForest)
model_learn_extnguisher_random_forest <- randomForest(STATUS ~ SIZE + FUEL + DESIBEL + AIRFLOW + FREQUENCY, data = train_df_extinguisher)
summary(model_learn_extnguisher_random_forest)


# Salvar modelo localmente:
save(model_learn_extnguisher_random_forest, file = "model_efficiency_extinguisher_random_forest.RDATA")


# carregar modelo e testar com dataset de teste com 20% dos dados:
?load
model_loaded = load("model_efficiency_extinguisher_random_forest.RDATA.RDATA")
summary(model_loaded)
View(test_df_cars)


predictions = predict(model_learn_extnguisher, newdata = test_df_extinguisher)

cat("Esperamos esses STATUS previstos: ", as.integer(predictions))




## Contagem de acertos de acertos:
test_df_extinguisher$STATUS
predictions

# colocar limiar threshold para converter para exatamente inteiros conforme a coluna STATUS

limiar = 0.5

previsoes_inteiras <- ifelse(predictions > limiar, 1, 0)
predictions <- previsoes_inteiras


test_df_extinguisher$STATUS
predictions

# fazer contagem de acertos:
count_acertos = 0
for (index in 1:length(predictions)) {
  value_test = test_df_extinguisher$STATUS[index]
  value_prediction = predictions[index]
  print(value_test)
  print(value_prediction)
  acerto <- ifelse(value_test == value_prediction, 1, 0)
  count_acertos = count_acertos + acerto
}

cat("Total de acerto: ", as.integer(count_acertos))
percent = (count_acertos / length(predictions)) * 100
cat("Porcentagem de acertos no DataSet de teste: ", as.character(percent))

# no caso do Random Forest quase fechou os 86%
