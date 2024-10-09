# Preditores-para-malignidade-positiva-em-tumores-de-mama

# Carregar pacotes necessários
library(tidyverse)
library(caret)

# Carregar os dados (substitua 'seu_arquivo.csv' pelo nome do seu arquivo)
dados <- read.csv("seu_arquivo.csv")

# Visualizar os primeiros registros
head(dados)

# Análise exploratória
summary(dados)
ggplot(dados, aes(x = idade, fill = cancer)) + 
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Distribuição de Idade por Status de Câncer", x = "Idade", y = "Contagem")

# Preparação dos dados
dados <- dados %>%
  mutate(cancer = as.factor(cancer))

# Divisão dos dados em treino e teste
set.seed(123)
index <- createDataPartition(dados$cancer, p = 0.7, list = FALSE)
treino <- dados[index, ]
teste <- dados[-index, ]

# Treinamento do modelo de regressão logística
modelo <- glm(cancer ~ idade + fator_risco1 + fator_risco2, data = treino, family = binomial)

# Resumo do modelo
summary(modelo)

# Previsões no conjunto de teste
previsoes <- predict(modelo, newdata = teste, type = "response")
previsoes_bin <- ifelse(previsoes > 0.5, 1, 0)

# Avaliação do modelo
confusionMatrix(as.factor(previsoes_bin), teste$cancer)
