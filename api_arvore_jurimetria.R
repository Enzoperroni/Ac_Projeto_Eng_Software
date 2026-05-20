# =========================
# PACOTES
# =========================

if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(caret)) install.packages("caret")
if (!require(plumber)) install.packages("plumber")

library(rpart)
library(rpart.plot)
library(caret)
library(plumber)

# =========================
# CARREGAR CSV
# =========================

dados <- read.csv(
  "C:/Users/zocap/Downloads/dados_jurimetria(1).csv",
  stringsAsFactors = TRUE,
  fileEncoding = "UTF-8"
)

# =========================
# RENOMEAR COLUNAS
# =========================

names(dados) <- c(
  "area",
  "valor_acao",
  "tipo_parte_autora",
  "ente_publico_reu",
  "advogado_especializado",
  "foro",
  "resultado"
)

# =========================
# CONVERTER TIPOS
# =========================

dados$area <- as.factor(dados$area)
dados$tipo_parte_autora <- as.factor(dados$tipo_parte_autora)
dados$ente_publico_reu <- as.factor(dados$ente_publico_reu)
dados$advogado_especializado <- as.factor(dados$advogado_especializado)
dados$foro <- as.factor(dados$foro)
dados$resultado <- as.factor(dados$resultado)

# =========================
# TREINO E TESTE
# =========================

set.seed(123)

indice_treino <- sample(
  1:nrow(dados),
  size = 0.7 * nrow(dados)
)

treino <- dados[indice_treino, ]
teste <- dados[-indice_treino, ]

# =========================
# MODELO
# =========================

modelo_arvore <- rpart(
  resultado ~ .,
  data = treino,
  method = "class"
)

# =========================
# AVALIACAO
# =========================

previsoes <- predict(
  modelo_arvore,
  newdata = teste,
  type = "class"
)

print(confusionMatrix(previsoes, teste$resultado))

# =========================
# PLOT DA ARVORE
# =========================

rpart.plot(
  modelo_arvore,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Arvore de Decisao Jurimetrica"
)

# =========================
# API PLUMBER
# =========================

#* Home
#* @get /
function() {
  list(
    status = "API funcionando"
  )
}

#* Fazer previsao
#* @param area
#* @param valor_acao
#* @param tipo_parte_autora
#* @param ente_publico_reu
#* @param advogado_especializado
#* @param foro
#* @get /prever
function(
    area,
    valor_acao,
    tipo_parte_autora,
    ente_publico_reu,
    advogado_especializado,
    foro
) {
  
  novo <- data.frame(
    area = factor(area, levels = levels(dados$area)),
    valor_acao = as.numeric(valor_acao),
    tipo_parte_autora = factor(
      tipo_parte_autora,
      levels = levels(dados$tipo_parte_autora)
    ),
    ente_publico_reu = factor(
      ente_publico_reu,
      levels = levels(dados$ente_publico_reu)
    ),
    advogado_especializado = factor(
      advogado_especializado,
      levels = levels(dados$advogado_especializado)
    ),
    foro = factor(
      foro,
      levels = levels(dados$foro)
    )
  )
  
  previsao <- predict(
    modelo_arvore,
    newdata = novo,
    type = "class"
  )
  
  probabilidades <- predict(
    modelo_arvore,
    newdata = novo,
    type = "prob"
  )
  
  list(
    previsao = as.character(previsao),
    probabilidades = as.list(probabilidades[1, ])
  )
}

# =========================
# RODAR API
# =========================

