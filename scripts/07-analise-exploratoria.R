# Arquivo: 07-analise-exploratoria.R
# Autor(a): Maria Eduarda Souza 
# Data: 25/05/2026
# Objetivos:
# 1. Calcular estatísticas descritivas amostrais.
# 2. Visualizar a distribuição empírica dos dados.
# 3. Comparar padrões entre grupos.

# 0. Configurações globais ---------------------------------------------

# Controla a forma como os valores numéricos aparecem no console.
options(digits = 5, scipen = 999)

# Carrega os pacotes usados para caminhos, manipulação e visualização.
library(here)
library(tidyverse)
library(tidyplots)


# 1. Carregamento de dados ---------------------------------------------------


# Permite que os gráficos do tidyplots usem o espaço disponível.
tidyplots_options(width = NA, height = NA)

#Usa o caminho relativo para encontrar os dados limpos 
caminho_dados <- here("dados/limpos/dados_marketing_limpos.rds")

#Carrega dados de marketing preparados na lista 2.
dados_marketing <- read_rds(caminho_dados)

#Confere variáveis, tipos e primeiras observaçoes dos dados 
glimpse(dados_marketing)


# 2. Inspeção inicial -----------------------------------------------------

#Verifica o tamanho dos dados: qauantidade de linhas e colunas 
dim(dados_marketing)

#Lista o nome das variáveis disponíveis 
names(dados_marketing)

#Mostra as primeiras linhas para inspeção rápida dos dados 
head(dados_marketing)

#Conta quantas semanas aparecem em cada status de promoção
dados_marketing |> 
  count(status_promocao)


# 3. Primeira inspeção estatística da receita -----------------------------

#Calcula estatísticas iniciais da receita semanal de vendas
inspecao_receita <- dados_marketing |> 
  summarize(
    n = n(),
    minimo = min(receita_vendas),
    maximo = max(receita_vendas),
    amplitude = max(receita_vendas) - min(receita_vendas)
  )

#Mostra a tabela de inspeção no console 
inspecao_receita


# 4. Distribuição empirica da receita -------------------------------------

#Visualiza como a receita semanal se distribui nos dados observados.
#tidyplot() inicia o gráfico e add_histogram() adiciona o histograma.
#O argumento bins controla a quantidade de classes do histograma 
dados_marketing |>
  tidyplot(x = receita_vendas) |>
  add_histogram(bins = 20)

x <- c(1,2,1000)
mean(x)   #usa media para distribuições simétricas
median(x) #usada para distribuições assimétricas


# 5. Valores tipicos da receita -------------------------------------------

#Calcula média e mediana para descrever o centro da distribuição.
centro_receita <- dados_marketing |>
  summarize(
    media = mean(receita_vendas),
    mediana = median(receita_vendas)
  )

centro_receita