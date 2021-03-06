#carregando os pacotes necessarios
library(data.table)
library(caTools)
library(dplyr)
library(ggplot2)
library(rpart)
library(dplyr)


#coletando os dados
treino = fread('treino.csv',header = T,stringsAsFactors = F,sep = ',')
teste = fread('teste.csv',header = T,stringsAsFactors = F,sep = ',')


#visualizando os dados
str(treino)
dim(treino)
View(treino)

#verificando se há valor ausente
sum(is.na(treino))

#visualizando os dados
str(teste)
dim(teste)
View(teste)

#verificando se há valor ausente
sum(is.na(teste))

#Todas as outras variaveis são do tipo inteiro, então decidir 
#transforma a data do click em inteira tambem
treino$Venta_hoy = as.integer(treino$Venta_hoy)
treino$Dev_proxima = as.integer(treino$Dev_proxima)

#normalizando os dados 
treino_norm = as.data.frame(scale(treino))
teste_norm = as.data.frame(scale(teste))


#gerando um subset para analisar a demanda por semana
grafico = treino %>%
  select(Semana,Demanda_uni_equil) %>%
  group_by(Semana) %>%
  summarise(Demanda = sum(Demanda_uni_equil))
  
#gerando um subset para analisar os produtos com mais demanda por semana
grafico2 = treino %>%
  select(Producto_ID,Demanda_uni_equil,Semana) %>%
  group_by(Semana,Producto_ID) %>%
  summarise(Demanda = sum(Demanda_uni_equil)) %>%
  filter(Demanda > 2000000) %>% 
  arrange(Semana)


ggplot(grafico,aes(x=Semana,y=Demanda))+
  geom_col()+
  ggtitle('Demanda Por Semana')
 

ggplot(grafico2, mapping = aes(x = Semana, y = Demanda, color = factor(Producto_ID))) +
  geom_point(shape=1) + 
  geom_line() +
ggtitle('Produtos mais demandados por Semana') 

#Não entendi porque o dataset de teste tem menos variaveis que o de treino
#Quando treinava o modelo dava erro. Acusando a falta dessas variaveis
#Não sabia como resolver então decidir apaga-las
treino = treino[,c(1:6,11)]


#1° o modelo
modelo = rpart(Demanda_uni_equil~.,data=treino_norm)

previsao = predict(modelo,teste_norm)

plot(modelo, uniform=TRUE)
text(modelo, use.n=TRUE, all=TRUE, cex=1)

#outro modelo
modelo2 = lm(Demanda_uni_equil~.,data=treino_norm)

previsao2 = predict(modelo,teste_norm)

plot(modelo2)


resultado = cbind(previsao2,treino_norm$Demanda_uni_equil)
colnames(resultado) = c('Previsto','Real')
resultado = as.data.frame(resultado)

mse = mean((resultado$Real-resultado$Previsto)^2)

rmse = mse^0.5

sse = sum((resultado$Real-resultado$Previsto)^2)
sst = sum((mean(treino_norm$Demanda_uni_equil) - resultado$Real)^2)

r2 = 1 - (sse/sst)



