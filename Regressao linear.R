#### Instalando Pacotes ####
install.packages("tidyverse")
install.packages("car")
install.packages("lmtest")
library(tidyverse)
library(car)
library(lmtest)

#Análise exploratória
plot(dados$area, dados$preco, main = "Diagrama de Dispersão",xlab="Área", ylab="Preço das casas",
                                                             pch=19)
#correlação
cor(dados$area, dados$preco)

cor.test(dados$area, dados$preco)
# p valor esta abaixo de 0.05, devido a isso rejeitamos a hipotese nula de que a correlacao nao e 
#significante


#vamos analisar agora preço e tempo
plot(dados$tempo, dados$preco)
#podemos visualizar que quanto mais antiga a casa é, mais barata ela é.


#fazendo uma analise de correlcao
cor.test(dados$tempo, dados$preco)
#Podemos concluir a mesma afirmação anterior, a unica difença é que a correlação é negativa, mas é forte
# e significante

#########GRAFICO DE BOXPLOT###########

#resumo
summary(dados$preco)

boxplot(dados$preco)
# Existe um valor discrepante no preços dos imoveis, vamos analisar

#função Boxplot do pacote car
Boxplot(dados$preco)
# verificando imovel no indice 79

dados$preco[79]

#indice das casas dos maiores preços
which(dados$preco > quantile(dados$preco, 0.75))

###############################################################################################

#Traçando uma reta Linear preco em função da area

mode1 <- lm(preco~area, data=dados)
mode1

#verificando o preco de uma casa de 70m²
preco_70 <-  499299 + 7890 *70
preco_70

plot(dados$area, dados$preco, main = 'Diagrama da Reta')
abline(mode1, col ='red')

summary(mode1)
#modelo e melhor comparado a média, mas quanto?


# Modelo do preço em função do tempo
mode2 <- lm(preco~tempo, data=dados)
mode2

#verificando a reta 
plot(dados$tempo, dados$preco, main = 'Diagrama da Reta')
abline(mode2, col ='red')

#reta é linear mas negativa
summary(mode2)

#preço em função do tempo cai, nesse caso e correlação forte negativa e
#estatisticamente significante 

###############################################################################################

################### RESIDUOS #####################

#plotando e verificando que os residuoes segue uma distribuição normal
plot(mode1$residuals)

#porem verificamos dois pontos, e localizamos dois indices e nesse caso essas duas observações
# foram as piores estimados no modelo de preco em função da area
identify(mode1$residuals, n=2)

#removendo esses dois indices
dados_59_82 <- dados[c(-59,-82),]
dados_59_82
###############################################################################################

####################      TESTE ESTATISTICOS  ######################################


#Realizando Teste Durbin-Watson que verifca Independência das variaveis, nesse caso são, pois o
# p value deu acima de 0.05
dwtest(mode1)

#verificando se o modelo e homacedastico, nesse caso os residuos segue uma mesma variancia
plot(mode1$fitted.values, mode1$residuals)

#fazendo teste Breusch-Pagan para definir
#se p valor for menor que 0.05 os eerros nao sao homecedastico
bptest(mode1)


#TESTE DE NORMALIDADE DE DADOS Shapiro
plot(mode1, 2)

shapiro.test(mode1$residuals)
#nesse caso se o p valor for maior que 0.05 os dados segue uma distribuição normal


###############################################################################################

#FAZENDO PREDIÇÃO DE VALORES PARA UMA CASA DE 60m² ou 70m²

dados_novos <- data.frame(area =c(60,70))

predict(mode1, newdata = dados_novos)

#colocando um intervalo de predição
predict(mode1, newdata = dados_novos, interval = 'prediction')

#colcoando o intervalo de confiança
predict(mode1, newdata = dados_novos, interval = 'confidence')
