# ------------------------------------------------------------------------------------

# ------------- An?lise Descritiva e ExprorAtéria da Amostra -------------------------

install.packages('ggplot')
library(ggplot2)

library(scales)

install.packages('readr')
library(readr)

library(forcats)

data <- read.csv(file = 'amostra.txt')

## Faixa et?ria por sexo  

#NORDESTE

#feminino <- split(banco, banco$sexo == "F")$`TRUE`
#summary(as.factor(feminino$fxetaria))/nrow(feminino)[1]  #3224328
#masculino <- split(banco, banco$sexo == "M")$`TRUE`
#summary(as.factor(masculino$fxetaria))/nrow(masculino)[1]  # 5916550

df2 <- data.frame(Sexo=rep(c("Feminino", "Masculino"), each=5),
                  faixa=rep(c("18-24", "25-29", "30-39", "40-49", "50+"),2),
                  len=c(0.20617660, 0.20586460, 0.33797461, 0.17236553, 0.07761865,
                        0.1876680, 0.1875916, 0.3312307, 0.1859242, 0.1075855 ))

df2$faixa<-as.factor(df2$faixa)
ggplot(data=df2, aes(x=faixa, y=len, fill=Sexo)) +
  geom_bar(stat="identity", position=position_dodge())+
  #scale_fill_brewer(palette="Paired")+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") + 
  #xlab(" Faixa Et?ria dos Trabalhadores NORDESTE")+
  xlab(" ")

#AMOSTRA

fem <- split(amostra, amostra$sexo == "F")$`TRUE`
summary(as.factor(fem$fxetaria))/nrow(fem)[1]  #727
masc <- split(amostra, amostra$sexo == "M")$`TRUE`
summary(as.factor(masc$fxetaria))/nrow(masc)[1]  # 1351


df2 <- data.frame(Sexo=rep(c("Feminino", "Masculino"), each=5),
                  faixa=rep(c("18-24", "25-29", "30-39", "40-49", "50+"),2),
                  len=c(0.21595598, 0.20220083, 0.33975241, 0.17193948, 0.07015131,
                        0.20116618, 0.18367347, 0.32798834, 0.18804665, 0.09912536))

df2$faixa<-as.factor(df2$faixa)
ggplot(data=df2, aes(x=faixa, y=len, fill=Sexo)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + ylab("Frequência relativa") + xlab(" Faixa Et?ria dos Trabalhadores AMOSTRA")


## ------------------------- Ra?a/cor por sexo ------------------------------- 

#summary(as.factor(feminino$raca.cor))/nrow(feminino)[1]
#summary(as.factor(masculino$raca.cor))/nrow(masculino)[1]

#NORDESTE

df2 <- data.frame(Sexo=rep(c("Feminino", "Masculino"), each=3),
                  ra?a=rep(c("Preta/Parda", "Branca/Amarela", "Outros"),2),
                  len=c(0.6493880,0.2434628,0.1071491,
                        0.6805013,0.2092263,0.1102724))

ggplot(data=df2, aes(x=ra?a, y=len, fill=Sexo)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("Preta/Parda", "Branca/Amarela", "Outros"), limits = c("Preta/Parda", "Branca/Amarela", "Outros"))+
  #scale_fill_brewer(palette="Paired")+
  theme_minimal(base_size = 18) + 
  ylab("Frequência relativa") + xlab(" ")
  #theme_bw(base_size = 14)
  
  


# AMOSTRA

#summary(as.factor(amostra$raca.cor))

#summary(as.factor(fem$raca.cor))/nrow(fem)[1]  
#summary(as.factor(masc$raca.cor))/nrow(masc)[1] 

df2 <- data.frame(Sexo=rep(c("Feminino", "Masculino"), each=3),
                  ra?a=rep(c("Preta/Parda", "Branca/Amarela", "Outros"),2),
                  len=c(0.6354883,0.2475928,0.1169188,
                        0.6690962,0.2244898,0.1064140))

ggplot(data=df2, aes(x=ra?a, y=len, fill=Sexo)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("Preta/Parda", "Branca/Amarela", "Outros"), limits = c("Preta/Parda", "Branca/Amarela", "Outros"))+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + ylab("Frequência relativa") + xlab("Ra?a/Cor dos Trabalhadores AMOSTRA")


# REGI?O METROPOLITANA E INTERIOR

## -------------- Faixa hor?ria contratual semanal --------------------------

#NORDESTE

#metropole <- split(banco, banco$regiao == "metropole" | banco$regiao == "capital")$`TRUE`
#interior <- split(banco, banco$regiao == "interior")$`TRUE`

#summary(as.factor(metropole$fxhoracontrat))/nrow(metropole)[1]
#summary(as.factor(interior$fxhoracontrat))/nrow(interior)[1]


df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=3),
                  tam=rep(c("Até 30h", "30-40h", "40-44h"),2),
                  len=c(0.05374764, 0.10489468 ,0.84135768,
                        0.04170909, 0.05787392 ,0.90041699))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("Até 30h", "30-40h", "40-44h"), limits = c("Até 30h", "30-40h", "40-44h"))+
  scale_fill_brewer(palette="Paired")+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") +
  xlab(" ")
  #xlab("Faixa hor?ria contratual semanal NORDESTE")


#AMOSTRA

summary(as.factor(amostra$regiao))

metropole <- split(amostra, amostra$regiao == "metropole" | amostra$regiao == "capital")$`TRUE`
interior <- split(amostra, amostra$regiao == "interior")$`TRUE`

summary(as.factor(metropole$fxhoracontrat))/nrow(metropole)[1]
summary(as.factor(interior$fxhoracontrat))/nrow(interior)[1]



df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=3),
                  tam=rep(c("Até 30h", "30-40h", "40-44h"),2),
                  len=c(0.05353160, 0.09516729, 0.85130112,
                        0.05305040, 0.06100796, 0.88594164 ))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("Até 30h", "30-40h", "40-44h"), limits = c("Até 30h", "30-40h", "40-44h"))+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + ylab("Frequência relativa") + xlab("Faixa hor?ria contratual semanal AMOSTRA")

## ------------------------------------ Faixa et?ria ---------------------

#NORDESTE

#summary(as.factor(metropole$fxetaria))/nrow(metropole)[1]
#summary(as.factor(interior$fxetaria))/nrow(interior)[1]

df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=5),
                  tam=rep(c("18-24", "25-29", "30-39", "40-49", "50+"),2),
                  len=c(0.1839774, 0.1877226, 0.3343884, 0.1894341, 0.1044775,
                        0.21216705, 0.20514114, 0.33223991, 0.16655934, 0.08389257))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("18-24", "25-29", "30-39", "40-49", "50+"), limits = c("18-24", "25-29", "30-39", "40-49", "50+"))+
  scale_fill_brewer(palette="Paired")+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") + 
  xlab(" ")
  #xlab("Faixa et?ria dos trabalhadores NORDESTE")

#AMOSTRA

summary(factor(metropole$fxetaria))/nrow(metropole)[1]
summary(factor(interior$fxetaria))/nrow(interior)[1]

df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=5),
                  tam=rep(c("18-24", "25-29", "30-39", "40-49", "50+"),2),
                  len=c(0.1992565 ,0.1747212, 0.3286245, 0.1947955, 0.1026022,
                        0.21883289, 0.21750663, 0.33819629, 0.16047745, 0.06498674))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("18-24", "25-29", "30-39", "40-49", "50+"), limits = c("18-24", "25-29", "30-39", "40-49", "50+"))+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + ylab("Frequência relativa") + xlab("Faixa et?ria dos trabalhadores AMOSTRA")



## Faixa de remunera??o m?dia 

#NORDESTE

#summary(as.factor(metropole$fxremunmedia))/nrow(metropole)[1]
#summary(as.factor(interior$fxremunmedia))/nrow(interior)[1]

df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=5),
                  tam=rep(c("Até 1", "1-3", "3-7", "7-15", "15+"),2),
                  len=c(0.095601959, 0.783821909, 0.089391708, 0.023879982, 0.007304441,
                        0.157326258, 0.772644669, 0.055399371, 0.011323768, 0.003305934 ))


ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("Até 1", "1-3", "3-7", "7-15", "15+"), limits = c("Até 1", "1-3", "3-7", "7-15", "15+"))+
  scale_fill_brewer(palette="Paired")+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") + 
  xlab(" ")
  #xlab("Faixa de Remune??o m?dia em sal?rios m?nimos NORDESTE")



df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=3),
                  tam=rep(c("0-3", "3-7", "7+"),2),
                  len=c(0.86617100, 0.09665428, 0.03717472,
                        0.91777188, 0.07161804, 0.01061008 ))


ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("0-3", "3-7", "7+"), limits = c("0-3", "3-7", "7+"))+
  scale_fill_brewer(palette="Paired")+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") + 
  xlab(" ")



#AMOSTRA

summary(factor(metropole$fxremunmedia))/nrow(metropole)[1]
summary(factor(interior$fxremunmedia))/nrow(interior)[1]

df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=5),
                  tam=rep(c("Até 1", "1-3", "3-7", "7-15", "15+"),2),
                  len=c(0.086245353, 0.779925651, 0.096654275, 0.028252788, 0.008921933,
                        0.14058355, 0.77718833, 0.07161804, 0.00795756, 0.00265252))


ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_x_discrete(labels = c("Até 1", "1-3", "3-7", "7-15", "15+"), limits = c("Até 1", "1-3", "3-7", "7-15", "15+"))+
  scale_fill_brewer(palette="Paired")+
  theme_minimal() + ylab("Frequência relativa") + xlab("Faixa de Remune??o m?dia em sal?rios m?nimos AMOSTRA")


## Tamanho do estabelecimento 

#NORDESTE

#summary(as.factor(metropole$tamestab))/nrow(metropole)[1]
#summary(as.factor(interior$tamestab))/nrow(interior)[1]

df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=3),
                  tam=rep(c("Micro","Pequena", "M?dia/Grande"),2),
                  len=c(0.1042686+0.2013159, 0.2469302, 0.2392285+0.2082567,
                        0.1839394+0.2637396, 0.2469248,0.1527646+0.1526316))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_x_discrete(labels = c("Micro","Pequena", "M?dia/Grande"), limits = c("Micro","Pequena", "M?dia/Grande"))+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") + 
  xlab(" ")
#xlab("Tamanhos dos Estabelecimentos NORDESTE")


#AMOSTRA 

summary(factor(metropole$tamestab))/nrow(metropole)[1]
summary(factor(interior$tamestab))/nrow(interior)[1]

df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=3),
                  tam=rep(c("Micro","Pequena", "M?dia/Grande"),2),
                  len=c(0.1174721+0.1992565, 0.2200743, 0.2446097+0.2185874,
                        0.2718833+0.2400531, 0.2400531, 0.1618037+0.1405836))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_x_discrete(labels = c("Micro","Pequena", "M?dia/Grande"), limits = c("Micro","Pequena", "M?dia/Grande"))+
  theme_minimal() + ylab("Frequência relativa") + xlab("Tamanhos dos Estabelecimentos AMOSTRA")

## Percentual de desligados

#NORDESTE

#summary(as.factor(metropole$desligado))/nrow(metropole)[1]
#summary(as.factor(interior$desligado))/nrow(interior)[1]

df2 <- data.frame(Regiao=rep(c("Metr?pole", "Interior"), each=2),
                  tam=rep(c("Desligado", "Não desligado"),2),
                  len=c(0.3257984,0.6742016,0.3141884,0.6858116))

ggplot(data=df2, aes(x=tam, y=len, fill=Regiao)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_x_discrete(labels = c("Desligado", "Não desligado"), 
                   limits = c("Desligado", "Não desligado"))+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") + 
  xlab(" ")
#xlab("Situa??o dos funcion?rios NORDESTE")

#AMOSTRA

summary(as.factor(metropole$desligado))/nrow(metropole)[1]
summary(as.factor(interior$desligado))/nrow(interior)


df2 <- data.frame(Regiao=rep(c("Metr?pole", "Interior"), each=2),
                  tam=rep(c("Desligado", "Não desligado"),2),
                  len=c( 0.3427509, 0.6572491,0.3116711,0.6883289))

ggplot(data=df2, aes(x=tam, y=len, fill=Regiao)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_x_discrete(labels = c("Desligado", "Não desligado"), 
                   limits = c("Desligado", "Não desligado"))+
  theme_minimal() + ylab("Frequência relativa") + xlab("Situa??o dos funcion?rios AMOSTRA")


## Escolaridade

# NORDESTE

#summary(factor(metropole$escolaridade))/nrow(metropole)[1]
#summary(factor(interior$escolaridade))/nrow(interior)[1]


df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=5),
                  tam=rep(c("Analfabeto","Fund. Completo","Fund. Incompleto", "M?dio completo", "Superior completo"),2),
                  len=c(0.005740552,0.149406227,0.109485442,0.628242500,0.107125279,
                        0.01626954,0.17361813,0.19381194,0.54897762,0.06732276))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_x_discrete(labels = c("Analfabeto","Fund. Completo","Fund. Incompleto", "M?dio completo", "Superior completo"), 
                   limits = c("Analfabeto","Fund. Completo","Fund. Incompleto", "M?dio completo", "Superior completo"))+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") + 
  xlab(" ")
#xlab("Nível de Escolaridade dos Trabalhadores")

# AMOSTRA

summary(factor(metropole$escolaridade))/nrow(metropole)[1]
summary(factor(interior$escolaridade))/nrow(interior)[1]


df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=5),
                  tam=rep(c("Analfabeto","Fund. completo","Fund. incompleto", "M?dio completo", "Superior completo"),2),
                  len=c(0.007434944,0.146468401,0.104089219,0.628252788,0.113754647,
                        0.0198939,0.1737401,0.1750663, 0.5517241,0.0795756))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_x_discrete(labels = c("Analfabeto","Fund. completo","Fund. incompleto", "M?dio completo", "Superior completo"), 
                   limits = c("Analfabeto","Fund. completo","Fund. incompleto", "M?dio completo", "Superior completo"))+
  theme_minimal() + ylab("Frequência relativa") + xlab("Nível de Escolaridade dos Trabalhadores AMOSTRA")


# ---------------------------------------------------------------------------------------

# Motivo desligamento

summary(as.factor(banco$desligado))

banco <- split(banco, banco$desligado == "Desligado")$'TRUE'

metropole <- split(banco, banco$regiao == "metropole" | banco$regiao == "capital")$`TRUE`
interior <- split(banco, banco$regiao == "interior")$`TRUE`

summary(factor(metropole$motivodeslig))/nrow(metropole)[1]
summary(factor(interior$motivodeslig))/nrow(interior)[1]


df2 <- data.frame(Regi?o=rep(c("Metr?pole", "Interior"), each=5),
                  tam=rep(c("Rescis?o com justa causa","Rescis?o sem justa causa","T?rmino do contrato", "Rescis?o indireta", "Exonera??o a pedido"),2),
                  len=c(0.0140023908, 0.6872357214, 0.1549416843, 0.0008174773, 0.1430027262 ,
                        0.0096143320, 0.6759263349, 0.1911314221, 0.0009967715, 0.1223311394))

ggplot(data=df2, aes(x=tam, y=len, fill=Regi?o)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Paired")+
  scale_x_discrete(labels = c("Rescis?o com justa causa","Rescis?o sem justa causa","T?rmino do contrato", "Rescis?o indireta", "Exonera??o a pedido"), 
                   limits = c("Rescis?o com justa causa","Rescis?o sem justa causa","T?rmino do contrato", "Rescis?o indireta", "Exonera??o a pedido"))+
  theme_minimal(base_size = 18) + ylab("Frequência relativa") + 
  xlab(" ")


#########################################################################################################3
#-----------------------------------------------------------------------------------------------
# ---------------------------------------- ARTIGO ----------------------------------------------------------------

install.packages("viridis")
install.packages("hrbrthemes")

library(ggplot2)
library(viridis)
library(hrbrthemes)


### RA?A/COR -----------------------------------------------------------------------------

summary(as.factor(banco$raca.cor))

pretos <- split(banco, banco$raca.cor == "Preta/Parda")$`TRUE`
summary(as.factor(pretos$desligado))
summary(as.factor(pretos$desligado))/nrow(pretos)
rm(pretos)

2037494/(2037494+4082566)

brancos <- split(banco, banco$raca.cor == "Branca/Amarela")$`TRUE`
summary(as.factor(brancos$desligado))/nrow(brancos)
rm(brancos)

566449/(566449+1456453)

outros <- split(banco, banco$raca.cor == "Outro")$`TRUE`
summary(as.factor(outros$desligado))/nrow(outros)
rm(outros)

335668/(335668+662248)



variavel <- c(rep("Preta/Parda" , 2) , rep("Branca/Amarela" , 2) , rep("Outros" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(2037494,4082566,566449,1456453, 335668,662248 )
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Preta/Parda", "Branca/Amarela", "Outros"), 
                   limits = c("Preta/Parda", "Branca/Amarela", "Outros"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607", "Gray")) +
  #annotate("text", x = "Preta/Parda", y = 5000000, label = "33,3%")+
  #annotate("text", x = "Branca/Amarela", y = 1750000, label = "28,0%")+
  #annotate("text", x = "Outros", y = 850000, label = "33,6%")+
  ylim(c(0,9000000))

# -------------------------------------------------------------

table(banco$raca.cor, banco$desligado)

566449/(566449+335668+2037494)  #desligados
1456453/(1456453+662248+4082566) # Não desligados

variavel <- c(rep("Preta/Parda" , 2) , rep("Branca/Amarela" , 2) , rep("Outros" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(0.6931169, 0.6583439, 0.1926952, 0.2348638, 0.1141879, 0.1067924)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência relativa") + xlab(" ")+
  scale_fill_manual(values = c("#F2D479", "#F28B50", "#6B3400"))
  #scale_fill_manual(values = c("#EE5347", "#EB1000", "#C70C00"))
 #scale_fill_manual(values = c("#84C2D9", "#45B6DF", "#2B5059"))
  #annotate("text", x = "Preta/Parda", y = 5000000, label = "33,3%")+
  #annotate("text", x = "Branca/Amarela", y = 1750000, label = "28,0%")+
  #annotate("text", x = "Outros", y = 850000, label = "33,6%")+
  #ylim(c(0,1))


### SEXO ----------------------------------------------------------------------------

summary(as.factor(banco$sexo))

masc <- split(banco, banco$sexo == "M")$`TRUE`
summary(as.factor(masc$desligado)) # Desligado: 2017492 - Não Deslig: 3899058 
summary(as.factor(masc$desligado))/nrow(masc)
rm(masc)

fem <- split(banco, banco$sexo == "F")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 922119  - Não Deslig: 2302209  
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)

variavel <- c(rep("Masculino" , 2) , rep("Feminino" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 2)
value <- c(2017492,3899058,922119,2302209 )
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Masculino","Feminino"), 
                   limits = c("Masculino","Feminino"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607", "Gray")) +
  #annotate("text", x = "Preta/Parda", y = 5000000, label = "33,3%")+
  #annotate("text", x = "Branca/Amarela", y = 1750000, label = "28,0%")+
  #annotate("text", x = "Outros", y = 850000, label = "33,6%")+
  ylim(c(0,9000000))

# ---------------------------------------------------

table(banco$sexo, banco$desligado)

numdesligados = 2939611
naodesligados = 2302209+3899058

922119/numdesligados
2302209/(3899058+2302209)

variavel <- c(rep("Masculino" , 2) , rep("Feminino" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 2)
value <- c(0.6863126,0.6287518,0.3136874,0.3712482)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência relativa") + xlab(" ")+
  scale_fill_manual(values = c("#F2D479","#6B3400"))

### FAIXA ET?RIA ------------------------------------------------------------------

summary(as.factor(banco$fxetaria))

banco$fxetaria <- fct_collapse(factor(banco$fxetaria),
                               
                               "Até 24 anos" = c("faixa1"),
                               
                               "25-49" = c("faixa2",  "faixa3"  ,"faixa4"),
                               
                               "50+" = c("faixa5"))

masc <- split(banco,banco$fxetaria == "Até 24 anos")$`TRUE`
summary(as.factor(masc$desligado)) # Desligado: 703547 - Não Deslig: 1071581 
summary(as.factor(masc$desligado))/nrow(masc)
rm(masc)

703547/(703547 +1071581 )

fem <- split(banco,banco$fxetaria == "25-49")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 2022172  - Não Deslig: 4456775 
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)

fem <- split(banco, banco$fxetaria == "50+")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 213892   - Não Deslig: 672911  
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)


variavel <- c(rep("Até 24 anos" , 2) , rep("25-49" , 2), rep("50+" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(703547, 1071581,2022172,4456775,213892,672911)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Até 24 anos","25-49","50+"), 
                   limits = c("Até 24 anos","25-49","50+"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607", "Gray")) +
  #annotate("text", x = "Preta/Parda", y = 5000000, label = "33,3%")+
  #annotate("text", x = "Branca/Amarela", y = 1750000, label = "28,0%")+
  #annotate("text", x = "Outros", y = 850000, label = "33,6%")+
  ylim(c(0,9000000))


# --------------------------------------------------------

table(banco$fxetaria, banco$desligado)

213892/numdesligados       
672911/naodesligados

variavel <- c(rep("Até 24 anos" , 2) , rep("25-49" , 2), rep("50+" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(0.2393334, 0.1728003,0.6879046,0.7186878,0.07276201,0.1085119)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  scale_fill_discrete(name = "", labels = c("Até 24 anos", "25-49", "50+"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência relativa") + xlab(" ")+
  scale_fill_manual(values = c("#F2D479", "#F2BD1D", "#F28F16"))


### REMUN. M?DIA -------------------------------------------------------------------------

summary(as.factor(banco$fxremunmedia))

banco$fxremunmedia <- fct_collapse(factor(banco$fxremunmedia),
                                     
                                     "0-3" = c("Teto1", "Teto2"),
                                     
                                     "3-7" = c("Teto3"),
                                     
                                     "7+" = c("Teto4", "Teto5"))


masc <- split(banco,banco$fxremunmedia == "0-3")$`TRUE`
summary(as.factor(masc$desligado)) # Desligado: 2730951 - Não Deslig: 5475255  
summary(as.factor(masc$desligado))/nrow(masc)
rm(masc)

fem <- split(banco,banco$fxremunmedia == "3-7")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 162422   - Não Deslig: 542055 
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)

fem <- split(banco, banco$fxremunmedia == "7+")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado:46238   - Não Deslig: 183957 
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)


variavel <- c(rep("0-3" , 2) , rep("3-7" , 2), rep("7+" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c( 2730951,5475255,162422,542055,46238,183957 )
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("0-3","3-7","7+"), 
                   limits = c("0-3","3-7","7+"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607", "Gray")) +
  #annotate("text", x = "Preta/Parda", y = 5000000, label = "33,3%")+
  #annotate("text", x = "Branca/Amarela", y = 1750000, label = "28,0%")+
  #annotate("text", x = "Outros", y = 850000, label = "33,6%")+
  ylim(c(0,9000000))


# ------------------------------------------------

table(banco$fxremunmedia, banco$desligado)

46238/numdesligados
183957/naodesligados

variavel <- c(rep("0-3" , 2) , rep("3-7" , 2), rep("7+" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(0.9290178,0.8829252,0.05525289,0.08741036,0.01572929,0.02966442)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência relativa") + xlab(" ")+
  scale_fill_manual(values = c("#F2D479", "#F2BD1D", "#F28F16"))



### HORAS CONTRATUAIS -------------------------------------------------------------------------


summary(as.factor(banco$fxhoracontrat))/nrow(banco)

banco$fxhoracontrat <- fct_collapse(factor(banco$fxhoracontrat),
                                      # Até 40 - fx 1 e 2
                                      "Até 40" = c("FxHora1", "FxHora2"),
                                      # 40-44 - fx 3
                                      "40-44" = c("FxHora3"))


masc <- split(banco,banco$fxhoracontrat == "Até 40")$`TRUE`
summary(as.factor(masc$desligado)) # Desligado: 322214 - Não Deslig: 932209 
summary(as.factor(masc$desligado))/nrow(masc)
rm(masc)


fem <- split(banco,banco$fxhoracontrat== "40-44")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 2617397  - Não Deslig: 5269058  
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)


variavel <- c(rep("Até 40h" , 2) , rep("40h - 44h" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 2)
value <- c(322214,932209,2617397,5269058)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Até 40h","40h - 44h"), 
                   limits = c("Até 40h","40h - 44h"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607", "Gray")) +
  #annotate("text", x = "Preta/Parda", y = 5000000, label = "33,3%")+
  #annotate("text", x = "Branca/Amarela", y = 1750000, label = "28,0%")+
  #annotate("text", x = "Outros", y = 850000, label = "33,6%")+
  ylim(c(0,9000000))

# -----------------------------------------------------

table(banco$fxhoracontrat, banco$desligado)

2617397/numdesligados        
5269058/naodesligados

variavel <- c(rep("Até 40h" , 2) , rep("40h - 44h" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 2)
value <- c(0.1096111, 0.1503256,0.8903889,0.8496744)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  #scale_fill_discrete(name = " ", labels = c("Até 40h","40h - 44h"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência relativa") + xlab(" ")+
  scale_fill_manual(values = c("#F2BD1D","#F2D479"),guide = guide_legend(reverse = TRUE))



### TAMANHO ESTAB. -------------------------------------------------------------------------

summary(as.factor(banco$tamestab))

banco$tamestab <- fct_collapse(factor(banco$tamestab),
                                 # Micro
                                 "Micro" = c("Micro1", "Micro2"),
                                 # Pequena
                                 "Pequena" = c("Pequena"),
                                 # M?dia/grande
                                 "Media/Grande" = c("Media", "Grande"))

masc <- split(banco,banco$tamestab == "Micro")$`TRUE`
summary(as.factor(masc$desligado)) # Desligado: 1163247  - Não Deslig: 2100928  
summary(as.factor(masc$desligado))/nrow(masc)
rm(masc)

1163247/(1163247+2100928 )

fem <- split(banco,banco$tamestab== "Pequena")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 725237   - Não Deslig: 1531904  
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)

725237/(725237+1531904)

fem <- split(banco, banco$tamestab == "Media/Grande")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 1051127    - Não Deslig: 2568435  
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)

1051127/(1051127+2568435)


variavel <- c(rep("Micro" , 2) , rep("Pequena" , 2), rep("M?dia/Grande" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(1163247,2100928,725237,1531904,1051127,2568435)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Micro","Pequena","M?dia/Grande"), 
                   limits = c("Micro","Pequena","M?dia/Grande"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607", "Gray")) +
  ylim(c(0,9000000))


# -------------------------------------------------------------------------

table(banco$tamestab, banco$desligado)

1051127/numdesligados       
2568435/naodesligados

variavel <- c(rep("Micro" , 2) , rep("Pequena" , 2), rep("M?dia/Grande" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(0.3957146,0.3387901,0.2467119,0.2470308,0.3575735,0.4141791)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  scale_fill_discrete(name = " ", labels = c("Micro","Pequena","M?dia/Grande"))+
  ggtitle("") + 
    theme_minimal(base_size = 18) + 
    theme(legend.title = element_blank()) +
    ylab("Frequência relativa") + xlab(" ")+
  scale_fill_manual(values = c("#F2BD1D", "#F2D479", "#F28F16"))



## ESCOLARIDADE ------------------------------------------------------


summary(as.factor(banco$escolaridade))

masc <- split(banco,banco$escolaridade == "Analfabeto")$`TRUE`
summary(as.factor(masc$desligado)) # Desligado: 35509  - Não Deslig: 51855  
summary(as.factor(masc$desligado))/nrow(masc)
rm(masc)

fem <- split(banco,banco$escolaridade== "Fund. incompleto")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 505527    - Não Deslig: 774702   
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)

fem <- split(banco,banco$escolaridade == "Fund. completo")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 504520    - Não Deslig: 941416 
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)

fem <- split(banco,banco$escolaridade == "M?dio completo")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 1716920    - Não Deslig: 3763105
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)

fem <- split(banco,banco$escolaridade == "Superior completo")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 177135     - Não Deslig:  670189 
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)



variavel <- c(rep("Analfabeto" , 2) , rep("Fund. Incompleto" , 2), rep("Fund. Completo" , 2), rep("M?dio Completo" , 2), rep("Superior Completo" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 5)
value <- c(35509,51855,505527,774702,504520,941416,1716920,3763105,177135,670189)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Analfabeto", "Fund. Incompleto",  "Fund. Completo", "M?dio Completo", "Superior Completo"), 
                   limits = c("Analfabeto", "Fund. Incompleto",  "Fund. Completo", "M?dio Completo", "Superior Completo"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607", "Gray")) +
  ylim(c(0,9000000))


# ---------------------------------------------------------

table(banco$escolaridade, banco$desligado)[,1]/numdesligados
table(banco$escolaridade, banco$desligado)[,2]/naodesligados


variavel <- c(rep("Analfabeto" , 2) , rep("Fund. Incompleto" , 2), rep("Fund. Completo" , 2), rep("M?dio Completo" , 2), rep("Superior Completo" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 5)
value <- c(0.01207949,0.008362001,0.17197071,0.124926406,0.17162815 ,0.151810267,0.58406367,0.606828411,0.06025797,0.108072915 )
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência relativa") + xlab(" ")+
  scale_fill_manual(values = c("#F2D479", "#F28B50", "#F28F16", "#F2BD1D", "#6B3400"))

  

## REGI?O -----------------------------------------------------------------------------

summary(as.factor(banco$regiao))


banco$regiao <- fct_collapse(factor(banco$regiao),
                               
                               "interior" = c("interior"),
                               
                               "metropole" = c("metropole", "capital"))


masc <- split(banco, banco$regiao == "interior")$`TRUE`
summary(as.factor(masc$desligado)) # Desligado: 1041138  - Não Deslig: 2272600  
summary(as.factor(masc$desligado))/nrow(masc)
rm(masc)

fem <- split(banco,banco$regiao == "metropole")$`TRUE`
summary(as.factor(fem$desligado)) # Desligado: 1898473 - Não Deslig: 3928667  
summary(as.factor(fem$desligado))/nrow(fem)
rm(fem)


variavel <- c(rep("Capitais e Regiões Metropolitanas" , 2) , rep("Municípios do Interior" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 2)
value <- c(1898473,3928667,1041138,2272600)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Capitais e Regiões Metropolitanas", "Municípios do Interior"), 
                   limits = c("Capitais e Regiões Metropolitanas", "Municípios do Interior"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607", "Gray")) +
  ylim(c(0,9000000))



# ---------------------------------------------------------------------------

numdesligados = 2939611
naodesligados = 2302209+3899058

table(banco$regiao, banco$desligado)

1041138/numdesligados       
2272600/naodesligados

variavel <- c(rep("Capitais e Regiões Metropolitanas" , 2) , rep("Municípios do Interior" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 2)
value <- c(0.6458246,0.6335265,0.3541754,0.3664735)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank()) +
  ylab("Frequência relativa") + xlab(" ")+
  scale_fill_manual(values = c("#F2BD1D", "#F28B50"))




# --------------------------------------------------------------------------



## Motivo do desligamento


masc <- split(banco, banco$motivodeslig != "0")$`TRUE`
summary(as.factor(masc$motivodeslig)) 
summary(as.factor(masc$motivodeslig))/nrow(masc)
rm(masc)


# Rescis?o sem justa causa: 2008926
# T?rmino de contrato:  493384
# Exonera??o a pedido: 398110
# Outros:39191


variavel <- c(rep("Rescis?o sem justa causa",1), rep("T?rmino do contrato",1), rep("Exonera??o a pedido",1), rep("Outros",1))
condition <- rep(c("Desligado") , 4)
value <- c(2008926,  493384, 398110, 39191)
data <- data.frame(variavel,condition,value)

ggplot(data, aes(fill=condition, y=value, x=variavel)) + 
  geom_bar(position="stack", stat="identity") +
  scale_x_discrete(labels = c("Rescis?o sem justa causa","T?rmino do contrato","Exonera??o a pedido", "Outros"), 
                   limits = c("Rescis?o sem justa causa","T?rmino do contrato","Exonera??o a pedido", "Outros"))+
  ggtitle("") + 
  theme_minimal(base_size = 18) + 
  theme(legend.title = element_blank(), legend.position = "none") +
  ylab("Frequência absoluta") + xlab(" ")+
  scale_fill_manual(values = c("#FF1607")) +
  ylim(c(0,2200000))


# ---------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------

####################################### EDA FINAL ##########################################################################

library(gridExtra)
library(ggplot2)
par(mfrow = c(2,2))

variavel <- c(rep("Preta/Parda" , 2) , rep("Branca/Amarela" , 2) , rep("Outros" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(0.6931169, 0.6583439, 0.1926952, 0.2348638, 0.1141879, 0.1067924)
data <- data.frame(variavel,condition,value)

p <- ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity", width = 0.40) +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  ggtitle("Raça/Cor") + 
  theme_classic(base_size = 14) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Frequência relativa") + xlab(" ")



variavel2 <- c(rep("Masculino" , 2) , rep("Feminino" , 2))
condition2 <- rep(c("Desligado" , "Não Desligado") , 2)
value2 <- c(0.6863126,0.6287518,0.3136874,0.3712482)
data2 <- data.frame(variavel2,condition2,value2)

p2 <- ggplot(data2, aes(fill=variavel2, y=value2, x=condition2)) + 
  geom_bar(position="stack", stat="identity", width = 0.40) +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  ggtitle("Sexo do Trabalhador") + 
  theme_classic(base_size = 14) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Frequência relativa") + xlab(" ")


variavel3 <- c(rep("Até 24 anos" , 2) , rep("25-49" , 2), rep("50+" , 2) )
condition3 <- rep(c("Desligado" , "Não Desligado") , 3)
value3 <- c(0.2393334, 0.1728003,0.6879046,0.7186878,0.07276201,0.1085119)
data3 <- data.frame(variavel3,condition3,value3)

p3 <-ggplot(data3, aes(fill=variavel3, y=value3, x=condition3)) + 
  geom_bar(position="stack", stat="identity", width = 0.40) +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  scale_fill_discrete(name = "", labels = c("Até 24 anos", "25-49", "50+"))+
  ggtitle("Faixa Etária") + 
  theme_classic(base_size = 14) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Frequência relativa") + xlab(" ")


variavel4 <- c(rep("0-3" , 2) , rep("3-7" , 2), rep("7+" , 2) )
condition4 <- rep(c("Desligado" , "Não Desligado") , 3)
value4 <- c(0.9290178,0.8829252,0.05525289,0.08741036,0.01572929,0.02966442)
data4 <- data.frame(variavel4,condition4,value4)

p4 <- ggplot(data4, aes(fill=variavel4, y=value4, x=condition4)) + 
  geom_bar(position="stack", stat="identity", width = 0.40) +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  scale_fill_discrete(name = " ", labels = c("0-3","3-7","7+"))+
  ggtitle("Faixa de Remuneração Média") + 
  theme_classic(base_size = 14) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Frequência relativa") + xlab(" ")


table(banco$fxhoracontrat, banco$desligado)
summary(as.factor(banco$escolaridade))/nrow(banco)


variavel <- c(rep("Até 40h" , 2) , rep("40h - 44h" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 2)
value <- c(0.1096111, 0.1503256,0.8903889,0.8496744)
data <- data.frame(variavel,condition,value)

p5 <- ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity", width = 0.40) +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  #scale_fill_discrete(name = " ", labels = c("Até 40h","40h - 44h"))+
  ggtitle("Faixa Horária Contratual") + 
  theme_classic(base_size = 14) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Frequência relativa") + xlab(" ")



variavel <- c(rep("Micro" , 2) , rep("Pequena" , 2), rep("Média/Grande" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 3)
value <- c(0.3957146,0.3387901,0.2467119,0.2470308,0.3575735,0.4141791)
data <- data.frame(variavel,condition,value)

p6 <- ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity", width = 0.40) +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  scale_fill_discrete(name = " ", labels = c("Micro","Pequena","Média/Grande"))+
  ggtitle("Tamanho do Estabelecimento") + 
  theme_classic(base_size = 14) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Frequência relativa") + xlab(" ")



variavel <- c(rep("Analfabeto" , 2) , rep("Fund. Incompleto" , 2), rep("Fund. Completo" , 2), rep("Médio Completo" , 2), rep("Superior Completo" , 2) )
condition <- rep(c("Desligado" , "Não Desligado") , 5)
value <- c(0.01207949,0.008362001,0.17197071,0.124926406,0.17162815 ,0.151810267,0.58406367,0.606828411,0.06025797,0.108072915 )
data <- data.frame(variavel,condition,value)

p7 <- ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity", width = 0.40) +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  scale_fill_discrete(name = "", labels = c("Analfabeto", "Fund. Incompleto",  "Fund. Completo", "Médio Completo", "Superior Completo"))+
  ggtitle("Nível de Escolaridade") + 
  theme_classic(base_size = 14) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Frequência relativa") + xlab(" ")


variavel <- c(rep("Capitais e Regiões Metropolitanas" , 2) , rep("Municípios do Interior" , 2))
condition <- rep(c("Desligado" , "Não Desligado") , 2)
value <- c(0.6458246,0.6335265,0.3541754,0.3664735)
data <- data.frame(variavel,condition,value)

p8 <- ggplot(data, aes(fill=variavel, y=value, x=condition)) + 
  geom_bar(position="stack", stat="identity", width = 0.40) +
  #scale_fill_viridis(discrete = T) +
  scale_x_discrete(labels = c("Desligado" , "Não Desligado"), 
                   limits = c("Desligado" , "Não Desligado"))+
  ggtitle("Região") + 
  theme_classic(base_size = 14) + 
  theme(legend.title = element_blank(), legend.position="bottom") +
  ylab("Frequência relativa") + xlab(" ")



#grid.arrange(p,p2,p3,p4,nrow =2)
#grid.arrange(p5, p6,p7,p8,nrow =2)


grid.arrange(p, p2,p3, p4,p5, p6,p7,p8,nrow =4)

grid.arrange(p, p2,p3, p4,nrow =2)
grid.arrange(p5, p6,p7,p8,nrow =2)

#############################################################################################################
#############################################################################################################

# ------------------------------------ SOBREV FINAL --------------------------------------------

par(mfrow = c(2,2))

var_1 <- Surv(tempos,cens)
km_var_1 <- survfit(var_1 ~ amostra$fxetaria, data=amostra)

plot(km_var_1, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa Etária")
legend("topright", lty=1, c("Até 24 anos", "25 a 49 anos", "50 anos ou mais"),
       col=c(1:5),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)

var_2 <- Surv(tempos,cens)
km_var_2 <- survfit(var_2 ~ amostra$sexo, data=amostra)

plot(km_var_2, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Sexo")
legend("topright", lty=1, c("Masculino", "Feminino"),
       col=c(1:2),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)

var_3 <- Surv(tempos,cens)
km_var_3 <- survfit(var_3 ~ amostra$fxremunmedia, data=amostra)

plot(km_var_3, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), ylim = c(0.0,1.0), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa de Remuneração Média")
legend("topright", lty=1, c("0-3", "3-7", "7+"),
       col=c(1:5),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)



var_4 <- Surv(tempos,cens)
km_var_4 <- survfit(var_4 ~ amostra$admitido, data=amostra)


plot(km_var_4, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), ylim = c(0.0,1.0), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Admissão")
legend("topright", lty=1, c(" Não admitido", "Admitido"),
       col=c(1:2),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)




var_5 <- Surv(tempos,cens)
km_var_5 <- survfit(var_5 ~ amostra$raca.cor, data=amostra)

plot(km_var_5, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Raça/Cor")
legend("topright", lty=1, c("Outro", "Branca/Amarela", "Preta/Parda"),
       col=c(1:4),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)


var_6 <- Surv(tempos,cens)
km_var_6 <- survfit(var_6 ~ amostra$tamestab, data=amostra)

plot(km_var_6, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Tamanho da Empresa")
legend("topright", lty=1, c("Micro","Pequena" ,"Média/Grande"),
       col=c(1:4),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)


var_7 <- Surv(tempos,cens)
km_var_7 <- survfit(var_7 ~ amostra$fxhoracontrat, data=amostra)

plot(km_var_7, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa Horária Contratual Semanal")
legend("topright", lty=1, c("40 a 44h", "Até 40h"),
       col=c(1:2),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)


var_8 <- Surv(tempos,cens)
km_var_8 <- survfit(var_8 ~ amostra$escolaridade, data=amostra)

plot(km_var_8, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Escolaridade")
legend("topright", lty=1, c("Analfabeto", "Fundamental incomp.", "Fundamental completo", "Médio completo", "Superior completo"),
       col=c(1:6),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 90)





# -------------------------------------------------------------------------------------------------------


par(mfrow = c(2,2))

var_1 <- Surv(tempos,cens)
km_var_1 <- survfit(var_1 ~ amostra$fxetaria, data=amostra)

plot(km_var_1, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa Et?ria")
legend("topright", lty=1, c("Até 24 anos", "25 a 49 anos", "50 anos ou mais"),
       col=c(1:5),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 90)

var_2 <- Surv(tempos,cens)
km_var_2 <- survfit(var_2 ~ amostra$sexo, data=amostra)

plot(km_var_2, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Sexo")
legend("topright", lty=1, c("Masculino", "Feminino"),
       col=c(1:2),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)

var_3 <- Surv(tempos,cens)
km_var_3 <- survfit(var_3 ~ amostra$fxremunmedia, data=amostra)

plot(km_var_3, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), ylim = c(0.0,1.0), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa de Remunera??o M?dia")
legend("topright", lty=1, c("0-3", "3-7", "7+"),
       col=c(1:5),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)



var_4 <- Surv(tempos,cens)
km_var_4 <- survfit(var_4 ~ amostra$admitido, data=amostra)

plot(km_var_4, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), ylim = c(0.0,1.0), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Admiss?o")
legend("topright", lty=1, c(" Não admitido", "Admitido"),
       col=c(1:2),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)


# --------------------------------------------------------------------------------------------------------------

par(mfrow = c(2,2))

var_5 <- Surv(tempos,cens)
km_var_5 <- survfit(var_5 ~ amostra$raca.cor, data=amostra)

plot(km_var_5, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Ra?a/Cor")
legend("topright", lty=1, c("Outro", "Branca/Amarela", "Preta/Parda"),
       col=c(1:4),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)


var_6 <- Surv(tempos,cens)
km_var_6 <- survfit(var_6 ~ amostra$tamestab, data=amostra)

plot(km_var_6, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Tamanho da Empresa")
legend("topright", lty=1, c("Micro","Pequena" ,"Media/Grande"),
       col=c(1:4),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)


var_7 <- Surv(tempos,cens)
km_var_7 <- survfit(var_7 ~ amostra$fxhoracontrat, data=amostra)

plot(km_var_7, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa Hor?ria Contratual Semanal")
legend("topright", lty=1, c("40 a 44h", "Até 40h"),
       col=c(1:2),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)


var_8 <- Surv(tempos,cens)
km_var_8 <- survfit(var_8 ~ amostra$escolaridade, data=amostra)

plot(km_var_8, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Escolaridade")
legend("topright", lty=1, c("Analfabeto", "Fundamental incomp.", "Fundamental completo", "M?dio completo", "Superior completo"),
       col=c(1:6),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 120)




# --------------------------------------------------------------------------------------------

# ----------------------------------- 12/08/2021 -----------------------------------------

par(mfrow = c(2,2))

var_1 <- Surv(tempos,cens)
km_var_1 <- survfit(var_1 ~ amostra$fxetaria, data=amostra)

plot(km_var_1, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa Etária")
legend("topright", lty=1, c("Até 24 anos", "25 a 49 anos", "50 anos ou mais"),
       col=c(1:5),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 90)



var_3 <- Surv(tempos,cens)
km_var_3 <- survfit(var_3 ~ amostra$fxremunmedia, data=amostra)

plot(km_var_3, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), ylim = c(0.0,1.0), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa de Remuneração Média")
legend("topright", lty=1, c("0-3", "3-7", "7+"),
       col=c(1:5),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)



var_5 <- Surv(tempos,cens)
km_var_5 <- survfit(var_5 ~ amostra$raca.cor, data=amostra)

plot(km_var_5, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Raça/Cor")
legend("topright", lty=1, c("Outro", "Branca/Amarela", "Preta/Parda"),
       col=c(1:4),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 90)


var_8 <- Surv(tempos,cens)
km_var_8 <- survfit(var_8 ~ amostra$escolaridade, data=amostra)

plot(km_var_8, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Escolaridade")
legend("topright", lty=1, c("Analfabeto", "Fundamental incomp.", "Fundamental completo", "Médio completo", "Superior completo"),
       col=c(1:6),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 120)










