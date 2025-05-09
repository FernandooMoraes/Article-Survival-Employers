library(readr)
banco <- read.csv(file = "C:/Users/ferna/Downloads/PIBIC 13.05.2021/PIBIC 13.05.2021/banco1904.txt")

# VER O TOTAL DE MUNIC?PIOS

# ------------------------------------------------------------

banco$motivodeslig <- as.factor(banco$motivodeslig)
banco$mesadmissao <- as.factor(banco$mesadmissao)
banco$mesdeslig <- as.factor(banco$mesdeslig)
banco$municipio <- as.factor(banco$municipio)

banco$nacionalidade <- as.factor(banco$nacionalidade)
banco$tipoadmissao <- as.factor(banco$tipoadmissao)
banco$tipovinculo <- as.factor(banco$tipovinculo)

# --------------------------------------------------------------------------

summary(banco$tempoemprego)
1 - sum(banco$tempoemprego > 420)/nrow(banco)

# [ANALISANDO OS INDIV?DUOS ACIMA DE 420 MESES]


banco <- split(banco, banco$tempoemprego > 420)$'TRUE' #15410 obs

summary(as.factor(banco$fxetaria))/nrow(banco)
# 99% acima de 50

summary(as.factor(banco$fxhoracontrat))/nrow(banco)
# predominio de 30-40h

summary(as.factor(banco$fxremunmedia))/nrow(banco)

summary(banco$remunmedia)

summary(as.factor(banco$escolaridade))/nrow(banco)
#73,8% com m?dio completo ou superior completo

summary(as.factor(banco$raca.cor))/nrow(banco)

summary(as.factor(banco$sexo))/nrow(banco)

summary(as.factor(banco$tamestab))/nrow(banco)
#75,64% em m?dias e grandes empresas

summary(as.factor(banco$regiao))/nrow(banco)
# somente 8% na reg metropolitana

summary(as.factor(banco$desligado))/nrow(banco)
# 90% n?o desligados em 2015

summary(as.factor(banco$admitido))/nrow(banco)

summary(as.factor(banco$regiao))/nrow(banco)

summary(banco$tempoemprego)

# ------------------------------------------------------------------------

# [RETIRANDO ESSAS OBSERVA??ES]
as.numeric(banco$tempoemprego)
banco <- read.csv(file = "C:/Users/ferna/Downloads/PIBIC 13.05.2021/PIBIC 13.05.2021/banco1904.txt")
banco <- split(banco, as.numeric(banco$tempoemprego) <= 420)$'TRUE'  # 9140878

# --------------------------------------------------------------------
# [REGIÕES]

AL <- split(banco, banco$estado == "Alagoas")$`TRUE`
BA <- split(banco, banco$estado == "Bahia")$`TRUE`
CE <- split(banco, banco$estado == "Ceara")$`TRUE`
MA <- split(banco, banco$estado == "Maranhao")$`TRUE`
PB <- split(banco, banco$estado == "Paraiba")$`TRUE`
PE <- split(banco, banco$estado == "Pernambuco")$`TRUE`
PI <- split(banco, banco$estado == "Piaui")$`TRUE`
RN <- split(banco, banco$estado == "Rio Grande do Norte")$`TRUE`
SE <- split(banco, banco$estado == "Sergipe")$`TRUE`


## Checando tamanho
(dim(AL)[1]+dim(BA)[1]+dim(CE)[1]+dim(MA)[1]+dim(PB)[1]+
    dim(PE)[1]+dim(PI)[1]+dim(RN)[1]+dim(SE)[1]) #igual a nrow
nrow(banco)

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

# [PROPORÇÃO - REGIÕES]

## Proporção de desligados pra cada estado 

table(AL$desligado) # 157315 
p1=table(AL$desligado)[[1]]/nrow(AL) 

table(BA$desligado) # 819957
p2=table(BA$desligado)[[1]]/nrow(BA)

table(CE$desligado) # 532787 
p3=table(CE$desligado)[[1]]/nrow(CE)

table(MA$desligado) # 220048 
p4=table(MA$desligado)[[1]]/nrow(MA)

table(PB$desligado) # 168852  
p5=table(PB$desligado)[[1]]/nrow(PB)

table(PE$desligado) # 602649 
p6=table(PE$desligado)[[1]]/nrow(PE)

table(PI$desligado) # 126680
p7=table(PI$desligado)[[1]]/nrow(PI)

table(RN$desligado) # 192300
p8=table(RN$desligado)[[1]]/nrow(RN)

table(SE$desligado) # 119023
p9=table(SE$desligado)[[1]]/nrow(SE)



# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# [AMOSTRAGEM ESTRATIFICADA]

amostra <-function(N1,N2,N3,N4,N5,N6,N7,N8,N9,
                   p1,p2,p3,p4,p5,p6,p7,p8,p9,
                   N,z,d){
  
  parte1 <- ((N1)^2*p1*(1-p1))/(N1/N) 
  parte2 <- ((N2)^2*p2*(1-p2))/(N2/N) 
  parte3 <- ((N3)^2*p3*(1-p3))/(N3/N) 
  parte4 <- ((N4)^2*p4*(1-p4))/(N4/N) 
  parte5 <- ((N5)^2*p5*(1-p5))/(N5/N) 
  parte6 <- ((N6)^2*p6*(1-p6))/(N6/N) 
  parte7 <- ((N7)^2*p7*(1-p7))/(N7/N) 
  parte8 <- ((N8)^2*p8*(1-p8))/(N8/N) 
  parte9 <- ((N9)^2*p9*(1-p9))/(N9/N)
  
  resultado <- ((z)^2*(parte1+parte2+parte3+
                         parte4+parte5+parte6+
                         parte7+parte8+parte9))/((N)^2*(d)^2)
  
  resultado <- trunc(resultado)+1
  
  amostra1 <- resultado*(N1/N)
  amostra2 <- resultado*(N2/N)
  amostra3 <- resultado*(N3/N)
  amostra4 <- resultado*(N4/N)
  amostra5 <- resultado*(N5/N)
  amostra6 <- resultado*(N6/N)
  amostra7 <- resultado*(N7/N)
  amostra8 <- resultado*(N8/N)
  amostra9 <- resultado*(N9/N)
  
  retorno<- c(resultado,amostra1,amostra2,amostra3,
              amostra4,amostra5,amostra6,
              amostra7,amostra8,amostra9)
  
  return(retorno)
}


N1=nrow(AL)
N2=nrow(BA)
N3=nrow(CE)
N4=nrow(MA)
N5=nrow(PB)
N6=nrow(PE)
N7=nrow(PI)
N8=nrow(RN)
N9=nrow(SE)

N=nrow(banco)

amostra(N1,N2,N3,N4,N5,N6,N7,N8,N9,
        p1,p2,p3,p4,p5,p6,p7,p8,p9,
        N,z=1.96,d=0.02)

# ========================================================================

#fixar a semente 

set.seed(2099) 

amostraAL<-AL[runif(115,1,nrow(AL)),] #n1
amostraBA<-BA[runif(572,1,nrow(BA)),] #n2
amostraCE<-CE[runif(379,1,nrow(CE)),] #n3

amostraMA<-MA[runif(154,1,nrow(MA)),] #n4
amostraPB<-PB[runif(128,1,nrow(PB)),] #n5
amostraPE<-PE[runif(421,1,nrow(PE)),] #n6

amostraPI<-PI[runif(96,1,nrow(PI)),] #n7
amostraRN<-RN[runif(141,1,nrow(RN)),] #n8
amostraSE<-SE[runif(93,1,nrow(SE)),] #n9


peso_amostraAL <- rep(N1/N, nrow(amostraAL))
peso_amostraBA <- rep(N2/N, nrow(amostraBA))
peso_amostraCE <- rep(N3/N, nrow(amostraCE))
peso_amostraMA <- rep(N4/N, nrow(amostraMA))
peso_amostraPB <- rep(N5/N, nrow(amostraPB))
peso_amostraPE <- rep(N6/N, nrow(amostraPE))
peso_amostraPI <- rep(N7/N, nrow(amostraPI))
peso_amostraRN <- rep(N8/N, nrow(amostraRN))
peso_amostraSE <- rep(N9/N, nrow(amostraSE))

#testando se a soma dos pesos dá 1
#peso1 + peso2 + peso3 + peso4+ peso5+peso6+peso7+peso8+peso9

#[1] 2095.00000  114.76014  571.96372  378.06465  153.48939  127.95783  420.31816   95.03557
#[9]  140.89676   92.51379

# 115+572+379+154+128+421+96+141+93 #----> N = 2099

amostra <- rbind(amostraAL,amostraBA,amostraCE,
                 amostraMA, amostraPB, amostraPE,
                 amostraPI, amostraRN, amostraSE)
amostra_peso <- c(peso_amostraAL,peso_amostraBA,peso_amostraCE,
                      peso_amostraMA, peso_amostraPB, peso_amostraPE,
                      peso_amostraPI, peso_amostraRN, peso_amostraSE)
names(amostra)

amostra$peso <- amostra_peso
summary(amostra$peso)
# Visualizar o dataframe resultante
head(amostra)

rm(amostraAL,amostraBA,amostraCE,
   amostraMA, amostraPB, amostraPE,
   amostraPI, amostraRN, amostraSE, AL,BA,CE,MA,PB,PE,PI,RN,SE)


# ----------------------------------------------------------------

#write.csv(amostra2, "amostra.txt", row.names = FALSE)

#amostra <- read.csv(file = "C:/Users/ferna/Downloads/PIBIC 13.05.2021/PIBIC 13.05.2021/amostra.txt")

# [ RECATEGORIZANDO AS VARI?VEIS ]

library(forcats)

# AGRUPANDO PRETOS E PARDOS E "OUTRO" COMO REFER?NCIA

summary(factor(amostra$raca.cor))

amostra$raca.cor <- fct_collapse(factor(amostra$raca.cor),
                                 # branca
                                 "Branca/Amarela" = c("Branca/Amarela"),
                                 # Preta e parda
                                 "Preta/Parda" = c("Preta/Parda"),
                                 # outro
                                 "Outro" = c("Outro"))

amostra$raca.cor <- relevel(as.factor(amostra$raca.cor) , ref='Outro')

# AGRUPANDO AS MICRO EMPRESAS E "GRANDE" COMO REFER?NCIA

summary(factor(amostra$tamestab))

amostra$tamestab <- fct_collapse(factor(amostra$tamestab),
                                 # Micro
                                 "Micro" = c("Micro1", "Micro2"),
                                 # Pequena
                                 "Pequena" = c("Pequena"),
                                 # M?dia/grande
                                 "Media/Grande" = c("Media", "Grande"))

amostra$tamestab <- relevel(as.factor(amostra$tamestab) , ref='Micro')


# SEXO MASCULINO COMO REFER?NCIA

amostra$sexo <- relevel(as.factor(amostra$sexo) , ref='M')


# CATEGORIZANDO O NUM. DE HORAS CONTRATUAIS E COLOCANDO 40-44 COMO REFERENCIA


# FAIXA HORA CONTRATUTAL (FAIXA 3 COMO REFER?NCIA)

summary(factor(amostra$fxhoracontrat))

amostra$fxhoracontrat <- fct_collapse(factor(amostra$fxhoracontrat),
                                      # At? 40 - fx 1 e 2
                                      "At? 40" = c("FxHora1", "FxHora2"),
                                      # 40-44 - fx 3
                                      "40-44" = c("FxHora3"))


amostra$fxhoracontrat <- relevel(as.factor(amostra$fxhoracontrat) , ref='40-44')

# Grau de Instru??o do trabalhador

summary(factor(amostra$escolaridade))

amostra$escolaridade <- relevel(as.factor(amostra$escolaridade) , ref='Analfabeto') # Refer?ncia Analfabeto


# Faixa Et?ria

summary(factor(amostra$fxetaria))


amostra$fxetaria <- fct_collapse(factor(amostra$fxetaria),
                                 
                                 "18-24" = c("faixa1"),
                                 
                                 "25-29" = c("faixa2"),
                                 
                                 "30-39" = c("faixa3"),
                                 
                                 "40-49" = c("faixa4"),
                                 
                                 "50+" = c("faixa5"))




amostra$fxetaria <- fct_collapse(factor(amostra$fxetaria),
                                 
                                 "At? 24 anos" = c("18-24"),
                                 
                                 "25-49" = c("25-29", "30-39","40-49" ),
                                 
                                 "50+" = c("50+"))


amostra$fxetaria <- relevel(as.factor(amostra$fxetaria) , ref="At? 24 anos")

#Admitido

summary(factor(amostra$admitido))

amostra$admitido <- relevel(as.factor(amostra$admitido) , ref='N\xe3o admitido')


# faixa de remunera??o m?dia

summary(as.factor(amostra$fxremunmedia))

amostra$fxremunmedia <- fct_collapse(factor(amostra$fxremunmedia),
                                 
                                 "0-3" = c("Teto1", "Teto2"),
                                 
                                 "3-7" = c("Teto3"),
                                 
                                 "7+" = c("Teto4", "Teto5"))


amostra$fxremunmedia <- relevel(as.factor(amostra$fxremunmedia) , ref='0-3')


amostra$estado <- fct_collapse(factor(amostra$estado),
                                 
                                 "Alagoas" = c("Alagoas"),
                                 
                                 "Bahia" = c("Bahia"),
                                 
                                 "Cear?" = c("Ceara"),
                                 
                                 "Maranh?o" = c("Maranhao"),
                               
                                 "Para?ba" = c("Paraiba"),
                               
                                 "Pernambuco" = c("Pernambuco"),
                                 
                                 "Piau?" = c("Piaui"),
                               
                                 "Rio Grande do Norte" = c("Rio Grande do Norte"),
                               
                                 "Sergipe" = c("Sergipe"))

#--------------------------------------------------------------------


# [ AN?LISE DE SOBREVIV?NCIA ]

require(survival)

amostra$desligado <- ifelse(amostra$desligado == "Desligado", 0,1)

tempos <- amostra[,c("tempoemprego")]
tempos <- as.vector(tempos)
cens <- as.numeric(amostra[,c("desligado")])
cens <- as.vector(cens)

### EXPONENCIAL ###
ajust1 <- survreg(Surv(tempos, cens) ~ 1, data = amostra, weights = amostra$peso, dist = 'exponential')
ajust1

alpha<-exp(ajust1$coefficients[1]) #estimador de m?xima verossimilhan?a
alpha

#exp(-time/49.37826)

### WEIBULL ###

ajust2<-survreg(Surv(tempos,cens) ~ 1, data = amostra, weights = amostra$peso,dist='weibull')
ajust2

alpha<-exp(ajust2$coefficients[1])
gama<-1/ajust2$scale
cbind(gama, alpha)


#exp(-(5/48.60044)^0.9318282)

##### LOG NORMAL #####

ajust3<-survreg(Surv(tempos,cens)~1, weights = amostra$peso,dist='lognorm')
ajust3

#pnorm((-log(3.14053)+3.299586)/1.301223)


##### GAMA GENERALIZADA ####
#install.packages("flexsurv")
#install.packages("Rcpp")

library(flexsurv)
library(Rcpp)
      
#ajust4 <- flexsurvreg(Surv(tempos,cens)~1, dist='gengamma')

ajust5 <- flexsurvreg(Surv(tempos,cens)~1,weights = amostra$peso, dist='gengamma.orig')
ajust5$loglik
ajust5
#summary(ajust5)

ajust5$coefficients
coef(ajust5)

pgengamma.orig(5, shape = 0.41, scale = 1.0709, k=4.226, lower.tail = FALSE, log.p = FALSE)


#0.41/(1.0709^(4.226*0.41))

# ------------------------------------------------------------------------------------------------------------  

AIC(ajust1,ajust2,ajust3, ajust5) 


###################### M?TODOS GR?FICOS #########


ekm <- survfit(Surv(tempos,cens)~1)
summary(ekm)

plot(ekm,lty=1, xlab="Tempo de Emprego", ylab="S(t) estimada", ylim=range(c(0,1)))
plot(ekm,lty=1, xlab="Tempo de Emprego", ylab="S(t) estimada", ylim=range(c(0,1)), conf.int=F)


time<-ekm$time
alpha<- exp(ajust1$coefficients[1])
alpha2<- exp(ajust2$coefficients[1])
gama <- 1/ajust2$scale
alpha3<- exp(ajust3$coefficients[1])
gama2<-1/ajust3$scale


(st<-ekm$surv)

(ste<-exp(-time/alpha))

(stw<-exp(-(time/alpha2)^gama))

(stln<-pnorm((-log(time)+ajust3$icoef[1])/ajust3$scale))

gg<-as.data.frame(summary(ajust5))
stgg<-gg$est

# Compara??o dos modelos
cbind(time,st,ste,stw,stln, stgg)

par(mfrow=c(1,1))
plot(ekm,conf.int = F,lwd= 3,xlab="Tempos",ylab="S(t)")
lines(c(0,time),c(1,ste),lty=2,lwd= 3,col = "orange")
lines(c(0,time),c(1,stw),lty=3,lwd= 3,col = "blue")
lines(c(0,time),c(1,stln),lty=4,lwd= 3,col = "green")
lines(c(0,gg$time), c(1,stgg), lty=5, lwd=3, col = "red")
legend(270,1, lty=c(1,2,3,4,5), lwd = c(3,3,3,3,3), c("Kaplan-Meier","Exponencial","Weibull","Log-normal", "Gama Generalizada"),
       col=c("black","orange","blue","green", "red"),bty="n")

 
# Weibull
plot(ekm,conf.int = F,xlab="Tempos",ylab="S(t)")
lines(c(0,time),c(1,stw),lty=3,col = "blue")
legend(25,0.8,lty=c(1,2),c("Kaplan-Meier","Weibull"),bty="n",cex=0.8)

# Lognorm
plot(ekm,conf.int = F,xlab="Tempos",ylab="S(t)")
lines(c(0,time),c(1,stln),lty=4,col = "green")
legend(25,0.8,lty=c(1,2),c("Kaplan-Meier","Log-normal"),bty="n",cex=0.8)

# Gama generalizada
plot(ekm, conf.int=F, xlab="Tempos", ylab="S(t)")
lines(c(0,gg$time), c(1,stgg), lty=2, lwd = 2, col = "red")
legend(25, 0.8, lty=c(1,2), c("Kaplan-Meier", "Gama generalizada"), col = c("black", "red"), bty="n", cex=0.8) 


# -----------------------------------------------------------------------------

# [AJUSTE DO MODELO LOGNORM]


ajuste1 <- survreg(Surv(tempos,cens)~ amostra$fxremunmedia + 
                    amostra$idade + 
                    amostra$raca.cor + 
                    amostra$sexo,
                  dist="lognorm")


summary(ajuste1)

AIC(ajuste1)

#1.96473+ 0 +0.03238*25 + 0.29246 + 0.07384



ajuste <- flexsurvreg(Surv(tempos,cens)~ 1 +amostra$fxremunmedia + 
                        amostra$idade + 
                        amostra$raca.cor + 
                        amostra$sexo, weights = amostra$peso,dist='gengamma.orig')
ajuste

summary(ajuste)



2.221 + 0.047 + 0.242 + 0.037*25 + 0.269



#?coef.flexsurvreg

(ajuste)

exp(0.79783)


ajuste$coefficients

#?`flexsurv-package`


coef(ajuste)
ajuste$coefficients

pgengamma.orig(3.704, shape = 0.79783, scale = 5.27111, k=1.58983, lower.tail = FALSE, log.p = FALSE)



# -----------------------------------------------------------------------------------

# [RES?DUOS]

resdev <- residuals(ajuste, type="deviance")
summary(resdev) # ylim

par(mfrow=c(1,1))

posit <- cbind(seq(1,length(resdev)), resdev)

plot(posit[which(cens==1),1], resdev[which(cens==1)], 
     xlab=" ", ylab="Martingale-type residual", main=" ", 
     ylim=c(-4, 3), pch=16, col="blue", cex=0.8)
points(posit[which(cens==0),1], resdev[which(cens==0)], 
       pch=17, col="blue", cex=0.8) #+ abline(h = c(-3,-2,2,3), lty = 3)


# [RES?DUOS]

#s?o chamados de gr?ficos QQ "sem tend?ncia" porque removem a 
#tend?ncia diagonal da linha de compara??o QQ.

# O eixo vertical do gr?fico worm retrata, para cada observa??o, 
#a diferen?a entre sua localiza??o nas distribui??es te?rica e emp?rica

#install.packages("gamlss")
library(gamlss)

#o pacote gamlss.cens para ajustar vari?veis de resposta censuradas

install.packages("gamlss.cens")
library(gamlss.cens)

model <- gamlss(Surv(tempos, cens) ~ amostra$fxremunmedia + 
                  amostra$idade + 
                  amostra$raca.cor + 
                  amostra$sexo,weights = amostra$peso, data = amostra,
                family=cens(LOGNO))

summary(model)

wp(model, ylim.all = 12*sqrt(1/length(fitted(model))))
set.seed(1)
wp(model)

# GAMA GENERALIZADA

modelgama <- gamlss(Surv(tempos, cens) ~ amostra$fxremunmedia + 
                  amostra$idade + 
                  amostra$raca.cor +
                    amostra$sexo,weights = amostra$peso,
                  data = amostra,
                family=cens(GG))

modelgama <- gamlss(Surv(tempos, cens) ~ amostra$fxremunmedia + 
                      amostra$idade + 
                      amostra$raca.cor +
                      amostra$sexo, weights = amostra$peso,
                    data = amostra,
                    family=cens(GG()))

summary(modelgama)
quantis<-resid(modelgama)

summary(quantis)
#matriz que vai receber os valores
matriz <- matrix(nrow = 2099, ncol = 50)
pGG(3.733368, mu=10.13961, sigma=0.9902083, nu=0.7656, lower.tail = FALSE,
    log.p = FALSE)

#for que percorre a linha e a coluna dessa matriz
#for que percorre a linha e a coluna dessa matriz
for (j in 1:50){
  set.seed(13444)
  for (i in 1:length(cens)) {
    if(cens[i]==0)
    {
      acum = tempos[i]
      acumulada_estimada = pGG(acum, mu=10.13961, sigma=0.9902083, nu=0.7656, lower.tail = FALSE,
                               log.p = FALSE)
      u = runif(1, min = acumulada_estimada, max = 1)
      matriz[i, j] =qnorm(u)
    }
    else
    {
      acum = tempos[i]
      u = pGG(acum, mu=10.13961, sigma=0.9902083, nu=0.7656, lower.tail = FALSE,
              log.p = FALSE)
      matriz[i, j] = qnorm(u)
    }
  }
}
residuos = as.data.frame(matriz)

residuos$cens = cens

#residuos[order(residuos$valores), ]
residuos_ordenado <- as.data.frame(lapply(residuos, function(x) sort(x, decreasing = FALSE)))
resid <- apply(residuos_ordenado, 1, median)
wp(modelgama, resid = resid, ylim.all = 12 * sqrt(1/length(resid)), 
   ylim.worm = 12 * sqrt(n.inter/length(resid)))
# mu = exp(2.247353)  intercepto
# sigma exp(-0.00774)  sigma 
# nu é o nu no modelo

#pGG(3.733368, mu=9.462655, sigma=0.9922899, nu=0.80379, lower.tail = FALSE, log.p = FALSE)
2.247353 +0.046778+0.241876 +0.269611+0.037110*25


########## ponderado
# mu = exp(2.31645)  intercepto
# sigma exp(-0.00984)  sigma 
# nu é o nu no modelo 0.7656



pGG(3.733368, mu=10.13961, sigma=0.9902083, nu=0.7656, lower.tail = FALSE,
    log.p = FALSE)

2.247353 +0.046778+0.241876 +0.269611+0.037110*25

?pGG

set.seed(2)
wp(modelgama)
summary(modelgama)
summary(ajuste)

ajuste

?wp

# WEIBULL

modwei <- gamlss(Surv(tempos, cens) ~ amostra$fxremunmedia + 
                      amostra$idade + 
                      amostra$raca.cor +
                      amostra$sexo, 
                    data = amostra,
                    family=cens(WEI))


wp(modwei)


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# [GRAFICO DAS CURVAS ESTIMADAS DAS COVARIAVEIS]


# --------------- FIGURA 01 -------------------------------------

par(mfrow = c(2,2))

var_1 <- Surv(tempos,cens)
km_var_1 <- survfit(var_1 ~ amostra$fxetaria, data=amostra)

plot(km_var_1, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa Et?ria")
legend(120,1, lty=1, c("At? 24 anos", "25 a 49 anos", "50 anos ou mais"),
       col=c(1:5),lwd=3, bty= "n", cex=0.9)



var_2 <- Surv(tempos,cens)
km_var_2 <- survfit(var_2 ~ amostra$sexo, data=amostra)

plot(km_var_2, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,420), 
     cex.axis=1.1, cex.lab=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Sexo")
legend(120,1, lty=1, c("Masculino", "Feminino"),
       col=c(1:2),lwd=3, bty= "n", cex=0.9)



var_3 <- Surv(tempos,cens)
km_var_3 <- survfit(var_3 ~ amostra$fxremunmedia, data=amostra)

plot(km_var_3, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,420), ylim = c(0.0,1.0), 
     cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Faixa de Remunera??o M?dia")
legend(100,1, 0.3, lty=1, c("0-3", "3-7", "7+"),
       col=c(1:5),lwd=3, bty= "n", cex=0.9)


var_4 <- Surv(tempos,cens)
km_var_4 <- survfit(var_4 ~ amostra$admitido, data=amostra)


plot(km_var_4, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,400), 
     cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3, main = "Admiss?o")
legend(120,1, lty=1, c(" N?o admitido", "Admitido"),
       col=c(1:2),lwd=3, bty= "n", cex=0.9)



# ---------------------------------------------------------------

summary(factor(amostra$fxetaria))
par(mfrow = c(1,1))
# Idade 
var_1 <- Surv(tempos,cens)
km_var_1 <- survfit(var_1 ~ amostra$fxetaria, data=amostra)

plot(km_var_1, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, 
     xlim=c(0,420), cex.axis=1.1, cex.lab=1.1, lty = 1, lwd = 3)
legend("topright", lty=1, c("At? 24 anos", "25 a 49 anos", "50 anos ou mais"),
       col=c(1:5),lwd=3, bty= "o", box.lwd = 1, box.lty = 2, cex=0.9, text.width = 70)



plot(km_var_1, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty = 1, c(levels(amostra[,c("fxetaria")])), col=c(1:5),lwd=1, bty= "n", cex=0.9)


# Sexo

var_2 <- Surv(tempos,cens)
km_var_2 <- survfit(var_2 ~ amostra$sexo, data=amostra)

plot(km_var_2, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty=1, c("Masculino", "Feminino"),
       col=c(1:2),lwd=1, bty= "n", cex=0.9)


var_2 <- Surv(tempos,cens)
km_var_2 <- survfit(var_2 ~ amostra$sexo, data=amostra)

plot(km_var_2, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty = 1, c(levels(amostra[,c("sexo")])), col=c(1:5),lwd=1, bty= "n", cex=0.9)


# Faixa de remunera??o m?dia

summary(as.factor(amostra$fxremunmedia))

var_3 <- Surv(tempos,cens)
km_var_3 <- survfit(var_3 ~ amostra$fxremunmedia, data=amostra)

plot(km_var_3, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,420), ylim = c(0.0,1.0), cex.axis=1.1, cex.lab=1.1)
legend("topright", 0.3, lty=1, c("0-3", "3-7", "7+"),
       col=c(1:5),lwd=1, bty= "n", cex=0.8)


plot(km_var_3, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty = 1, c(levels(amostra[,c("fxremunmedia")])), col=c(1:5),lwd=1, bty= "n", cex=0.9)



# Admitido
var_4 <- Surv(tempos,cens)
km_var_4 <- survfit(var_4 ~ amostra$admitido, data=amostra)
summary(km_var_4)

plot(km_var_4, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,400), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty=1, c(" N?o admitido", "Admitido"),
       col=c(1:2),lwd=1, bty= "n", cex=0.9)


# Ra?a/Cor

summary(amostra$raca.cor)

var_5 <- Surv(tempos,cens)
km_var_5 <- survfit(var_5 ~ amostra$raca.cor, data=amostra)
summary(km_var_5)

plot(km_var_5, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty=1, c("Outro", "Branca/Amarela", "Preta/Parda"),
       col=c(1:4),lwd=1, bty= "n", cex=0.9)


plot(km_var_5, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty = 1, c(levels(amostra[,c("raca.cor")])), col=c(1:5),lwd=1, bty= "n", cex=0.9)



# Tamanho da empresa

summary(amostra$tamestab)

var_6 <- Surv(tempos,cens)
km_var_6 <- survfit(var_6 ~ amostra$tamestab, data=amostra)
summary(km_var_6)

plot(km_var_6, col = c(1:3), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty=1, c("Micro","Pequena" ,"Media/Grande"),
       col=c(1:4),lwd=1, bty= "n", cex=0.8)


plot(km_var_6, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty = 1, c(levels(amostra[,c("tamestab")])), col=c(1:5),lwd=1, bty= "n", cex=0.9)





# Quantidade de Horas Contratuais

summary(amostra$fxhoracontrat)

var_7 <- Surv(tempos,cens)
km_var_7 <- survfit(var_7 ~ amostra$fxhoracontrat, data=amostra)
summary(km_var_7)

plot(km_var_7, col = c(1:2), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty=1, c("40 a 44h", "At? 40h"),
       col=c(1:2),lwd=2, bty= "n", cex=0.9)


plot(km_var_7, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty = 1, c(levels(amostra[,c("fxhoracontrat")])), col=c(1:5),lwd=1, bty= "n", cex=0.9)



# Grau de Instru??o do trabalhador

summary(amostra$escolaridade)

var_8 <- Surv(tempos,cens)
km_var_8 <- survfit(var_8 ~ amostra$escolaridade, data=amostra)
summary(km_var_8)

plot(km_var_8, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty=1, c("Analfabeto", "Fundamental incompleto", "Fundamental completo", "M?dio completo", "Superior completo"),
       col=c(1:6),lwd=1, bty= "n", cex=0.9)


plot(km_var_8, col = c(1:5), xlab = "Tempo", ylab = "S(t) estimada", 
     conf.int = F, xlim=c(0,420), cex.axis=1.1, cex.lab=1.1)
legend("topright", lty = 1, c(levels(amostra[,c("escolaridade")])), col=c(1:5),lwd=1, bty= "n", cex=0.9)


# -----------------------------------------------------------------------------------------------------------------------------------------------------------------------

# --- Ra?a/Cor -------

ggplot(amostra, aes(y= tempoemprego, fill = raca.cor)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  #theme_bw() +
  theme_minimal(base_size = 18) +
  scale_fill_brewer(palette = "Greens") + ylab("Tempo de Emprego") + labs(fill = "Ra?a/Cor:")



# ------- Escolaridade ------


ggplot(amostra, aes(y= tempoemprego, fill = escolaridade)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  theme_minimal(base_size = 18) +
  scale_fill_brewer(palette = "Greens") + ylab("Tempo de Emprego") + labs(fill = "Escolaridade:")


# --- Sexo -------

ggplot(amostra, aes(y= tempoemprego, fill = sexo)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  theme_minimal(base_size = 18) +
  scale_fill_brewer(palette = "Greens") + ylab("Tempo de Emprego") + labs(fill = "Sexo:")

# ----- Faixa de remunera??o m?dia ----- 

ggplot(amostra, aes(y= tempoemprego, fill = fxremunmedia)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  theme_minimal(base_size = 18) +
  scale_fill_brewer(palette = "Greens") + ylab("Tempo de Emprego") + labs(fill = "Fx Remun:")


# ---- Tamanho estab. ----

ggplot(amostra, aes(y= tempoemprego, fill = tamestab)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  theme_minimal(base_size = 18) +
  scale_fill_brewer(palette = "Greens") + ylab("Tempo de Emprego") + labs(fill = "Tamanho da Empresa:")



# ---- Regi?o ----

ggplot(amostra, aes(y= tempoemprego, fill = regiao)) +
  geom_boxplot() +
  scale_x_discrete(name = "") +
  theme_minimal(base_size = 18) +
  scale_fill_brewer(palette = "Greens") + ylab("Tempo de Emprego") + labs(fill = "Regi?o:")








