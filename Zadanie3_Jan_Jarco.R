#Zadanie 3 Jan Jarco nr albumu 82762
#Zadanie 1
rm(list=ls())
getwd()
setwd("C:/Users/User/Documents/R/ME")
library(car)
library(lmtest)
library(AER)
library(tseries)
install.packages('stargazer')
library(stargazer)
df <- read.csv('Export.csv')
head(df)
summary((df))
#usuwam wiersze z brakuj¹cymi danymi
df <- na.omit(df)
summary((df))
nrow(df)
hist(df$Export)
lexp <- log(df$Export)
hist(lexp)
length(lexp)
#Zdecydowanie bardziej mo¿emy siê spodziewaæ po logarytmie tej zmiennej zachowania tej w³asnoœci
#Zadanie 2
colnames(df)
lgdprep <- log(df$GDP_Reporter)
lgdppar <- log(df$GDP_Partner)
ldist <- log(df$dist)
help(lm)
model1 <- lm(lexp ~ lgdprep +lgdppar + ldist )
summary(model1)

scatterplot(lgdprep, lgdppar)
#Model jest statystycznie istotny
#beta1 -Wzrost PKB kraju eksportuj¹cego o 1% powoduje wzrost eksportu brutto towarów w tys. o 1.32%, ceteris paribus 
#beta2 - Wzrost PKB partnera handlowego o 1% powoduje wzrost eksportu brutto towarów w tys. o 0.91%, ceteris paribus 
#beta3 - Wzrost dystansu pomiêdzy krajami o 1% powoduje spadek eksportu brutto [...] o 1.56%, ceteris paribus 
#Wszystkie oszacowania modelu s¹ statystycznie istotne
#Zadanie 3
#Podany model objasnia 61% zmiennoœci eksportu brutto wzglêdem danych empirycznych
#Zadanie 4
# TEST RESET
#Wyrzucam wiersze z niepe³nymi danymi 

nrow(df)
model1
yhat=model1$fitted.values
yhat
length(yhat)
yhat2=yhat^2
yhat3=yhat^3


model_reset=lm(lexp~lgdppar+lgdprep+ldist + yhat2+ yhat3, data = df)
linearHypothesis(model_reset,c(
  "yhat2=0",
  "yhat3=0"
))

# w pakiecie lmtest
reset(model1)
#Brak poprawnej postaci funkcyjnej modelu

#Zadanie 5
# H0 : Beta2 = -Beta3
# H0 : Beta2 != -Beta3
linearHypothesis(model1,  "lgdppar= -ldist")
#H0 :PKB partnera handlowego wzrasta procentowo wraz z spadkiem procentowym dystansu pomiêdzy krajami
#Przy danym poziomie pvalue odrzucam hipotezê zerow¹ wspomniane¹ linijkê wy¿ej na rzecz hipotezy alternatywnej,
#nie ma ona sensu ekonomicznego. S¹ to zmienne niezale¿ne.
#Zadanie 6
# H0 : Beta1 = Beta2 = -Beta3
# H1: Beta1 = Beta2 = -Beta3 (co najmniej jedno nie jest prawdziwe)
#H0 PKB kraju eksportuj¹cego i partnera handlowego wzrasta procentowo wraz z spadkiem procentowym dystansu 
#pomiêdzy krajami

linearHypothesis(model1, c( "lgdppar= -ldist","lgdppar= lgdprep" ))
#Przy danym poziomie pvalue odrzucam hipotezê zerow¹ ,co najmniej jedna równoœæ nie jest spe³niona 
#Tak samo, nie ma ona sensu ekonomicznego. Wynika to równie¿ z wczeœniejszej hipotezy z zadania 5

#Zadanie 7
eu_countries = factor(c( 'AUT', 'BEL', 'BGR','HRV', 'CZE', 'DNK', 'EST', 'FIN', 'FRA',
                'DEU', 'GRC', 'HUN', 'IRL', 'ITA', 'LVA', 'LTU', 'LUX', 'MLT',
                'NLD', 'POL', 'PRT', 'ROU', 'SVK', 'SVN', 'ESP', 'SWE', 'GBR'))

df$EU_rep <- ifelse(df$Reporter %in%eu_countries,1,0)
df$EU_par <- ifelse(df$Partner %in% eu_countries,1,0)

model2 <-lm(lexp ~ lgdprep +lgdppar + ldist + df$EU_rep + df$EU_par, data = df )
summary(model2)
#beta1 -Wzrost PKB kraju eksportuj¹cego o 1% powoduje wzrost eksportu brutto towarów w tys. przeciêtnie o 1.29%, ceteris paribus
#beta2 - Wzrost PKB partnera handlowego o 1% powoduje wzrost eksportu brutto towarów w tys. przeciêtnie o 0.91%, ceteris paribus 
#beta3 - Wzrost dystansu pomiêdzy krajami o 1% powoduje spadek eksportu brutto [...] przeciêtnie o 1.56%, ceteris paribus 
#beta4 - jeœli kraj eksportuj¹cy jest z UE to powoduje wzrost eksportu kraju eksportuj¹cego œrednio o 0,59%, ceteris paribus
#beta5 - jeœli kraj partnerski jest z UE to powoduje wzrost eksportu w kraju gospodarki eksportuj¹cej œrednio o 0,05%, ceteris paribus
# Pierwsze 3 ró¿ni¹ siê one, ale nieznacznie. Zmienna  
#H0: df$EU_par=df$EU_rep
linearHypothesis(model2,'df$EU_par=df$EU_rep')
#Odrzucam hipotezê H0 przy tym pvalue . Te oszacowania paramteróW s¹ od siebie statystycznie ró¿ne
#Zatem wy³¹cznie to,¿e kraj eksportuj¹cy jest z UE istotnie wp³ywa na wartoœæ eksportu. Cz³onkowstwo w UE partnera handlowego nie jest statystycznie istotne.






