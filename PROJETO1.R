# 1 - Definindo o problema de negócio

#Construir um modelo de Machine Learning capaz de prever o consumo de energia de carros
#elétricos com base em diversos fatores


# 2 - Decisões

#O problema de negócio já informa que é requerido um modelo de 
#Machine Learning. No dataset temos a coluna 
#"mean - Energy consumption [kWh/100 km]" que é a variável 
#que queremos prever. 
#Desta forma, iremos utilizar aprendizagem supervisionada.
#Falta definir pré processamento.
#O trabalho será entregue de duas formas, a saber: gráfico e tabela.

# 3 - Definindo o diretório de trabalho
setwd("C:/Users/Chilov/FCD/PROJETOS/PROJETO1")
getwd()

# 4 - Instalando os pacotes
install.packages("stats")
install.packages("e1071")
install.packages("readxl")
install.packages("corrplot")
install.packages("gmodels")
install.packages("dplyr")
install.packages("car")
install.packages("ggplot2")
install.packages("randomForest")
install.packages("xgboost")
install.packages("fastDummies")
install.packages("MASS")
install.packages("caTools")


# 5 - Carregando os pacotes
library(stats) #para funções estatísticas
library(e1071) #para modelo SVM
library(readxl)#permite leitura de arquivo excel
library(corrplot) #permite gráfico de correlação
library(gmodels) # Algumas ferramentas para treinamento de modelo em R 
library(dplyr) #pré processamento de dados
library(car) #para VIF
library(ggplot2)#permite gráficos
library(randomForest)#para modelo Random Forest
library(xgboost) #para modelo XGBOOST
library(fastDummies)#para criação de variavéis dummy
library(caTools)#permite split em treino e teste


# 6 - Dicionário de dados
#[1] "Car full name" - nome completo do carro (texto/objeto)                         
#[2] "Make"  - nome do fabricante (texto/objeto)                               
#[3] "Model" - nome do modelo (texto/objeto)                              
#[4] "Minimal price (gross) [PLN]" - preço mínimo bruto          
#[5] "Engine power [KW]" - Força do motor                    
#[6] "Maximum torque [Nm]" - Torque máximo                  
#[7] "Type of brakes" - Tipos de freio                       
#[8] "Drive type" - tipo de direção                            
#[9] "Battery capacity [kWh]" - capacidade da bateria               
#[10] "Range (WLTP) [km]" Alcance(Worldwide Harmonised Light Vehicle Test Procedure)                    
#[11] "Wheelbase [cm]" - distância entre eixos                       
#[12] "Length [cm]"  - comprimento                         
#[13] "Width [cm]" - largura                           
#[14] "Height [cm]" - altura                          
#[15] "Minimal empty weight [kg]" - peso mínimo vazio            
#[16] "Permissable gross weight [kg]" - peso bruto admissível         
#[17] "Maximum load capacity [kg]"  - capacidade de carregamento máximo          
#[18] "Number of seats" - número de assentos                     
#[19] "Number of doors" - número de portas                     
#[20] "Tire size [in]" - tamanho do pneu                        
#[21] "Maximum speed [kph]" - velocidade máxima                 
#[22] "Boot capacity (VDA) [l]" - capacidade de inicialização (Verband der Automobilindustrie)              
#[23] "Acceleration 0-100 kph [s]" - aceleração          
#[24] "Maximum DC charging power [kW]" - potência máxima de carregamento DC      
#[25] "mean - Energy consumption [kWh/100 km]" - energia consumida (média)

#Variável target está coluna 25

# 7 - Carregando o dataset e convertendo em dataframe
df_original <- read_excel("FEV-data-Excel.xlsx", sheet = "Auta elektryczne")
#visualiza primeiras linhas
head(df_original)
#visualiza resumo estatístico
summary(df_original)
#outra forma de visualizar nomes das colunas e valores
str(df_original)
#nomes das colunas
names(df_original)
#dimensões do dataframe
dim(df_original)
#visualiza dados em forma de tabela
View(df_original)


#vamos renomear as colunas
colnames(df_original)[1] <- "car_name"
colnames(df_original)[2] <- "make" 
colnames(df_original)[3] <- "model" 
colnames(df_original)[4] <- "min_price_gross"
colnames(df_original)[5] <- "eng_power"
colnames(df_original)[6] <- "max_torq"
colnames(df_original)[7] <- "type_brak"
colnames(df_original)[8] <- "driv_type"
colnames(df_original)[9] <- "bat_capac"
colnames(df_original)[10] <- "range"
colnames(df_original)[11] <- "wheelbase"
colnames(df_original)[12] <- "length"
colnames(df_original)[13] <- "width"
colnames(df_original)[14] <- "height" 
colnames(df_original)[15] <- "min_emp_weig"
colnames(df_original)[16] <- "perm_gross_weig"                 
colnames(df_original)[17] <- "max_load_capac"          
colnames(df_original)[18] <- "seats"
colnames(df_original)[19] <- "doors"
colnames(df_original)[20] <- "tire_size"            
colnames(df_original)[21] <- "max_speed"    
colnames(df_original)[22] <- "boot_capac"                     
colnames(df_original)[23] <- "accel"                 
colnames(df_original)[24] <- "max_charg_power" 
colnames(df_original)[25] <- "ener_consump"         
View(df_original)

#visualiza variável target
df_original["ener_consump"]

# 8 - EDA Análise exploratória de dados

# 8.1 verificar valores duplicados
duplicated(df_original)
sum(duplicated(df_original))
#não há valores duplicados

# 8.2 verificar valores missing

sapply(df_original, function(y) sum(is.na(y)))

#coluna 7 type_brak        1 dado missing
#coluna 16 perm_gross_weig 8 dados missing
#coluna 17 max_load_capac  8 dados missing
#coluna 22 boot_capac      1 dados missing
#coluna 23 accel           3 dados missing
#coluna 25 ener_consump    9 dados missing

#devemos tratar a coluna 25 que é a variável target

#decisão: vamos imputar a média na coluna da variável target
#calculando a média da coluna da variável target
df_2<-df_original
df_2$ener_consump[is.na(df_2$ener_consump)] <- mean(df_2$ener_consump, na.rm = TRUE)
#verificando os valores missing que sobraram

sapply(df_2, function(y) sum(is.na(y)))

#coluna type_brak       1 dados missing
#coluna perm_gross_weig 8 dados missing
#coluna max_load_capac  8 dados missing
#coluna boot_capac      1 dados missing
#coluna accel           3 dados missing

#decisão: vamos imputar a média nas colunas perm_gross_weig,
#boot_capac, accel e max_load_capac
df_3<-df_2
df_3$perm_gross_weig[is.na(df_3$perm_gross_weig)] <- mean(df_3$perm_gross_weig, na.rm = TRUE)
df_3$boot_capac[is.na(df_3$boot_capac)] <- mean(df_3$boot_capac, na.rm = TRUE)
df_3$accel[is.na(df_3$accel)] <- mean(df_3$accel, na.rm = TRUE)
df_3$max_load_capac[is.na(df_3$max_load_capac)] <- mean(df_3$max_load_capac, na.rm = TRUE)
View(df_3)
sapply(df_3, function(y) sum(is.na(y)))

#coluna type_brak       1 dados missing
#decisão: vamos excluir esta linha
df_4 <- df_3[!is.na(df_3$type_brak),]
sapply(df_4, function(y) sum(is.na(y)))
sum(is.na(df_4))
View(df_4)
#não há mais valores missing

#vamos descartar informações que não vamos utilizar
rm(df_original,df_2,df_3)

#8.3 Explorando as variáveis numéricas de interesse

# Análise univariada
# Ao interpretar a variância, números maiores indicam que
# os dados estão espalhados mais amplamente em torno da
# média. O desvio padrão indica, em média, a quantidade
# de cada valor diferente da média.
dim(df_4)# 52 linhas e 25 colunas
boxplot(df_4$eng_power, main = "Boxplot para Força do motor (KW)")
#parece que tem outlier superior#vou verificar
hist(df_4$eng_power,main = "Histograma para Força do motor (KW)")
#maioria até 200KW
round(median(df_4$eng_power),2) #mediana 204
round(mean(df_4$eng_power),2) #média 271
round(var(df_4$eng_power),2)# variância 33427.21
round(sd(df_4$eng_power),2)#desvio padrão  182.8311
round(range(df_4$eng_power), 2)# vai de 82 a 772
diff(range(df_4$eng_power))# variação total de 690
iqr <- round(IQR(df_4$eng_power), 2)
iqr #diferença interquartil 243
Q3 <- round(quantile(df_4$eng_power, probs=0.75), 2)
Q3 # terceiro quartil 379
Q1 <- round(quantile(df_4$eng_power, probs=0.25), 2)
Q1 # primeiro quartil 136
outliersup_eng_power <- Q3+(1.5*iqr) 
outliersup_eng_power# valores acima de 743.5 são outliers
outlierinf_eng_power <- Q1-(1.5*iqr) 
outlierinf_eng_power# valores abaixo de -228.5 são outliers
length(which(df_4$eng_power >= outliersup_eng_power))#apenas 2 valores são outliers

#o maior valor de outlier tem 1049cv (1cv =0,7355kW) que é um valor
#que pode ser atingido e assim o valor é real mesmo 
#(pesquisei valores de referência)


boxplot(df_4$max_load_capac, main = "Boxplot para Capacidade de carregamento máximo (Kg)")
#parece que tem outlier superior#vou verificar
hist(df_4$max_load_capac, main = "Histograma para capacidade de carregamento máximo (Kg)")
#maioria de 400 a 600 - confirma outlier visto no boxplot
round(median(df_4$max_load_capac), 2) #mediana 495
round(mean(df_4$max_load_capac), 2) #média 513
round(var(df_4$max_load_capac), 2) #variância 14702.89
round(sd(df_4$max_load_capac), 2) #desvio padrão  121.2555
round(range(df_4$max_load_capac), 2)# vai de 290 a 1056 
diff(range(df_4$max_load_capac))# variação total de 766
iqr <- round(IQR(df_4$max_load_capac), 2)
iqr #diferença interquartil 101.25
Q3 <- round(quantile(df_4$max_load_capac, probs=0.75), 2)
Q3 # terceiro quartil 546.25
Q1 <- round(quantile(df_4$max_load_capac, probs=0.25), 2)
Q1 # primeiro quartil 445
outliersup_max_load_capac <- Q3+(1.5*iqr)  
outliersup_max_load_capac# valores acima de 698.125 são outliers
outlierinf_max_load_capac <- Q1-(1.5*iqr)  
outlierinf_max_load_capac# valores abaixo de 293.125 são outliers
length(which(df_4$max_load_capac >= outliersup_max_load_capac))#apenas 2 valores são outliers

#o maior valor de outlier tem 1056Kg que é um valor
#que pode ser atingido e assim o valor é real mesmo
#(pesquisei valores de referência)


boxplot(df_4$max_charg_power, main = "Boxplot para Potência máxima de carregamento DC (KW)")
#parece que tem outlier superior e inferior #vou verificar
hist(df_4$max_charg_power, main = "Histograma para Potência máxima de carregamento DC (KW)")
#maioria até 150 - valores de 200 para cima a serem analisados se forem utilizados no modelo
round(median(df_4$max_charg_power), 2) #mediana 100
round(mean(df_4$max_charg_power), 2) #média 113
round(var(df_4$max_charg_power), 2)#variância 3331.896
round(sd(df_4$max_charg_power), 2)#desvio padrão 57.72258
round(range(df_4$max_charg_power), 2)# vai de 22 a 270 
diff(range(df_4$max_charg_power))# variação total de 248
iqr <- round(IQR(df_4$max_charg_power), 2)
iqr #diferença interquartil 50
Q3 <- round(quantile(df_4$max_charg_power, probs=0.75), 2)
Q3 # terceiro quartil 150
Q1 <- round(quantile(df_4$max_charg_power, probs=0.25), 2)
Q1 # primeiro quartil 100
outliersup_max_charg_power <- Q3+(1.5*iqr)  
outliersup_max_charg_power# valores acima de 225 são outliers
outlierinf_max_charg_power <- Q1-(1.5*iqr)  
outlierinf_max_charg_power# valores abaixo de 25 são outliers
length(which(df_4$max_charg_power >= outliersup_max_charg_power))#apenas 4 valores são outliers superiores
length(which(df_4$max_charg_power <= outlierinf_max_charg_power))#apenas 2 valores são outliers inferiores

#valor do outlier inferior é muito próximo do encontrado nos dados (22 / 25)
#decisão: manter valores outliers inferiores
#o maior valor de outlier tem 270kW que é um valor
#que pode ser atinigido e assim o valor é real mesmo
#(pesquisei valores de referência)



boxplot(df_4$perm_gross_weig, main = "Boxplot para Peso bruto máximo admissível (Kg)")
# a princípio sem outliers
hist(df_4$perm_gross_weig, main = "Histograma para Peso bruto máximo admissível (Kg)")
round(median(df_4$perm_gross_weig), 2) #mediana 2240
round(mean(df_4$perm_gross_weig), 2) #média 2265.553
round(var(df_4$perm_gross_weig), 2)#variância 239115.6
round(sd(df_4$perm_gross_weig), 2)#desvio padrão 488.9945
round(range(df_4$perm_gross_weig), 2)# vai de 1310 a 3130  
diff(range(df_4$perm_gross_weig))# variação total de 1820
iqr <- round(IQR(df_4$perm_gross_weig), 2)
iqr #diferença interquartil 726.75
Q3 <- round(quantile(df_4$perm_gross_weig, probs=0.75), 2)
Q3 # terceiro quartil 2683.75
Q1 <- round(quantile(df_4$perm_gross_weig, probs=0.25), 2)
Q1 # primeiro quartil 1957
outliersup_perm_gross_weig <- Q3+(1.5*iqr)  
outliersup_perm_gross_weig# valores acima de 3773.875 são outliers
outlierinf_perm_gross_weig <- Q1-(1.5*iqr)  
outlierinf_perm_gross_weig# valores abaixo de 866.875 são outliers
length(which(df_4$perm_gross_weig >= outliersup_perm_gross_weig))#não há valores outliers superiores
length(which(df_4$perm_gross_weig <= outlierinf_perm_gross_weig))#não há valores outliers inferiores



boxplot(df_4$ener_consump, main = "Boxplot para Consumo de energia - média(kWh/100 km)")
hist(df_4$ener_consump, main = "Histograma para Consumo de energia - média (kWh/100 km)")
#maior parte consome até 20kWh/100Km
round(median(df_4$ener_consump), 2) #mediana 17.8
round(mean(df_4$ener_consump), 2) #média 18.817
round(var(df_4$ener_consump), 2)#variância 14.76523
round(sd(df_4$ener_consump), 2)#desvio padrão 3.842555
round(range(df_4$ener_consump), 2)# vai de 13.1 a 27.55   
diff(range(df_4$ener_consump))# variação total de 14.55
iqr <- round(IQR(df_4$ener_consump), 2)
iqr #diferença interquartil 5.51
Q3 <- round(quantile(df_4$ener_consump, probs=0.75), 2)
Q3 # terceiro quartil 21.36
Q1 <- round(quantile(df_4$ener_consump, probs=0.25), 2)
Q1 # primeiro quartil 15.85
outliersup_ener_consump <- Q3+(1.5*iqr)  
outliersup_ener_consump# valores acima de 29.63 são outliers
outlierinf_ener_consump <- Q1-(1.5*iqr)  
outlierinf_ener_consump# valores abaixo de 7.58 são outliers
length(which(df_4$ener_consump >= outliersup_ener_consump))#não há valores outliers superiores
length(which(df_4$ener_consump <= outlierinf_ener_consump))#não há valores outliers inferiores

#DECISÃO: os dados serão padronizados para atenuar os efeitos dos valores outliers. 
#outro motivo para esta decisão é que temos muitas variáveis e ficaria inviável 
#estudar cada uma delas e tratar outlier de cada coluna (imputando dados ou 
#excluindo, embora devido à baixa quantidade registros, o ideal seria a imputação) 


# Análise bivariada

# Scatterplot Força do motor x consumo de energia de carros
plot(x = df_4$eng_power , y = df_4$ener_consump,
     main = "Scatterplot - Consumo do motor x Consumo de energia média",
     xlab = " Força do motor (KW)",
     ylab = "Consumo de energia - média (kWh/100 km)")
#vê-se uma leve correlação positiva - bom para ML


# Scatterplot capacidade de carregamento máximo x consumo de energia de carros
plot(x = df_4$max_load_capac , y = df_4$ener_consump,
     main = "Scatterplot -  Capacidade de carregamento máximo x Consumo de energia média",
     xlab = "Capacidade de carregamento máximo (Kg)",
     ylab = "Consumo de energia - média (kWh/100 km)")
#não se vê claramente uma correlação - talvez a variável não seja útil para ML


# Scatterplot Potência máxima de carregamento DC x consumo de energia de carros
plot(x = df_4$max_charg_power , y = df_4$ener_consump,
     main = "Scatterplot -  Potência máxima de carregamento DC x Consumo de energia média",
     xlab = "Potência máxima de carregamento DC (KW)",
     ylab = "Consumo de energia - média (kWh/100 km)")
#não se vê uma correlação - bom para ML


# Scatterplot Peso bruto máximo admissível x consumo de energia de carros
plot(x = df_4$perm_gross_weig , y = df_4$ener_consump,
     main = "Scatterplot - Peso bruto máximo admissível (Kg) x Consumo de energia de carros",
     xlab = "Peso bruto máximo admissível (Kg)",
     ylab = "Consumo de energia - média (kWh/100 km)")
#observa-se correlação positiva - bom para ML


# Scatterplot capacidade de carregamento máximo x potência máxima de carregamento DC
plot(x = df_4$max_load_capac, y = df_4$max_charg_power,
     main = "Scatterplot - capacidade de carregamento máximo x potência máxima de carregamento DC",
     xlab = "Capacidade de carregamento máximo (Kg)",
     ylab = "Potência máxima de carregamento DC (KW)")
# a princípio sem correlação - bom para ML


# Scatterplot capacidade de carregamento máximo x potência máxima de carregamento DC
plot(x = df_4$max_load_capac, y = df_4$max_charg_power,
     main = "Scatterplot - capacidade de carregamento máximo x potência máxima de carregamento DC",
     xlab = "Capacidade de carregamento máximo (Kg)",
     ylab = "Potência máxima de carregamento DC (KW)")
# a princípio tem correlação positiva - ruim para ML

# Scatterplot Peso bruto máximo admissível x Força do motor
plot(x = df_4$perm_gross_weig, y = df_4$eng_power,
     main = "Scatterplot - capacidade de carregamento máximo x potência máxima de carregamento DC",
     xlab = "Peso bruto máximo admissível (Kg)",
     ylab = "Força do motor (KW)")
# a princípio sem correlação - bom para ML

# Visualizando relacionamento entre as variáveis: Scatterplot
?pairs
pairs(df_4[c("max_load_capac","max_charg_power","ener_consump")])


#8.4 Explorando as variáveis categóricas de interesse

#algumas variáveis não influirão na resposta à pergunta de negócio e por esta 
#razão serão excluídas do dataset.
#excluindo as colunas car_name, make e model
excluir <- c("car_name", "make", "model")
df_5 <- df_4[,!(names(df_4)%in% excluir)]
dim(df_5)# 52 linhas por 22 colunas
View(df_5)
#vamos descartar informações que não vamos utilizar
rm(df_4)

#algumas variáveis foram classificadas erroneamente pelo interpretador 
#da linguagem R.
#as variáveis type_brak, driv_type, seats, doors e tire_size 
#estão como numéricas e devem ser convertidas para categóricas.

df_5$type_brak <- as.factor(df_5$type_brak)
df_5$driv_type <- as.factor(df_5$driv_type)
df_5$seats <- as.factor(df_5$seats)
df_5$doors <- as.factor(df_5$doors)
df_5$tire_size <- as.factor(df_5$tire_size)

str(df_5)
length(unique(df_5$type_brak))# 2 tipos
length(unique(df_5$driv_type))# 3 tipos
length(unique(df_5$tire_size))# 8 tipos


#Tabela de frequência e proporções desta tabela

sort(table(df_5$type_brak), decreasing=TRUE)
type_brak_table <- table(df_5$type_brak)
w<-sort(prop.table(type_brak_table)*100, decreasing=TRUE)
round(w, digits = 2)
#maioria dos tipos de breque tem disco (frontal+traseiro)

sort(table(df_5$driv_type), decreasing=TRUE)
driv_type_table <- table(df_5$driv_type)
w<-sort(prop.table(driv_type_table)*100, decreasing=TRUE)
round(w, digits = 2)
#maioria dos carros tem tipo de direção 2WD (frontal) 

sort(table(df_5$seats), decreasing=TRUE)
driv_type_table <- table(df_5$seats)
w<-sort(prop.table(driv_type_table)*100, decreasing=TRUE)
round(w, digits = 2)
#maioria dos carros tem 5 assentos

sort(table(df_5$doors), decreasing=TRUE)
doors_table <- table(df_5$doors)
w<-sort(prop.table(doors_table)*100, decreasing=TRUE)
round(w, digits = 2)
#maioria dos carros tem 5 portas

sort(table(df_5$tire_size), decreasing=TRUE)
tire_size_table <- table(df_5$tire_size)
w<-sort(prop.table(tire_size_table)*100, decreasing=TRUE)
round(w, digits = 2)
#maioria dos pneus são de tamanho 16, 19, 17 e 20


#relacionamento entre variáveis categóricas
CrossTable(x=df_5$tire_size, y=df_5$driv_type)
CrossTable(x=df_5$tire_size, y=df_5$type_brak)
CrossTable(x=df_5$type_brak, y=df_5$driv_type)
#metade dos casos está com tamanho do pneu 16, 17 ou 19 e breque tipo disco (frontal e traseiro)

# 8.5 Correlação
# Desejado: alta correlação entre variáveis preditoras e variável target
# Desejado: baixa correlação entre variáveis preditoras

#Indica uma correlação positiva entre min_price_gross e ener_consump
cor(df_5$min_price_gross, df_5$ener_consump) #0.696


#vamos ordenar as colunas antes de executar a correlação
df_6 <- df_5 %>% select(type_brak,driv_type, seats, doors, tire_size, everything())
View(df_6)
dim(df_6) #52x22
#vamos descartar informações que não vamos utilizar
rm(df_5)

# a correlação avalia somente variáveis numéricas
# criando mapa de correlação com números coloridos

L <- cor(df_6[,6:22])
#corrplot(L, method = 'number') 
View(L)

#ver linha da variável target
dim(L)
str(L)
View(L[17,])
target_cor<-L[17,]
target_cor
mode(target_cor)
class(target_cor)
length(target_cor)
typeof(target_cor)
target_cor <- sort(target_cor, decreasing=TRUE)
target_cor


#critério utilizado para utilizar variáveis mais significantes:
#correlação que seja menor que 0.4 e maior que -0.4
target_cor[target_cor < 0.4 & target_cor > -0.4]
#Análise: colunas não relevantes para o modelo: width, height e range

excluir <- c("width", "height","range")
df_7 <- df_6[,!(names(df_6)%in% excluir)]
str(df_7)
dim(df_7)
View(df_7)
#vamos descartar informações que não vamos utilizar
rm(df_6,L)




# 9 - Pré processamento

# Vamos separar variáveis numéricas e categóricas
dados_num <- df_7[,!unlist(lapply(df_7, is.factor))]
dim(dados_num)
dados_fator <- df_7[,unlist(lapply(df_7, is.factor))]
dim(dados_fator)

#antes de iniciar, vamos padronizar as variáveis numéricas
# Padronização
dados_num_norm <- scale(dados_num)
dados_final <- cbind(dados_num_norm, dados_fator)
dim(dados_final)
View(dados_final)
#vamos descartar informações que não vamos utilizar
rm(df_7)

# 10 - Modelo Machine Learning

# 10.1 Modelo de regressão linear múltipla
#vamos fazer o modelo base com regressão linear múltipla
#vamos utilizar todas as variáveis


#dividir em dados de teste e treino

# Criando as amostras de forma randômica

#vamos utilizar regressão linear múltipla para buscar as variáveis mais relevantes
avalia_var<-lm(ener_consump~.,data=dados_final)
summary(avalia_var)

#Residual standard error: 0.3309 on 22 degrees of freedom
#Multiple R-squared:  0.9528,	Adjusted R-squared:  0.8905 
#F-statistic: 15.31 on 29 and 22 DF,  p-value: 4.802e-09


#análise: variáveis para modelo final: 
#driv_type, tire_size, max_torq, max_charg_power 


set.seed(101)
#?sample.split
amostra <- sample.split(dados_final$ener_consump, SplitRatio = 0.80)
# ***** Treinamos nosso modelo nos dados de treino *****
# ***** Fazemos as predições nos dados de teste *****
# Criando dados de treino - 80% dos dados
treino = subset(dados_final, amostra == TRUE)
# Criando dados de teste - 20% dos dados
teste = subset(dados_final, amostra == FALSE)
head(treino)
head(teste)

# Gerando o Modelo_v1 base (Usando todos os atributos)
modelo_v1 <- lm(ener_consump ~ ., treino)
# modelo_v2 contem as variáveis mais importantes conforme avalia_var  
modelo_v2 <- lm(ener_consump ~ driv_type+tire_size+max_torq+max_charg_power, treino)
#tentando outras combinações - variáveis importantes no modelo_v1            
modelo_v3 <- lm(ener_consump ~ max_torq+accel+max_charg_power, treino)
modelo_v4 <- lm(ener_consump ~ max_torq+accel+max_charg_power+tire_size+seats,treino)
#tentando outra combinação - variáveis importantes no modelo_v4  
modelo_v5 <- lm(ener_consump ~ max_torq+seats+max_charg_power,treino)


# Interpretando os modelos
#r2 é o coeficiente de determinação - qaunto maior, melhor (cuidado com overfitting)
summary(modelo_v1) # R2_v1 = 0.9813
summary(modelo_v2) # R2_v2 = 0.6464
summary(modelo_v3) # R2_v3 = 0.4371
summary(modelo_v4) # R2_v4 = 0.788
summary(modelo_v5) # R2_v5 = 0.5548

#Dos modelos de regressão linear, por ora a melhor opção é o modelo_v4.
#O modelo_v1 é muito complexo e pode estar em overfitting.


# 10.2 Modelo de regressão de floresta de decisão para buscar as melhores variáveis
#Decision Forest Regression
#não contente com o percentual de acerto do modelo, decidi utilizar
#o random forest para escolher as melhores variáveis preditoras

set.seed(4543)
data(dados_final)
rf.fit <- randomForest(ener_consump ~ ., data=dados_final, ntree=5,
                       keep.forest=FALSE, importance=TRUE)

### Visualizando a importância das variáveis

# Obtendo a importância das variáveis do modelo de treino
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)

ggplot(ImpData, aes(x=Var.Names, y=`%IncMSE`)) +
  geom_segment( aes(x=Var.Names, xend=Var.Names, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )


#A regressão florestal aleatória em R fornece duas saídas: 
#  diminuição no erro quadrático médio (MSE) e pureza do nó. 
#O erro de previsão descrito como MSE é baseado na permutação de 
#seções prontas dos dados por árvore individual e preditor, 
#e os erros são então calculados. No contexto de regressão, 
#a pureza do nó é a diminuição total na soma residual dos quadrados 
#ao dividir em uma variável calculada a média de todas as árvores 
#(ou seja, quão bem um preditor diminui a variância). 
#O MSE é uma medida mais confiável de importância variável. 
#Se as duas métricas de importância mostrarem resultados diferentes, ouça o MSE.
#por este método as variáveis importantes são: 
#max_load_capac, perm_gross_weig, wheelbase, length, tire_size, eng_power



# 10.3 Modelo de regressão linear múltipla com variáveis importantes obtidas do 
#Decision RandomForest
modelo_v6 <- lm(ener_consump ~ max_load_capac+perm_gross_weig+wheelbase+
                  length+tire_size+eng_power, treino)
summary(modelo_v6) # R2_v6 = 0.8684
#até o momento o modelo_v6 é o melhor


#Estimate Std. Error t value
#(Intercept)      0.64578    0.38387   1.682
#max_load_capac   0.17830    0.14643   1.218
#perm_gross_weig  0.91647    0.15140   6.053
#wheelbase       -0.14382    0.27811  -0.517
#length          -0.00497    0.27841  -0.018
#tire_size15      0.44778    0.42943   1.043
#tire_size16     -0.19699    0.38630  -0.510
#tire_size17     -0.69699    0.39893  -1.747
#tire_size18     -0.72941    0.63069  -1.157
#tire_size19     -1.07643    0.47662  -2.258
#tire_size20     -0.89069    0.48552  -1.834
#tire_size21     -1.04987    0.63837  -1.645
#eng_power        0.31867    0.19212   1.659


# 10.4 Modelo de RandomForest com variáveis importantes obtidas do 
#Decision RandomForest
#?randomForest
set.seed(123)
modelo_v7 <- randomForest(ener_consump ~ max_load_capac+perm_gross_weig+wheelbase+
                       length+tire_size+eng_power,
                        data = treino, 
                        ntree = 40,
                        nodesize = 5)
print(modelo_v7)
plot(modelo_v7) #20 árvores estabiliza o erro
 
#Type of random forest: regression
#Number of trees: 40
#No. of variables tried at each split: 2
#Mean of squared residuals: 0.2410387
#% Var explained: 72.94


 
modelo_v8 <- randomForest(ener_consump ~ max_load_capac+perm_gross_weig+wheelbase+
                            length+tire_size+eng_power,
                          data = treino, 
                          ntree = 20,
                          nodesize = 5)
print(modelo_v8)
plot(modelo_v8)
#% Var explained: 72.94
#até o momento o modelo_v6 é o melhor.


## 10.5 Modelo de Boosted Decision Tree Regression 
## Construindo o modelo

y <- dados_final$ener_consump
X <- dados_final %>% select(-ener_consump)
str(X)
#só aceita valores numéricos
#transforma fator em variável dummy
X <- dummy_cols(X, remove_first_dummy = TRUE)
dataset_xgboost <- cbind(X,y)

#?dummy_cols
split <- sample.split(dataset_xgboost, SplitRatio = 0.8)
treino_xgboost <- subset(dataset_xgboost, split == TRUE)
teste_xgboost <- subset(dataset_xgboost, split == FALSE)
View(treino_xgboost)#43 x 35
#isolar a variável target
#train.y <- as.numeric(as.factor(train.y)) - 1
treino.y <- treino_xgboost$y
teste.y <- teste_xgboost$y
#trocar a variável target de tipo fator para tipo numérico
treino.y <- as.numeric(treino.y) - 1
teste.y <- as.numeric(teste.y) - 1
View(treino.y)
#isolar a variável X (que tem que ser tipo de matriz neste algoritmo)
View(treino_xgboost)
treino.x <- data.matrix(treino_xgboost[,1:34])
teste.x <- data.matrix(teste_xgboost[,1:34])
str(treino.x)
#configurando os parâmetros
?xgboost
?params
params <- list(eta = 0.3, max_depth = 6, 
               subsample = 1,
               colsample_bytree=1,
               min_child_weight= 1,
               gamma = 0,
               eval_metric = "rmse", 
               objective = "reg:squarederror",
               booster = "gblinear")



#iniciar o xgboost
?xgboost
modelo_v9 <- xgboost(data = treino.x, label = treino.y, 
                     params = params, 
                     set_seed = 102,
                     nround = 20,
                     verbose = 1)


modelo_v9


#conclusão: o modelo_v6 foi o melhor até o momento. 
#Tendo em vista a baixa quantidade de dados, é de se 
#esperar que os modelos mais robustos não tenham um desempenho ótimo


## 10.6 Teste de normalidade
#vamos verificar o teste de shapiro
#O Teste de Shapiro-Wilk para normalidade avalia a aderência dos resíduos 
#à distribuição Normal. 
#O p-valor se refere à hipótese de que os resíduos seguem de fato uma distribuição Normal,
#e essa hipótese é rejeitada, de modo geral, quando p é menor que 0.05

shapiro.test(modelo_v6$residuals)
#data:  modelo_v6$residuals
#W = 0.98523, p-value = 0.2965
#podemos assumir que os resíduos seguem distribuição normal


# 11 - Otimização do Modelo
#removendo os outros modelos
rm(modelo_v1, modelo_v2, modelo_v3, modelo_v4, modelo_v5, modelo_v7, 
   modelo_v8, modelo_v9)
   
#otimização do modelo_v6
?lm

#verificamos se há correlação entre as variáveis preditoras
modelo_v6 <- lm(ener_consump ~ max_load_capac+perm_gross_weig+wheelbase+
                 length+tire_size+eng_power, treino)
summary(modelo_v6)
#Residual standard error: 0.441

manter <- c("ener_consump", "max_load_capac", "perm_gross_weig","wheelbase","length",
             "tire_size","eng_power")
df_final <- dados_final[,(names(dados_final)%in% manter)]
dim(df_final)# 52 linhas por 7 colunas
View(df_final)


#cor(dados_final$max_load_capac, dados_final$perm_gross_weig,dados_final$eng_power, dados_final$length)
L <- cor(df_final[,1:5])
#corrplot(L, method = 'number') 
View(L)

#vamos ver se é possível melhorar a correlação entre as variáveis preditoras
modelo <- lm (data=df_final,ener_consump ~ max_load_capac+perm_gross_weig+wheelbase+
                                  length+tire_size+eng_power)


step(modelo, direction="both", scale = 0.4434^2 )
#sugerido pelo step deixar somente perm_gross_weig+tire_size+eng_power

modelo_final <- lm(ener_consump ~ perm_gross_weig+tire_size+eng_power, treino)
summary(modelo_final)
#R2_modelo_final_treino = 0.8601 que é muito próximo do
# 0.8684 (modelo _v6 que tinha sido obtido com mais variáveis)


modelo_final_teste <- lm(ener_consump ~ perm_gross_weig+tire_size+eng_power, teste)
summary(modelo_final_teste)

#R2_modelo_final_teste = 0.7847

#modelo_final

