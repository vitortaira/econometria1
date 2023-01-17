library(tidyr)
library(dplyr)
library(readxl)
library(lubridate)
library("stringr", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library("stringi", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
planilha_covid_100 <- read_excel("Downloads/fgv/5 semestre/econometria 1/trabalho/planilha covid 100.xlsx")
#planilha formato numerico
num_covid1<-as.data.frame(planilha_covid_100)
num_covid1<-as.data.frame(group_by(num_covid1,codmun))

#excluindo cidades com menos de 30 observaçoes
num_dia<-as.data.frame(summarise(group_by(num_covid1,codmun),n()))
num_covid1<-as.data.frame(merge(num_covid1,num_dia))
num_covid<-as.data.frame(filter(num_covid1, num_covid1$`n()` >= 20))


y=matrix(num_covid$codmun)
x<-matrix(0,nrow=nrow(num_covid))
x[1,]=1
for(i in 2:nrow(num_covid))
{if(y[i,]!=y[i-1,])
{x[i,]=1}
}
num_covid<-as.data.frame(mutate(num_covid,mud=x))

z<-matrix(0,nrow=nrow(num_covid))
for(i in 1:nrow(num_covid))
{if(num_covid[i,21]==1)
{z[i+19,]=1}
}


num_covid<-as.data.frame(mutate(num_covid,mud1=z))

c<-num_covid$`dobra50/20`
z<-matrix(0,nrow=nrow(num_covid))
for(i in 1:nrow(num_covid))
{if(num_covid[i,22]==1)
{num_covid$`dobra50/20`[i]=(log(c[i]-c[i-19]))}
}

for(i in 1:nrow(num_covid))
{ if(num_covid$casosAcumulado[i]>=40 && num_covid$mud[i]==1)
{num_covid$mud1[i+19]=0} } 




num_covid<-as.data.frame(filter(num_covid,mud1==1))

View(num_covid)


##fusao dados radiacao com covid
global_horizontal_means_sedes.munic <- read.csv("~/Downloads/fgv/5 semestre/econometria 1/trabalho/global_horizontal_means_sedes-munic.txt", sep=";")
View(global_horizontal_means_sedes.munic)
insol<-as.data.frame(global_horizontal_means_sedes.munic)
insol<-insol[,c(2,3,4,6,10,11,12)]
names(insol) <- c("LON","LAT", "municipio","estado","radia_mar","radia_abril","radia_maio")

x<-insol
y<-matrix(0,nrow = nrow(x))
c<-x$estado

for(i in 1:nrow(x))
{ 
  if(str_detect(c[i],"RONDÔNIA"))
  {y[i,]=11}
  if(str_detect(c[i],"ACRE"))
  {y[i,]=12}
  if(str_detect(c[i],"AMAZONAS"))
  {y[i,]=13}
  if(str_detect(c[i],"RORAIMA"))
  {y[i,]=14}
  if(str_detect(c[i],"PARÁ"))
  {y[i,]=15}
  if(str_detect(c[i],"AMAPÁ"))
  {y[i,]=16}
  if(str_detect(c[i],"TOCANTINS"))
  {y[i,]=17}
  if(str_detect(c[i],"MARANHÃO"))
  {y[i,]=21}
  if(str_detect(c[i],"PIAUÍ"))
  {y[i,]=22}
  if(str_detect(c[i],"CEARÁ"))
  {y[i,]=23}
  if(str_detect(c[i],"RIO GRANDE DO NORTE"))
  {y[i,]=24}
  if(str_detect(c[i],"PARAÍBA"))
  {y[i,]=25}
  if(str_detect(c[i],"PERNAMBUCO"))
  {y[i,]=26}
  if(str_detect(c[i],"ALAGOAS"))
  {y[i,]=27}
  if(str_detect(c[i],"SERGIPE"))
  {y[i,]=28}
  if(str_detect(c[i],"BAHIA"))
  {y[i,]=29}
  if(str_detect(c[i],"MINAS GERAIS"))
  {y[i,]=31}
  if(str_detect(c[i],"ESPÍRITO SANTO"))
  {y[i,]=32}
  if(str_detect(c[i],"RIO DE JANEIRO"))
  {y[i,]=33}
  if(str_detect(c[i],"SÃO PAULO"))
  {y[i,]=35}
  if(str_detect(c[i],"PARANÁ"))
  {y[i,]=41}
  if(str_detect(c[i],"SANTA CATARINA"))
  {y[i,]=42}
  if(str_detect(c[i],"RIO GRANDE DO SUL"))
  {y[i,]=43}
  if(str_detect(c[i],"MATO GROSSO DO SUL"))
  {y[i,]=50}
  if(str_detect(c[i],"MATO GROSSO"))
  {y[i,]=51}
  if(str_detect(c[i],"GOIÁS"))
  {y[i,]=52}
  if(str_detect(c[i],"DISTRITO FEDERAL"))
  {y[i,]=53}
}
insol<-as.data.frame(mutate(x,coduf=y))
insol<-as.data.frame(insol[,-4])

base_covid<-as.data.frame(merge(num_covid,insol,by = intersect(num_covid$municipio,insol$NAME)))
base_covid<-as.data.frame(filter(base_covid,municipio.x==municipio.y & coduf.x==coduf.y))
View(base_covid)

base_covid1<-base_covid[,c(1,4,5,8,10,11,19,23,24,26,27,28)]###atencao

#fusao dados quarentena e base_cavid1
library(readxl)
quarentena <- read_excel("Downloads/fgv/5 semestre/econometria 1/trabalho/quarentena.xlsx", 
                         col_names = FALSE, col_types = c("numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "date", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric"))
isolamento<-as.data.frame(quarentena)
isolamento<-isolamento[,31:58]
linha<-isolamento[1,]
names(isolamento)<-isolamento[1,]
names(isolamento)[1]<-c("data")
isolamento<-isolamento[-1,]

x<-as.data.frame(merge(base_covid1,isolamento))
y<-matrix(0,nrow = nrow(x))
c<-x$coduf
as.numeric(x$coduf)

for(i in 1:nrow(x))
{ 
  if(c[i]==11)
  {y[i,]=x$`11`[i]}
  if(c[i]==12)
  {y[i]=x$`12`[i]}
  if(c[i]==13)
  {y[i]=x$`13`[i]}
  if(c[i]==14)
  {y[i]=x$`14`[i]}
  if(c[i]==15)
  {y[i]=x$`15`[i]}
  if(c[i]==16)
  {y[i]=x$`16`[i]}
  if(c[i]==17)
  {y[i]=x$`17`[i]}
  if(c[i]==21)
  {y[i]=x$`21`[i]}
  if(c[i]==22)
  {y[i]=x$`22`[i]}
  if(c[i]==23)
  {y[i]=x$`23`[i]}
  if(c[i]==24)
  {y[i]=x$`24`[i]}
  if(c[i]==25)
  {y[i]=x$`25`[i]}
  if(c[i]==26)
  {y[i]=x$`26`[i]}
  if(c[i]==27)
  {y[i]=x$`27`[i]}
  if(c[i]==28)
  {y[i]=x$`28`[i]}
  if(c[i]==29)
  {y[i]=x$`29`[i]}
  if(c[i]==31)
  {y[i]=x$`31`[i]}
  if(c[i]==32)
  {y[i]=x$`32`[i]}
  if(c[i]==33)
  {y[i]=x$`33`[i]}
  if(c[i]==35)
  {y[i]=x$`35`[i]}
  if(c[i]==41)
  {y[i]=x$`41`[i]}
  if(c[i]==42)
  {y[i]=x$`42`[i]}
  if(c[i]==43)
  {y[i]=x$`43`[i]}
  if(c[i]==55)
  {y[i]=x$`50`[i]}
  if(c[i]==51)
  {y[i]=x$`51`[i]}
  if(c[i]==52)
  {y[i]=x$`52`[i]}  
  if(c[i]==53)
  {y[i]=x$`53`[i]}
}

x<-as.data.frame(mutate(x,tax_isolamento=y))

base_covid1<-as.data.frame(x[,-c(13:39)]) ###atencao


View(x)
View(base_covid1)
View(x[,c(1,2,3,4,38)])


## fusao com denside demografica idh
library(readxl)
dados_popula_IBGE <- read_excel("Downloads/fgv/5 semestre/econometria 1/trabalho/dados popula IBGE.xlsx", 
                                col_types = c("text", "numeric", "numeric", 
                                              "numeric", "numeric", "numeric", 
                                              "numeric"))
dados_popula_IBGE<-dados_popula_IBGE[,2:6]
View(dados_popula_IBGE)

dados_popula_IBGE<-as.data.frame(dados_popula_IBGE)
base_covid1<-as.data.frame(merge(base_covid1,dados_popula_IBGE))
View(base_covid1)

### selecionando mes de insolaçao com mes da observaçao

z<-matrix(0,nrow=nrow(base_covid1))
y<-base_covid1[,2]###atençao
y<-as_date(y)
c<-base_covid1[,10:12]###atençao
for(i in 1:nrow(c))
{ if(month(y[i])==4 & day(y[i])>=15)
{z[i]=(c[i,1]+c[i,2])/2}
  if(month(y[i])==5 & day(y[i])<15)
  {z[i]=c[i,2]}
  if(month(y[i])==5 & day(y[i])>=15)
  {z[i]=(c[i,3]+c[i,2])/2}
  if(month(y[i])==6 & day(y[i])<15)
  {z[i]=c[i,3]}
}

base_covid1<-as.data.frame(mutate(base_covid1,radiacao=z))

#Extras
base_covid1$populacaoTCU2019<-as.numeric(base_covid1$populacaoTCU2019)
base_covid1$radiacao<-as.numeric(base_covid1$radiacao)
base_covid1$tax_isolamento<-as.numeric(base_covid1$tax_isolamento)
base_covid3<-as.data.frame(filter(base_covid1, coduf.x==31| coduf.x==32|coduf.x==33|coduf.x==35|coduf.x==41|coduf.x==42|coduf.x==43|coduf.x==50|coduf.x==51|coduf.x==52|coduf.x==53))
base_covid3<-as.data.frame(base_covid1[503:838,])
base_covid4<-as.data.frame(filter(base_covid1,base_covid1$coduf.x!=35))
base_covid4<-as.data.frame(merge(base_covid4,base_covidsp,all.x = TRUE,all.y = TRUE))

###Incluindo Voto por candidato(base_covid6)

votacao<-as.data.frame(votacao_por_candidato_TOP_1_)
x<-votacao
y<-matrix(0,nrow = nrow(x))
c<-x$UF

for(i in 1:nrow(x))
{ 
  if(str_detect(c[i],"RN"))
  {y[i,]=11}
  if(str_detect(c[i],"AC"))
  {y[i,]=12}
  if(str_detect(c[i],"AM"))
  {y[i,]=13}
  if(str_detect(c[i],"RR"))
  {y[i,]=14}
  if(str_detect(c[i],"PA"))
  {y[i,]=15}
  if(str_detect(c[i],"AP"))
  {y[i,]=16}
  if(str_detect(c[i],"TO"))
  {y[i,]=17}
  if(str_detect(c[i],"MA"))
  {y[i,]=21}
  if(str_detect(c[i],"PI"))
  {y[i,]=22}
  if(str_detect(c[i],"CE"))
  {y[i,]=23}
  if(str_detect(c[i],"RN"))
  {y[i,]=24}
  if(str_detect(c[i],"PB"))
  {y[i,]=25}
  if(str_detect(c[i],"PE"))
  {y[i,]=26}
  if(str_detect(c[i],"AL"))
  {y[i,]=27}
  if(str_detect(c[i],"SE"))
  {y[i,]=28}
  if(str_detect(c[i],"BA"))
  {y[i,]=29}
  if(str_detect(c[i],"MG"))
  {y[i,]=31}
  if(str_detect(c[i],"ES"))
  {y[i,]=32}
  if(str_detect(c[i],"RJ"))
  {y[i,]=33}
  if(str_detect(c[i],"SP"))
  {y[i,]=35}
  if(str_detect(c[i],"PR"))
  {y[i,]=41}
  if(str_detect(c[i],"SC"))
  {y[i,]=42}
  if(str_detect(c[i],"RS"))
  {y[i,]=43}
  if(str_detect(c[i],"MS"))
  {y[i,]=50}
  if(str_detect(c[i],"MT"))
  {y[i,]=51}
  if(str_detect(c[i],"GO"))
  {y[i,]=52}
  if(str_detect(c[i],"DF"))
  {y[i,]=53}
}
votacao<-as.data.frame(mutate(x,coduf=y))
votacao<-as.data.frame(votacao[,-1])

base_covid5<-as.data.frame(merge(base_covid1,votacao,all.x = TRUE,all.y = TRUE))
base_covid6<-as.data.frame(filter(base_covid5,coduf.x==coduf))
View(base_covid6)

####clima

###retirando acento e outros caracteries de municipios base_covid1(vira base_covid7)
### Início da Função para retirar os Acentos
# Rotinas e funções úteis V 1.0
# rm.accent - REMOVE ACENTOS DE PALAVRAS
# Função que tira todos os acentos e pontuações de um vetor de strings.
# Parâmetros:
# str - vetor de strings que terão seus acentos retirados.
# patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
#            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
#            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
#            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  pattern <- unique(pattern)
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  accentTypes <- c("´","`","^","~","¨","ç")
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  return(str)
}

municipio_sem_acento<-rm_accent(base_covid6$municipio.x)

base_covid7<-base_covid6
base_covid7[1]<-municipio_sem_acento

# conventional_weather_stations_inmet_brazil_1961_2019 <- read.csv("Downloads/fgv/5 semestre/econometria 1/trabalho/conventional_weather_stations_inmet_brazil_1961_2019.csv", sep=";")
tempo<-conventional_weather_stations_inmet_brazil_1961_2019[,c(1,2,3,4,7,8,9,14,17,18,20)]
tempo$Data<-dmy(tempo$Data)
x<-filter(tempo,year(tempo$Data)>=2010)
tempo<-x
tempo<-as.data.frame(group_by(tempo,Estacao))
x<-filter(tempo, !is.na(tempo$TempMaxima) & !is.na(tempo$Temp.Comp.Media))
tempo<-x
tempo<-filter(tempo, month(tempo$Data)==3 |month(tempo$Data)==4 | month(tempo$Data)==5  )
tempo_mar<-filter(tempo,month(tempo$Data)==3)
tempo_abr<-filter(tempo,month(tempo$Data)==4)
tempo_mai<-filter(tempo,month(tempo$Data)==5)
tempo_mar<-as.data.frame(summarise(group_by(tempo_mar,Estacao),temp.media.max.mar=mean(TempMaxima),temp.media.mar=mean(Temp.Comp.Media)))
tempo_abr<-as.data.frame(summarise(group_by(tempo_abr,Estacao),temp.media.max.abr=mean(TempMaxima),temp.media.abr=mean(Temp.Comp.Media)))
tempo_mai<-as.data.frame(summarise(group_by(tempo_mai,Estacao),temp.media.max.mai=mean(TempMaxima),temp.media.mai=mean(Temp.Comp.Media)))
tempo<-as.data.frame(merge(tempo_mar,tempo_abr))
tempo<-as.data.frame(merge(tempo,tempo_mai))

datasets_476825_892717_weather_stations_codes <- read.csv("~/Downloads/fgv/5 semestre/econometria 1/trabalho/1datasets_476825_892717_weather_stations_codes.csv", sep=";")
dicionario<-datasets_476825_892717_weather_stations_codes[,-c(7,8)]
tempo<-as.data.frame(merge(tempo,dicionario))

x<-tempo
y<-matrix(0,nrow = nrow(x))
c<-x$UF

for(i in 1:nrow(x))
{ 
  if(str_detect(c[i],"RN"))
  {y[i,]=11}
  if(str_detect(c[i],"AC"))
  {y[i,]=12}
  if(str_detect(c[i],"AM"))
  {y[i,]=13}
  if(str_detect(c[i],"RR"))
  {y[i,]=14}
  if(str_detect(c[i],"PA"))
  {y[i,]=15}
  if(str_detect(c[i],"AP"))
  {y[i,]=16}
  if(str_detect(c[i],"TO"))
  {y[i,]=17}
  if(str_detect(c[i],"MA"))
  {y[i,]=21}
  if(str_detect(c[i],"PI"))
  {y[i,]=22}
  if(str_detect(c[i],"CE"))
  {y[i,]=23}
  if(str_detect(c[i],"RN"))
  {y[i,]=24}
  if(str_detect(c[i],"PB"))
  {y[i,]=25}
  if(str_detect(c[i],"PE"))
  {y[i,]=26}
  if(str_detect(c[i],"AL"))
  {y[i,]=27}
  if(str_detect(c[i],"SE"))
  {y[i,]=28}
  if(str_detect(c[i],"BA"))
  {y[i,]=29}
  if(str_detect(c[i],"MG"))
  {y[i,]=31}
  if(str_detect(c[i],"ES"))
  {y[i,]=32}
  if(str_detect(c[i],"RJ"))
  {y[i,]=33}
  if(str_detect(c[i],"SP"))
  {y[i,]=35}
  if(str_detect(c[i],"PR"))
  {y[i,]=41}
  if(str_detect(c[i],"SC"))
  {y[i,]=42}
  if(str_detect(c[i],"RS"))
  {y[i,]=43}
  if(str_detect(c[i],"MS"))
  {y[i,]=50}
  if(str_detect(c[i],"MT"))
  {y[i,]=51}
  if(str_detect(c[i],"GO"))
  {y[i,]=52}
  if(str_detect(c[i],"DF"))
  {y[i,]=53}
}

tempo<-as.data.frame(mutate(x,coduf=y))

names(tempo)[8]<-c("municipio")
base_covid7<-as.data.frame(base_covid7[,-20])

x<-merge(base_covid7,tempo)
y<-matrix(0,nrow=nrow(x),ncol=1)
lat1<-x$Latitude
lat2<-x$LAT
lon1<-x$Longitude
lon2<-x$LON

for(i in 1:nrow(x))
{ if((lat1[i]-lat2[i])(lat1[i]-lat2[i]) +(lon1[i]-lon2[i])(lon1[i]-lon2[i])<=0.04)
{y[i]=1}
}
tempo<-as.data.frame(mutate(x,dist=y))
base_covid7<-as.data.frame(filter(tempo,municipio.x == municipio))

z<-matrix(0,nrow=nrow(base_covid7))
y<-base_covid7[,3]###atençao
y<-as_date(y)
c<-base_covid7[,21:26]###atençao
for(i in 1:nrow(c))
{ if(month(y[i])==4 & day(y[i])>=15)
{z[i]=(c[i,2]+c[i,4])/2}
  if(month(y[i])==5 & day(y[i])<15)
  {z[i]=c[i,4]}
  if(month(y[i])==5 & day(y[i])>=15)
  {z[i]=(c[i,4]+c[i,6])/2}
  if(month(y[i])==6 & day(y[i])<15)
  {z[i]=c[i,6]}
}

base_covid7<-as.data.frame(mutate(base_covid7,temperatura.media=z))

base_covid8<-as.data.frame(filter(tempo,dist==1))

z<-matrix(0,nrow=nrow(base_covid8))
y<-base_covid8[,3]###atençao
y<-as_date(y)
c<-base_covid8[,21:26]###atençao
for(i in 1:nrow(c))
{ if(month(y[i])==4 & day(y[i])>=15)
{z[i]=(c[i,2]+c[i,4])/2}
  if(month(y[i])==5 & day(y[i])<15)
  {z[i]=c[i,4]}
  if(month(y[i])==5 & day(y[i])>=15)
  {z[i]=(c[i,4]+c[i,6])/2}
  if(month(y[i])==6 & day(y[i])<15)
  {z[i]=c[i,6]}
}

base_covid8<-as.data.frame(mutate(base_covid8,temperatura.media=z))
base_covid9<-na.omit(base_covid8)
View(base_covid9)
base_covid8<-base_covid9

#z<-matrix(0,nrow=nrow(base_covid8))
#for(i in 1:nrow(base_covid8))
#{ z[i]=base_covid8$casosAcumulado[i]-exp(base_covid8$`dobra50/20`[i])
#}
#base_covid8<-as.data.frame(mutate(base_covid8,casos_acumulados_inicio =z))

### Resultados
TRANSMISSÃO <- base_covid8$`dobra50/20`
RADIAÇÃO <- base_covid8$radiacao
TEMPERATURA <-base_covid8$temperatura.media
IDHM <- base_covid8$`IDHM <span>Índice de desenvolvimento humano municipal</span> [2010]`
DENSIDADE <- base_covid8$`Densidade demográfica - hab/km² [2010]`
ISOLAMENTO <- base_covid8$tax_isolamento
BOLSONARO <- base_covid8$`votos válidos`

modelo1 <- lm(TRANSMISSÃO ~ RADIAÇÃO)
modelo2 <- lm(TRANSMISSÃO ~ RADIAÇÃO +  TEMPERATURA + IDHM + DENSIDADE)
modelo3 <- lm(TRANSMISSÃO ~ RADIAÇÃO + TEMPERATURA +  IDHM + DENSIDADE + ISOLAMENTO)
mle <- glm(TRANSMISSÃO ~ RADIAÇÃO + TEMPERATURA + IDHM + DENSIDADE + ISOLAMENTO)

library(stargazer)
stargazer(modelo1, modelo2, modelo3, type = "text", keep.stat = c("n", "rsq"))

b.INT <- summary(modelo3)$coefficient[1]
b.RAD <- summary(modelo3)$coefficient[2]
b.TEMP <- summary(modelo3)$coefficient[3]
b.IDHM <- summary(modelo3)$coefficient[4]
b.DENS <- summary(modelo3)$coefficient[5]
b.ISOL <- summary(modelo3)$coefficient[6]

RAD.med <- mean(RADIAÇÃO)
TEMP.med <- mean(TEMPERATURA)
IDHM.med <- mean(IDHM)
DENS.med <- mean(DENSIDADE)
ISOL.med <- mean(ISOLAMENTO)
TRANS.med <- mean(TRANSMISSÃO)
BOLSO.med <- mean(BOLSONARO)

library(lmtest)
#TESTE DE BREUSCH-PAGAN LM-TEST
significancia <- 0.05
critico <- qchisq(1-significancia, df = modelo3$rank-1) #Essa definição de df assume que todos os regressores explicam a variância.
estatistica <- bptest(modelo3)$statistic
if("estatistica">"critico") {print("Como a estatística de teste é maior que o valor crítico, rejeitamos a H0 de homocedasticidade dos erros")} else {print("Como a estatística de teste é menor que o valor crítico, não rejeitamos a H0 de homocedasticidade dos erros")}

#GRÁFICO RESÍDUOS X VALORES PREDITOS (MODELO 3)
plot(x=modelo3$fitted.values, y=modelo3$residuals, main="RESÍDUOS X VALORES PREDITOS (MODELO 3)", sub="Avaliado na média dos demais regressores", xlab="VALORES PREDITOS", ylab="RESÍDUOS")
abline(0,0, col="blue")

#GRÁFICO TRANSMISSÃO X RADIAÇÃO (MODELO3)
plot(x=RADIAÇÃO, y=TRANSMISSÃO, main="TRANSMISSÃO X RADIAÇÃO (MODELO 3)", sub="Avaliado na média dos demais regressores", xlab="RADIAÇÃO", ylab="TRANSMISSÃO")
abline(b.INT+b.TEMP*TEMP.med+b.IDHM*IDHM.med+b.DENS*DENS.med+b.ISOL*ISOL.med, b.RAD,col="red")

var(summary(modelo2)$residuals)
var(summary(modelo3)$residuals)

#SEM
library(systemfit)
eq1 <- TRANSMISSÃO ~ RADIAÇÃO  + TEMPERATURA + IDHM + DENSIDADE + ISOLAMENTO 
eq2 <- ISOLAMENTO ~ TRANSMISSÃO + IDHM + DENSIDADE + BOLSONARO
eq.sis<-list(eq1,eq2)
instrumentos <- ~ RADIAÇÃO + TEMPERATURA + IDHM + DENSIDADE + BOLSONARO 

#2sls
sem2 <- systemfit(eq.sis,ins=instrumentos,data=base_covid8,method="2SLS")

b.INT.2.1 <- summary(sem2)$coefficients[1]
b.RAD.2.1 <- summary(sem2)$coefficients[2]
b.TEMP.2.1 <- summary(sem2)$coefficients[3]
b.IDHM.2.1 <- summary(sem2)$coefficients[4]
b.DENS.2.1 <- summary(sem2)$coefficients[5]
b.ISOL.2.1 <- summary(sem2)$coefficients[6]

b.INT.2.2 <- summary(sem2)$coefficients[7]
b.TRANS.2.2 <- summary(sem2)$coefficients[8]
b.IDHM.2.2 <- summary(sem2)$coefficients[9]
b.BOLSO.2.2 <- summary(sem2)$coefficients[10]

library(lmtest)
#TESTE DE BREUSCH-PAGAN LM-TEST
significancia <- 0.05
critico <- qchisq(1-significancia, df = sem2[1]$rank-1) #Essa definição de df assume que todos os regressores explicam a variância.
estatistica <- bptest(sem2[1])$statistic
if("estatistica">"critico") {print("Como a estatística de teste é maior que o valor crítico, rejeitamos a H0 de homocedasticidade dos erros")} else {print("Como a estatística de teste é menor que o valor crítico, não rejeitamos a H0 de homocedasticidade dos erros")}

#GRÁFICO TRANSMISSÃO X RADIAÇÃO (SEM - 2SLS)
plot(x=RADIAÇÃO, y=TRANSMISSÃO, main="TRANSMISSÃO X RADIAÇÃO (SEM - 2SLS)", sub="Avaliado na média dos demais regressores", xlab="RADIAÇÃO", ylab="TRANSMISSÃO")
abline(b.INT.2.1+b.TEMP.2.1*TEMP.med+b.IDHM.2.1*IDHM.med+b.DENS.2.1*DENS.med+b.ISOL.2.1*ISOL.med ,b.RAD.2.1 , col="red")

#GRÁFICO ISOLAMENTO X TRANSMISSÃO (SEM - 2SLS)
plot(x=TRANSMISSÃO, y=ISOLAMENTO, main="ISOLAMENTO X TRANSMISSÃO (SEM - 2SLS)", sub="Avaliado na média dos demais regressores", xlab="TRANSMISSÃO", ylab="ISOLAMENTO")
abline(b.INT.2.2+b.IDHM.2.2*IDHM.med+b.BOLSO.2.2*BOLSO.med ,b.TRANS.2.2 , col="red")

#3sls
sem3 <- systemfit(eq.sis, ins=instrumentos, data=base_covid8, method="3SLS")

b.INT.3.1 <- summary(sem3)$coefficients[1]
b.RAD.3.1 <- summary(sem3)$coefficients[2]
b.TEMP.3.1 <- summary(sem3)$coefficients[3]
b.IDHM.3.1 <- summary(sem3)$coefficients[4]
b.DENS.3.1 <- summary(sem3)$coefficients[5]
b.ISOL.3.1 <- summary(sem3)$coefficients[6]

b.INT.3.2 <- summary(sem3)$coefficients[7]
b.TRANS.3.2 <- summary(sem3)$coefficients[8]
b.IDHM.3.2 <- summary(sem3)$coefficients[9]
b.BOLSO.3.2 <- summary(sem3)$coefficients[10]

#GRÁFICO TRANSMISSÃO X RADIAÇÃO (SEM - 3SLS)
plot(x=RADIAÇÃO, y=TRANSMISSÃO, main="TRANSMISSÃO X RADIAÇÃO (SEM - 3SLS)", sub="Avaliado na média dos demais regressores", xlab="RADIAÇÃO", ylab="TRANSMISSÃO")
abline(b.INT.3.1+b.TEMP.3.1*TEMP.med+b.IDHM.3.1*IDHM.med+b.DENS.3.1*DENS.med+b.ISOL.3.1*ISOL.med ,b.RAD.3.1 , col="red")

#GRÁFICO ISOLAMENTO X TRANSMISSÃO (SEM - 3SLS)
plot(x=base_covid11$y_fitted, y=base_covid11$resíduos, main="RESÍDUOS X VALORES PREDITOS (SEM - 3SLS)", xlab="RESÍDUOS", ylab="VALORES PREDITOS")
abline(0, 0, col="blue")


####PLOT SEM 2SLS EQ 1
base_covid11<-base_covid8[,c(7,13,18,19,14,16,34)]
y_hat<-matrix(0,nrow = nrow(base_covid8))
for(i in 1:nrow(y_hat))
{y_hat[i]= b.INT.2.1 + b.RAD.2.1*base_covid11$radiacao[i] + b.TEMP.2.1*base_covid11$temperatura.media[i] + b.IDHM.2.1*base_covid11$`IDHM <span>Índice de desenvolvimento humano municipal</span> [2010]`[i] + b.DENS.2.1*base_covid11$`Densidade demográfica - hab/km² [2010]`[i] + b.ISOL.2.1*base_covid11$tax_isolamento[i]
}

e<-matrix(0,nrow = nrow(base_covid11))
for(i in 1:nrow(e))
{e[i]=base_covid11$`dobra50/20`[i]-y_hat[i]
}
base_covid11<-mutate(base_covid11,y_fitted=y_hat)
base_covid11<-mutate(base_covid11,resido=e)
base_covid11<-base_covid11[,c(8,9)]
names(base_covid11)[2]<-c("RESÍDUOS 3SLS.1")

plot(x=base_covid11$y_fitted, y=base_covid11$`RESÍDUOS 3SLS.1`, main="RESÍDUOS X VALORES PREDITOS (SEM-2SLS)", xlab = "VALORES PREDITOS", ylab = "RESÍDUOS")
abline(0,0,col="blue")

####PLOT SEM 2SLS EQ 2
base_covid11<-base_covid8[,c(7,13,18,19,14,16,34)]
y_hat<-matrix(0,nrow = nrow(base_covid11))
for(i in 1:nrow(y_hat))
{y_hat[i]= b.INT.2.2 + b.TRANS.2.2*base_covid11$`dobra50/20`[i] + b.IDHM.2.2*base_covid11$`IDHM <span>Índice de desenvolvimento humano municipal</span> [2010]`[i] + b.BOLSO.2.2*base_covid11$`votos válidos`[i]
}

e<-matrix(0,nrow = nrow(base_covid8))
for(i in 1:nrow(e))
{e[i]=base_covid11$tax_isolamento[i] - y_hat[i]
}
base_covid11<-mutate(base_covid11,y_fitted=y_hat)
base_covid11<-mutate(base_covid11,resido=e)
base_covid11<-base_covid11[,c(8,9)]
names(base_covid11)[2]<-c("RESÍDUOS 3SLS.2")

###### 
base_covid12<-base_covid8[,c(7,13,18,19,14,16,34)]
names(base_covid12)<-c("TRANSMISSAO","ISOLAMENTO", "RADIAÇÃO", "BOLSONARO","DENSIDADE","IDHM","TEMPERATURA")