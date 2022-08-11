## Intervalo de confianza para muestras de una distribución normal

## Promedio = 100 y variando el tamaño de muestra y la desviación estándar.
# Simulación para intervalos del 95% de confianza

ni<-rep(100,6) #numero de nuestras a elegir
n<-c(10,10,10,50,50,50) #tamaño de cada una de las muestras
sigma<-c(5,10,15,5,10,15)
MED<-100
Simnorm<-list()
for (o in 1:length(n)){
  Simnorm[[o]]<-matrix(0,nrow=ni[o],ncol=3)
  colnames(Simnorm[[o]])<-rep(paste("n=",n[o],"Sig=",sigma[o]),3)
  for (d in 1:nrow(Simnorm[[o]])){
    Simnorm[[o]][d,1]<-mean(rnorm(n[o],MED,sigma[o]))
    Simnorm[[o]][d,2]<-Simnorm[[o]][d,1]-((qt(0.95,n[o]-1))*(sigma[o]/(sqrt(n[o]))))
    Simnorm[[o]][d,3]<-Simnorm[[o]][d,1]+((qt(0.95,n[o]-1))*(sigma[o]/(n[o]^0.5)))
  }
}
par(mfrow=c(2,3))
for(i in 1:length(Simnorm)){3
  dta<- Simnorm[[i]]
  plot(seq(min(unlist(Simnorm)),max(unlist(Simnorm)),l=100), seq(1:100), type="n", xlab=c(""), ylab=c(""),
       main=names(Simnorm[[i]][d,3]))
  segments(MED,1,MED,100)
  for (f in 1:nrow(dta)){
    if (MED>dta[f,2] & MED<dta[f,3]){
      segments(dta[f,3],f,dta[f,2],f, col="blue")
    }else (
      segments(dta[f,3],f,dta[f,2],f, col="red")
    )
    points(dta[f,1], f, pch=17, col="black", cex=0.5)
  }
}

# Intervalo de Confianza para la media de una Distribución Binomial
# Parámetros: numero de ensayos (n) y probabilidad de éxito (p)
# Simulación para intervalos del 95% de confianza


Simbin<-list() #### Un archivo lista donde se almacenará para cada muestra, el promedio, límite inferior y superior del intervalo
miu<-c() #### Promedio de la distribución original
sigma2<-c() #### Varianza de la variable original
sigma<-c() #### Desviación estándar de la variable original
media<-list()
a<-0.05 ## Nivel de significancia
qnorm(1-a/2) ### cuantil asociado a la confianza estipulada
Pe<-c(0.1,0.5,0.9,0.1,0.5,0.9) ## Probabilidades de éxito
r<-rep(8,6) ### Número de réplicas
ni<-rep(100,6) ### Numero de intervalos
n<-c(10,10,10,90,90,90) ## Tamaño de la muestra
for(y in 1:length(ni)){
  miu[y]<-r[y]*Pe[y] ### calcula miu para cada distribución
  sigma2[y]<-r[y]*Pe[y]*(1-Pe[y]) ### calcula sigma2 para cada distribución
  sigma[y]<-sqrt(sigma2[y]) ### calcula sigma para cada distribución
  Simbin[[y]]<-matrix(0,nrow=ni[y],ncol=3)
  for (d in 1:nrow(Simbin[[y]])){
    Simbin[[y]][d,1]<-mean(rbinom(n[y],r[y],Pe[y])) ### El promedio para el intervalo
    Simbin[[y]][d,2]<-Simbin[[y]][d,1]+(qnorm(1-a/2)*(sigma[y]/(sqrt(n[y])))) #### Limite superior del intervalo
    Simbin[[y]][d,3]<-Simbin[[y]][d,1]-(qnorm(1-a/2)*(sigma[y]/(n[y]^0.5))) #### Limite inferior del intervalo
  }
}
##### Creación de las graficas
par(mfrow=c(2,3))
for(i in 1:length(Simbin)){
  dta<- Simbin[[i]]
  plot(seq(min(c(dta[,3])),max(c(dta[,2])),l=100), seq(1:100), type="n", xlab=c(""), ylab=c(""))
  segments(miu[i],1,miu[i],100)
  for (f in 1:nrow(dta)){
    if (miu[i]>dta[f,3] & miu[i]<dta[f,2]){
      segments(dta[f,3],f,dta[f,2],f, col="blue")
    }else (
      segments(dta[f,3],f,dta[f,2],f, col="red")
    )
  }
}

