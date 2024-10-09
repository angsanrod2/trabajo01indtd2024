
##Problema 1 -------------------------------------------------------------------
#Aplicar los criterios de decisión bajo incertidumbre a los problemas cuya matriz 
#de valores numéricos viene dada en la tabla siguiente:

###### e1  ## e2  ## e3 |
-------------------------
  #d1: |  5  |  4   |  6 |
  #d2: |  2  |  3   |  1 |
  #d3: | -1  |  7   |  6 |
  #d4: |  5  |  2   |  0 |
-------------------------
  
source("teoriadecision_funciones_incertidumbre.R")

tb01 = crea.tablaX(c(5,4,6,
                     2,3,1,
                     -1,7,6,
                     5,2,0), numalternativas = 4, numestados = 3)
tb01 


##CASO FAVORABLE ##########################################################
### Criterio Pesimista (Wald) -------
s01_wald_fav = criterio.Wald(tb01,T)
s01_wald_fav 
names(s01_wald_fav$AlternativaOptima) #La alternativa óptima es la d1.

###Criterio Optimista -------
s01_optima_fav=criterio.Optimista(tb01,T)
s01_optima_fav
names(s01_optima_fav$AlternativaOptima) #La alternativa óptima es la d3

###Criterio de Hurwizc --------
s01_hurwicz_fav=criterio.Hurwicz(tb01,alfa=0.4,T)
s01_hurwicz_fav
names(s01_hurwicz_fav$AlternativaOptima) #La alternativa óptima es la d1

###Criterio de Savage --------
#Situacion favorable:
s01_savage_fav=criterio.Savage(tb01,T)
s01_savage_fav
names(s01_savage_fav$AlternativaOptima) #La alternativa óptima es d1

###Criterio de Laplace --------
s01_laplace_fav=criterio.Laplace(tb01,T)
s01_laplace_fav
names(s01_laplace_fav$AlternativaOptima) #La alternativa optima es d1 

###Criterio del Punto Ideal -------
s01_pid_fav=criterio.PuntoIdeal(tb01,T)
s01_pid_fav$AlternativaOptima #La alternativa optima es d1



##CASO DESFAVORABLE ############################################################
### Criterio Pesimista (Wald) -------
s01_wald_desf = criterio.Wald(tb01,F)
s01_wald_desf
names(s01_wald_desf$AlternativaOptima) #La alternativa óptima es la d2.

###Criterio Optimista -------
s01_optima_desf=criterio.Optimista(tb01,F)
s01_optima_desf
names(s01_optima_desf$AlternativaOptima) #La alternativa óptima es la d3

###Criterio de Hurwizc --------
s01_hurwicz_desf=criterio.Hurwicz(tb01,alfa=0.4,F)
s01_hurwicz_desf
names(s01_hurwicz_desf$AlternativaOptima) #La alternativa óptima es la d2

###Criterio de Savage --------
s01_savage_desf=criterio.Savage(tb01,F)
s01_savage_desf
names(s01_savage_desf$AlternativaOptima) #La alternativa óptima es d2

###Criterio de Laplace --------
s01_laplace_desf=criterio.Laplace(tb01,F)
s01_laplace_desf
names(s01_laplace_desf$AlternativaOptima) #La alternativa optima es d2

###Criterio del Punto Ideal -------
s01_pid_desf=criterio.PuntoIdeal(tb01,F)
s01_pid_desf
s01_pid_desf$AlternativaOptima  #La alternativa optima es d2


#CONCLUSION: para casos favorables, la mejor alternativa es d1,
#mientras que para casos desfavorables es d2.




##Problema 2 -------------------------------------------------------------------
#Una empresa de marketing digital está evaluando tres posibles estrategias publicitarias
#para una nueva campaña. Dependiendo de la reacción del mercado (positivo, moderado
#o negativo), los ingresos proyectados (en miles de euros) son los siguientes:
#Publicidad en redes sociales: 100, 50, 1
#Publicidad en televisión: 80, 60, 30
#Publicidad en radio: 60, 40, 20 

#¿Cuál sería la opción que usted le recomendaría a la empresa?

tb02=crea.tablaX(c(100,50,1,
                   80,60,30,
                   60,40,20),numalternativas = 3,numestados = 3)
tb02

sol02=criterio.Todos(tb02,alfa = 0.5,favorable = T)
sol02

#CONCLUSION: la opción recomendada a la empresa es d2