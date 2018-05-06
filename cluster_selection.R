###############################################################
# Selección de conglomerados y número de viviendas
###############################################################

rm(list=ls())

if (!require(foreign)) install.packages("foreign"); require(foreign) # Para importar conjuntos de datos en múltiples formatos 
if (!require(dplyr)) install.packages("dplyr"); require(dplyr) # Para manipular objetos de tipo data frame
if (!require(survey)) install.packages("survey"); require(survey) # Para trabajar muestras complejas 

path = file.path("/home/hector/GoogleDriveUBB",
                 "OLR Ñuble - Observatorio laboral de Ñuble",
                  "Bases de datos/Censo 2002",
                 "portafolio.sav")

portafolio = read.spss(path,use.value.labels = FALSE,
                      to.data.frame= TRUE, use.missings = TRUE)

portafolio = portafolio %>% filter(Comuna == 8401, Area ==1)

path1 = file.path("/home/hector/GoogleDriveUBB",
                 "OLR Ñuble - Observatorio laboral de Ñuble",
                 "Bases de datos/Censo 2002",
                 "vivienda.sav")

vivienda = read.spss(path1,use.value.labels = FALSE,
                     to.data.frame= TRUE, use.missings = TRUE)
vivienda = vivienda %>% rename(Portafolios = Portafolio)
datos = left_join(portafolio, vivienda)

viviendas_manzana = datos %>% group_by(Portafolios) %>% count()

datos = left_join(datos, viviendas_manzana) %>% rename(total_viviendas = n)

datos = datos %>%
   mutate(grupo = ifelse(total_viviendas>=0 & total_viviendas<=7,0,
                  ifelse(total_viviendas>=8 & total_viviendas<=9,1,
                  ifelse(total_viviendas>=10 & total_viviendas<=13,2,
                  ifelse(total_viviendas>=14 & total_viviendas<=17,4,
                  ifelse(total_viviendas>=18 & total_viviendas<=23,5,
                  ifelse(total_viviendas>=24 & total_viviendas<=25,6,
                  ifelse(total_viviendas>=26 & total_viviendas<=29,7,
                  ifelse(total_viviendas>=30 & total_viviendas<=33,8,
                  ifelse(total_viviendas>=34 & total_viviendas<=37,9,
                  ifelse(total_viviendas>=38 & total_viviendas<=44,10,
                  ifelse(total_viviendas==45,11,
                  ifelse(total_viviendas>=46 & total_viviendas<=49,12,
                  ifelse(total_viviendas>=50 & total_viviendas<=53,13,
                  ifelse(total_viviendas>=54 & total_viviendas<=57,14,
                  ifelse(total_viviendas>=58 & total_viviendas<=61,15,
                  ifelse(total_viviendas>=62 & total_viviendas<=65,16,
                  ifelse(total_viviendas>=66 & total_viviendas<=69,17,
                  ifelse(total_viviendas>=70 & total_viviendas<=73,18,NA)))))))))))))))))))

viviendas_grupo = datos %>% group_by(grupo) %>% count() %>% rename(M = n)

datos = left_join(datos, viviendas_grupo)

manzanas_grupo = datos %>% group_by(grupo,Portafolios) %>%
  count() %>% group_by(grupo) %>% count() %>% rename(N = nn)

datos = left_join(datos, manzanas_grupo)

## Marco de manzanas 
manzanas = datos %>% filter(!duplicated(Portafolios)) %>% 
           mutate(W = M*N)

grupos = manzanas %>% filter(!duplicated(grupo)) %>%
  select(grupo, M,N,W) %>% mutate(P = W/sum(W)) %>% filter(grupo!=0)

manzanas = left_join(manzanas, grupos) %>% filter(grupo!=0)

set.seed(1234)

muestra = 80
grupo = NULL
total = NULL
manzana = NULL
conjunto = NULL
total = 0
i = 1
while(total < muestra){
grupo[i] = grupos[sample(nrow(grupos),size = 1,prob = grupos$P),c("grupo")]
conjunto[[i]] = manzanas[manzanas$grupo==grupo[i],] %>% sample_n(size=1)
muestra[i] = conjunto[[i]]$total_viviendas
total = sum(muestra[i])
i=i+1
print(total)
}


repeat{
  i = i+1
}