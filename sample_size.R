######################################################################
# Creación de directorio de variables y match de las bases de datos 
######################################################################

t = proc.time() # Inicia el cronómetro

rm(list=ls())

library(foreign) # Para importar conjuntos de datos en múltiples formatos 
library(dplyr) # Para manipular objetos de tipo data frame
library(survey) # Para trabajar muestras complejas 

#setwd("C:\\Users\\Usuario\\Google Drive\\OLR Ñuble - Observatorio laboral de Ñuble\\Bases de datos\\Encuesta de Caracterización Socioeconómica Nacional (CASEN)\\")
setwd("/home/hector/GoogleDriveUBB/OLR Ñuble - Observatorio laboral de Ñuble/Bases de datos/Encuesta de Caracterización Socioeconómica Nacional (CASEN)/")

list.files()

casen2015 = read.spss("Casen 2015 SPSS.sav",use.value.labels = FALSE,
                      to.data.frame= FALSE, use.missings = TRUE)

etiquetas.casen = attr(casen2015, "label.table")
directorio.casen = attr(casen2015,"variable.labels")
directorio.casen = data.frame(names(directorio.casen),directorio.casen)

casen2015 = data.frame(casen2015) 

casen2015 = casen2015 %>% filter(comuna==8401)

diseno = svydesign(id = ~varunit, strata = ~varstrat, weights = ~expr, nest = TRUE, data = casen2015)

pobreza = svyratio(~I(pobreza==1),denominator = ~I(zona==1),
                   design = diseno, deff = TRUE, na.rm = TRUE)

pobreza_freq = casen2015 %>% filter(pobreza==1) %>% count()



### Definición del tamaño muestral 

### calculo de viviendas por conglomerado
viviendas = casen2015 %>% group_by(varunit, id_vivienda) %>% count() 
segmentos = casen2015 %>% group_by(varunit) %>% count()

m_ = round(mean(viviendas$n),0)
n_ = nrow(segmentos)
 # Número de viviendas por manzana (supuesto, necesito el censo)
d_ef = deff(pobreza) # efecto del diseño 


v_p = sqrt(pobreza$var) # error estándar medición de la pobreza

Sp2 = ((n_*m_)/d_ef)*v_p %>% as.numeric()# cuasivarianza poblacional de la pobreza
  
z = qnorm(0.975) # Coeficiente de confianza 95% 

tnr = 0.175 # Tasa de no respuesta región del Biobío

e = 0.05

m0 = ((z^2)*Sp2)/(e^2) 

m1 = m0*d_ef

simp = m0/(1+(m0/18960*10)) # Si la muestra fuera simple

m2 = m1/(1+(m1/18960*10))

m3 = m2/(1-tnr)

simp2 = simp/(1-tnr)