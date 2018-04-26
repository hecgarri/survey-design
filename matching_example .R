rm(list=ls())

if (!require(plyr)) install.packages("plyr"); require(plyr)
if (!require(wakefield)) install.packages("wakefield"); require(wakefield)
if (!require(MatchIt)) install.packages("MatchIt"); require(MatchIt)



set.seed(1234)

df = r_data_frame(n = 2500, id,age(x=30:78, name ='edad'), 
                           sex(x =c("Hombre", "Mujer"),
                               prob = c(.49,.51), 
                               name = "sexo"), 
                           race)
df$Sample <- as.factor('Patients')
a = summary(df)

set.seed(1234)
df.population <- r_data_frame(n = 10000,id, 
                              age(x = 18:80, 
                                  name = 'edad'), 
                              sex(x = c("Hombre", "Mujer"), 
                                  prob = c(0.50, 0.50), 
                                  name = "sexo"))
df.population$Sample <- as.factor('Population')


df2 = df[,c("ID","Race")]
df = df[,-c(4)]



b = summary(df.population)

mydata <- rbind.fill(df, df.population)


mydata$Group = as.logical(mydata$Sample == 'Patients')

mydata$Distress <- ifelse(mydata$sexo == 'hombre', age(nrow(mydata),
                                    x = 0:42, name = 'Distress'),
                          age(nrow(mydata), x = 15:42, name = 'Distress'))


set.seed(1234)
match.it <- matchit(Group ~ edad + sexo, data = mydata, method="nearest", ratio=1)
a <- summary(match.it)

emparejados = get_matches(match.it, mydata)