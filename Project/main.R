#################################################################
########################### ANOVA ###############################
#################################################################

dosis <- c(rep('Control',12),rep('25mg',12),rep('50mg',12),rep('100mg',12),rep('125mg',12))
parasitos_en_peces <- c(50,65,72,46,38,29,70,85,72,40,57,59,
                        49,47,30,62,62,60,19,28,56,62,55,40,
                        20,59,64,61,28,47,29,41,60,57,61,38,
                        20,23,38,31,27,16,27,18,22,12,24,11,
                        18,30,22,26,31,11,15,12,31,36,16,13)

df <- data.frame(dosis,parasitos_en_peces)
boxplot(parasitos_en_peces~dosis,data=df)

#Hay diferencias entre los parasitos promedios en las distintas dosis

dosis.anova <- aov(parasitos_en_peces~dosis,data=df)
summary(dosis.anova)

#Como el p-value = 0, se rechaza H0 y se acepta que al menos un par de tratamientos o suministros de dosis 
#tienen un efecto de proporcion de parasitos promedio diferente en los distintos peces.

res <- dosis.anova$residuals
layout(matrix(c(1,2,3,4),2,2,byrow = T))
plot(dosis.anova$fitted.values,rstandard(dosis.anova),main = "Residuos Estandarizdos",xlab = "Predicciones", ylab = "Residuos",ylim =  c(-2.5,2.5))
abline(h=0,lty = 2)
hist(res, main = "Histograma de residuos")
qqnorm(dosis.anova$residuals)
qqline(dosis.anova$residuals)
#Se puede observar varianza constante, y algo de normalidad en los residuos. Comprobar los supuestos para mejor seguridad 

shapiro.test(res)
#El p-value es mayor que 0.05, no podemos rechazar la H0 por lo que los errores provienen de una distribucion normal

bartlett.test(res, df$dosis)
#El p-value es mayor que 0.05, por lo que podemos confirmar la homogeneidad de las varianzas (homocedasticidad)

dwtest(dosis.anova)
#El p-value es menor que 0.05, por lo que el test de independencia es significativo, los errores no son independientes



#################################################################
################# REDUCCION DE DIMENSIONES ######################
#################################################################

adult.data <- read.csv('./adult.data.csv', sep = ',', fill = F, strip.white = T)
adult.data$X <- NULL

Adult <- as.data.frame(unclass(adult.data), stringsAsFactors = TRUE)
factors <- sapply(Adult, is.factor)

#Removiendo los espacios en blanco de las variables factor
Adult <- data.frame(cbind(sapply(Adult[,factors], trimws), Adult[,!factors]))
Adult <- as.data.frame(unclass(Adult), stringsAsFactors = TRUE)

#Ajustando el income en binario para la dualidad de condicion
levels(Adult$income)[levels(Adult$income)=="<=50K."] <- "<=50K"
levels(Adult$income)[levels(Adult$income)==">50K."] <- ">50K"
levels(Adult$income)[levels(Adult$income)=="<=50K"] <- "0"
levels(Adult$income)[levels(Adult$income)==">50K"] <- "1"

#Cambiando el tipo de datos a numerico (factor -> numeric)
Adult$income <- as.numeric(Adult$income)-1

Adult <- as.data.frame(unclass(Adult), stringsAsNumeric = TRUE)
census_income <- data.frame(Adult$age,Adult$education_num,Adult$hours_per_week,Adult$capital,Adult$income)
colnames(census_income) <- c('age','education_num','hours_per_week','capital','income')
adult.data <- NULL

#plot de la tabla
ci <- cor(census_income)
ci
symnum(ci)
#No hay + * ni B por lo que nuestra matriz no esta altamente correlacionada
#Las variables son independientes por lo que este analisis es util para reducir dimension

acp <- prcomp(census_income,scale=TRUE)
summary(acp)
#De acuerdo al criterio de Kaiser necesitamos PC1, PC2, PC3 para justificar mas del 70% de los datos
#Estas son nuestras componentes principales
plot(acp)
acp$rotation

#biplot(acp)
#max/2 -> modulo de la columna

#PC1 toma personas en las cuales no interesa la edad para recibir mayor ingreso
#PC2 toma personas en las cuales importa las horas por semana trabajadas y afecta la edad que poseen
#PC3 toma personas en las cuales importa las horas por semana trabajadas y la edad no parecen ser
#    un problema, sin embargo la educacion o nivel de escolaridad que poseen si lo es



# normalizando con rango 0 a 1
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

census_income$age <- normalize(census_income$age)
census_income$education_num <- normalize(census_income$education_num)
census_income$hours_per_week <- normalize(census_income$hours_per_week)
census_income$capital <- normalize(census_income$capital)

#plot estandarizado
#Cluster Jerarquico Completo y Algoritmo KMEANS
d <- dist(census_income, method = "euclidean") #matriz de distancias con la distancia euclidiana
fit <- hclust(d, method = "complete") #ajuste completo
d2 <- as.dendrogram(fit)
#plot(fit)
#Dibuja rectangulos rojos alrededor de los 4 cluster
#rect.hclust(fit,k=3,border = "red")

fit.k = kmeans(census_income,3)
fit.k
#plot(census_income,col=fit.k$cluster)
plot(census_income$hours_per_week,census_income$capital,col = fit.k$cluster, lwd=2)


#Arbol de Clasificacion
#l = cardinalidad de la poblacion
l <- length(Adult[,1])

#sub = se escoge al azar las 2/3 de la poblacion
#      como conjunto entrenante para crear el arbol
sub <- sample(1:l,2*l/3)

#Conjunto entrenante
Adult[sub,]
#Conjunto de prueba
Adult[-sub,]

#Construyendo el arbol con el conjunto entrenante
Adult.tree <- rpart(income~.,data=Adult[sub,],cp=0.01,maxdepth=3)
summary(Adult.tree)
Adult.tree
plot(Adult.tree)
text(Adult.tree,use.n = TRUE,all = TRUE,pretty = 0,xpd = TRUE)

plotcp(Adult.tree)
printcp(Adult.tree)

#Haciendo prediccion con el conjunto de prueba
#teniendo en cuenta el arbol cart obtenido
Adult.pred <- predict(Adult.tree,newdata = Adult[-sub,],type = "vector")
#matriz de confusion
tb <- table(Adult.pred,Adult[-sub,]$income)
#Calculo del error del cart
error.rpart <- 1 - (sum(diag(tb))/sum(tb))
tb
error.rpart
