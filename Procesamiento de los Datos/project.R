adult.data <- read.csv('./adult.data.csv', sep = ',', fill = F, strip.white = T)
colnames(adult.data) <- c('age', 'workclass', 'fnlwgt', 'education', 
                          'education_num', 'marital_status', 'occupation', 'relationship', 'race', 'sex', 
                          'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income')
Adult <- as.data.frame(unclass(adult.data), stringsAsFactors = TRUE)

factors <- sapply(Adult, is.factor)

# remove whitespcaes in Factor variables
Adult <- data.frame(cbind(sapply(Adult[,factors], trimws), Adult[,!factors]))
Adult <- as.data.frame(unclass(Adult), stringsAsFactors = TRUE)

# adjusting the income variable to binary target.
levels(Adult$income)[levels(Adult$income)=="<=50K."] <- "<=50K"
levels(Adult$income)[levels(Adult$income)==">50K."] <- ">50K"

# Checking the levels of each factor variable
#levels(Adult$income)[levels(Adult$income)=="<=50K"] <- "0"
#levels(Adult$income)[levels(Adult$income)==">50K"] <- "1"

# changing data type to numeric, factor -> numeric
#Adult$income <- as.numeric(Adult$income)-1

# re-levelling marital status factor
levels(Adult$marital_status)[levels(Adult$marital_status)=="Married-AF-spouse"] <- "Married"
levels(Adult$marital_status)[levels(Adult$marital_status)=="Married-civ-spouse"] <- "Married"
levels(Adult$marital_status)[levels(Adult$marital_status)=="Married-spouse-absent"] <- "Married"

#La variable 'fnlwgt' (significa peso final) 
#se elimina porque no tiene poder predictivo, ya que es una función destinada a asignar pesos similares 
#a personas con características demográficas similares. Se elimina 'Educación' ya que es solo una etiqueta
#en 'education_num' (número de años de educación). 

# deleting fnlwgt and education from dataframe
Adult$education <- NULL
Adult$fnlwgt <- NULL

#La 'ganancia de capital' y la 'pérdida de capital' se convierten en una variable de 'capital' que se 
#calcula restando la pérdida de capital de la ganancia de capital.

# creating 'capital' variable
Adult <- Adult %>% mutate(capital = capital_gain - capital_loss)
Adult$capital_gain <- NULL
Adult$capital_loss <- NULL

# No hay muchos individuos en cada categoría distinta de país de origen que no sea EE. UU., 
# por lo tanto, los agrupamos. Cambiar los niveles de 'País nativo' a 'EE. UU.' u 'Otro' aumentará el 
# poder predictivo de estos atributos al modelar.

# binning all other native countries
Adult<- Adult %>% mutate(native_country = ifelse(grepl("United.",native_country), "USA", "Other"))
Adult$native_country <- as.factor(Adult$native_country)

# check for infinite values
numerics <- sapply(Adult, is.numeric)
special <- function(x){
  if (is.numeric(x)) is.infinite(x)
}

#removing '?' in workclass and occupation
is.na(Adult) = Adult=='?'
is.na(Adult) = Adult==' ?'
Adult = na.omit(Adult)

#removing lost values
is.na(Adult$capital) = Adult$capital=='0'
Adult <- drop_na(Adult)
is.na(Adult$capital) = Adult$capital< 0
Adult <- drop_na(Adult)


# normalizing
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#Adult$age <- normalize(Adult$age)
#Adult$education_num <- normalize(Adult$education_num)
#Adult$hours_per_week <- normalize(Adult$hours_per_week)
#Adult$capital <- normalize(Adult$capital)

#metodo backwards
#primera iteración
#creando tabla
tabla <- data.frame(Adult$capital, Adult$hours_per_week, Adult$age)
#ganancias dependiente
#mes, vendedores, precio independientes
multi.fit <- lm(Adult$capital~ Adult$hours_per_week + Adult$age)
summary(multi.fit)
#analizando correlación
cor(tabla)
#supuestos
#la media de los errores es cero y la suma de los errores es cero
mean(multi.fit$residuals)
sum(multi.fit$residuals)
#errores normalmente distribuidos
shapiro.test(multi.fit$residuals)
#independencia de los residuos
dwtest(multi.fit)
#supuesto de Homocedasticidad
bptest(multi.fit)
#plots
layout(matrix(c(1,2,3,4),2,2,byrow = T))
plot(multi.fit$fitted.values,rstandard(multi.fit),main = "Residuos Estandarizdos",xlab = "Predicciones", ylab = "Residuos",ylim =  c(-2.5,2.5))
abline(h=0,lty = 2)
plot(Adult$capital, multi.fit$residuals,main = "Residuales vs Recuento",xlab = "Recuento",ylab = "Residuos")
abline(h = 0, lty = 2)
res <- multi.fit$residuals
hist(res, main = "Histograma de residuos")
qqnorm(multi.fit$residuals)
qqline(multi.fit$residuals)


