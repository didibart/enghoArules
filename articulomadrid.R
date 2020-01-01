#datos engho 2012
enghoGastos <- read.csv("ArticuloMadridAbril2020/bases_datos_engho2012/ENGHo-Gastos.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8")
enghoCodigosGastos <- read.csv("ArticuloMadridAbril2020/ENGHo_Codigos_de_bienes_y_servicios.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8")

View(enghoGastos)
View(enghoCodigosGastos)
View(enghoGastos1)
#division 1 alimentos y bebidas
#division 5 atención médica y gastos de salud 
library(sqldf)
enghoGastos1 <- sqldf("SELECT enghoGastos.MONTO, enghoCodigosGastos.DESCRIPCION 
                      FROM enghoGastos, enghoCodigosGastos 
                      WHERE enghoGastos.ARTICULO = enghoCodigosGastos.ARTICULO
                      AND enghoCodigosGastos.DIVISION = '500000'")

enghoGastos3 <- sqldf("SELECT enghoCodigosGastos.DESCRIPCION, (SUM(enghoGastos.MONTO) 
                / (SELECT COUNT(enghoGastos.MONTO) FROM enghoGastos)) * 100  
                FROM enghoGastos, enghoCodigosGastos
                WHERE enghoGastos.ARTICULO = enghoCodigosGastos.ARTICULO
                AND enghoGastos.DIVISION = '500000'
                GROUP BY enghoGastos.ARTICULO")

enghoGastos2 <- sqldf("SELECT enghoCodigosGastos.DESCRIPCION, enghoGastos.MONTO 
                FROM enghoGastos, enghoCodigosGastos
              WHERE enghoGastos.ARTICULO = enghoCodigosGastos.ARTICULO
              AND enghoGastos.DIVISION = '500000'")



write.csv(enghoGastos3, file = "gastosensaludEngho2012.csv")
View(enghoGastos3)
install.packages("arules")
library(arules)
tData <- as (enghoGastos, "transactions")
summary(enghoGastos)
48978.77 - 44050
