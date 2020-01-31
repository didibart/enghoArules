install.packages("arules")
#datos engho 2012
enghoGastos <- read.csv("ArticuloMadridAbril2020/bases_datos_engho2012/ENGHo-Gastos.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
enghoCodigosGastos <- read.csv("ArticuloMadridAbril2020/ENGHo_Codigos_de_bienes_y_servicios.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8")
enghoHogares <- read.csv("ArticuloMadridAbril2020/bases_datos_engho2012/ENGHo-Hogares.csv", header = TRUE, sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8", strip.white = TRUE, dec = ".")

View(enghoGastos)
View(enghoCodigosGastos)
View(enghoGastos1)
View(enghoHogares)
#division 1 alimentos y bebidas
#division 5 atención médica y gastos de salud 
library(sqldf)
library(Matrix)
library(ggplot2)
library(MASS)
library(dplyr)
library(stringr)
library(arules)
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
              AND enghoGastos.DIVISION = '100000'") 
enghoGastos3 <- sqldf("SELECT enghoCodigosGastos.DESCRIPCION 
                FROM enghoGastos, enghoCodigosGastos
                      WHERE enghoGastos.ARTICULO = enghoCodigosGastos.ARTICULO
                      AND enghoGastos.GRUPO = '120000'") 

enghoGastos4 <- sqldf("SELECT *
                FROM enghoGastos
                      WHERE enghoGastos.GRUPO = '120000'") 

enghoGastosAlimentos <- sqldf("SELECT
enghoGastos.CLASE,
enghoHogares.CV1B01_C,
enghoHogares.CH09,
enghoHogares.JSITOCUP,
enghoHogares.DECIPTHT,
enghoCodigosGastos.DESCRIPCION,
enghoGastos.CANTIDAD,
enghoGastos.MONTO
FROM enghoGastos, enghoCodigosGastos, enghoHogares
WHERE enghoGastos.DIVISION = enghoCodigosGastos.DIVISION
AND enghoGastos.GRUPO = enghoCodigosGastos.GRUPO
AND enghoGastos.CLASE = enghoCodigosGastos.CLASE
AND enghoGastos.SUBCLASE = enghoCodigosGastos.SUBCLASE
AND enghoGastos.ARTICULO = enghoCodigosGastos.ARTICULO
AND enghoGastos.REGION = enghoHogares.REGION
AND enghoGastos.SUBREGION = enghoHogares.SUBREGION
AND enghoGastos.PROVINCIA = enghoHogares.PROVINCIA
AND enghoGastos.CLAVE = enghoHogares.CLAVE
AND enghoGastos.DIVISION = '100000'")

nrow(enghoGastosAlimentos)
View(enghoGastosAlimentos)

#REGION, SUBREGION, PROVINCIA, CLAVE es la primary key
#comidas fuera del hogar 130000 GRUPO
#bebidas 120000 GRUPO 
#otros alimentos 119000 GRUPO (condimentos) 
#azucar 118000 GRUPO
#verduras 117000 GRUPO
#frutas 116000 GRUPO
#leches y huevos 115000 GRUPO
#aceites y grasas 114000 GRUPO
#pescados y marsicos 113000 GRUPO (le tengo fe)
#carnes y derivados 112000 GRUPO
#pan y cereales 111000 GRUPO 
#salud 500000 DIVISION
#alimentos 100000 DIVISION
#comidas fuera del hogar 131100 SUBCLASE
#conedores escolares 131200 SUBCLASE
#bebidas 120000 GRUPO

enghoAlimentos <- as.data.frame(enghoGastosAlimentos)
enghoAlimentos$RedCloacal <-enghoGastosAlimentos$CV1B01_C
enghoAlimentos$CombustibleCocina <-enghoGastosAlimentos$CH09
enghoAlimentos$OcupacionJefeHogar <-enghoGastosAlimentos$JSITOCUP
enghoAlimentos$DecilIngreso <-enghoGastosAlimentos$DECIPTHT
enghoAlimentos$DESCALIMENTO <-enghoGastosAlimentos$DESCRIPCION
enghoAlimentos$GrupoAlimento <-enghoGastosAlimentos$CLASE
enghoAlimentos$MONTO <-enghoGastosAlimentos$MONTO
enghoAlimentos$CANTIDAD <-enghoGastosAlimentos$CANTIDAD
nrow(enghoAlimentos)

#red cloacal
enghoAlimentos$RedCloacal[enghoAlimentos$RedCloacal == 1] <-"SI"
enghoAlimentos$RedCloacal[enghoAlimentos$RedCloacal == 2] <-"NO"
#combustible cocina
enghoAlimentos$CombustibleCocina[enghoAlimentos$CombustibleCocina == 1] <-"REDGAS"
enghoAlimentos$CombustibleCocina[enghoAlimentos$CombustibleCocina == 2] <-"ZEPPELINGAS"
enghoAlimentos$CombustibleCocina[enghoAlimentos$CombustibleCocina == 3] <-"TUBOGAS"
enghoAlimentos$CombustibleCocina[enghoAlimentos$CombustibleCocina == 4] <-"GARRAFAGAS"
enghoAlimentos$CombustibleCocina[enghoAlimentos$CombustibleCocina == 5] <-"ELECTRICIDAD"
enghoAlimentos$CombustibleCocina[enghoAlimentos$CombustibleCocina == 6] <-"LENIA"
enghoAlimentos$CombustibleCocina[enghoAlimentos$CombustibleCocina == 7] <-"OTROS"
#Ocupacion jefe de hogar
enghoAlimentos$OcupacionJefeHogar[enghoAlimentos$OcupacionJefeHogar == 1] <-"NOOCUPADO"
enghoAlimentos$OcupacionJefeHogar[enghoAlimentos$OcupacionJefeHogar == 2] <-"OCUPADOASALARIADO"
enghoAlimentos$OcupacionJefeHogar[enghoAlimentos$OcupacionJefeHogar == 3] <-"OCUPADOCUENTAPROPIA"
#decil de ingreso
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '1'] <-"1erDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '2'] <-"2doDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '3'] <-"3erDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '4'] <-"4toDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '5'] <-"5toDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '6'] <-"6toDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '7'] <-"7moDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '8'] <-"8voDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '9'] <-"9noDecil"
enghoAlimentos$DecilIngreso[enghoAlimentos$DecilIngreso == '10'] <-"10moDecil"
#clase de alimentos
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '111000'] <-"PanesYCereales"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '112000'] <-"Carnes"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '113000'] <-"Pescados"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '114000'] <-"AceitesYGrasas"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '115000'] <-"LechesYHuevos"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '116000'] <-"Frutas"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '117000'] <-"VerdurasTuberculosLegumbres"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '118000'] <-"AzucarYGolosinas"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '119000'] <-"CondimentosYSopas"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '121000'] <-"BebidasNoAlcoholicas"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '122000'] <-"BebidasAlcoholicas"
enghoAlimentos$GrupoAlimento[enghoAlimentos$GrupoAlimento == '131000'] <-"AlimentosFueraDelHogar"

#reseta la memoria
gc(reset = TRUE)
gc()
#resetea la memoria
#escribo el archivo
write.csv(enghoAlimentos, file = "ArticuloMadridAbril2020/datosengho2012Prep.csv", sep =";", col.names = TRUE)
#ARULES
# todos los datos deben ser categóricos
#disparo el arules
#leo el csv directamente
enghoGasRules <- read.csv(file ="ArticuloMadridAbril2020/datosengho2012Prep.csv", sep = ",", stringsAsFactors = TRUE)
enghoGasRules1 <- sqldf("SELECT DecilIngreso, GrupoAlimento FROM enghoGasRules")

trans3 <- as(enghoGasRules1, "transactions")
rules <- apriori(trans3, parameter = list(sup = 0.001, 
                                          conf = 0.08, target="rules",minlen=2))
#"lift", "confidence", "support"
inspect(head(rules, n = 5, by = "support"))
inspect(rules)
View(enghoGasRules1)
size(head(enghoGasRules1)) # number of items in each observation
LIST(head(enghoGasRules1, 3)) # convert 'transactions' to a list, note the LIST in CAPS
frequentItems <- eclat (enghoGasRules1, parameter = list(supp = 0.07, maxlen = 15)) # calculates support for frequent items
inspect(frequentItems)

#######
# Correlaciones
###########
lm1 <- lm(gastoXpercing$CANTPING~gastoXpercing$GC_1)
# e da un R^2 de casi 1, altísima correlación)
summary.lm(lm1)
all(is.nan(gastoXpercing$GC_1))
aov1 <-aov(as.numeric(GC_1)~as.integer(CANTPING),data = gastoXpercing,na.action=na.exclude)
summary.aov(aov1)
qplot(GC_1,CANTPING,data=gastoXpercing)

