#datos engho 2012
enghoGastos <- read.csv("ArticuloMadridAbril2020/bases_datos_engho2012/ENGHo-Gastos.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8")
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

enghoGastos5 <- sqldf("SELECT
enghoHogares.CV1B01_C,
enghoHogares.CH09,
enghoHogares.JSITOCUP,
enghoHogares.DECIPTHT,
enghoCodigosGastos.DESCRIPCION
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
AND enghoGastos.GRUPO IN ('120000', '119000', '118000', '117000',
                      '116000', '115000', '114000', '113000',
                      '112000', '111000') ")

nrow(enghoGastos5)
nrow(enghoGastos)
View(enghoHogGasRules)

pa <- is.null(enghoHogGasRules$TipoDeVivienda)
pa1 <- enghoGastos5[enghoGastos5$JSITOCUP]
pa <- enghoGastos5$JSITOCUP!=1 & enghoGastos5$JSITOCUP!=2 & enghoGastos5$JSITOCUP!=3
is.numeric(enghoHogares$JSITOCUP)
which(pa)

enghoHogGasRules <- as.data.frame(enghoGastos5)
enghoHogGasRules$RedCloacal <- as.factor(enghoHogGasRules$RedCloacal)
enghoHogGasRules$CombustibleCocina <- as.factor(enghoHogGasRules$CombustibleCocina)
enghoHogGasRules$OcupacionJefeHogar <- as.factor(enghoHogGasRules$OcupacionJefeHogar)
enghoHogGasRules$DecilIngresor <- as.factor(enghoHogGasRules$DecilIngreso)
enghoHogGasRules$DESCALIMENTO <- as.factor(enghoHogGasRules$DESCALIMENTO)

enghoHogGasRules$RedCloacal <-enghoGastos5$CV1B01_C
enghoHogGasRules$CombustibleCocina <-enghoGastos5$CH09
enghoHogGasRules$OcupacionJefeHogar <-enghoGastos5$JSITOCUP
enghoHogGasRules$DecilIngreso <-enghoGastos5$DECIPTHT
enghoHogGasRules$DESCALIMENTO <-enghoGastos5$DESCRIPCION
#reseta la memoria
gc(reset = TRUE)
gc()
enghoHogGasRules$RedCloacal
#red cloacal
enghoHogGasRules$RedCloacal[enghoHogGasRules$RedCloacal == 1] <-"SI"
enghoHogGasRules$RedCloacal[enghoHogGasRules$RedCloacal == 2] <-"NO"
#combustible cocina
enghoHogGasRules$CombustibleCocina[enghoHogGasRules$CombustibleCocina == 1] <-"REDGAS"
enghoHogGasRules$CombustibleCocina[enghoHogGasRules$CombustibleCocina == 2] <-"ZEPPELINGAS"
enghoHogGasRules$CombustibleCocina[enghoHogGasRules$CombustibleCocina == 3] <-"TUBOGAS"
enghoHogGasRules$CombustibleCocina[enghoHogGasRules$CombustibleCocina == 4] <-"GARRAFAGAS"
enghoHogGasRules$CombustibleCocina[enghoHogGasRules$CombustibleCocina == 5] <-"ELECTRICIDAD"
enghoHogGasRules$CombustibleCocina[enghoHogGasRules$CombustibleCocina == 6] <-"LENIA"
enghoHogGasRules$CombustibleCocina[enghoHogGasRules$CombustibleCocina == 7] <-"OTROS"
#Ocupacion jefe de hogar
enghoHogGasRules$OcupacionJefeHogar[enghoHogGasRules$OcupacionJefeHogar == 1] <-"NOOCUPADO"
enghoHogGasRules$OcupacionJefeHogar[enghoHogGasRules$OcupacionJefeHogar == 2] <-"OCUPADOASALARIADO"
enghoHogGasRules$OcupacionJefeHogar[enghoHogGasRules$OcupacionJefeHogar == 3] <-"OCUPADOCUENTAPROPIA"
#decil de ingreso
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '1'] <-"1erDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '2'] <-"2doDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '3'] <-"3erDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '4'] <-"4toDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '5'] <-"5toDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '6'] <-"6toDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '7'] <-"7moDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '8'] <-"8voDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '9'] <-"9noDecil"
enghoHogGasRules$DecilIngreso[enghoHogGasRules$DecilIngreso == '10'] <-"10moDecil"

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
gc()
gc(reset = TRUE)
colnames(enghoHogGasRules)
nrow(enghoHogGasRules)
head(enghoHogGasRules)
enghoHogGasRules$CombustibleCocina <-as.factor(enghoHogGasRules$CombustibleCocina)
enghoHogGasRules$DecilIngreso <-as.factor(enghoHogGasRules$DecilIngreso)
write.csv(enghoHogGasRules, file = "ArticuloMadridAbril2020/datosengho2012Prep.csv", sep =";", col.names = TRUE)
#disparo el arules
#elimino las columnas que no necesito
enghoHogGasRules$CombustibleCocina <-NULL
pa <- is.null(enghoHogGasRules$DESCALIMENTO)
which(pa)
trans3 <- as(enghoHogGasRules, "transactions")
rules <- apriori(trans3, parameter = list(sup = 0.1, 
                                          conf = 0.1, target="rules",minlen=1))

summary(rules)
inspect(head(rules, n = 10, by = "lift"))
inspect(rules)
View(enghoGastos5)
dim(enghoGastos2)
write.csv(enghoGastos3, file = "gastosensaludEngho2012.csv")
View(enghoGastos3)
install.packages("arules")


#######
# todos los datos deben ser categóricos
# gastos.articulo
# hogares.CV1B01_C (red cloacal) (1 = si, 2 = no)
# hogares.CV1B02 (tipo de vivienda) (1 Casa 2 Rancho 3 Casilla 4 Departamento 5 Pieza en inquilinato 6 Pieza en hotel familiar o pensión 7 Local construído para habitación 8 Otros (especificar)
#)
# hogares.CH09 (combustible para cocinar) (1 ...gas de red? 2 ...gas a granel (zeppelin)? 3 ...gas a tubo? 4 ...gas en garrafa? 5 ...electricidad? 6 ...leña o carbón? 7 otro (especificar)
#)
# hogares.JSITOCUP (situación ocupacional del jefe de hogar)1 No ocupado 2 Ocupado asalariado (incluye trabajador familiar sin pago) 3 Ocupado cuenta propia o patrón
# hogares.GC_1 (gasto en alimentos del hogar)
# hogares.QUIGAPHT (quintil de gastos por país)1 Primer quintil 2 Segundo quintil 3 Tercer quintil 4 Cuarto quintil 5 Quinto quintil
# hogares.DECIPTHT (decil de ingreso por hogar por país)
# regiones.CRGN (codigo de region 1 gba, 2 pamp, 3 noroeste, 4 noreste, 5 cuyo, 6 patagonia)

#lo primero que hago es poner las etiquetas en estas variables seleccionadas
enghoHogRules <- as.data.frame(enghoHogares)
is.data.frame(enghoHogRules)
enghoHogRules$RedCloacal <-enghoHogares$CV1B01_C
enghoHogRules$TipoDeVivienda <-enghoHogares$CV1B02
enghoHogRules$CombustibleCocina <-enghoHogares$CH09
enghoHogRules$OcupacionJefeHogar <-enghoHogares$JSITOCUP
enghoHogRules$GastoAlimentos <-enghoHogares$GC_1
enghoHogRules$DecilIngreso <-enghoHogares$DECIPTHT
# elimino las columnas restantes
colnames(enghoHogRules[1:119])
enghoHogRules[1:119] <- list(NULL)
colnames(enghoHogRules)
#me fijo si son factors
class(enghoHogRules$DecilIngreso)
#convierto a factor
enghoHogRules$RedCloacal <- as.factor(enghoHogRules$RedCloacal)
enghoHogRules$TipoDeVivienda <- as.factor(enghoHogRules$TipoDeVivienda)
enghoHogRules$CombustibleCocina <- as.factor(enghoHogRules$CombustibleCocina)
enghoHogRules$OcupacionJefeHogar <- as.factor(enghoHogRules$OcupacionJefeHogar)
enghoHogRules$GastoAlimentos <- as.numeric(enghoHogRules$GastoAlimentos)
colnames(enghoHogRules[5])
enghoHogRules[5] <- list(NULL)
#vuelo tipo de vivienda, es siempre casa
enghoHogRules[2] <- list(NULL)
enghoHogRules$DecilIngreso <- as.factor(enghoHogRules$DecilIngreso)
# hago categorias del gasto en alimentos

#red cloacal
enghoHogRules$RedCloacal[enghoHogRules$RedCloacal == 1] <-"SI"
enghoHogRules$RedCloacal[enghoHogRules$RedCloacal == 2] <-"NO"
#tipo de vivienda
enghoHogRules$TipoDeVivienda[enghoHogRules$TipoDeVivienda == 1] <-"CASA"
enghoHogRules$TipoDeVivienda[enghoHogRules$TipoDeVivienda == 2] <-"RANCHO"
enghoHogRules$TipoDeVivienda[enghoHogRules$TipoDeVivienda == 3] <-"CASILLA"
enghoHogRules$TipoDeVivienda[enghoHogRules$TipoDeVivienda == 4] <-"DEPARTAMENTO"
enghoHogRules$TipoDeVivienda[enghoHogRules$TipoDeVivienda == 5] <-"PIEZAINQUILINATO"
enghoHogRules$TipoDeVivienda[enghoHogRules$TipoDeVivienda == 6] <-"PIEZAHOTEL"
enghoHogRules$TipoDeVivienda[enghoHogRules$TipoDeVivienda == 7] <-"LOCAL"
enghoHogRules$TipoDeVivienda[enghoHogRules$TipoDeVivienda == 8] <-"OTROS"
#combustible cocina
enghoHogRules$CombustibleCocina[enghoHogRules$CombustibleCocina == 1] <-"REDGAS"
enghoHogRules$CombustibleCocina[enghoHogRules$CombustibleCocina == 2] <-"ZEPPELINGAS"
enghoHogRules$CombustibleCocina[enghoHogRules$CombustibleCocina == 3] <-"TUBOGAS"
enghoHogRules$CombustibleCocina[enghoHogRules$CombustibleCocina == 4] <-"GARRAFAGAS"
enghoHogRules$CombustibleCocina[enghoHogRules$CombustibleCocina == 5] <-"ELECTRICIDAD"
enghoHogRules$CombustibleCocina[enghoHogRules$CombustibleCocina == 6] <-"LENIA"
enghoHogRules$CombustibleCocina[enghoHogRules$CombustibleCocina == 7] <-"OTROS"
#Ocupacion jefe de hogar
enghoHogRules$OcupacionJefeHogar[enghoHogRules$OcupacionJefeHogar == 1] <-"NOOCUPADO"
enghoHogRules$OcupacionJefeHogar[enghoHogRules$OcupacionJefeHogar == 2] <-"OCUPADOASALARIADO"
enghoHogRules$OcupacionJefeHogar[enghoHogRules$OcupacionJefeHogar == 3] <-"OCUPADOCUENTAPROPIA"
#decil de ingreso
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 1] <-"1erDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 2] <-"2doDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 3] <-"3erDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 4] <-"4toDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 5] <-"5toDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 6] <-"6toDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 7] <-"7moDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 8] <-"8voDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 9] <-"9noDecil"
enghoHogRules$DecilIngreso[enghoHogRules$DecilIngreso == 10] <-"10moDecil"
#empieza el arules
enghoHogRules$RedCloacal <- as.factor(enghoHogRules$RedCloacal)
enghoHogRules$TipoDeVivienda <- as.factor(enghoHogRules$TipoDeVivienda)
enghoHogRules$CombustibleCocina <- as.factor(enghoHogRules$CombustibleCocina)
enghoHogRules$OcupacionJefeHogar <- as.factor(enghoHogRules$OcupacionJefeHogar)
enghoHogRules$DecilIngreso <- as.factor(enghoHogRules$DecilIngreso)
View(enghoHogRules)
#arules no lee data frame sino list con vectores
#items <- strsplit(as.character(enghoHogRules$Tags), ", ")
trans3 <- as(enghoHogRules, "transactions")
rules <- apriori(trans3, parameter = list(sup = 0.1, 
                                          conf = 0.5, target="rules",minlen=1))

summary(rules)
inspect(head(rules, n = 10, by = "lift"))
###########
lm1 <- lm(gastoXpercing$CANTPING~gastoXpercing$GC_1)
# e da un R^2 de casi 1, altísima correlación)
summary.lm(lm1)
all(is.nan(gastoXpercing$GC_1))
aov1 <-aov(as.numeric(GC_1)~as.integer(CANTPING),data = gastoXpercing,na.action=na.exclude)
summary.aov(aov1)
qplot(GC_1,CANTPING,data=gastoXpercing)

