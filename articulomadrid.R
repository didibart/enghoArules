#datos engho 2012
enghoGastos <- read.csv("ArticuloMadridAbril2020/bases_datos_engho2012/ENGHo-Gastos.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8")
enghoCodigosGastos <- read.csv("ArticuloMadridAbril2020/ENGHo_Codigos_de_bienes_y_servicios.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8")
enghoHogares <- read.csv("ArticuloMadridAbril2020/bases_datos_engho2012/ENGHo-Hogares.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, encoding = "UTF-8")

View(enghoGastos)
View(enghoCodigosGastos)
View(enghoGastos1)
View(enghoHogares)
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
# regiones.CRGN (codigo de region 1 gba, 2 pamp, 3 noroeste, 4 noreste, 5 cuyo, 6 patagonia)

#lo primero que hago es poner las etiquetas en estas variables seleccionadas

View(enghoHogares$CANTPING)
max(enghoHogares$CANTPING)
min(enghoHogares$CANTPING)
mean(enghoHogares$CANTPING)
sd(enghoHogares$CANTPING)
gastoXpercing <- enghoHogares$CANTPING
gastoXpercing <- enghoHogares$GC_1
gastoXpercing <- sqldf("SELECT enghoHogares.GC_1, enghoHogares.CANTPING FROM
                       enghoHogares WHERE enghoHogares.REGION in(1,2)")
library(Matrix)
library(ggplot2)
library(MASS)
gastoXpercing1M <- data.matrix(gastoXpercing)
gastoXpercing1 <-sparseMatrix(gastoXpercing1M["CANTPING"],gastoXpercing1M["GC_1"])
lm1 <- lm(gastoXpercing$CANTPING~gastoXpercing$GC_1)
# e da un R^2 de casi 1, altísima correlación)
summary.lm(lm1)
all(is.nan(gastoXpercing$GC_1))
aov1 <-aov(as.numeric(GC_1)~as.integer(CANTPING),data = gastoXpercing,na.action=na.exclude)
summary.aov(aov1)
qplot(GC_1,CANTPING,data=gastoXpercing)
View(gastoXpercing)
dim(gastoXpercing)

enghoHogRules$RedCloacal <-enghoHogares$CV1B01_C
enghoHogRules$TipoDeVivienda <-enghoHogares$CV1B02
enghoHogRules$CombustibleCocina <-enghoHogares$CH09
enghoHogRules$OcupacionJefeHogar <-enghoHogares$JSITOCUP
enghoHogRules$GastoAlimentos <-enghoHogares$GC_1
enghoHogRules$Quintil <-enghoHogares$QUIGAPHT

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

enghoHogRules[order(enghoHogRules$GastoAlimentos)]
names(enghoHogRules)
p <-min(enghoHogRules$GastoAlimentos)

dim(enghoHogRules)
colnames(enghoHogRules)
enghoHogRules$RedCloacal
