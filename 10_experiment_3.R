library(tidyr)
library(dplyr)
library(stringr)
library(car)

#erstes level - wörter
#wir definieren hier mit ströbel die kategorien von statischen dynamischen und gerichteten kopf arm und bein verben
data$tener_count = str_count(data$text, paste(c(" tener ", #inf
                                                " tengo ", #presente
                                                " tienes ", 
                                                " tenés ", #voseo
                                                " tenes ", #errata
                                                " tiene ",
                                                " tene ",
                                                " tené ", #imperativo voseante
                                                " tenemos ",
                                                " teneis ", #errata
                                                " tenéis ",
                                                " tienen ",
                                                " tenía ", #imperfecto
                                                " tenia ",
                                                " tenías ",
                                                " tenias ",
                                                " teníamos ",
                                                " teniamos ",
                                                " teníais ",
                                                " teniais ", #errata
                                                " tenían ",
                                                " tenian ",
                                                " tuve ", #perfecto simple
                                                " tuviste ",
                                                " tuvistes ",
                                                " tuvo ",
                                                " tuvimos ",
                                                " tuvisteis ",
                                                " tuvieron ",
                                                " tendré ", #futuro simple
                                                " tendre ",
                                                " tendrás ",
                                                " tendras ",
                                                " tendra ",
                                                " tendrá ",
                                                " tendremos ",
                                                " tendréis ",
                                                " tendreis ",
                                                " tendrán ",
                                                " tendran ",
                                                " teniendo ", #gerundio
                                                " tenido ", #participio
                                                " tenga ", #subjuntivo presente
                                                " tengas ",
                                                " tengás ",
                                                " tenga ",
                                                " tengamos ",
                                                " tengáis ",
                                                " tengais ",
                                                " tengan ",
                                                " tuviera ", #subjuntivo imperfecto
                                                " tuviéra ",
                                                " tuviéras ",
                                                " tuvieras ",
                                                " tuviéra ",
                                                " tuviera ",
                                                " tuviéramos ",
                                                " tuvieramos ",
                                                " tuviérais ",
                                                " tuvierais ",
                                                " tuvieran ",
                                                " tuviéran ",
                                                " tuviese ",
                                                " tuvieses ",
                                                " tuviésemos ",
                                                " tuviesemos ",
                                                " tuvieseis ",
                                                " tuviéseis ",
                                                " tuviesen ",
                                                " tuviésen ",
                                                " tendría ",
                                                " tendria ",
                                                " tendrías ",
                                                " tendrias ",
                                                " tendríamos ",
                                                " tendriamos ",
                                                " tendríais ",
                                                " tendriais ",
                                                " tendrían ",
                                                " tendrian "), 
                                              collapse = "|"))


data$tener_logi = FALSE
data$tener_logi[data$tener_count != 0] = TRUE

tener = table(data$tener_logi, data$neighbors)
hasnt_tener = tener[c(TRUE,FALSE)]
has_tener = tener[c(FALSE,TRUE)]
prop = has_tener/hasnt_tener
neighbors = c(1:length(prop))
tener_data = as.data.frame(cbind(prop, neighbors))
tener_data$concept = "tener"

data$ser_count = str_count(data$text, paste(c(" ser ",
                                              " soy ",
                                              " eres ",
                                              " sos ",
                                              " es ",
                                              " sé ",
                                              " somos ",
                                              " sois ",
                                              " son ",
                                              " era ",
                                              " eras ",
                                              " eramos ",
                                              " erais ",
                                              " eran ", #fui wird "ir" zugeordnet
                                              " seré ",
                                              " sere ",
                                              " serás ",
                                              " seras ",
                                              " será ",
                                              " sera ",
                                              " seremos ",
                                              " seráis ",
                                              " serais ",
                                              " serán ",
                                              " seran ",
                                              " siendo ",
                                              " sido ",
                                              " sea ",
                                              " seas ",
                                              " seás ",
                                              " seamos ",
                                              " seáis ",
                                              " seais ",
                                              " sean ",
                                              " sería ",
                                              " serías ",
                                              " seríamos ",
                                              " seriamos ",
                                              " seríais ",
                                              " seriais ",
                                              " serían ",
                                              " serian "), #fuera wird ausgelassen weil es als hilfsverb vorkommt 
                                            collapse = "|"))

data$ser_logi = FALSE
data$ser_logi[data$ser_count != 0] = TRUE

ser = table(data$ser_logi, data$neighbors)
hasnt_ser = ser[c(TRUE,FALSE)]
has_ser = ser[c(FALSE,TRUE)]
prop = has_ser/hasnt_ser
neighbors = c(1:length(prop))
ser_data = as.data.frame(cbind(prop, neighbors))
ser_data$concept = "ser"

data$estar_count = str_count(data$text, paste(c(" estar ",
                                                " estoy ",
                                                " estás ",
                                                " está ",
                                                " estamos ",
                                                " estáis ",
                                                " estais ",
                                                " están ",
                                                " estan ",
                                                " estaba ",
                                                " estabas ",
                                                " estabamos ",
                                                " estabais ",
                                                " estaban ",
                                                " estuve ",
                                                " estuviste ",
                                                " estuvistes ",
                                                " estuvo ",
                                                " estuvimos ",
                                                " estuvísteis ",
                                                " estuvisteis ",
                                                " estuvieron ",
                                                " estaré ",
                                                " estare ",
                                                " estarás ",
                                                " estaras ",
                                                " estará ",
                                                " estara ",
                                                " estaremos ",
                                                " estaréis ",
                                                " estareis ",
                                                " estarán ",
                                                " estaran ",
                                                " estando ",
                                                " estado ",
                                                " esté ",
                                                " estés ",
                                                " estes ",
                                                " estemos ",
                                                " estéis ",
                                                " esteis ",
                                                " esten ",
                                                " estén ",
                                                " estuviera ",
                                                " estuvieras ",
                                                " estuviéramos ",
                                                " estuvieramos ",
                                                " estuviérais ",
                                                " estuvierais ",
                                                " estuvieran ",
                                                " estuviéran ",
                                                " estuviese ",
                                                " estuvieses ",
                                                " estuviéses ",
                                                " estuviésemos ",
                                                " estuviesemos ",
                                                " estuviéseis ",
                                                " estuvieseis ",
                                                " estuviesen ",
                                                " estuviésen ",
                                                " estaría ",
                                                " estaria ",
                                                " estarías ",
                                                " estarias ",
                                                " estaríamos ",
                                                " estariamos ",
                                                " estaríais ",
                                                " estariais ",
                                                " estarían ",
                                                " estarian "), #fuera wird ausgelassen weil es als hilfsverb vorkommt 
                                            collapse = "|"))

data$estar_logi = FALSE
data$estar_logi[data$estar_count != 0] = TRUE

estar = table(data$estar_logi, data$neighbors)
hasnt_estar = estar[c(TRUE,FALSE)]
has_estar = estar[c(FALSE,TRUE)]
prop = has_estar/hasnt_estar
neighbors = c(1:length(prop))
estar_data = as.data.frame(cbind(prop, neighbors))
estar_data$concept = "estar"

data$hacer_count = str_count(data$text, paste(c(" hacer ",
                                                " hago ",
                                                " haces ",
                                                " hacés ",
                                                " hace ",
                                                " hacé ",
                                                " hacemos ",
                                                " hacéis ",
                                                " haceis ",
                                                " hacen ",
                                                " hacía ",
                                                " hacías ",
                                                " hacias ",
                                                " haciamos ",
                                                " hacíamos ",
                                                " hacíais ",
                                                " haciais ",
                                                " hacian ",
                                                " hacían ",
                                                " hice ",
                                                " hictiste ",
                                                " hicistes ",
                                                " hizo ",
                                                " hicimos ",
                                                " hicísteis ",
                                                " hicisteis ",
                                                " hicieron ",
                                                " hiciéron ",
                                                " haré ",
                                                " hare ",
                                                " harás ",
                                                " haras ",
                                                " hará ",
                                                " hara ",
                                                " haremos ",
                                                " haréis ",
                                                " hareis ",
                                                " harán ",
                                                " haran ",
                                                " haciendo ",
                                                " hecho ",
                                                " haga ",
                                                " hagas ",
                                                " hagás ",
                                                " hagamos ",
                                                " hagáis ",
                                                " hagais ",
                                                " hagan ",
                                                " hiciera ",
                                                " hiciéras ",
                                                " hicieras ",
                                                " hiciéramos ",
                                                " hicieramos ",
                                                " hiciéran ",
                                                " hicieran ",
                                                " hiciese ",
                                                " hicieses ",
                                                " hiciéramos ",
                                                " hicieramos ",
                                                " hicierais ",
                                                " hiciérais ",
                                                " hiciéran ",
                                                " hicieran ",
                                                " haría ",
                                                " haria ",
                                                " harías ",
                                                " harias ",
                                                " haríamos ",
                                                " hariamos ",
                                                " haríais ",
                                                " hariais ",
                                                " harían ",
                                                " harian "), 
                                            collapse = "|"))

data$hacer_logi = FALSE
data$hacer_logi[data$hacer_count != 0] = TRUE

hacer = table(data$hacer_logi, data$neighbors)
hasnt_hacer = hacer[c(TRUE,FALSE)]
has_hacer = hacer[c(FALSE,TRUE)]
prop = has_hacer/hasnt_hacer
neighbors = c(1:length(prop))
hacer_data = as.data.frame(cbind(prop, neighbors))
hacer_data$concept = "hacer"

data$dar_count = str_count(data$text, paste(c(" dar ",
                                              " doy ",
                                              " das ",
                                              " da ",
                                              " damos ",
                                              " dáis ",
                                              " dais ",
                                              " dan ",
                                              " daba ",
                                              " dabas ",
                                              " daba ",
                                              " dábamos ",
                                              " dabamos ",
                                              " dabais ",
                                              " dábais ",
                                              " daban ",
                                              " di ",
                                              " dí ",
                                              " diste ",
                                              " distes ",
                                              " dio ",
                                              " dió ",
                                              " dimos ",
                                              " disteis ",
                                              " dísteis ",
                                              " dieron ",
                                              " diéron ",
                                              " daré ",
                                              " dare ",
                                              " darás ",
                                              " daras ",
                                              " dara ",
                                              " dará ",
                                              " daremos ",
                                              " daréis ",
                                              " dareis ",
                                              " darán ",
                                              " daran ",
                                              " dando ",
                                              " dado ",
                                              " dé ",
                                              " des ",
                                              " demos ",
                                              " déis ",
                                              " deis ",
                                              " den ",
                                              " diera ",
                                              " dieras ",
                                              " diéras ",
                                              " diéramos ",
                                              " dieramos ",
                                              " diérais ",
                                              " dierais ",
                                              " dieran ",
                                              " diese ",
                                              " dieses ",
                                              " diesemos ",
                                              " diésemos ",
                                              " diéseis ",
                                              " dieseis ",
                                              " diesen ",
                                              " daría ",
                                              " daria ",
                                              " darías ",
                                              " darias ",
                                              " daríamos ",
                                              " dariamos ",
                                              " daríais ",
                                              " dariais ",
                                              " darían ",
                                              " darian "), 
                                            collapse = "|"))

data$dar_logi = FALSE
data$dar_logi[data$dar_count != 0] = TRUE

dar = table(data$dar_logi, data$neighbors)
hasnt_dar = dar[c(TRUE,FALSE)]
has_dar = dar[c(FALSE,TRUE)]
prop = has_dar/hasnt_dar
neighbors = c(1:length(prop))
dar_data = as.data.frame(cbind(prop, neighbors))
dar_data$concept = "dar"

data$quedar_count = str_count(data$text, paste(c(" quedar ",
                                                 " quedo ",
                                                 " quedas ",
                                                 " quedás ",
                                                 " queda ",
                                                 " quedá ",
                                                 " quedamos ",
                                                 " quedáis ",
                                                 " quedais ",
                                                 " quedan ",
                                                 " quedaba ",
                                                 " quedabas ",
                                                 " quedábamos ",
                                                 " quedabamos ",
                                                 " quedábais ",
                                                 " quedabais ",
                                                 " quedaban ",
                                                 " quedé ",
                                                 " quedaste ",
                                                 " quedastes ",
                                                 " quedó ",
                                                 " quedásteis ",
                                                 " quedasteis ",
                                                 " quedaron ",
                                                 " quedaré ",
                                                 " quedare ",
                                                 " quedarás ",
                                                 " quedará ",
                                                 " quedaremos ",
                                                 " quedaréis ",
                                                 " quedaráis ",
                                                 " quedarán ",
                                                 " quedando ",
                                                 " quedado ",
                                                 " quede ",
                                                 " quedes ",
                                                 " quedés ",
                                                 " quedemos ",
                                                 " quedéis ",
                                                 " quedeis ",
                                                 " queden ",
                                                 " quedara ",
                                                 " quedaras ",
                                                 " quedáramos ",
                                                 " quedaramos ",
                                                 " quedárais ",
                                                 " quedarais ",
                                                 " quedaran ",
                                                 " quedase ",
                                                 " quedases ",
                                                 " quedasemos ",
                                                 " quedaseis ",
                                                 " quedáseis ",
                                                 " quedasen ",
                                                 " quedaría ",
                                                 " quedaria ",
                                                 " quedarías ",
                                                 " quedarias ",
                                                 " quedaríamos ",
                                                 " quedariamos ",
                                                 " quedaríais ",
                                                 " quedariais ",
                                                 " quedarían ",
                                                 " quedarian "), 
                                            collapse = "|"))

data$quedar_logi = FALSE
data$quedar_logi[data$quedar_count != 0] = TRUE

quedar = table(data$quedar_logi, data$neighbors)
hasnt_quedar = quedar[c(TRUE,FALSE)]
has_quedar = quedar[c(FALSE,TRUE)]
prop = has_quedar/hasnt_quedar
neighbors = c(1:length(prop))
quedar_data = as.data.frame(cbind(prop, neighbors))
quedar_data$concept = "quedar"

data$ir_count = str_count(data$text, paste(c(" ir",
                                             " voy ",
                                             " vas ",
                                             " va ",
                                             " ve ",
                                             " vamos ",
                                             " vais ",
                                             " váis ",
                                             " van ",
                                             " iba ",
                                             " ibas ",
                                             " ibamos ",
                                             " ibais ",
                                             " íbais ",
                                             " iban ",
                                             " fui a ",
                                             " fuiste a ",
                                             " fuistes a ",
                                             " fue a ",
                                             " fué a ",
                                             " fuimos a ",
                                             " fuísteis a ",
                                             " fuisteis a ",
                                             " fueron a ",
                                             " iré ",
                                             " ire ",
                                             " irás ",
                                             " iras ",
                                             " irá ",
                                             " iremos ",
                                             " iréis ",
                                             " ireis ",
                                             " irán ",
                                             " iran ",
                                             " yendo ",
                                             " ido ",
                                             " vaya ",
                                             " vayas ",
                                             " vayás ",
                                             " vayámos ",
                                             " vayamos ",
                                             " vayáis ",
                                             " vayais ",
                                             " vayan ",
                                             " iría ",
                                             " iria ",
                                             " irías ",
                                             " irias ",
                                             " iríamos ",
                                             " iriamos ",
                                             " iríais ",
                                             " iriais ",
                                             " irían ",
                                             " irian "
                                             ) , 
                                            collapse = "|"))

data$ir_logi = FALSE
data$ir_logi[data$ir_count != 0] = TRUE

ir = table(data$ir_logi, data$neighbors)
hasnt_ir = ir[c(TRUE,FALSE)]
has_ir = ir[c(FALSE,TRUE)]
prop = has_ir/hasnt_ir
neighbors = c(1:length(prop))
ir_data = as.data.frame(cbind(prop, neighbors))
ir_data$concept = "ir"


data$llegar_count = str_count(data$text, paste(c(" llegar ",
                                                 " llego ",
                                                 " llegas ",
                                                 " llegás ",
                                                 " llega ",
                                                 " llegá ",
                                                 " llegamos ",
                                                 " llegáis ",
                                                 " llegan ",
                                                 " llegaba ",
                                                 " llegabas ",
                                                 " llegabamos ",
                                                 " llegábamos ",
                                                 " llegábais ",
                                                 " llegabais ",
                                                 " llegaban ",
                                                 " llegában ",
                                                 " llegué ",
                                                 " llegaste ",
                                                 " llegastes ",
                                                 " llegó ",
                                                 " llegásteis ",
                                                 " llegasteis ",
                                                 " llegaron ",
                                                 " llegaré ",
                                                 " llegarás ",
                                                 " llegará ",
                                                 " llegaremos ",
                                                 " llegarémos ",
                                                 " llegaréis ",
                                                 " llegareis ",
                                                 " llegarán ",
                                                 " llegando ",
                                                 " llegado ",
                                                 " llegue ",
                                                 " llegues ",
                                                 " llegués ",
                                                 " lleguemos ",
                                                 " lleguémos ",
                                                 " lleguéis ",
                                                 " llegueis ",
                                                 " lleguen ",
                                                 " llegára ",
                                                 " llegara ",
                                                 " llegáras ",
                                                 " llegaras ",
                                                 " llegáramos ",
                                                 " llegaramos ",
                                                 " llegárais ",
                                                 " llegarais ",
                                                 " llegaran ",
                                                 " llegáran ",
                                                 " llegase ",
                                                 " llegáseis ",
                                                 " llegaseis ",
                                                 " llegásemos ",
                                                 " llegasemos ",
                                                 " llegáseis ",
                                                 " llegaseis ",
                                                 " llegasen ",
                                                 " llegásen ",
                                                 " llegaría ",
                                                 " llegaria ",
                                                 " llegarías ",
                                                 " llegarias ",
                                                 " llegaríamos ",
                                                 " llegariamos ",
                                                 " llegaríais ",
                                                 " llegariais ",
                                                 " llegarían ",
                                                 " llegarian "), 
                                            collapse = "|"))

data$llegar_logi = FALSE
data$llegar_logi[data$llegar_count != 0] = TRUE

llegar = table(data$llegar_logi, data$neighbors)
hasnt_llegar = llegar[c(TRUE,FALSE)]
has_llegar = llegar[c(FALSE,TRUE)]
prop = has_llegar/hasnt_llegar
neighbors = c(1:length(prop))
llegar_data = as.data.frame(cbind(prop, neighbors))
llegar_data$concept = "llegar"


data$regresar_count = str_count(data$text, paste(c(" regresar ",
                                                   " regreso ",
                                                   " regresas ",
                                                   " regresás ",
                                                   " regresa ",
                                                   " regresamos ",
                                                   " regresáis ",
                                                   " regresais ",
                                                   " regresan ",
                                                   " regresaba ",
                                                   " regresabas ",
                                                   " regresábamos ",
                                                   " regresabamos ",
                                                   " regresábais ",
                                                   " regresabais ",
                                                   " regresaban ",
                                                   " regresé ",
                                                   " regrestaste ",
                                                   " regrestastes ",
                                                   " regresó ",
                                                   " regresásteis ",
                                                   " regresasteis ",
                                                   " regresaron ",
                                                   " regresaré ",
                                                   " regresare ",
                                                   " regresarás ",
                                                   " regresará ",
                                                   " regresaremos ",
                                                   " regresaréis ",
                                                   " regresareis ",
                                                   " regresarán ",
                                                   " regresado ",
                                                   " regresando ",
                                                   " regrese ",
                                                   " regreses ",
                                                   " regresés ",
                                                   " regresemos ",
                                                   " regrésemos ",
                                                   " regreséis ",
                                                   " regreseis ",
                                                   " regresen ",
                                                   " regresara ",
                                                   " regresaras ",
                                                   " regresaramos ",
                                                   " regresáramos ",
                                                   " regresárais ",
                                                   " regresarais ",
                                                   " regresaran ",
                                                   " regresase ",
                                                   " regresases ",
                                                   " regresasemos ",
                                                   " regresásemos ",
                                                   " regresáseis ",
                                                   " regresaseis ",
                                                   " regresasen ",
                                                   " regresaría ",
                                                   " regresaria ",
                                                   " regresarías ",
                                                   " regresarias ",
                                                   " regresaríamos ",
                                                   " regresariamos ",
                                                   " regresaríais ",
                                                   " regresariais ",
                                                   " regresarían ",
                                                   " regresarian "), 
                                            collapse = "|"))

data$regresar_logi = FALSE
data$regresar_logi[data$regresar_count != 0] = TRUE

regresar = table(data$regresar_logi, data$neighbors)
hasnt_regresar = regresar[c(TRUE,FALSE)]
has_regresar = regresar[c(FALSE,TRUE)]
prop = has_regresar/hasnt_regresar
neighbors = c(1:length(prop))
regresar_data = as.data.frame(cbind(prop, neighbors))
regresar_data$concept = "regresar"


data$coger_count = str_count(data$text, paste(c(" coger ",
                                                " cojo ",
                                                " coges ",
                                                " cojes ",
                                                " cogés ",
                                                " cojés ",
                                                " coge ",
                                                " coje ",
                                                " cogemos ",
                                                " cojemos ",
                                                " cogéis ",
                                                " cojéis ",
                                                " cogen ",
                                                " cojen ",
                                                " cogía ",
                                                " cojía ",
                                                " cogia ",
                                                " cojia ",
                                                " cogías ",
                                                " cogias ",
                                                " cojias ",
                                                " cojías ",
                                                " cogíamos ",
                                                " cogiamos ",
                                                " cojíamos ",
                                                " cojiamos ",
                                                " cojíais ",
                                                " cojiais ",
                                                " cogiais ",
                                                " cogíais ",
                                                " cogían ",
                                                " cogian ",
                                                " cojían ",
                                                " cojian ",
                                                " cogí ",
                                                " cogi ",
                                                " coji ",
                                                " cojí ",
                                                " cogiste ",
                                                " cojiste ",
                                                " cogió ",
                                                " cogio ",
                                                " cojió ",
                                                " cojio ",
                                                " cogimos ",
                                                " cojimos ",
                                                " cojísteis ",
                                                " cojisteis ",
                                                " cogisteis ",
                                                " cogísteis ",
                                                " cogieron ",
                                                " cojieron ",
                                                " cogeré ",
                                                " cogere ",
                                                " cojeré ",
                                                " cojere ",
                                                " cogerás ",
                                                " cogeras ",
                                                " cojerás ",
                                                " cojeras ",
                                                " cogerá ",
                                                " cogera ",
                                                " cojerá ",
                                                " cojera ",
                                                " cogerémos ",
                                                " cogeremos ",
                                                " cojeremos ",
                                                " cojerémos ",
                                                " cojeráis ",
                                                " cogeráis ",
                                                " cojerais ",
                                                " cogerais ",
                                                " cogerán ",
                                                " cogeran ",
                                                " cojerán ",
                                                " cogeran ",
                                                " cogiendo ",
                                                " cojiendo ",
                                                " cojido ",
                                                " cogido ",
                                                " coja ",
                                                " cojas ",
                                                " cojás ",
                                                " cojamos ",
                                                " cojáis ",
                                                " cojan ",
                                                " cojára ",
                                                " cojara ",
                                                " cojaras ",
                                                " cojáras ",
                                                " cojarais ",
                                                " cojárais ",
                                                " cojaran ",
                                                " cojáran ",
                                                " cojase ",
                                                " cojases ",
                                                " cojáses ",
                                                " cojásemos ",
                                                " cojasemos ",
                                                " cojáseis ",
                                                " cojaseis ",
                                                " cojasen ",
                                                " cogería ",
                                                " cojería ",
                                                " cogeria ",
                                                " cojeria ",
                                                " cogerías ",
                                                " cojerías ",
                                                " cogerias ",
                                                " cojerias ",
                                                " cogeríamos ",
                                                " cojeríamos ",
                                                " cogeriamos ",
                                                " cojeriamos ",
                                                " cogeríais ",
                                                " cojeríais ",
                                                " cogeriais ",
                                                " cojeriais ",
                                                " cogerían ",
                                                " cojerían ",
                                                " cogerian ",
                                                " cojerian "
                                                ) , 
                                            collapse = "|"))

data$coger_logi = FALSE
data$coger_logi[data$coger_count != 0] = TRUE

coger = table(data$coger_logi, data$neighbors)
hasnt_coger = coger[c(TRUE,FALSE)]
has_coger = coger[c(FALSE,TRUE)]
prop = has_coger/hasnt_coger
neighbors = c(1:length(prop))
coger_data = as.data.frame(cbind(prop, neighbors))
coger_data$concept = "coger"


data$tomar_count = str_count(data$text, paste(c(" tomar ",
                                                " tomo ",
                                                " tomas ",
                                                " tomás ",
                                                " toma ",
                                                " tomá ",
                                                " tomamos ",
                                                " tomáis ",
                                                " tomais ",
                                                " toman ",
                                                " tomaba ",
                                                " tomabas ",
                                                " tomábamos ",
                                                " tomabamos ",
                                                " tomábais ",
                                                " tomabais ",
                                                " tomaban ",
                                                " tomában ",
                                                " tomé ",
                                                " tomaste ",
                                                " tomó ",
                                                " tomásteis ",
                                                " tomasteis ",
                                                " tomaron ",
                                                " tomaré ",
                                                " tomare ",
                                                " tomarás ",
                                                " tomará ",
                                                " tomaremos ",
                                                " tomarémos ",
                                                " tomaréis ",
                                                " tomareis ",
                                                " tomarais ",
                                                " tomaráis ",
                                                " tomarán ",
                                                " tomado ",
                                                " tomando ",
                                                " tome ",
                                                " tomes ",
                                                " tomés ",
                                                " tomemos ",
                                                " toméis ",
                                                " tomeis ",
                                                " tomen ",
                                                " tomara ",
                                                " tomára ",
                                                " tomaras ",
                                                " tomáras ",
                                                " tomaramos ",
                                                " tomáramos ",
                                                " tomárais ",
                                                " tomaran ",
                                                " tomáran ",
                                                " tomase ",
                                                " tomasen ",
                                                " tomasemos ",
                                                " tomásemos ",
                                                " tomáseis ",
                                                " tomaseis ",
                                                " tomasen ",
                                                " tomaría ",
                                                " tomaria ",
                                                " tomarías ",
                                                " tomarias ",
                                                " tomaríamos ",
                                                " tomariamos ",
                                                " tomaríais ",
                                                " tomariais ",
                                                " tomarían ",
                                                " tomarian "
                                                ) , 
                                            collapse = "|"))

data$tomar_logi = FALSE
data$tomar_logi[data$tomar_count != 0] = TRUE

tomar = table(data$tomar_logi, data$neighbors)
hasnt_tomar = tomar[c(TRUE,FALSE)]
has_tomar = tomar[c(FALSE,TRUE)]
prop = has_tomar/hasnt_tomar
neighbors = c(1:length(prop))
tomar_data = as.data.frame(cbind(prop, neighbors))
tomar_data$concept = "tomar"


data$agarrar_count = str_count(data$text, paste(c(" agarrar ",
                                                  " agarro ",
                                                  " agarras ",
                                                  " agarrás ",
                                                  " agarra ",
                                                  " agarrá ",
                                                  " agarramos ",
                                                  " agarráis ",
                                                  " agarrais ",
                                                  " agarran ",
                                                  " agarraba ",
                                                  " agarrabas ",
                                                  " aggarábamos ",
                                                  " agarrabamos ",
                                                  " agarrábais ",
                                                  " agarrabais ",
                                                  " agarraban ",
                                                  " agarré ",
                                                  " agarraste ",
                                                  " agarrastes ",
                                                  " agarró ",
                                                  " agarrasteis ",
                                                  " agarrásteis ",
                                                  " agarraron ",
                                                  " agarraré ",
                                                  " agarrarás ",
                                                  " agarrará ",
                                                  " agarraramos ",
                                                  " agarraréis ",
                                                  " agarrareis ",
                                                  " agarrarán ",
                                                  " agarrando ",
                                                  " agarrado ",
                                                  " agarre ",
                                                  " agarres ",
                                                  " agarremos ",
                                                  " agarréis ",
                                                  " agarren ",
                                                  " agarrara ",
                                                  " agarraras ",
                                                  " agarraramos ",
                                                  " agarrarais ",
                                                  " agarráramos ",
                                                  " agarrárais ",
                                                  " agarraran ",
                                                  " agarráran ",
                                                  " agarrase ",
                                                  " agarrases ",
                                                  " agarrasemos ",
                                                  " agarrásemos ",
                                                  " agarráseis ",
                                                  " agarraseis ",
                                                  " agarrasen ",
                                                  " agarrásen ",
                                                  " agarraría ",
                                                  " agarraria ",
                                                  " agarrarías ",
                                                  " agarrarias ",
                                                  " agarraríamos ",
                                                  " agarrariamos ",
                                                  " agarraríais ",
                                                  " agarrariais ",
                                                  " agarrarian ",
                                                  " agarrarían "
                                                  ), 
                                            collapse = "|"))

data$agarrar_logi = FALSE
data$agarrar_logi[data$agarrar_count != 0] = TRUE

agarrar = table(data$agarrar_logi, data$neighbors)
hasnt_agarrar = agarrar[c(TRUE,FALSE)]
has_agarrar = agarrar[c(FALSE,TRUE)]
prop = has_agarrar/hasnt_agarrar
neighbors = c(1:length(prop))
agarrar_data = as.data.frame(cbind(prop, neighbors))
agarrar_data$concept = "agarrar"


verb_data = rbind(tener_data,
                  estar_data,
                  ser_data,
                  hacer_data,
                  dar_data,
                  quedar_data,
                  ir_data,
                  llegar_data,
                  regresar_data,
                  coger_data,
                  tomar_data,
                  agarrar_data)

verb_data$dynamicity[verb_data$concept == "tener"] = "static"
verb_data$dynamicity[verb_data$concept == "estar"] = "static"
verb_data$dynamicity[verb_data$concept == "agarrar"] = "directed"
verb_data$dynamicity[verb_data$concept == "coger"] = "directed"
verb_data$dynamicity[verb_data$concept == "hacer"] = "dynamic"
verb_data$dynamicity[verb_data$concept == "ir"] = "dynamic"
verb_data$dynamicity[verb_data$concept == "llegar"] = "directed"
verb_data$dynamicity[verb_data$concept == "quedar"] = "static"
verb_data$dynamicity[verb_data$concept == "regresar"] = "directed"
verb_data$dynamicity[verb_data$concept == "ser"] = "static"
verb_data$dynamicity[verb_data$concept == "tomar"] = "directed"
verb_data$dynamicity[verb_data$concept == "dar"] = "dynamic"


verb_data = verb_data[verb_data$prop != 0 & verb_data$prop != Inf,]

#unified representation
ggplot(verb_data, aes(x = prop, y = neighbors, col = dynamicity)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "poisson")) + 
  facet_wrap(concept ~ ., scales = "free_x") +
  theme_minimal() +
  scale_x_continuous(name = "Concept Probability") +
  scale_y_continuous(name = "Degree")
  
#each verb in itself
tener_model = glm(data = verb_data[verb_data$concept == "tener",], formula = neighbors ~ prop, family = "poisson")
summary(tener_model)
shapiro.test(tener_model$residual)
Anova(tener_model)

estar_model = glm(data = verb_data[verb_data$concept == "estar",], formula = neighbors ~ prop, family = "poisson")
summary(estar_model)
shapiro.test(estar_model$residuals)
Anova(estar_model)

agarrar_model = glm(data = verb_data[verb_data$concept == "agarrar",], formula = neighbors ~ prop, family = "poisson")
summary(agarrar_model)
shapiro.test(agarrar_model$residuals)
Anova(agarrar_model)

coger_model = glm(data = verb_data[verb_data$concept == "coger",], formula = neighbors ~ prop, family = "poisson")
summary(coger_model)
shapiro.test(coger_model$residuals)
Anova(coger_model)

dar_model = glm(data = verb_data[verb_data$concept == "dar",], formula = neighbors ~ prop, family = "poisson")
summary(dar_model)
shapiro.test(dar_model$residuals)
Anova(dar_model)

hacer_model = glm(data = verb_data[verb_data$concept == "hacer",], formula = neighbors ~ prop, family = "poisson")
summary(hacer_model)
shapiro.test(hacer_model$residuals)
Anova(hacer_model)

ir_model = glm(data = verb_data[verb_data$concept == "ir",], formula = neighbors ~ prop, family = "poisson")
summary(ir_model)
shapiro.test(ir_model$residuals)
Anova(ir_model)

llegar_model = glm(data = verb_data[verb_data$concept == "llegar",], formula = neighbors ~ prop, family = "poisson")
summary(llegar_model)
shapiro.test(llegar_model$residuals)
Anova(llegar_model)

quedar_model = glm(data = verb_data[verb_data$concept == "quedar",], formula = neighbors ~ prop, family = "poisson")
summary(quedar_model)
shapiro.test(quedar_model$residuals)
Anova(quedar_model)

regresar_model = glm(data = verb_data[verb_data$concept == "regresar",], formula = neighbors ~ prop, family = "poisson")
summary(regresar_model)
shapiro.test(regresar_model$residuals)
Anova(regresar_model)

ser_model = glm(data = verb_data[verb_data$concept == "ser",], formula = neighbors ~ prop, family = "poisson")
summary(ser_model)
shapiro.test(ser_model$residuals)
Anova(ser_model)

tomar_model = glm(data = verb_data[verb_data$concept == "tomar",], formula = neighbors ~ prop, family = "poisson")
summary(tomar_model)
shapiro.test(tomar_model$residuals)
Anova(tomar_model)


#compare the quality of the models according to the type of concept

models = list(tener_model, estar_model, agarrar_model, coger_model, hacer_model, ir_model, llegar_model, quedar_model, regresar_model, dar_model, ser_model)
concept = c("tener", "estar", "agarrar", "coger", "hacer", "ir", "llegar", "quedar", "regresar", "dar", "ser")
coef = c()
rsquared = c()
max = c()

append_model_params = function(models = models){
  for(model in models){
    coef <<- append(coef, model$coefficients[2])
    rsquared <<- append(rsquared, rsq(model))
    max <<- append(max, max(model$model["neighbors"]))
  }
}

append_model_params(models = models)


model_data = as.data.frame(cbind(concept, coef, rsquared, max), stringsAsFactors = FALSE) 
model_data$coef = as.numeric(model_data$coef)
model_data$rsquared = as.numeric(model_data$rsquared)
model_data$max = as.numeric(model_data$max)

model_data = model_data %>% mutate(slope = coef / max)


model_data$dynamicity[model_data$concept == "tener"] = "static"
model_data$dynamicity[model_data$concept == "estar"] = "static"
model_data$dynamicity[model_data$concept == "agarrar"] = "directed"
model_data$dynamicity[model_data$concept == "coger"] = "directed"
model_data$dynamicity[model_data$concept == "hacer"] = "dynamic"
model_data$dynamicity[model_data$concept == "ir"] = "dynamic"
model_data$dynamicity[model_data$concept == "llegar"] = "directed"
model_data$dynamicity[model_data$concept == "quedar"] = "static"
model_data$dynamicity[model_data$concept == "regresar"] = "directed"
model_data$dynamicity[model_data$concept == "ser"] = "static"
model_data$dynamicity[model_data$concept == "tomar"] = "directed"
model_data$dynamicity[model_data$concept == "dar"] = "dynamic"

ggplot(model_data, aes(x = as.factor(dynamicity), y = slope, fill = as.factor(dynamicity))) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal() +
  scale_x_discrete(name = "Concept Dynamicity") +
  scale_y_continuous(name = "Model Slope (Coefficient / Max Degree)")

slope_model = lm(slope ~ as.factor(dynamicity), model_data)
summary(slope_model)

ggplot(model_data, aes(x = as.factor(dynamicity), y = rsquared, fill = as.factor(dynamicity))) +
  geom_boxplot(show.legend = FALSE) +
  theme_minimal() +
  scale_x_discrete(name = "Concept Dynamicity") +
  scale_y_continuous(name = "R-Squared of Concept Models")
  

rsq_model = (lm(rsquared ~ as.factor(dynamicity), model_data))
summary(rsq_model)

