### ==============================================================================

### RECLASIFICACIÓN DE MUERTES MATERNAS SEGÚN LA CIE-MM

#Realizo: Mtra. Samara Michelle Rodriguez Saavedra

### ==============================================================================

### Este script provee el diccionario oficial CIE-MM y una función generalizada para procesar bases de datos de mortalidad de cualquier país.

library(data.table)
library(stringr)

### 1) Diccionario Global CIE-MM (Mantener códigos completos a 4 dígitos)

DICC_CIEMM <- list(
  g1 = c("O000","O001","O002","O008","O009","O010","O011","O019","O020","O021","O028","O029",
         "O030","O031","O032","O033","O034","O035","O036","O037","O038","O039",
         "O040","O041","O042","O043","O044","O045","O046","O047","O048","O049",
         "O050","O051","O052","O053","O054","O055","O056","O057","O058","O059",
         "O060","O061","O062","O063","O064","O065","O066","O067","O068","O069",
         "O070","O071","O072","O073","O074","O075","O076","O077","O078","O079"),
  g2 = c("O11X","O110","O111","O112","O113","O114","O115","O116","O117","O118","O119",
         "O120","O121","O122", "O13X","O130","O131","O132","O133","O134","O135","O136","O137","O138","O139",
         "O140","O141","O142","O149", "O150","O151","O152","O159",
         "O16X","O160","O161","O162","O163","O164","O165","O166","O167","O168","O169"),
  g3 = c("O200","O208","O209","O432","O440","O441","O450","O458","O459","O460","O468","O469",
         "O670","O678","O679","O710","O711","O713","O714","O717","O720","O721","O722","O723"),
  g4 = c("O230","O231","O232","O233","O234","O235","O239","O411","O753",
         "O85X","O850","O851","O852","O853","O854","O855","O856","O857","O858","O912", # Se movió O859 y corregido sangrado
         "O860","O861","O862","O863","O864","O868","O910","O911","O859"),
  g5 = c("O211","O212","O223","O225","O228","O229","O244","O266","O269","O712","O715","O716","O718","O719",
         "O730","O731","O754","O758","O759","O871","O873","O879","O881","O882","O883","O888",
         "O900","O901","O902","O903","O904","O905","O908","O909"),
  g6 = c("O290","O291","O292","O293","O295","O296","O298","O299",
         "O740","O741","O742","O743","O744","O746","O747","O748","O749",
         "O890","O891","O892","O893","O895","O896","O898","O899"),
  g7 = c("O100","O101","O102","O103","O104","O109", "O240","O241","O242","O243","O249",
         "O980","O981","O982","O983","O984","O985","O986","O987","O988","O989",
         "O990","O991","O992","O993","O994","O995","O996","O997","O998"),
  parto = c("O800","O801","O808","O809", "O810","O811","O812","O813","O814","O815",
            "O820","O821","O822","O828","O829", "O830","O831","O832","O833","O834","O838","O839",
            "O840","O841","O842","O848","O849", "O870","O872","O878","O894",
            "O920","O921","O922","O923","O924","O925","O926","O927"),
  contrib_O4 = c("O080","O081","O082","O083","O084","O085","O086","O087","O088","O089",
                 "O210","O218","O219", "O220","O221","O222","O224", "O25X","O250","O251","O252","O253","O254","O255","O256","O257","O258","O259",
                 "O260","O261","O262","O263","O264","O265","O267","O268","O269", "O280","O281","O282","O283","O284","O285","O288","O289",
                 "O294", "O300","O301","O302","O308","O309","O310","O311","O312","O318", "O320","O321","O322","O323","O324","O325","O326","O328","O329",
                 "O330","O331","O332","O333","O334","O335","O336","O337","O338","O339", "O340","O341","O342","O343","O344","O345","O346","O347","O348","O349",
                 "O350","O351","O352","O353","O354","O355","O356","O357","O358","O359", "O360","O361","O362","O363","O364","O365","O366","O367","O368","O369",
                 "O40X","O400","O401","O402","O403","O404","O405","O406","O407","O408","O409", "O410","O418","O419", "O420","O421","O422","O429",
                 "O430","O431","O438","O439", "O470","O471","O479", "O48X","O480","O481","O482","O483","O484","O485","O486","O487","O488","O489",
                 "O60X","O600","O601","O602","O603", "O610","O611","O618","O619", "O620","O621","O622","O623","O624","O628","O629", "O630","O631","O632","O639",
                 "O640","O641","O642","O643","O644","O645","O648","O649", "O650","O651","O652","O653","O654","O655","O658","O659", "O660","O661","O662","O663","O664","O665","O668","O669",
                 "O680","O681","O682","O683","O688","O689", "O690","O691","O692","O693","O694","O695","O698","O699", "O700","O701","O702","O703","O709","O745",
                 "O750","O751","O752","O755","O756","O757"),
  contrib_noO_3 = c("A34","C58"),
  contrib_noO_4 = c("D392","F530")
)

### 2) Función de Reclasificación Universal

reclasificar_ciemm <- function(datos, col_causa, digitos = 3) {
  
  ### Copiar datos para evitar efectos secundarios en data.table original
  
  dt <- copy(as.data.table(datos))
  
  ### Normalizar columna causa (Remover caracteres raros y pasar a Mayúsculas)
  
  dt[, causa_limpia := str_replace_all(str_to_upper(get(col_causa)), "[^A-Z0-9]", "")]
  dt[, causa_limpia := substr(causa_limpia, 1, digitos)]
  dt[causa_limpia %in% c("", "NUL"), causa_limpia := NA_character_]
  
  ### Adaptar diccionario según la longitud de dígitos seleccionada (3 o 4)
  
  cod_recortado <- lapply(DICC_CIEMM, function(x) unique(substr(x, 1, digitos)))
  
  ### Construir conjuntos lógicos de códigos base
  
  mm_strict   <- unique(unlist(cod_recortado[c("g1","g2","g3","g4","g5","g6","g7")]))
  mm_contrib  <- unique(unlist(cod_recortado[c("contrib_O4","contrib_noO_3","contrib_noO_4")]))
  parto_codes <- unique(unlist(cod_recortado[["parto"]]))
  
  ### Regla de inclusión/exclusión adaptada
  
  mm_reclas <- unique(c(mm_strict, mm_contrib, substr(c("O96", "O97"), 1, digitos)))
  mm_reclas <- setdiff(mm_reclas, parto_codes)
  
  ### Filtrar únicamente los registros que califiquen como muerte materna
  
  maternas <- dt[!is.na(causa_limpia) & (causa_limpia %chin% mm_reclas)]
  
  ### Asignación jerárquica de grupos de la CIE-MM
  
  maternas[, DEF_MATERNA := fcase(
    causa_limpia %chin% cod_recortado$g1, "G1_termina_aborto",
    causa_limpia %chin% cod_recortado$g2, "G2_hipertensivos",
    causa_limpia %chin% cod_recortado$g3, "G3_hemorragia",
    causa_limpia %chin% cod_recortado$g4, "G4_infeccion",
    causa_limpia %chin% cod_recortado$g5, "G5_obstetricas",
    causa_limpia %chin% cod_recortado$g6, "G6_anestesia",
    causa_limpia %chin% cod_recortado$g7, "G7_no_obstetricas",
    causa_limpia == substr("O95", 1, digitos), "G8_desconocido",
    causa_limpia %chin% substr(c("O96","O97"), 1, digitos), "G9_tardia",
    causa_limpia %chin% mm_contrib, "contribuyentes",
    default = NA_character_
  )]
  
  ### Clasificación binaria OMS Estricta vs Ampliada
  
  maternas[, OMS_ESTRICTA := !is.na(DEF_MATERNA) & !(DEF_MATERNA %chin% c("G9_tardia", "contribuyentes"))] 
  
  return(maternas)
}

### 3) Vector de Etiquetas Legibles (Exportable)

ETIQUETAS_CIEMM <- c(
  "G1_termina_aborto" = "Embarazo terminado en aborto",
  "G2_hipertensivos"   = "Trastornos hipertensivos",
  "G3_hemorragia"     = "Hemorragia obstétrica",
  "G4_infeccion"      = "Infecciones relacionadas con embarazo, parto o puerperio",
  "G5_obstetricas"    = "Otras causas obstétricas directas",
  "G6_anestesia"      = "Complicaciones de la anestesia obstétrica",
  "G7_no_obstetricas"  = "Causas obstétricas indirectas",
  "G8_desconocido"    = "Causa obstétrica no especificada",
  "G9_tardia"         = "Muerte materna tardía",
  "contribuyentes"    = "Causas contribuyentes"
)

#Para cualquier sugerencia o aclaración de la clasificiacion, escriba al siguiente correo:
# smrodriguez@colmex.mx