library("openxlsx")

# datas <- read.xlsx(file.choose(), sheet = "Munka1", detectDates = TRUE) # "/Excel/2025_KSH__Emobiliti.xlsx"
# datas$Berendezés.üzembehelyezésének.dátuma <- as.Date(datas$Berendezés.üzembehelyezésének.dátuma, origin = "1899-12-30")

library("ROracle")
library("askpass")
library("tidytable")
library("tidygeocoder")
library("stringr")
source("Functions.R")

datas <- read.csv(paste0(getwd(), "/CSV/2025_KSH__Emobiliti.csv"), header = TRUE, sep = ";", colClasses = "character")
dim(datas)

colnames(datas) <- c("M003", "DQEA002", "DQEA003", "DQEA004", "DQEA005", "DQEA006", 
                     "DQEA007", "DQEA008", "DQEA009", "DQEA010", "DQEA011", "DQEA012", 
                     "DQEA013", "DQEA014", "DQEA015", "DQEA016", "DQEA017", "DQEA018", 
                     "DQEA019", "DQEA020", "DQEA021", "DQEA022", "DQEA023", "DQEA024", 
                     "DQEA025", "DQEA026", "DQEA027", "DQEA028", "DQEA029")

datas$TEV <- "2025"

datas$DQEA011 <- gsub("\\.", "-", datas$DQEA011)
datas$DQEA012 <- gsub("\\.", "-", datas$DQEA012)

datas$DQEA011 <- as.Date(datas$DQEA011)
datas$DQEA012 <- as.Date(datas$DQEA012)

Sys.setenv(TZ = "CET")
Sys.setenv(ORA_SDTZ = "CET")

password <- askpass()

drv <- Oracle()
con <- dbConnect(drv, username = Sys.getenv("USERNAME"), password = password, dbname = "emerald.ksh.hu")

res <- dbSendQuery(con, paste("select NEV, M009 from VT.F009_251231"))

M009_List <- fetch(res)
dbClearResult(res)
dim(M009_List) # 3200 sor és 2 oszlop

datas %>% left_join(M009_List, by = c("DQEA005" = "NEV")) -> datas

# datas <- datas %>% select(-address, -lat, -long)
columns_list(datas, ncol(datas))

rs <- dbSendQuery(con, paste0("insert into DQ.N_YM_2607_EMOBILITI_V25_E_V00(", columns, ") values (", values, ")"), data = datas)
dbCommit(con)
dbClearResult(rs)

dbDisconnect(con)


# koordináták javítása
datas$address <- paste0(datas$DQEA003, " ", datas$DQEA005, ", ", datas$DQEA006)
datas <- datas %>%
  mutate(address = str_remove(address, ",?\\s*HRSZ.*$")) %>%
  mutate(address = str_trim(address)) # HRSZ és utána jövő karakterek tisztítása
datas <- datas %>% geocode(address, method = "arcgis") # "osm"

# address_2 <- address_2 %>%
#   mutate(address = str_replace(address, "Margisztsziget", "Margitsziget"))

str(datas)
datas$DQEA007 <- gsub(",", "\\.", datas$DQEA007)
datas$DQEA008 <- gsub(",", "\\.", datas$DQEA008)

View(datas[(abs(as.numeric(datas$DQEA007) - as.numeric(datas$lat)) > 0.16 | abs(as.numeric(datas$DQEA008) - as.numeric(datas$long)) > 0.16) & abs(as.numeric(datas$DQEA008) - as.numeric(datas$long)) < 9, ])

datas <- datas %>%
  mutate(helyzet_ellenorzes = if_else(
    (abs(as.numeric(DQEA007) - as.numeric(lat)) > 0.16 | 
       abs(as.numeric(DQEA008) - as.numeric(long)) > 0.16) & 
      abs(as.numeric(DQEA008) - as.numeric(long)) < 9,
    "Javított", 
    "Megfelelő",
    missing = "Hiányzó adat" # Ha valamelyik koordináta NA, ezt írja be
  ))

write.xlsx(datas, "javított_koordináták.xlsx")

# talalt_cimek <- datas %>% reverse_geocode(lat = DQEA007, long = DQEA008, method = "arcgis", address = "teljes_cim")
reverse_geo(lat = 48.385188, long = 21.634325, method = "arcgis")
# 48.4  21.6 3980, Sátoraljaújhely, Sátoraljaújhelyi járás, Borsod-Abaúj-Zemplén, Ostromgyűrű köz, HUN
reverse_geo(lat = 47.533292, long = 19.052174, method = "arcgis")
# 47.5  19.1 VitalCenter Margitsziget, 1007, Margitsziget, Budapest, HUN

