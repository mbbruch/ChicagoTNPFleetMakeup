.libPaths(c("U:/R_Libs", .libPaths()))
library(data.table)
library(stringdist)
library(tidyr)
library(sjmisc)

cars <- fread("U:/Scripts/Inputs/Transportation_Network_Providers_-_Vehicles.csv")
cars <- cars[substr(MONTH_REPORTED,1,4)==2019]
annual.use <- cars[!is.na(NUMBER_OF_TRIPS) & NUMBER_OF_TRIPS>0,.(trips=sum(NUMBER_OF_TRIPS,na.rm=TRUE),cars=.N),by=MONTH_REPORTED][,.(trips=mean(trips),cars=mean(cars))][,.(trips.per.car=trips*12.0/cars)]$trips.per.car
cars[,':='(make=toupper(MAKE),model=toupper(MODEL))][,':='(MAKE=NULL,MODEL=NULL)]
models <- cars[,.(trips=sum(NUMBER_OF_TRIPS,na.rm=TRUE)),by=.(make,model,year=YEAR)][trips>0]

car.years <- cars[substr(MONTH_REPORTED,1,4)==2021,
                  .(trips=sum(NUMBER_OF_TRIPS,na.rm=TRUE)),by=(year=as.numeric(YEAR))]
car.years[,weighted.mean(year,w=trips)] #2014

economies <- fread("U:/Scripts/Inputs/vehicles.csv")
economies[,':='(make=toupper(make),model=toupper(model))]

models[,make:=gsub("&","AND",make)]
models[,make:=gsub("-"," ",make)]
models[,make:=gsub("CHEVY","CHEVROLET",make)]
models[,make:=gsub("MERCEDESBENZ","MERCEDES BENZ",make)]
models[,make:=gsub("HUNDAI","HYUNDAI",make)]
models[,make:=gsub("HYUNDIA","HYUNDAI",make)]
models[,make:=gsub("VOLKSWAGON","VOLKSWAGEN",make)]
models[,make:=gsub("VW","VOLKSWAGEN",make)]
models[,make:=gsub("^MERCEDES$","MERCEDES BENZ",make)]
models[,make:=gsub("RAM TRUCKS","RAM",make)]
models[,model:=gsub("CMAX","C MAX",model)]
models[,make:=trimws(make)]
models[,model:=gsub("&","AND",model)]
models[,model:=gsub("-"," ",model)]
models[,model:=gsub("MAZDA","",model)]
models[,model:=gsub("RAV 4","RAV4",model)]
models[,model:=gsub("CR V","CRV",model)]
models[,model:=gsub("HR V","HRV",model)]
models[,model:=gsub("VERSA NOTE","VERSA",model)]
models[,model:=trimws(model)]
models[make=="MERCEDES BENZ",model:=gsub("CLASS","",model)]
models[make=="BMW",model:=gsub("SERIES","",model)]
models[make=="INFINITI",model:=gsub("SEDAN","",model)]
models[,model:=gsub("CX ","CX",model)]
models[,model:=gsub("F 150","F150",model)]
models[year>2014,model:=gsub("RAV4 EV","RAV4 HYBRID",model)]
models[,model:=gsub("PRIUS HYBRID","PRIUS",model)]
models[,model:=gsub("RX HYBRID","RX 450H",model)]
models[,model:=gsub("HS HYBRID","HS 250H",model)]
models[,model:=gsub("GS HYBRID","GS 450H",model)]
models[,model:=gsub("ES HYBRID","ES 300H",model)]
models[,model:=gsub("M HYBRID","M35H",model)]
models[,model:=gsub("M35H HYBRID","M35H",model)]
models[,model:=gsub("PLUG IN","PLUGIN",model)]
models[,model:=gsub("COROLLA MATRIX","MATRIX",model)]
models[,model:=gsub("323","3",model)]
models[,model:=gsub("626","6",model)]
models[make=="FIAT",model:=gsub("500 X","500X",model)]
models[make=="TOYOTA",model:=gsub("C HR","CHR",model)]
models[make=="TOYOTA",model:=gsub("4 RUNNER","4RUNNER",model)]
models[,model:=gsub("IONIQ HYBRID","IONIQ BLUE",model)]
models[,model:=trimws(model)]
models <- models[!(make %in% c("MV AGUSTA","AKT","AM GENERAL"))]

economies[,make:=gsub("&","AND",make)]
economies[,make:=gsub("-"," ",make)]
economies[,make:=gsub("CHEVY","CHEVROLET",make)]
economies[,make:=gsub("MERCEDESBENZ","MERCEDES BENZ",make)]
economies[,make:=gsub("^MERCEDES$","MERCEDES BENZ",make)]
economies[,make:=trimws(make)]
economies[,model:=gsub("&","AND",model)]
economies[,model:=gsub("-"," ",model)]
economies[,model:=gsub("CX ","CX",model)]
economies[,model:=gsub("RAV 4","RAV4",model)]
economies[,model:=gsub("CR V","CRV",model)]
economies[,model:=gsub("HR V","HRV",model)]
economies[,model:=gsub("F 150","F150",model)]
economies[,model:=gsub("500 X","500X",model)]
economies[,model:=gsub("PLUG IN","PLUGIN",model)]
economies[,model:=gsub("IONIQ HYBRID","IONIQ BLUE",model)]
economies[make=="TOYOTA",model:=gsub("C HR","CHR",model)]
economies[make=="TOYOTA",model:=gsub("4 RUNNER","4RUNNER",model)]
economies[,model:=trimws(model)]

models <- models[,.(trips=sum(trips,na.rm=TRUE)),by=.(make,model,year)][trips>0]

combos <- crossing(models[,.(make,model,year,trips)],economies[,.(make2=make,model2=model,year2=year,
                                                                  combined=comb08,city=city08,hwy=highway08)])
combos <- data.table(combos)
combos <- combos[year==year2][,year2:=NULL]
combos[,':='(match.make=(make==make2))]
combos <- combos[match.make==TRUE][,':='(match.make=NULL,make2=NULL)]
combos[,':='(match.model=(model==model2))]
combos[,':='(model.1.in.2=(str_detect(model2,model)))]
combos[,':='(model.2.in.1=(str_detect(model,model2)))]
combos[,':='(best.match.model=max(as.numeric(match.model))),by=.(make,model,year)]
combos[,':='(best.model.1.in.2=max(as.numeric(model.1.in.2))),by=.(make,model,year)]
combos[,':='(best.model.2.in.1=max(as.numeric(model.2.in.1))),by=.(make,model,year)]
combos <- combos[!(best.match.model & !match.model)]
combos <- combos[match.model | !(best.model.1.in.2 & !model.1.in.2)]
combos[,matched.so.far:=max(match.model | model.1.in.2),by=.(make,model,year)]
combos[,':='(model.1.word.1=str_extract(model,'[A-Za-z0-9]+'),model.2.word.1=str_extract(model2,'[A-Za-z0-9]+'))]
combos[,':='(match.model.word1=(model.1.word.1==model.2.word.1))]
combos[,':='(best.match.word1=max(as.numeric(match.model.word1))),by=.(make,model,year)]
combos <- combos[match.model | model.1.in.2 | !(best.match.word1 & !match.model.word1)]
combos[,matched.so.far:=max(match.model | model.1.in.2 | match.model.word1),by=.(make,model,year)]
combos <- combos[,.(city=mean(city,na.rm=TRUE),hwy=mean(hwy,na.rm=TRUE),combined=mean(combined,na.rm=TRUE)),
       by=.(make,model,year,matched.so.far,trips)]

avg <- combos[matched.so.far==TRUE,weighted.mean(city,w=trips,na.rm=TRUE)]
