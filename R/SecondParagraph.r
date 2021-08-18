myYear <- 2020
strYear <- substr(as.character(myYear),3,4)

trawl <- getTable(uid=uid,
                  pwd=pwd,
                  schemaName = "crepo",
                  tableName = "Trawl Info")
trawl <- trawl[substr(trawl$`Station Code`,5,6)==strYear,]

contents <- getTable(uid=uid,
                          pwd=pwd,
                          schemaName = "crepo",
                     tableName = "Trawl Contents")
contents <- contents[contents$`Station Code`%in%trawl$`Station Code`,]
contents$effort <- trawl$`Trawling distance (km)`[match(contents$`Station Code`,trawl$`Station Code`)]
contents <- as.data.frame(contents)

transectID <- getTable(uid=uid,
                       pwd=pwd,
                       schemaName = "crepo",
                       tableName = "Transect Codes")

subs <- contents[contents$Age_ClassByLength=="subyearling",]
subs_ag_count <- aggregate(list(total=subs$Count), by = list(sp = subs$`Common Name`, station = subs$`Station Code`), sum)
subs_ag_count$effort <- trawl$`Trawling distance (km)`[match(subs_ag_count$station,trawl$`Station Code`)]
subs_ag_count$CPUE <- subs_ag_count$total/subs_ag_count$effort
subs_ag_count$transect <- transectCode$`Transect Name`[match(substr(subs_ag_count$station,7,8),transectCode$`Transect Code`)]
subs_ag_count <- subs_ag_count[order(-subs_ag_count$CPUE),]


yrlg <- contents[contents$Age_ClassByLength=='yearling',]
yrlg_ag_count <- aggregate(list(total=yrlg$Count), by = list(sp = yrlg$`Common Name`, station = yrlg$`Station Code`), sum)
yrlg_ag_count$effort <- trawl$`Trawling distance (km)`[match(yrlg_ag_count$station,trawl$`Station Code`)]
yrlg_ag_count$CPUE <- yrlg_ag_count$total/yrlg_ag_count$effort
yrlg_ag_count$transect <- transectCode$`Transect Name`[match(substr(yrlg_ag_count$station,7,8),transectCode$`Transect Code`)]
yrlg_ag_count <- yrlg_ag_count[order(-yrlg_ag_count$CPUE),]

ch_yrlg_ag_count <- yrlg_ag_count[yrlg_ag_count$sp=="Chinook salmon",]
co_yrlg_ag_count <- yrlg_ag_count[yrlg_ag_count$sp=="Coho salmon",]

Chinook <- ag_count[grep("Chinook",ag_count$sp),]
Coho <- contents[grep("Coho",contents$`Common Name`),]
Steelhead <- contents[grep("Steelhead",contents$`Common Name`),]
Chum <- contents[grep("Chum",contents$`Common Name`),]
Sockeye <- contents[grep("Sockeye",contents$`Common Name`),]
Salmon <- contents[grep("salmon",contents$`Common Name`),]

Ch_perc_sub <- sum(Chinook$Count[Chinook$Age_ClassByLength!="subadult/adult"])/sum(Chinook$Count)
Co_perc_sub <- sum(Coho$Count[Coho$Age_ClassByLength!="subadult/adult"])/sum(Coho$Count)
St_perc_sub <- sum(Steelhead$Count[Steelhead$Age_ClassByLength!="subadult/adult"])/sum(Steelhead$Count)
Chum_perc_sub <- sum(Chum$Count[Chum$Age_ClassByLength!="subadult/adult"])/sum(Chum$Count)
So_perc_sub <- sum(Sockeye$Count[Sockeye$Age_ClassByLength!="subadult/adult"])/sum(Sockeye$Count)
