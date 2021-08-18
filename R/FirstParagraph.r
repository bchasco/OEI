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
Chinook <- contents[grep("Chinook",contents$`Common Name`),]
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
