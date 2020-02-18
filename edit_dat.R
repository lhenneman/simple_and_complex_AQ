library(data.table)
library( disperseR)

D <- data.table( units)
D <- D[ year %in% c( 2006, 2011), .( uID, Longitude, Latitude, year)]
D[, uID := gsub('\\*', '.', uID)]

dataset.an <- fread( 'dataset_annual.csv')
dataset.mo <- fread( 'dataset_monthly.csv')

dataset.an2 <- merge( D, dataset.an, by = c( 'uID', 'year'), all = T)

testuID <- unique( dataset.an2[is.na( Longitude)]$uID)
testuID.D <- data.table( uID1 = testuID, uID2 = paste0( testuID, '0'))

D2 <- merge( D, testuID.D, by.x = 'uID', by.y = 'uID2', all.x = T)
D2[!is.na( uID1), uID := uID1]
D2[,uID1 := NULL]

dataset.an3 <- merge( D2, dataset.an, by = c( 'uID', 'year'), all = T)[!is.na( state)]

fwrite( dataset.an3, 'dataset_annual.csv')
