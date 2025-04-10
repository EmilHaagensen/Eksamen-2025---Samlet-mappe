library(RMariaDB)

con = dbConnect(MariaDB(),
                user = "root",
                dbname = "Fodbolddata",
                host = "localhost",
                port = 3306,
                password = "nuh-uh"
                )

csv.folder = "data/"

csv.filer = list.files(csv.folder, pattern = "^wyscout.*\\.csv$", full.names = TRUE)

for (fil in csv.filer) { 
  
  filnavn = basename(fil)
  
  df = read.csv(fil, stringsAsFactors = F)
  
  tablenavn <- sub("^wyscout", "", tools::file_path_sans_ext(filnavn))
  
  dbWriteTable(con, name = tablenavn, value = df, row.names = F, overwrite = T)
  
  print(paste("Importeret tabel: ", tablenavn))
  
}

dbDisconnect()

print("færdig med at importere")


