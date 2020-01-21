filelist <- c(

"190705_5_cor_steps.txt"

) #Pig epo and GMPCPP


toobig <- 12 #nm

# main loop ############# open each file adn delete big foresteps

for (i in filelist[1:length(filelist)]) {

#setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/reanalyze")

meta <- readLines(i, n=6)

bthr3 <- read.table(i, skip = 6,sep=" ") 

e <- bthr3$amplitude < toobig

bthr3[which(!e)+1,"dwell"] <- NA

bthr4 <- bthr3[e,]

setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/reanalyze/processed")
write(meta, i, append=TRUE)
write.table(bthr4,i, append=TRUE)

}

