setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis")
source("steps_and_dirs_compiled.R")

filelist <- c(pombe)
directory <- c(d_pombe)

layout(matrix(c(1,1,1,2), 1, 1))

title <- strsplit(directory,"/"); title <- unlist(title); title <- title[length(title)]

coll=1

#par(new=TRUE) #allows overlaying plots

par(mar=c(5,10,5,2))
layout(matrix(c(1,1,2), 3, 1, byrow=T))

#exclude false dwells
setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/")
falsedwells <- read.table("false_dwells.txt")
falsesteps <- read.table("false_steps.txt")

#

setwd(directory)

slopes <- c(); intercepts <- c(); c <- list(); dffit2 <- c()
minback <- -12
interval <- 1
ffrom <- 3
fto <- 8

# read in
bthr3 <- read.table(filelist[1], skip = 6,sep=" ") 
bthr3$file <- rep(as.character(read.table(filelist[1],nrow=1)[1,1]),nrow(bthr3))

for (i in filelist[2:length(filelist)]) {

bthr3_temp <- read.table(i, skip = 6,sep=" ")
bthr3_temp$file <- rep(as.character(read.table(i,nrow=1)[1,1]),nrow(bthr3_temp))
bthr3 <- rbind(bthr3, bthr3_temp)

}

bthr3 <- bthr3[!((bthr3$time %in% falsedwells$time) & (bthr3$tscore %in% falsedwells$tscore)),] #remove steps with false dwells
bthr3 <- bthr3[!((bthr3$time %in% falsesteps$time) & (bthr3$tscore %in% falsesteps$tscore)),] #remove false steps

par(mar=c(5,10,5,2))
par(new=TRUE) #allows overlaying plots


stepTypeFilter_list <- list (
	bthr3$amplitude>0, 					#forward
	bthr3$amplitude<0 & bthr3$amplitude> minback,   #backsteps
	bthr3$mean1 < 20 & bthr3$amplitude < 0		#detachments
 	#bthr3$amplitude < minback 				#detachments+big backsteps
	#bthr3$mean1 > 20 & bthr3$amplitude < 0 & bthr3$amplitude < -12 # big backsteps
	#bthr3$amplitude<0						#back+big+detachments
	

)

coll_list <- list ("black", "orange", "grey")

avD_list <- list()
nD_list <- list()


#########################  bin dwells by force for each step type ###########################


for (i in 1:length(stepTypeFilter_list) ) {
	coll <- coll_list[[i]]
	stepTypeFilter <- stepTypeFilter_list[[i]]

	setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/my_papers/backsteps/Fig3")
	source("plot_dwells_module1.R")
	moduleOutput <- plotDwells()
	avD_list[[i]] <- moduleOutput[[1]]
	nD_list[[i]] <- moduleOutput[[2]]
}

############################ bin dwells end #######################

avF <- ffrom:fto

mtext(title,3,padj=-1)
mtext("average dwells",3,padj=-1,adj=1)
mtext("Force, pN", 1,padj=+3)
mtext("Dwell time, s", 2,padj=-3)


axis(1)
axis(2)
box()

legend( 3,10,c("Forsteps", "Backsteps < 12 nm", "Detachments"),col=unlist(coll_list),pch=19)
args.legend = list(x = 10, y=1.18,cex=0.8),main=title)


################################# write out the numbers

par(mar=c(10,10,3,2))

a1 <- 2.2
a2 <- 12.1
#a2 <- 8
line_height <- 11
nn <- length(avF)-1

#plot(avF,c(1,2,3,4,5,12,1,2,3,2), axes=F, xlab="",ylab="",col="white")
plot(avF,c(1,2,3,4,5,12), axes=F, xlab="",ylab="",col="white")



if (0) {
for (i in nD_list) {
	mmm <- matrix(c(seq(a1,a2,by=(a2-a1)/nn),rep(line_height,length(avF))),length(avF),2)
	text(mmm,as.character(i),col=1,adj=1)
	line_height <- line_height-3
}

############### write out step types

mmm <- matrix( c(rep(0.5,3),5,8,11),3,2)
text(mmm, c("Detachments", "Backsteps", "Foresteps"),xpd=NA,adj=0,col=unlist(coll_list))
}

setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/my_papers/backsteps/Fig3/20191212")


dev.copy(pdf,"pombe.pdf", useDingbats=FALSE)
dev.off()


