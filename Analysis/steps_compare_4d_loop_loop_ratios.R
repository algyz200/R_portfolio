# this is improved step analysis code:
# 1) binning by function rather than for loop with bootstrap error
# 2) bootstrap error

# parts of the code:
#1. Data
#2. Dif plots loop
#3. File batch loop

library("boot")

###### 1. data

setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis")
source("steps_and_dirs_compiled.R")

##############################

par(mfrow = c(2, 2))

for (iii in 1:4) {
  #2. Dif plots loop ##########
  
  ######### binning "loop"
  
  filelist_list <- list(gdp, taxol)
  directory_list <- list(d_gdp, d_taxol)
  col_list <- c(3, 4)
  #col_list <- c("cadetblue3",4)
  
  for (o in 1:length(filelist_list)) {
    #3. File batch loop ########
    if (o == length(filelist_list))
      par(new = TRUE) #allows overlaying plots
    
    filelist <- unlist(filelist_list[o])
    directory <- unlist(directory_list[o])
    coll <- col_list[o]
    
    title <-
      strsplit(directory, "/")
    title <- unlist(title)
    title <- title[length(title)]
    
    #coll=4
    
    #exclude false dwells
    setwd("C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/")
    falsedwells <- read.table("false_dwells.txt")
    falsesteps <- read.table("false_steps.txt")
    
    #
    
    setwd(directory)
    
    slopes <- c()
    intercepts <- c()
    c <- list()
    dffit2 <- c()
    minback <- -12
    interval <- 1
    ffrom <- 2
    fto <- 9
    
    # read in
    bthr3 <- read.table(filelist[1], skip = 6, sep = " ")
    bthr3$file <-
      rep(as.character(read.table(filelist[1], nrow = 1)[1, 1]), nrow(bthr3))
    
    for (i in filelist[2:length(filelist)]) {
      bthr3_temp <- read.table(i, skip = 6, sep = " ")
      bthr3_temp$file <-
        rep(as.character(read.table(i, nrow = 1)[1, 1]), nrow(bthr3_temp))
      bthr3 <- rbind(bthr3, bthr3_temp)
      
    }
    
    bthr3 <-
      bthr3[!((bthr3$time %in% falsedwells$time) &
                (bthr3$tscore %in% falsedwells$tscore)), ] #remove steps with false dwells
    bthr3 <-
      bthr3[!((bthr3$time %in% falsesteps$time) &
                (bthr3$tscore %in% falsesteps$tscore)), ] #remove false steps
    
    ############ 3. binning loop
    
    force <- seq(ffrom, fto, by = interval)
    bin_fun <-
      function (x, y)
        y >= x - interval / 2 &
      y < x + interval / 2 # x - force, y - data vector
    
    ratio_function <- function (bthr3, j) {
      data <- bthr3[j, ]
      
      f_filt <- data$amplitude > 0 #forward steps
      b_filt <- data$amplitude < 0 & data$amplitude > minback #backsteps
      d_filt <- data$mean1 < 20 & data$amplitude < 0 # Detachments only
      bs_filt <-
        data$mean1 > 20 &
        data$amplitude < 0 & bthr3$amplitude < minback #backslips only
      all_filt <- data$amplitude < 100 #all steps
      
      #num_list <- list(f_filt,b_filt,bs_filt,d_filt)
      
      num_list <-
        list(f_filt, f_filt,               b_filt,        b_filt | bs_filt)
      
      den_list <- list(b_filt, b_filt |
                         bs_filt | d_filt, d_filt,        d_filt)
      
      numerator <- num_list[[iii]]
      #denominator <- all_filt
      denominator <- den_list[[iii]]
      
      
      v1 <- data[numerator, ]$force
      v2 <- data[denominator, ]$force
      
      unlist(lapply(lapply(force, function(i)
        bin_fun(i, v1))
        , sum)) / unlist(lapply(lapply(force, function(i)
          bin_fun(i, v2))
          , sum))
    } #ratio fun ends
    
    ###############
    
    ratio <- ratio_function(bthr3, 1:nrow(bthr3))
    
    boot_error <- boot(bthr3, ratio_function, R = 100, stype = "i")
    boot_error_sd <- apply(boot_error$t, 2, sd)
    boot_error_sd[boot_error$t0 == 0] <- NA
    
    error <- boot_error_sd
    
    lower <- ratio - error
    upper <- ratio + error
    lower[lower < 0] <- 0.0001
    
    #plot(force, ratio, type="p", col = coll, xlim=c(ffrom,fto),ylim=c(0.001,1), xlab="Force, pN", ylab="",pch=19,cex=1.2,log="y")
    plot(
      force,
      ratio,
      type = "p",
      col = coll,
      xlim = c(ffrom, fto),
      ylim = c(0.01, 100),
      xlab = "Force, pN",
      ylab = "",
      pch = 19,
      cex = 1.2,
      log = "y"
    )
    
    
    
    arrows(
      ffrom:fto,
      lower,
      ffrom:fto,
      upper,
      length = 0.05,
      angle = 90,
      code = 3,
      col = coll,
      lwd = 3
    )
    
    setwd(
      "C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/Dkin_PigMT/subtilisin/boot"
    )
    
    #mtext_list <- c("Forward step probability (log)","Backstep probability (log)",
    #"Big Backstep probability (log)","Detachment probability (log)")
    mtext_list <- c("F/B", "F/(B+BS+D)", "B/D", "(B+BS)/D")
    mtext(mtext_list[iii], 2, 3, cex = 0.8)
    
  } # 3. file batch loop end
  
  plotTitle <- "gdp_vs_taxol"
  
  mtext(plotTitle)
}     #2. Dif plots loop end


setwd(
  "C:/Users/algis/Desktop/Algis/Algis_UoW/analysis/compare/ratios/20191213_ratios"
)

dev.copy(pdf, paste(plotTitle, "pdf", sep = "."), useDingbats = FALSE)
dev.off()
