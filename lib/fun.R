# fun.R
# Function definitions.
# CC BY-SA. Arbër Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# Execute normality tests & related plots
# for continuous (isDiscrete = 0) or discrete data
NormTestsWithVisuals <- function(vecData, lblData, isDiscrete) {
  # Density chart
  grDensity <- ggdensity(vecData, 
            main = paste("Density plot of " , lblData , " Votes"),
            xlab = paste(lblData , 
                         " vote proportions"))
  
  # QQ chart
  grQQ <- ggqqplot(vecData)
  
  # Plot the K-S test results as S-curves
  # EDF vector:
  vecEDFlength <- length(vecData) # number of variables in vecData
  # Create vecEDF as the EDF of KS Test
  vecEDF <- vector(mode = "numeric", length = vecEDFlength)
  for (i in 1:vecEDFlength) {
    vecEDF[i] <- as.double(i)/as.double(vecEDFlength)
  }
  
  # sort data ascending
  vecData <- sort(vecData, decreasing = FALSE)
  # Build CDF of our data
  vecDataFx <- pnorm(vecData, mean(vecData), sd(vecData))
  
  # create a data set with the three vectors we wish to plot
  # vecData should be on our x-axis and the rest are our CDF curves
  frmEDF <- data.frame(vecData, vecEDF)
  names(frmEDF) <- c("p", "cdf")
  frmFx <- data.frame(vecData, vecDataFx)
  names(frmFx) <- c("p", "cdf")
  
  # Find the x_0 and y_0, y_1 coordinates for D_max = max|Fx - EDF|
  maxD <- abs(frmEDF$cdf[1] - frmFx$cdf[1])
  pos <- 1
  for (i in 2:vecEDFlength) {
    if (abs(frmEDF$cdf[i] - frmFx$cdf[i]) > maxD) {
      maxD = abs(frmEDF$cdf[i] - frmFx$cdf[i])
      pos <- i
    }
  }
  # max D is at position pos
  x0 <- vecData[pos]
  y0 <- frmEDF$cdf[pos]
  y1 <- frmFx$cdf[pos]  
  
  # Plot the curves & D_max
  grKS <- ggplot() +
    geom_line(frmEDF, mapping = aes(x = p, y = cdf, color = "EDF")) +
    geom_line(frmFx, mapping = aes(x = p, y = cdf, color = 
                                     paste("F(", lblData, ")"))) +
    scale_color_manual(values=c("navy", "orange")) +
    xlab(paste(lblData, " Vote Proportions")) +
    ylab("CDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1], linetype = "Dmax"),
                 linetype = "solid", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=2) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=2) +
    ggtitle(paste("K-S Normality Test: ", lblData)) +
    theme(legend.title=element_blank(), legend.position = "bottom")
  
  resultKS <- vector(mode = "numeric", length = length(vecData))
  resultKSL <- vector(mode = "numeric", length = length(vecData))
  if (isDiscrete == 0) { # continuous distributions
    # Run the default KS test
    resultKS <- ks.test(vecData, "pnorm", mean(vecData), sd(vecData))
    
    # Run the Lilliefors normality test
    resultKSL <- LillieTest(vecData) 
    
  } else { # discrete distributions
    # Run the ks.boot function from lib Matching for the discrete KS test
    resultKS <- ks.boot(vecData, pnorm(length(vecData)), nboots = 1000, 
                        alternative = "two.sided")
  }
  
  # Run the Shapiro normality test
  resultShap <- shapiro.test(vecData)
  
  # Save results in a return list
  # NB: if discrete, resultKSL returns an empty vector
  results <- list(grDensity, grQQ, grKS, resultKS, resultKSL, resultShap)
  
  return(results)
}

# Execute two-sample discrete K-S test & related plots for Benford's Law
# sample1 - vector of integers
DiscreteBenfordKSTestWithVisuals <- function(sample1, lblData) {
  
  sample1 <- ExtractLeadingDigitVector(sample1)
  
  # Group sample1 by values
  sample1 <- tabulate(sample1, nbins = 9)
  # sort sample1 asc
  sample1 <- sort(sample1, decreasing = FALSE)
  # compute proportions
  sum1 <- as.double(sum(sample1))
  for (i in 1:9) {
    sample1[i] = sample1[i]/sum1
  }

  # save relative frequencies in a vector to use as x-axis in ggplot
  xsample1 <- sample1

  # build CDF
  for (i in 2:9) {
    sample1[i] = sample1[i] + sample1[i-1]
  }
  
  # Build CDF per Benford's Law
  sample2 <- vector(mode = "numeric", length = 9)
  for (i in 1:9) {
    sample2[i] = as.double(log10(1+1/i)) # distributions
  }
  # sort sample2 asc
  sample2 <- sort(sample2, decreasing = FALSE)
  sum2 = as.double(sum(sample2))
  # Benford's CDF
  for (i in 2:9) {
    sample2[i] = sample2[i] + sample2[i-1]
  }
  
  Fsample1 <- sample1
  Fsample2 <- sample2

  # Find the x_0 and y_0, y_1 coordinates for D_max = max|Fx - EDF|
  maxD <- abs(Fsample1[1] - Fsample2[1])
  pos <- 1
  for (i in 2:length(Fsample1)) {
    if (abs(Fsample1[i] - Fsample2[i]) > maxD) {
      maxD = abs(Fsample1[i] - Fsample2[i])
      pos <- i
    }
  }
  # max D is at position pos
  x0 <- xsample1[pos]
  y0 <- Fsample1[pos]
  y1 <- Fsample2[pos]  
  
  # create frames in order to work with ggplot
  frame1 <- data.frame(Fsample1)
  frame2 <- data.frame(Fsample2)
  # Plot the curves & D_max
  grKS <- ggplot() +
    geom_line(frame1, mapping = aes(x = xsample1, y = Fsample1, color = 
                                      paste("F(", lblData, ")"))) +
    geom_line(frame2, mapping = aes(x = xsample1, y = Fsample2, color =
                                     "F(Benford's Law)")) +
    scale_color_manual(values=c("navy", "orange")) +
    xlab(paste(lblData, " Vote Proportions")) +
    ylab("CDF") +
    #geom_line(size=1) +
    geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                 linetype = "solid", color = "red") +
    geom_point(aes(x = x0[1] , y= y0[1]), color="red", size=2) +
    geom_point(aes(x = x0[1] , y= y1[1]), color="red", size=2) +
    ggtitle(paste("Discrete Two-Sample K-S Test: ", lblData)) +
    theme(legend.title=element_blank(), legend.position = "bottom")

  # Run the ks.boot function from lib Matching for two-sample discrete KS test
  resultKS <- ks.boot(Fsample1, Fsample2, nboots = 1000, 
                      alternative = "two.sided")

  # Save results in a return list
  results <- list(grKS, resultKS)
  
  return(results)
}

# Extract & return a vector of leading digits from a given integer vector
ExtractLeadingDigitVector <- function(vecData) {
  
  vecLeading <- vector(mode = "integer", length = length(vecData))
  
  for (i in 1:length(vecData)) {
    vecLeading[i] = as.integer(substr(vecData[i], 1, 1))
  }

  return(vecLeading)
}

# Check for possible distribution candidates for a given vector
FitOtherDistributions <- function(vecData, lblData, isDiscrete) {
  datPlots <- descdist(vecData, discrete = isDiscrete)
  dtlognorm <- fitdist(vecData, distr = "lnorm")
  dtnorm <- fitdist(vecData, distr = "norm")
  dtweibull <- fitdist(vecData, distr = "weibull")
  # ignore beta-fit for PS/PD ratio:
  dtbeta <- dtweibull
  if (lblData != "PS/PD") {
    dtbeta <- fitdist(vecData, distr = "beta")
  }
  # plot(dtlognorm)
  # plot(dtnorm)
  # plot(dtweibull)
  # plot(dtbeta)
  
  # find best fit
  dtDistr <- list(dtlognorm, dtnorm, dtweibull, dtbeta)
  
  # Check best fit using Akaike values
  bestFit <- dtDistr[[1]]
  pos <- 1
  for (i in 2:length(dtDistr)) {
    if (dtDistr[[i]]$aic < bestFit$aic) {
      bestFit <- dtDistr[[i]]
      pos <- i
    }
  }
  
  results <- list(datPlots, bestFit, pos, bestFit$aic)
  
  return (results)
}
