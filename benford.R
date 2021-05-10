# benford.R
# Checking conformity with Benford's Law of leading and second digits for 
# reported figures -- party votes and invalid ballots -- at the admin unit.
# CC BY-SA. Arbër Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# ----
# Plot chart
PlotBenford(NULL, "Benford's Law", FALSE)


###############################
# LEADING DIGITS
# --------------
# Leading-digit PS
benfordPS <- benford(partyVotes$PS, number.of.digits = 1)

# Leading-digit plots
df <- data.frame(benfordPS[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "PS", TRUE)

# plot a big picture of analysis
plot(benfordPS)

par(mfrow = c(1, 1))
#plot of PS log-votes
hist(log10(partyVotes$PS), breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Histogram of PS vote (log)"), 
     xlab = "PS vote (log)")
rug(log10(partyVotes$PS), col = "gray")

# View suspects
partyVotesPS <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PS)
suspectsPS <- getSuspects(benfordPS, partyVotesPS)
view(suspectsPS)

# Leading-digit PD
benfordPD <- benford(partyVotes$PD, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordPD[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "PD", TRUE)

plot(benfordPD)

#plot of PD log-votes
hist(log10(partyVotes$PD), breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Histogram of PD vote (log)"), 
     xlab = "PD vote (log)")
rug(log10(partyVotes$PD), col = "gray")

partyVotesPD <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PD)
suspectsPD <- getSuspects(benfordPD, partyVotesPD)
view(suspectsPD)

# Leading-digit Other Parties
benfordOP <- benford(partyVotes$`Other Parties`, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordOP[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "OP", TRUE)

plot(benfordOP)
partyVotesOP <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Other Parties`)
suspectsOP <- getSuspects(benfordOP, partyVotesOP)
view(suspectsOP)

# Leading-digit Invalid Ballots
benfordIB <- benford(partyVotes$`Invalid Ballots`, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordIB[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "Invalid Ballots", TRUE)

plot(benfordIB)
partyVotesIB <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Invalid Ballots`)
suspectsIB <- getSuspects(benfordIB, partyVotesIB)
view(suspectsIB)

# Total eligible vote count
benfordTEV <- benford(partyVotes$`Total Eligible Votes`, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordTEV[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "Total Eligible", TRUE)

plot(benfordTEV)
partyVotesTEV <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Total Eligible Votes`)
suspectsIB <- getSuspects(benfordTEV, partyVotesTEV)
view(suspectsTEV)

# Total tally
benfordTV <- benford(partyVotes$`Total Votes`, number.of.digits = 1)
# Leading-digit plots
df <- data.frame(benfordTV[[2]]) # second position in list contains frequency
names(df) <- c("AdminUnit", "Votes", "Mantissa", "LeadingDigit")
PlotBenford(df, "Total", TRUE)

# Plot histogram of total votes (log) and overlay party vote curves
totV <- data.frame(log10(partyVotes$`Total Votes`))
names(totV) <- c("LogV")

psV <- data.frame(log10(partyVotes$PS))
names(psV) <- c("LogV")
pdV <- data.frame(log10(partyVotes$PD))
names(pdV) <- c("LogV")
opV <- data.frame(log10(partyVotes$`Other Parties`))
names(opV) <- c("LogV")
ibV <- data.frame(log10(partyVotes$`Invalid Ballots`))
names(ibV) <- c("LogV")

# Combine log-votes into a vector for plotting frequency lines against
# total log-vote histogram:
combined <- vector("numeric")
combined <- append(combined, psV$LogV)
combined <- append(combined, pdV$LogV)
combined <- append(combined, opV$LogV)
combined <- append(combined, ibV$LogV)
totV <- data.frame(combined)
names(totV) <- c("LogV")

nrBins <- 40
gp <- ggplot() +
  geom_histogram(bins = nrBins, totV , mapping = aes(x = LogV,
                             color = paste("Total log-vote histogram")),
                 fill = "white") +
  geom_freqpoly(bins = nrBins, psV, mapping = aes(x = LogV,
                                          color = "PS log-votes"), size = 0.9) +
  geom_freqpoly(bins = nrBins, pdV, mapping = aes(x = LogV,
                                    color = "PD log-votes"), size = 0.9) +
  geom_freqpoly(bins = nrBins, opV, mapping = aes(x = LogV,
                                   color = "OP log-votes"), size = 0.6) +
  geom_freqpoly(bins = nrBins, ibV, mapping = aes(x = LogV,
                                   color = "IB log-votes"), size = 0.3) +
  scale_color_manual(values=c("red2", "navy", "orange", "green4", "grey", "black")) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  xlab(paste("Log-vote")) +
  ylab("Frequency") +
  ggtitle(paste("Log-plot of total votes, party votes, and invalid ballots")) +
  theme(legend.title=element_blank(), legend.position = "bottom")

# plot gp
gp
# view plot data for analysis -- stored in $data[[i]]
histData <- ggplot_build(gp) # contains the histogram data by ggplot
dx <- histData$data[[5]]
view(dx)
view(ibV)

plot(benfordTV)
partyVotesTV <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Total Votes`)
suspectsIB <- getSuspects(benfordTV, partyVotesTV)
view(suspectsTV)

# FIRST TWO DIGITS
# --------------
# Two-digit PS
benfordPS <- benford(partyVotes$PS, number.of.digits = 2)
plot(benfordPS)
partyVotesPS <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PS)
suspectsPS <- getSuspects(benfordPS, partyVotesPS)
view(suspectsPS)

# Two-digit PD
benfordPD <- benford(partyVotes$PD, number.of.digits = 2)
plot(benfordPD)
partyVotesPD <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PD)
suspectsPD <- getSuspects(benfordPD, partyVotesPD)
view(suspectsPD)

# Two-digit Other Parties
benfordOP <- benford(partyVotes$`Other Parties`, number.of.digits = 2)
plot(benfordOP)
partyVotesOP <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Other Parties`)
suspectsOP <- getSuspects(benfordOP, partyVotesOP)
view(suspectsOP)

# Two-digit Invalid Ballots
benfordIB <- benford(partyVotes$`Invalid Ballots`, number.of.digits = 2)
plot(benfordIB)
partyVotesIB <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Invalid Ballots`)
suspectsIB <- getSuspects(benfordIB, partyVotesIB)
view(suspectsIB)

#####
# Testing Benford's Law w/ K-S:
# ----
# PS
benfordPS <- DiscreteBenfordKSTestWithVisuals(partyVotes$PS, "PS")
# Print results - graph and text - in files

benfordPS[[1]]
benfordPS[[2]]

# PD
benfordPD <- DiscreteBenfordKSTestWithVisuals(partyVotes$PD, "PD")
# Print results - graph and text - in files

benfordPD[[1]]
benfordPD[[2]]

# OP
benfordOP <- DiscreteBenfordKSTestWithVisuals(partyVotes$`Other Parties`, 
                                                "Other Parties")
# Print results - graph and text - in files

benfordOP[[1]]
benfordOP[[2]]

# IB
benfordIB <- DiscreteBenfordKSTestWithVisuals(partyVotes$`Invalid Ballots`, 
                                                "Invalid Ballots")
# Print results - graph and text - in files

benfordIB[[1]]
benfordIB[[2]]
