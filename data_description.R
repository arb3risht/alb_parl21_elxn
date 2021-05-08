# data_description.R
# Describe data visually and numerically using various statistical functions.
# CC BY-SA. Arbër Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# Output file locations
partyVotesRawByAdminUnit <- "out/party_votes_raw_admin_unit.csv"
partyVotesByAdminUnit <- "out/party_votes_admin_unit.csv"
partyVotePByAdminUnit <- "out/party_votes_p_admin_unit.csv"
descripStatsFile <- "out/descript_stat.csv"
partyVotesByDistrictFile <- "out/party_votes_by_district.csv"
partyVotesByMunicipalityFile <- "out/party_votes_by_municipality.csv"

# Create a total columns for each administrative unit: TotalEligibleVotes as
# the sum of reported party votes, and TotalVotes as the latter sum plus
# invalid ballots. We will use the partyVotesRaw data set in our normality test 
# for party votes at each administrative unit, among other analyses.
partyVotesRaw <- within(partyVotesRaw, 
                        TotalEligibleVotes <- PS + PD + LSI + `Other Parties`)
partyVotesRaw <- within(partyVotesRaw, 
                        TotalVotes <- PS + PD + LSI + `Other Parties` 
                        + `Invalid Ballots`)
# Add turnout counts as Eligible Voters - Total Votes
partyVotesRaw <- within(partyVotesRaw, 
                        Turnout <- TotalVotes / `Eligible Voters`)
# write the data set out
write_csv(partyVotesRaw, partyVotesRawByAdminUnit, na = "NA", append = FALSE)

# Combine LSI + Other Parties into one vector due to their relative
# insignificance to PS and PD - the two major parties
partyVotes <- data.frame(partyVotesRaw$District,
                         partyVotesRaw$Municipality,
                         partyVotesRaw$`Administrative Unit`,
                         partyVotesRaw$PS,
                         partyVotesRaw$PD,
                         partyVotesRaw$LSI + partyVotesRaw$`Other Parties`,
                         partyVotesRaw$`Invalid Ballots`,
                         partyVotesRaw$`Eligible Voters`,
                         partyVotesRaw$TotalEligibleVotes,
                         partyVotesRaw$TotalVotes,
                         partyVotesRaw$Turnout)
names(partyVotes) <- c("District", "Municipality", "Administrative Unit", "PS", 
                       "PD", "Other Parties", "Invalid Ballots", 
                       "Eligible Voters", "Total Eligible Votes", 
                       "Total Votes", "Turnout")
# write the data set out
write_csv(partyVotes, partyVotesByAdminUnit, na = "NA", append = FALSE)

# Summarize data set
summary(partyVotes)
# describe() requires package `psych`
describe(partyVotes)

# Compute empirical probabilities for eligible votes for each party,
# invalid ballot/total votes , turnout/ eligible voters
partyVotesP <- data.frame(partyVotes$District,
                          partyVotes$Municipality,
                          partyVotes$`Administrative Unit`,
                          partyVotes$PS / partyVotes$`Total Eligible Votes`,
                          partyVotes$PD / partyVotes$`Total Eligible Votes`,
                          partyVotes$PS / partyVotes$PD,
                          partyVotes$`Other Parties` / 
                            partyVotes$`Total Eligible Votes`,
                          partyVotes$`Invalid Ballots` / 
                            partyVotes$`Total Votes`,
                          partyVotes$`Eligible Voters`,
                          partyVotes$`Total Eligible Votes`,
                          partyVotes$`Total Votes`,
                          partyVotes$Turnout)
names(partyVotesP) <- c("District", "Municipality", "Administrative Unit", "pPS", 
                       "pPD", "pPSPD", "pOP", "pInvalid", 
                       "Eligible Voters", "Total Eligible Votes", 
                       "Total Votes", "pTurnout")
# write the data set out
write_csv(partyVotesP, partyVotePByAdminUnit, na = "NA", append = FALSE)
# Quick look at some stats
summary(partyVotesP)

# Plot densities of PS, PD, OP, Turnout, and Invalid:
# in a 3x2 grid:
par(mfrow = c(3, 2))

# build a common x-axis range for the party plots
xrange <- range(c(partyVotesP$pPS, partyVotesP$pPD, partyVotesP$pOP) )

hist(partyVotesP$pPS, breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Histogram of PS vote share"), xlim = xrange, 
     xlab = "PS vote share")
lines(density(partyVotesP$pPS), col = "navy", lwd = 2)
rug(partyVotesP$pPS, col = "gray")

hist(partyVotesP$pTurnout, breaks = "FD", freq = FALSE, col = "#56bd70",
     main = paste("Histogram of turnout rates"),
     xlab = "Turnout rate")
lines(density(partyVotesP$pTurnout), col = "navy", lwd = 2)
rug(partyVotesP$pTurnout, col = "gray")

hist(partyVotesP$pPD, breaks = "FD", freq = FALSE, col = "orange", 
     main = paste("Histogram of PD vote share"), xlim = xrange,
     xlab = "PD vote share")
lines(density(partyVotesP$pPD), col = "navy", lwd = 2)
rug(partyVotesP$pPD, col = "gray")

hist(partyVotesP$pInvalid, breaks = "FD", freq = FALSE, col = "#FE6F5E", 
     main = paste("Histogram of invalid ballot rates"), 
     xlab = "Invalid ballot rate")
lines(density(partyVotesP$pInvalid), col = "navy", lwd = 2)
rug(partyVotesP$pInvalid, col = "gray")

hist(partyVotesP$pOP, breaks = "FD", freq = FALSE, col = "orange", 
     main = paste("Histogram of other parties' vote shares"),
     xlim = xrange, xlab = "Other parties' vote share")
lines(density(partyVotesP$pOP), col = "navy", lwd = 2)
rug(partyVotesP$pOP, col = "gray")

# plot a normal curve for reference
plot(dnorm, -3, 3, col = "navy", lwd = 3, 
     main = "Standard Normal Distribution",
     xlab = "Z",
     ylab = "Density"
)

# reset chart area
par(mfrow = c(1, 1))

# Group aggregated votes by district to look at the constituency level
votesGroupedByDistrict <- partyVotes %>% group_by(District) %>% 
  summarize(PS = sum(PS), PD = sum(PD), `Other Parties` = sum(`Other Parties`),
            `Invalid Ballots` = sum(`Invalid Ballots`),
            `Eligible Voters` = sum(`Eligible Voters`),
            `Total Eligible Votes` = sum(`Total Eligible Votes`),
            `Total Votes` = sum(`Total Votes`), 
            Turnout = sum(`Total Votes`) / sum(`Eligible Voters`))
# Add some computed empirical probabilities:
votesGroupedByDistrict <- within(votesGroupedByDistrict, 
                                 pPS <- PS / `Total Eligible Votes`)
votesGroupedByDistrict <- within(votesGroupedByDistrict,
                                 pPD <- PD / `Total Eligible Votes`)
votesGroupedByDistrict <- within(votesGroupedByDistrict,
                                 pOP <- `Other Parties` / 
                                   `Total Eligible Votes`)
votesGroupedByDistrict <- within(votesGroupedByDistrict,
                                 pInvalid <- `Invalid Ballots` 
                                 / `Total Eligible Votes`)
votesGroupedByDistrict <- within(votesGroupedByDistrict,
                                 pTurnout <- Turnout)
# Print the district-level data set for later reference
write_csv(votesGroupedByDistrict, partyVotesByDistrictFile, na = "NA", 
          append = FALSE)

# Some insights
# ----
# Summary Table - create empty frame with two columns
summaryTable <- data.frame(character(0), numeric(0))
names(summaryTable) <- c("Data", "Result")

# Add data results to the frame:
newRow <- c("Eligible Voters", sum(partyVotes$`Eligible Voters`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Total Votes Cast", sum(partyVotes$`Total Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Total Eligible Votes", sum(partyVotes$`Total Eligible Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Invalid Ballots", 1.0 - sum(partyVotes$`Total Eligible Votes`) / 
              sum(partyVotes$`Total Votes`))       
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Turnout", sum(partyVotes$`Total Votes`) / 
              sum(partyVotes$`Eligible Voters`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PS Tally", sum(partyVotes$PS)) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PS %-age", sum(partyVotes$PS) / 
              sum(partyVotes$`Total Eligible Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PS Seats", 74) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PD Tally", sum(partyVotes$PD)) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PD %-age", sum(partyVotes$PD) / 
              sum(partyVotes$`Total Eligible Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("PD Seats", 59) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Other Parties Tally", sum(partyVotes$`Other Parties`)) 
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Other Parties %-age", sum(partyVotes$`Other Parties`) / 
              sum(partyVotes$`Total Eligible Votes`))
summaryTable[nrow(summaryTable)+1, ] <- newRow
newRow <- c("Other Parties Seats", 7) 
summaryTable[nrow(summaryTable)+1, ] <- newRow

view(summaryTable)
write_csv(summaryTable, descripStatsFile, na = "NA", append = FALSE)
# ----
# Largest and smallest turnouts were in districts:
minTurnout <- votesGroupedByDistrict[which(votesGroupedByDistrict$pTurnout == 
                               min(votesGroupedByDistrict$pTurnout)), ]
maxTurnout <- votesGroupedByDistrict[which(votesGroupedByDistrict$pTurnout == 
                               max(votesGroupedByDistrict$pTurnout)), ]
view(minTurnout)
view(maxTurnout)

# ----
# Largest and smallest relative invalid ballot proportions were in districts:
minInvalid <- votesGroupedByDistrict[which(votesGroupedByDistrict$pInvalid == 
                               min(votesGroupedByDistrict$pInvalid)), ]
maxInvalid <- votesGroupedByDistrict[which(votesGroupedByDistrict$pInvalid == 
                               max(votesGroupedByDistrict$pInvalid)), ]
# Total invalid ballots & proportion
totalInvalidBallots <- sum(votesGroupedByDistrict$`Invalid Ballots`)
pInvalidBallots <- sum(votesGroupedByDistrict$`Invalid Ballots`) / 
                      sum(votesGroupedByDistrict$`Total Votes`)
view(minInvalid)
view(maxInvalid)
totalInvalidBallots
pInvalidBallots
# ----

# Is the observed ~5% invalid ballot unusual, given prior election results?
# Let's test the hypotheses:
#   H_0: We expect around 2% invalid ballots
#        (The observed 5% invalid ballot proportion is not unusual.)
#   H_A: We got more than 2% invalid ballots
#        (The observed 5% invalid ballot proportion is unusual, given 
#        previous elections' results averaging at ~2%.)
# Since invalid ballots were normally distributed at 95% conf. level, we can
# use the z-scores to test the hypotheses above. Let b denote the proportion
# of invalid ballots. Then, if [1 - P(b < 5%)] <= p-value of 5%, we reject H_0
# in favor of H_A. This is a dichotomous experiment with p_success & failure, 
# where p_success = expected, i.e. our historical control proportion.

# We expect 2% of total votes being invalid ballots based on prior elections
expected <- 0.02

# standard deviation of sample proportion from these elections
# given the fact that invalid ballots were found to be normally distributed
stdev <- sd(partyVotesP$pInvalid) / sqrt(length(partyVotesP$pInvalid))

# verify the sample is large enough to warrant use of normal distribution
# by confirming that the following interval is within [0, 1]
paste("[", expected - 3*stdev, expected + 3*stdev, "]")

# z-score
z <- (pInvalidBallots - expected) / stdev

# P(b >= z) = 1 - P(b < z):
pValue <- 1 - pnorm(z, mean = 0, sd = 1, lower.tail = TRUE)
pValue

# confidence interval @95%
lInt <- expected - 1.96*stdev
rInt <- expected + 1.96*stdev
paste("C.I. at 95%: (", lInt, ", ", rInt, 
      "); Observed invalid ballots:", pInvalidBallots)

if (pInvalidBallots < lInt | pInvalidBallots > rInt) {
  paste("Reject H_0 in favor of H_A")
} else {
  paste("Failed to Reject H_0")
}
# ----

# Group aggregated votes by municipality to look at that level
votesGroupedByMunicipality <- partyVotes %>% group_by(Municipality) %>% 
  summarize(PS = sum(PS), PD = sum(PD), `Other Parties` = sum(`Other Parties`),
            `Invalid Ballots` = sum(`Invalid Ballots`),
            `Eligible Voters` = sum(`Eligible Voters`),
            `Total Eligible Votes` = sum(`Total Eligible Votes`),
            `Total Votes` = sum(`Total Votes`), 
            Turnout = sum(`Total Votes`) / sum(`Eligible Voters`))
# Add some computed empirical probabilities:
votesGroupedByMunicipality <- within(votesGroupedByMunicipality, 
                                 pPS <- PS / `Total Eligible Votes`)
votesGroupedByMunicipality <- within(votesGroupedByMunicipality,
                                 pPD <- PD / `Total Eligible Votes`)
votesGroupedByMunicipality <- within(votesGroupedByMunicipality,
                                 pOP <- `Other Parties` / 
                                   `Total Eligible Votes`)
votesGroupedByMunicipality <- within(votesGroupedByMunicipality,
                                 pInvalid <- `Invalid Ballots` 
                                 / `Total Eligible Votes`)
votesGroupedByMunicipality <- within(votesGroupedByMunicipality,
                                 pTurnout <- Turnout)

# Print the municipality-level data set for later reference
write_csv(votesGroupedByMunicipality, partyVotesByMunicipalityFile, na = "NA", 
          append = FALSE)

# ----
# control chart for variability
qic(partyVotesP$pPS, chart="i", agg.fun = c("mean"), 
    title = "PS Vote Share Control Chart",
    xlab = "Administrative Unit",
    ylab = "Vote Share",
    print.summary = TRUE)
qic(partyVotesP$pPD, chart="i", agg.fun = c("mean"), 
    title = "PD Vote Share Control Chart",
    xlab = "Administrative Unit",
    ylab = "Vote Share",
    print.summary = TRUE)

#########################################################
#
# Vote-Seat share analysis
# ----
# Convert seats to shares (%-ages)
voteSeatShare <- within(voteSeatShare, 
                        TotalSeats <- (`PS Seats` + `PD Seats` + `OP Seats`))
voteSeatShare <- within(voteSeatShare, 
                        PSSeatShare <- `PS Seats` / TotalSeats)
voteSeatShare <- within(voteSeatShare, 
                        PDSeatShare <- `PD Seats` / TotalSeats)
voteSeatShare <- within(voteSeatShare, 
                        OPSeatShare <- `OP Seats` / TotalSeats)

# Next, plot the vote-seat share for each party to infer the curve:
#plot(x = voteSeatShare$`PS Vote Share`, y = voteSeatShare$PSSeatShare, type = "p")
