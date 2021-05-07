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
                          partyVotes$PS / (partyVotes$PS + partyVotes$PD),
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
