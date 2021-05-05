# data_description.R
# Describe data visually and numerically using various statistical functions.
# CC BY-SA. Arbër Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# Output file locations
partyVotesByAdminUnit <- "out/party_votes_admin_unit.csv"
partyVotePByAdminUnit <- "out/party_votes_p_admin_unit.csv"
descripStatsFile <- "out/descript_stat"
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
                        Turnout <- `Eligible Voters` - TotalVotes)

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
                          partyVotes$`Other Parties` / 
                            partyVotes$`Total Eligible Votes`,
                          partyVotes$`Invalid Ballots` / 
                            partyVotes$`Total Votes`,
                          partyVotes$`Eligible Voters`,
                          partyVotes$`Total Eligible Votes`,
                          partyVotes$`Total Votes`,
                          partyVotes$Turnout / partyVotes$`Eligible Voters`)
names(partyVotesP) <- c("District", "Municipality", "Administrative Unit", "pPS", 
                       "pPD", "pOP", "pInvalid", 
                       "Eligible Voters", "Total Eligible Votes", 
                       "Total Votes", "pTurnout")
# write the data set out
write_csv(partyVotesP, partyVotePByAdminUnit, na = "NA", append = FALSE)
# Quick look at some stats
summary(partyVotesP)

# Group aggregated votes by district to look at the constituency level
votesGroupedByDistrict <- partyVotes %>% group_by(District) %>% 
  summarize(PS = sum(PS), PD = sum(PD), `Other Parties` = sum(`Other Parties`),
            `Invalid Ballots` = sum(`Invalid Ballots`),
            `Eligible Voters` = sum(`Eligible Voters`),
            `Total Eligible Votes` = sum(`Total Eligible Votes`),
            `Total Votes` = sum(`Total Votes`), 
            Turnout = sum(Turnout))
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
                                 pTurnout <- Turnout 
                                 / `Eligible Voters`)
# Print the district-level data set for later reference
write_csv(votesGroupedByDistrict, partyVotesByDistrictFile, na = "NA", 
          append = FALSE)

# Some insights
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

# Is the observed ~5% invalid ballot unusual, given prior election results?
# Let's test the hypotheses:
#   H_0: We expect around 2.5% invalid ballots
#        (The observed 5% invalid ballot proportion is not unusual.)
#   H_A: We got more than 2.5% invalid ballots
#        (The observed 5% invalid ballot proportion is unusual, given 
#        previous elections' results averaging at ~2.5%.)
# Since invalid ballots were normally distributed at 95% conf. level, we can
# use the z-scores to test the hypotheses above. Let b denote the proportion
# of invalid ballots. Then, if [1 - P(b < 5%)] <= p-value of 5%, we reject H_0
# in favor of H_A. This is a dichotomous experiment with p_success & failure, 
# where p_success = expected, i.e. our historical control proportion.

# We expect 2.5% invalid ballots based on prior elections
expected <- 0.025

# standard deviation of sample proportion
stdev <- sqrt(expected * (1-expected) / length(partyVotesP$pInvalid))

# verify the sample is large enough to warrant use of normal distribution
# by confirming that the following interval is within [0, 1]
paste("[", expected - 3*stdev, expected + 3*stdev, "]")

# z-score
z <- (pInvalidBallots - expected) / stdev
  
# P(b >= z) = 1 - P(b < z):
pValue <- 1 - pnorm(z, mean = 0, sd = 1, lower.tail = TRUE)
pValue

# confidence interval @95%
lInt <- expected - 1.96*sqrt(expected*(1-expected)/length(partyVotesP$pInvalid))
rInt <- expected + 1.96*sqrt(expected*(1-expected)/length(partyVotesP$pInvalid))
paste("C.I at 95%: (", lInt, ", ", rInt, 
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
            Turnout = sum(Turnout))
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
                                 pTurnout <- Turnout 
                                 / `Eligible Voters`)

view(votesGroupedByMunicipality)
# Print the municipality-level data set for later reference
write_csv(votesGroupedByMunicipality, partyVotesByMunicipalityFile, na = "NA", 
          append = FALSE)
