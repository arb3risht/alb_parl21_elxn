# benford.R
# Checking conformity with Benford's Law of leading and second digits for 
# reported figures -- party votes and invalid ballots -- at the admin unit.
# CC BY-SA. Arbër Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# LEADING DIGITS
# --------------
# Leading-digit PS
benfordPS <- benford(partyVotes$PS, number.of.digits = 1)
plot(benfordPS)
partyVotesPS <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PS)
suspectsPS <- getSuspects(benfordPS, partyVotesPS)
view(suspectsPS)

# Leading-digit PD
benfordPD <- benford(partyVotes$PD, number.of.digits = 1)
plot(benfordPD)
partyVotesPD <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, partyVotes$PD)
suspectsPD <- getSuspects(benfordPD, partyVotesPD)
view(suspectsPD)

# Leading-digit Other Parties
benfordOP <- benford(partyVotes$`Other Parties`, number.of.digits = 1)
plot(benfordOP)
partyVotesOP <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Other Parties`)
suspectsOP <- getSuspects(benfordOP, partyVotesOP)
view(suspectsOP)

# Leading-digit Invalid Ballots
benfordIB <- benford(partyVotes$`Invalid Ballots`, number.of.digits = 1)
plot(benfordIB)
partyVotesIB <- data.frame(partyVotes$District, partyVotes$Municipality,
                           partyVotes$`Administrative Unit`, 
                           partyVotes$`Invalid Ballots`)
suspectsIB <- getSuspects(benfordIB, partyVotesIB)
view(suspectsIB)


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
benfordPS <- DiscreteTwoSampleKSTestWithVisuals(partyVotes$PS, "PS")
# Print results - graph and text - in files

benfordPS[[1]]
benfordPS[[2]]

# PD
benfordPD <- DiscreteTwoSampleKSTestWithVisuals(partyVotes$PD, "PD")
# Print results - graph and text - in files

benfordPD[[1]]
benfordPD[[2]]

# OP
benfordOP <- DiscreteTwoSampleKSTestWithVisuals(partyVotes$`Other Parties`, 
                                                "Other Parties")
# Print results - graph and text - in files

benfordOP[[1]]
benfordOP[[2]]

# IB
benfordIB <- DiscreteTwoSampleKSTestWithVisuals(partyVotes$`Invalid Ballots`, 
                                                "Invalid Ballots")
# Print results - graph and text - in files

benfordIB[[1]]
benfordIB[[2]]
