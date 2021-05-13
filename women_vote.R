# women_vote.R
# Analyze women vote gap & other differences using the 5199 data points.
# CC BY-SA. W.A. Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

districtLevelStats <- here("out/women_men_district.csv")

# Summarize men & women voters by district for the major political groupings
votesByDistrict <- fPartyVotes %>% group_by(District) %>% 
  summarize(PS = sum(PS), 
            PD = sum(PD), 
            Others = sum(PSD, PBK, PLDSH, BD, ABEOK, LSI, NTH, LRE,
                           ADR, LN, Other),
            InvalidBallots = sum(InvalidBallots),
            VotingWomen = sum(VotingWomen),
            VotingMen = sum(VotingMen),
            UnaccountedBallots = sum(VotingWomen) + sum(VotingMen) - (sum(PS) +
               sum(PD) + sum(PSD, PBK, PLDSH, BD, ABEOK, LSI, NTH, LRE,
                                      ADR, LN, Other) + sum(InvalidBallots)),
            pVotingWomen = sum(VotingWomen) / 
              (sum(VotingWomen) + sum(VotingMen)),
            pVotingMen = sum(VotingMen) / 
              (sum(VotingWomen) + sum(VotingMen))
            )

write_csv(votesByDistrict, districtLevelStats, na = "NA", append = FALSE)


m = sum(votesByDistrict$VotingMen)
w = sum(votesByDistrict$VotingWomen)
pM = m / (m + w)
pW = 1.0 - pM
paste("Voting men: ", m, " (", round(pM * 100, 2), "%)", sep = "")
paste("Voting women: ", w, " (", round(pW * 100, 2), "%)", sep = "")
