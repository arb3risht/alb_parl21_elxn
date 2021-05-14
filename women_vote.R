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
            RegisteredMen = sum(EligibleMenVoters),
            RegisteredWomen = sum(EligibleWomenVoters),
            VotingWomen = sum(VotingWomen),
            VotingMen = sum(VotingMen),
            UnaccountedBallots = sum(VotingWomen) + sum(VotingMen) - (sum(PS) +
               sum(PD) + 
                 sum(PSD, PBK, PLDSH, BD, ABEOK, LSI, NTH, LRE, ADR, LN, Other) 
               + sum(InvalidBallots)),
            pVotingWomen = sum(VotingWomen) / 
              (sum(VotingWomen) + sum(VotingMen)),
            pVotingMen = sum(VotingMen) / 
              (sum(VotingWomen) + sum(VotingMen))
            )

write_csv(votesByDistrict, districtLevelStats, na = "NA", append = FALSE)

# Men and women who voted
m = sum(votesByDistrict$VotingMen)
w = sum(votesByDistrict$VotingWomen)
pM = m / (m + w)
pW = 1.0 - pM
paste("Voting men: ", m, " (", round(pM * 100, 2), "%)", sep = "")
paste("Voting women: ", w, " (", round(pW * 100, 2), "%)", sep = "")

# Turnout for men and women
rM <- sum(votesByDistrict$RegisteredMen)
rW <- sum(votesByDistrict$RegisteredWomen)
tM <- m / rM
tW <- w / rW
paste("Registered men: ", rM, " (", round(tM * 100, 2), "%)", sep = "")
paste("Registered women: ", rW, " (", round(tW * 100, 2), "%)", sep = "")
pRM <- rM / (rM + rW)
pRW <- 1.0 - pRM
pRM
pRW

# Plot district-level turnout via a group bar plot
# x: district.Men and district.Women; y: proportions

# wrangle the vectors into a new frame,
# and update Voters column values to user-friendly names for the legend
districtData <- data.frame(votesByDistrict$District, 
                 votesByDistrict$RegisteredMen,
                 votesByDistrict$RegisteredWomen,
                 votesByDistrict$VotingMen,
                 votesByDistrict$VotingWomen)
names(districtData) <- c("District", 
                         "Registered Men", 
                         "Registered Women",
                         "Voting Men", 
                         "Voting Women"
                         )
districtData %>% pivot_longer(cols = -District, 
                              names_to = "Voters",
                              values_to = "Tally (thousands)"
) -> districtData

# Pivot turnout data into a separate frame for labeling bars
# with the trick of zeroing out non-proportions so labels are not displayed
# on geom_text, except for proportion labels
turnoutData <- data.frame(votesByDistrict$District, 
                          0, 
                          0,
                          votesByDistrict$pVotingMen,
                          votesByDistrict$pVotingWomen)
names(turnoutData) <- c("District", 
                        "Registered Men", 
                        "Registered Women",
                        "Voting Men", 
                        "Voting Women")
turnoutData %>% pivot_longer(cols = -District, 
                             names_to = "Voters",
                             values_to = "Tally (thousands)"
                             ) -> turnoutData
strTunrout <-c("Registered Men", 
               "Registered Women", 
               "Voting Men",
               "Voting Women" 
               )

# factor levels to preserve order for proper bar-label matching:
districtData$Voters <- factor(districtData$Voters)
turnoutData$Voters <- factor(turnoutData$Voters)

ggplot(districtData, aes(x = District, y = `Tally (thousands)`/1000, fill = Voters)) +
  theme_minimal()+
  geom_bar(stat='identity', position='dodge') + 
  coord_flip() +
  geom_text(data = subset(turnoutData, Voters %in% strTunrout),
    aes(label = ifelse(`Tally (thousands)` == 0, 
                       "", 
                       paste(round(`Tally (thousands)` * 100, 2), 
                             "%", sep = ""
                             )
                       )
        ),
    position = position_dodge(width = .95),  # center around the right two bars
    vjust = 0.5,    # nudge above top of bar
    hjust = 1.1,
    size = 3,
    show.legend = FALSE) +
  xlab(paste("District: Registered and voting men and women with turnout %-ages")) +
  ylab("Tally (thousands)") +
  scale_fill_manual(values = c("#cdd9e4", "#8392a1", "#ffa600", "#0066cc")) +
  ggtitle(paste("Men and women tournout: ", 
                "2021 Albanian parliamentary elections")) +
  theme(legend.title=element_blank(), legend.position = "bottom") + 
  theme(plot.title = element_text(size=12))

#----
#### Research question ###
# Was there any significant difference between the turnout rates of men
# and women voters?
# H_0: m-w = 0
# H_A: m-w != 0
