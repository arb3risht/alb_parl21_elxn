# source_data.R
# Load source data from CSV files located in the ./data directory.
# CC BY-SA. Arbër Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# File locations
partyVotesFile <- here('data/alb_parl21_elxn_party_votes.csv')
voteSeatShareFile <- here('data/vote-seat-share.csv')

# Read CSV data into tibbles
partyVotesRaw <- read_csv(partyVotesFile)
voteSeatShare <- read_csv(voteSeatShareFile)

