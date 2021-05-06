# source_data.R
# Load source data from CSV files located in the ./data directory.
# CC BY-SA. Arbër Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

# Save file locations in variables
partyVotesFile <- here('data/alb_parl21_elxn_party_votes.csv')

# Read CSV data into tibbles
partyVotesRaw <- read_csv(partyVotesFile)

