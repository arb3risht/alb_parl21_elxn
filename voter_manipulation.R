# voter_manipulation.R
# Analyze small poll stations for voter manipulation by examining the
# vote/ turnout bivariate distributions for potential signs of rigging.
# CC BY-SA. W.A. Boriçi, 2021. Contact: arberisht@gmail.com. 
# Full license terms at https://creativecommons.org/licenses/by-sa/4.0/.

smearFile <- here("out/smear_PS.csv")

###############################
# ---- DATA WRANGLING
###############################
# Get administrative units (level 3) for Albania (2018 GADM data)
# GADM tibble has some name_3 errors, which I've fixed by the manual mapping
# see file gadm_mapping.csv for the GID_3 code mapping.
# The following GADM level-3 elements remain unmapped to the 383 units:
#  ALB.7.3.3_1	Inland Water Body (Korce)
#  ALB.7.4.5_1	Inland Water Body (Korce)
#  ALB.10.1.2_1	Inland Water Body (Shkoder)
#  ALB.10.3.2_1	Barbullush (Shkoder), as of 2015 part of Vau i Dejes -> Bushat
#  ALB.12.2.3_1	Inland Water Body (Vlore)
# The 11 administrative units for Tirane - ALB.11.2.15_1 - need to be
# added manually via polygons based on the svg path located at
# https://tirana.al/njesite-administrative

gadm_mapping_file <- here("data/gadm_mapping.csv")
gadm_mapping <- read_csv(gadm_mapping_file)
#view(gadm_mapping)

# read polling-station-level election results
# defined in data_description.R; contains Mesopotam, where turnout was 0%.
pResults <- read_csv(partyVotesRawByPollingStation) 
#view(pResults)

# Summarize polling-level results into admin units to establish one-to-one
# relationship with gadm_mapping for later plotting
auResults <- pResults %>% 
  group_by(District, Municipality, AdministrativeUnit) %>%
  summarize(EligibleVoters = sum(EligibleVoters),
            EligibleWomenVoters = sum(EligibleWomenVoters),
            EligibleMenVoters = sum(EligibleMenVoters),
            VotesCast = sum(VotesCast),
            VotingWomen = sum(VotingWomen),
            VotingMen = sum(VotingMen),
            ValidBallots = sum(ValidBallots),
            InvalidBallots = sum(InvalidBallots),
            PSD = sum(PSD),
            PBK = sum(PBK),
            PLDSH = sum(PLDSH),
            BD = sum(BD),
            ABEOK = sum(ABEOK),
            LSI = sum(LSI),
            NTH = sum(NTH),
            LRE = sum(LRE),
            PD = sum(PD),
            ADR = sum(ADR),
            LN = sum(LN),
            PS = sum(PS),
            Other = sum(Other)
  )
# view(auResults)

# ---- Load the map of Albania
albania  <- raster::getData("GADM", country = 'ALB', level = 3)
#view(albania)
write_csv(as.data.frame(albania), here("out/GADM_au.csv"))
#plot(albania[which(albania$GID_3 == "ALB.11.2.15_1"), ])
#plot(albania)

# Download the GADM map for Albania & use native functions to wrangle polygons
# alb <- gadm_sf_loadCountries("ALB", level = 3, basefile = here("data/"))
# gadm_plot(alb) + theme_light()
# view(alb)
# gadm_plot( gadm_subset(alb, regions = "Tiranë", level = 3))

# ---- 
# Merge ALB.10.3.2_1 (Barbullush) with the polygon for Bushat (ALB.10.3.4_1)
#barbullushID <- as.integer(row.names(albania[which(albania$GID_3 == "ALB.10.3.2_1"), ])) - 1
#bushatID <- as.integer(row.names(albania[which(albania$GID_3 == "ALB.10.3.4_1"), ])) - 1
# merge the two GIDs into that of Bushat, including the corresponding names
albania$NAME_1[albania$NAME_1 %in% c("Shkodër", "Shkodër")] <- "Shkodër"
albania$NAME_2[albania$NAME_2 %in% c("Shkodrës", "Shkodrës")] <- "Shkodrës"
albania$NAME_3[albania$NAME_3 %in% c("Barbullush", "Bushat")] <- "Bushat"
albania$GID_3[albania$GID_3 %in% c("ALB.10.3.2_1", "ALB.10.3.4_1")] <- "ALB.10.3.4_1"
# merge the two polygons
bushatBarbullush_sp <- unionSpatialPolygons(albania, albania$GID_3)
# Merge the associated data
bushatBarbullush_data <- unique(albania@data[ , c("NAME_1", "NAME_2", "NAME_3", "GID_3")])
# rownames must match polygons IDs - update them to GID_3
rownames(bushatBarbullush_data) <- bushatBarbullush_data$GID_3
# complete merge via sp into a new gadm tibble to use in the analysis
albania_wrangled <- SpatialPolygonsDataFrame(bushatBarbullush_sp, bushatBarbullush_data) 
#view(albania_wrangled) # map includes the four water bodies, which can be ignored

# The 11 subdivisions of Tirana do not exist on GADM - create the polygons
# @TODO


###############################
# ---- PLOT VOTER TURNOUT
###############################

# ---- 
# First, merge the 11 sub-divisions of Tirane into one one admin unit
auMerged <- data.frame(auResults$District,
                       auResults$Municipality,
                       auResults$AdministrativeUnit,
                       auResults$EligibleVoters,
                       auResults$VotesCast,
                       0
)
names(auMerged) <- c("District", 
                     "Municipality",
                     "AdministrativeUnit",
                     "EligibleVoters",
                     "VotesCast",
                     "MappingID")

# Next, add the GADM mappings
for (i in 1:length(auMerged$MappingID)) {
  found <- FALSE
  for (j in 1:length(gadm_mapping$MappingID)) {
    if (str_squish(str_to_lower(auMerged$AdministrativeUnit[i])) == 
        str_squish(str_to_lower(gadm_mapping$`Administrative Unit`[j]))){
      auMerged$MappingID[i] <- gadm_mapping$MappingID[j]
      found <- TRUE
    }
    if (found) break
  }
}
 
# Find the 11 subdivisions
units <- grepl("Tirane", auMerged$AdministrativeUnit)
# remove them from auMerged, for now, until polygons are added later manually
trEligibleVotes <- 0
trVotesCast <- 0
n <- length(auResults$EligibleVoters)
idx <- NULL # list of row indices containing sub-divisions to remove
for (i in 1:n){
  if (units[i] == TRUE){
    trEligibleVotes <- trEligibleVotes + sum(auResults$EligibleVoters[i])
    trVotesCast <- trVotesCast + sum(auResults$VotesCast[i])
    idx <- append(idx, i)
    mapID <- auMerged$MappingID[i]
  }
}

# remove these records from auMerged, to collapse them later into one]
auMerged <- auMerged[-idx, ]

# add the one collapsed row back to auMerged
trMerged <- data.frame("Tirane", 
                       "Bashkia Tirane", 
                       "Tirane", 
                       trEligibleVotes, 
                       trVotesCast,
                       mapID)
names(trMerged) <- c("District", 
                     "Municipality",
                     "AdministrativeUnit",
                     "EligibleVoters",
                     "VotesCast",
                     "MappingID")

# bind trMerged to auMerged:
auMerged <- rbind(auMerged, trMerged)
#view(auMerged)

auTurnout <- data.frame(auMerged,
                        auMerged$VotesCast/auMerged$EligibleVoters)
colnames(auTurnout)[colnames(auTurnout) == 
                      'auMerged.VotesCast.auMerged.EligibleVoters'] <- 'pTurnout'

# Finally, plot turnout rates
# cross-reference the GID_3 IDs in the gadm map and the auTurnout tibble
#view(ab)
ab <- subset(albania_wrangled, NAME_3 != "Inland Water Body")
# join turnout results to the ab polygon data for the heatmap plot
ab@data <- inner_join(ab@data, auTurnout, by = c("GID_3" = "MappingID"),  )
# RGB plot - since the min and max turnout form a range 73..227, 
# scale it to 0..255 for a better range of rgb coloring:
minT <- min(ab$pTurnout)
maxT <- max(ab$pTurnout)
minC <- 0.02
maxC <- 0.98
# plot(ab,
#      col = rgb(255,
#       (1 - ((ab$pTurnout - minT) / (maxT - minT) * (maxC - minC) + minC)) * 255,
#       (1 - ((ab$pTurnout - minT) / (maxT - minT) * (maxC - minC) + minC)) * 255,
#       alpha = 230,
#       maxColorValue =  255
#       ),
#      border = 1, lwd = 0.35,
#      main = "2021 Albanian parliamentary elections: \nTurnout heatmap")

# plot in ggplot2 for more features
abF <- NULL
abF <- fortify(ab, region = "GID_3")
abF <- inner_join(abF, ab@data, by = c("id" = "GID_3"))
trnout <- abF$pTurnout
ggplot() + 
  geom_polygon(data = abF, aes(x = long, 
                               y = lat, 
                               group = group,
                               fill = trnout), 
              color = "gray", size = 0.2) +
  scale_fill_distiller(name="Turnout %-age\n", 
                       palette = "Reds",
                       trans = "reverse",
                       guide = "colorbar",
                     #  breaks = scales::pretty_breaks(n = 3),
                       breaks = c(minT, (maxT + minT)/2, maxT)
                     ) +
  labs(title="2021 Albanian parliamentary elections: \nTurnout heatmap") +
  theme(aspect.ratio = 2.2,
        legend.direction = "vertical",
        legend.position = "right",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# ----
# Plot a neighborhood of administrative units as a visual for the 
# voter/ turnout analysis for polling-station voter manipulation
# Simple plot:
# ----
# build neigborhood matrix by gadm GIDs (requires library rgeos)
# for this to work, include the full albania_wrangled map with the inland waters
# otherwise, I find there's a bug with polygons being different administrative
# units than the ones expected.
neighbors <- gTouches(albania_wrangled, byid = TRUE)
# view(neighbors)

view(albania_wrangled@data)
# get neighbors of an admin unit & plot after joining with the auTurnout tibble
# note: for the albania_wrangled tibble, use the correpsonding admin unit name,
# which may be different from the name in the ngh tibble (mapping differences)
ngh <- GetNeighbors(albania_wrangled, "Lumas", "ALB.1.1.4_1", neighbors)
ngh@data <- inner_join(ngh@data, auTurnout, by = c("GID_3" = "MappingID"),  )
# get center points of the polygons; the row IDs are the GID_3 IDs:
centers <- as.data.frame(coordinates(ngh))
centers <- cbind(centers, rownames(centers))
names(centers) = c("c1", "c2", "cid")
centers <- inner_join(centers, auTurnout, by = c("cid" = "MappingID"))
# plot with centroids
plotAU <- PlotNeighborsWithTurnout(ngh, "Lumas", centers)
plotAU

# TODO:
# Build a function PlotNeighborsWithVoteShares() to illustrate winning parties
# in each neighboring administrative unit.

################################
# Vote/ Turnout analysis
################################

# Univariate histograms:
hist(fPartyVotes$pPS, breaks = "FD", freq = FALSE, col = "orange",
     main = paste("Histogram of PS vote share"),  
     xlab = "PS vote share")
lines(density(fPartyVotes$pPS), col = "navy", lwd = 2)
rug(fPartyVotes$pPS, col = "gray")

hist(fPartyVotes$pTurnout, breaks = "FD", freq = FALSE, col = "#56bd70",
     main = paste("Histogram of turnout rates"),
     xlab = "Turnout rate")
lines(density(fPartyVotes$pTurnout), col = "navy", lwd = 2)
rug(fPartyVotes$pTurnout, col = "gray")


# Plot the PS vote %-age vs. turnout %-age for all administrative units
# in a 2D histogram contour heat-map:
view(fPartyVotes)
gg <- ggplot(fPartyVotes, aes(pTurnout, pPS)) +
  geom_bin2d() +
  scale_fill_gradientn(colors = c("#0a044d", "green", "yellow", "red")) +
  geom_density2d(color = "white") +
  labs(title = paste("2021 Albanian parliamentary elections: \n",
                     "Turnout vs. PS vote %-age fingerprint"), 
       fill = "Density\n") +
  xlab("Turnout %-age") + 
  ylab("PS vote %-age") +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  theme(#aspect.ratio = 1,
    legend.direction = "vertical",
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#0a044d"),
    plot.background = element_blank()
  )

smears <- subset(gg$data, 
                 pTurnout > 0.3 & pTurnout < 0.6 & pPS > 0.6 & pPS < 0.75 | 
                 pTurnout > 0.5 & pTurnout < 0.6 & pPS > 0.57 & pPS < 0.65 | 
                 pTurnout > 0.57 & pTurnout < 0.62 & pPS > 0.47 & pPS < 0.62) %>%
  dplyr::select(District, 
                Municipality, 
                AdministrativeUnit, 
                PollingStation, 
                pTurnout, 
                pPS, 
                PS)
    
view(smears)
sum(smears$PS)
# Remove Kukes, Lezhe, Shkoder as they add up to a minority of 11 stations
smears <- subset(smears, District != "Kukes" & District != "Lezhe" & District != "Shkoder")
# print the final smear list for reference:
write_csv(smears, smearFile, na = "NA", append = FALSE)

# Berat polling stations contested as per the news report at
# https://exit.al/en/2021/05/17/albanian-cec-opens-ballot-boxes-to-check-for-evidence-of-vote-rigging/
# Ura Vajgurore, Poshnje, Kutalli, Cukalat, Zhepe, Vendreshe, Qender - Skrapar,
# Potom, Leshnje, Gjerbes, Corovode, Cepan, Bogove, Vertop, Terpan, Polican,
# Perondi, Lumas, Kucove, Kozare, Velabisht, Sinje, Roshnik, Otllak, Berat.
# Can map and use these as an illustration for the analysis

