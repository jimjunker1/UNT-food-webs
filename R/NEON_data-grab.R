library(neonstore)
library(tidyverse)
# library(googleCloudStorageR)
# library(googleAuthR)
# library(arrow)
# library(DBI)
# source("./R/update-data-products.R")
# neonstore::neon_dir();neonstore::neon_db_dir()
# update_data_products("resources")
# download NEON stream data not in update_data products
# dps = c("DP1.20063.001",
#         "DP1.20066.001",
#         "DP1.20072.001",
#         "DP1.20086.001",
#         "DP1.20277.001",
#         "DP1.20093.001",
#         "DP1.20097.001",
#         "DP1.20163.001",
#         "DP1.20275.001",
#         "DP1.20191.001",
#         "DP1.20194.001",
#         "DP1.20206.001"
#         )
# neonstore::neon_download(product = dps)

# grab data summaries for 2018-06 onwards

streams_meta = read.csv(file = here::here("data/NEONdata/site_latlong.csv"))
streams_vec = streams_meta$site
date_cutoff = as.Date("2018-04-01")

plant_biomass = readRDS(here::here("data/NEONdata/aquaticplant_biomass.rds"))
microbe_abun = readRDS(here::here("data/NEONdata/benthicmicrobe_abun.rds"))
waq_lab = readRDS(here::here("data/NEONdata/waterquality_lab.rds"))
waq_labalk = readRDS(here::here("data/NEONdata/waterquality_lab_alk.rds"))
waq_field = readRDS(here::here("data/NEONdata/waterquality_fieldmeta.rds"))
epi_extlab = readRDS(here::here("data/NEONdata/epi_extlab.rds"))
epi_lab = readRDS(here::here("data/NEONdata/epi_lab.rds"))
epi_field = readRDS(here::here("data/NEONdata/epi_field.rds"))
riparian_cov = readRDS(here::here("data/NEONdata/riparian_field.rds"))
riparian_comp = readRDS(here::here("data/NEONdata/riparian_comp.rds"))
sed_lab = readRDS(here::here("data/NEONdata/sed_lab.rds"))
sed_fieldPoint = readRDS(here::here("data/NEONdata/sed_fieldDataPoint.rds"))
sed_fieldStation = readRDS(here::here("data/NEONdata/sed_fieldDataStation.rds"))
w_SI = readRDS(here::here("data/NEONdata/water_SI.rds"))
w_SIfield = readRDS(here::here("data/NEONdata/water_SIfield.rds"))

#### stream-date areal biomass of total plants

site_date_plant_biomass_m2 = plant_biomass %>%
  group_by(siteID, collectDate) %>%
  summarise(arealAdjDryMass_gm2 = sum(arealAdjDryMass, na.rm = TRUE),
            arealAdjAshFreeDryMass_gm2 = sum(arealAdjAshFreeDryMass, na.rm = TRUE))

saveRDS(site_date_plant_biomass_m2, here::here("data/derived-data/site_date_plant_biomass_m2.rds"))
#### stream-date of microbial abundance

#### stream-date water quality ----
# this is a list
site_date_waq_lab = waq_lab %>%
  group_by(siteID, collectDate) %>%
  # unite("measure", analyte, analyteUnits, sep = "\n", remove = TRUE) %>%
  pivot_wider(id_cols = c(siteID, collectDate),
              names_from = c(analyte, analyteUnits),
              names_sep = "\n", values_from = analyteConcentration,
              values_fn = list, values_fill = NA)

site_date_waq_lab_summ = waq_lab %>%
  group_by(siteID, collectDate) %>%
  # unite("measure", analyte, analyteUnits, sep = "\n", remove = TRUE) %>%
  pivot_wider(id_cols = c(siteID, collectDate),
              names_from = c(analyte, analyteUnits),
              names_sep = "\n", values_from = analyteConcentration,
              values_fn = mean, values_fill = NA)

saveRDS(site_date_waq_lab_summ, here::here("data/derived-data/site_date_waq_lab_summ.rds"))
#### site-date epilithon chem ----

site_date_epi_chlorophyll = epi_field %>%
  select(siteID, collectDate, namedLocation, sampleID = parentSampleID, benthicArea) %>%
  filter( grepl("episammon|epiphyton", sampleID, ignore.case = TRUE)) %>%
  mutate(collectDate = as.Date(collectDate)) %>%
  left_join(epi_lab %>%
              filter(grepl("chlorophyll", analysisType),
                     grepl("episammon|epiphyton", sampleID, ignore.case = TRUE)) %>%
              select(siteID, collectDate, namedLocation, sampleID,  fieldSampleVolume) %>% #analysisType
              mutate(collectDate = as.Date(collectDate)) %>%
              rowwise() %>%
              mutate(sampleID = paste(sapply(strsplit(sampleID, "\\."), "[", 1:4), collapse = "."))) %>%
  mutate(arealMult = fieldSampleVolume/benthicArea) %>%
  left_join(epi_extlab %>%
              filter(grepl("chlorophyll", sampleType),
                     grepl("episammon|epiphyton", sampleID, ignore.case = TRUE)) %>%
              select(siteID, collectDate, namedLocation, sampleID, percentFilterAnalyzed, analyte, analyteConcentration, plantAlgaeLabUnits) %>%
              rowwise() %>%
              mutate(sampleID = paste(sapply(strsplit(sampleID, "\\."), "[", 1:4), collapse = "."))) %>%
  mutate(analyteArealMass = analyteConcentration * arealMult) %>%
  mutate(analyteArealMass = case_when(grepl("micrograms.*", plantAlgaeLabUnits) ~ analyteArealMass/1000,
                                      .default = analyteArealMass),
         plantAlgaeLabUnits = case_when(grepl("micrograms.*", plantAlgaeLabUnits) ~ "milligrams_m2",
                                        .default = plantAlgaeLabUnits)) %>%
  select(siteID, collectDate, namedLocation, analyte, analyteArealMass, plantAlgaeLabUnits) %>%
  filter(!is.na(analyte),
         analyte != 'total chlorophyll a') %>%
  pivot_wider(id_cols = c(siteID, collectDate, namedLocation),
              names_from = c(analyte, plantAlgaeLabUnits),
              names_sep = "\n",
              values_from = analyteArealMass,
              values_fn = mean, values_fill = NA) %>%
  select(-namedLocation)

saveRDS(site_date_epi_chlorophyll, here::here("data/derived-data/site_date_epi_chlorophyll.rds"))

site_date_epi_chem = epi_field %>%
  select(siteID, collectDate, namedLocation, sampleID = parentSampleID, benthicArea) %>%
  filter( grepl("episammon|epiphyton", sampleID, ignore.case = TRUE)) %>%
  mutate(collectDate = as.Date(collectDate)) %>%
  left_join(epi_lab %>%
              filter(!grepl("chlorophyll", analysisType),
                     grepl("episammon|epiphyton", sampleID, ignore.case = TRUE)) %>%
              select(siteID, collectDate, namedLocation, sampleID,  fieldSampleVolume) %>% #analysisType
              mutate(collectDate = as.Date(collectDate)) %>%
              rowwise() %>%
              mutate(sampleID = paste(sapply(strsplit(sampleID, "\\."), "[", 1:4), collapse = "."))) %>%
  mutate(arealMult = fieldSampleVolume/benthicArea) %>%
  left_join(epi_extlab %>%
              filter(!grepl("chlorophyll", sampleType),
                     !grepl("perMill", plantAlgaeLabUnits),
                     grepl("episammon|epiphyton", sampleID, ignore.case = TRUE)) %>%
              select(siteID, collectDate, namedLocation, sampleID, percentFilterAnalyzed, analyte, analyteConcentration, plantAlgaeLabUnits) %>%
              rowwise() %>%
              mutate(sampleID = paste(sapply(strsplit(sampleID, "\\."), "[", 1:4), collapse = "."))) %>%
  mutate(analyteArealMass = analyteConcentration * arealMult) %>%
  mutate(analyteArealMass = case_when(grepl("micrograms.*", plantAlgaeLabUnits) ~ analyteArealMass/1e6,
                                      .default = analyteArealMass),
         plantAlgaeLabUnits = case_when(grepl("micrograms.*", plantAlgaeLabUnits) ~ "grams_m2",
                                        .default = plantAlgaeLabUnits)) %>%
  select(siteID, collectDate, namedLocation, analyte, analyteArealMass, plantAlgaeLabUnits) %>%
  filter(!is.na(analyteArealMass)) %>%
  pivot_wider(id_cols = c(siteID, collectDate),
              names_from = c(analyte, plantAlgaeLabUnits),
              names_sep = "\n", values_from = c(analyteArealMass),
              values_fn = mean, values_fill = NA) %>%
  mutate(`carbon\nmol_m2` = `carbon\ngrams_m2`/12.001,
         `nitrogen\nmol_m2` = `nitrogen\ngrams_m2`/14.007,
         `phosphorus\nmol_m2` = `phosphorus\ngrams_m2`/30.974,
         `sulfur\nmol_m2` = `sulfur\ngrams_m2`/32.06) %>%
  mutate(across(-collectDate, ~ifelse(.x == 0, NA_real_, .x)))

saveRDS(site_date_epi_chem, here::here("data/derived-data/site_date_epi_chem.rds"))


site_date_epi_field = epi_field %>%
  select(siteID, collectDate, namedLocation, sampleID = parentSampleID, benthicArea) %>%
  mutate(collectDate = as.Date(collectDate)) %>%
  left_join(epi_lab %>%
              select(siteID, collectDate, namedLocation, sampleID,  fieldSampleVolume) %>% #analysisType
              mutate(collectDate = as.Date(collectDate))) %>%
  mutate(arealMult = fieldSampleVolume/benthicArea) %>%
  left_join(epi_extlab %>%
              select(siteID, collectDate, namedLocation, sampleID, percentFilterAnalyzed, analyte, analyteConcentration, plantAlgaeLabUnits) %>%
              rowwise() %>%
              mutate(sampleID = paste(sapply(strsplit(sampleID, "\\."), "[", 1:4), collapse = ".")))


site_date_seston = site_date_epi_field %>%
  filter(grepl("SESTON", sampleID)) %>%
  select(siteID, collectDate, namedLocation, sampleID, fieldSampleVolume, percentFilterAnalyzed, analyte, analyteConcentration, plantAlgaeLabUnits) %>%
  filter(!is.na(analyte)) %>%
  pivot_wider(id_cols = c(siteID, collectDate),
              names_from = c(analyte, plantAlgaeLabUnits),
              values_from = analyteConcentration,
              values_fn = mean,
              values_fill = NA)

saveRDS(site_date_seston, here::here("data/derived-data/site_date_seston.rds"))

## need to figure out why there is no volumeFiltered values ## on many of the samples
# site_date_epilithon = site_date_epi_field %>%
#   filter(grepl("EPILITHON", sampleID)) %>%
#   filter(!is.na(analyte))


# site_date_epi_extlab = epi_extlab %>%
#   rename(namedLocation1 = namedLocation) %>%
#   pivot_wider(id_cols = c(siteID, collectDate, namedLocation1),
#               names_from = c(analyte, plantAlgaeLabUnits), names_sep = "\n",
#               values_from = analyteConcentration,
#               values_fn =list, values_fill = NA)

# site_date_epi_extlab_summ = epi_extlab %>%
#   # rename(namedLocation1 = namedLocation) %>%
#   pivot_wider(id_cols = c(siteID, collectDate, namedLocation),
#               names_from = c(analyte, plantAlgaeLabUnits), names_sep = "\n",
#               values_from = analyteConcentration,
#               values_fn =mean, values_fill = NA)
#### stream-date of riparian cover ----

site_date_riparian = riparian_cov %>%
  filter(riparianSubsystem == "stream") %>%
  select(siteID, namedLocation, startDate, riparianDominantVegetation,
         riparianWaterDepth, wettedWidth, riparianClass, riparianSubclass,
         riparianDominantVegetation, bigTreeType, bigTreeCoverClass,
         smallTreeType, smallTreeCoverClass, woodyShrubSaplingType,
         woodyShrubGroundCoverClass, shortHerbCoverClass, standingWaterCoverClass,
         bareDirtCoverClass, buildingPresence, roadPresence, boatRampPresence,
         lawnsParksPresence, agriculturePresence, industryPresence, pavementPresence,
         trashPresence, dominantBankAngle, bankUndercut)

saveRDS(site_date_riparian, here::here("data/derived-data/site_date_riparian.rds"))

site_date_riparian_comp = riparian_comp %>%
  select(siteID, startDate, measurementLocation, measurementDirection, totalDensiometerPoints,
         coveredDensiometerPoints, canopyCoverPercent)

saveRDS(site_date_riparian_comp, here::here("data/derived-data/site_date_riparian_comp.rds"))
#### stream-date of sediment cover ----

site_date_sed_lab = sed_lab %>%
  filter(is.na(analyteConcText)) %>%
  select(siteID, sampleID, collectDate = "startDate", analyte, analyteConcentration, analyteUnits) %>%
  pivot_wider(id_cols = c(siteID, collectDate),
              names_from = c(analyte, analyteUnits),
              values_from = analyteConcentration,
              values_fn = mean,
              values_fill = NA)

saveRDS(site_date_sed_lab, here::here("data/derived-data/site_date_sed_lab.rds"))
# site_date_sed_text = sed_lab %>%
#   filter(!is.na(analyteConcText)) %>%
#   select(siteID, namedLocation, sampleID, startDate, analyte, analyteConcText) #%>%
#
# sed_fieldPoint %>% View()
#
# sed_fieldStation %>% View()

#### site-date-water isotopes

