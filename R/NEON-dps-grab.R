### specific data product codes
#### aquatic plants ----
neon_read(product = "DP1.20066.001",
          table = "apl_biomass-basic",
          altrep = FALSE) %>%
  select(siteID, domainID, collectDate, fieldID, sampleID, taxonID, scientificName, initialSubsamplePercent,
         benthicArea, adjDryMass, adjAshFreeDryMass, arealAdjDryMass, arealAdjAshFreeDryMass, chemSubsampleBarcode) %>%
  saveRDS(here::here("data/NEONdata/aquaticplant_biomass.rds"))
#### microbes ----
neon_read(product = "DP1.20277.001",
          table = "mga_benthicGroupAbundances-basic",
          altrep = FALSE) %>%
  saveRDS(here::here("data/NEONdata/benthicmicrobe_abun.rds"))
#### water quality ----
neon_read(product = "DP1.20093.001",
          table = "swc_externalLabDataByAnalyte-basic",
          altrep = FALSE) %>%
  select(siteID, domainID, sampleID, collectDate, namedLocation, analyte, analyteConcentration, analyteUnits, belowDetectionQF, shipmentWarmQF,externalLabDataQF) %>%
  saveRDS(here::here("data/NEONdata/waterquality_lab.rds"))

neon_read(product = "DP1.20093.001",
          table = "swc_domainLabData-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'collectDate', 'parentSampleID','alkMeqPerL', 'alkMgPerL', 'ancMeqPerL', 'ancMgPerL'))) %>%
  saveRDS(here::here("data/NEONdata/waterquality_lab_alk.rds"))

neon_read(product = "DP1.20093.001",
          table = "swc_fieldData-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'collectDate', 'parentSampleID','sampleVolumeFiltered'))) %>%
  saveRDS(here::here("data/NEONdata/waterquality_fieldmeta.rds"))
#### epilithon ----
neon_read(product = "DP1.20163.001",
          table = "alg_algaeExternalLabDataPerSample-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'collectDate', 'sampleType','sampleVolumeFiltered','percentFilterAnalyzed','analyte','analyteConcentration','plantAlgaeLabUnits'))) %>%
  saveRDS(here::here("data/NEONdata/epi_extlab.rds"))

neon_read(product = "DP1.20163.001",
          table = "alg_domainLabChemistry-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'collectDate', 'sampleType','filterNumber','analysisType','fieldSampleVolume','domainFilterVolume'))) %>%
  saveRDS(here::here("data/NEONdata/epi_lab.rds"))

neon_read(product = "DP1.20163.001",
          table = "alg_fieldData-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'collectDate','parentSampleID', 'algalSampleType','habitatType','dominantHabitat','algalSampleType','benthicArea'))) %>%
  saveRDS(here::here("data/NEONdata/epi_field.rds"))
#### riparian cover ----
neon_read(product = "DP1.20275.001",
          table = "rip_assessment-basic",
          altrep = FALSE) %>%
  # select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'collectDate','parentSampleID', 'algalSampleType','habitatType','dominantHabitat','algalSampleType','benthicArea'))) %>%
  saveRDS(here::here("data/NEONdata/riparian_field.rds"))

neon_read(product = "DP1.20191.001",
          table = "rip_percentComposition-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'startDate','measurementLocation','measurementDirection','totalDensiometerPoints','coveredDensiometerPoints','canopyCoverPercent'))) %>%
  saveRDS(here::here("data/NEONdata/riparian_comp.rds"))
#### sediment ----
neon_read(product = "DP1.20194.001",
          table = "asc_externalLabData-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'startDate','analyte','primaryMatrix','analyteConcentration','analyteConcText','analyteUnits'))) %>%
  saveRDS(here::here("data/NEONdata/sed_lab.rds"))

neon_read(product = "DP1.20194.001",
          table = "asc_fieldDataPoint-basic",
          altrep = FALSE) %>%
  # select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'startDate','analyte','primaryMatrix','analyteConcentration','analyteConcText','analyteUnits'))) %>%
  saveRDS(here::here("data/NEONdata/sed_fieldDataPoint.rds"))

neon_read(product = "DP1.20194.001",
          table = "asc_fieldDataStation-basic",
          altrep = FALSE) %>%
  # select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'startDate','analyte','primaryMatrix','analyteConcentration','analyteConcText','analyteUnits'))) %>%
  saveRDS(here::here("data/NEONdata/sed_fieldDataStation.rds"))

# neon_read(product = "DP1.20194.001",
#           table = "asc_fieldDataZone-basic",
#           altrep = FALSE) %>%
#   # select(any_of(c('siteID', 'domainID', 'sampleID', 'namedLocation', 'startDate','analyte','primaryMatrix','analyteConcentration','analyteConcText','analyteUnits'))) %>%
#   saveRDS(here::here("data/NEONdata/sed_fieldDataZone.rds"))
#### water stable isotopes ----
neon_read(product = "DP1.20206.001",
          table = "asi_externalLabH2OIsotopes-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'isotopeH2OSampleID', 'namedLocation', 'collectDate','d18OWater','d2HWater'))) %>%
  saveRDS(here::here("data/NEONdata/water_SI.rds"))

neon_read(product = "DP1.20206.001",
          table = "asi_fieldData-basic",
          altrep = FALSE) %>%
  select(any_of(c('siteID', 'domainID', 'isotopeH2OSampleID', 'namedLocation','collectDate' ,'sampleVolumeFilteredPOMRep1','sampleVolumeFilteredPOMRep2'))) %>%
  saveRDS(here::here("data/NEONdata/water_SIfield.rds"))

