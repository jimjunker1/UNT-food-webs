library(neonstore)
library(tidyverse)
# library(googleCloudStorageR)
# library(googleAuthR)
# library(arrow)
# library(DBI)
source("./R/update-data-products.R")
neonstore::neon_dir();neonstore::neon_db_dir()
update_data_products("resources")
# download NEON stream data not in update_data products
dps = c("DP1.20063.001",
        "DP1.20066.001",
        "DP1.20072.001",
        "DP1.20086.001",
        "DP1.20277.001",
        "DP1.20093.001",
        "DP1.20097.001",
        "DP1.20163.001",
        "DP1.20275.001",
        "DP1.20191.001",
        "DP1.20194.001",
        "DP1.20206.001"
        )
neonstore::neon_download(product = dps)

# grab data summaries for 2018-06 onwards

streams_meta = read.csv(file = here::here("data/NEONdata/site_latlong.csv"))
streams_vec = streams_meta$site

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

