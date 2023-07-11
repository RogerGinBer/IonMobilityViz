## Special dependencies that might change more often
## Bruker MS data interface
BiocManager::install('Spectra')
remotes::install_github("michalsta/opentims", subdir="opentimsr", force = TRUE)
remotes::install_github('rformassspectrometry/MsBackendTimsTof', force = TRUE)
remotes::install_github(repo="RogerGinBer/xcms", ref="spectra-ion-mobility", force = TRUE)
