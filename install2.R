## Special dependencies that might change more often
## Bruker MS data interface
BiocManager::install('Spectra')
remotes::install_github("michalsta/opentims", subdir="opentimsr")
remotes::install_github('rformassspectrometry/MsBackendTimsTof')
remotes::install_github(repo="RogerGinBer/xcms", ref="spectra-ion-mobility")
