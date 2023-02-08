## Special dependencies
install.packages("gert")
install.packages("gh")
install.packages("devtools")

BiocManager::install('Spectra')
devtools::install_github("michalsta/opentims", subdir="opentimsr")
devtools::install_github('rformassspectrometry/MsBackendTimsTof')
