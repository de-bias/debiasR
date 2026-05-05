# Workshop Data

Workshop examples now use the empirical MSOA travel-to-work inputs from
`debiasRdata`.

The MPD input is `msoa_OD_travel2work.csv` from the Zenodo-derived
`debiasRdata` package. The benchmark is the matching Census 2021 `ODWP01EW`
MSOA workplace-flow extract, exposed in the vignette workflow as
`census_msoa_OD_travel2work`.

Use `debiasR::debiasR_example_data()` in vignettes so both raw files are
normalised to the package `origin`, `destination`, `flow` schema before examples
run. The helper also derives the area-level `coverage`, `active_users`,
`population`, and `covariates` tables used by the adjustment and validation
examples.
