# debiasR Training Workshop

This folder scaffolds the seven-part workshop/vignette sequence described in
`../training-workshop-plan.docx`.

The running example is the empirical MSOA travel-to-work workflow from
`debiasRdata`. Vignettes load `msoa_OD_travel2work` as the observed
mobile-phone-derived OD matrix and `census_msoa_OD_travel2work` as the Census
benchmark OD matrix via `debiasR::debiasR_example_data()`.

## Structure

1. `01-landing-page.qmd`
2. `02-why-this-matters.qmd`
3. `03-getting-set-up.qmd`
4. `04-measuring-coverage-bias.qmd`
5. `05-identifying-and-explaining-bias.qmd`
6. `06-adjusting-biases.qmd`
7. `07-validation.qmd`

Supporting materials belong in:

- `data/README.md`
- `figures/README.md`
- `exercises/README.md`

Longer method-testing notebooks live in `testing/`, including
`testing/empirical-methods-walkthrough.qmd`.
