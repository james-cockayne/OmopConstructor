
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OmopConstructor

<!-- badges: start -->

[![R-CMD-check](https://github.com/OHDSI/OmopConstructor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/OHDSI/OmopConstructor/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/OmopConstructor)](https://CRAN.R-project.org/package=OmopConstructor)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

**OmopConstructor** is a package that contains functionality to
construct standardised tables from health care data formatted according
to the Observational Medical Outcomes Partnership Common Data Model
([OMOP CDM](https://ohdsi.github.io/CommonDataModel/)). The package
includes tools to build key tables such as observation period and drug
era, among others.

## Tested sources

[![local datasets with
omopgenerics](https://github.com/OHDSI/OmopConstructor/actions/workflows/test-local.yaml/badge.svg?branch=main)](https://github.com/OHDSI/OmopConstructor/actions/workflows/test-local.yaml)
[![DuckDB with
CDMConnector](https://github.com/OHDSI/OmopConstructor/actions/workflows/test-duckdb-CDMConnector.yaml/badge.svg?branch=main)](https://github.com/OHDSI/OmopConstructor/actions/workflows/test-duckdb-CDMConnector.yaml)

## Installation

You can install OmopConstructor from cran using:

``` r
install.packages("OmopConstructor")
```

Or, you can install the development version of OmopConstructor from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pkg_install("ohdsi/OmopConstructor")
```

## Main functionality

Currently **OmopConstructor** main functionalities are:

- [`generateObservationPeriod()`](https://ohdsi.github.io/OmopConstructor/reference/generateObservationPeriod.html)
  to generate observation periods based on the data already in the `cdm`
  object.

### Building observation periods

You can generate the observation period table directly from the data
contained in the `cdm` object, which is particularly useful for creating
custom observation periods on a study-by-study basis. In this example,
the observation period is defined as one record per person, starting
from the first available observation in the data and ending at the
earliest of the following: (1) a recorded death, (2) reaching 120 years
of age, or (3) the extraction date (here, set to 01-01-2010).

``` r
library(omock)
library(OmopConstructor)

cdm <- mockCdmFromDataset(datasetName = "GiBleed", source = "duckdb")
#> ℹ Reading GiBleed tables.
#> ℹ Adding drug_strength table.

cdm <- generateObservationPeriod(cdm = cdm,
                                 collapseEra = Inf,
                                 persistenceWindow = Inf, 
                                 censorDate = as.Date("2010-01-01"), 
                                 censorAge = 120)
cdm
#> 
#> ── # OMOP CDM reference (duckdb) of GiBleed ────────────────────────────────────
#> • omop tables: care_site, cdm_source, concept, concept_ancestor, concept_class,
#> concept_relationship, concept_synonym, condition_era, condition_occurrence,
#> cost, death, device_exposure, domain, dose_era, drug_era, drug_exposure,
#> drug_strength, fact_relationship, location, measurement, metadata, note,
#> note_nlp, observation, observation_period, payer_plan_period, person,
#> procedure_occurrence, provider, relationship, source_to_concept_map, specimen,
#> visit_detail, visit_occurrence, vocabulary
#> • cohort tables: -
#> • achilles tables: -
#> • other tables: -
```

We can use *OmopSketch* to visualise the characteristics of the new
observation period table that we have built:

``` r
library(OmopSketch)

result <- summariseObservationPeriod(observationPeriod = cdm$observation_period)

tableObservationPeriod(result = result)
```

<div id="owvdanqzfq" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#owvdanqzfq table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#owvdanqzfq thead, #owvdanqzfq tbody, #owvdanqzfq tfoot, #owvdanqzfq tr, #owvdanqzfq td, #owvdanqzfq th {
  border-style: none;
}
&#10;#owvdanqzfq p {
  margin: 0;
  padding: 0;
}
&#10;#owvdanqzfq .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #D9D9D9;
  border-right-style: solid;
  border-right-width: 3px;
  border-right-color: #D9D9D9;
  border-bottom-style: solid;
  border-bottom-width: 3px;
  border-bottom-color: #D9D9D9;
  border-left-style: solid;
  border-left-width: 3px;
  border-left-color: #D9D9D9;
}
&#10;#owvdanqzfq .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#owvdanqzfq .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#owvdanqzfq .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#owvdanqzfq .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#owvdanqzfq .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#owvdanqzfq .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#owvdanqzfq .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#owvdanqzfq .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#owvdanqzfq .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#owvdanqzfq .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#owvdanqzfq .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#owvdanqzfq .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#owvdanqzfq .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#owvdanqzfq .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#owvdanqzfq .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owvdanqzfq .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#owvdanqzfq .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#owvdanqzfq .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#owvdanqzfq .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owvdanqzfq .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#owvdanqzfq .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owvdanqzfq .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#owvdanqzfq .gt_table_body {
  border-top-style: solid;
  border-top-width: 3px;
  border-top-color: #D9D9D9;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owvdanqzfq .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#owvdanqzfq .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#owvdanqzfq .gt_left {
  text-align: left;
}
&#10;#owvdanqzfq .gt_center {
  text-align: center;
}
&#10;#owvdanqzfq .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#owvdanqzfq .gt_font_normal {
  font-weight: normal;
}
&#10;#owvdanqzfq .gt_font_bold {
  font-weight: bold;
}
&#10;#owvdanqzfq .gt_font_italic {
  font-style: italic;
}
&#10;#owvdanqzfq .gt_super {
  font-size: 65%;
}
&#10;#owvdanqzfq .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#owvdanqzfq .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#owvdanqzfq .gt_indent_1 {
  text-indent: 5px;
}
&#10;#owvdanqzfq .gt_indent_2 {
  text-indent: 10px;
}
&#10;#owvdanqzfq .gt_indent_3 {
  text-indent: 15px;
}
&#10;#owvdanqzfq .gt_indent_4 {
  text-indent: 20px;
}
&#10;#owvdanqzfq .gt_indent_5 {
  text-indent: 25px;
}
&#10;#owvdanqzfq .katex-display {
  display: inline-flex !important;
  margin-bottom: 0.75em !important;
}
&#10;#owvdanqzfq div.Reactable > div.rt-table > div.rt-thead > div.rt-tr.rt-tr-group-header > div.rt-th-group:after {
  height: 0px !important;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" style="text-align: center; font-weight: bold; border-left-width: 1px; border-left-style: solid; border-left-color: White; border-right-width: 1px; border-right-style: solid; border-right-color: White; border-top-width: 1px; border-top-style: solid; border-top-color: White; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: White;" scope="col" id="Observation-period-ordinal">Observation period ordinal</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" style="text-align: center; font-weight: bold; border-left-width: 1px; border-left-style: solid; border-left-color: White; border-right-width: 1px; border-right-style: solid; border-right-color: White; border-top-width: 1px; border-top-style: solid; border-top-color: White; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: White;" scope="col" id="Variable-name">Variable name</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" style="text-align: center; font-weight: bold; border-left-width: 1px; border-left-style: solid; border-left-color: White; border-right-width: 1px; border-right-style: solid; border-right-color: White; border-top-width: 1px; border-top-style: solid; border-top-color: White; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: White;" scope="col" id="Estimate-name">Estimate name</th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="1" style="background-color: #D9D9D9; text-align: center; font-weight: bold; border-left-width: 1px; border-left-style: solid; border-left-color: White; border-right-width: 1px; border-right-style: solid; border-right-color: White; border-top-width: 1px; border-top-style: solid; border-top-color: White; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: White;" scope="col" id="spanner-[header_name]CDM name&#10;[header_level]GiBleed">
        <div class="gt_column_spanner">CDM name</div>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="background-color: #E1E1E1; text-align: center; font-weight: bold; border-left-width: 1px; border-left-style: solid; border-left-color: White; border-right-width: 1px; border-right-style: solid; border-right-color: White; border-top-width: 1px; border-top-style: solid; border-top-color: White; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: White;" scope="col" id="[header_name]CDM-name-[header_level]GiBleed">GiBleed</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left;">all</td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Number records</td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">N</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">2,694</td></tr>
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000;"></td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Number subjects</td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">N</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">2,694</td></tr>
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000;"></td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Records per person</td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">mean (sd)</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">1.00 (0.00)</td></tr>
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000;"></td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;"></td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">median [Q25 - Q75]</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">1 [1 - 1]</td></tr>
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000;"></td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Duration in days</td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">mean (sd)</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">18,552.21 (5,740.03)</td></tr>
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000;"></td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;"></td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">median [Q25 - Q75]</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">17,724 [14,289 - 21,288]</td></tr>
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left;">1st</td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Number subjects</td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">N</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">2,694</td></tr>
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000;"></td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Duration in days</td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">mean (sd)</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">18,552.21 (5,740.03)</td></tr>
    <tr><td headers="Observation period ordinal" class="gt_row gt_left" style="text-align: left; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000;"></td>
<td headers="Variable name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: hidden; border-top-color: #000000; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;"></td>
<td headers="Estimate name" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">median [Q25 - Q75]</td>
<td headers="[header_name]CDM name
[header_level]GiBleed" class="gt_row gt_left" style="text-align: right;">17,724 [14,289 - 21,288]</td></tr>
  </tbody>
  &#10;  
</table>
</div>
