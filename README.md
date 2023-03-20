# WI-Assembly-Hypotheticals-2022

This repository includes the results of Wisconsin's 2022 November election tallied up in selected real and hypothetical state Assembly district schemes.

## Data

The file `WI_2022_Votes_in_Assembly_Districts.csv` contains the following columns:

* plan - one of "2022" for the current map (drawn by legislative Republicans), "2012" for the previous map, "Evers least change" for the map submitted by Gov. Evers to the WI Supreme Court, or "People's Map Commission" for the final map submitted by the body.
* office - one of "GOV" (Governor), "SOS" (Secretary of State), "USH" (U.S. House), "USS" (U.S. Senate), "WAG" (WI Attorney General), "WSA" (WI State Assembly), or "WST" (WI State Treasurer). WI State Senate results are not shown because they only occur in half of districts in any given election year, making reallocation impossible.
* district - a number, 1 - 99
* DEM - votes cast for the Democratic candidate (may be fractional in the case of reallocated districts)
* REP - votes cast for the Republican candidate (may be fractional in the case of reallocated districts)
* TOT - total votes cast for any candidate (may be fractional in the case of reallocated districts)
* margin - the Democratic margin (Dem % minus Rep %)
* winner - one of "dem" or "rep"

The folder `/district-shapes/` contains geojson files for each Assembly map.

## Methodology

Votes were allocated as follows:

* I created a reporting unit GIS file following the process [outlined here](https://github.com/jdjohn215/wi-nov-2022).
* I obtained the WI voter file, with records for all the individuals who voted in the 2022 November election.
* I intersected the address coordinates for each 2022 voter with (1) the reporting unit polygon file and (2) each alternative Assembly district polygon.
* Having coded each voter by their reporting unit and district under each mapping scheme, I calculated allocation factors between each reporting unit and the selected Assembly maps. The allocation factor is the proportion of voters in that reporting unit also located in the given Assembly district. In the vast majority of cases, reporting units were entirely located within a single Assembly district.
* For those cases where a reporting unit's voters straddled an Assembly district, I multiplied the vote totals for that reporting unit by the relevant allocation factor.

