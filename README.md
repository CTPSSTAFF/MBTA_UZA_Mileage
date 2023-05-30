readme
================

## Generating MBTA Track Mileage by UZA

In 2022, the US Census Bureau released updated urbanized area
boundaries. The `.qmd` in this repo outlines the process of intersecting
the [MassGIS Data:
Trains](https://www.mass.gov/info-details/massgis-data-trains) dataset
over these geographies.

We use the MassGIS dataset because it appears to have more detail on the
layout of the actual tracks–it appears to account for single track
segments and all of the tracks at terminal stations. The GTFS file is
used to more accurately end the Foxboro service, which is represented as
the full length of service between Walpole and the Providence Line.
Other changes including fixing a small segment of track on the Lowell
Line that was observed during the checking process. We also remove
Plymouth Station, which is no longer in service.

#### Sources

Trains:

- Tracks: https://www.mass.gov/info-details/massgis-data-trains
- Tracks (GTFS): https://cdn.mbtace.com/archive/20220812.zip

UZAs:

- 2010 UZAs:
  https://www2.census.gov/geo/tiger/TIGER2020/UAC/tl_2020_us_uac10.zip
- 2020 UZAs:
  https://www2.census.gov/geo/tiger/TIGER2020/UAC/tl_2020_us_uac20.zip
