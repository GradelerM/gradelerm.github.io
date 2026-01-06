# ========================================================
# PREP LM DATA FOR GEONUM EVAL
# ========================================================

library(readr)
library(sf)

library(dplyr)
library(janitor)
library(tidyr)
library(stringr)
library(purrr)
library(lubridate)

library(ggplot2)
library(tmap)

library(rnaturalearth)

# --- Filters ---
# Deal size >= 200 ha
# Negotiation status = Concluded
# Year of initiation >= 2000 (unknown included)
# Implementation status == In Operation
# Intention of investment == Fodder crops
# Scope == Transnational

# --- Import LM countries ---
source_lm_countries <- read_delim(
  "data/scripts/2025_dataprep/landmatrix_data/lm_countries.csv"
)

# --- Import LM source data ---
source_deals <- read_delim(
  "data/scripts/2025_dataprep/landmatrix_data/export/deals.csv",
  delim = ";"
) %>%
  clean_names()

source_locations <- st_read(
  "data/scripts/2025_dataprep/landmatrix_data/locations.geojson"
)

# --- Add only one location to each deal ---
source_deals_loc <- source_deals %>%
  left_join(
    source_locations %>% group_by(deal_id) %>% slice(1),
    by = "deal_id"
  ) %>%
  st_as_sf()

ggplot(source_deals_loc) +
  geom_sf()

# --- Prep data with interesting columns ---
source_deals_columns_selection <- source_deals_loc %>%
  # Only keep deals with a sufficient level of accuracy
  filter(
    level_of_accuracy %in%
      c("COORDINATES", "EXACT_LOCATION", "APPROXIMATE_LOCATION")
  ) %>%
  select(
    deal_id,
    deal_size,
    region,
    country,
    created_at,
    crops_area_yield_export,
    name_of_community,
    name_of_indigenous_people,
    community_consultation,
    community_reaction,
    presence_of_land_conflicts,
    displacement_of_people,
    negative_impacts_for_local_communities,
    materialized_benefits_for_local_communities
  )

# --- Unnest column: crops_area_yield_export ---
column_crops <- source_deals_columns_selection %>%
  st_drop_geometry() %>%
  select(deal_id, crops_area_yield_export) %>%
  # Retrieve current
  mutate(crops_by_year = str_split(crops_area_yield_export, "\\|")) %>%
  mutate(crops_current = map_chr(crops_by_year, first)) %>%
  # Only keep production name
  mutate(crop_current_split = str_split(crops_current, "#")) %>%
  mutate(crops = map_chr(crop_current_split, last)) %>%
  select(deal_id, crops) %>%
  # Create unique columns for crops we want to focus on
  mutate(
    crop_oil_palm = case_when(str_detect(crops, "Oil Palm") ~ T, .default = F)
  ) %>%
  mutate(
    crop_sugar_cane = case_when(
      str_detect(crops, "Sugar Cane") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    crop_soya_beans = case_when(
      str_detect(crops, "Soya Beans") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    crop_corn = case_when(str_detect(crops, "Corn (Maize)") ~ T, .default = F)
  ) %>%
  mutate(
    crop_jatropha = case_when(str_detect(crops, "Jatropha") ~ T, .default = F)
  )

# --- Unnest columns: negative impacts ---
column_negative_impacts <- source_deals_columns_selection %>%
  st_drop_geometry() %>%
  select(
    deal_id,
    displacement_of_people,
    negative_impacts_for_local_communities
  ) %>%
  # Create columns
  mutate(
    impact_environmental_degradation = case_when(
      str_detect(
        negative_impacts_for_local_communities,
        "Environmental degradation"
      ) ~ T,
      .default = F
    )
  ) %>%
  mutate(
    impact_socio_economic = case_when(
      str_detect(negative_impacts_for_local_communities, "Socio-economic") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    impact_cultural_loss = case_when(
      str_detect(negative_impacts_for_local_communities, "Cultural loss") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    impact_displacement = case_when(
      str_detect(negative_impacts_for_local_communities, "Displacement") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    impact_eviction = case_when(
      str_detect(negative_impacts_for_local_communities, "Eviction") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    impact_violence = case_when(
      str_detect(negative_impacts_for_local_communities, "Violence") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    impact_other = case_when(
      str_detect(negative_impacts_for_local_communities, "Other") ~ T,
      .default = F
    )
  ) %>%
  # Extra info on displacement from "displacement" column
  mutate(
    impact_displacement = case_when(
      displacement_of_people == "Yes" ~ T,
      .default = impact_displacement
    )
  ) %>%
  select(-displacement_of_people)

# --- Unnest columns: benefits ---
column_benefits <- source_deals_columns_selection %>%
  st_drop_geometry() %>%
  select(deal_id, materialized_benefits_for_local_communities) %>%
  # Create columns
  mutate(
    benefit_health = case_when(
      str_detect(materialized_benefits_for_local_communities, "Health") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    benefit_education = case_when(
      str_detect(materialized_benefits_for_local_communities, "Education") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    benefit_productive_infrastructure = case_when(
      str_detect(
        materialized_benefits_for_local_communities,
        "Productive infrastructure"
      ) ~ T,
      .default = F
    )
  ) %>%
  mutate(
    benefit_roads = case_when(
      str_detect(materialized_benefits_for_local_communities, "Roads") ~ T,
      .default = F
    )
  ) %>%
  mutate(
    benefit_capacity_building = case_when(
      str_detect(
        materialized_benefits_for_local_communities,
        "Capacity building"
      ) ~ T,
      .default = F
    )
  ) %>%
  mutate(
    benefit_community_shares = case_when(
      str_detect(
        materialized_benefits_for_local_communities,
        "Community shares in the investment project"
      ) ~ T,
      .default = F
    )
  ) %>%
  mutate(
    benefit_financial_support = case_when(
      str_detect(
        materialized_benefits_for_local_communities,
        "Financial support"
      ) ~ T,
      .default = F
    )
  ) %>%
  mutate(
    benefit_other = case_when(
      str_detect(materialized_benefits_for_local_communities, "Other") ~ T,
      .default = F
    )
  )

# --- Merge everything together + last cleanup ---
# Only keep deals with known crops
deals <- source_deals_columns_selection %>%
  select(
    id = deal_id,
    surface_ha = deal_size,
    region,
    country,
    created_at,
    name_of_community,
    name_of_indigenous_people,
    community_consultation,
    community_reaction
  ) %>%
  # ID as integer
  mutate(id = as.integer(id)) %>%
  # Date as year (integer)
  mutate(created_at = as.integer(year(created_at))) %>%
  # Create "indigenous people and local communities" column
  mutate(
    indigenous_people_or_local_communities = case_when(
      is.na(name_of_community) &
        is.na(name_of_indigenous_people) &
        is.na(community_consultation) &
        is.na(community_reaction) ~ F,
      .default = T
    )
  ) %>%
  relocate(
    indigenous_people_or_local_communities,
    .before = name_of_community
  ) %>%
  select(-name_of_community, -name_of_indigenous_people) %>%
  # Add crops
  left_join(column_crops, by = join_by("id" == "deal_id")) %>%
  # Add negative impacts
  left_join(column_negative_impacts, by = join_by("id" == "deal_id")) %>%
  # Add benefits
  left_join(column_benefits, by = join_by("id" == "deal_id")) %>%
  # Reorder elements
  relocate(geometry, .after = benefit_other) %>%
  # Remove other useless elements
  select(-crop_corn, -crop_jatropha) %>%
  # Add country iso code
  left_join(
    source_lm_countries %>% select(name, code_alpha3),
    by = join_by("country" == "name")
  ) %>%
  relocate(code_alpha3, .after = country)

# --- Few tests to define subjects for students ---
world <- ne_countries(scale = "medium", returnclass = "sf")

# Crop production in the world (basemap for everyone)
# Allow to highlight/filter oil palm, sugar cane, soya beans
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = deals) +
  theme_minimal()

# I want one layer with proportional circles and/or one choropleth
deals %>%
  st_drop_geometry() %>%
  group_by(code_alpha3) %>%
  summarise(n = n(), surface_ha = sum(surface_ha, na.rm = T)) %>%
  left_join(
    world %>% select(su_a3, geometry),
    by = join_by("code_alpha3" == "su_a3")
  ) %>%
  st_as_sf() %>%
  st_centroid() %>%
  ggplot() +
  geom_sf(data = world) +
  geom_sf(aes(size = n)) +
  theme_minimal()

# One group to show the investments increase over the years
# One group to work on the negative impacts on local communities, show the negative side of the data
# One group to work on benefits, want to show the positive side of the data
# -> For each of the above, focus on soy, oil palm or sugar cane depending on the groups?

# --- Export data once ready ---
st_write(deals, dsn = "data/land_matrix/land_matrix_agri.gpkg", layer = "deals")

# --- Read and display data ---
land_matrix_agri <- st_read(
  "data/land_matrix/land_matrix_agri.gpkg",
  layer = "deals"
)
