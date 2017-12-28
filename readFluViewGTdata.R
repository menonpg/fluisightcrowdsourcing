# devtools::install_github("hrbrmstr/cdcfluview")
# library(cdcfluview)

utils::globalVariables(c(".", "mmwrid", "season", "seasonid"))

# CDC U.S. region names to ID map
.region_map <- c(national=3, hhs=1, census=2, state=5)

# CDC hospital surveillance surveillance area name to internal pkg use map
.surv_map <- c(`FluSurv-NET`="flusurv", `EIP`="eip", `IHSP`="ihsp")
.surv_rev_map <- c(flusurv="FluSurv-NET", eip="EIP", ihsp="IHSP")

# CDC P&I mortality GepID mapping
.geoid_map <- c(national="1", state="2", region="3")

# Our bot's user-agent string
.cdcfluview_ua <- "Mozilla/5.0 (compatible; R-cdcvluview Bot/2.0; https://github.com/hrbrmstr/cdcfluview)"

# CDC Basemaps
.national_outline <- "https://gis.cdc.gov/grasp/fluview/FluView2References/Data/US_84.json"
.hhs_subregions_basemap <- "https://gis.cdc.gov/grasp/fluview/FluView2References/Data/HHSRegions_w_SubGroups.json"
.census_divisions_basemap <- "https://gis.cdc.gov/grasp/fluview/FluView2References/Data/CensusDivs_w_SubGroups.json"
.states_basemap <- "https://gis.cdc.gov/grasp/fluview/FluView2References/Data/StatesFluView.json"
.spread_basemap <- "https://gis.cdc.gov/grasp/fluview/FluView8References/Data/States_Territories_labels.json"
.surv_basemap <- "https://gis.cdc.gov/grasp/fluview/FluView1References/data/US_States_w_PR_labels.json"

# CDC Age Groups
.age_grp <- c("0-4 yr", "5-24 yr", "25-64 yr", "65+ yr")

# CDC Virus Groups
.vir_grp <- c("A (Subtyping not Performed)", "A (H1N1)pdm09", "A (Unable to Subtype)",
              "B (Lineage Unspecified)", "A (H1)", "A (H3)", "B (Victoria Lineage)",
              "B (Yamagata Lineage)", "H3N2v")

# Global HTTR timeout
.httr_timeout <- 120

.mcga <- function(tbl) {
  
  x <- colnames(tbl)
  x <- tolower(x)
  x <- gsub("[[:punct:][:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^_|_$)", "", x)
  x <- gsub("^x_", "", x)
  x <- make.unique(x, sep = "_")
  
  colnames(tbl) <- x
  
  tbl
  
}

to_num <- function(x) {
  x <- gsub("%", "", x, fixed=TRUE)
  x <- gsub(">", "", x, fixed=TRUE)
  x <- gsub("<", "", x, fixed=TRUE)
  x <- gsub(",", "", x, fixed=TRUE)
  x <- gsub(" ", "", x, fixed=TRUE)
  as.numeric(x)
}

ilinet <- function(region=c("national", "hhs", "census", "state"), years=NULL) {
  
  region <- match.arg(tolower(region), c("national", "hhs", "census", "state"))
  
  meta <- jsonlite::fromJSON("https://gis.cdc.gov/grasp/flu2/GetPhase02InitApp?appVersion=Public")
  
  list(
    AppVersion = "Public",
    DatasourceDT = list(list(ID = 1, Name = "ILINet")),
    RegionTypeId = .region_map[region]
  ) -> params
  
  params$SubRegionsDT <- switch(region,
                                national = { list(list(ID=0, Name="")) },
                                hhs = { lapply(1:10, function(i) list(ID=i, Name=as.character(i))) },
                                census = { lapply(1:9, function(i) list(ID=i, Name=as.character(i))) },
                                state = { lapply(1:59, function(i) list(ID=i, Name=as.character(i))) }
  )
  
  available_seasons <- sort(meta$seasons$seasonid)
  
  if (is.null(years)) { # ALL YEARS
    years <- available_seasons
  } else { # specified years or seasons or a mix
    
    years <- as.numeric(years)
    years <- ifelse(years > 1996, years - 1960, years)
    years <- sort(unique(years))
    years <- years[years %in% available_seasons]
    
    if (length(years) == 0) {
      years <- rev(sort(meta$seasons$seasonid))[1]
      curr_season_descr <- meta$seasons[meta$seasons$seasonid == years, "description"]
      message(sprintf("No valid years specified, defaulting to this flu season => ID: %s [%s]",
                      years, curr_season_descr))
    }
    
  }
  
  params$SeasonsDT <- lapply(years, function(i) list(ID=i, Name=as.character(i)))
  
  tf <- tempfile(fileext = ".zip")
  td <- tempdir()
  
  on.exit(unlink(tf), TRUE)
  
  httr::POST(
    url = "https://gis.cdc.gov/grasp/flu2/PostPhase02DataDownload",
    httr::user_agent(.cdcfluview_ua),
    httr::add_headers(
      Origin = "https://gis.cdc.gov",
      Accept = "application/json, text/plain, */*",
      Referer = "https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html"
    ),
    encode = "json",
    body = params,
    # httr::verbose(),
    httr::write_disk(tf)
  ) -> res
  
  httr::stop_for_status(res)
  
  nm <- unzip(tf, overwrite = TRUE, exdir = td)
  
  xdf <- read.csv(nm, skip = 1, stringsAsFactors=FALSE)
  xdf <- .mcga(xdf)
  
  suppressWarnings(xdf$weighted_ili <- to_num(xdf$weighted_ili))
  suppressWarnings(xdf$unweighted_ili <- to_num(xdf$unweighted_ili))
  suppressWarnings(xdf$age_0_4 <- to_num(xdf$age_0_4))
  suppressWarnings(xdf$age_25_49 <- to_num(xdf$age_25_49))
  suppressWarnings(xdf$age_25_64 <- to_num(xdf$age_25_64))
  suppressWarnings(xdf$age_5_24 <- to_num(xdf$age_5_24))
  suppressWarnings(xdf$age_50_64 <- to_num(xdf$age_50_64))
  suppressWarnings(xdf$age_65 <- to_num(xdf$age_65))
  suppressWarnings(xdf$ilitotal <- to_num(xdf$ilitotal))
  suppressWarnings(xdf$num_of_providers <- to_num(xdf$num_of_providers))
  suppressWarnings(xdf$total_patients <- to_num(xdf$total_patients))
  suppressWarnings(xdf$week_start <- as.Date(sprintf("%s-%02d-1", xdf$year, xdf$week), "%Y-%U-%u"))
  
  if (region == "national") xdf$region <- "National"
  if (region == "hhs") xdf$region <- factor(xdf$region, levels=sprintf("Region %s", 1:10))
  
  class(xdf) <- c("tbl_df", "tbl", "data.frame")
  
  suppressMessages(readr::type_convert(xdf))
  
}

library(hrbrthemes)
library(tidyverse)

# current verison
# packageVersion("cdcfluview")

# Age Group Distribution of Influenza Positive Tests Reported by Public Health Laboratories
# glimpse(age_group_distribution(years=2015))

# Retrieve CDC U.S. Coverage Map
# plot(cdc_basemap("national"))



## TESTS:
# walk(c("national", "hhs", "census", "state"), ~{
#   
#   ili_df <- ilinet(region = .x)
#   
#   print(glimpse(ili_df))
#   
#   # ggplot(ili_df, aes(week_start, unweighted_ili, group=region, color=region)) +
#   #   geom_line() +
#   #   viridis::scale_color_viridis(discrete=TRUE) +
#   #   labs(x=NULL, y="Unweighted ILI", title=ili_df$region_type[1]) +
#   #   theme_ipsum_rc(grid="XY") +
#   #   theme(legend.position = "none") -> gg
#   # 
#   # print(gg)
#   
# })

# ili_df_National <- ilinet(region = c("national"))     
# ili_df_National <- ili_df_National[ili_df_National$year %in% c(2017,2018), c("region","year", "week", "weighted_ili")]
# View(ili_df_National)
# ili_df_HHS <- ilinet(region = c( "hhs" ))
# ili_df_HHS <- ili_df_National[ili_df_National$year %in% c(2017,2018), c("region","year", "week", "weighted_ili")]
# View(ili_df_HHS)
