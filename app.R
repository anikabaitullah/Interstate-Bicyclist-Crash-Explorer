
# ============================================================
# Interstate Bicyclist Fatal Crash Explorer
# Study CCA Profiles + Custom CCA + Crash/Street View +
# Cluster Membership Explanation + Roadway Context + Countermeasure Mechanism Explorer
#
# Analytical design:
# - Dark and Daylight are ALWAYS separate for Study CCA.
# - Study CCA/cluster/SHAP panels display the validated outputs used in the manuscript.
# - Custom CCA is the live recomputation workspace using the supplied CCA workflow.
# - No model-variable-importance panel is displayed.
# - Custom CCA lets users choose variables, filters, exclusions and K.
# - Crash diagnosis merges FARS/PBCAT + Study CCA/SHAP + validated/user-confirmed roadway context +
#   user-confirmed Google Street View observations. Mapped roadway tags are hints only.
# ============================================================

required_packages <- c(
  "shiny","leaflet","ggplot2","ggrepel","clustrd","FactoMineR",
  "htmltools","scales"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  if (interactive()) {
    install.packages(missing_packages, repos = "https://cloud.r-project.org")
  } else {
    stop(
      paste0(
        "Missing packages: ", paste(missing_packages, collapse = ", "),
        "\nInstall with install.packages(c(",
        paste(sprintf('"%s"', missing_packages), collapse = ", "),
        "))"
      ),
      call. = FALSE
    )
  }
}

suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(ggplot2)
  library(ggrepel)
  library(clustrd)
  library(FactoMineR)
  library(htmltools)
  library(scales)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ============================================================
# Optional private API configuration
# ============================================================
# The app never stores an API key in app.R. If the user's private AWS API key
# is available in the environment, it can be used only to refine the written
# case explanation from already-established crash/CCA/SHAP/site evidence.
# The API is NOT required for mapping, CCA, Street View, or countermeasure logic.
PRIVATE_API_KEY <- Sys.getenv("AWS_BEARER_TOKEN_BEDROCK", unset = "")
PRIVATE_API_REGION <- Sys.getenv("AWS_REGION", unset = "us-east-1")
PRIVATE_API_MODEL <- Sys.getenv(
  "BEDROCK_MODEL_ID",
  unset = "us.anthropic.claude-3-5-haiku-20241022-v1:0"
)
PRIVATE_API_AVAILABLE <- nzchar(PRIVATE_API_KEY)
PRIVATE_API_CACHE <- new.env(parent = emptyenv())
SITE_CONTEXT_CACHE <- new.env(parent = emptyenv())

private_api_text <- function(prompt, cache_key = NULL) {
  if (!PRIVATE_API_AVAILABLE) {
    return(list(ok = FALSE, text = NULL, message = "Private API key is not configured."))
  }
  if (!requireNamespace("curl", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(list(ok = FALSE, text = NULL, message = "Optional packages 'curl' and 'jsonlite' are not installed. Core app functions are unaffected."))
  }
  if (!is.null(cache_key) && exists(cache_key, envir = PRIVATE_API_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = PRIVATE_API_CACHE, inherits = FALSE))
  }

  url <- paste0(
    "https://bedrock-runtime.", PRIVATE_API_REGION,
    ".amazonaws.com/model/", PRIVATE_API_MODEL, "/converse"
  )
  body <- list(
    messages = list(list(
      role = "user",
      content = list(list(text = prompt))
    )),
    inferenceConfig = list(maxTokens = 420, temperature = 0)
  )

  ans <- tryCatch({
    h <- curl::new_handle()
    curl::handle_setheaders(
      h,
      "Content-Type" = "application/json",
      "Authorization" = paste("Bearer", PRIVATE_API_KEY)
    )
    payload <- jsonlite::toJSON(body, auto_unbox = TRUE, null = "null", digits = NA)
    curl::handle_setopt(h, post = TRUE, postfields = payload, timeout = 45)
    resp <- curl::curl_fetch_memory(url, handle = h)
    if (resp$status_code < 200 || resp$status_code >= 300) {
      stop(paste0("API returned HTTP ", resp$status_code))
    }
    parsed <- jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE)
    content <- parsed$output$message$content
    txt <- ""
    if (is.list(content)) {
      for (z in content) {
        if (is.list(z) && !is.null(z$text)) txt <- paste(txt, z$text)
      }
    }
    txt <- trimws(txt)
    if (!nzchar(txt)) stop("API response did not contain text.")
    list(ok = TRUE, text = txt, message = NULL)
  }, error = function(e) {
    list(ok = FALSE, text = NULL, message = conditionMessage(e))
  })

  if (!is.null(cache_key)) assign(cache_key, ans, envir = PRIVATE_API_CACHE)
  ans
}

parse_speed_mph <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_real_)
  val <- suppressWarnings(as.numeric(sub(".*?([0-9]+).*", "\\1", as.character(x))))
  if (!is.finite(val)) NA_real_ else val
}

parse_lanes <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(x)) return(NA_integer_)
  val <- suppressWarnings(as.integer(sub(".*?([0-9]+).*", "\\1", as.character(x))))
  if (is.na(val) || val < 1) NA_integer_ else val
}

fetch_osm_road_context <- function(lat, lon) {
  cache_key <- paste0("osm_", round(lat, 5), "_", round(lon, 5))
  if (exists(cache_key, envir = SITE_CONTEXT_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = SITE_CONTEXT_CACHE, inherits = FALSE))
  }

  # Overpass is used only for reviewed roadway context. No key is required.
  query <- paste0(
    '[out:csv(::id,::type,name,ref,highway,lanes,oneway,maxspeed,lit,shoulder,cycleway,bicycle,access,junction;true;"\t")][timeout:10];',
    'way(around:90,', sprintf("%.7f", lat), ',', sprintf("%.7f", lon), ')[highway];out tags 25;'
  )
  url <- paste0("https://overpass-api.de/api/interpreter?data=", utils::URLencode(query, reserved = TRUE))
  txt <- tryCatch(paste(readLines(url, warn = FALSE), collapse = "\n"), error = function(e) "")
  out <- list(
    available = FALSE, name = NA_character_, ref = NA_character_, highway = NA_character_,
    lanes = NA_character_, oneway = NA_character_, maxspeed = NA_character_, lit = NA_character_,
    shoulder = NA_character_, cycleway = NA_character_, bicycle = NA_character_, access = NA_character_,
    junction = NA_character_, suggestions = character(0), roadway_form = "Interstate / controlled-access facility",
    lane_summary = "Exact lane count not mapped", divided = NA, total_lanes = NA_integer_, source = "Study facility context"
  )

  if (nzchar(txt)) {
    tab <- tryCatch(read.delim(text = txt, sep = "\t", quote = "", check.names = FALSE, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(tab) && nrow(tab) > 0 && "highway" %in% names(tab)) {
      hw <- tolower(ifelse(is.na(tab$highway), "", tab$highway))
      priority <- match(hw, c("motorway","motorway_link","trunk","trunk_link","primary","primary_link","secondary","secondary_link","tertiary","residential","service","cycleway","footway"))
      priority[is.na(priority)] <- 999
      row <- tab[order(priority), , drop = FALSE][1, , drop = FALSE]
      getv <- function(nm) if (nm %in% names(row) && nzchar(as.character(row[[nm]][1]))) as.character(row[[nm]][1]) else NA_character_
      out$available <- TRUE
      out$name <- getv("name"); out$ref <- getv("ref"); out$highway <- getv("highway")
      out$lanes <- getv("lanes"); out$oneway <- getv("oneway"); out$maxspeed <- getv("maxspeed")
      out$lit <- getv("lit"); out$shoulder <- getv("shoulder"); out$cycleway <- getv("cycleway")
      out$bicycle <- getv("bicycle"); out$access <- getv("access"); out$junction <- getv("junction")
      out$source <- "OpenStreetMap reviewed roadway context"
    }
  }

  nz_text <- function(x) if (is.null(x) || length(x) == 0 || is.na(x) || !nzchar(as.character(x))) "" else as.character(x)
  hw <- tolower(nz_text(out$highway))
  lanes_tag <- parse_lanes(out$lanes)
  is_ramp <- grepl("_link$", hw) || grepl("ramp", tolower(nz_text(out$junction)))

  # Mapped tags are retained only as review hints. They are NOT treated as validated
  # divided/undivided status or lane count for the crash-mechanism diagram.
  out$divided <- NA
  out$total_lanes <- NA_integer_
  out$roadway_form <- if (is_ramp) {
    "Mapped ramp / link context (review required)"
  } else if (hw %in% c("motorway","trunk")) {
    "Mapped controlled-access roadway (review required)"
  } else if (nzchar(hw)) {
    paste0("Mapped road class: ", hw, " (review required)")
  } else {
    "Mapped roadway context unavailable"
  }
  out$lane_summary <- if (!is.na(lanes_tag)) {
    paste0("OSM lanes tag = ", lanes_tag, " (review required)")
  } else "No validated lane count"

  sugg <- character(0)
  # Every record in the study is an interstate crash; high-speed geometry is therefore a defensible base suggestion.
  sugg <- c(sugg, "highspeed")
  if (is_ramp) sugg <- c(sugg, "ramp", "merge")
  if (tolower(nz_text(out$shoulder)) %in% c("no","none","absent")) sugg <- c(sugg, "limited_shoulder")
  if (tolower(nz_text(out$lit)) == "no") sugg <- c(sugg, "low_light")
  if (tolower(nz_text(out$bicycle)) %in% c("no","dismount") || tolower(nz_text(out$access)) %in% c("no","private")) sugg <- c(sugg, "route_guidance")
  out$suggestions <- unique(sugg)
  assign(cache_key, out, envir = SITE_CONTEXT_CACHE)
  out
}

roadway_display_name <- function(ctx) {
  nm <- ctx$name %||% NA_character_
  rf <- ctx$ref %||% NA_character_
  if (!is.na(nm) && nzchar(nm)) return(nm)
  if (!is.na(rf) && nzchar(rf)) return(rf)
  ctx$roadway_form %||% "Interstate roadway"
}


# ============================================================
# Embedded crash data
# ============================================================

EMBEDDED_CSV <- "CRASH_VEH_NUM1,CRASH_VehPer_NUM1,PedAGE,PedGender,Bike Crash Type,Bike Location,Bicyclist Direction,Season,DOW,Land Type,Lighting Condition,Weather,MV DR Age,MV DR Gender,MV Type,Driver Impairment,Speeding Related,Driver Related Factor,Prior Critical Event,Critical Event Occurred by,Attempted for Avoidance,LATITUDENAME,LONGITUDNAME
2016_200310,2016_2003101,45 to 65 Years,Female,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Rural,Daylight,Clear,25 to 44 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,60.50091667,-150.9950167
2016_607670,2016_6076701,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekend,Urban,Dark,Clear,25 to 44 Years,Male,Bus,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,34.07144444,-118.1239917
2016_624540,2016_6245401,25 to 44 Years,Male,Initial Crossing Paths,Sidewalk,With Traffic,Fall,Weekday,Urban,Dark,Clear,Unknown,Unknown,Unknown,Other,Within Speed Limit,Unknown,Negotiating a Curve,Pedalcyclist/NMV,Unknown,37.63144167,-122.087227799999
2016_1214030,2016_12140301,25 to 44 Years,Male,Motorist Overtaking,Bicycle Lane,With Traffic,Summer,Weekday,Urban,Daylight,Clear,More than 65 Years,Male,Car,Impaired,Within Speed Limit,Aggressive Driving,Going Straight,Involved Vehicle,Unknown,27.931611109999896,-82.5763611099999
2016_1214490,2016_12144901,25 to 44 Years,Male,Bicyclist Failed to Yield,Sidewalk,With Traffic,Summer,Weekday,Urban,Dark,Clear,45 to 65 Years,Female,Large Truck,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,26.68896944,-81.7985999999999
2016_1222660,2016_12226601,45 to 65 Years,Male,Motorist Turning  Error,Sidewalk,Facing Traffic,Fall,Weekday,Urban,Daylight,Unclear,45 to 65 Years,Male,Large Truck,Unimpaired,Within Speed Limit,,At Curve,Involved Vehicle,Unknown,26.318088889999895,-80.11615
2016_2205320,2016_22053201,45 to 65 Years,Male,Others,Travel Lane,Facing Traffic,Fall,Weekend,Urban,Daylight,Clear,Unknown,Unknown,Unknown,Other,Exceed Speed Limit,Unknown,Going Straight,Pedalcyclist/NMV,Unknown,29.9663833299999,-90.0710972199999
2016_4506490,2016_45064901,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,32.86621111,-80.00004443999988
2016_4703070,2016_47030701,45 to 65 Years,Male,Bicyclist Turned,Travel Lane,With Traffic,Spring,Weekend,Urban,Daylight,Clear,Less than 25 Years,Female,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking,36.3127694399999,-88.77293056
2016_4802290,2016_48022901,45 to 65 Years,Male,Others,Sidewalk,Facing Traffic,Winter,Weekday,Urban,Dark,Unclear,Unknown,Unknown,Unknown,Other,Exceed Speed Limit,Unknown,Negotiating a Curve,Pedalcyclist/NMV,Unknown,29.72396111,-95.49170277999987
2016_4804780,2016_48047801,Less than 25,Male,Wrong-Way,Travel Lane,Facing Traffic,Winter,Weekday,Rural,Dark,Clear,25 to 44 Years,Male,Car,Other,Within Speed Limit,Others,Going Straight,Pedalcyclist/NMV,Unknown,30.121127779999895,-93.76747777999987
2016_4808100,2016_48081001,25 to 44 Years,Male,Others,Others,Unknown,Spring,Weekend,Urban,Dark,Clear,Unknown,Unknown,Unknown,Other,Within Speed Limit,,Others,Pedalcyclist/NMV,Unknown,32.7765888899999,-97.31953056
2016_4817360,2016_48173601,45 to 65 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Summer,Weekday,Rural,Dark,Clear,25 to 44 Years,Female,Car,Other,Within Speed Limit,Non-compliant Driving,At Curve,Pedalcyclist/NMV,No Avoidance Maneuver,31.2166,-105.4882611
2016_4818100,2016_48181001,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekend,Rural,Dark,Clear,25 to 44 Years,Male,Car,Impaired,Within Speed Limit,Others,Going Straight,Pedalcyclist/NMV,Unknown,35.1911388899999,-102.0711417
2016_4818560,2016_48185601,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Urban,Dark,Clear,Unknown,Unknown,Unknown,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,32.84004444,-97.28581667
2016_4829860,2016_48298601,Less than 25,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Pick Up,Other,Within Speed Limit,Aggressive Driving,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,31.03790833,-97.47188332999987
2016_5301780,2016_53017801,25 to 44 Years,Male,Bicyclist Turned,Travel Lane,With Traffic,Summer,Weekday,Urban,Daylight,Clear,25 to 44 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,48.74771389,-122.4648833
2016_5302830,2016_53028301,Less than 25,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Urban,Dark,Clear,Less than 25 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,45.7735138899999,-122.6704417
2016_5304270,2016_53042701,25 to 44 Years,Male,Motorist Turning  Error,Travel Lane,Facing Traffic,Fall,Weekday,Urban,Dark,Unclear,45 to 65 Years,Female,Car,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,No Avoidance Maneuver,47.04880833,-122.8203222
2017_401850,2017_4018501,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekend,Urban,Dark,Clear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,Changing Lanes,Pedalcyclist/NMV,Unknown,33.5374249999999,-112.1119083
2017_621800,2017_6218001,More than 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Winter,Weekday,Urban,Dark,Clear,More than 65 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,34.07956111,-117.9980889
2017_1222520,2017_12225201,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekday,Rural,Dark,Clear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,Aggressive Driving,Going Straight,Pedalcyclist/NMV,Unknown,27.99425833,-82.35915833
2017_1601720,2017_16017201,45 to 65 Years,Male,Others,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Unclear,More than 65 Years,Unknown,Unknown,Unimpaired,Within Speed Limit,Unknown,Negotiating a Curve,Pedalcyclist/NMV,Unknown,47.66306111,-116.7473
2017_1704200,2017_17042001,25 to 44 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Summer,Weekday,Rural,Daylight,Clear,45 to 65 Years,Male,Large Truck,Other,Within Speed Limit,Aggressive Driving,Going Straight,Involved Vehicle,Unknown,41.45786667,-90.49507778
2017_2502140,2017_25021401,Less than 25,Male,Bicyclist Failed to Yield,Travel Lane,Unknown,Summer,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,41.95897222,-71.0638333299999
2017_3701460,2017_37014601,45 to 65 Years,Male,Others,Travel Lane,With Traffic,Winter,Weekday,Urban,Dark,Clear,Less than 25 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,35.91586667,-80.00040278
2017_4807350,2017_48073501,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekend,Urban,Dark,Unclear,Less than 25 Years,Female,Car,Impaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,32.53999444,-96.82255277999987
2017_4807350,2017_48073502,45 to 65 Years,Female,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekend,Urban,Dark,Unclear,Less than 25 Years,Female,Car,Impaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,32.53999444,-96.82255277999987
2017_4810110,2017_48101101,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,Facing Traffic,Spring,Weekend,Urban,Dark,Clear,Unknown,Unknown,Unknown,Other,Within Speed Limit,,Others,Pedalcyclist/NMV,Unknown,29.56001944,-98.341825
2017_4813140,2017_48131401,More than 65 Years,Male,Motorist Fail to Yeild,Travel Lane,Facing Traffic,Summer,Weekday,Urban,Daylight,Clear,More than 65 Years,Male,Pick Up,Other,Within Speed Limit,Non-compliant Driving,Going Straight,Pedalcyclist/NMV,Unknown,26.4095,-97.78445555999987
2017_4813470,2017_48134701,45 to 65 Years,Male,Others,Travel Lane,With Traffic,Summer,Weekend,Urban,Dark,Clear,45 to 65 Years,Female,Car,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,29.74239167,-96.32603056
2017_4822830,2017_48228301,25 to 44 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Fall,Weekend,Urban,Dark,Clear,25 to 44 Years,Female,Pick Up,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,29.7076361099999,-95.50965832999987
2017_4823090,2017_48230901,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Unclear,Unknown,Unknown,Unknown,Other,Exceed Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,29.7923583299999,-95.08069166999987
2017_5301950,2017_53019501,45 to 65 Years,Male,Bicyclist Failed to Yield,Travel Lane,Facing Traffic,Summer,Weekday,Urban,Daylight,Clear,Less than 25 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,47.5876583299999,-122.3201944
2018_400360,2018_4003601,45 to 65 Years,Male,Initial Crossing Paths,Travel Lane,Unknown,Winter,Weekend,Urban,Daylight,Unclear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,33.55616667,-112.1130139
2018_404320,2018_4043201,25 to 44 Years,Male,Others,Travel Lane,Facing Traffic,Summer,Weekend,Rural,Dark,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,Others,Unknown,Pedalcyclist/NMV,Unknown,32.30693611,-109.395475
2018_605610,2018_6056101,25 to 44 Years,Male,Motorist Overtaking,Bicycle Lane,With Traffic,Spring,Weekend,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Within Speed Limit,Non-compliant Driving,Going Straight,Pedalcyclist/NMV,Unknown,37.33043889,-121.8665861
2018_625200,2018_6252001,25 to 44 Years,Male,Bicyclist Turned,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Unclear,25 to 44 Years,Female,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,37.33609167,-121.8574833
2018_634360,2018_6343601,25 to 44 Years,Male,Motorist Turning  Error,Sidewalk,With Traffic,Fall,Weekday,Urban,Daylight,Clear,45 to 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,34.04690833,-118.4468139
2018_1205610,2018_12056101,Less than 25,Male,Bicyclist Failed to Yield,Sidewalk,Not applicable,Winter,Weekday,Urban,Dark,Unclear,Less than 25 Years,Female,Car,Unimpaired,Within Speed Limit,Non-compliant Driving,Going Straight,Pedalcyclist/NMV,Unknown,27.94078889,-82.32676944
2018_4004470,2018_40044701,45 to 65 Years,Male,Others,Travel Lane,Facing Traffic,Summer,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Pick Up,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,36.0878,-96.0403
2018_4803990,2018_48039901,Less than 25,Male,Bicyclist Failed to Yield,Travel Lane,Unknown,Winter,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Pick Up,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,32.72973333,-97.78890556
2018_4815390,2018_48153901,25 to 44 Years,Female,Motorist Turning  Error,Sidewalk,With Traffic,Summer,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,32.8623972199999,-96.89375278
2018_4818170,2018_48181701,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekend,Urban,Dark,Clear,25 to 44 Years,Female,Car,Other,Within Speed Limit,Others,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,32.67160278,-96.98511944
2018_4820560,2018_48205601,25 to 44 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Summer,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Car,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,32.66806667,-97.23198888999987
2018_4821680,2018_48216801,45 to 65 Years,Male,Others,Others,Unknown,Fall,Weekend,Urban,Dark,Unclear,Less than 25 Years,Male,Pick Up,Other,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,30.09518333,-94.10149721999989
2019_605370,2019_6053701,More than 65 Years,Male,Initial Crossing Paths,Sidewalk,Facing Traffic,Spring,Weekday,Urban,Daylight,Clear,45 to 65 Years,Male,Pick Up,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,No Avoidance Maneuver,32.6907138899999,-117.1229139
2019_619410,2019_6194101,Less than 25,Male,Bicyclist Failed to Yield,Travel Lane,With Traffic,Summer,Weekend,Urban,Dark,Unclear,45 to 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,32.83299722,-117.1660556
2019_621790,2019_6217901,More than 65 Years,Male,Initial Crossing Paths,Sidewalk,Not applicable,Summer,Weekend,Urban,Dark,Clear,25 to 44 Years,Male,Pick Up,Other,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,33.9343277799999,-118.1769194
2019_630380,2019_6303801,25 to 44 Years,Male,Others,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,More than 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,33.7996083299999,-118.1113167
2019_1309890,2019_13098901,Less than 25,Male,Motorist Turning  Error,Travel Lane,With Traffic,Fall,Weekday,Urban,Daylight,Clear,45 to 65 Years,Female,Small Truck,Other,Within Speed Limit,Non-compliant Driving,Turning Right,Pedalcyclist/NMV,Unknown,30.77611667,-83.29773611
2019_2902460,2019_29024601,45 to 65 Years,Male,Bicyclist Failed to Yield,Travel Lane,Facing Traffic,Spring,Weekday,Urban,Daylight,Clear,Less than 25 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,38.7757777799999,-90.33586110999987
2019_2903880,2019_29038801,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Rural,Dark,Clear,45 to 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking,36.88741667,-94.43075
2019_3600950,2019_36009501,More than 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekend,Urban,Daylight,Clear,More than 65 Years,Male,Pick Up,Other,Within Speed Limit,Non-compliant Driving,Negotiating a Curve,Pedalcyclist/NMV,No Avoidance Maneuver,40.76677222,-73.69566111
2019_4002800,2019_40028001,45 to 65 Years,Male,Others,Travel Lane,Facing Traffic,Summer,Weekend,Urban,Dark,Unclear,25 to 44 Years,Female,Car,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,35.5002861099999,-98.95706110999987
2019_4003840,2019_40038401,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekday,Urban,Dark,Unclear,25 to 44 Years,Male,Car,Unimpaired,Exceed Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,35.22091111,-97.48589721999988
2019_4505110,2019_45051101,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,32.875,-80.02648055999988
2019_4803360,2019_48033601,45 to 65 Years,Male,Motorist Overtaking,Bicycle Lane,With Traffic,Winter,Weekday,Urban,Daylight,Unclear,45 to 65 Years,Female,Small Truck,Impaired,Within Speed Limit,Aggressive Driving,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,29.610030559999895,-98.60382221999987
2019_4819250,2019_48192501,45 to 65 Years,Male,Bicyclist Failed to Yield,Travel Lane,With Traffic,Summer,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Small Truck,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,29.78398333,-95.56266943999987
2019_4826600,2019_48266001,Less than 25,Male,Bicyclist Turned,Travel Lane,With Traffic,Fall,Weekday,Urban,Daylight,Clear,25 to 44 Years,Female,Large Truck,Other,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Braking and Steering Maneuver,29.51791111,-98.46682778
2020_402190,2020_4021901,45 to 65 Years,Male,Others,Travel Lane,Unknown,Spring,Weekday,Rural,Dark,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,99.99989999999987,999.9999
2020_404540,2020_4045401,More than 65 Years,Male,Others,Others,Unknown,Spring,Weekend,Urban,Daylight,Unclear,More than 65 Years,Male,Unknown,Other,Exceed Speed Limit,,Others,Pedalcyclist/NMV,Unknown,31.37422222,-110.9354833
2020_603390,2020_6033901,45 to 65 Years,Male,Bicyclist Turned,Sidewalk,With Traffic,Spring,Weekday,Urban,Daylight,Clear,Less than 25 Years,Male,Large Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,38.6471027799999,-121.3831972
2020_617140,2020_6171401,25 to 44 Years,Female,Bicyclist Turned,Sidewalk,With Traffic,Summer,Weekend,Urban,Daylight,Clear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,33.6650388899999,-117.7935083
2020_2204280,2020_22042801,25 to 44 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Summer,Weekday,Urban,Daylight,Clear,45 to 65 Years,Male,Pick Up,Other,Within Speed Limit,,Changing Lanes,Pedalcyclist/NMV,No Avoidance Maneuver,30.235888889999895,-93.2142
2020_2901590,2020_29015901,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekend,Rural,Dark,Unclear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,36.95113889,-94.42977777999987
2020_3405090,2020_34050901,25 to 44 Years,Male,Others,Others,Unknown,Spring,Weekday,Urban,Daylight,Unclear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,40.55235278,-74.48695277999988
2020_4000010,2020_40000101,25 to 44 Years,Male,Others,Travel Lane,With Traffic,Winter,Weekday,Rural,Daylight,Clear,25 to 44 Years,Female,Pick Up,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,35.4573972199999,-95.4333
2020_4709870,2020_47098701,25 to 44 Years,Male,Others,Travel Lane,Facing Traffic,Fall,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,36.06898056,-89.4035083299999
2021_500270,2021_5002701,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Winter,Weekend,Urban,Dark,Clear,Less than 25 Years,Male,Car,Unimpaired,Exceed Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,36.1741666699999,-94.18640555999987
2021_502970,2021_5029701,25 to 44 Years,Female,Others,Others,Unknown,Summer,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,34.6809277799999,-92.35297778
2021_606890,2021_6068901,45 to 65 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Summer,Weekend,Urban,Daylight,Clear,45 to 65 Years,Male,Pick Up,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,34.067525,-117.9815528
2021_612660,2021_6126601,More than 65 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Spring,Weekday,Urban,Dark,Clear,25 to 44 Years,Female,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,34.03928889,-118.0497417
2021_617150,2021_6171501,45 to 65 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Spring,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Braking,32.73058056,-117.154736099999
2021_629970,2021_6299701,25 to 44 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Fall,Weekend,Urban,Dark,Clear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,38.25291389,-122.066916699999
2021_632260,2021_6322601,45 to 65 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Summer,Weekday,Urban,Daylight,Clear,25 to 44 Years,Female,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,37.82656389,-122.2866194
2021_804210,2021_8042101,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekday,Rural,Daylight,Clear,Less than 25 Years,Male,Pick Up,Unimpaired,Exceed Speed Limit,,At Curve,Pedalcyclist/NMV,Braking,39.2898555599999,-104.8948556
2021_1807860,2021_18078601,Less than 25,Male,Others,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,More than 65 Years,Male,Small Truck,Other,Within Speed Limit,,Changing Lanes,Pedalcyclist/NMV,Braking and Steering Maneuver,40.51878889,-85.54972222
2021_2700860,2021_27008601,More than 65 Years,Male,Bicyclist Failed to Yield,Sidewalk,Facing Traffic,Spring,Weekday,Urban,Daylight,Unclear,45 to 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,No Avoidance Maneuver,45.2993416699999,-93.79853056
2021_2904780,2021_29047801,Less than 25,Female,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,38.64480556,-90.34769443999987
2021_3401850,2021_34018501,25 to 44 Years,Male,Others,Others,Unknown,Spring,Weekend,Urban,Dark,Clear,Less than 25 Years,Female,Car,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,40.7122833299999,-74.20054167
2021_3608930,2021_36089301,25 to 44 Years,Male,Bicyclist Failed to Yield,Travel Lane,Unknown,Fall,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Car,Other,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,42.7120666699999,-73.82689166999988
2021_4104370,2021_41043701,45 to 65 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Fall,Weekday,Urban,Dark,Unclear,25 to 44 Years,Male,Pick Up,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,44.99995278,-122.9992694
2021_4712310,2021_47123101,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Winter,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,35.0153805599999,-85.2769444399999
2021_4820470,2021_48204701,45 to 65 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Summer,Weekday,Rural,Dark,Unclear,45 to 65 Years,Male,Car,Other,Within Speed Limit,,Changing Lanes,Pedalcyclist/NMV,No Avoidance Maneuver,33.80338056,-101.837774999999
2022_107400,2022_1074001,25 to 44 Years,Female,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekend,Rural,Dark,Clear,25 to 44 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,32.68672222,-85.32090556
2022_200580,2022_2005801,More than 65 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Fall,Weekday,Urban,Daylight,Clear,45 to 65 Years,Female,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking,61.1820916699999,-149.8602583
2022_602400,2022_6024001,45 to 65 Years,Male,Bicyclist Loss of Control,Others,With Traffic,Winter,Weekend,Urban,Daylight,Clear,45 to 65 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Involved Vehicle,Unknown,33.89490833,-118.1874806
2022_616530,2022_6165301,25 to 44 Years,Female,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,34.0481305599999,-118.2145694
2022_617360,2022_6173601,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,34.02113611,-118.2784389
2022_624450,2022_6244501,25 to 44 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Winter,Weekend,Urban,Dark,Clear,25 to 44 Years,Female,Car,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,No Avoidance Maneuver,40.58553056,-122.3651028
2022_632740,2022_6327401,25 to 44 Years,Male,Initial Crossing Paths,Travel Lane,Not applicable,Winter,Weekday,Urban,Dark,Clear,25 to 44 Years,Female,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,34.14719444,-117.3212639
2022_2002970,2022_20029701,More than 65 Years,Male,Initial Crossing Paths,Sidewalk,Not applicable,Fall,Weekday,Urban,Daylight,Clear,Less than 25 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,39.07081111,-94.6181805599999
2022_2200920,2022_22009201,25 to 44 Years,Male,Wrong-Way,Travel Lane,Facing Traffic,Winter,Weekday,Rural,Dark,Unclear,25 to 44 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,31.46349722,-92.79507221999987
2022_2203960,2022_22039601,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,Unknown,Summer,Weekday,Urban,Daylight,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,Others,Pedalcyclist/NMV,Unknown,30.441108329999896,-91.01213056
2022_4600400,2022_46004001,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Rural,Daylight,Clear,45 to 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,44.54680556,-96.75730833
2022_4709060,2022_47090601,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,36.18658056,-86.27583056
2022_4711190,2022_47111901,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekend,Urban,Dark,Clear,25 to 44 Years,Male,Pick Up,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,35.98838889,-86.5856388899999
2022_4806910,2022_48069101,45 to 65 Years,Male,Initial Crossing Paths,Bicycle Lane,Not applicable,Spring,Weekend,Urban,Daylight,Clear,45 to 65 Years,Male,Pick Up,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,29.397008329999895,-98.64935556
2022_4814070,2022_48140701,Less than 25,Male,Initial Crossing Paths,Others,Not applicable,Spring,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Pick Up,Other,Within Speed Limit,,Changing Lanes,Pedalcyclist/NMV,Unknown,32.82665278,-97.20571110999987
2022_4818900,2022_48189001,25 to 44 Years,Male,Others,Travel Lane,Unknown,Summer,Weekday,Urban,Daylight,Clear,Less than 25 Years,Male,Large Truck,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,29.78448333,-95.57937499999989
2022_4834290,2022_48342901,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Pick Up,Other,Exceed Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,31.7694777799999,-106.4769083
2022_5109500,2022_51095001,45 to 65 Years,Male,Bicyclist Failed to Yield,Travel Lane,Facing Traffic,Winter,Weekday,Urban,Daylight,Unclear,25 to 44 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,38.29684444,-77.50567499999988
2023_502180,2023_5021801,25 to 44 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Summer,Weekend,Urban,Dark,Clear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,36.04008056,-94.18953611
2023_619060,2023_6190601,25 to 44 Years,Male,Bicyclist Turned,Travel Lane,Not applicable,Fall,Weekend,Urban,Dark,Clear,45 to 65 Years,Female,Large Truck,Impaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking,33.82568889,-118.2348583
2023_623750,2023_6237501,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,37.87179167,-121.2788972
2023_626970,2023_6269701,Less than 25,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Spring,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,34.06399444,-117.270730599999
2023_634340,2023_6343401,25 to 44 Years,Female,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekend,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Unimpaired,Exceed Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,37.7321111099999,-122.4303556
2023_1310290,2023_13102901,45 to 65 Years,Male,Others,Travel Lane,Unknown,Fall,Weekend,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,32.0743277799999,-81.0984972199999
2023_1706950,2023_17069501,25 to 44 Years,Male,Others,Travel Lane,With Traffic,Summer,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Small Truck,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,42.0525666699999,-87.7616555599999
2023_1709280,2023_17092801,45 to 65 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Fall,Weekday,Urban,Daylight,Clear,More than 65 Years,Female,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,39.8170055599999,-89.64319722
2023_1804680,2023_18046801,Less than 25,Male,Others,Travel Lane,With Traffic,Summer,Weekend,Rural,Dark,Unclear,25 to 44 Years,Male,Car,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking,41.73051111,-86.34501111
2023_2202020,2023_22020201,45 to 65 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekday,Urban,Dark,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,29.99236111,-90.0493
2023_2203810,2023_22038101,25 to 44 Years,Female,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekend,Urban,Daylight,Clear,More than 65 Years,Female,Unknown,Other,Exceed Speed Limit,,Others,Pedalcyclist/NMV,Unknown,30.02061111,-90.0137166699999
2023_2803750,2023_28037501,45 to 65 Years,Male,Others,Bicycle Lane,With Traffic,Summer,Weekday,Urban,Daylight,Clear,45 to 65 Years,Female,Large Truck,Unimpaired,Within Speed Limit,,At Curve,Object,No Avoidance Maneuver,32.2767388899999,-90.17649167
2023_3402310,2023_34023101,More than 65 Years,Male,Others,Travel Lane,Unknown,Summer,Weekend,Urban,Daylight,Clear,More than 65 Years,Male,Large Truck,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,No Avoidance Maneuver,40.74575833,-74.1606666699999
2023_3404900,2023_34049001,25 to 44 Years,Male,Others,Bicycle Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Pick Up,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Braking and Steering Maneuver,40.71434722,-74.28063611
2023_3607140,2023_36071401,45 to 65 Years,Male,Bicyclist Turned,Travel Lane,With Traffic,Fall,Weekday,Urban,Daylight,Clear,More than 65 Years,Male,Small Truck,Other,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,42.6998027799999,-73.71229721999988
2023_3607220,2023_36072201,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Fall,Weekend,Urban,Dark,Unclear,25 to 44 Years,Male,Large Truck,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,43.1332833299999,-77.55301389
2023_3706410,2023_37064101,25 to 44 Years,Female,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekday,Rural,Dark,Clear,Less than 25 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Involved Vehicle,Unknown,34.620375,-79.21600277999988
2023_4000170,2023_40001701,45 to 65 Years,Male,Bicyclist Turned,Travel Lane,With Traffic,Winter,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,36.1462,-96.0035
2023_4805580,2023_48055801,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Spring,Weekday,Urban,Dark,Unclear,45 to 65 Years,Male,Pick Up,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,30.1324,-93.98439444
2023_4810040,2023_48100401,45 to 65 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Spring,Weekday,Urban,Daylight,Clear,Less than 25 Years,Male,Small Truck,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,29.69818056,-95.28968055999987
2023_4824430,2023_48244301,25 to 44 Years,Male,Motorist Overtaking,Travel Lane,With Traffic,Summer,Weekend,Rural,Dark,Clear,45 to 65 Years,Male,Small Truck,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,No Avoidance Maneuver,34.0985444399999,-98.549225
2023_4825140,2023_48251401,25 to 44 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Fall,Weekday,Urban,Dark,Clear,25 to 44 Years,Male,Small Truck,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,32.87071944,-96.89866388999987
2023_4827810,2023_48278101,45 to 65 Years,Male,Others,Bicycle Lane,With Traffic,Fall,Weekday,Urban,Dark,Clear,More than 65 Years,Male,Car,Other,Within Speed Limit,,Changing Lanes,Involved Vehicle,Unknown,29.3851,-98.47050278
2023_4830820,2023_48308201,More than 65 Years,Male,Initial Crossing Paths,Travel Lane,Not applicable,Summer,Weekday,Urban,Daylight,Clear,45 to 65 Years,Male,Car,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Others,29.520780559999896,-98.39389443999987
2023_4834870,2023_48348701,25 to 44 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Fall,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Small Truck,Other,Within Speed Limit,,Going Straight,Pedalcyclist/NMV,Unknown,29.8402638899999,-95.33357499999987
2023_5307080,2023_53070801,25 to 44 Years,Male,Bicyclist Failed to Yield,Travel Lane,Not applicable,Fall,Weekday,Urban,Dark,Clear,45 to 65 Years,Male,Pick Up,Unimpaired,Within Speed Limit,,At Curve,Pedalcyclist/NMV,Unknown,47.2897027799999,-122.3011778
2023_5600560,2023_56005601,45 to 65 Years,Male,Others,Bicycle Lane,Unknown,Summer,Weekday,Urban,Daylight,Clear,45 to 65 Years,Male,Small Truck,Unimpaired,Within Speed Limit,,At Curve,Involved Vehicle,Unknown,41.60993611,-109.222927799999
"

dat <- read.csv(
  text = EMBEDDED_CSV,
  check.names = FALSE,
  stringsAsFactors = FALSE
)

safe_chr <- function(x, fallback = "Not reported") {
  y <- as.character(x)
  y[is.na(y) | trimws(y) == ""] <- fallback
  y
}

norm_chr <- function(x) tolower(trimws(safe_chr(x, "")))
contains_ci <- function(x, pattern) grepl(pattern, norm_chr(x), perl = TRUE)

dat[["Driver Related Factor"]] <- safe_chr(dat[["Driver Related Factor"]], "None")
dat$Crash_ID <- safe_chr(dat$CRASH_VEH_NUM1)
dat$Record_ID <- safe_chr(dat$CRASH_VehPer_NUM1)
dat$Year <- substr(dat$Crash_ID, 1, 4)
dat$Latitude <- suppressWarnings(as.numeric(dat$LATITUDENAME))
dat$Longitude <- suppressWarnings(as.numeric(dat$LONGITUDNAME))
dat$Mappable <- is.finite(dat$Latitude) & is.finite(dat$Longitude) &
  dat$Latitude >= 18 & dat$Latitude <= 72 &
  dat$Longitude >= -180 & dat$Longitude <= -60
dat$.row_index <- seq_len(nrow(dat))

# ============================================================
# Validated roadway/site context
# ============================================================
# A site-context file is bundled with one row per analytical record. Populate it
# after reviewing Street View/site imagery. Only rows with review_status =
# "validated" are automatically applied to the mechanism diagram.
SITE_CONTEXT_FILE <- file.path("data", "site_context_validated.csv")
if (file.exists(SITE_CONTEXT_FILE)) {
  SITE_CONTEXT_VALIDATED <- tryCatch(
    read.csv(SITE_CONTEXT_FILE, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) data.frame()
  )
} else {
  SITE_CONTEXT_VALIDATED <- data.frame()
}

ROAD_GEOMETRY_CHOICES <- c(
  "Not yet confirmed" = "unknown",
  "2-lane undivided" = "2u",
  "2-lane divided" = "2d",
  "3-lane undivided" = "3u",
  "3-lane divided" = "3d",
  "4-lane undivided" = "4u",
  "4-lane divided" = "4d",
  "5-lane undivided" = "5u",
  "5-lane divided" = "5d",
  "6-lane undivided" = "6u",
  "6-lane divided" = "6d",
  "8-lane divided" = "8d",
  "1-lane one-way ramp / connector" = "ramp1",
  "2-lane one-way ramp / connector" = "ramp2",
  "3-lane one-way ramp / connector" = "ramp3",
  "Merge / weaving / ramp-terminal context" = "merge",
  "Other / geometry uncertain" = "other"
)

validated_site_row <- function(record_id) {
  if (!nrow(SITE_CONTEXT_VALIDATED) || !("Record_ID" %in% names(SITE_CONTEXT_VALIDATED))) return(NULL)
  hit <- which(as.character(SITE_CONTEXT_VALIDATED$Record_ID) == as.character(record_id))
  if (!length(hit)) return(NULL)
  SITE_CONTEXT_VALIDATED[hit[1], , drop = FALSE]
}

geometry_context_from_code <- function(code, mapped_ctx = list(), source_label = NULL) {
  code <- as.character(code %||% "unknown")
  base <- list(
    geometry_confirmed = FALSE,
    geometry_code = code,
    divided = NA,
    total_lanes = NA_integer_,
    roadway_form = "Roadway geometry not yet confirmed",
    lane_summary = "Review Street View or validate site_context_validated.csv",
    source = source_label %||% "Site review pending",
    name = mapped_ctx$name %||% NA_character_,
    ref = mapped_ctx$ref %||% NA_character_,
    highway = mapped_ctx$highway %||% NA_character_,
    maxspeed = mapped_ctx$maxspeed %||% NA_character_
  )
  if (code %in% c("2u","2d","3u","3d","4u","4d","5u","5d","6u","6d","8d")) {
    lane_count <- suppressWarnings(as.integer(substr(code, 1, 1)))
    divided <- substr(code, 2, 2) == "d"
    lane_word <- c(`2`="Two", `3`="Three", `4`="Four", `5`="Five", `6`="Six", `8`="Eight")[[as.character(lane_count)]] %||% as.character(lane_count)
    div_word <- if (divided) "divided" else "undivided"
    base$geometry_confirmed <- TRUE
    base$divided <- divided
    base$total_lanes <- lane_count
    base$roadway_form <- paste0(lane_word, "-lane ", div_word, " roadway")
    base$lane_summary <- paste0(lane_count, " lanes, ", div_word)
  }
  if (code == "ramp1") { base$geometry_confirmed <- TRUE; base$divided <- FALSE; base$total_lanes <- 1L; base$roadway_form <- "One-way ramp / connector"; base$lane_summary <- "1-lane one-way ramp/connector" }
  if (code == "ramp2") { base$geometry_confirmed <- TRUE; base$divided <- FALSE; base$total_lanes <- 2L; base$roadway_form <- "Two-lane one-way ramp / connector"; base$lane_summary <- "2-lane one-way ramp/connector" }
  if (code == "ramp3") { base$geometry_confirmed <- TRUE; base$divided <- FALSE; base$total_lanes <- 3L; base$roadway_form <- "Three-lane one-way ramp / connector"; base$lane_summary <- "3-lane one-way ramp/connector" }
  if (code == "merge") { base$geometry_confirmed <- TRUE; base$divided <- NA; base$total_lanes <- NA_integer_; base$roadway_form <- "Merge / weaving / ramp-terminal context"; base$lane_summary <- "Complex ramp / weaving geometry" }
  if (code == "other") { base$geometry_confirmed <- FALSE; base$roadway_form <- "Roadway geometry marked uncertain"; base$lane_summary <- "Geometry remains uncertain"; base$source <- source_label %||% "User/site review - uncertain" }
  base
}

n_total <- nrow(dat)
n_map <- sum(dat$Mappable)
n_dark <- sum(norm_chr(dat[["Lighting Condition"]]) == "dark")
n_day <- sum(norm_chr(dat[["Lighting Condition"]]) == "daylight")

STUDY_VARS <- c(
  "PedAGE", "PedGender", "Bike Crash Type", "Bike Location",
  "Bicyclist Direction", "Season", "DOW", "Land Type", "Weather",
  "MV DR Age", "MV DR Gender", "MV Type", "Driver Impairment",
  "Speeding Related", "Driver Related Factor", "Prior Critical Event",
  "Critical Event Occurred by", "Attempted for Avoidance"
)

# ============================================================
# Manuscript elbow figures only
# ============================================================

ELBOW_DARK_URI <- paste0("data:image/png;base64,", "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EALHjh3D6tWrERkZKXU91Een00nLohFDBD/55BPcf//9AKqPYk+YMAHnz5+vc32LxeLwuYiISD4svImIqMXr1asXdu/eLR25y8nJweTJk2ut5+bm5vDxFRUVLo3PVllZmcPnve222wDYF27r1q1DYWEhCgsLUVZWhvLycpSXl9eaTg2gwWFqtrp06SItl5eXO4zHdp1b9eijj0rLFy5cQFFRkcP1UlJS8Oabbza4vVt938aMGYOsrCykp6dj3rx5CAkJAVD9Wp9//vkGn8/Pz89uh8rx48dRWFiIoqIi6T2wHUBny9H74OPjg/379+PUqVNYtWoVHnjgAWm9rVu32h1dd8R2AJ21/b0+er0e//jHP6Sj77/88gvGjx+P3Nxch+vfvHlTWrbt4CAiIvmw8CYiolZh2LBh+Pjjj6WCyXbytJVtUWFbbFy+fNn1ATp4ruzsbGnZ2h48btw46bZz587ZPXb79u2YMGFCsy+dZntesW08tsuNOfe4LuPGjUNUVBSA6iP/qamptdbJysrCnDlz6ixgbd3q+zZw4EDk5eVhwoQJeOONN3D69Gn07dsXAHD16lVpPdsj/BaLBRaLBRs2bIBGo0FERIR0X833YcGCBY2aRm716quvYu3atejXrx+SkpKwf/9+LFmyRLrfNiZHRo4cKS07Os3AEW9vb+zbtw933HEHAODixYt48MEHUVhYWGtd6za7d++OHj16NGr7RETkXCy8iYio1Zg4cSJWrVpV5/1BQUEYMGAAAODYsWMoLi7Gf//7X3z22WdyhYi0tDRcuXIFv/zyC9LS0gBUH9GcM2cOgOqhXtZW7dTUVOn83JMnTyIpKQnjxo27paPbjjzxxBNSYb17926cPXsWpaWleP/99wFU58m2MGyKtLQ0jB8/HgAwf/58fPjhhygvL5darR9++GEMGjQIL7zwQoPbutX37fLly0hISMAvv/wCADh//rzUyh8dHS2tFxoaKi1funQJR48exbx58+Du7o41a9ZIbfjLli1DXl4ehBDYuXMn3nnnnVvaMXH9+nUsW7YMhw4dghACRqMRJ06cAFBdINsO63Nk7Nix0lHvW5lf0LFjR3z++efo3bs3AODHH39ETEyMXddFTk6O1DY/ZcqURm+biIicTMmR6kRERLa++eYb0aFDB6HT6YTBYKjzUkwzZ86ULq1U08mTJ8W9994rPD09hb+/v5g+fbpITEyU1jcYDGLmzJni0qVLokOHDkKj0dhdKuqbb74RXl5e0m3u7u5i5syZYubMmcLd3V263cvLS6xYsUJER0cLvV4v3Z6YmChGjRolDAaD8PHxEY8//ri4cOGCXYx5eXkiISFB9O7dW7i7uwt/f38xcuRIsXHjRmmdzZs3i/bt20vb1el0UnyNYTKZxNq1a8XQoUOFl5eX8PT0FH379hWJiYmioKBAWi86OrrW62rMJbCEEMJisYgdO3aIiRMniu7duwuDwSD8/f3FoEGDxMqVK0VxcbHde2YwGKTn0ev1Ijo6+pbfNyGEeO6558TIkSNFly5dhI+Pj/Dw8BD9+vUTSUlJoqSkRNrm5cuXxZgxY6TXHxYWJrZs2SLdn5mZKWJjY0WXLl2Eu7u76Nmzp4iJibHLsaO4a+Znx44dIioqSvTq1Uv4+fmJdu3aCX9/fzFx4kRx7NixRuVy06ZNAoDw8PCwu5TZzJkz7T4H1s+pNRdCCHHu3Dnh7+9v9x4uXbpUCCHEmjVrBADRtWtXcf369UbFQkREzqcRopn9bERERETUbImJiXjzzTcRFxeHv/71r83e3tWrVzFgwABUVVXhs88+s2tpJyIiebHVnIiIiKgFeOONN5CSkoLdu3fXe7m4xsjPz0dERAT69OmDw4cPs+gmIlIYj3gTERERtSAFBQVIT09HXFxck7eRnZ2No0eP4rHHHrOb4E5ERMpg4U1ERERERETkQtwFSkRERERERORCLLyJiIiIiIiIXIiFNxEREREREZELsfAmIiIiIiIiciEW3kREREREREQuxMKbiIiIiIiIyIVYeBMRERERERG5EAtvIiIiIiIiIhdi4U1ERERERETkQv8DJRL9WCnnfgEAAAAASUVORK5CYII=")
ELBOW_DAY_URI  <- paste0("data:image/png;base64,", "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tOzYe8Bx2bF7P1XPiRMnsG7dOkyaNMlpoiSqnsjISKcuzo4X8B0v1vFCXfXs3r0bW7ZscVonhJDu3z1o0CCn3jJUfYMHD5a+2xcuXJDWOy4//fTTUCgUPo+tMlh4E/nB6NGj8dtvv+HIkSO477770Lp1a6cHecfkyZNx/PhxAEWzXY4ZMwaZmZkAiq6esls/BYqPPvoIe/fuhRACp0+fxvTp0wEAOp0OixYt8nN0ge/pp5/Ggw8+CKCoh0FOTg7S0tKwZ88eAMCf/vQnDB482J8hBpW5c+eiVq1abO32Mp1Oh6lTp0rPFy9eDJvNhosXL2LTpk0AimaEfuGFF/wVYlDYsWMHhg0bhr179wIouivN+PHjkZ6ejjZt2rArvwzq16+PhQsXQqFQ4NixY9ixYwfMZjM++OADAEWTCs6YMcPPUVZMIUruAE/k4OGHH8bevXtRWFgoTcKh0WgQFhaGhIQEtmBVU2JiolPLqysZGRk1vstMTfbhhx/i22+/xZEjR2AwGJCVlYVatWqhTZs2SElJwbPPPsvJT7zo999/R4cOHWA2m6WJqlQqFcLDwzF16lSnPwapclJTU7FixQp8//33uHnzJrKyslC3bl307t0b06dP5+0HvcRsNmPhwoVYuXIlzp49C4vFgsTERAwaNAjTpk2DXq/3d4hB4dSpU2jTpg1effVVzJw509/hBKV169bho48+wqFDh5Cfnw8hBOLj49GnTx9MmTIFzZo183eIAe2bb77BggULcPz4ceTm5sJms6F58+YYOHAgJk6cyDkLKqGy9ca2bdvwxhtv4IcffoDFYkFsbCwGDhyIadOmOc3VUVOx8CYiIiIiIiKSEbuaExEREREREcmIhTcRERERERGRjFh4ExEREREREcmIhTcRERERERGRjFh4ExEREREREcmIhTcRERERERGRjFh4ExEREREREcmIhTcRERERERGRjFh4ExEREREREcmIhTcRERERERGRjFh4ExEREREREcmIhTcRERERERGRjFh4ExEREREREcmIhTcRERERERGRjFh4ExEREREREcmIhTcRERERERGRjFh4ExEREd1GLBYL8vPzvXa+rKwsr52LiChYsfAmIiLZnT9/HgqFwunxwAMPwGq1utw/MTGxzP7nz5/3bdAVuPfee2t8jN5w9uxZDB8+HPHx8dBoNNDr9UhISEBKSorH5/jmm28wYsQItGrVCpGRkdBoNIiJiUH37t3x3HPP4csvv4TRaATg+rsycuRImd7d7cViseDtt99G48aN8csvvwBw/X+t5DFr1izpWFf7LVu2DADQoUMHjBkzBteuXfP9myIiChAsvImISHaNGzeG0WjEhx9+KK1LTU3FuHHjXO5/6tQppKWlAQC2bt0Ko9GIxo0b+yRWT3377bfYunWrv8OQVU5ODu6++258+umnuHjxIt544w1cvHgRw4cPx86dOys8/sKFC+jVqxceeughLF++HB07dsT27duRmZmJffv2YeDAgfjkk0+QkpKCXr16Afjju9KzZ0+53165du3aFVQXVc6cOYPOnTtj8uTJeOGFF3DPPfcAKPq/VnLRo8Rf/vIXGI1GvPLKK9K6Xbt2oX379mjdujXS09NhNBoxbNgwAMCqVavwxRdfoGXLltiwYYPv3hQRUQBh4U1ERLJTKBQICQmBRqNxWr948WK8/vrrZfbX6XTQ6XQAAK1Wi5CQECgUCp/E6imtVgutVuvvMGSVmpqK69evS8/HjBmD6OhoTJ8+Hd988025x167dg29e/fGnj17AABPPPEEVq9ejW7duiEqKgotW7bE3//+d6xbtw5KpRIWiwXAH98VpZJ/onjLmTNncPfdd+PXX3/F6NGj8dJLL0nbdDodQkJCnPZXqVQICQmBWq0GAPz666/o3bs3oqOjsW/fPjRv3hwhISFQqVQAgJ49e+KDDz6AwWDA448/js8//9x3b46IKEDwtxoREflcyR/sADB9+nSsXbvWj9GQO3l5eU7Pw8LCpH+7d+9e7rETJ05ERkaG9Py1115zuV9ycjKSk5OrGSm5Y7PZMGjQIGRmZkKr1br9HNzZtm0bevTogbvvvhvffvstoqOjXe43aNAgdO3aFUIIjBw5MuB7CBAReRsLbyIi8rkFCxZIy0IIDB8+HN9//32Fxz388MNux1WX3paYmCgdt2zZsjLHvfXWWxg3bhzi4+Oh1WoRGxuLF154AUajEfn5+Zg0aRLi4uKg1+vRsWNHfPLJJxXGt3PnTvTo0QN6vR56vR533nknPv30U5f7btq0Cf3790f9+vWh0WhQp04dJCcnY926dU77uRpbm5GRgSeffBIxMTFQKpUe9wbIyMjA888/j9atW0Ov1yMkJARNmzbFyJEjpTG/jq/79NNPO61z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")

# ============================================================
# Study cluster metadata from the manuscript
# ============================================================

study_cluster_meta <- data.frame(
  Lighting = rep(c("Dark", "Daylight"), each = 4),
  Cluster = rep(paste0("C", 1:4), 2),
  Manuscript_N = c(46,20,14,7, 16,13,8,6),
  Manuscript_Share = c(52.9,23.0,16.1,8.0, 37.2,30.2,18.6,14.0),
  Title = c(
    "Yielding and Braking-Related Crashes",
    "Lane-Change and Mixed-Traffic Contexts",
    "Speed and Demographic Related Crashes",
    "Unclear Maneuver and Crossing-Path Crashes",
    "Turning and Lane-Change Related Crashes",
    "Yield and Crossing Crashes",
    "Maneuver and Control-Related Crashes",
    "Speed-Related Crashes"
  ),
  stringsAsFactors = FALSE
)

cluster_title <- function(light, cluster) {
  x <- study_cluster_meta[
    study_cluster_meta$Lighting == light &
      study_cluster_meta$Cluster == cluster, "Title"
  ]
  if (length(x) == 0) cluster else x[1]
}

cluster_colors <- c(
  C1 = "#FEC44F",
  C2 = "#FE9929",
  C3 = "#D95F0E",
  C4 = "#7F2704",
  C5 = "#6A51A3",
  C6 = "#3182BD",
  C7 = "#31A354",
  C8 = "#756BB1"
)

lighting_colors <- c(Dark = "#203864", Daylight = "#D9A51B")
brand_burgundy <- "#721C3B"

# ============================================================
# CCA helpers
# ============================================================

prepare_factor_data <- function(df, vars) {
  vars <- intersect(vars, names(df))
  out <- df[, vars, drop = FALSE]
  out[] <- lapply(out, function(x) factor(safe_chr(x)))
  keep <- vapply(out, function(x) nlevels(x) >= 2, logical(1))
  out <- out[, keep, drop = FALSE]
  out
}

all_permutations4 <- function() {
  p <- as.matrix(expand.grid(a = 1:4, b = 1:4, c = 1:4, d = 1:4))
  p[apply(p, 1, function(z) length(unique(z)) == 4), , drop = FALSE]
}

map_study_cluster_labels <- function(raw_cluster, light) {
  expected <- if (light == "Dark") c(46,20,14,7) else c(16,13,8,6)
  raw_sizes <- as.numeric(table(factor(raw_cluster, levels = 1:4)))
  perms <- all_permutations4()
  costs <- apply(
    perms, 1,
    function(p) sum(abs(raw_sizes[p] - expected))
  )
  best <- perms[which.min(costs), ]
  mapping <- integer(4)
  for (manuscript_cluster in 1:4) {
    mapping[best[manuscript_cluster]] <- manuscript_cluster
  }
  mapping[raw_cluster]
}

make_dummy_matrix <- function(factor_df) {
  mats <- lapply(names(factor_df), function(v) {
    x <- factor_df[[v]]
    levs <- levels(x)
    m <- sapply(levs, function(lv) as.numeric(x == lv))
    if (is.null(dim(m))) m <- matrix(m, ncol = 1)
    colnames(m) <- paste0(v, " = ", levs)
    m
  })
  out <- do.call(cbind, mats)
  storage.mode(out) <- "numeric"
  out
}

cca_residuals <- function(factor_df, clusters) {
  dummy <- make_dummy_matrix(factor_df)
  k <- length(unique(clusters))
  cl <- as.integer(factor(clusters, levels = sort(unique(clusters))))
  C <- matrix(0, nrow(dummy), k)
  for (j in seq_len(k)) C[cl == j, j] <- 1

  P <- t(dummy) %*% C
  P <- P / sum(P)
  c_marg <- colSums(P)
  r_marg <- rowSums(P)

  keep <- r_marg > 0
  P <- P[keep, , drop = FALSE]
  r_marg <- r_marg[keep]
  labels <- rownames(P)

  inv_r <- diag(1 / sqrt(r_marg), nrow = length(r_marg))
  inv_c <- diag(1 / sqrt(c_marg), nrow = length(c_marg))
  expected <- r_marg %*% t(c_marg)
  dev <- inv_r %*% (P - expected) %*% inv_c
  val <- dev * sqrt(nrow(dummy))

  out <- do.call(rbind, lapply(seq_len(k), function(j) {
    data.frame(
      Cluster = paste0("C", j),
      Category = labels,
      Residual = as.numeric(val[, j]),
      stringsAsFactors = FALSE
    )
  }))
  out
}

run_cca <- function(df, vars, k = 4, study_light = NULL) {
  factor_df <- prepare_factor_data(df, vars)

  if (nrow(factor_df) < max(12, k * 2)) {
    stop("Too few records remain for the requested CCA.")
  }
  if (ncol(factor_df) < 2) {
    stop("At least two non-constant CCA variables are required.")
  }

  set.seed(1234)
  res <- clustrd::clusmca(
    factor_df,
    k,
    2,
    method = "clusCA",
    nstart = 10
  )

  raw_cluster <- as.integer(res$cluster)

  if (!is.null(study_light) && k == 4) {
    mapped_cluster <- map_study_cluster_labels(raw_cluster, study_light)
  } else {
    mapped_cluster <- raw_cluster
  }

  cluster_label <- paste0("C", mapped_cluster)

  coords <- data.frame(
    row_index = df$.row_index,
    Dim1 = as.numeric(res$obscoord[, 1]),
    Dim2 = as.numeric(res$obscoord[, 2]),
    Cluster = cluster_label,
    stringsAsFactors = FALSE
  )

  counts <- table(factor(cluster_label, levels = paste0("C", seq_len(k))))
  shares <- round(100 * counts / sum(counts), 1)

  # Category coordinate labels from factor levels
  att_labels <- unlist(lapply(names(factor_df), function(v) {
    paste0(v, " = ", levels(factor_df[[v]]))
  }), use.names = FALSE)

  att_coords <- NULL
  if (!is.null(res$attcoord) && nrow(res$attcoord) == length(att_labels)) {
    att_coords <- data.frame(
      Dim1 = as.numeric(res$attcoord[, 1]),
      Dim2 = as.numeric(res$attcoord[, 2]),
      Category = att_labels,
      stringsAsFactors = FALSE
    )
    att_coords$Distance <- sqrt(att_coords$Dim1^2 + att_coords$Dim2^2)
  }

  residual_df <- cca_residuals(factor_df, mapped_cluster)

  list(
    data = df,
    factor_data = factor_df,
    variables = names(factor_df),
    k = k,
    raw_result = res,
    clusters = cluster_label,
    mapped_numeric = mapped_cluster,
    coords = coords,
    att_coords = att_coords,
    counts = as.numeric(counts),
    shares = as.numeric(shares),
    residuals = residual_df
  )
}

plot_cca_map <- function(obj, show_categories = TRUE, selected_row_index = NULL) {
  k <- obj$k
  d <- obj$coords
  palette <- cluster_colors[paste0("C", seq_len(k))]
  names(palette) <- paste0("C", seq_len(k))

  cent <- aggregate(cbind(Dim1, Dim2) ~ Cluster, data = d, FUN = mean)

  p <- ggplot(d, aes(Dim1, Dim2, color = Cluster)) +
    geom_hline(yintercept = 0, color = "#D0D5DD", linewidth = 0.35) +
    geom_vline(xintercept = 0, color = "#D0D5DD", linewidth = 0.35) +
    geom_point(size = 2.5, alpha = 0.75) +
    geom_point(
      data = cent,
      aes(Dim1, Dim2, color = Cluster),
      shape = 4, stroke = 2.2, size = 7, inherit.aes = FALSE
    ) +
    geom_text(
      data = cent,
      aes(Dim1, Dim2, label = Cluster),
      color = "#172033", fontface = "bold",
      vjust = -1.1, inherit.aes = FALSE
    ) +
    scale_color_manual(values = palette, drop = FALSE) +
    labs(x = "CCA Dimension 1", y = "CCA Dimension 2", color = "Cluster") +
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank(),
      plot.margin = ggplot2::margin(8, 12, 8, 8)
    )

  if (isTRUE(show_categories) && !is.null(obj$att_coords)) {
    att <- obj$att_coords
    nshow <- min(18, nrow(att))
    att <- att[order(att$Distance, decreasing = TRUE), , drop = FALSE]
    att <- head(att, nshow)

    p <- p +
      geom_point(
        data = att, aes(Dim1, Dim2),
        inherit.aes = FALSE,
        shape = 3, size = 2.5, color = "#667085"
      ) +
      ggrepel::geom_text_repel(
        data = att,
        aes(Dim1, Dim2, label = Category),
        inherit.aes = FALSE,
        size = 3.8,
        fontface = "bold",
        color = "#475467",
        max.overlaps = 30,
        box.padding = 0.25,
        point.padding = 0.1,
        segment.size = 0.2
      )
  }

  if (!is.null(selected_row_index)) {
    sel <- d[d$row_index == selected_row_index, , drop = FALSE]
    if (nrow(sel) == 1) {
      p <- p +
        geom_point(
          data = sel,
          aes(Dim1, Dim2),
          inherit.aes = FALSE,
          shape = 21,
          size = 7,
          fill = "#FFFFFF",
          color = "#000000",
          stroke = 1.5
        ) +
        geom_point(
          data = sel,
          aes(Dim1, Dim2),
          inherit.aes = FALSE,
          shape = 21,
          size = 4,
          fill = "#E53935",
          color = "#E53935"
        )
    }
  }

  p
}

plot_cluster_residual <- function(obj, cluster, top_n = 18) {
  d <- obj$residuals[obj$residuals$Cluster == cluster, , drop = FALSE]
  d <- d[order(abs(d$Residual), decreasing = TRUE), , drop = FALSE]
  d <- head(d, min(top_n, nrow(d)))
  d$Category <- factor(d$Category, levels = rev(d$Category))
  d$Direction <- ifelse(d$Residual >= 0, "Over-represented", "Under-represented")

  ggplot(d, aes(Residual, Category, fill = Direction)) +
    geom_col(width = 0.72) +
    geom_vline(xintercept = 0, color = "#475467", linewidth = 0.4) +
    scale_fill_manual(
      values = c(
        "Over-represented" = brand_burgundy,
        "Under-represented" = "#B8C0CC"
      )
    ) +
    labs(
      x = "Standardized residual",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.text = element_text(size=12, face="bold"),
      axis.text.x = element_text(size=12, face="bold"),
      axis.title.x = element_text(size=13, face="bold"),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 11.5, face="bold"),
      plot.margin = ggplot2::margin(5, 10, 5, 5)
    )
}

# ============================================================
# RF + SHAP helpers based on the supplied notebook workflow
# ============================================================

fit_cluster_explainer <- function(obj) {
  X <- obj$factor_data
  y <- factor(obj$clusters, levels = paste0("C", seq_len(obj$k)))

  set.seed(42)
  rf <- randomForest::randomForest(
    x = X,
    y = y,
    ntree = 300,
    importance = FALSE,
    keep.forest = TRUE,
    norm.votes = TRUE
  )

  list(
    model = rf,
    X = X,
    y = y,
    row_index = obj$data$.row_index
  )
}

predict_cluster_probability <- function(model_obj, newdata, target_cluster) {
  pr <- predict(model_obj$model, newdata = newdata, type = "prob")
  if (is.null(dim(pr)) || !(target_cluster %in% colnames(pr))) {
    return(rep(0, nrow(newdata)))
  }
  as.numeric(pr[, target_cluster])
}

# Monte-Carlo Shapley approximation implemented internally so the app has
# no dependency on external SHAP packages. The reference/background observation is sampled
# from the empirical cohort and features are switched in following random
# permutations. Marginal probability changes are averaged across permutations.
mc_shap_one <- function(model_obj, pos, target_cluster, nsim = 20) {
  X <- model_obj$X
  p <- ncol(X)
  if (is.na(pos) || pos < 1 || pos > nrow(X) || p == 0) return(rep(NA_real_, p))

  x <- X[pos, , drop = FALSE]
  phi <- numeric(p)

  for (s in seq_len(max(1, nsim))) {
    bg_pos <- sample.int(nrow(X), 1)
    z <- X[bg_pos, , drop = FALSE]
    ord <- sample.int(p, p, replace = FALSE)
    prev <- predict_cluster_probability(model_obj, z, target_cluster)[1]

    for (j in ord) {
      z[[j]] <- x[[j]]
      now <- predict_cluster_probability(model_obj, z, target_cluster)[1]
      phi[j] <- phi[j] + (now - prev)
      prev <- now
    }
  }

  phi / max(1, nsim)
}

global_shap_df <- function(model_obj, target_cluster, nsim = 8, top_n = 10, max_rows = 40) {
  X <- model_obj$X
  if (nrow(X) == 0 || ncol(X) == 0) return(data.frame())

  set.seed(42)
  rows <- seq_len(nrow(X))
  if (length(rows) > max_rows) rows <- sample(rows, max_rows)

  p <- ncol(X)
  sh <- matrix(NA_real_, nrow = length(rows), ncol = p)
  colnames(sh) <- names(X)
  for (ii in seq_along(rows)) {
    sh[ii, ] <- mc_shap_one(model_obj, rows[ii], target_cluster, nsim = nsim)
  }

  mean_abs <- colMeans(abs(sh), na.rm = TRUE)
  top <- names(sort(mean_abs, decreasing = TRUE))
  top <- head(top, min(top_n, length(top)))
  if (length(top) == 0) return(data.frame())

  long <- do.call(rbind, lapply(top, function(v) {
    xv <- X[rows, v]
    value_code <- if (is.factor(xv)) as.numeric(xv) else suppressWarnings(as.numeric(xv))
    if (all(is.na(value_code))) value_code <- rep(0, length(xv))
    rng <- range(value_code, na.rm = TRUE)
    if (!all(is.finite(rng)) || diff(rng) == 0) {
      value_scaled <- rep(0.5, length(value_code))
    } else {
      value_scaled <- (value_code - rng[1]) / diff(rng)
    }
    data.frame(
      Feature = v,
      SHAP = sh[, v],
      FeatureValue = value_scaled,
      MeanAbs = mean_abs[v],
      stringsAsFactors = FALSE
    )
  }))

  long$Feature <- factor(long$Feature, levels = rev(top))
  long
}

local_shap_df <- function(model_obj, row_index, target_cluster, nsim = 30, top_n = 10) {
  pos <- match(row_index, model_obj$row_index)
  if (is.na(pos)) return(NULL)

  set.seed(42)
  vals <- mc_shap_one(model_obj, pos, target_cluster, nsim = nsim)
  names(vals) <- names(model_obj$X)
  ord <- order(abs(vals), decreasing = TRUE)
  ord <- head(ord, min(top_n, length(ord)))

  features <- names(vals)[ord]
  actual <- vapply(features, function(v) as.character(model_obj$X[pos, v]), character(1))

  data.frame(
    Feature = features,
    Value = actual,
    SHAP = vals[ord],
    Label = paste0(features, " = ", actual),
    stringsAsFactors = FALSE
  )
}

cluster_oob_probability <- function(model_obj, row_index, target_cluster) {
  pos <- match(row_index, model_obj$row_index)
  if (is.na(pos)) return(NA_real_)
  votes <- model_obj$model$votes
  if (is.null(votes) || !(target_cluster %in% colnames(votes))) return(NA_real_)
  as.numeric(votes[pos, target_cluster])
}

plot_global_shap <- function(d) {
  ggplot(d, aes(SHAP, Feature, color = FeatureValue)) +
    geom_vline(xintercept = 0, color = "#98A2B3", linewidth = 0.45) +
    geom_point(
      position = position_jitter(height = 0.14, width = 0),
      size = 1.7, alpha = 0.78
    ) +
    scale_color_gradient(
      low = "#2C7BB6", high = "#D7191C",
      limits = c(0, 1), breaks = c(0, 1), labels = c("Low", "High")
    ) +
    labs(
      x = "SHAP value (impact on cluster classification)",
      y = NULL,
      color = "Feature value"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(face = "bold"),
      legend.position = "right"
    )
}

plot_local_shap <- function(d) {
  if (is.null(d) || nrow(d) == 0) return(ggplot() + theme_void())
  d$Label <- factor(d$Label, levels = rev(d$Label))
  d$Direction <- ifelse(d$SHAP >= 0, "Toward selected cluster", "Away from selected cluster")

  ggplot(d, aes(SHAP, Label, fill = Direction)) +
    geom_col(width = 0.72) +
    geom_vline(xintercept = 0, color = "#475467", linewidth = 0.4) +
    scale_fill_manual(
      values = c(
        "Toward selected cluster" = "#D95F0E",
        "Away from selected cluster" = "#8FA7C1"
      )
    ) +
    labs(x = "Local SHAP contribution", y = NULL, fill = NULL) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      legend.text = element_text(size=12, face="bold"),
      axis.text.x = element_text(size=12, face="bold"),
      axis.title.x = element_text(size=13, face="bold"),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 11.5, face="bold")
    )
}

# ============================================================
# Precompute Study CCA: separate Dark and Daylight
# ============================================================

dark_df <- dat[norm_chr(dat[["Lighting Condition"]]) == "dark", , drop = FALSE]
day_df  <- dat[norm_chr(dat[["Lighting Condition"]]) == "daylight", , drop = FALSE]

STUDY_CCA <- list(
  Dark = run_cca(dark_df, STUDY_VARS, 4, study_light = "Dark"),
  Daylight = run_cca(day_df, STUDY_VARS, 4, study_light = "Daylight")
)

# ============================================================
# Countermeasure library
# ============================================================

countermeasure_library <- list(
  P1 = list(
    name = "Bicyclist education and route awareness",
    short = "Route awareness",
    target = "Unintended interstate entry and prolonged high-speed exposure",
    interruption = "Acts before exposure by improving route decisions and discouraging unintended entry onto controlled-access facilities.",
    actions = c(
      "Public outreach on restricted facilities and safer route planning",
      "Emphasize ramp and interchange avoidance where bicycle access is restricted",
      "Integrate route-awareness messages into local bicycle programs and enforcement communications"
    ),
    pathway = c("Earlier route decision", "Less interstate bicycle exposure", "Fewer high-speed bicycle–motor-vehicle encounters"),
    reference = "Brookshire et al. (2016)",
    full_reference = "Brookshire, K., Sandt, L., Sundstrom, C., & Blomberg, R. (2016). Advancing Pedestrian and Bicyclist Safety: A Primer for Highway Safety Professionals (DOT HS 812 258). National Highway Traffic Safety Administration, Washington, DC."
  ),
  P2 = list(
    name = "Driver expectancy and response training",
    short = "Driver expectancy",
    target = "Late detection, unsafe passing, turning, and limited evasive response",
    interruption = "Acts during rare bicyclist encounters by supporting earlier recognition, speed reduction, lane-position response, and safe passing behavior.",
    actions = c(
      "Emphasize bicyclist scanning near ramps, shoulders, and interchange areas",
      "Reinforce safe passing, lane-change, and speed-reduction responses",
      "Include rare freeway bicyclist encounters in driver education and fleet training"
    ),
    pathway = c("Earlier bicyclist detection", "More time and lateral clearance", "Lower likelihood of a severe conflict"),
    reference = "NHTSA (2013)",
    full_reference = "National Highway Traffic Safety Administration (2013). Rules of the Road for Riding Safely. Washington, DC."
  ),
  P3 = list(
    name = "Navigation and information systems",
    short = "Navigation / wayfinding",
    target = "Navigation-driven freeway entry and unclear bicycle routing near ramps",
    interruption = "Acts upstream of the conflict by redirecting bicyclists before freeway entry and clarifying where bicycle travel is permitted.",
    actions = c(
      "Coordinate bicycle routing with digital navigation providers",
      "Improve wayfinding near ramp areas",
      "Deploy route-signing practices that clarify bicycle routing and prevent freeway entry where restricted"
    ),
    pathway = c("Correct route before ramp", "Avoid controlled-access entry", "Reduce exposure to interstate-speed traffic"),
    reference = "Petritsch and Fellerhoff (2014)",
    full_reference = "Petritsch, T., & Fellerhoff, C. (2014). U.S. Bicycle Route Signing (NCHRP 20-07). Transportation Research Board, Washington, DC."
  ),
  P4 = list(
    name = "Targeted enforcement and high-visibility operations",
    short = "Enforcement / visibility",
    target = "Speeding, unsafe passing, and non-compliant behavior in high-severity contexts",
    interruption = "Acts on risky operating behavior through visible enforcement and coordinated safety messaging at targeted locations.",
    actions = c(
      "Conduct periodic enforcement near relevant interchange areas",
      "Focus on speeding and unsafe passing",
      "Coordinate enforcement activity with education and warning messages"
    ),
    pathway = c("Higher perceived enforcement", "Lower unsafe speed / maneuver behavior", "Lower conflict severity"),
    reference = "NHTSA (2023)",
    full_reference = "National Highway Traffic Safety Administration (2023). Traffic Safety Fact Report: 2023 Data - Speeding (DOT HS 813 721). U.S. Department of Transportation, Washington, DC."
  ),
  P5 = list(
    name = "Shoulder rumble strips",
    short = "Shoulder rumble strips",
    target = "Run-off-road encroachment onto paved shoulders where bicyclists may be present on rural high-speed segments",
    interruption = "Provides tactile and audible warning before a vehicle departs the travel lane toward the shoulder, while preserving usable shoulder space for bicyclists.",
    actions = c(
      "Install milled or rolled shoulder rumble strips on rural interstate segments with documented bicyclist presence",
      "Maintain at least 4 ft of clear usable shoulder width where applicable",
      "Prioritize segments with overtaking or run-off-road crash history"
    ),
    pathway = c("Earlier lane-departure warning", "Reduced shoulder encroachment", "Lower run-off-road exposure near bicyclists"),
    reference = "Torbic et al. (2009)",
    full_reference = "Torbic, D.J., Hutton, J.M., Bokenkroger, C.D., Bauer, K.M., Harwood, D.W., Gilmore, D.K., Dunn, J.M., Ronchetto, J.J., Donnell, E.T., Sommer III, H.J., Garvey, P., Persaud, B., & Lyon, C. (2009). Guidance for the Design and Application of Shoulder and Centerline Rumble Strips. Transportation Research Board, Washington, DC. Reported CMF = 0.77 (CRF = 23%)."
  ),
  P6 = list(
    name = "Enhanced wrong-way signing at interchange ramp terminals",
    short = "Enhanced wrong-way signing",
    target = "Wrong-way entry at off-ramp gore areas and frontage-road intersections that can create opposing-direction conflicts",
    interruption = "Acts at the ramp terminal by increasing wrong-way recognition and discouraging prohibited entry before an opposing-path conflict develops.",
    actions = c(
      "Install additional Wrong Way and Do Not Enter signs at off-ramp gore areas and ramp terminals using MUTCD-compliant retroreflective sheeting",
      "Supplement with LED-enhanced panels at high-incident locations where appropriate",
      "Coordinate signing with geometric channelization improvements where repeated wrong-way events are documented"
    ),
    pathway = c("Earlier wrong-way warning", "Reduced prohibited ramp entry", "Lower opposing-direction conflict exposure"),
    reference = "Avelar et al. (2023)",
    full_reference = "Avelar, R., Kutela, B., & Finley, M. (2023). Developing Crash Modification Factors for Wrong-Way-Driving Countermeasures (FHWA-HRT-22-115). Texas A&M Transportation Institute, Texas, USA. Reported CMF = 0.153 (CRF = 84.7%)."
  )
)

intervention_point_text <- function(policy_key) {
  switch(policy_key,
    P1 = "Reduce unintended interstate entry before high-speed exposure",
    P2 = "Create earlier bicyclist recognition and safer driver response time",
    P3 = "Resolve bicycle route ambiguity before freeway entry",
    P4 = "Reduce unsafe speed and maneuver behavior before the encounter",
    P5 = "Warn drivers before lane departure reaches the bicyclist-occupied shoulder",
    P6 = "Prevent wrong-way entry at ramp terminals before opposing paths develop",
    "Interrupt the relevant exposure or response condition before conflict escalation"
  )
}

# ============================================================
# Manuscript countermeasure linkage and case mechanism helpers
# ============================================================

countermeasure_cluster_map <- list(
  P1 = c("Dark C1", "Dark C2", "Daylight C1", "Daylight C2"),
  P2 = c("Dark C1", "Dark C2", "Dark C3", "Daylight C1", "Daylight C2", "Daylight C3"),
  P3 = c("Dark C2", "Dark C4", "Daylight C1", "Daylight C2"),
  P4 = c("Dark C3", "Daylight C4"),
  P5 = c("Dark C1", "Dark C4", "Daylight C1"),
  P6 = c("Dark C3", "Daylight C2")
)

# Manuscript-validated SHAP feature highlights.
# These are taken from the study interpretation and are used on the
# single-crash page instead of a different R-based SHAP approximation.
study_shap_features <- list(
  "Dark C1" = c("MV Type", "MV DR Age", "Bike Crash Type", "Attempted for Avoidance"),
  "Dark C2" = c("MV Type", "MV DR Age", "Bike Crash Type"),
  "Dark C3" = c("Speeding Related", "MV DR Age", "MV DR Gender"),
  "Dark C4" = c("MV DR Age", "MV DR Gender", "Prior Critical Event", "Bike Location"),
  "Daylight C1" = c("Bike Crash Type", "MV DR Age", "Attempted for Avoidance", "MV Type"),
  "Daylight C2" = c("Bike Crash Type", "Attempted for Avoidance", "MV Type", "Prior Critical Event"),
  "Daylight C3" = c("Critical Event Occurred by", "Bike Location", "Driver Impairment", "Driver Related Factor", "MV Type"),
  "Daylight C4" = c("Speeding Related", "MV DR Age", "MV DR Gender", "Prior Critical Event", "DOW")
)

choose_primary_countermeasure <- function(assignment, r, site_obs = character(0)) {
  key <- paste(assignment$light, assignment$cluster)
  linked <- names(countermeasure_cluster_map)[vapply(
    countermeasure_cluster_map, function(x) key %in% x, logical(1)
  )]
  if (length(linked) == 0) return("P2")

  bct <- norm_chr(r[["Bike Crash Type"]])
  bkl <- norm_chr(r[["Bike Location"]])
  bcd <- norm_chr(r[["Bicyclist Direction"]])
  spd <- norm_chr(r[["Speeding Related"]])
  drf <- norm_chr(r[["Driver Related Factor"]])
  ata <- norm_chr(r[["Attempted for Avoidance"]])
  land <- norm_chr(r[["Land Type"]])
  pce <- norm_chr(r[["Prior Critical Event"]])

  # Highly specific countermeasures are evaluated first.
  if ("P6" %in% linked &&
      (grepl("wrong", bct) || grepl("facing", bcd) ||
       ("ramp" %in% site_obs && grepl("oppos|wrong", pce)))) {
    return("P6")
  }

  if ("P5" %in% linked &&
      ("limited_shoulder" %in% site_obs ||
       (grepl("rural", land) && (grepl("overtak", bct) || "highspeed" %in% site_obs)))) {
    return("P5")
  }

  if ("P4" %in% linked &&
      (grepl("exceed|too fast", spd) || grepl("aggressive|non-compliant", drf))) {
    return("P4")
  }

  if ("P3" %in% linked &&
      (grepl("wrong", bct) || grepl("facing", bcd) ||
       any(c("ramp","route_guidance","no_parallel") %in% site_obs))) {
    return("P3")
  }

  if ("P2" %in% linked &&
      (grepl("overtak|turn", bct) ||
       grepl("braking|steering|no avoidance", ata) ||
       any(c("highspeed","limited_shoulder","merge") %in% site_obs))) {
    return("P2")
  }

  if ("P1" %in% linked &&
      (grepl("yield|cross", bct) || grepl("travel lane|sidewalk", bkl))) {
    return("P1")
  }

  fallback <- c(
    "Dark C1"="P2", "Dark C2"="P2", "Dark C3"="P4", "Dark C4"="P3",
    "Daylight C1"="P2", "Daylight C2"="P1", "Daylight C3"="P2", "Daylight C4"="P4"
  )
  cand <- unname(fallback[key])
  if (length(cand) == 1 && cand %in% linked) cand else linked[1]
}

cluster_mechanism_summary <- function(light, cluster, r, site_obs = character(0)) {
  key <- paste(light, cluster)
  base <- switch(
    key,
    "Dark C1" = "Yielding conflict under dark conditions with braking or steering response demands.",
    "Dark C2" = "Lane-change or overtaking interaction under dark mixed-traffic conditions.",
    "Dark C3" = "Speed-related high-severity interaction under dark conditions.",
    "Dark C4" = "Unclear maneuver or crossing-path interaction with limited response opportunity.",
    "Daylight C1" = "Turning or lane-change maneuver conflict during daylight operation.",
    "Daylight C2" = "Yielding or crossing conflict during daylight operation.",
    "Daylight C3" = "Maneuver and control conflict involving roadway position and vehicle interaction.",
    "Daylight C4" = "Speed-related turning interaction during daylight operation.",
    "Cluster-defined interstate bicyclist conflict."
  )

  modifiers <- character(0)
  if ("ramp" %in% site_obs) modifiers <- c(modifiers, "ramp/interchange exposure")
  if ("highspeed" %in% site_obs) modifiers <- c(modifiers, "high-speed geometry")
  if ("limited_shoulder" %in% site_obs) modifiers <- c(modifiers, "limited lateral recovery space")
  if ("merge" %in% site_obs) modifiers <- c(modifiers, "complex merge/turning context")
  if ("low_light" %in% site_obs) modifiers <- c(modifiers, "limited roadway lighting")
  if ("route_guidance" %in% site_obs) modifiers <- c(modifiers, "unclear bicycle route guidance")

  if (length(modifiers) > 0) {
    paste0(base, " Site review adds: ", paste(modifiers, collapse = "; "), ".")
  } else {
    base
  }
}

case_cues_for_countermeasure <- function(policy_key, r, assignment, site_obs, shap_features) {
  bct <- norm_chr(r[["Bike Crash Type"]])
  bkl <- norm_chr(r[["Bike Location"]])
  bcd <- norm_chr(r[["Bicyclist Direction"]])
  spd <- norm_chr(r[["Speeding Related"]])
  drf <- norm_chr(r[["Driver Related Factor"]])
  ata <- norm_chr(r[["Attempted for Avoidance"]])
  sf <- tolower(shap_features)
  cues <- character(0)

  if (policy_key == "P1") {
    if (grepl("travel lane|sidewalk", bkl)) cues <- c(cues, paste0("Bicycle location: ", safe_chr(r[["Bike Location"]])))
    if (grepl("wrong|yield|cross", bct)) cues <- c(cues, paste0("Crash type: ", safe_chr(r[["Bike Crash Type"]])))
    if ("ramp" %in% site_obs) cues <- c(cues, "Street View/site review: ramp or freeway-entry context")
    if ("no_parallel" %in% site_obs || "route_guidance" %in% site_obs) cues <- c(cues, "Street View/site review: route-guidance concern")
  }
  if (policy_key == "P2") {
    if (grepl("overtak|turn|yield|cross", bct)) cues <- c(cues, paste0("Crash interaction: ", safe_chr(r[["Bike Crash Type"]])))
    if (grepl("braking|steering|no avoidance|unknown", ata)) cues <- c(cues, paste0("Avoidance: ", safe_chr(r[["Attempted for Avoidance"]])))
    if (any(c("highspeed","merge","limited_shoulder") %in% site_obs)) cues <- c(cues, "Street View/site review: limited response/clearance context")
  }
  if (policy_key == "P3") {
    if (grepl("wrong", bct) || grepl("facing", bcd)) cues <- c(cues, "Crash record indicates route/direction concern")
    if ("ramp" %in% site_obs) cues <- c(cues, "Street View/site review: freeway-entry/ramp context")
    if ("route_guidance" %in% site_obs || "no_parallel" %in% site_obs) cues <- c(cues, "Street View/site review: wayfinding/parallel-route concern")
  }
  if (policy_key == "P4") {
    if (grepl("exceed|too fast", spd)) cues <- c(cues, paste0("Speeding: ", safe_chr(r[["Speeding Related"]])))
    if (grepl("aggressive|non-compliant", drf)) cues <- c(cues, paste0("Driver factor: ", safe_chr(r[["Driver Related Factor"]])))
    if ("highspeed" %in% site_obs || "low_light" %in% site_obs) cues <- c(cues, "Street View/site review: high-speed/visibility context")
  }
  if (policy_key == "P5") {
    land <- norm_chr(r[["Land Type"]])
    if (grepl("rural", land)) cues <- c(cues, "Crash record: rural interstate context")
    if (grepl("overtak", bct)) cues <- c(cues, paste0("Crash interaction: ", safe_chr(r[["Bike Crash Type"]])))
    if ("limited_shoulder" %in% site_obs) cues <- c(cues, "Street View/site review: shoulder/recovery-space concern")
    if ("highspeed" %in% site_obs) cues <- c(cues, "Street View/site review: high-speed operating context")
  }
  if (policy_key == "P6") {
    if (grepl("wrong", bct) || grepl("facing", bcd)) cues <- c(cues, "Crash record: wrong-way/opposing-direction concern")
    if ("ramp" %in% site_obs) cues <- c(cues, "Street View/site review: interchange ramp-terminal context")
    if ("route_guidance" %in% site_obs) cues <- c(cues, "Street View/site review: directional-signing/wayfinding concern")
  }
  if (length(sf) > 0) cues <- unique(c(cues, paste0("SHAP highlights: ", paste(head(shap_features, 2), collapse = ", "))))
  unique(head(cues, 4))
}

# ============================================================
# UI helpers
# ============================================================

card <- function(title, body, class = "") {
  div(class = paste("card-box", class),
      div(class = "card-title", title),
      body)
}

metric_box <- function(value, label) {
  div(class = "metric-box",
      div(class = "metric-value", value),
      div(class = "metric-label", label))
}

chip <- function(text, color = "#F4E1A6") {
  span(
    class = "evidence-chip",
    style = paste0("background:", color, ";"),
    text
  )
}

data_field <- function(label, value) {
  div(class = "data-field",
      div(class = "data-label", label),
      div(class = "data-value", safe_chr(value)))
}

cluster_share_table <- function(obj, light = NULL) {
  k <- obj$k
  out <- data.frame(
    Cluster = paste0("C", seq_len(k)),
    N = obj$counts,
    Share = paste0(sprintf("%.1f", obj$shares), "%"),
    stringsAsFactors = FALSE
  )
  if (!is.null(light) && k == 4) {
    m <- study_cluster_meta[study_cluster_meta$Lighting == light, , drop = FALSE]
    out$Manuscript <- paste0(m$Manuscript_N, " (", sprintf("%.1f", m$Manuscript_Share), "%)")
    out$Pattern <- m$Title
  }
  out
}

# ============================================================
# Mechanism and countermeasure scoring
# ============================================================

get_study_assignment <- function(global_row_index) {
  r <- dat[dat$.row_index == global_row_index, , drop = FALSE]
  light <- safe_chr(r[["Lighting Condition"]])[1]
  obj <- STUDY_CCA[[light]]
  pos <- match(global_row_index, obj$coords$row_index)
  if (is.na(pos)) return(NULL)
  cl <- obj$coords$Cluster[pos]
  list(
    light = light,
    cluster = cl,
    title = cluster_title(light, cl),
    share = obj$shares[as.integer(sub("C","",cl))],
    count = obj$counts[as.integer(sub("C","",cl))],
    obj = obj
  )
}


# Crash-specific explanation of a hard CCA assignment.
# The assignment itself comes directly from clusmca(). The two percentages below
# are explanatory diagnostics, NOT crash-risk probabilities:
#   1) relative coordinate proximity to the four CCA centroids;
#   2) residual profile alignment of the crash's own categorical levels.
cca_membership_evidence <- function(assignment, global_row_index) {
  obj <- assignment$obj
  pos <- match(global_row_index, obj$coords$row_index)
  if (is.na(pos)) return(NULL)

  pt <- obj$coords[pos, c("Dim1", "Dim2"), drop = FALSE]
  cl_levels <- paste0("C", seq_len(obj$k))
  centroids <- do.call(rbind, lapply(cl_levels, function(cl) {
    d <- obj$coords[obj$coords$Cluster == cl, c("Dim1", "Dim2"), drop = FALSE]
    data.frame(
      Cluster = cl,
      Dim1 = mean(d$Dim1, na.rm = TRUE),
      Dim2 = mean(d$Dim2, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  centroids$Distance <- sqrt((centroids$Dim1 - pt$Dim1[1])^2 + (centroids$Dim2 - pt$Dim2[1])^2)
  inv <- 1 / pmax(centroids$Distance, 1e-6)
  centroids$RelativeProximity <- 100 * inv / sum(inv)
  centroids$Assigned <- centroids$Cluster == assignment$cluster

  local_pos <- match(global_row_index, obj$data$.row_index)
  fd <- obj$factor_data
  vars <- names(fd)
  crash_cats <- data.frame(
    Variable = vars,
    Value = vapply(vars, function(v) as.character(fd[[v]][local_pos]), character(1)),
    stringsAsFactors = FALSE
  )
  crash_cats$Category <- paste0(crash_cats$Variable, " = ", crash_cats$Value)

  rd <- obj$residuals[obj$residuals$Cluster == assignment$cluster, c("Category", "Residual"), drop = FALSE]
  prof <- merge(crash_cats, rd, by = "Category", all.x = TRUE, sort = FALSE)
  prof$Residual[is.na(prof$Residual)] <- 0
  prof$Support <- prof$Residual > 0
  prof$Direction <- ifelse(prof$Residual > 0, "Supports assigned cluster", "Opposes / weakens")
  prof <- prof[order(abs(prof$Residual), decreasing = TRUE), , drop = FALSE]

  denom <- sum(abs(prof$Residual), na.rm = TRUE)
  alignment <- if (denom > 0) 100 * sum(pmax(prof$Residual, 0), na.rm = TRUE) / denom else NA_real_
  supportive_n <- sum(prof$Support, na.rm = TRUE)
  available_n <- nrow(prof)
  assigned_prox <- centroids$RelativeProximity[centroids$Cluster == assignment$cluster][1]

  list(
    centroids = centroids,
    profile = prof,
    alignment = alignment,
    supportive_n = supportive_n,
    available_n = available_n,
    assigned_proximity = assigned_prox
  )
}

countermeasure_evidence_support <- function(policy_key, assignment, r, site_obs, shap_features) {
  key <- paste(assignment$light, assignment$cluster)
  cluster_support <- key %in% countermeasure_cluster_map[[policy_key]]

  bct <- norm_chr(r[["Bike Crash Type"]])
  bkl <- norm_chr(r[["Bike Location"]])
  bcd <- norm_chr(r[["Bicyclist Direction"]])
  spd <- norm_chr(r[["Speeding Related"]])
  drf <- norm_chr(r[["Driver Related Factor"]])
  ata <- norm_chr(r[["Attempted for Avoidance"]])
  sf <- tolower(shap_features)

  crash_support <- switch(
    policy_key,
    P1 = grepl("yield|cross|wrong", bct) || grepl("travel lane|sidewalk", bkl),
    P2 = grepl("overtak|turn|yield|cross", bct) || grepl("braking|steering|no avoidance|unknown", ata),
    P3 = grepl("wrong", bct) || grepl("facing", bcd),
    P4 = grepl("exceed|too fast", spd) || grepl("aggressive|non-compliant", drf),
    P5 = grepl("rural", norm_chr(r[["Land Type"]])) || grepl("overtak", bct),
    P6 = grepl("wrong", bct) || grepl("facing", bcd),
    FALSE
  )

  shap_support <- switch(
    policy_key,
    P1 = any(grepl("bike crash type|bike location|bicyclist direction|prior critical", sf)),
    P2 = any(grepl("bike crash type|attempted for avoidance|mv type|mv dr age", sf)),
    P3 = any(grepl("bike crash type|bicyclist direction|bike location|prior critical", sf)),
    P4 = any(grepl("speeding related|driver related factor|mv dr age|mv dr gender", sf)),
    P5 = any(grepl("bike location|bike crash type|prior critical|mv type", sf)),
    P6 = any(grepl("bike crash type|bicyclist direction|prior critical|bike location", sf)),
    FALSE
  )

  site_available <- length(site_obs) > 0
  site_support <- switch(
    policy_key,
    P1 = any(c("ramp","route_guidance","no_parallel") %in% site_obs),
    P2 = any(c("highspeed","limited_shoulder","merge","low_light") %in% site_obs),
    P3 = any(c("ramp","route_guidance","no_parallel") %in% site_obs),
    P4 = any(c("highspeed","low_light") %in% site_obs),
    P5 = any(c("limited_shoulder","highspeed") %in% site_obs),
    P6 = any(c("ramp","route_guidance") %in% site_obs),
    FALSE
  )

  d <- data.frame(
    Source = c("CCA cluster linkage", "Crash-record evidence", "Validated SHAP", "Street View / site review"),
    Available = c(TRUE, TRUE, length(shap_features) > 0, site_available),
    Supported = c(cluster_support, crash_support, shap_support, site_support),
    stringsAsFactors = FALSE
  )
  denom <- sum(d$Available)
  pct <- if (denom > 0) 100 * sum(d$Supported & d$Available) / denom else NA_real_
  list(table = d, percentage = pct, supported = sum(d$Supported & d$Available), available = denom)
}

mechanism_definitions <- function(r, assignment, shap_features, site_obs) {
  bct <- norm_chr(r[["Bike Crash Type"]])
  bkl <- norm_chr(r[["Bike Location"]])
  bcd <- norm_chr(r[["Bicyclist Direction"]])
  spd <- norm_chr(r[["Speeding Related"]])
  ata <- norm_chr(r[["Attempted for Avoidance"]])
  pce <- norm_chr(r[["Prior Critical Event"]])
  dri <- norm_chr(r[["Driver Impairment"]])
  drf <- norm_chr(r[["Driver Related Factor"]])
  light <- assignment$light
  ctitle <- tolower(assignment$title)
  sf <- tolower(shap_features)

  shap_has <- function(pattern) any(grepl(pattern, sf))
  site_has <- function(x) x %in% site_obs

  defs <- list(
    M1 = list(
      title = "Unexpected route-entry or opposing-direction exposure",
      short = "Route-entry / wrong-way",
      criteria = c(
        grepl("wrong", bct) || grepl("facing", bcd),
        grepl("travel lane", bkl),
        site_has("ramp"),
        site_has("route_guidance"),
        grepl("cross|yield|unclear", ctitle),
        shap_has("bike crash type|bicyclist direction|bike location")
      ),
      crash_idx = 1:2, analysis_idx = 5:6, site_idx = 3:4,
      description = "The bicycle is exposed in a controlled-access travel environment where its direction or route entry may be unexpected to approaching motorists."
    ),
    M2 = list(
      title = "Yielding or crossing conflict with limited response time",
      short = "Yield / crossing",
      criteria = c(
        grepl("yield|crossing", bct),
        grepl("no avoidance|unknown", ata),
        site_has("merge"),
        site_has("ramp"),
        grepl("yield|crossing", ctitle),
        shap_has("bike crash type|attempted for avoidance|prior critical event")
      ),
      crash_idx = 1:2, analysis_idx = 5:6, site_idx = 3:4,
      description = "A crossing or yielding decision occurs in a high-speed environment where gap acceptance and evasive response time are limited."
    ),
    M3 = list(
      title = "Overtaking conflict with limited lateral clearance and driver expectancy",
      short = "Overtaking / clearance",
      criteria = c(
        grepl("overtak", bct),
        grepl("travel lane", bkl),
        site_has("limited_shoulder"),
        site_has("highspeed"),
        grepl("lane-change|mixed-traffic", ctitle),
        shap_has("mv type|bike location|prior critical event")
      ),
      crash_idx = 1:2, analysis_idx = 5:6, site_idx = 3:4,
      description = "A motor vehicle passes a bicyclist within an interstate-speed environment where lateral clearance, expectation, and response margins may be limited."
    ),
    M4 = list(
      title = "Turning, lane-change, or control conflict in a complex maneuver area",
      short = "Maneuver / turning",
      criteria = c(
        grepl("turn", bct) || grepl("changing lanes|curve", pce),
        site_has("merge"),
        grepl("turn|maneuver|control", ctitle),
        shap_has("bike crash type|prior critical event|bike location"),
        grepl("no avoidance|unknown", ata)
      ),
      crash_idx = c(1,5), analysis_idx = 3:4, site_idx = 2,
      description = "Active turning, lane-change, or control demands combine with interchange geometry and limited response opportunity."
    ),
    M5 = list(
      title = "Speed and visibility related high-severity interaction",
      short = "Speed / visibility",
      criteria = c(
        grepl("exceed|too fast", spd),
        site_has("highspeed"),
        light == "Dark" || site_has("low_light"),
        grepl("speed", ctitle),
        shap_has("speeding related|driver impairment")
      ),
      crash_idx = c(1,3), analysis_idx = 4:5, site_idx = 2,
      description = "High operating speed and reduced detection or response margins increase the severity potential of a bicyclist–motor-vehicle interaction."
    )
  )

  defs
}

mechanism_score_table <- function(defs) {
  keys <- names(defs)
  data.frame(
    Key = keys,
    Mechanism = vapply(defs, function(x) x$short, character(1)),
    Support = vapply(defs, function(x) round(100 * mean(x$criteria)), numeric(1)),
    Matched = vapply(defs, function(x) sum(x$criteria), numeric(1)),
    Total = vapply(defs, function(x) length(x$criteria), numeric(1)),
    stringsAsFactors = FALSE
  )
}

countermeasure_scores <- function(r, assignment, shap_features, site_obs, primary_mechanism_key) {
  bct <- norm_chr(r[["Bike Crash Type"]])
  bkl <- norm_chr(r[["Bike Location"]])
  bcd <- norm_chr(r[["Bicyclist Direction"]])
  spd <- norm_chr(r[["Speeding Related"]])
  drf <- norm_chr(r[["Driver Related Factor"]])
  ata <- norm_chr(r[["Attempted for Avoidance"]])
  ctitle <- tolower(assignment$title)
  sf <- tolower(shap_features)
  site_has <- function(x) x %in% site_obs
  shap_has <- function(pattern) any(grepl(pattern, sf))

  criteria <- list(
    P1 = c(
      primary_mechanism_key %in% c("M1","M2"),
      grepl("travel lane", bkl),
      grepl("wrong|yield|crossing", bct),
      site_has("ramp"),
      grepl("yield|crossing|unclear", ctitle)
    ),
    P2 = c(
      primary_mechanism_key %in% c("M2","M3","M4"),
      grepl("overtak|turn|yield|crossing", bct),
      grepl("braking|no avoidance|unknown", ata),
      site_has("highspeed") || site_has("merge") || site_has("limited_shoulder"),
      shap_has("mv type|mv dr age|attempted for avoidance|bike crash type")
    ),
    P3 = c(
      primary_mechanism_key == "M1",
      grepl("wrong", bct) || grepl("facing", bcd),
      site_has("ramp"),
      site_has("route_guidance"),
      grepl("cross|unclear|yield", ctitle)
    ),
    P4 = c(
      primary_mechanism_key == "M5" || grepl("exceed|too fast", spd),
      site_has("highspeed"),
      grepl("aggressive|non-compliant", drf),
      grepl("speed", ctitle),
      assignment$light == "Dark"
    ),
    P5 = c(
      paste(assignment$light, assignment$cluster) %in% countermeasure_cluster_map$P5,
      grepl("rural", norm_chr(r[["Land Type"]])),
      site_has("limited_shoulder"),
      grepl("overtak", bct),
      site_has("highspeed")
    ),
    P6 = c(
      paste(assignment$light, assignment$cluster) %in% countermeasure_cluster_map$P6,
      grepl("wrong", bct) || grepl("facing", bcd),
      site_has("ramp"),
      site_has("route_guidance"),
      grepl("cross|yield|speed", ctitle)
    )
  )

  data.frame(
    Key = names(criteria),
    Countermeasure = vapply(names(criteria), function(k) countermeasure_library[[k]]$short, character(1)),
    Applicability = vapply(criteria, function(x) round(100 * mean(x)), numeric(1)),
    Matched = vapply(criteria, function(x) sum(x), numeric(1)),
    Total = vapply(criteria, length, numeric(1)),
    stringsAsFactors = FALSE
  )
}

source_support <- function(def) {
  calc <- function(idx) {
    if (length(idx) == 0) return(NA_real_)
    round(100 * mean(def$criteria[idx]))
  }
  c(
    `Crash record` = calc(def$crash_idx),
    `CCA + SHAP` = calc(def$analysis_idx),
    `Street View` = calc(def$site_idx)
  )
}


# ============================================================
# Case-specific roadway diagram helpers
# ============================================================

crash_scenario <- function(r) {
  bct <- norm_chr(r[["Bike Crash Type"]])
  bcd <- norm_chr(r[["Bicyclist Direction"]])

  if (grepl("wrong", bct) || grepl("facing", bcd)) return("opposing")
  if (grepl("loss of control", bct)) return("loss_control")
  if (grepl("motorist turning", bct)) return("motorist_turning")
  if (grepl("bicyclist turned|bicyclist turn", bct)) return("bicyclist_turning")
  if (grepl("initial crossing", bct)) return("crossing")
  if (grepl("bicyclist failed to yield|motorist fail", bct)) return("yield_conflict")
  if (grepl("overtak", bct)) return("overtaking")
  "interaction"
}

crash_mechanism_text <- function(r, ctx, assignment) {
  scenario <- crash_scenario(r)
  loc <- safe_chr(r[["Bike Location"]], "recorded roadway position")
  bct <- safe_chr(r[["Bike Crash Type"]], "recorded bicycle crash type")
  avoid <- safe_chr(r[["Attempted for Avoidance"]], "Unknown")
  road <- paste(ctx$roadway_form %||% "interstate roadway", ctx$lane_summary %||% "lane context unavailable", sep = "; ")

  if (scenario == "interaction" || grepl("others|unknown", norm_chr(bct))) {
    return(paste0(
      "The selected record places the bicyclist in ", loc, " on a controlled-access, high-speed facility. ",
      "PBCAT classifies the crash as ", bct, " and the recorded avoidance action is ", avoid,
      ", so the exact pre-crash maneuver cannot be reconstructed from the available crash record. ",
      "The case is nevertheless assigned to ", assignment$light, " ", assignment$cluster, " (", assignment$title,
      "), a configuration characterized by unclear maneuver/crossing-path patterns. ",
      "Reviewed roadway context indicates ", road, ". The diagram therefore represents the documented exposure configuration rather than inventing an unobserved movement sequence."
    ))
  }

  base <- switch(
    scenario,
    opposing = paste0("The crash record indicates an opposing or unexpected bicycle travel direction with the bicyclist in ", loc,
                      ". The opposing paths reduce the available recognition and response interval in a high-speed operating environment."),
    crossing = paste0("The crash record indicates an initial crossing-path configuration with the bicyclist in ", loc,
                      ". The bicycle path intersects the motor-vehicle travel stream, creating a spatial conflict that requires recognition before the paths converge."),
    yield_conflict = paste0("The crash record indicates a failure-to-yield configuration with the bicyclist in ", loc,
                            ". The relevant road-user paths converge within the travel area; the diagram represents that convergence without assigning legal fault beyond the recorded PBCAT category."),
    motorist_turning = paste0("The crash record indicates a motorist-turning interaction with the bicyclist in ", loc,
                              ". The turning vehicle path converges with the bicycle path, so the case is represented as a turning conflict rather than a rear-approach event."),
    bicyclist_turning = paste0("The crash record indicates a bicyclist-turning interaction with the bicyclist in ", loc,
                               ". The bicycle maneuver changes the path relationship with the motor vehicle and is therefore represented as a turning/convergence configuration."),
    loss_control = paste0("The crash record identifies bicyclist loss of control with the bicyclist in ", loc,
                          ". Because the bicycle trajectory becomes unstable before impact, the diagram shows an irregular bicycle path rather than a normal straight-line interaction."),
    overtaking = paste0("The crash record indicates a motorist-overtaking interaction with the bicyclist in ", loc,
                       ". The vehicle approaches from behind while the bicyclist occupies or borders the travel area, making recognition, speed response, and lateral clearance important."),
    paste0("The crash record places the bicyclist in ", loc, " under a high-speed controlled-access operating context.")
  )
  paste0(base, " The case is assigned to ", assignment$light, " ", assignment$cluster,
         " (", assignment$title, "). Reviewed roadway context indicates ", road, ".")
}

countermeasure_effect_text <- function(policy_key, p) {
  switch(
    policy_key,
    P1 = paste0("Bicyclist education and route awareness acts before freeway exposure. Clear information about controlled-access restrictions, ramp avoidance, and route planning can support an earlier route decision before the rider reaches the interstate entrance. The pathway is exposure reduction rather than modification of the interstate roadway itself."),
    P2 = paste0("Driver expectancy and response training acts during the encounter. Earlier recognition of a rare bicyclist presence can support earlier speed reduction, lane-position adjustment, and greater lateral clearance before the vehicle reaches the bicyclist. The pathway addresses response timing and safe passing behavior."),
    P3 = paste0("Navigation and information systems acts upstream of the recorded exposure. Advance digital route guidance and ramp-area wayfinding can identify an appropriate permitted bicycle route before freeway entry, reducing the opportunity for interstate travel-lane exposure."),
    P4 = paste0("Targeted enforcement and high-visibility operations acts before the conflict by addressing speeding and unsafe maneuver behavior at locations and times where rare bicyclist presence may produce severe consequences. The intervention is behavioral and location-focused."),
    P5 = paste0("Shoulder rumble strips act at the lane-edge interface. A tactile and audible warning can alert drivers to unintended lane departure before the vehicle encroaches onto a paved shoulder where a bicyclist may be present. Application should preserve clear usable shoulder width and be targeted to appropriate rural interstate segments."),
    P6 = paste0("Enhanced wrong-way signing acts at off-ramp gore areas and ramp terminals before an opposing-direction conflict develops. Additional Wrong Way and Do Not Enter signing, potentially supplemented by enhanced visibility treatments, can strengthen recognition of prohibited entry and reduce wrong-way movement into the conflict space."),
    p$interruption
  )
}

cca_policy_link_text <- function(policy_key, assignment) {
  switch(
    policy_key,
    P1 = paste0(assignment$light, " ", assignment$cluster, " is a CCA configuration involving bicycle exposure and interaction conditions for which earlier route awareness can act before controlled-access entry."),
    P2 = paste0(assignment$light, " ", assignment$cluster, " contains interaction and response-related conditions for which earlier motorist recognition and response are operationally relevant."),
    P3 = paste0(assignment$light, " ", assignment$cluster, " contains route/access or maneuver conditions for which navigation and wayfinding can act before freeway entry."),
    P4 = paste0(assignment$light, " ", assignment$cluster, " contains speed-related conditions for which targeted speed and maneuver enforcement is operationally relevant."),
    P5 = paste0(assignment$light, " ", assignment$cluster, " is linked in the revised policy framework to shoulder rumble strips where rural high-speed shoulder exposure or run-off-road encroachment is relevant."),
    P6 = paste0(assignment$light, " ", assignment$cluster, " is linked in the revised policy framework to enhanced wrong-way signing where ramp-terminal or opposing-direction entry is relevant."),
    paste0(assignment$light, " ", assignment$cluster, " is linked to the selected countermeasure through the CCA-based policy framework.")
  )
}

countermeasure_case_rationale <- function(policy_key, r, assignment, site_obs, shap_features, ctx) {
  p <- countermeasure_library[[policy_key]]
  bct <- safe_chr(r[["Bike Crash Type"]])
  bloc <- safe_chr(r[["Bike Location"]])
  avoid <- safe_chr(r[["Attempted for Avoidance"]])
  road <- paste(ctx$roadway_form %||% "interstate roadway", ctx$lane_summary %||% "lane context unavailable", sep = "; ")

  site_labs <- c(ramp="ramp/freeway-entry context", highspeed="high-speed geometry", limited_shoulder="limited shoulder/recovery space", merge="merge/turning complexity", low_light="limited roadway lighting", route_guidance="route-guidance concern", no_parallel="no evident parallel bicycle facility")
  site_txt <- if (length(site_obs) > 0) paste(unname(site_labs[site_obs[site_obs %in% names(site_labs)]]), collapse=", ") else road

  relevant_shap <- intersect(shap_features, c("Bike Location", "Prior Critical Event", "Bike Crash Type", "Attempted for Avoidance", "Speeding Related", "MV Type", "Driver Impairment"))
  if (!length(relevant_shap)) relevant_shap <- head(shap_features, 2)
  shap_txt <- if (length(relevant_shap)) paste(relevant_shap, collapse=", ") else "the validated cluster-defining variables"

  if (policy_key == "P3") {
    return(paste0(
      "CCA assigns the selected case to ", assignment$light, " ", assignment$cluster, " (", assignment$title,
      "), which represents an unclear-maneuver/crossing-path configuration. The crash record places the bicyclist in ", bloc,
      " with PBCAT crash type = ", bct, " and avoidance = ", avoid, ". The validated SHAP profile identifies ", shap_txt,
      " among the variables that distinguish the cluster, while the site review indicates ", site_txt, ". ",
      "Navigation and information systems is therefore selected because it acts upstream of the observed interstate exposure by reducing route ambiguity before freeway entry. The recommendation does not assume that navigation error caused the crash."
    ))
  }

  if (policy_key == "P5") {
    return(paste0(
      "CCA assigns the selected case to ", assignment$light, " ", assignment$cluster, " (", assignment$title, "). ",
      "The crash record and site review indicate conditions relevant to rural high-speed shoulder exposure, while the validated SHAP profile identifies ", shap_txt, ". ",
      "Shoulder rumble strips are selected because they act at the lane edge by warning a driver before unintended encroachment reaches the paved shoulder. The recommendation is limited to locations where site review confirms suitable shoulder geometry and adequate usable shoulder width."
    ))
  }

  if (policy_key == "P6") {
    return(paste0(
      "CCA assigns the selected case to ", assignment$light, " ", assignment$cluster, " (", assignment$title, "). ",
      "The crash record indicates a wrong-way or opposing-direction concern, and the site review indicates ", site_txt, ". The validated SHAP profile identifies ", shap_txt, ". ",
      "Enhanced wrong-way signing is selected because it acts at the ramp terminal before prohibited entry creates an opposing-direction conflict. The recommendation does not imply that signing deficiency caused the individual crash."
    ))
  }

  paste0(
    "CCA assigns the selected case to ", assignment$light, " ", assignment$cluster, " (", assignment$title, "). ",
    "The crash record shows PBCAT crash type = ", bct, ", bicycle location = ", bloc, ", and avoidance = ", avoid, ". ",
    "The validated SHAP profile highlights ", shap_txt, ", while the site review indicates ", site_txt, ". ",
    p$name, " is selected because its intervention pathway directly addresses the exposure or response condition represented by the combined CCA, crash-record, SHAP, and site evidence without claiming a causal effect for the individual crash."
  )
}

# Vector car and bicycle icons for the conceptual crash diagram.
car_icon_layers <- function(x, y, scale = 1, color = "#315A86") {
  list(
    annotate("rect", xmin=x-0.8*scale, xmax=x+0.8*scale, ymin=y-0.32*scale, ymax=y+0.32*scale, fill=color, color="#203864", linewidth=0.5),
    annotate("rect", xmin=x-0.35*scale, xmax=x+0.35*scale, ymin=y+0.20*scale, ymax=y+0.48*scale, fill="#9EC5E5", color="#203864", linewidth=0.35),
    annotate("point", x=x-0.52*scale, y=y-0.40*scale, shape=21, size=3.4*scale, fill="#172033", color="#172033"),
    annotate("point", x=x+0.52*scale, y=y-0.40*scale, shape=21, size=3.4*scale, fill="#172033", color="#172033")
  )
}

bike_icon_layers <- function(x, y, scale = 1, color = "#D95F0E") {
  list(
    annotate("point", x=x-0.42*scale, y=y-0.22*scale, shape=1, size=6.2*scale, color=color, stroke=1.0),
    annotate("point", x=x+0.42*scale, y=y-0.22*scale, shape=1, size=6.2*scale, color=color, stroke=1.0),
    annotate("segment", x=x-0.42*scale, xend=x, y=y-0.22*scale, yend=y+0.20*scale, color=color, linewidth=1.0),
    annotate("segment", x=x, xend=x+0.42*scale, y=y+0.20*scale, yend=y-0.22*scale, color=color, linewidth=1.0),
    annotate("segment", x=x-0.42*scale, xend=x+0.42*scale, y=y-0.22*scale, yend=y-0.22*scale, color=color, linewidth=1.0),
    annotate("segment", x=x, xend=x+0.15*scale, y=y+0.20*scale, yend=y+0.48*scale, color=color, linewidth=0.9),
    annotate("point", x=x+0.15*scale, y=y+0.65*scale, shape=21, size=3.2*scale, fill="#F4C7A1", color="#6B4F3A")
  )
}

add_road_panel <- function(g, xmin, xmax, ctx, title, title_color) {
  total <- ctx$total_lanes
  if (is.na(total) || total < 1) total <- if (isTRUE(ctx$divided)) 4L else 2L
  total <- min(max(total, 1L), 6L)
  y0 <- 2.0; y1 <- 6.2
  g <- g + annotate("rect", xmin=xmin, xmax=xmax, ymin=y0, ymax=y1, fill="#59636E", color="#2F3943", linewidth=0.6)
  if (isTRUE(ctx$divided) && total >= 4) {
    g <- g + annotate("rect", xmin=xmin, xmax=xmax, ymin=3.90, ymax=4.30, fill="#C9B458", color="#A38B2D")
    upper_n <- max(2L, floor(total/2)); lower_n <- max(2L, total-upper_n)
    for (j in seq_len(max(upper_n-1,0))) {
      yy <- 4.30 + (y1-4.30) * j/upper_n
      if (j>0) g <- g + annotate("segment", x=xmin, xend=xmax, y=yy, yend=yy, linetype="dashed", color="white", linewidth=0.7)
    }
    for (j in seq_len(max(lower_n-1,0))) {
      yy <- y0 + (3.90-y0) * j/lower_n
      if (j>0) g <- g + annotate("segment", x=xmin, xend=xmax, y=yy, yend=yy, linetype="dashed", color="white", linewidth=0.7)
    }
  } else {
    if (total > 1) {
      for (j in 1:(total-1)) {
        yy <- y0 + (y1-y0)*j/total
        g <- g + annotate("segment", x=xmin, xend=xmax, y=yy, yend=yy, linetype="dashed", color="white", linewidth=0.7)
      }
    }
    if (total == 2) g <- g + annotate("segment", x=xmin, xend=xmax, y=(y0+y1)/2, yend=(y0+y1)/2, color="#F8D34A", linewidth=0.7)
  }
  g + annotate("text", x=(xmin+xmax)/2, y=7.25, label=title, fontface="bold", color=title_color, size=5.0) +
    annotate("text", x=(xmin+xmax)/2, y=6.68, label=paste0(ctx$roadway_form, " · ", ctx$lane_summary), fontface="bold", color="#344054", size=3.25)
}


# ============================================================
# Responsive SVG crash/countermeasure diagram
# ============================================================
svg_escape <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)

svg_car <- function(x, y, sc = 1, color = "#2F5F8F") {
  # Simple front-view car silhouette based on the user-provided reference image.
  # The raster asset is bundled under www/car_icon.png and contains no external dependency.
  w <- 118 * sc
  h <- 82 * sc
  sprintf(
    '<image href="car_icon.png" x="%s" y="%s" width="%s" height="%s" preserveAspectRatio="xMidYMid meet" style="filter:drop-shadow(0 0 2px #FFFFFF) drop-shadow(0 0 2px #FFFFFF);"/>',
    x - w/2, y - h/2, w, h
  )
}

svg_bicycle <- function(x, y, sc = 1, color = "#D95F0E", angle = 0) {
  sprintf(paste0(
    '<g transform="translate(%s,%s) rotate(%s) scale(%s)" stroke="%s" stroke-width="5" fill="none" stroke-linecap="round" stroke-linejoin="round">',
    '<circle cx="-34" cy="20" r="24"/><circle cx="36" cy="20" r="24"/>',
    '<path d="M-34,20 L-2,-18 L18,20 L-34,20 L2,20 L36,20 L10,-16"/>',
    '<path d="M-2,-18 L18,-18"/>',
    '<path d="M10,-16 L28,-28"/>',
    '<circle cx="5" cy="-42" r="10" fill="#F2C7A5" stroke="#76513F" stroke-width="3"/>',
    '<path d="M4,-31 L-2,-8 L18,4"/>',
    '</g>'), x, y, angle, sc, color)
}

svg_road <- function(x, y, w, h, ctx) {
  confirmed <- isTRUE(ctx$geometry_confirmed)
  total <- suppressWarnings(as.integer(ctx$total_lanes))
  parts <- c(sprintf('<rect x="%s" y="%s" width="%s" height="%s" rx="6" fill="#737F8B" stroke="#303A44" stroke-width="3"/>',x,y,w,h))

  if (!confirmed || !length(total) || is.na(total[1]) || total[1] < 1) {
    parts <- c(parts,
      sprintf('<text x="%s" y="%s" text-anchor="middle" font-family="Arial, sans-serif" font-size="17" font-weight="700" fill="#FFFFFF">ROADWAY GEOMETRY NOT YET CONFIRMED</text>', x+w/2, y+h/2-4),
      sprintf('<text x="%s" y="%s" text-anchor="middle" font-family="Arial, sans-serif" font-size="13" font-weight="600" fill="#E6EAF0">Confirm from Street View/site review before interpreting lane geometry</text>', x+w/2, y+h/2+22)
    )
    return(paste(parts, collapse=''))
  }

  total <- min(max(total[1], 1L), 8L)
  if (isTRUE(ctx$divided) && total >= 4) {
    mid <- y + h/2
    parts <- c(parts, sprintf('<rect x="%s" y="%s" width="%s" height="16" fill="#C9B458"/>',x,mid-8,w))
    per_dir <- max(2L, total %/% 2L)
    # Draw lane separators inside each carriageway only; the median remains uncrossed.
    for (j in seq_len(per_dir-1L)) {
      frac <- j/per_dir
      yy1 <- y + (h/2)*frac
      yy2 <- mid + (h/2)*frac
      parts <- c(parts,
        sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="white" stroke-width="3" stroke-dasharray="18 14"/>',x+8,yy1,x+w-8,yy1),
        sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="white" stroke-width="3" stroke-dasharray="18 14"/>',x+8,yy2,x+w-8,yy2)
      )
    }
  } else if (total == 1L) {
    # One-way single-lane connector: no internal separator.
  } else {
    for (j in seq_len(max(total-1L,0L))) {
      yy <- y + h*j/total
      center <- (total %% 2L == 0L && j == total/2L)
      col <- if (center) '#F4D03F' else 'white'
      dash <- if (center) '' else ' stroke-dasharray="18 14"'
      parts <- c(parts, sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="%s" stroke-width="3"%s/>',x+8,yy,x+w-8,yy,col,dash))
    }
  }
  paste(parts, collapse='')
}

svg_text <- function(x,y,text,size=18,weight=600,fill="#172033",anchor="middle") {
  sprintf('<text x="%s" y="%s" text-anchor="%s" font-family="Arial, sans-serif" font-size="%s" font-weight="%s" fill="%s">%s</text>',
          x,y,anchor,size,weight,fill,svg_escape(text))
}

svg_multiline <- function(x,y,lines,size=17,weight=500,fill="#344054",anchor="middle",line_gap=24) {
  tsp <- vapply(seq_along(lines), function(i) sprintf('<tspan x="%s" dy="%s">%s</tspan>',x,ifelse(i==1,0,line_gap),svg_escape(lines[i])), character(1))
  sprintf('<text x="%s" y="%s" text-anchor="%s" font-family="Arial, sans-serif" font-size="%s" font-weight="%s" fill="%s">%s</text>',x,y,anchor,size,weight,fill,paste(tsp,collapse=''))
}

build_case_diagram_svg <- function(r, ctx, key, p, assignment) {
  road_form <- ctx$roadway_form %||% "Roadway geometry not yet confirmed"
  lane_sum <- ctx$lane_summary %||% "Review required"
  scenario <- crash_scenario(r)
  bike_loc <- safe_chr(r[["Bike Location"]], "Recorded roadway position")
  bike_dir <- safe_chr(r[["Bicyclist Direction"]], "Unknown")
  bct <- safe_chr(r[["Bike Crash Type"]], "Unknown")
  mvtype <- safe_chr(r[["MV Type"]], "Motor vehicle")
  avoid <- safe_chr(r[["Attempted for Avoidance"]], "Unknown")
  confirmed <- isTRUE(ctx$geometry_confirmed)

  defs <- paste0(
    '<defs>',
    '<marker id="arrowOrange" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="#D95F0E"/></marker>',
    '<marker id="arrowGreen" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="#2E8540"/></marker>',
    '<marker id="arrowBlue" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="#245A8D"/></marker>',
    '</defs>'
  )

  # Compact two-panel figure.
  left_x <- 20; right_x <- 510; panel_w <- 465
  road_y <- 72; road_h <- 155

  lane_centers <- function(ctx, y, h) {
    total <- suppressWarnings(as.integer(ctx$total_lanes))
    if (!isTRUE(ctx$geometry_confirmed) || is.na(total) || total < 1) return(y + h/2)
    total <- min(max(total, 1L), 8L)
    if (isTRUE(ctx$divided) && total >= 4L) {
      n <- max(2L, total %/% 2L)
      return(y + ((seq_len(n) - 0.5) / n) * (h/2))
    }
    if (total == 1L) return(y + h/2)
    n <- max(1L, total %/% 2L)
    y + ((seq_len(n) - 0.5) / total) * h
  }

  same_dir <- lane_centers(ctx, road_y, road_h)
  main_y <- tail(same_dir, 1)
  adj_y <- if (length(same_dir) >= 2) same_dir[max(1, length(same_dir)-1)] else main_y
  has_adjacent <- length(same_dir) >= 2

  bicycle_y <- function(loc, default_y) {
    z <- norm_chr(loc)
    if (grepl("sidewalk", z)) return(road_y + 13)
    if (grepl("bicycle lane", z)) return(road_y + 28)
    default_y
  }

  # ----- PANEL HEADERS -----
  left <- c(
    svg_text(left_x+panel_w/2, 24, "BEFORE: RECORDED CRASH CONFIGURATION", 18, 800, "#7F2704"),
    svg_text(left_x+panel_w/2, 47, paste0(road_form, " | ", lane_sum), 12.5, 700, "#475467"),
    svg_road(left_x,road_y,panel_w,road_h,ctx)
  )

  right <- c(
    svg_text(right_x+panel_w/2, 24, "AFTER: COUNTERMEASURE PATHWAY", 18, 800, "#203864"),
    svg_text(right_x+panel_w/2, 47, p$name, 12.8, 800, "#6F1D3A")
  )

  if (!confirmed) {
    right <- c(right, svg_road(right_x,road_y,panel_w,road_h,ctx))
    left <- c(left, svg_text(left_x+panel_w/2, 252, "Roadway geometry requires site confirmation", 12.5, 700, "#B42318"))
    right <- c(right, svg_text(right_x+panel_w/2, 252, "Intervention geometry appears after site confirmation", 12.5, 700, "#203864"))
    return(paste0(
      '<div class="diagram-svg-wrap"><svg viewBox="0 0 1000 275" role="img" aria-label="Case-specific crash mechanism and countermeasure pathway">',
      defs, paste(c(left,right),collapse=''), '</svg></div>'
    ))
  }

  # ----- BEFORE: case-specific crash geometry -----
  car_x <- left_x + 115
  bike_x <- left_x + 350
  by <- bicycle_y(bike_loc, main_y)
  conflict_x <- left_x + 305
  conflict_y <- main_y

  if (scenario == "overtaking") {
    by <- bicycle_y(bike_loc, main_y)
    left <- c(left,
      svg_car(car_x, main_y, 0.58),
      svg_bicycle(bike_x, by, 0.45),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#D95F0E" stroke-width="4" stroke-dasharray="10 7" marker-end="url(#arrowOrange)"/>',
              car_x+48, main_y, bike_x-55, by),
      sprintf('<circle cx="%s" cy="%s" r="18" fill="none" stroke="#B42318" stroke-width="4" stroke-dasharray="6 5"/>',
              bike_x-15, by),
      svg_text(left_x+panel_w/2, 244, "Motorist overtaking: vehicle approaches bicyclist from behind", 12.5, 700, "#344054")
    )
  } else if (scenario %in% c("crossing","yield_conflict")) {
    by <- road_y + road_h*0.78
    conflict_x <- left_x + 325
    conflict_y <- main_y
    left <- c(left,
      svg_car(car_x, main_y, 0.58),
      svg_bicycle(conflict_x, by, 0.45, angle=-90),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#D95F0E" stroke-width="4" marker-end="url(#arrowOrange)"/>',
              car_x+48, main_y, conflict_x-35, main_y),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#D95F0E" stroke-width="4" marker-end="url(#arrowOrange)"/>',
              conflict_x, by-24, conflict_x, main_y+14),
      sprintf('<circle cx="%s" cy="%s" r="18" fill="none" stroke="#B42318" stroke-width="4" stroke-dasharray="6 5"/>',
              conflict_x, main_y),
      svg_text(left_x+panel_w/2, 244, if (scenario=="crossing") "Initial crossing paths: bicycle path intersects traffic stream" else "Failure-to-yield conflict: paths converge at the travel lane", 12.2, 700, "#344054")
    )
  } else if (scenario == "opposing") {
    left <- c(left,
      svg_car(car_x, main_y, 0.58),
      svg_bicycle(bike_x, main_y, 0.45, angle=180),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#D95F0E" stroke-width="4" marker-end="url(#arrowOrange)"/>',
              car_x+48, main_y, left_x+255, main_y),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#D95F0E" stroke-width="4" marker-end="url(#arrowOrange)"/>',
              bike_x-45, main_y, left_x+270, main_y),
      sprintf('<circle cx="%s" cy="%s" r="18" fill="none" stroke="#B42318" stroke-width="4" stroke-dasharray="6 5"/>',
              left_x+265, main_y),
      svg_text(left_x+panel_w/2, 244, "Opposing/wrong-way movement: road users approach one another", 12.3, 700, "#344054")
    )
  } else if (scenario %in% c("motorist_turning","bicyclist_turning")) {
    bike_y2 <- bicycle_y(bike_loc, main_y)
    left <- c(left,
      svg_car(car_x, main_y, 0.58),
      svg_bicycle(bike_x, bike_y2, 0.45),
      sprintf('<path d="M %s %s C %s %s, %s %s, %s %s" fill="none" stroke="#D95F0E" stroke-width="4" marker-end="url(#arrowOrange)"/>',
              car_x+48, main_y, left_x+220, main_y, left_x+260, bike_y2, bike_x-45, bike_y2),
      sprintf('<circle cx="%s" cy="%s" r="18" fill="none" stroke="#B42318" stroke-width="4" stroke-dasharray="6 5"/>',
              bike_x-25, bike_y2),
      svg_text(left_x+panel_w/2, 244, if (scenario=="motorist_turning") "Motorist turning error: turning path converges with bicycle path" else "Bicyclist turning: bicycle movement converges with vehicle path", 12.1, 700, "#344054")
    )
  } else if (scenario == "loss_control") {
    left <- c(left,
      svg_car(car_x, main_y, 0.58),
      svg_bicycle(bike_x, main_y, 0.45, angle=-22),
      sprintf('<path d="M %s %s C %s %s, %s %s, %s %s" fill="none" stroke="#D95F0E" stroke-width="4" stroke-dasharray="8 6" marker-end="url(#arrowOrange)"/>',
              bike_x-80, main_y-22, bike_x-45, main_y+26, bike_x-20, main_y-18, bike_x+8, main_y+8),
      sprintf('<circle cx="%s" cy="%s" r="18" fill="none" stroke="#B42318" stroke-width="4" stroke-dasharray="6 5"/>',
              bike_x-18, main_y),
      svg_text(left_x+panel_w/2, 244, "Bicyclist loss of control: bicycle trajectory becomes unstable", 12.4, 700, "#344054")
    )
  } else {
    # Others/unclear: preserve recorded positions but do not invent a trajectory.
    left <- c(left,
      svg_car(car_x, main_y, 0.58),
      svg_bicycle(bike_x, bicycle_y(bike_loc, main_y), 0.45),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#D95F0E" stroke-width="4" stroke-dasharray="5 8" marker-end="url(#arrowOrange)"/>',
              car_x+48, main_y, left_x+270, main_y),
      svg_text(left_x+panel_w/2, 244, "Recorded exposure shown; exact pre-impact trajectory is unavailable", 12.4, 700, "#344054")
    )
  }

  # Actor/data labels are kept small and outside the conflict arrows.
  left <- c(left,
    svg_text(left_x+105, 267, paste0("Vehicle: ", mvtype), 11.3, 700, "#172033"),
    svg_text(left_x+345, 267, paste0("Bicycle: ", bike_loc, " | ", bike_dir), 11.3, 700, "#7F2704")
  )

  # ----- AFTER: countermeasure must create a visibly different state -----
  if (key %in% c("P1","P3")) {
    # Upstream route-choice scene, intentionally different from the crash roadway.
    right <- c(right,
      sprintf('<rect x="%s" y="%s" width="%s" height="%s" rx="6" fill="#737F8B" stroke="#303A44" stroke-width="3"/>',
              right_x, road_y, panel_w, road_h),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="white" stroke-width="3" stroke-dasharray="18 14"/>',
              right_x+12, main_y, right_x+panel_w-12, main_y),
      sprintf('<rect x="%s" y="%s" width="118" height="42" rx="8" fill="#FEE4E2" stroke="#B42318" stroke-width="2"/>',
              right_x+330, road_y+88),
      svg_text(right_x+389, road_y+105, "FREEWAY", 12.5, 800, "#B42318"),
      svg_text(right_x+389, road_y+121, "ENTRY", 11.5, 800, "#B42318"),
      sprintf('<path d="M %s %s C %s %s, %s %s, %s %s" fill="none" stroke="#2E8540" stroke-width="5" marker-end="url(#arrowGreen)"/>',
              right_x+125, main_y, right_x+210, main_y, right_x+235, road_y+30, right_x+330, road_y+26),
      sprintf('<path d="M %s %s L %s %s" stroke="#B42318" stroke-width="4" stroke-dasharray="7 6"/>',
              right_x+260, main_y, right_x+330, main_y),
      svg_bicycle(right_x+120, main_y, 0.45),
      sprintf('<rect x="%s" y="%s" width="150" height="40" rx="8" fill="#E8F5EA" stroke="#2E8540" stroke-width="2"/>',
              right_x+270, road_y+7),
      svg_text(right_x+345, road_y+24, if (key=="P3") "ROUTE GUIDANCE" else "ROUTE AWARENESS", 12.2, 800, "#1D6B35"),
      svg_text(right_x+345, road_y+39, "PERMITTED ROUTE", 11.3, 800, "#1D6B35"),
      svg_text(right_x+panel_w/2, 244, "Bicycle route decision changes before controlled-access entry", 12.3, 700, "#1D6B35"),
      svg_text(right_x+panel_w/2, 267, "Resulting state: bicyclist is not placed in the interstate conflict space", 11.3, 700, "#344054")
    )
  } else if (key == "P2") {
    right <- c(right, svg_road(right_x,road_y,panel_w,road_h,ctx))
    rcx <- right_x + 115
    rbx <- right_x + 360
    rby <- bicycle_y(bike_loc, main_y)

    if (scenario == "overtaking") {
      if (has_adjacent) {
        right <- c(right,
          svg_car(rcx, adj_y, 0.58),
          svg_bicycle(rbx, rby, 0.45),
          sprintf('<path d="M %s %s C %s %s, %s %s, %s %s" fill="none" stroke="#2E8540" stroke-width="5" marker-end="url(#arrowGreen)"/>',
                  rcx-30, main_y, rcx+5, main_y, rcx+35, adj_y, rcx+85, adj_y),
          svg_text(right_x+panel_w/2, road_y+23, "EARLY LANE-POSITION RESPONSE", 12.6, 800, "#1D6B35"),
          svg_text(right_x+panel_w/2, 244, "Vehicle moves to the adjacent same-direction lane before passing", 12.1, 700, "#1D6B35")
        )
      } else {
        right <- c(right,
          svg_car(rcx, main_y, 0.58),
          svg_bicycle(rbx, rby, 0.45),
          sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#2E8540" stroke-width="4" stroke-dasharray="9 7"/>',
                  rcx+48, main_y, rbx-85, rby),
          svg_text(right_x+panel_w/2, road_y+23, "EARLY SPEED REDUCTION / SAFE FOLLOWING", 12.3, 800, "#1D6B35"),
          svg_text(right_x+panel_w/2, 244, "Vehicle remains behind until a safe passing opportunity exists", 12.1, 700, "#1D6B35")
        )
      }
    } else if (scenario %in% c("crossing","yield_conflict")) {
      cross_x <- right_x + 330
      cross_by <- road_y + road_h*0.78
      right <- c(right,
        svg_car(right_x+120, main_y, 0.58),
        svg_bicycle(cross_x, cross_by, 0.45, angle=-90),
        sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#2E8540" stroke-width="6"/>',
                cross_x-60, road_y+7, cross_x-60, road_y+road_h-7),
        sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#2E8540" stroke-width="4" marker-end="url(#arrowGreen)"/>',
                right_x+170, main_y, cross_x-75, main_y),
        svg_text(right_x+245, road_y+23, "EARLY DETECTION + DECELERATION", 12.5, 800, "#1D6B35"),
        svg_text(right_x+panel_w/2, 244, "Vehicle response begins before the crossing conflict point", 12.1, 700, "#1D6B35")
      )
    } else if (scenario == "opposing") {
      right <- c(right,
        svg_car(right_x+120, main_y, 0.58),
        svg_bicycle(right_x+360, main_y, 0.45, angle=180),
        sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#2E8540" stroke-width="4" stroke-dasharray="9 7"/>',
                right_x+170, main_y, right_x+250, main_y),
        svg_text(right_x+panel_w/2, road_y+23, "EARLY RECOGNITION + SPEED REDUCTION", 12.5, 800, "#1D6B35"),
        svg_text(right_x+panel_w/2, 244, "More separation is preserved before the opposing encounter", 12.1, 700, "#1D6B35")
      )
    } else if (scenario %in% c("motorist_turning","bicyclist_turning")) {
      right <- c(right,
        svg_car(right_x+120, main_y, 0.58),
        svg_bicycle(right_x+355, bicycle_y(bike_loc, main_y), 0.45),
        sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#2E8540" stroke-width="6"/>',
                right_x+285, road_y+8, right_x+285, road_y+road_h-8),
        svg_text(right_x+panel_w/2, road_y+23, "YIELD / CHECK BEFORE TURNING MOVEMENT", 12.2, 800, "#1D6B35"),
        svg_text(right_x+panel_w/2, 244, "Turning response is delayed until the bicycle conflict space is clear", 11.9, 700, "#1D6B35")
      )
    } else {
      right <- c(right,
        svg_car(right_x+110, main_y, 0.58),
        svg_bicycle(right_x+375, bicycle_y(bike_loc, main_y), 0.45),
        sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#2E8540" stroke-width="4" stroke-dasharray="9 7"/>',
                right_x+160, main_y, right_x+275, main_y),
        svg_text(right_x+panel_w/2, road_y+23, "EARLIER RECOGNITION / MORE RESPONSE TIME", 12.4, 800, "#1D6B35"),
        svg_text(right_x+panel_w/2, 244, "The vehicle is shown farther from the bicyclist at the response point", 11.9, 700, "#1D6B35")
      )
    }
    right <- c(right,
      svg_text(right_x+panel_w/2, 267, paste0("Recorded avoidance: ", avoid), 11.1, 700, "#344054")
    )
  } else {
    # P4: speed/maneuver enforcement produces a different pre-conflict state.
    right <- c(right,
      svg_road(right_x,road_y,panel_w,road_h,ctx),
      svg_car(right_x+105, main_y, 0.58),
      svg_bicycle(right_x+375, bicycle_y(bike_loc, main_y), 0.45),
      sprintf('<rect x="%s" y="%s" width="178" height="44" rx="8" fill="#E8F5EA" stroke="#2E8540" stroke-width="2"/>',
              right_x+185, road_y+7),
      svg_text(right_x+274, road_y+25, "HIGH-VISIBILITY", 11.8, 800, "#1D6B35"),
      svg_text(right_x+274, road_y+40, "SPEED / MANEUVER CONTROL", 11.2, 800, "#1D6B35"),
      sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" stroke="#2E8540" stroke-width="4" stroke-dasharray="9 7"/>',
              right_x+155, main_y, right_x+280, main_y),
      svg_text(right_x+panel_w/2, 244, "Lower-risk operating state is established before the encounter", 12.0, 700, "#1D6B35"),
      svg_text(right_x+panel_w/2, 267, "No individual-crash effect size is implied", 11.1, 700, "#344054")
    )
  }

  paste0(
    '<div class="diagram-svg-wrap"><svg viewBox="0 0 1000 275" role="img" aria-label="Case-specific crash mechanism and countermeasure pathway">',
    defs, paste(c(left,right),collapse=''), '</svg></div>'
  )
}

# ============================================================
# UI
# ============================================================

css <- "
body { background:#F7F4F2; color:#172033; }
.navbar-inverse { background:__BRAND__; border-color:__BRAND__; }
.navbar-inverse .navbar-brand, .navbar-inverse .navbar-nav>li>a { color:#FFFFFF; font-weight:700; }
.navbar-inverse .navbar-nav>.active>a, .navbar-inverse .navbar-nav>.active>a:focus, .navbar-inverse .navbar-nav>.active>a:hover { background:#8B2749; }
.container-fluid { padding-left:18px; padding-right:18px; }
.section-card { background:#FFFFFF; border:1px solid #E4E7EC; border-radius:12px; padding:14px; margin-bottom:14px; box-shadow:0 1px 3px rgba(16,24,40,.05); }
.selected-cluster-banner { background:#FFF7E6; border:2px solid #E3AD20; border-radius:12px; padding:12px 16px; margin:10px 0 12px; }
.selected-cluster-label { color:#7F2704; font-size:12px; font-weight:800; text-transform:uppercase; letter-spacing:.45px; }
.selected-cluster-main { color:#6F1D3A; font-size:22px; font-weight:800; margin-top:3px; }
.selected-cluster-sub { color:#344054; font-size:14px; font-weight:600; margin-top:4px; }
.card-title { color:__BRAND__; font-weight:800; font-size:18px; margin-bottom:8px; }
.subtle { color:#667085; font-size:13px; line-height:1.45; }
.metric-row { display:grid; grid-template-columns:repeat(4,1fr); gap:10px; margin-bottom:14px; }
.metric-box { background:#FFFFFF; border:1px solid #E4E7EC; border-radius:12px; padding:13px 15px; }
.metric-value { color:__BRAND__; font-size:25px; font-weight:800; }
.metric-label { color:#667085; font-size:10px; text-transform:uppercase; letter-spacing:.45px; }
.source-badge { display:inline-block; padding:5px 9px; border-radius:12px; font-size:11px; text-transform:uppercase; font-weight:800; letter-spacing:.35px; background:#EEF2F6; color:#475467; margin-bottom:7px; }
.data-grid { display:grid; grid-template-columns:repeat(2,1fr); gap:7px; }
.data-field { background:#F9FAFB; border:1px solid #E4E7EC; border-radius:9px; padding:8px; }
.data-label { color:#667085; font-size:10.5px; font-weight:700; text-transform:uppercase; }
.data-value { font-weight:800; font-size:13px; margin-top:2px; }
.evidence-chip { display:inline-block; border-radius:15px; padding:5px 8px; margin:3px 3px 3px 0; font-size:10px; border:1px solid rgba(0,0,0,.07); }
.street-frame { width:100%; height:350px; border:1px solid #D0D5DD; border-radius:10px; background:#F2F4F7; }
.study-layout { display:grid; grid-template-columns:32% 68%; gap:12px; }
.study-right-top { display:grid; grid-template-columns:43% 57%; gap:12px; }
.study-right-bottom { display:grid; grid-template-columns:40% 60%; gap:12px; margin-top:12px; }
.cluster-kpi-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:7px; margin-top:8px; }
.cluster-kpi { border-radius:10px; padding:9px; color:#3D2B00; min-height:72px; }
.cluster-kpi b { font-size:15px; display:block; }
.cluster-kpi span { font-size:9px; line-height:1.2; }

.membership-grid { display:grid; grid-template-columns:38% 62%; gap:12px; margin-top:12px; }
.membership-kpis { display:grid; grid-template-columns:repeat(3,1fr); gap:8px; margin-bottom:10px; }
.membership-kpi { background:#F8FAFC; border:1px solid #D9E0E8; border-radius:10px; padding:10px; }
.membership-kpi .kpi-big { font-size:22px; font-weight:800; color:#6F1D3A; }
.membership-kpi .kpi-small { font-size:11px; font-weight:700; color:#667085; text-transform:uppercase; letter-spacing:.35px; }
.explain-note { font-size:12.5px; color:#667085; line-height:1.45; margin-top:6px; }
.support-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:8px; margin:10px 0 12px; }
.support-box { border:1px solid #D9E0E8; border-radius:10px; padding:9px; background:#F8FAFC; min-height:82px; }
.support-box.yes { background:#FFF4D6; border-color:#E3AD20; }
.support-box.pending { background:#F2F4F7; color:#667085; }
.support-box .support-source { font-size:10.5px; text-transform:uppercase; font-weight:800; color:#667085; }
.support-box .support-state { font-size:16px; font-weight:800; margin-top:5px; color:#203864; }
.support-box.yes .support-state { color:#7F2704; }
.counter-preview-grid { display:grid; grid-template-columns:42% 58%; gap:12px; margin-top:12px; }
.counter-site-frame { width:100%; height:290px; border:1px solid #D0D5DD; border-radius:10px; background:#F2F4F7; }
.analysis-strip { display:grid; grid-template-columns:repeat(4,1fr); gap:10px; }
.analysis-card { border:1px solid #E4E7EC; border-radius:11px; padding:11px; background:#FBFCFD; min-height:130px; }
.site-and-flow-grid { display:grid; grid-template-columns:1fr 1fr; gap:12px; align-items:start; margin-top:10px; }
.flowchart-card { border:1px solid #E4E7EC; border-radius:11px; padding:12px; background:#FBFCFD; min-height:360px; }
.flowchart-title { font-size:19px; font-weight:800; color:#203864; margin:2px 0 4px; }
.flowchart-sub { font-size:12px; line-height:1.45; color:#667085; margin-bottom:10px; }
.flow-evidence-grid { display:grid; grid-template-columns:1fr 1fr; gap:10px; margin-bottom:10px; }
.flow-evidence { background:#FFFFFF; border:1px solid #D8DEE7; border-radius:10px; padding:10px; min-height:88px; }
.flow-evidence .src { font-size:10px; text-transform:uppercase; letter-spacing:.35px; font-weight:800; color:#667085; margin-bottom:5px; }
.flow-evidence .val { font-size:13px; line-height:1.42; color:#203864; font-weight:700; }
.flow-arrow-line { text-align:center; color:#D95F0E; font-size:26px; font-weight:900; line-height:1; margin:1px 0 6px; }
.flow-stage { border-radius:12px; padding:12px; margin-bottom:8px; border:1px solid #D8DEE7; background:#FFFFFF; }
.flow-stage .k { font-size:10px; text-transform:uppercase; letter-spacing:.35px; font-weight:800; color:#667085; margin-bottom:6px; }
.flow-stage .v { font-size:15px; line-height:1.4; font-weight:800; color:#203864; }
.flow-stage.intervention { background:#FFF8E8; border-color:#E3AD20; }
.flow-stage.countermeasure { background:#FFF3E0; border-color:#D95F0E; }
.flow-stage.pathway { background:#F8FAFC; border-color:#D0D5DD; }
.flow-pathway-row { display:flex; flex-wrap:wrap; gap:5px; align-items:center; }
.flow-pathway-chip { display:inline-block; background:#FDE7A9; border:1px solid #F6C45A; border-radius:14px; padding:5px 8px; font-size:11px; font-weight:700; color:#4B2B00; }
.flow-pathway-arrow { color:#D95F0E; font-size:18px; font-weight:900; }
.visual-convergence { display:grid; gap:8px; }
.vc-evidence-grid { display:grid; grid-template-columns:1fr 1fr; gap:9px; }
.vc-card { border:1px solid #D9E0E8; border-radius:11px; padding:10px 11px; background:#FFFFFF; min-height:86px; }
.vc-card.crash { border-left:5px solid #D95F0E; }
.vc-card.cca { border-left:5px solid #7F2704; }
.vc-card.shap { border-left:5px solid #203864; }
.vc-card.site { border-left:5px solid #2E8540; }
.vc-label { font-size:10px; text-transform:uppercase; letter-spacing:.4px; font-weight:800; color:#667085; margin-bottom:4px; }
.vc-value { font-size:13px; line-height:1.4; font-weight:750; color:#172033; }
.vc-converge { text-align:center; color:#D95F0E; font-weight:900; font-size:23px; line-height:1; padding:1px 0; }
.vc-intervention { background:#FFF7E6; border:2px solid #E3AD20; border-radius:12px; padding:11px 13px; text-align:center; }
.vc-intervention .vc-value { font-size:15px; color:#6F1D3A; }
.vc-countermeasure { background:#FFF1E6; border:2px solid #D95F0E; border-radius:12px; padding:11px 13px; text-align:center; }
.vc-countermeasure .vc-value { font-size:16px; color:#7F2704; }
.vc-pathway { border:1px solid #D0D5DD; border-radius:12px; padding:10px 12px; background:#F8FAFC; }
.vc-pathway-row { display:flex; align-items:center; justify-content:center; gap:6px; flex-wrap:wrap; margin-top:5px; }
.vc-chip { background:#FDE7A9; border:1px solid #F6C45A; color:#4B2B00; border-radius:16px; padding:6px 9px; font-size:11px; font-weight:800; }
.vc-arrow { color:#D95F0E; font-size:17px; font-weight:900; }
.flow-grid { display:grid; grid-template-columns:1fr 35px 1fr 35px 1fr 35px 1fr; align-items:stretch; gap:4px; }
.flow-node { background:#FFFFFF; border:1px solid #D8DEE7; border-radius:12px; padding:12px; min-height:125px; }
.flow-node.action { background:#FFF6E5; border-color:#E3AD20; }
.flow-arrow { display:flex; align-items:center; justify-content:center; color:#D95F0E; font-size:24px; font-weight:800; }
.flow-kicker { color:#667085; font-size:8px; text-transform:uppercase; letter-spacing:.4px; }
.flow-title { color:#203864; font-weight:800; margin:5px 0; font-size:13px; }
.flow-text { color:#475467; font-size:10px; line-height:1.45; }
.counter-text { background:#FFFAF0; border:1px solid #E3AD20; border-radius:12px; padding:13px; margin-top:12px; }
.counter-text h4 { color:#7F2704; font-weight:800; margin:3px 0 8px; }
.tab-content { padding-top:12px; }
.nav-tabs>li>a { font-weight:700; color:#203864; }
.btn-primary { background:__BRAND__; border-color:__BRAND__; }
.btn-warning { background:#D95F0E; border-color:#D95F0E; color:#fff; }
@media(max-width:1100px) {
  .study-layout,.study-right-top,.study-right-bottom,.analysis-strip,.flow-grid,.site-and-flow-grid { display:block; }
  .vc-evidence-grid { grid-template-columns:1fr; }
  .flow-arrow { transform:rotate(90deg); height:30px; }
  .metric-row,.cluster-kpi-grid { grid-template-columns:repeat(2,1fr); }
}
@media(max-width:700px) {
  .metric-row,.cluster-kpi-grid,.data-grid { grid-template-columns:1fr; }
}
.case-flow-wrap { width:100%; }
.flow-grid-v2 { display:grid; grid-template-columns:1fr 30px 1fr 30px 1fr 30px 1fr; gap:5px; align-items:stretch; }
.flow-node-v2 { background:#FBFCFD; border:1px solid #D8DEE7; border-radius:11px; padding:12px; min-height:126px; }
.flow-arrow-v2 { display:flex; align-items:center; justify-content:center; font-size:24px; font-weight:800; color:#D95F0E; }
.mechanism-arrow { text-align:center; font-size:28px; color:#D95F0E; font-weight:800; margin:4px 0; }
.mechanism-box-v2 { max-width:820px; margin:0 auto; background:#FFF7E6; border:2px solid #E3AD20; border-radius:14px; padding:14px 18px; text-align:center; }
.mechanism-title-v2 { font-size:18px; font-weight:800; color:#6F1D3A; margin:3px 0 5px; }
.mechanism-text-v2 { font-size:12px; line-height:1.5; color:#344054; }
.counter-grid-v2 { display:grid; grid-template-columns:repeat(2,1fr); gap:12px; }
.counter-card-v2 { border-radius:13px; padding:14px; border:1px solid #D0D5DD; background:#F8FAFC; min-height:360px; }
.counter-card-v2.linked { background:#FFF7E6; border:2px solid #E3AD20; box-shadow:0 2px 7px rgba(217,95,14,.10); }
.counter-status-v2 { display:inline-block; font-size:9px; text-transform:uppercase; letter-spacing:.4px; font-weight:800; padding:4px 7px; border-radius:12px; margin-bottom:7px; }
.counter-status-v2.linked { background:#F6C45A; color:#4B2B00; }
.counter-status-v2.comparison { background:#EAECF0; color:#667085; }
.counter-target-v2 { background:#FFFFFF; border:1px solid #E4E7EC; border-radius:8px; padding:8px; font-size:11px; margin-bottom:9px; }
.counter-minihead { font-weight:800; color:#6F1D3A; font-size:12px; text-transform:uppercase; letter-spacing:.35px; margin-top:9px; }
.safety-path-v2 { display:flex; gap:5px; align-items:center; flex-wrap:wrap; margin:7px 0; }
.safety-path-v2 span:nth-child(odd) { background:#FDE7A9; border-radius:12px; padding:5px 7px; font-size:9.5px; }
.safety-path-v2 span:nth-child(even) { color:#D95F0E; font-weight:800; }
.counter-ref-v2 { margin-top:10px; font-size:12px; color:#475467; }
@media (max-width:1100px) {
  .analysis-strip { grid-template-columns:repeat(2,1fr); }
  .flow-grid-v2 { grid-template-columns:1fr; }
  .flow-arrow-v2 { transform:rotate(90deg); min-height:20px; }
  .counter-grid-v2 { grid-template-columns:1fr; }
}

.api-status { border-radius:10px; padding:9px 11px; margin:8px 0; font-size:12px; font-weight:700; }
.api-status.on { background:#EAF7EF; border:1px solid #78B88D; color:#1E6A39; }
.api-status.off { background:#F2F4F7; border:1px solid #D0D5DD; color:#667085; }
.site-context-grid { display:grid; grid-template-columns:repeat(3,1fr); gap:7px; margin:8px 0; }
.site-context-item { background:#F8FAFC; border:1px solid #D9E0E8; border-radius:9px; padding:8px; }
.site-context-item b { display:block; color:#203864; font-size:11px; margin-bottom:2px; }
.site-context-item span { font-size:12px; color:#344054; font-weight:600; }
.support-detail { font-size:11px; color:#475467; line-height:1.35; margin-top:5px; }
.diagram-explain-grid { display:grid; grid-template-columns:1fr 1fr; gap:10px; margin-top:9px; }
.diagram-explain { border-radius:10px; padding:11px; border:1px solid #D9E0E8; background:#FBFCFD; }
.diagram-explain.before { border-left:5px solid #B42318; }
.diagram-explain.after { border-left:5px solid #2E8540; }
.diagram-explain b { color:#203864; font-size:13px; }
.diagram-explain p { font-size:12px; line-height:1.45; margin:5px 0 0; }
.rationale-box { background:#FFF9EA; border:1px solid #E3AD20; border-radius:10px; padding:11px; font-size:13px; line-height:1.5; margin:9px 0; }
.study-plot { width:100%; height:auto; max-height:600px; object-fit:contain; }

.site-and-diagram-stack { display:grid; grid-template-columns:1fr; gap:14px; }
.site-and-diagram-stack > .analysis-card { max-width:820px; }
.diagram-svg-wrap { width:100%; max-width:980px; margin:0 auto; background:#FFFFFF; border:1px solid #E4E7EC; border-radius:12px; padding:8px 10px; overflow:hidden; }
.diagram-svg-wrap svg { display:block; width:100%; height:auto; min-height:0; max-height:330px; }
.rationale-box { font-size:14px !important; line-height:1.55 !important; }
.support-state { font-size:14px !important; font-weight:800 !important; }
.support-detail { font-size:12.5px !important; line-height:1.45 !important; }
@media(max-width:850px) { .site-context-grid,.diagram-explain-grid { grid-template-columns:1fr; } }

"
css <- gsub("__BRAND__", brand_burgundy, css, fixed = TRUE)

ui <- navbarPage(
  title = "Interstate Bicyclist Crash Explorer",
  inverse = TRUE,
  header = tagList(
    tags$head(
      tags$style(HTML(css))
    )
  ),

  tabPanel(
    "Crash + Street View",
    div(class = "metric-row",
        metric_box(n_total, "Analytical records"),
        metric_box(n_map, "Mapped records"),
        metric_box(n_dark, "Dark"),
        metric_box(n_day, "Daylight")),

    fluidRow(
      column(
        8,
        div(
          class = "section-card",
          div(class = "card-title", "Spatial distribution"),
          div(class = "subtle",
              "Each point is one mappable fatal-crash record. The spatial view shows lighting only: Dark or Daylight."),
          radioButtons(
            "map_lighting",
            NULL,
            choices = c("All" = "All", "Dark" = "Dark", "Daylight" = "Daylight"),
            selected = "All",
            inline = TRUE
          ),
          leafletOutput("crash_map", height = 610)
        )
      ),
      column(
        4,
        div(
          class = "section-card",
          div(class = "card-title", "Selected crash"),
          selectizeInput(
            "record_select",
            NULL,
            choices = setNames(dat$Record_ID, paste0(dat$Crash_ID, " | ", dat$Record_ID)),
            selected = dat$Record_ID[1],
            options = list(maxOptions = 130)
          ),
          uiOutput("crash_record_panel")
        ),
        div(
          class = "section-card",
          div(class = "card-title", "Google Street View + roadway context"),
          div(class = "subtle",
              "Mapped roadway tags are shown only as review hints. Lane count and divided/undivided status are never treated as ground truth. Review Street View, confirm the roadway geometry below, then confirm or edit the other visible conditions."),
          uiOutput("street_view_panel"),
          uiOutput("site_context_summary"),
          selectInput(
            "road_geometry_confirmed",
            "Confirmed roadway geometry",
            choices = ROAD_GEOMETRY_CHOICES,
            selected = "unknown"
          ),
          div(class="subtle", style="margin-top:-7px;margin-bottom:8px;",
              "Confirm from Street View/site review. A validated value in data/site_context_validated.csv is loaded automatically; mapped tags are not auto-applied."),
          actionButton("apply_auto_site", "Reapply automatic condition suggestions", class = "btn btn-primary", style="margin-bottom:8px;"),
          checkboxGroupInput(
            "site_obs",
            "Confirmed / user-edited roadway conditions",
            choices = c(
              "Ramp / freeway-entry context" = "ramp",
              "High-speed roadway geometry" = "highspeed",
              "Limited shoulder / recovery space" = "limited_shoulder",
              "Complex merge / turning context" = "merge",
              "Limited roadway lighting" = "low_light",
              "Unclear / insufficient bicycle route guidance" = "route_guidance",
              "No evident parallel bicycle facility" = "no_parallel"
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "CCA Explorer",
    tabsetPanel(
      id = "cca_mode",

      tabPanel(
        "Study CCA Profiles",
        div(
          class = "study-layout",
          div(
            class = "section-card",
            div(class = "card-title", "Study cohort"),
            selectInput(
              "study_cohort",
              NULL,
              choices = c("Dark", "Daylight"),
              selected = "Dark"
            ),
            uiOutput("study_configuration"),
            hr(),
            selectInput(
              "study_cluster",
              "Impact cluster",
              choices = paste0("C", 1:4),
              selected = "C1"
            ),
            uiOutput("study_cluster_summary")
          ),
          div(
            div(
              class = "study-right-top",
              div(
                class = "section-card",
                div(class = "card-title", "Elbow method"),
                div(class = "subtle", "Manuscript figure. K = 4 is used for the automated Study CCA."),
                uiOutput("study_elbow")
              ),
              div(
                class = "section-card",
                div(class = "card-title", "CCA configuration map"),
                div(class = "subtle", "Validated study output used in the manuscript."),
                uiOutput("study_cca_map")
              )
            ),
            div(
              class = "study-right-bottom",
              div(
                class = "section-card",
                div(class = "card-title", "Cluster shares"),
                tableOutput("study_cluster_shares")
              ),
              div(
                class = "section-card",
                div(class = "card-title", "Cluster image"),
                div(class = "subtle", "Validated standardized-residual cluster profile used in the manuscript."),
                uiOutput("study_cluster_image")
              )
            )
          )
        ),
        div(
          class = "section-card",
          div(class = "card-title", "SHAP profile for selected cluster"),
          div(class = "subtle",
              "Validated SHAP output generated by the manuscript workflow: Python RandomForestClassifier (100 trees, random_state = 42) with shap.TreeExplainer and LabelEncoder."),
          uiOutput("study_shap")
        )
      ),

      tabPanel(
        "Custom CCA",
        fluidRow(
          column(
            4,
            div(
              class = "section-card",
              div(class = "card-title", "Custom analysis controls"),
              selectInput(
                "custom_cohort",
                "Analysis cohort",
                choices = c("Dark", "Daylight", "All records"),
                selected = "Dark"
              ),
              selectizeInput(
                "custom_vars",
                "CCA variables",
                choices = STUDY_VARS,
                selected = STUDY_VARS,
                multiple = TRUE,
                options = list(plugins = list("remove_button"))
              ),
              selectizeInput(
                "custom_excluded_levels",
                "Excluded levels",
                choices = c(
                  "Not Reported","Others","Other","None","Not Applicable",
                  "Not applicable","Unknown"
                ),
                selected = character(0),
                multiple = TRUE,
                options = list(create = TRUE, plugins = list("remove_button"))
              ),
              hr(),
              selectizeInput(
                "custom_year",
                "Year",
                choices = sort(unique(dat$Year)),
                selected = sort(unique(dat$Year)),
                multiple = TRUE
              ),
              selectizeInput(
                "custom_land",
                "Urban / Rural",
                choices = sort(unique(safe_chr(dat[["Land Type"]]))),
                selected = sort(unique(safe_chr(dat[["Land Type"]]))),
                multiple = TRUE
              ),
              selectizeInput(
                "custom_crash_type",
                "Crash type",
                choices = sort(unique(safe_chr(dat[["Bike Crash Type"]]))),
                selected = sort(unique(safe_chr(dat[["Bike Crash Type"]]))),
                multiple = TRUE
              ),
              selectizeInput(
                "custom_bike_location",
                "Bicycle location",
                choices = sort(unique(safe_chr(dat[["Bike Location"]]))),
                selected = sort(unique(safe_chr(dat[["Bike Location"]]))),
                multiple = TRUE
              ),
              selectizeInput(
                "custom_vehicle",
                "Motor vehicle",
                choices = sort(unique(safe_chr(dat[["MV Type"]]))),
                selected = sort(unique(safe_chr(dat[["MV Type"]]))),
                multiple = TRUE
              ),
              selectizeInput(
                "custom_speeding",
                "Speeding related",
                choices = sort(unique(safe_chr(dat[["Speeding Related"]]))),
                selected = sort(unique(safe_chr(dat[["Speeding Related"]]))),
                multiple = TRUE
              ),
              numericInput("custom_k", "Number of clusters (K)", value = 4, min = 2, max = 8),
              checkboxInput("custom_show_categories", "Show category labels", TRUE),
              actionButton("run_custom", "Run Custom CCA", class = "btn-primary")
            )
          ),
          column(
            8,
            fluidRow(
              column(
                7,
                div(
                  class = "section-card",
                  div(class = "card-title", "Custom CCA map"),
                  uiOutput("custom_status"),
                  plotOutput("custom_cca_map", height = 390)
                )
              ),
              column(
                5,
                div(
                  class = "section-card",
                  div(class = "card-title", "Cluster shares"),
                  tableOutput("custom_cluster_shares"),
                  selectInput("custom_cluster", "Cluster image", choices = "C1")
                )
              )
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "section-card",
                  div(class = "card-title", "Custom cluster image"),
                  div(class = "subtle", "Generated from the user-selected Custom CCA settings."),
                  plotOutput("custom_cluster_image", height = 360)
                )
              )
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Mechanism + Countermeasure",
    div(
      class = "section-card",
      div(class = "card-title", "Why the selected crash belongs to its CCA cluster"),
      div(class = "subtle",
          "The hard cluster assignment comes directly from clusmca(). The percentages below explain the assignment using CCA geometry and cluster-category residuals; they are not crash-risk probabilities."),
      uiOutput("selected_cluster_banner"),
      uiOutput("cluster_membership_kpis"),
      div(
        class = "membership-grid",
        div(plotOutput("cluster_proximity_plot", height = 360)),
        div(plotOutput("cluster_attribute_alignment_plot", height = 360))
      ),
      uiOutput("cluster_membership_text")
    ),

    div(
      class = "section-card",
      div(class = "card-title", "Proposed countermeasure for the selected crash"),
      div(class = "subtle",
          "Street View provides the physical site context on the left. The flowchart on the right combines the selected crash record, Study CCA, validated SHAP, and confirmed site review into one intervention pathway. No crash-reduction probability is estimated."),
      div(
        class = "site-and-flow-grid",
        uiOutput("countermeasure_site_preview"),
        uiOutput("countermeasure_flowchart")
      ),
      uiOutput("private_api_control"),
      uiOutput("private_api_rationale"),
      uiOutput("countermeasure_text")
    )
  ),

  tabPanel(
    "About",
    div(
      class = "section-card",
      div(class = "card-title", "About the analytical tool"),
      tags$p(
        "The application separates validated Study CCA profiles from Custom CCA. Dark and Daylight study analyses remain independent. ",
        "Study CCA maps, standardized-residual cluster profiles, and SHAP figures are the validated outputs used in the manuscript. Custom CCA is the live analysis workspace for user-selected variables, filters, exclusions, and K."
      ),
      tags$p(
        "For single-crash diagnosis, FARS/PBCAT fields remain clearly separated from automatically suggested and user-confirmed roadway/site observations. ",
        "Google Street View is opened with the no-key preview/link. A separately configured private API may optionally refine the written case explanation, but the app does not depend on it. For each selected crash, only the primary CCA-linked countermeasure is shown after the exact Study CCA cluster and selected-case evidence are established."
      ),
      tags$p(
        tags$b("Spatial coverage: "),
        paste0(n_total, " analytical records; ", n_map, " records with valid map coordinates.")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================

server <- function(input, output, session) {

  # ----------------------------------------------------------
  # Selected crash
  # ----------------------------------------------------------

  selected_record <- reactiveVal(dat$Record_ID[1])

  observeEvent(input$record_select, {
    req(input$record_select)
    selected_record(input$record_select)
  }, ignoreInit = TRUE)

  selected_row <- reactive({
    i <- match(selected_record(), dat$Record_ID)
    req(!is.na(i))
    dat[i, , drop = FALSE]
  })


  # ----------------------------------------------------------
  # Spatial map - Dark vs Daylight only
  # ----------------------------------------------------------

  output$crash_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 3)) |>
      addProviderTiles(providers$OpenStreetMap.Mapnik) |>
      addLegend(
        position = "bottomright",
        colors = unname(lighting_colors),
        labels = names(lighting_colors),
        title = "Lighting condition",
        opacity = 0.9
      ) |>
      setView(lng = -98.5, lat = 39.2, zoom = 4)
  })

  observe({
    d <- dat[dat$Mappable, , drop = FALSE]
    if (!is.null(input$map_lighting) && input$map_lighting != "All") {
      d <- d[safe_chr(d[["Lighting Condition"]]) == input$map_lighting, , drop = FALSE]
    }

    cols <- unname(lighting_colors[safe_chr(d[["Lighting Condition"]])])
    cols[is.na(cols)] <- "#667085"

    popup <- vapply(seq_len(nrow(d)), function(i) {
      r <- d[i, , drop = FALSE]
      paste0(
        "<b>FARS crash:</b> ", htmltools::htmlEscape(r$Crash_ID), "<br>",
        "<b>Record:</b> ", htmltools::htmlEscape(r$Record_ID), "<br>",
        "<b>Lighting:</b> ", htmltools::htmlEscape(safe_chr(r[["Lighting Condition"]])), "<br>",
        "<b>PBCAT type:</b> ", htmltools::htmlEscape(safe_chr(r[["Bike Crash Type"]])), "<br>",
        "<b>Bicycle location:</b> ", htmltools::htmlEscape(safe_chr(r[["Bike Location"]]))
      )
    }, character(1))

    leafletProxy("crash_map", data = d) |>
      clearMarkers() |>
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        layerId = ~Record_ID,
        radius = 6,
        color = "#FFFFFF",
        weight = 1,
        fillColor = cols,
        fillOpacity = 0.9,
        popup = popup
      )
  })

  observeEvent(input$crash_map_marker_click, {
    id <- input$crash_map_marker_click$id
    if (!is.null(id) && id %in% dat$Record_ID) {
      selected_record(id)
      updateSelectizeInput(session, "record_select", selected = id)
    }
  })

  output$crash_record_panel <- renderUI({
    r <- selected_row()
    div(
      div(class = "source-badge", "Crash database: FARS / PBCAT"),
      tags$h4(style = "margin-top:0;color:#203864;font-weight:800;",
              paste0("FARS ", r$Crash_ID)),
      div(class = "subtle", paste0("Record ", r$Record_ID, " | ", safe_chr(r[["Lighting Condition"]]))),
      br(),
      div(
        class = "data-grid",
        data_field("PBCAT crash type", r[["Bike Crash Type"]]),
        data_field("Bicycle location", r[["Bike Location"]]),
        data_field("Bicycle direction", r[["Bicyclist Direction"]]),
        data_field("Motor vehicle", r[["MV Type"]]),
        data_field("Speeding related", r[["Speeding Related"]]),
        data_field("Avoidance", r[["Attempted for Avoidance"]]),
        data_field("Prior critical event", r[["Prior Critical Event"]]),
        data_field("Driver factor", r[["Driver Related Factor"]])
      )
    )
  })

  site_context <- eventReactive(selected_record(), {
    r <- selected_row()
    if (!isTRUE(r$Mappable[1])) {
      return(list(available=FALSE, suggestions=character(0), roadway_form="Location unavailable", lane_summary="No valid coordinates", source="Crash record"))
    }
    fetch_osm_road_context(r$Latitude[1], r$Longitude[1])
  }, ignoreInit = FALSE)

  observeEvent(site_context(), {
    ctx <- site_context()
    updateCheckboxGroupInput(session, "site_obs", selected = ctx$suggestions %||% character(0))
  }, ignoreInit = FALSE)

  observeEvent(selected_record(), {
    vr <- validated_site_row(selected_record())
    code <- "unknown"
    if (!is.null(vr) && nrow(vr) > 0 && "review_status" %in% names(vr) &&
        tolower(trimws(as.character(vr$review_status[1]))) == "validated" &&
        "roadway_geometry_code" %in% names(vr)) {
      candidate <- trimws(as.character(vr$roadway_geometry_code[1]))
      if (candidate %in% unname(ROAD_GEOMETRY_CHOICES)) code <- candidate
    }
    updateSelectInput(session, "road_geometry_confirmed", selected = code)
  }, ignoreInit = FALSE)

  diagram_context <- reactive({
    mapped <- site_context()
    code <- input$road_geometry_confirmed %||% "unknown"
    vr <- validated_site_row(selected_record())
    source_label <- "User-confirmed Street View/site review"
    if (!is.null(vr) && nrow(vr) > 0 && "review_status" %in% names(vr) &&
        tolower(trimws(as.character(vr$review_status[1]))) == "validated" &&
        "roadway_geometry_code" %in% names(vr) &&
        trimws(as.character(vr$roadway_geometry_code[1])) == code && code != "unknown") {
      source_label <- "Validated site-context table"
    } else if (code == "unknown") {
      source_label <- "Site review pending"
    }
    geometry_context_from_code(code, mapped_ctx = mapped, source_label = source_label)
  })

  observeEvent(input$apply_auto_site, {
    ctx <- site_context()
    updateCheckboxGroupInput(session, "site_obs", selected = ctx$suggestions %||% character(0))
  })

  output$street_view_panel <- renderUI({
    r <- selected_row()
    if (!isTRUE(r$Mappable[1])) {
      return(div(class = "subtle", "No valid latitude/longitude is available for this analytical record."))
    }
    lat <- r$Latitude[1]; lon <- r$Longitude[1]
    full_url <- sprintf("https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=%.7f,%.7f", lat, lon)
    embed_url <- sprintf("https://maps.google.com/maps?layer=c&cbll=%.7f,%.7f&cbp=11,0,0,0,0&source=embed&output=svembed", lat, lon)
    tagList(
      tags$iframe(class="street-frame", src=embed_url, loading="lazy", allowfullscreen="allowfullscreen"),
      tags$a(href=full_url, target="_blank", class="btn btn-warning", style="margin-top:8px;", "Open full Google Street View")
    )
  })

  output$site_context_summary <- renderUI({
    mapped <- site_context()
    dctx <- diagram_context()
    sugg <- mapped$suggestions %||% character(0)
    labels <- c(ramp="Ramp/freeway-entry", highspeed="High-speed geometry", limited_shoulder="Limited shoulder", merge="Merge/turning complexity", low_light="Limited roadway lighting", route_guidance="Route-guidance concern", no_parallel="No evident parallel bicycle facility")
    sugg_txt <- if (length(sugg)) paste(unname(labels[sugg]), collapse=", ") else "No additional condition could be suggested automatically. Review Street View manually."
    div(
      div(class="site-context-grid",
          div(class="site-context-item", tags$b("Mapped roadway"), span(roadway_display_name(mapped))),
          div(class="site-context-item", tags$b("Mapped class hint"), span(mapped$highway %||% "Not mapped")),
          div(class="site-context-item", tags$b("Mapped lanes hint"), span(mapped$lanes %||% "Not mapped")),
          div(class="site-context-item", tags$b("Mapped one-way hint"), span(mapped$oneway %||% "Not mapped")),
          div(class="site-context-item", tags$b("Diagram geometry"), span(if (isTRUE(dctx$geometry_confirmed)) paste0(dctx$roadway_form, " - ", dctx$lane_summary) else "Not yet confirmed")),
          div(class="site-context-item", tags$b("Diagram context source"), span(dctx$source %||% "Site review pending"))
      ),
      div(class="subtle", tags$b("Automatic condition suggestions: "), sugg_txt,
          " Mapped lane/divided status is not used for the crash diagram until the site geometry is confirmed or validated.")
    )
  })


  # ----------------------------------------------------------
  # Study CCA Profiles
  # ----------------------------------------------------------

  study_obj <- reactive(STUDY_CCA[[input$study_cohort]])

  output$study_configuration <- renderUI({
    obj <- study_obj()
    light <- input$study_cohort
    div(
      class = "subtle",
      tags$b("Study configuration"),
      tags$ul(
        tags$li(paste0("Cohort: ", light)),
        tags$li(paste0("Records used: ", nrow(obj$data))),
        tags$li(paste0("Retained CCA variables: ", length(obj$variables))),
        tags$li("CCA method: clusCA"),
        tags$li("Dimensions: 2"),
        tags$li("Clusters: K = 4"),
        tags$li("Random starts: 10")
      )
    )
  })

  output$study_cluster_summary <- renderUI({
    light <- input$study_cohort
    cl <- input$study_cluster
    meta <- study_cluster_meta[
      study_cluster_meta$Lighting == light &
        study_cluster_meta$Cluster == cl, , drop = FALSE
    ]
    obj <- study_obj()
    j <- as.integer(sub("C","",cl))

    div(
      class = "subtle",
      tags$b(paste0(light, " ", cl, " - ", meta$Title)),
      tags$p(
        paste0(
          "Study cluster: ", meta$Manuscript_N, " crashes (",
          sprintf("%.1f", meta$Manuscript_Share), "%)."
        )
      )
    )
  })

  output$study_elbow <- renderUI({
    f <- if (input$study_cohort == "Dark") "study_dark_elbow.png" else "study_day_elbow.png"
    tags$a(href=f, target="_blank", tags$img(src = f, class="study-plot"))
  })

  output$study_cca_map <- renderUI({
    f <- if (input$study_cohort == "Dark") "study_dark_cca.png" else "study_day_cca.png"
    tags$a(href=f, target="_blank", tags$img(src = f, class="study-plot"))
  })

  output$study_cluster_shares <- renderTable({
    light <- input$study_cohort
    d <- study_cluster_meta[study_cluster_meta$Lighting == light, c("Cluster","Manuscript_N","Manuscript_Share","Title")]
    names(d) <- c("Cluster","N","Share (%)","Pattern")
    d
  }, striped = TRUE, bordered = FALSE, spacing = "xs")

  output$study_cluster_image <- renderUI({
    light <- if (input$study_cohort == "Dark") "dark" else "day"
    f <- paste0("study_", light, "_", tolower(input$study_cluster), ".png")
    tags$a(href=f, target="_blank", tags$img(src = f, class="study-plot"))
  })

  output$study_shap <- renderUI({
    light <- if (input$study_cohort == "Dark") "dark" else "day"
    f <- paste0("study_", light, "_shap_", tolower(input$study_cluster), ".png")
    tags$a(href=f, target="_blank", tags$img(src = f, class="study-plot"))
  })

  # ----------------------------------------------------------
  # Custom CCA
  # ----------------------------------------------------------

  custom_result <- eventReactive(input$run_custom, {
    vars <- input$custom_vars
    validate(need(length(vars) >= 2, "Select at least two CCA variables."))

    d <- dat

    if (input$custom_cohort == "Dark") {
      d <- d[norm_chr(d[["Lighting Condition"]]) == "dark", , drop = FALSE]
    } else if (input$custom_cohort == "Daylight") {
      d <- d[norm_chr(d[["Lighting Condition"]]) == "daylight", , drop = FALSE]
    }

    if (length(input$custom_year) > 0) {
      d <- d[d$Year %in% input$custom_year, , drop = FALSE]
    }
    if (length(input$custom_land) > 0) {
      d <- d[safe_chr(d[["Land Type"]]) %in% input$custom_land, , drop = FALSE]
    }
    if (length(input$custom_crash_type) > 0) {
      d <- d[safe_chr(d[["Bike Crash Type"]]) %in% input$custom_crash_type, , drop = FALSE]
    }
    if (length(input$custom_bike_location) > 0) {
      d <- d[safe_chr(d[["Bike Location"]]) %in% input$custom_bike_location, , drop = FALSE]
    }
    if (length(input$custom_vehicle) > 0) {
      d <- d[safe_chr(d[["MV Type"]]) %in% input$custom_vehicle, , drop = FALSE]
    }
    if (length(input$custom_speeding) > 0) {
      d <- d[safe_chr(d[["Speeding Related"]]) %in% input$custom_speeding, , drop = FALSE]
    }

    excluded <- trimws(tolower(input$custom_excluded_levels %||% character(0)))
    excluded <- excluded[excluded != ""]

    if (length(excluded) > 0 && nrow(d) > 0) {
      check_vars <- intersect(vars, names(d))
      bad <- Reduce(
        `|`,
        lapply(d[, check_vars, drop = FALSE], function(x) {
          trimws(tolower(safe_chr(x, ""))) %in% excluded
        })
      )
      d <- d[!bad, , drop = FALSE]
    }

    validate(need(nrow(d) >= input$custom_k * 2, "Too few records remain after filters/exclusions."))
    validate(need(input$custom_k < nrow(d), "K must be smaller than the number of remaining records."))

    obj <- run_cca(
      d,
      vars = vars,
      k = input$custom_k,
      study_light = NULL
    )
    list(
      obj = obj,
      n = nrow(d),
      cohort = input$custom_cohort
    )
  }, ignoreInit = TRUE)

  observeEvent(custom_result(), {
    x <- custom_result()
    choices <- paste0("C", seq_len(x$obj$k))
    updateSelectInput(session, "custom_cluster", choices = choices, selected = choices[1])
  })

  output$custom_status <- renderUI({
    if (input$run_custom == 0) {
      return(div(class = "subtle", "Choose settings and click Run Custom CCA."))
    }
    x <- custom_result()
    div(
      class = "subtle",
      paste0(
        x$n, " records | ",
        length(x$obj$variables), " retained variables | K = ", x$obj$k
      )
    )
  })

  output$custom_cca_map <- renderPlot({
    req(custom_result())
    plot_cca_map(
      custom_result()$obj,
      show_categories = isTRUE(input$custom_show_categories)
    )
  })

  output$custom_cluster_shares <- renderTable({
    req(custom_result())
    cluster_share_table(custom_result()$obj, NULL)
  }, striped = TRUE, spacing = "xs")

  output$custom_cluster_image <- renderPlot({
    req(custom_result(), input$custom_cluster)
    plot_cluster_residual(custom_result()$obj, input$custom_cluster, top_n = 18)
  })


  # ----------------------------------------------------------
  # Selected crash analytical evidence
  # ----------------------------------------------------------

  selected_assignment <- reactive({
    get_study_assignment(selected_row()$.row_index[1])
  })


  selected_membership_evidence <- reactive({
    a <- selected_assignment()
    req(a)
    cca_membership_evidence(a, selected_row()$.row_index[1])
  })

  output$cluster_membership_kpis <- renderUI({
    a <- selected_assignment()
    e <- selected_membership_evidence()
    req(a, e)
    div(
      class = "membership-kpis",
      div(class = "membership-kpi",
          div(class = "kpi-big", paste0(a$light, " ", a$cluster)),
          div(class = "kpi-small", "Exact study-cluster assignment")),
      div(class = "membership-kpi",
          div(class = "kpi-big", paste0(sprintf("%.1f", e$assigned_proximity), "%")),
          div(class = "kpi-small", "Relative CCA proximity")),
      div(class = "membership-kpi",
          div(class = "kpi-big", ifelse(is.na(e$alignment), "NA", paste0(sprintf("%.1f", e$alignment), "%"))),
          div(class = "kpi-small", paste0("Residual profile alignment · ", e$supportive_n, "/", e$available_n, " supportive attributes")))
    )
  })

  output$cluster_proximity_plot <- renderPlot({
    a <- selected_assignment()
    e <- selected_membership_evidence()
    req(a, e)
    d <- e$centroids
    d$Cluster <- factor(d$Cluster, levels = rev(paste0("C", 1:4)))
    d$Fill <- ifelse(d$Assigned, "Assigned cluster", "Other cluster")
    ggplot(d, aes(RelativeProximity, Cluster, fill = Fill)) +
      geom_col(width = 0.68) +
      geom_text(aes(label = paste0(sprintf("%.1f", RelativeProximity), "%")), hjust = -0.08, size = 4.7, fontface = "bold") +
      scale_fill_manual(values = c("Assigned cluster" = "#D95F0E", "Other cluster" = "#CBD5E1")) +
      coord_cartesian(xlim = c(0, max(55, max(d$RelativeProximity) * 1.18))) +
      labs(title = "Relative position in CCA space", subtitle = "Inverse distance to each cluster centroid; sums to 100%", x = "Relative CCA proximity", y = NULL, fill = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top", legend.text=element_text(size=12,face="bold"), axis.text=element_text(size=12,face="bold"), axis.title=element_text(size=13,face="bold"), panel.grid.major.y = element_blank(), plot.title = element_text(face="bold",size=16), plot.subtitle=element_text(size=12,face="bold"))
  })

  output$cluster_attribute_alignment_plot <- renderPlot({
    a <- selected_assignment()
    e <- selected_membership_evidence()
    req(a, e)
    d <- head(e$profile, 10)
    if (nrow(d) == 0) return(ggplot() + theme_void())
    d$Label <- paste0(d$Variable, " = ", d$Value)
    d$Label <- factor(d$Label, levels = rev(d$Label))
    d$Direction <- ifelse(d$Residual > 0, "Supports assigned cluster", "Opposes / weakens")
    ggplot(d, aes(Residual, Label, fill = Direction)) +
      geom_col(width = 0.68) +
      geom_vline(xintercept = 0, color = "#475467", linewidth = 0.35) +
      scale_fill_manual(values = c("Supports assigned cluster"="#7F2704", "Opposes / weakens"="#B8C4D1")) +
      labs(title = "Why the crash profile fits the cluster", subtitle = "Selected crash attributes ranked by cluster standardized residual", x = "Standardized residual", y = NULL, fill = NULL) +
      theme_minimal(base_size = 13.5) +
      theme(legend.position = "top", legend.text=element_text(size=11.5,face="bold"), axis.text.x=element_text(size=11.5,face="bold"), axis.title.x=element_text(size=12.5,face="bold"), panel.grid.major.y = element_blank(), axis.text.y = element_text(size=10.5,face="bold"), plot.title = element_text(face="bold",size=15.5), plot.subtitle=element_text(size=11.5,face="bold"))
  })

  output$cluster_membership_text <- renderUI({
    a <- selected_assignment()
    e <- selected_membership_evidence()
    req(a, e)
    sup <- e$profile[e$profile$Residual > 0, , drop = FALSE]
    sup <- head(sup, 5)
    attrs <- if (nrow(sup) == 0) "No positive residual attributes were identified." else paste(paste0(sup$Variable, " = ", sup$Value), collapse = "; ")
    div(
      class = "explain-note",
      tags$b("How to read the result: "),
      paste0(
        "clusmca assigns the crash directly to ", a$light, " ", a$cluster, ". ",
        "The proximity percentage summarizes where the selected observation sits relative to all four cluster centroids. ",
        "The profile-alignment percentage summarizes whether the crash's own category levels are over-represented in the assigned cluster. ",
        "Strongest supportive attributes: ", attrs, "."
      )
    )
  })

  selected_local_shap <- reactive({
    a <- selected_assignment()
    req(a)
    key <- paste(a$light, a$cluster)
    feats <- study_shap_features[[key]] %||% character(0)
    if (length(feats) == 0) return(NULL)
    r <- selected_row()
    data.frame(
      Feature = feats,
      Value = vapply(feats, function(v) if (v %in% names(r)) safe_chr(r[[v]]) else "Not available", character(1)),
      SHAP = NA_real_,
      Label = feats,
      stringsAsFactors = FALSE
    )
  })

  selected_shap_features <- reactive({
    a <- selected_assignment()
    req(a)
    study_shap_features[[paste(a$light, a$cluster)]] %||% character(0)
  })

  mechanism_defs <- reactive({
    mechanism_definitions(
      selected_row(),
      selected_assignment(),
      selected_shap_features(),
      input$site_obs %||% character(0)
    )
  })

  mechanism_table <- reactive({
    mechanism_score_table(mechanism_defs())
  })

  primary_mechanism_key <- reactive({
    d <- mechanism_table()
    d$Key[which.max(d$Support)]
  })

  countermeasure_table <- reactive({
    countermeasure_scores(
      selected_row(),
      selected_assignment(),
      selected_shap_features(),
      input$site_obs %||% character(0),
      primary_mechanism_key()
    )
  })

  primary_countermeasure_key <- reactive({
    d <- countermeasure_table()
    d$Key[which.max(d$Applicability)]
  })

  output$mechanism_support_plot <- renderPlot({
    d <- mechanism_table()
    d$Mechanism <- factor(d$Mechanism, levels = rev(d$Mechanism))

    ggplot(d, aes(Support, Mechanism)) +
      geom_col(fill = "#315A86", width = 0.68) +
      geom_text(
        aes(label = paste0(Support, "%  (", Matched, "/", Total, ")")),
        hjust = -0.08, size = 3.5, fontface = "bold"
      ) +
      coord_cartesian(xlim = c(0, 108)) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "Matched evidence criteria", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank())
  })

  output$countermeasure_plot <- renderPlot({
    d <- countermeasure_table()
    d$Countermeasure <- factor(d$Countermeasure, levels = rev(d$Countermeasure))

    ggplot(d, aes(Applicability, Countermeasure)) +
      geom_col(fill = "#D95F0E", width = 0.68) +
      geom_text(
        aes(label = paste0(Applicability, "%  (", Matched, "/", Total, ")")),
        hjust = -0.08, size = 3.5, fontface = "bold"
      ) +
      coord_cartesian(xlim = c(0, 108)) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      labs(x = "Case applicability", y = NULL) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.y = element_blank())
  })

  output$selected_cluster_banner <- renderUI({
    a <- selected_assignment()
    req(a)
    div(
      class = "selected-cluster-banner",
      div(class = "selected-cluster-label", "Exact Study CCA membership for the selected crash"),
      div(class = "selected-cluster-main", paste0(a$light, " ", a$cluster, " - ", a$title)),
      div(
        class = "selected-cluster-sub",
        paste0(
          "Cluster prevalence: ", a$count, " of ", nrow(a$obj$data), " ",
          tolower(a$light), " crashes (", sprintf("%.1f", a$share), "%)."
        )
      )
    )
  })

  output$evidence_source_cards <- renderUI({
    r <- selected_row()
    a <- selected_assignment()
    shap_feats <- selected_shap_features()

    site_items <- input$site_obs %||% character(0)
    site_labels <- c(
      ramp = "Ramp / freeway-entry context",
      highspeed = "High-speed roadway geometry",
      limited_shoulder = "Limited shoulder / recovery space",
      merge = "Complex merge / turning context",
      low_light = "Limited roadway lighting",
      route_guidance = "Unclear / insufficient bicycle route guidance",
      no_parallel = "No evident parallel bicycle facility"
    )

    shap_items <- if (length(shap_feats) == 0) {
      list(tags$li("No manuscript SHAP highlight is available for this cluster."))
    } else {
      lapply(head(shap_feats, 4), function(v) {
        val <- if (v %in% names(r)) safe_chr(r[[v]]) else "Not available"
        tags$li(paste0(v, ": ", val))
      })
    }

    site_html <- if (length(site_items) == 0) {
      list(tags$li("Review Google Street View and mark visible roadway conditions on the Crash + Street View tab."))
    } else {
      lapply(site_items, function(x) tags$li(site_labels[[x]]))
    }

    div(
      class = "analysis-strip",
      div(
        class = "analysis-card",
        div(class = "source-badge", "1. FARS / PBCAT crash record"),
        tags$h4(safe_chr(r[["Bike Crash Type"]])),
        tags$ul(
          tags$li(paste0("Bicycle location: ", safe_chr(r[["Bike Location"]]))),
          tags$li(paste0("Direction: ", safe_chr(r[["Bicyclist Direction"]]))),
          tags$li(paste0("Avoidance: ", safe_chr(r[["Attempted for Avoidance"]]))),
          tags$li(paste0("Speeding: ", safe_chr(r[["Speeding Related"]])))
        )
      ),
      div(
        class = "analysis-card",
        div(class = "source-badge", "2. Study CCA"),
        tags$h4(paste0(a$light, " ", a$cluster, " · ", a$title)),
        tags$ul(
          tags$li(paste0("Cluster prevalence: ", a$count, "/", nrow(a$obj$data), " (", sprintf("%.1f", a$share), "%)")),
          tags$li("Dark and Daylight are analyzed in separate four-cluster CCA models."),
          tags$li("See the validated manuscript CCA map and cluster image on the CCA Explorer tab.")
        )
      ),
      div(
        class = "analysis-card",
        div(class = "source-badge", "3. Validated manuscript SHAP"),
        tags$h4("Cluster-level SHAP highlights"),
        tags$ul(shap_items)
      ),
      div(
        class = "analysis-card",
        div(class = "source-badge", "4. Google Street View / site review"),
        tags$h4("Visible roadway context"),
        tags$ul(site_html)
      )
    )
  })

  output$visual_pathway <- renderUI({
    r <- selected_row()
    a <- selected_assignment()
    shap_feats <- selected_shap_features()
    shap_txt <- if (length(shap_feats) == 0) "No SHAP highlight available" else paste(head(shap_feats, 2), collapse = " + ")
    mechanism <- cluster_mechanism_summary(a$light, a$cluster, r, input$site_obs %||% character(0))

    div(
      class = "case-flow-wrap",
      div(
        class = "flow-grid-v2",
        div(class = "flow-node-v2",
            div(class = "flow-kicker", "Crash record"),
            div(class = "flow-title", safe_chr(r[["Bike Crash Type"]])),
            div(class = "flow-text", paste0(safe_chr(r[["Bike Location"]]), " · ", safe_chr(r[["Attempted for Avoidance"]])))),
        div(class = "flow-arrow-v2", "→"),
        div(class = "flow-node-v2",
            div(class = "flow-kicker", "Study CCA"),
            div(class = "flow-title", paste0(a$light, " ", a$cluster)),
            div(class = "flow-text", paste0(a$title, " · ", sprintf("%.1f", a$share), "% of ", a$light, " crashes"))),
        div(class = "flow-arrow-v2", "→"),
        div(class = "flow-node-v2",
            div(class = "flow-kicker", "SHAP"),
            div(class = "flow-title", shap_txt),
            div(class = "flow-text", "Key features highlighted by the validated manuscript SHAP for the selected cluster")),
        div(class = "flow-arrow-v2", "→"),
        div(class = "flow-node-v2",
            div(class = "flow-kicker", "Street View"),
            div(class = "flow-title", ifelse(length(input$site_obs %||% character(0)) > 0, paste(length(input$site_obs), "visible conditions marked"), "Site review pending")),
            div(class = "flow-text", "Physical roadway conditions remain visually verified rather than inferred from FARS"))
      ),
      div(class = "mechanism-arrow", "↓"),
      div(
        class = "mechanism-box-v2",
        div(class = "flow-kicker", "Merged crash mechanism"),
        div(class = "mechanism-title-v2", a$title),
        div(class = "mechanism-text-v2", mechanism)
      )
    )
  })


  selected_policy_key <- reactive({
    a <- selected_assignment()
    req(a)
    choose_primary_countermeasure(a, selected_row(), input$site_obs %||% character(0))
  })

  selected_policy_support <- reactive({
    a <- selected_assignment()
    req(a)
    countermeasure_evidence_support(
      selected_policy_key(), a, selected_row(), input$site_obs %||% character(0), selected_shap_features()
    )
  })

  output$countermeasure_support_summary <- renderUI({
    a <- selected_assignment()
    key <- selected_policy_key()
    p <- countermeasure_library[[key]]
    ev <- selected_policy_support()
    r <- selected_row()
    ctx <- diagram_context()
    shap_features <- selected_shap_features()
    site <- input$site_obs %||% character(0)
    req(a, p, ev)

    site_labels <- c(
      ramp="ramp/freeway-entry context",
      highspeed="high-speed geometry",
      limited_shoulder="limited shoulder/recovery space",
      merge="merge/turning complexity",
      low_light="limited roadway lighting",
      route_guidance="route-guidance concern",
      no_parallel="no evident parallel bicycle facility"
    )

    detail_for <- function(source) {
      if (source == "CCA cluster linkage") {
        return(cca_policy_link_text(key, a))
      }
      if (source == "Crash-record evidence") {
        return(paste0("FARS/PBCAT: crash type = ", safe_chr(r[["Bike Crash Type"]]),
                      "; bicycle location = ", safe_chr(r[["Bike Location"]]),
                      "; avoidance = ", safe_chr(r[["Attempted for Avoidance"]]), "."))
      }
      if (source == "Validated SHAP") {
        if (length(shap_features)) {
          return(paste0("Validated ", a$light, " ", a$cluster, " SHAP profile highlights ",
                        paste(head(shap_features, 4), collapse = ", "), ". These variables describe the cluster; only case-relevant features are used to interpret the intervention pathway."))
        }
        return("Validated cluster-level SHAP is unavailable for the selected case.")
      }
      if (source == "Street View / site review") {
        shown <- unname(site_labels[site[site %in% names(site_labels)]])
        if (length(shown)) {
          return(paste0("Confirmed site context: ", paste(shown, collapse = ", "), "."))
        }
        return(paste0("Reviewed context: ", ctx$roadway_form %||% "interstate roadway",
                      "; ", ctx$lane_summary %||% "lane context unavailable",
                      ". Confirm visible conditions in Street View."))
      }
      ""
    }

    state_for <- function(source, available, supported) {
      if (source == "CCA cluster linkage") return("CCA-based relevance")
      if (source == "Crash-record evidence") return("Case-specific crash record")
      if (source == "Validated SHAP") return("Validated SHAP evidence")
      if (source == "Street View / site review") {
        if (available) return("Confirmed site context") else return("Site review pending")
      }
      if (supported) "Supporting evidence" else "Contextual evidence"
    }

    boxes <- lapply(seq_len(nrow(ev$table)), function(i) {
      z <- ev$table[i, ]
      cls <- if (z$Source == "Street View / site review" && !z$Available) {
        "support-box pending"
      } else if (z$Supported || z$Source %in% c("CCA cluster linkage","Crash-record evidence","Validated SHAP")) {
        "support-box yes"
      } else "support-box"
      div(class = cls,
          div(class = "support-source", z$Source),
          div(class = "support-state", state_for(z$Source, z$Available, z$Supported)),
          div(class = "support-detail", detail_for(z$Source)))
    })

    rationale <- countermeasure_case_rationale(key, r, a, site, shap_features, ctx)
    div(
      div(class = "selected-cluster-banner",
          div(class = "selected-cluster-label", "Case-specific recommendation evidence"),
          div(class = "selected-cluster-main", p$name),
          div(class = "selected-cluster-sub",
              paste0("Selected from the CCA-linked countermeasures for ", a$light, " ", a$cluster,
                     ". The evidence below explains the choice; no effect-size or crash-reduction probability is estimated."))),
      div(class="rationale-box", tags$b("Why the selected intervention is relevant: "), rationale),
      div(class = "support-grid", boxes)
    )
  })

  output$countermeasure_membership_kpis <- renderUI({
    a <- selected_assignment()
    e <- selected_membership_evidence()
    req(a, e)
    div(
      class = "analysis-mini-kpis",
      div(class = "analysis-mini-kpi",
          div(class = "big", paste0(a$light, " ", a$cluster)),
          div(class = "small", "Exact study-cluster assignment")),
      div(class = "analysis-mini-kpi",
          div(class = "big", paste0(sprintf("%.1f", e$assigned_proximity), "%")),
          div(class = "small", "Relative CCA proximity")),
      div(class = "analysis-mini-kpi",
          div(class = "big", ifelse(is.na(e$alignment), "NA", paste0(sprintf("%.1f", e$alignment), "%"))),
          div(class = "small", paste0("Residual profile alignment · ", e$supportive_n, "/", e$available_n, " supportive attributes")))
    )
  })

  output$countermeasure_cluster_proximity_plot <- renderPlot({
    a <- selected_assignment()
    e <- selected_membership_evidence()
    req(a, e)
    d <- e$centroids
    d$Cluster <- factor(d$Cluster, levels = rev(paste0("C", 1:4)))
    d$Fill <- ifelse(d$Assigned, "Assigned cluster", "Other cluster")
    ggplot(d, aes(RelativeProximity, Cluster, fill = Fill)) +
      geom_col(width = 0.68) +
      geom_text(aes(label = paste0(sprintf("%.1f", RelativeProximity), "%")), hjust = -0.08, size = 4.2, fontface = "bold") +
      scale_fill_manual(values = c("Assigned cluster" = "#D95F0E", "Other cluster" = "#CBD5E1")) +
      coord_cartesian(xlim = c(0, max(55, max(d$RelativeProximity) * 1.18))) +
      labs(title = "Relative position in CCA space", subtitle = "Inverse distance to each cluster centroid; sums to 100%", x = "Relative CCA proximity", y = NULL, fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.text=element_text(size=10.5,face="bold"), axis.text=element_text(size=11,face="bold"), axis.title=element_text(size=12,face="bold"), panel.grid.major.y = element_blank(), plot.title = element_text(face="bold",size=15), plot.subtitle=element_text(size=11,face="bold"))
  })

  output$countermeasure_cluster_alignment_plot <- renderPlot({
    a <- selected_assignment()
    e <- selected_membership_evidence()
    req(a, e)
    d <- head(e$profile, 8)
    if (nrow(d) == 0) return(ggplot() + theme_void())
    d$Label <- paste0(d$Variable, " = ", d$Value)
    d$Label <- factor(d$Label, levels = rev(d$Label))
    d$Direction <- ifelse(d$Residual > 0, "Supports assigned cluster", "Opposes / weakens")
    ggplot(d, aes(Residual, Label, fill = Direction)) +
      geom_col(width = 0.68) +
      geom_vline(xintercept = 0, color = "#475467", linewidth = 0.35) +
      scale_fill_manual(values = c("Supports assigned cluster"="#7F2704", "Opposes / weakens"="#B8C4D1")) +
      labs(title = "Why the crash profile fits the cluster", subtitle = "Selected crash attributes ranked by cluster standardized residual", x = "Standardized residual", y = NULL, fill = NULL) +
      theme_minimal(base_size = 12.5) +
      theme(legend.position = "top", legend.text=element_text(size=10.5,face="bold"), axis.text.x=element_text(size=10.5,face="bold"), axis.title.x=element_text(size=11.5,face="bold"), panel.grid.major.y = element_blank(), axis.text.y = element_text(size=10,face="bold"), plot.title = element_text(face="bold",size=14.5), plot.subtitle=element_text(size=10.5,face="bold"))
  })

  output$countermeasure_membership_note <- renderUI({
    a <- selected_assignment()
    e <- selected_membership_evidence()
    req(a, e)
    sup <- e$profile[e$profile$Residual > 0, , drop = FALSE]
    sup <- head(sup, 4)
    attrs <- if (nrow(sup) == 0) "No positive residual attributes were identified." else paste(paste0(sup$Variable, " = ", sup$Value), collapse = "; ")
    div(class = "analysis-panel-note",
        tags$b("Interpretation: "),
        paste0("The assigned cluster is explained using CCA proximity and supportive residual attributes. Strongest supportive attributes for the selected case: ", attrs, ". These values are descriptive and do not represent crash-risk probabilities."))
  })

  output$countermeasure_flowchart <- renderUI({
    r <- selected_row()
    a <- selected_assignment()
    key <- selected_policy_key()
    p <- countermeasure_library[[key]]
    shap_features <- selected_shap_features()
    site <- input$site_obs %||% character(0)
    req(r, a, p)

    shap_txt <- if (length(shap_features)) paste(head(shap_features, 3), collapse = ", ") else "Validated SHAP features unavailable"
    site_labels <- c(
      ramp="Ramp/freeway-entry context", highspeed="High-speed geometry",
      limited_shoulder="Limited shoulder/recovery space", merge="Merge/turning complexity",
      low_light="Limited roadway lighting", route_guidance="Route-guidance concern",
      no_parallel="No evident parallel bicycle facility"
    )
    shown <- unname(site_labels[site[site %in% names(site_labels)]])
    site_txt <- if (length(shown)) paste(shown, collapse = "; ") else "Site review pending / no visible condition confirmed"

    div(
      class = "flowchart-card",
      div(class = "source-badge", "Evidence-to-countermeasure visual"),
      div(class = "flowchart-title", "Why the selected intervention fits this case"),
      div(class = "flowchart-sub", "Four independent evidence streams converge into one intervention point and one selected countermeasure."),
      div(class = "visual-convergence",
          div(class = "vc-evidence-grid",
              div(class = "vc-card crash", div(class = "vc-label", "Crash record"), div(class = "vc-value", paste0(safe_chr(r[["Bike Crash Type"]]), " • ", safe_chr(r[["Bike Location"]]), " • avoidance: ", safe_chr(r[["Attempted for Avoidance"]])))),
              div(class = "vc-card cca", div(class = "vc-label", "Study CCA"), div(class = "vc-value", paste0(a$light, " ", a$cluster, " • ", a$title))),
              div(class = "vc-card shap", div(class = "vc-label", "Validated SHAP"), div(class = "vc-value", shap_txt)),
              div(class = "vc-card site", div(class = "vc-label", "Street View / site review"), div(class = "vc-value", site_txt))
          ),
          div(class = "vc-converge", "↘   ↓   ↙"),
          div(class = "vc-intervention", div(class = "vc-label", "Intervention point"), div(class = "vc-value", intervention_point_text(key))),
          div(class = "vc-converge", "↓"),
          div(class = "vc-countermeasure", div(class = "vc-label", "Selected countermeasure"), div(class = "vc-value", p$name)),
          div(class = "vc-pathway",
              div(class = "vc-label", "Expected safety pathway"),
              div(class = "vc-pathway-row",
                  span(class = "vc-chip", p$pathway[1]), span(class = "vc-arrow", "→"),
                  span(class = "vc-chip", p$pathway[2]), span(class = "vc-arrow", "→"),
                  span(class = "vc-chip", p$pathway[3])))
      )
    )
  })

  output$countermeasure_site_preview <- renderUI({
    r <- selected_row()
    if (!isTRUE(r$Mappable[1])) {
      return(div(class="analysis-card", div(class="source-badge", "Selected site"), tags$h4("Street View unavailable"), div(class="subtle", "No valid coordinate is available for this crash record.")))
    }
    lat <- r$Latitude[1]; lon <- r$Longitude[1]
    full_url <- sprintf("https://www.google.com/maps/@?api=1&map_action=pano&viewpoint=%.7f,%.7f", lat, lon)
    embed_url <- sprintf("https://maps.google.com/maps?layer=c&cbll=%.7f,%.7f&cbp=11,0,0,0,0&source=embed&output=svembed", lat, lon)
    ctx <- diagram_context()
    div(
      class = "analysis-card",
      div(class = "source-badge", "Selected crash site"),
      tags$h4("Google Street View preview"),
      tags$iframe(class="counter-site-frame", src=embed_url, loading="lazy", allowfullscreen="allowfullscreen"),
      div(class="subtle", paste0(ctx$roadway_form %||% "Interstate roadway", " · ", ctx$lane_summary %||% "lane context unavailable")),
      tags$a(href=full_url, target="_blank", class="btn btn-warning", style="margin-top:7px;", "Open full Street View")
    )
  })

  output$countermeasure_impact_diagram <- renderUI({
    r <- selected_row()
    a <- selected_assignment()
    key <- selected_policy_key()
    p <- countermeasure_library[[key]]
    ctx <- diagram_context()
    req(r, a, p, ctx)
    htmltools::HTML(build_case_diagram_svg(r, ctx, key, p, a))
  })

  output$diagram_explanation <- renderUI({
    r <- selected_row(); a <- selected_assignment(); key <- selected_policy_key(); p <- countermeasure_library[[key]]; ctx <- diagram_context()
    req(r,a,p,ctx)
    before_txt <- crash_mechanism_text(r, ctx, a)
    after_txt <- countermeasure_effect_text(key, p)
    div(class="diagram-explain-grid",
        div(class="diagram-explain before", tags$b("Technical interpretation of the selected crash mechanism"), tags$p(before_txt)),
        div(class="diagram-explain after", tags$b("Technical interpretation of the intervention pathway"), tags$p(after_txt)))
  })


  api_rationale_value <- reactiveVal(NULL)

  observeEvent(list(selected_record(), input$site_obs, input$road_geometry_confirmed), {
    api_rationale_value(NULL)
  }, ignoreInit = TRUE)

  output$private_api_control <- renderUI({
    if (!PRIVATE_API_AVAILABLE) return(NULL)
    div(
      class = "api-refine-box",
      actionButton("refresh_private_api", "Refine explanation with private API", class = "btn btn-default"),
      span(class = "subtle", style = "margin-left:8px;",
           "Optional: refines the wording from the already-established crash, CCA, SHAP, and site evidence. It does not change the cluster assignment or countermeasure selection.")
    )
  })

  observeEvent(input$refresh_private_api, {
    r <- selected_row()
    a <- selected_assignment()
    key <- selected_policy_key()
    p <- countermeasure_library[[key]]
    ctx <- diagram_context()
    site <- input$site_obs %||% character(0)
    shap_features <- selected_shap_features()
    req(r, a, p, ctx)

    base_rationale <- countermeasure_case_rationale(key, r, a, site, shap_features, ctx)
    prompt <- paste(
      "Write exactly two concise sentences for a transportation-safety research dashboard.",
      "Use only the evidence provided below. Do not invent roadway geometry, causation, probabilities, effect sizes, or additional countermeasures.",
      "Sentence 1 must explain how the selected crash evidence aligns with the assigned CCA cluster and validated SHAP profile.",
      "Sentence 2 must explain why the selected countermeasure is relevant to that case and where it acts in the exposure/response sequence, without claiming causation or measured effectiveness.",
      paste0("Selected crash record: ", r$Record_ID[1], "."),
      paste0("Assigned Study CCA: ", a$light, " ", a$cluster, " - ", a$title, "."),
      paste0("Crash type: ", safe_chr(r[["Bike Crash Type"]]), "."),
      paste0("Bicycle location: ", safe_chr(r[["Bike Location"]]), "."),
      paste0("Avoidance: ", safe_chr(r[["Attempted for Avoidance"]]), "."),
      paste0("Validated SHAP variables: ", paste(head(shap_features, 5), collapse = ", "), "."),
      paste0("Reviewed roadway context: ", ctx$roadway_form %||% "unknown", "; ", ctx$lane_summary %||% "unknown", "."),
      paste0("User-confirmed site codes: ", if (length(site)) paste(site, collapse = ", ") else "none confirmed", "."),
      paste0("Selected countermeasure: ", p$name, "."),
      paste0("Countermeasure target: ", p$target, "."),
      paste0("Deterministic case rationale: ", base_rationale),
      sep = "\n"
    )
    cache_key <- paste0("case_", gsub("[^A-Za-z0-9]", "_", r$Record_ID[1]), "_", key, "_", paste(sort(site), collapse="_"))
    api_rationale_value(private_api_text(prompt, cache_key))
  })

  output$private_api_rationale <- renderUI({
    z <- api_rationale_value()
    if (is.null(z)) return(NULL)
    if (isTRUE(z$ok)) {
      div(class="rationale-box", tags$b("Private API-refined explanation: "), z$text)
    } else {
      div(class="subtle", paste0("Private API refinement was unavailable: ", z$message, " The deterministic explanation above remains active."))
    }
  })

  output$countermeasure_text <- renderUI({
    r <- selected_row()
    a <- selected_assignment()
    site <- input$site_obs %||% character(0)
    shap_features <- selected_shap_features()
    ctx <- diagram_context()

    policy_key <- selected_policy_key()
    p <- countermeasure_library[[policy_key]]
    rationale <- countermeasure_case_rationale(policy_key, r, a, site, shap_features, ctx)

    div(
      class = "counter-card-v2 linked",
      div(class = "counter-status-v2 linked", paste0("Selected for ", a$light, " ", a$cluster)),
      tags$h3(style="margin-top:4px;color:#7F2704;", p$name),
      div(class = "counter-target-v2", tags$b("Targets: "), p$target),

      div(class = "counter-minihead", "Why it applies to this selected crash"),
      div(class="rationale-box", rationale),

      div(class = "counter-minihead", "How the countermeasure interrupts the crash mechanism"),
      tags$p(style="font-size:13px;line-height:1.5;", countermeasure_effect_text(policy_key, p)),

      div(class = "counter-minihead", "Implementation actions"),
      tags$ul(lapply(p$actions, function(x) tags$li(style="font-size:12.5px;margin-bottom:4px;", x))),

      div(class = "counter-minihead", "Study reference"),
      tags$p(class = "counter-ref-v2", p$full_reference)
    )
  })


}

shinyApp(ui, server)
