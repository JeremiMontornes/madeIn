script_path <- function() {
  args <- commandArgs(FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
  }
  normalizePath("scripts/build_manufacturing_domestic_va_countries_Figaro.R", mustWork = FALSE)
}

root <- normalizePath(file.path(dirname(script_path()), ".."), mustWork = TRUE)
data_dir <- file.path(root, "data")
raw_dir <- file.path(data_dir, "raw_Figaro")
fig_dir <- file.path(root, "figures")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

if (!requireNamespace("data.table", quietly = TRUE) ||
    !requireNamespace("R.utils", quietly = TRUE)) {
  stop(
    "This script needs data.table and R.utils for fast FIGARO reads. ",
    "Install them with install.packages(c('data.table', 'R.utils')).",
    call. = FALSE
  )
}

start_year <- 2010L
end_year <- 2023L
years <- start_year:end_year

manufacturing_codes <- c(
  "C10-12", "C13-15", "C16", "C17", "C18", "C19", "C20", "C21",
  "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29",
  "C30", "C31_32", "C33"
)
value_added_rows <- c("B2A3G", "D1", "D21X31", "D29X39")

eu_members <- c(
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES",
  "FI", "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT",
  "NL", "PL", "PT", "RO", "SE", "SI", "SK"
)

entities <- data.frame(
  entity = c("CN", "EU27_2020", "US", "DE", "ES", "FR", "IT"),
  label = c("China", "European Union", "United States", "Germany", "Spain", "France", "Italy"),
  panel = c(1L, 1L, 1L, 2L, 2L, 2L, 2L),
  color = c("#ff7f79", "#20c653", "#6da5ff", "#ff7f79", "#86b80d", "#20c3c7", "#c277f2"),
  stringsAsFactors = FALSE
)

download_cached <- function(dataset, url) {
  path <- file.path(raw_dir, paste0(dataset, ".tsv.gz"))
  if (!file.exists(path)) {
    message("Downloading ", dataset)
    ok <- FALSE
    for (attempt in 1:5) {
      ok <- tryCatch({
        download.file(url, path, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) {
        if (file.exists(path)) unlink(path)
        message("Retry ", attempt, " for ", dataset, ": ", conditionMessage(e))
        Sys.sleep(2 * attempt)
        FALSE
      })
      if (ok) break
    }
    if (!ok) stop("Could not download ", dataset)
  }
  path
}

read_figaro_manufacturing_va <- function() {
  datasets <- paste0("naio_10_fcp_ii", 1:4)
  destination_countries <- unique(c(entities$entity[entities$entity != "EU27_2020"], eu_members))
  aggregate_origins <- c("DOM", "EU27_2020", "NEU27_2020", "TOTAL")
  out <- list()

  for (dataset in datasets) {
    url <- paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
      dataset,
      "?format=tsv&compressed=true"
    )
    path <- download_cached(paste0(dataset, "_Figaro"), url)
    con <- gzfile(path, open = "rt")
    header <- readLines(con, n = 1L, warn = FALSE)
    close(con)
    header_parts <- strsplit(header, "\t", fixed = TRUE)[[1]]
    file_years <- trimws(header_parts[-1])

    dt <- data.table::fread(
      path,
      sep = "\t",
      header = FALSE,
      skip = 1L,
      showProgress = FALSE
    )
    data.table::setnames(dt, c("dims", file_years))

    dim_cols <- data.table::tstrsplit(dt$dims, ",", fixed = TRUE)
    data.table::set(
      dt,
      j = "freq",
      value = dim_cols[[1L]]
    )
    data.table::set(dt, j = "ind_use", value = dim_cols[[2L]])
    data.table::set(dt, j = "ind_ava", value = dim_cols[[3L]])
    data.table::set(dt, j = "c_dest", value = dim_cols[[4L]])
    data.table::set(dt, j = "unit", value = dim_cols[[5L]])
    data.table::set(dt, j = "c_orig", value = dim_cols[[6L]])
    dt[, dims := NULL]

    keep_years <- intersect(as.character(years), file_years)
    individual_entities <- entities$entity[entities$entity != "EU27_2020"]

    make_entity_rows <- function(x) {
      x_individual <- x[c_dest %in% individual_entities]
      x_individual[, entity := c_dest]
      x_eu <- x[c_dest %in% eu_members]
      x_eu[, entity := "EU27_2020"]
      data.table::rbindlist(list(x_individual, x_eu), use.names = TRUE)
    }

    domestic_dt <- dt[
      freq == "A" &
        !(ind_use %in% value_added_rows) &
        ind_ava %in% manufacturing_codes &
        c_dest %in% destination_countries &
        !(c_orig %in% aggregate_origins) &
        unit == "MIO_EUR"
    ]
    domestic_dt <- make_entity_rows(domestic_dt)
    domestic_long <- data.table::melt(
      domestic_dt,
      id.vars = c("entity", "c_orig"),
      measure.vars = keep_years,
      variable.name = "year",
      value.name = "value_added_mio_eur"
    )
    domestic_long[, year := as.integer(as.character(year))]

    domestic <- domestic_long[
      (entity == "EU27_2020" & c_orig %in% eu_members) |
        (entity != "EU27_2020" & c_orig == entity),
      .(value_added_mio_eur = sum(value_added_mio_eur, na.rm = TRUE)),
      by = .(entity, year)
    ]
    domestic[, type := "domestic"]

    foreign_dt <- dt[
      freq == "A" &
        ind_use %in% manufacturing_codes &
        !(ind_ava %in% value_added_rows) &
        c_dest %in% destination_countries &
        !(c_orig %in% aggregate_origins) &
        unit == "MIO_EUR"
    ]
    foreign_dt <- make_entity_rows(foreign_dt)
    foreign_long <- data.table::melt(
      foreign_dt,
      id.vars = c("entity", "c_orig"),
      measure.vars = keep_years,
      variable.name = "year",
      value.name = "value_added_mio_eur"
    )
    foreign_long[, year := as.integer(as.character(year))]

    foreign <- foreign_long[
      (entity == "EU27_2020" & !(c_orig %in% eu_members)) |
        (entity != "EU27_2020" & c_orig != entity),
      .(value_added_mio_eur = sum(value_added_mio_eur, na.rm = TRUE)),
      by = .(entity, year)
    ]
    foreign[, type := "foreign"]

    out[[dataset]] <- rbind(domestic, foreign)
  }

  out <- data.table::rbindlist(out, use.names = TRUE)
  out <- out[, .(
    value_added_mio_eur = sum(value_added_mio_eur, na.rm = TRUE)
  ), by = .(entity, year, type)]
  as.data.frame(out)
}

va <- read_figaro_manufacturing_va()
wide <- reshape(va, idvar = c("entity", "year"), timevar = "type", direction = "wide")
names(wide) <- sub("^value_added_mio_eur\\.", "", names(wide))
wide$total <- wide$domestic + wide$foreign
wide$domestic_va_share <- wide$domestic / wide$total
wide$domestic_va_share_pct <- 100 * wide$domestic_va_share
df <- merge(wide, entities, by = "entity", all.x = TRUE)
df <- df[order(df$panel, df$entity, df$year), ]

read_eurostat_tsv <- function(path) {
  d <- read.delim(gzfile(path), check.names = FALSE, stringsAsFactors = FALSE)
  dims <- do.call(rbind, strsplit(d[[1]], ",", fixed = TRUE))
  dim_names <- strsplit(names(d)[1], ",", fixed = TRUE)[[1]]
  dim_names[length(dim_names)] <- sub("\\\\TIME_PERIOD$", "", dim_names[length(dim_names)])
  colnames(dims) <- dim_names
  out <- data.frame(dims, stringsAsFactors = FALSE)
  for (yr in as.character(years)) {
    out[[yr]] <- suppressWarnings(as.numeric(trimws(d[[yr]])))
  }
  out
}

to_long <- function(d, value_name) {
  out <- list()
  for (yr in as.character(years)) {
    tmp <- d[, setdiff(names(d), as.character(years)), drop = FALSE]
    tmp$year <- as.integer(yr)
    tmp[[value_name]] <- d[[yr]]
    out[[yr]] <- tmp
  }
  do.call(rbind, out)
}

read_figaro_gva <- function(geos) {
  datasets <- paste0("naio_10_fcp_ii", 1:4)
  out <- list()
  for (dataset in datasets) {
    path <- download_cached(
      paste0(dataset, "_Figaro"),
      paste0(
        "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
        dataset,
        "?format=tsv&compressed=true"
      )
    )
    con <- gzfile(path, open = "rt")
    header <- readLines(con, n = 1L, warn = FALSE)
    close(con)
    header_parts <- strsplit(header, "\t", fixed = TRUE)[[1]]
    file_years <- trimws(header_parts[-1])
    keep_years <- intersect(as.character(years), file_years)

    dt <- data.table::fread(path, sep = "\t", header = FALSE, skip = 1L, showProgress = FALSE)
    data.table::setnames(dt, c("dims", file_years))
    dim_cols <- data.table::tstrsplit(dt$dims, ",", fixed = TRUE)
    data.table::set(dt, j = "freq", value = dim_cols[[1L]])
    data.table::set(dt, j = "ind_use", value = dim_cols[[2L]])
    data.table::set(dt, j = "ind_ava", value = dim_cols[[3L]])
    data.table::set(dt, j = "c_dest", value = dim_cols[[4L]])
    data.table::set(dt, j = "unit", value = dim_cols[[5L]])
    dt[, dims := NULL]
    dt <- dt[
      freq == "A" &
        ind_use %in% manufacturing_codes &
        ind_ava %in% value_added_rows &
        c_dest %in% geos &
        unit == "MIO_EUR"
    ]
    long <- data.table::melt(
      dt,
      id.vars = "c_dest",
      measure.vars = keep_years,
      variable.name = "year",
      value.name = "gross_value_added_ths_eur"
    )
    long[, year := as.integer(as.character(year))]
    long[, gross_value_added_ths_eur := 1000 * gross_value_added_ths_eur]
    out[[dataset]] <- long[, .(
      gross_value_added_ths_eur = sum(gross_value_added_ths_eur, na.rm = TRUE)
    ), by = .(entity = c_dest, year)]
  }
  out <- data.table::rbindlist(out)
  out <- out[, .(
    gross_value_added_ths_eur = sum(gross_value_added_ths_eur, na.rm = TRUE)
  ), by = .(entity, year)]
  as.data.frame(out)
}

build_indicator_formula_df <- function() {
  geos <- c("EU27_2020", "DE", "ES", "FR", "IT")
  fgfd <- read_eurostat_tsv(download_cached(
    "naio_10_fgfd_Figaro",
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/naio_10_fgfd?format=tsv&compressed=true"
  ))
  fgdf <- read_eurostat_tsv(download_cached(
    "naio_10_fgdf_Figaro",
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/naio_10_fgdf?format=tsv&compressed=true"
  ))

  foreign_in_domestic <- to_long(
    fgfd[
      fgfd$freq == "A" &
        fgfd$nace_r2 == "C" &
        fgfd$geo %in% geos &
        fgfd$c_orig == "TOTAL" &
        fgfd$unit == "THS_EUR",
    ],
    "foreign"
  )
  names(foreign_in_domestic)[names(foreign_in_domestic) == "geo"] <- "entity"

  domestic_in_foreign <- to_long(
    fgdf[
      fgdf$freq == "A" &
        fgdf$nace_r2 == "C" &
        fgdf$geo %in% geos &
        !(fgdf$c_dest %in% c(geos, "TOTAL", "EU27_2020", "NEU27_2020")) &
        fgdf$unit == "THS_EUR",
    ],
    "domestic_in_foreign"
  )
  names(domestic_in_foreign)[names(domestic_in_foreign) == "geo"] <- "entity"
  domestic_in_foreign <- aggregate(
    domestic_in_foreign ~ entity + year,
    domestic_in_foreign,
    sum,
    na.rm = TRUE
  )

  gva <- read_figaro_gva(geos)
  out <- merge(gva, domestic_in_foreign, by = c("entity", "year"), all = FALSE)
  out <- merge(
    out,
    foreign_in_domestic[, c("entity", "year", "foreign")],
    by = c("entity", "year"),
    all = FALSE
  )
  out$domestic <- out$gross_value_added_ths_eur - out$domestic_in_foreign
  out$total <- out$domestic + out$foreign
  out$domestic_va_share <- out$domestic / out$total
  out$domestic_va_share_pct <- 100 * out$domestic_va_share
  out[, c("entity", "year", "domestic", "foreign", "total", "domestic_va_share", "domestic_va_share_pct")]
}

indicator_df <- build_indicator_formula_df()
df <- df[!(df$entity %in% indicator_df$entity), ]
df <- rbind(
  df[, names(indicator_df)],
  indicator_df
)
df <- merge(df, entities, by = "entity", all.x = TRUE)
df <- df[order(df$panel, df$entity, df$year), ]

csv_path <- file.path(data_dir, "manufacturing_domestic_va_content_in_final_internal_demand_countries_Figaro.csv")
write.csv(df, csv_path, row.names = FALSE)

draw_chart_device <- function(df_panel, panel_number) {
  op <- par(
    bg = "white",
    fg = "#222222",
    mar = c(9.5, 5.8, 1.2, 1.8),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )
  on.exit(par(op))

  plot(
    NA,
    xlim = c(start_year, end_year),
    ylim = c(20, 100),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = ""
  )
  abline(h = seq(20, 100, 20), col = "#bdbdbd", lwd = 2, lty = "dashed")
  abline(v = c(2010, 2015, 2020, 2023), col = "#bdbdbd", lwd = 2, lty = "dashed")
  axis(
    1,
    at = c(2010, 2015, 2020, 2023),
    col = NA,
    col.ticks = NA,
    col.axis = "#555555",
    cex.axis = 1.4,
    font = 2
  )
  axis(
    2,
    at = seq(20, 100, 20),
    labels = seq(20, 100, 20),
    las = 1,
    col = NA,
    col.ticks = NA,
    col.axis = "#555555",
    cex.axis = 1.4,
    font = 2
  )

  panel_entities <- entities[entities$panel == panel_number, ]
  for (i in seq_len(nrow(panel_entities))) {
    series <- df_panel[df_panel$entity == panel_entities$entity[[i]], ]
    series <- series[order(series$year), ]
    lines(
      series$year,
      series$domestic_va_share_pct,
      col = panel_entities$color[[i]],
      lwd = 7,
      lend = "square",
      ljoin = "mitre"
    )
  }

  legend(
    "bottom",
    inset = c(0, -0.28),
    legend = panel_entities$label,
    col = panel_entities$color,
    lwd = 6,
    ncol = if (panel_number == 1L) 3 else 4,
    bg = "white",
    box.col = "#cfcfcf",
    text.col = "black",
    cex = if (panel_number == 1L) 1.25 else 1.2,
    xpd = TRUE,
    seg.len = 1.5
  )
}

draw_charts <- function(df) {
  paths <- character()
  for (panel_number in sort(unique(entities$panel))) {
    df_panel <- df[df$panel == panel_number, ]
    png_path <- file.path(fig_dir, paste0("manufacturing_domestic_va_content_in_final_internal_demand_countries_Figaro_", panel_number, ".png"))

    png(png_path, width = 2200, height = 1280, res = 144, bg = "white")
    draw_chart_device(df_panel, panel_number)
    dev.off()

    paths <- c(paths, png_path)
  }
  paths
}

chart_paths <- draw_charts(df)
message("Wrote ", csv_path)
for (path in chart_paths) message("Wrote ", path)
