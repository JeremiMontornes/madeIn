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

    dt <- dt[
      freq == "A" &
        ind_use %in% manufacturing_codes &
        !(ind_ava %in% value_added_rows) &
        c_dest %in% destination_countries &
        !(c_orig %in% aggregate_origins) &
        unit == "MIO_EUR"
    ]

    if (nrow(dt) == 0L) next

    keep_years <- intersect(as.character(years), file_years)
    individual_entities <- entities$entity[entities$entity != "EU27_2020"]
    dt_individual <- dt[c_dest %in% individual_entities]
    dt_individual[, entity := c_dest]
    dt_eu <- dt[c_dest %in% eu_members]
    dt_eu[, entity := "EU27_2020"]
    dt <- data.table::rbindlist(list(dt_individual, dt_eu), use.names = TRUE)

    long <- data.table::melt(
      dt,
      id.vars = c("entity", "c_orig"),
      measure.vars = keep_years,
      variable.name = "year",
      value.name = "value_added_mio_eur"
    )
    long[, year := as.integer(as.character(year))]

    total <- long[, .(
      value_added_mio_eur = sum(value_added_mio_eur, na.rm = TRUE)
    ), by = .(entity, year)]
    total[, type := "total"]

    domestic <- long[
      (entity == "EU27_2020" & c_orig %in% eu_members) |
        (entity != "EU27_2020" & c_orig == entity),
      .(value_added_mio_eur = sum(value_added_mio_eur, na.rm = TRUE)),
      by = .(entity, year)
    ]
    domestic[, type := "domestic"]

    out[[dataset]] <- rbind(total, domestic)
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
wide$domestic_va_share <- wide$domestic / wide$total
wide$domestic_va_share_pct <- 100 * wide$domestic_va_share
df <- merge(wide, entities, by = "entity", all.x = TRUE)
df <- df[order(df$panel, df$entity, df$year), ]

csv_path <- file.path(data_dir, "manufacturing_domestic_va_content_in_final_internal_demand_countries_Figaro.csv")
write.csv(df, csv_path, row.names = FALSE)

draw_chart_device <- function(df_panel, panel_number) {
  op <- par(
    bg = "black",
    fg = "#666666",
    mar = c(9.5, 5.8, 1.2, 1.8),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )
  on.exit(par(op))

  plot(
    NA,
    xlim = c(start_year, end_year),
    ylim = c(50, 100),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = ""
  )
  abline(h = seq(50, 100, 10), col = "#cfcfcf", lwd = 2, lty = "dashed")
  abline(v = c(2010, 2015, 2020, 2023), col = "#cfcfcf", lwd = 2, lty = "dashed")
  axis(
    1,
    at = c(2010, 2015, 2020, 2023),
    col = NA,
    col.ticks = NA,
    col.axis = "#666666",
    cex.axis = 1.4,
    font = 2
  )
  axis(
    2,
    at = seq(50, 100, 10),
    labels = seq(50, 100, 10),
    las = 1,
    col = NA,
    col.ticks = NA,
    col.axis = "#666666",
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
    box.col = "#d8d8d8",
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
    svg_path <- file.path(fig_dir, paste0("manufacturing_domestic_va_content_in_final_internal_demand_countries_Figaro_", panel_number, ".svg"))
    png_path <- file.path(fig_dir, paste0("manufacturing_domestic_va_content_in_final_internal_demand_countries_Figaro_", panel_number, ".png"))

    svg(svg_path, width = 15.3, height = 8.9, bg = "black")
    draw_chart_device(df_panel, panel_number)
    dev.off()

    png(png_path, width = 2200, height = 1280, res = 144, bg = "black")
    draw_chart_device(df_panel, panel_number)
    dev.off()

    paths <- c(paths, svg_path, png_path)
  }
  paths
}

chart_paths <- draw_charts(df)
message("Wrote ", csv_path)
for (path in chart_paths) message("Wrote ", path)
