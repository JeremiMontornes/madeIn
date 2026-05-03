script_path <- function() {
  args <- commandArgs(FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
  }
  normalizePath("scripts/build_france_domestic_va.R", mustWork = FALSE)
}

root <- normalizePath(file.path(dirname(script_path()), ".."), mustWork = TRUE)
data_dir <- file.path(root, "data")
raw_dir <- file.path(data_dir, "raw")
fig_dir <- file.path(root, "figures")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

base_url <- paste0(
  "https://sdmx.oecd.org/sti-public/rest/data/",
  "OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV"
)
start_year <- 1995L
end_year <- 2022L

sectors <- data.frame(
  sector = c(
    "Agriculture",
    "Energy",
    "Manufacturing Industry",
    "Construction",
    "Market Services",
    "Non-Market Services"
  ),
  activity = c("A", "D_E", "C", "F", "GTN", "OTQ"),
  color = c("#ff7f79", "#14c64f", "#20c3c7", "#c2b20b", "#6da5ff", "#ed61dd"),
  stringsAsFactors = FALSE
)

fetch_series <- function(key) {
  cache_file <- file.path(raw_dir, paste0(key, ".xml"))
  if (!file.exists(cache_file)) {
    url <- paste0(base_url, "/", key, "?startPeriod=", start_year, "&endPeriod=", end_year)
    message("Downloading ", key)
    download.file(url, cache_file, mode = "wb", quiet = TRUE)
  }
  parse_sdmx_generic(cache_file)
}

parse_sdmx_generic <- function(path) {
  x <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "")
  obs <- gregexpr("<generic:Obs>.*?</generic:Obs>", x, perl = TRUE)[[1]]
  if (identical(obs, -1L)) {
    return(data.frame(year = integer(), value = numeric()))
  }
  blocks <- regmatches(x, list(obs))[[1]]
  year <- as.integer(sub('.*<generic:ObsDimension[^>]*value="([0-9]{4})".*', "\\1", blocks))
  value <- as.numeric(sub('.*<generic:ObsValue[^>]*value="([^"]+)".*', "\\1", blocks))
  data.frame(year = year, value = value)
}

build_dataset <- function() {
  out <- list()
  k <- 1L
  for (i in seq_len(nrow(sectors))) {
    sector <- sectors$sector[[i]]
    activity <- sectors$activity[[i]]
    domestic_key <- paste("FD_VA", "FRA", activity, "FRA", "USD", "A", sep = ".")
    total_key <- paste("FD_VA", "FRA", activity, "W", "USD", "A", sep = ".")

    message("Processing ", sector, ": ", domestic_key, " and ", total_key)
    domestic <- fetch_series(domestic_key)
    total <- fetch_series(total_key)
    names(domestic)[names(domestic) == "value"] <- "domestic_va_musd"
    names(total)[names(total) == "value"] <- "total_va_musd"

    merged <- merge(domestic, total, by = "year")
    merged$sector <- sector
    merged$activity <- activity
    merged$domestic_share <- merged$domestic_va_musd / merged$total_va_musd
    merged$domestic_share_pct <- 100 * merged$domestic_share
    out[[k]] <- merged[, c(
      "year", "sector", "activity", "domestic_va_musd", "total_va_musd",
      "domestic_share", "domestic_share_pct"
    )]
    k <- k + 1L
  }
  do.call(rbind, out)
}

write_outputs <- function(df) {
  csv_path <- file.path(data_dir, "french_va_content_in_french_internal_final_demand_by_sector.csv")
  df <- df[order(df$sector, df$year), ]
  write.csv(df, csv_path, row.names = FALSE)
  csv_path
}

draw_chart_device <- function(df) {
  op <- par(
    bg = "white",
    fg = "#222222",
    mar = c(10.8, 5.5, 2.8, 1.8),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )
  on.exit(par(op))

  plot(
    NA,
    xlim = c(start_year, end_year),
    ylim = c(30, 100),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = ""
  )
  abline(h = seq(40, 100, 20), col = "#cfcfcf", lwd = 2, lty = "dashed")
  abline(v = seq(1995, 2020, 5), col = "#d9d9d9", lwd = 2, lty = "dotdash")
  axis(1, at = c(seq(1995, 2020, 5), 2022), col = NA, col.ticks = NA, col.axis = "#555555", cex.axis = 1.35, font = 2)
  axis(2, at = seq(40, 100, 20), labels = paste0(seq(40, 100, 20), "%"), las = 1, col = NA, col.ticks = NA, col.axis = "#555555", cex.axis = 1.35, font = 2)

  title(
    main = "French value-added content in French internal final demand by sector",
    col.main = "#111111",
    cex.main = 1.25,
    font.main = 2,
    line = 0.8
  )
  mtext(
    "French share of value added embodied in French internal final demand",
    side = 3,
    line = -0.8,
    col = "#555555",
    cex = 0.85
  )

  for (i in seq_len(nrow(sectors))) {
    series <- df[df$sector == sectors$sector[[i]], ]
    series <- series[order(series$year), ]
    lines(series$year, series$domestic_share_pct, col = sectors$color[[i]], lwd = 7, lend = "square", ljoin = "mitre")
  }

  legend(
    "bottom",
    inset = c(0, -0.28),
    legend = sectors$sector,
    col = sectors$color,
    lwd = 6,
    ncol = 3,
    bg = "white",
    box.col = "#cfcfcf",
    text.col = "black",
    cex = 1.15,
    xpd = TRUE,
    seg.len = 1.5
  )
  mtext(
    "Source: OECD TiVA 2025, dataflow DSD_TIVA_MAINLV@DF_MAINLV. Calculation: FD_VA(FRA, sector, FRA) / FD_VA(FRA, sector, W).",
    side = 1,
    line = 9.0,
    col = "#555555",
    cex = 0.78,
    adj = 0
  )
}

draw_charts <- function(df) {
  svg_path <- file.path(fig_dir, "french_va_content_in_french_internal_final_demand_by_sector.svg")
  png_path <- file.path(fig_dir, "french_va_content_in_french_internal_final_demand_by_sector.png")

  svg(svg_path, width = 15.3, height = 8.9, bg = "white")
  draw_chart_device(df)
  dev.off()

  png(png_path, width = 2200, height = 1280, res = 144, bg = "white")
  draw_chart_device(df)
  dev.off()

  c(svg = svg_path, png = png_path)
}

df <- build_dataset()
csv_path <- write_outputs(df)
chart_paths <- draw_charts(df)
message("Wrote ", csv_path)
message("Wrote ", chart_paths[["svg"]])
message("Wrote ", chart_paths[["png"]])
