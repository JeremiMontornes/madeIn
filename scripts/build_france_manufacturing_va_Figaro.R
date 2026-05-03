script_path <- function() {
  args <- commandArgs(FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
  }
  normalizePath("scripts/build_france_manufacturing_va_Figaro.R", mustWork = FALSE)
}

root <- normalizePath(file.path(dirname(script_path()), ".."), mustWork = TRUE)
data_dir <- file.path(root, "data")
raw_dir <- file.path(data_dir, "raw_Figaro")
fig_dir <- file.path(root, "figures")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

start_year <- 2010L
end_year <- 2023L
years <- start_year:end_year

subsectors <- data.frame(
  sector = c(
    "Food, beverages & tobacco",
    "Textiles, apparel & leather",
    "Wood products",
    "Paper products",
    "Printing",
    "Petroleum products",
    "Chemicals",
    "Pharmaceuticals",
    "Rubber & plastics",
    "Non-metallic minerals",
    "Basic metals",
    "Fabricated metals",
    "Electronics",
    "Electrical equipment",
    "Machinery",
    "Motor vehicles",
    "Other transport equipment",
    "Furniture & other manufacturing",
    "Repair & installation"
  ),
  indicator = c(
    "C10-C12", "C13-C15", "C16", "C17", "C18", "C19", "C20", "C21",
    "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30",
    "C31_C32", "C33"
  ),
  gva = c(
    "C10-12", "C13-15", "C16", "C17", "C18", "C19", "C20", "C21",
    "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29", "C30",
    "C31_32", "C33"
  ),
  panel = c(rep(1L, 10L), rep(2L, 9L)),
  color = c(
    "#e15759", "#f28e2b", "#edc948", "#59a14f", "#76b7b2",
    "#4e79a7", "#af7aa1", "#ff9da7", "#9c755f", "#bab0ac",
    "#e15759", "#f28e2b", "#edc948", "#59a14f", "#76b7b2",
    "#4e79a7", "#af7aa1", "#ff9da7", "#9c755f"
  ),
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

sum_by_subsector <- function(d, code_col, value_col, code_col_in_map) {
  out <- list()
  for (i in seq_len(nrow(subsectors))) {
    code <- subsectors[[code_col_in_map]][[i]]
    x <- d[d[[code_col]] == code, ]
    if (nrow(x) == 0) stop("No rows for ", subsectors$sector[[i]], " / ", code)
    agg <- aggregate(x[[value_col]], by = list(year = x$year), FUN = sum, na.rm = TRUE)
    names(agg)[2] <- value_col
    agg$sector <- subsectors$sector[[i]]
    agg$indicator <- subsectors$indicator[[i]]
    agg$gva <- subsectors$gva[[i]]
    agg$panel <- subsectors$panel[[i]]
    out[[subsectors$sector[[i]]]] <- agg
  }
  do.call(rbind, out)
}

figaro_fgfd_url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/naio_10_fgfd?format=tsv&compressed=true"
figaro_fgdf_url <- "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/naio_10_fgdf?format=tsv&compressed=true"
fgfd <- read_eurostat_tsv(download_cached("naio_10_fgfd_Figaro", figaro_fgfd_url))
fgdf <- read_eurostat_tsv(download_cached("naio_10_fgdf_Figaro", figaro_fgdf_url))

foreign_in_french <- to_long(
  fgfd[
    fgfd$freq == "A" &
      fgfd$geo == "FR" &
      fgfd$c_orig == "TOTAL" &
      fgfd$unit == "THS_EUR" &
      fgfd$nace_r2 %in% subsectors$indicator,
  ],
  "foreign_va_in_french_final_use_ths_eur"
)

french_in_foreign <- to_long(
  fgdf[
    fgdf$freq == "A" &
      fgdf$geo == "FR" &
      !(fgdf$c_dest %in% c("FR", "TOTAL", "EU27_2020", "NEU27_2020")) &
      fgdf$unit == "THS_EUR" &
      fgdf$nace_r2 %in% subsectors$indicator,
  ],
  "french_va_in_foreign_final_use_ths_eur"
)

read_figaro_gva_from_io <- function() {
  datasets <- c("naio_10_fcp_ii1", "naio_10_fcp_ii2", "naio_10_fcp_ii3", "naio_10_fcp_ii4")
  va_rows <- c("B2A3G", "D1", "D21X31", "D29X39")
  needed_industries <- subsectors$gva
  out <- data.frame(year = integer(), nace_r2 = character(), gross_value_added_ths_eur = numeric())

  for (dataset in datasets) {
    url <- paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
      dataset,
      "?format=tsv&compressed=true"
    )
    path <- download_cached(paste0(dataset, "_Figaro"), url)
    con <- gzfile(path, open = "rt")
    header <- readLines(con, n = 1L, warn = FALSE)
    header_parts <- strsplit(header, "\t", fixed = TRUE)[[1]]
    file_years <- as.integer(trimws(header_parts[-1]))
    keep_years <- which(file_years %in% years)
    file_years <- file_years[keep_years]

    repeat {
      lines <- readLines(con, n = 100000L, warn = FALSE)
      if (!length(lines)) break
      parts <- strsplit(lines, "\t", fixed = TRUE)
      dims <- do.call(rbind, strsplit(vapply(parts, `[[`, character(1), 1L), ",", fixed = TRUE))
      colnames(dims) <- c("freq", "ind_use", "ind_ava", "c_dest", "unit", "c_orig")
      keep <- dims[, "freq"] == "A" &
        dims[, "c_dest"] == "FR" &
        dims[, "unit"] == "MIO_EUR" &
        dims[, "ind_ava"] %in% va_rows &
        dims[, "ind_use"] %in% needed_industries
      if (!any(keep)) next

      vals <- do.call(rbind, lapply(parts[keep], function(z) {
        suppressWarnings(as.numeric(trimws(z[-1][keep_years])))
      }))
      tmp <- data.frame(
        nace_r2 = rep(dims[keep, "ind_use"], times = length(file_years)),
        year = rep(file_years, each = sum(keep)),
        gross_value_added_ths_eur = as.vector(vals) * 1000
      )
      out <- rbind(out, tmp)
    }
    close(con)
  }
  aggregate(gross_value_added_ths_eur ~ year + nace_r2, out, sum, na.rm = TRUE)
}

gva_fr <- read_figaro_gva_from_io()

foreign_sector <- sum_by_subsector(foreign_in_french, "nace_r2", "foreign_va_in_french_final_use_ths_eur", "indicator")
foreign_abs_sector <- sum_by_subsector(french_in_foreign, "nace_r2", "french_va_in_foreign_final_use_ths_eur", "indicator")
gva_sector <- sum_by_subsector(gva_fr, "nace_r2", "gross_value_added_ths_eur", "gva")

df <- merge(gva_sector, foreign_abs_sector, by = c("year", "sector", "indicator", "gva", "panel"), all = FALSE)
df <- merge(df, foreign_sector, by = c("year", "sector", "indicator", "gva", "panel"), all = FALSE)
df$french_va_in_french_final_use_ths_eur <- df$gross_value_added_ths_eur - df$french_va_in_foreign_final_use_ths_eur
df$total_va_in_french_final_use_ths_eur <- df$french_va_in_french_final_use_ths_eur + df$foreign_va_in_french_final_use_ths_eur
df$french_share <- df$french_va_in_french_final_use_ths_eur / df$total_va_in_french_final_use_ths_eur
df$french_share_pct <- 100 * df$french_share
df <- merge(df, subsectors[, c("sector", "color")], by = "sector", all.x = TRUE)
df <- df[order(df$panel, df$sector, df$year), ]

csv_path <- file.path(data_dir, "french_va_content_in_french_internal_final_demand_manufacturing_subsectors_Figaro.csv")
write.csv(df, csv_path, row.names = FALSE)

draw_chart_device <- function(df_panel, panel_number) {
  op <- par(
    bg = "white",
    fg = "#222222",
    mar = c(11.6, 5.5, 2.8, 1.8),
    xaxs = "i",
    yaxs = "i",
    family = "sans"
  )
  on.exit(par(op))

  plot(NA, xlim = c(start_year, end_year), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "", main = "")
  abline(h = seq(0, 100, 20), col = "#cfcfcf", lwd = 2, lty = "dashed")
  abline(v = c(2010, 2015, 2020, 2023), col = "#d9d9d9", lwd = 2, lty = "dotdash")
  axis(1, at = c(2010, 2015, 2020, 2023), col = NA, col.ticks = NA, col.axis = "#555555", cex.axis = 1.25, font = 2)
  axis(2, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"), las = 1, col = NA, col.ticks = NA, col.axis = "#555555", cex.axis = 1.25, font = 2)

  title(
    main = paste0("Made in France: manufacturing domestic value added content in domestic final demand (", panel_number, "/2)"),
    col.main = "#111111",
    cex.main = 1.05,
    font.main = 2,
    line = 0.8
  )

  panel_sectors <- subsectors$sector[subsectors$panel == panel_number]
  panel_colors <- subsectors$color[subsectors$panel == panel_number]
  for (i in seq_along(panel_sectors)) {
    series <- df_panel[df_panel$sector == panel_sectors[[i]], ]
    series <- series[order(series$year), ]
    lines(series$year, series$french_share_pct, col = panel_colors[[i]], lwd = 5, lend = "square", ljoin = "mitre")
  }

  legend(
    "bottom",
    inset = c(0, -0.33),
    legend = panel_sectors,
    col = panel_colors,
    lwd = 5,
    ncol = 2,
    bg = "white",
    box.col = "#cfcfcf",
    text.col = "black",
    cex = 0.86,
    xpd = TRUE,
    seg.len = 1.5
  )
  mtext("Source: Eurostat FIGARO", side = 1, line = 9.9, col = "#555555", cex = 0.78, adj = 0)
}

draw_charts <- function(df) {
  paths <- character()
  for (panel_number in sort(unique(df$panel))) {
    df_panel <- df[df$panel == panel_number, ]
    svg_path <- file.path(fig_dir, paste0("french_va_content_in_french_internal_final_demand_manufacturing_subsectors_Figaro_", panel_number, ".svg"))
    png_path <- file.path(fig_dir, paste0("french_va_content_in_french_internal_final_demand_manufacturing_subsectors_Figaro_", panel_number, ".png"))

    svg(svg_path, width = 15.3, height = 8.9, bg = "white")
    draw_chart_device(df_panel, panel_number)
    dev.off()

    png(png_path, width = 2200, height = 1280, res = 144, bg = "white")
    draw_chart_device(df_panel, panel_number)
    dev.off()

    paths <- c(paths, svg_path, png_path)
  }
  paths
}

chart_paths <- draw_charts(df)
message("Wrote ", csv_path)
for (path in chart_paths) message("Wrote ", path)
