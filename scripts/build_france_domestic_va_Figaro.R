script_path <- function() {
  args <- commandArgs(FALSE)
  file_arg <- args[grepl("^--file=", args)]
  if (length(file_arg) > 0) {
    return(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
  }
  normalizePath("scripts/build_france_domestic_va_Figaro.R", mustWork = FALSE)
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

sectors <- data.frame(
  sector = c(
    "Agriculture",
    "Energy",
    "Manufacturing Industry",
    "Construction",
    "Market Services",
    "Non-Market Services"
  ),
  color = c("#ff7f79", "#14c64f", "#20c3c7", "#c2b20b", "#6da5ff", "#ed61dd"),
  stringsAsFactors = FALSE
)

sector_map <- list(
  "Agriculture" = list(
    indicator = c("A"),
    gva = c("A01", "A02", "A03")
  ),
  "Energy" = list(
    indicator = c("D35", "E36", "E37-E39"),
    gva = c("D35", "E36", "E37-39")
  ),
  "Manufacturing Industry" = list(
    indicator = c("C"),
    gva = c(
      "C10-12", "C13-15", "C16", "C17", "C18", "C19", "C20", "C21",
      "C22", "C23", "C24", "C25", "C26", "C27", "C28", "C29",
      "C30", "C31_32", "C33"
    )
  ),
  "Construction" = list(
    indicator = c("F"),
    gva = c("F")
  ),
  "Market Services" = list(
    indicator = c("G-I", "J", "K", "L", "M_N"),
    gva = c(
      "G45", "G46", "G47", "H49", "H50", "H51", "H52", "H53", "I",
      "J58", "J59_60", "J61", "J62_63", "K64", "K65", "K66", "L",
      "M69_70", "M71", "M72", "M73", "M74_75", "N77", "N78", "N79", "N80-82"
    )
  ),
  "Non-Market Services" = list(
    indicator = c("O-Q"),
    gva = c("O84", "P85", "Q86", "Q87_88")
  )
)

download_cached <- function(dataset, url) {
  path <- file.path(raw_dir, paste0(dataset, ".tsv.gz"))
  if (!file.exists(path)) {
    message("Downloading ", dataset)
    download.file(url, path, mode = "wb", quiet = TRUE)
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

sum_by_sector <- function(d, code_col, value_col, codes_by_sector) {
  out <- list()
  for (sector in names(codes_by_sector)) {
    x <- d[d[[code_col]] %in% codes_by_sector[[sector]], ]
    agg <- aggregate(x[[value_col]], by = list(year = x$year), FUN = sum, na.rm = TRUE)
    names(agg)[2] <- value_col
    agg$sector <- sector
    out[[sector]] <- agg
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
      fgfd$unit == "THS_EUR",
  ],
  "foreign_va_in_french_final_use_ths_eur"
)

french_in_foreign <- to_long(
  fgdf[
    fgdf$freq == "A" &
      fgdf$geo == "FR" &
      !(fgdf$c_dest %in% c("FR", "TOTAL", "EU27_2020", "NEU27_2020")) &
      fgdf$unit == "THS_EUR",
  ],
  "french_va_in_foreign_final_use_ths_eur"
)

figaro_codes <- lapply(sector_map, `[[`, "indicator")
gva_codes <- lapply(sector_map, `[[`, "gva")

read_figaro_gva_from_io <- function() {
  datasets <- c("naio_10_fcp_ii1", "naio_10_fcp_ii2", "naio_10_fcp_ii3", "naio_10_fcp_ii4")
  va_rows <- c("B2A3G", "D1", "D21X31", "D29X39")
  needed_industries <- unique(unlist(gva_codes))
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

foreign_sector <- sum_by_sector(foreign_in_french, "nace_r2", "foreign_va_in_french_final_use_ths_eur", figaro_codes)
foreign_abs_sector <- sum_by_sector(french_in_foreign, "nace_r2", "french_va_in_foreign_final_use_ths_eur", figaro_codes)
gva_sector <- sum_by_sector(gva_fr, "nace_r2", "gross_value_added_ths_eur", gva_codes)

df <- merge(gva_sector, foreign_abs_sector, by = c("year", "sector"), all = FALSE)
df <- merge(df, foreign_sector, by = c("year", "sector"), all = FALSE)
df$french_va_in_french_final_use_ths_eur <- df$gross_value_added_ths_eur - df$french_va_in_foreign_final_use_ths_eur
df$total_va_in_french_final_use_ths_eur <- df$french_va_in_french_final_use_ths_eur + df$foreign_va_in_french_final_use_ths_eur
df$french_share <- df$french_va_in_french_final_use_ths_eur / df$total_va_in_french_final_use_ths_eur
df$french_share_pct <- 100 * df$french_share
df <- df[order(df$sector, df$year), ]

csv_path <- file.path(data_dir, "french_va_content_in_french_internal_final_demand_by_sector_Figaro.csv")
write.csv(df, csv_path, row.names = FALSE)

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
  abline(v = c(2010, 2015, 2020, 2023), col = "#d9d9d9", lwd = 2, lty = "dotdash")
  axis(1, at = c(2010, 2015, 2020, 2023), col = NA, col.ticks = NA, col.axis = "#555555", cex.axis = 1.35, font = 2)
  axis(2, at = seq(40, 100, 20), labels = paste0(seq(40, 100, 20), "%"), las = 1, col = NA, col.ticks = NA, col.axis = "#555555", cex.axis = 1.35, font = 2)

  title(
    main = "French value-added content in French internal final demand by sector",
    col.main = "#111111",
    cex.main = 1.25,
    font.main = 2,
    line = 0.8
  )

  for (i in seq_len(nrow(sectors))) {
    series <- df[df$sector == sectors$sector[[i]], ]
    series <- series[order(series$year), ]
    lines(series$year, series$french_share_pct, col = sectors$color[[i]], lwd = 7, lend = "square", ljoin = "mitre")
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
  mtext("Source: Eurostat FIGARO", side = 1, line = 9.0, col = "#555555", cex = 0.78, adj = 0)
}

draw_charts <- function(df) {
  svg_path <- file.path(fig_dir, "french_va_content_in_french_internal_final_demand_by_sector_Figaro.svg")
  png_path <- file.path(fig_dir, "french_va_content_in_french_internal_final_demand_by_sector_Figaro.png")

  svg(svg_path, width = 15.3, height = 8.9, bg = "white")
  draw_chart_device(df)
  dev.off()

  png(png_path, width = 2200, height = 1280, res = 144, bg = "white")
  draw_chart_device(df)
  dev.off()

  c(svg = svg_path, png = png_path)
}

chart_paths <- draw_charts(df)
message("Wrote ", csv_path)
message("Wrote ", chart_paths[["svg"]])
message("Wrote ", chart_paths[["png"]])
