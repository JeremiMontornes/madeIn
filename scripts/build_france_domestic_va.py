from __future__ import annotations

import csv
import math
import sys
import time
from pathlib import Path
from urllib.error import HTTPError, URLError
from urllib.parse import quote
from urllib.request import urlopen
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[1]
DATA_DIR = ROOT / "data"
FIG_DIR = ROOT / "figures"
CACHE_DIR = DATA_DIR / "raw"

BASE_URL = (
    "https://sdmx.oecd.org/sti-public/rest/data/"
    "OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV"
)

START_YEAR = 1995
END_YEAR = 2022

SECTORS = [
    {
        "label": "Agriculture",
        "activity": "A",
        "color": "#ff7f79",
    },
    {
        "label": "Energy",
        "activity": "D_E",
        "color": "#14c64f",
    },
    {
        "label": "Manufacturing Industry",
        "activity": "C",
        "color": "#20c3c7",
    },
    {
        "label": "Construction",
        "activity": "F",
        "color": "#c2b20b",
    },
    {
        "label": "Market Services",
        "activity": "GTN",
        "color": "#6da5ff",
    },
    {
        "label": "Non-Market Services",
        "activity": "OTQ",
        "color": "#ed61dd",
    },
]

NAMESPACES = {
    "generic": "http://www.sdmx.org/resources/sdmxml/schemas/v2_1/data/generic",
}


def fetch_series(key: str, retries: int = 3) -> dict[int, float]:
    url = (
        f"{BASE_URL}/{quote(key, safe='.')}"
        f"?startPeriod={START_YEAR}&endPeriod={END_YEAR}"
    )
    cache_path = CACHE_DIR / f"{key}.xml"
    if cache_path.exists():
        return parse_sdmx_generic(cache_path.read_bytes())

    last_error: Exception | None = None
    for attempt in range(1, retries + 1):
        try:
            with urlopen(url, timeout=90) as response:
                payload = response.read()
            CACHE_DIR.mkdir(parents=True, exist_ok=True)
            cache_path.write_bytes(payload)
            return parse_sdmx_generic(payload)
        except (HTTPError, URLError, TimeoutError) as exc:
            last_error = exc
            if attempt == retries:
                break
            time.sleep(1.5 * attempt)
    raise RuntimeError(f"Could not fetch {key}: {last_error}")


def parse_sdmx_generic(payload: bytes) -> dict[int, float]:
    root = ET.fromstring(payload)
    out: dict[int, float] = {}
    for obs in root.findall(".//generic:Obs", NAMESPACES):
        period = obs.find("generic:ObsDimension", NAMESPACES)
        value = obs.find("generic:ObsValue", NAMESPACES)
        if period is None or value is None:
            continue
        year = int(period.attrib["value"])
        out[year] = float(value.attrib["value"])
    return out


def build_dataset() -> list[dict[str, str | int | float]]:
    rows: list[dict[str, str | int | float]] = []
    for sector in SECTORS:
        label = sector["label"]
        activity = sector["activity"]
        total_key = f"FD_VA.FRA.{activity}.W.USD.A"
        domestic_key = f"FD_VA.FRA.{activity}.FRA.USD.A"

        print(f"Fetching {label}: {domestic_key} and {total_key}")
        domestic = fetch_series(domestic_key)
        total = fetch_series(total_key)

        years = sorted(set(domestic) & set(total))
        for year in years:
            total_value = total[year]
            domestic_value = domestic[year]
            share = domestic_value / total_value if total_value else math.nan
            rows.append(
                {
                    "year": year,
                    "sector": label,
                    "activity": activity,
                    "domestic_va_musd": domestic_value,
                    "total_va_musd": total_value,
                    "domestic_share": share,
                    "domestic_share_pct": share * 100,
                }
            )
    return rows


def write_csv(rows: list[dict[str, str | int | float]]) -> Path:
    DATA_DIR.mkdir(parents=True, exist_ok=True)
    path = DATA_DIR / "french_va_content_in_french_internal_final_demand_by_sector.csv"
    fields = [
        "year",
        "sector",
        "activity",
        "domestic_va_musd",
        "total_va_musd",
        "domestic_share",
        "domestic_share_pct",
    ]
    with path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=fields)
        writer.writeheader()
        writer.writerows(sorted(rows, key=lambda r: (str(r["sector"]), int(r["year"]))))
    return path


def points_to_path(points: list[tuple[float, float]]) -> str:
    if not points:
        return ""
    head, *tail = points
    commands = [f"M {head[0]:.2f} {head[1]:.2f}"]
    commands.extend(f"L {x:.2f} {y:.2f}" for x, y in tail)
    return " ".join(commands)


def draw_svg(rows: list[dict[str, str | int | float]]) -> Path:
    FIG_DIR.mkdir(parents=True, exist_ok=True)
    width, height = 2200, 1280
    margin_left, margin_right = 130, 80
    margin_top, margin_bottom = 85, 300
    plot_w = width - margin_left - margin_right
    plot_h = height - margin_top - margin_bottom

    min_year, max_year = START_YEAR, END_YEAR
    y_min, y_max = 30, 100

    def x_pos(year: int) -> float:
        return margin_left + ((year - min_year) / (max_year - min_year)) * plot_w

    def y_pos(percent: float) -> float:
        return margin_top + ((y_max - percent) / (y_max - y_min)) * plot_h

    by_sector: dict[str, list[dict[str, str | int | float]]] = {}
    for row in rows:
        by_sector.setdefault(str(row["sector"]), []).append(row)

    svg: list[str] = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">',
        '<rect width="100%" height="100%" fill="#000000"/>',
        '<style>text{font-family:Arial, Helvetica, sans-serif}.axis{fill:#757575;font-weight:700}.legend{fill:#111111;font-size:38px}.title{fill:#f2f2f2;font-size:42px;font-weight:700}.note{fill:#b8b8b8;font-size:24px}</style>',
        '<text x="130" y="48" class="title">French value-added content in French internal final demand by sector</text>',
        '<text x="130" y="78" class="note">OECD TiVA 2025, FD_VA(FRA, sector, FRA) / FD_VA(FRA, sector, W)</text>',
    ]

    for tick in range(40, 101, 20):
        y = y_pos(tick)
        svg.append(
            f'<line x1="{margin_left}" y1="{y:.2f}" x2="{width - margin_right}" y2="{y:.2f}" '
            'stroke="#d7d7d7" stroke-width="4" stroke-dasharray="18 8" opacity="0.85"/>'
        )
        svg.append(
            f'<text x="{margin_left - 28}" y="{y + 13:.2f}" class="axis" '
            f'font-size="42" text-anchor="end">{tick}%</text>'
        )

    for year in range(1995, 2023, 5):
        x = x_pos(year)
        svg.append(
            f'<line x1="{x:.2f}" y1="{margin_top - 45}" x2="{x:.2f}" y2="{height - margin_bottom + 35}" '
            'stroke="#d7d7d7" stroke-width="4" stroke-dasharray="10 8 2 8" opacity="0.85"/>'
        )
        svg.append(
            f'<text x="{x:.2f}" y="{height - margin_bottom + 82}" class="axis" '
            f'font-size="42" text-anchor="middle">{year}</text>'
        )
    x = x_pos(2022)
    svg.append(
        f'<text x="{x:.2f}" y="{height - margin_bottom + 82}" class="axis" '
        'font-size="42" text-anchor="middle">2022</text>'
    )

    for sector in SECTORS:
        label = sector["label"]
        color = sector["color"]
        series = sorted(by_sector.get(label, []), key=lambda r: int(r["year"]))
        points = [
            (x_pos(int(row["year"])), y_pos(float(row["domestic_share_pct"])))
            for row in series
        ]
        svg.append(
            f'<path d="{points_to_path(points)}" fill="none" stroke="{color}" '
            'stroke-width="17" stroke-linecap="square" stroke-linejoin="round"/>'
        )

    legend_x, legend_y = 410, 1030
    legend_w, legend_h = 1380, 185
    svg.append(
        f'<rect x="{legend_x}" y="{legend_y}" width="{legend_w}" height="{legend_h}" '
        'rx="18" fill="#ffffff"/>'
    )
    legend_positions = [
        (legend_x + 80, legend_y + 70),
        (legend_x + 80, legend_y + 140),
        (legend_x + 420, legend_y + 70),
        (legend_x + 420, legend_y + 140),
        (legend_x + 930, legend_y + 70),
        (legend_x + 930, legend_y + 140),
    ]
    for sector, (lx, ly) in zip(SECTORS, legend_positions):
        svg.append(
            f'<line x1="{lx}" y1="{ly}" x2="{lx + 65}" y2="{ly}" '
            f'stroke="{sector["color"]}" stroke-width="15"/>'
        )
        svg.append(
            f'<text x="{lx + 95}" y="{ly + 14}" class="legend">{sector["label"]}</text>'
        )

    svg.append("</svg>")

    path = FIG_DIR / "french_va_content_in_french_internal_final_demand_by_sector.svg"
    path.write_text("\n".join(svg), encoding="utf-8")
    return path


def main() -> int:
    rows = build_dataset()
    if not rows:
        print("No observations returned.", file=sys.stderr)
        return 1
    csv_path = write_csv(rows)
    svg_path = draw_svg(rows)
    print(f"Wrote {csv_path}")
    print(f"Wrote {svg_path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
