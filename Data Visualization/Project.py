import kagglehub
import numpy as np
import pandas as pd

from bokeh.io import curdoc
from bokeh.layouts import column, row
from bokeh.models import ColumnDataSource, DateRangeSlider, Range1d, Select, HoverTool, Span, BoxAnnotation, Legend, DatetimeTickFormatter, NumeralTickFormatter, Title
from bokeh.plotting import figure

# Color controls
HOME_LINE_COLOR = "#E74C3C"   # red
AWAY_LINE_COLOR = "#0072B2"   # blue

# Darker Covid shading
PHASE_COLORS = {
    "hard":     {"fill_color": "#2B2B2B", "fill_alpha": 0.30},
    "closed":   {"fill_color": "#555555", "fill_alpha": 0.26},
    "limited":  {"fill_color": "#888888", "fill_alpha": 0.22},
    "capacity": {"fill_color": "#BDBDBD", "fill_alpha": 0.18},
}

# Covid lines
COVID_LINE_COLOR = "black"
COVID_START_LINE_DASH  = "dashed"
COVID_END_LINE_DASH = "dotted"
COVID_LINE_WIDTH = 2

# 1) Load & prepare data
path = kagglehub.dataset_download("prateekchauhands/football-data-top-5-european-leagues")

df_dtypes = {
    'Season':'string','Div':'string','Date':'string','Time':'string',
    'HomeTeam':'string','AwayTeam':'string','FTR':'string','HTR':'string','Referee':'string'
}
df = pd.read_csv(f"{path}/past-data.csv", dtype=df_dtypes)
df["Date"] = pd.to_datetime(df["Date"], dayfirst=True, format="mixed", errors="coerce")

TOP5 = ["Premier League","Bundesliga","Ligue 1","Serie A","LaLiga"]
START_DATE = pd.Timestamp("2003-08-01")

df = df[df["Div"].isin(TOP5) & (df["Date"] >= START_DATE)].copy()

# Points per match
ftr = df["FTR"]
df["HomePoints"] = np.where(ftr.eq("H"), 3, np.where(ftr.eq("D"), 1, np.where(ftr.eq("A"), 0, np.nan)))
df["AwayPoints"] = np.where(ftr.eq("A"), 3, np.where(ftr.eq("D"), 1, np.where(ftr.eq("H"), 0, np.nan)))
df = df.dropna(subset=["Date","HomePoints","AwayPoints"]).sort_values(["Div","Date"])

# 2) 365-day centered rolling per league
def rolling_365(g: pd.DataFrame) -> pd.DataFrame:
    r = g[["Date","HomePoints","AwayPoints"]].rolling("365D", on="Date", min_periods=1, center=True).mean()
    return pd.DataFrame({"Date": g["Date"].values, "home_roll": r["HomePoints"].values, "away_roll": r["AwayPoints"].values})

rolled = df.groupby("Div", group_keys=False, as_index=False).apply(lambda g: rolling_365(g).assign(Div=g.name), include_groups=False)

# 3) Figure
initial_div = "Premier League"

def select_division(div: str) -> pd.DataFrame:
    return rolled[rolled["Div"] == div].copy()

initial_div_df = select_division(initial_div)

# Date bounds
date_min, date_max = initial_div_df["Date"].min(), initial_div_df["Date"].max()

# Figure
source = ColumnDataSource(initial_div_df)
p = figure(
    title="Home vs Away points per game, Europe’s Top 5 Leagues",
    x_axis_type="datetime",
    x_range=Range1d(start=date_min, end=date_max),
    width=1600, height=700,
    tools="pan,wheel_zoom,box_zoom,reset,save",
    x_axis_label="Date", y_axis_label="Average points per game"
)

# Data lines
home_r = p.line("Date","home_roll", source=source, line_width=3, alpha=0.95, color=HOME_LINE_COLOR)
away_r = p.line("Date","away_roll", source=source, line_width=3, alpha=0.95, color=AWAY_LINE_COLOR)

# Subtitle
subtitle = Title(text=f"{initial_div}: 365-day centered rolling average of points", text_font_size="10pt", text_font_style="italic")
p.add_layout(subtitle, "above")

# 4) Covid phases
PHASES = {
    "Premier League": [
        ("hard",     "13-03-2020", "17-06-2020"),
        ("closed",   "17-06-2020", "02-12-2020"),
        ("limited",  "02-12-2020", "17-05-2021"),
        ("capacity", "17-05-2021", "19-07-2021"),
    ],
    "LaLiga": [
        ("hard",     "12-03-2020", "11-06-2020"),
        ("closed",   "11-06-2020", "16-05-2021"),
        ("limited",  "16-05-2021", "01-08-2021"),
        ("capacity", "01-08-2021", "01-10-2021"),
    ],
    "Bundesliga": [
        ("hard",     "13-03-2020", "16-05-2020"),
        ("closed",   "16-05-2020", "15-09-2020"),
        ("limited",  "15-09-2020", "02-02-2022"),
        ("capacity", "02-02-2022", "09-04-2022"),
    ],
    "Serie A": [
        ("hard",     "09-03-2020", "20-06-2020"),
        ("closed",   "20-06-2020", "19-09-2020"),
        ("limited",  "19-09-2020", "21-07-2021"),
        ("capacity", "21-07-2021", "01-04-2022"),
    ],
    "Ligue 1": [
        ("hard",     "13-03-2020", "11-07-2020"),
        ("limited",  "11-07-2020", "23-07-2021"),
        ("capacity", "23-07-2021", "02-02-2022"),
    ],
}

# Build shaded boxes and start/end lines once; initially hidden (toggled per league)
for league, spans in PHASES.items():
    # Shaded areas
    for phase_key, left_s, right_s in spans:
        left_dt  = pd.to_datetime(left_s,  dayfirst=True)
        right_dt = pd.to_datetime(right_s, dayfirst=True)
        
        style = PHASE_COLORS.get(phase_key, {"fill_color": "#CCCCCC", "fill_alpha": 0.20})
        box = BoxAnnotation(left=left_dt, right=right_dt, line_alpha=0.0, level="underlay",
                            name="phase_box", visible=False, tags=[league, phase_key], **style)
        p.add_layout(box)

    # Covid start line
    start_dt = min(pd.to_datetime(ls, dayfirst=True) for _, ls, _ in spans)
    p.add_layout(Span(location=start_dt, dimension="height", line_color=COVID_LINE_COLOR, line_dash=COVID_START_LINE_DASH , 
                            line_width=COVID_LINE_WIDTH, name="covid_start_line", visible=False, tags=[league]))
    # Covid end line
    end_dt = max(pd.to_datetime(rs, dayfirst=True) for _, _, rs in spans)
    p.add_layout(Span(location=end_dt, dimension="height", line_color=COVID_LINE_COLOR, line_dash=COVID_END_LINE_DASH, 
                    line_width=COVID_LINE_WIDTH, name="covid_end_line", visible=False, tags=[league]))

def update_covid_shading(value):
    for box in p.select(name="phase_box"):
        box.visible = (value in getattr(box, "tags"))
    for line in p.select(name="covid_start_line"):
        line.visible = (value in getattr(line, "tags"))
    for line in p.select(name="covid_end_line"):
        line.visible = (value in getattr(line, "tags"))

# Show initial league’s shading + lines
update_covid_shading(initial_div)

# 5) Hover — clear labels
hover = HoverTool(
    tooltips=[
        ("League", "@Div"),
        ("Date", "@Date{%F}"),
        ("Points in home games", "@home_roll{0.00}"),
        ("Points in away games", "@away_roll{0.00}")
    ],
    formatters={"@Date": "datetime"}
)
p.add_tools(hover)

# 6) League select
select = Select(title="Competition", value=initial_div, options=TOP5)

def update_div(attr, old, new):
    division_df = select_division(select.value)
    source.data = dict(division_df)

    # Update the subtitle
    subtitle.text = f"{select.value}: 365-day centered rolling average of points"

    # Update the covid data for this league
    update_covid_shading(select.value)

select.on_change("value", update_div)

# 7) Date slider
date_slider = DateRangeSlider(
    title="Display window",
    start=date_min, end=date_max, value=(date_min, date_max),
    step=1, format="%Y-%m-%d"
)
def update_slider(attr, old, new):
    p.x_range.start, p.x_range.end = date_slider.value_as_datetime
date_slider.on_change("value", update_slider)

# 8) Legend
legend_items = [("Points in home games", [home_r]), ("Points in away games", [away_r])]

# COVID start marker
dummy_start = p.segment(x0=[np.nan], y0=[np.nan], x1=[np.nan], y1=[np.nan], line_color=COVID_LINE_COLOR, line_dash=COVID_START_LINE_DASH, line_width=4)
legend_items += [("Start of COVID restrictions", [dummy_start])]

# Dummies for the label
dummy_hard     = p.square(x=[np.nan], y=[np.nan], size=20, line_color=None, **PHASE_COLORS["hard"])
dummy_closed   = p.square(x=[np.nan], y=[np.nan], size=20, line_color=None, **PHASE_COLORS["closed"])
dummy_limited  = p.square(x=[np.nan], y=[np.nan], size=20, line_color=None, **PHASE_COLORS["limited"])
dummy_capacity = p.square(x=[np.nan], y=[np.nan], size=20, line_color=None, **PHASE_COLORS["capacity"])
legend_items += [
    ("Matches suspended (lockdown)", [dummy_hard]),
    ("Closed-door matches", [dummy_closed]),
    ("Limited fans (≤25% capacity)", [dummy_limited]),
    ("Capacity increased (>25% capacity)", [dummy_capacity])
]

# COVID end marker
dummy_end   = p.segment(x0=[np.nan], y0=[np.nan], x1=[np.nan], y1=[np.nan], line_color=COVID_LINE_COLOR, line_dash=COVID_END_LINE_DASH, line_width=4)
legend_items += [("COVID restrictions fully lifted", [dummy_end])]

# Construct legend
legend = Legend(items=legend_items, title="Legend", orientation="vertical")

# Legend options
legend.click_policy = "mute"
legend.title_text_font_size = "12pt"
legend.title_text_font_style = "bold"
legend.label_text_font_size = "10pt"
legend.glyph_width  = 30
legend.glyph_height = 22
legend.spacing = 6
legend.padding = 8
legend.margin = 8
legend.label_standoff = 8
p.add_layout(legend, 'right')

# 9) Plot formatting
p.title.text_font_size = "15pt"
p.title.text_font_style = "bold"
p.title.align = "left"

p.xaxis.axis_label_text_font_size = "11pt"
p.yaxis.axis_label_text_font_size = "11pt"
p.axis.major_label_text_font_size = "10pt"
p.axis.axis_label_text_font_style = "normal"

p.xaxis.formatter = DatetimeTickFormatter(days="%d %b %Y", months="%b %Y", years="%Y")
p.xaxis.major_label_orientation = 0.5
p.yaxis.formatter = NumeralTickFormatter(format="0.00")
p.y_range.start = 0
p.y_range.end = 3.0

p.background_fill_color = "#ffffff"
p.border_fill_color = "#ffffff"
p.outline_line_color = "#d9d9d9"
p.outline_line_alpha = 0.6
p.grid.grid_line_color = "#b0b0b0"
p.grid.grid_line_alpha = 0.6
p.grid.grid_line_dash = "dotted"
p.xgrid.minor_grid_line_color = None
p.ygrid.minor_grid_line_color = None

# Layout
curdoc().add_root(column(row(select, date_slider), p))
curdoc().title = "Leagues Overview"
