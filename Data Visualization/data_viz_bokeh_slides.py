#!/usr/bin/env python
"""
Interactive Presentation App with Bokeh
Run with: bokeh serve bokeh_app.py
Then open: http://localhost:5006/06_interactive_presentation

=== BOKEH SERVER ARCHITECTURE ===
Bokeh Server creates a bidirectional websocket connection between Python and the browser.
This allows:
1. Python callbacks to run on the server (not in JavaScript)
2. Real-time updates from server to client
3. Stateful applications with server-side logic
4. Multiple clients connecting to the same app instance
"""

# === CORE BOKEH IMPORTS ===
# figure: The main plotting interface - creates a Plot object with axes, grids, tools
# curdoc: Current document - represents the Bokeh document that will be synchronized to the browser
from bokeh.plotting import figure, curdoc

# === BOKEH MODELS ===
# Models are the building blocks of Bokeh applications
# Each model represents a component that can be rendered in the browser
from bokeh.models import (
    # === WIDGETS ===
    Div,  # HTML div element for custom HTML/CSS content
    Button,  # Interactive button widget - triggers Python callbacks on click
    Slider,  # Numeric slider widget - triggers callbacks on value change
    Select,  # Dropdown selection widget - triggers callbacks on selection
    # === DATA HANDLING ===
    ColumnDataSource,  # CRITICAL: Bokeh's fundamental data structure
    # - Holds columnar data (like a DataFrame)
    # - Automatically syncs between Python and JavaScript
    # - Updates to .data property trigger re-rendering
    # - Enables streaming, patching, and efficient updates
    # === TOOLS ===
    HoverTool,  # Interactive hover tooltips - shows data on mouse hover
    # - Can use @ prefix to reference column names
    # - Supports custom HTML formatting
    # === COLOR MAPPING ===
    LinearColorMapper,  # Maps numeric values to colors linearly
    # - Used for heatmaps, choropleth maps
    # - Requires palette (list of colors) and low/high range
    ColorBar,  # Visual legend for color mappers
    # - Shows the color scale with numeric labels
    # === FORMATTING ===
    BasicTicker,  # Controls tick mark locations on axes
    PrintfTickFormatter,  # Formats tick labels using printf-style strings
)

# === LAYOUT SYSTEM ===
# Bokeh uses a responsive layout system similar to CSS flexbox
from bokeh.layouts import (
    column,  # Vertical layout - stacks elements top to bottom
    row,  # Horizontal layout - places elements side by side
    layout,  # Grid layout - accepts nested lists for complex layouts
    # Example: layout([[plot1, plot2], [plot3]]) creates 2 rows
)

# === COLOR PALETTES ===
# Bokeh provides pre-defined color palettes for consistent styling
from bokeh.palettes import (
    RdYlBu11,  # Red-Yellow-Blue diverging palette with 11 colors
    # Good for showing positive/negative values
    Category20,  # Categorical palette with up to 20 distinct colors
    # Dictionary with keys for different numbers of colors (3,4,5...20)
)

# === TRANSFORMS ===
# Transforms are client-side operations that happen in the browser
from bokeh.transform import (
    factor_cmap,  # Maps categorical factors to colors
    # More efficient than manually assigning colors
)

# Standard Python libraries
import numpy as np
import pandas as pd
import base64  # For encoding images as base64 strings to embed in HTML
import os
import matplotlib.pyplot as plt
from io import BytesIO

import kagglehub
from bokeh.models import Range1d, BoxAnnotation, Span, Title, DateRangeSlider, NumeralTickFormatter, DatetimeTickFormatter, Legend


class InteractivePresentation:
    """
    Main application class for the Bokeh presentation system.

    === BOKEH APPLICATION PATTERN ===
    This follows the Object-Oriented pattern for Bokeh apps:
    1. Initialize state variables
    2. Create UI components (widgets, plots)
    3. Set up callbacks (event handlers)
    4. Compose layout
    5. Add to document

    The class encapsulates all presentation logic and state.
    """

    def __init__(self):
        # === STATE MANAGEMENT ===
        # Bokeh server apps are stateful - these variables persist across callbacks
        self.current_slide = 0  # Track which slide is currently displayed
        self.total_slides = 4  # Total number of slides in presentation
        self.slides = []  # Will hold Bokeh layout objects for each slide
        self.auto_play = False  # Flag for auto-advance mode
        self.auto_play_callback = None  # Reference to periodic callback for cleanup

        # === INITIALIZATION ORDER MATTERS ===
        # 1. Create slides first (generates all content)
        self.create_slides()

        # 2. Create navigation (needs to know about slides)
        self.create_navigation()

        # 3. Create main layout (combines slides + navigation)
        self.create_layout()

    def create_navigation(self):
        """Create navigation controls

        === BOKEH WIDGETS ===
        Widgets are interactive components that trigger Python callbacks.
        Unlike JavaScript frameworks, these callbacks run on the SERVER.
        """

        # === BUTTON WIDGETS ===
        # Button constructor parameters:
        # - label: Text displayed on button (supports Unicode emoji)
        # - button_type: Bootstrap-style types ("default", "primary", "success", "warning", "danger")
        # - width/height: Size in pixels (responsive by default if not set)
        # - disabled: Boolean to enable/disable interaction
        self.prev_button = Button(label="◀ Previous", button_type="primary", width=100)
        self.next_button = Button(label="Next ▶", button_type="primary", width=100)
        self.home_button = Button(label="🏠 Home", button_type="warning", width=100)

        # === SELECT WIDGET ===
        # Select creates a dropdown menu
        # Options format: List of tuples (value, label)
        # - value: What gets stored in widget.value (usually string)
        # - label: What user sees in dropdown
        slide_options = [
            (str(i), f"Slide {i + 1}: {self.get_slide_title(i)}") for i in range(self.total_slides)
        ]
        self.slide_select = Select(
            title="Jump to:",  # Label above dropdown
            value="0",  # Initial selection (must match a value from options)
            options=slide_options,  # List of (value, label) tuples
            width=300,
        )

        # === DIV WIDGET ===
        # Div renders arbitrary HTML/CSS
        # SECURITY NOTE: Bokeh sanitizes HTML to prevent XSS attacks
        # Supports inline styles and basic HTML tags
        self.progress_div = Div(
            text=self.get_progress_html(),  # HTML string
            width=200,  # Width in pixels
        )

        # === CALLBACK ATTACHMENT ===
        # CRITICAL CONCEPT: Callbacks in Bokeh Server
        # These callbacks run in PYTHON on the SERVER, not JavaScript in browser
        # When user clicks button → browser sends message → server runs Python function → updates sent back

        # Button callbacks: .on_click(function)
        # Function receives no arguments (for buttons)
        self.prev_button.on_click(self.prev_slide)
        self.next_button.on_click(self.next_slide)
        self.home_button.on_click(self.go_home)

        # Property change callbacks: .on_change("property_name", function)
        # Function receives (attr, old_value, new_value)
        # Common properties: "value", "active", "data"
        self.slide_select.on_change("value", self.jump_to_slide)

    def get_slide_title(self, index):
        """Get title for each slide"""
        titles = [
            "Radar Chart",
            "Delta Chart",
            "Rankings and Comparisons",
            "Leagues Overview",
        ]
        return titles[index] if index < len(titles) else f"Slide {index + 1}"

    def get_progress_html(self):
        """Generate progress bar HTML"""
        progress_pct = ((self.current_slide + 1) / self.total_slides) * 100
        return f"""
        <div style="text-align: center;">
            <b>Slide {self.current_slide + 1} of {self.total_slides}</b><br>
            <div style="width: 100%; background-color: #f0f0f0; border-radius: 5px;">
                <div style="width: {progress_pct}%; background-color: #4CAF50; 
                           height: 20px; border-radius: 5px;"></div>
            </div>
        </div>
        """

    def create_slides(self):
        """Create all presentation slides"""
        self.slides = [
            self.create_slide_1_radar(),
            self.create_slide_2_delta(),
            self.create_slide_3_ranking(),
            self.create_slide_4_leagues()
        ]


    def create_slide_1_radar(self):
        """Slide 1: Radar chart Home vs Away statistics"""
        
        # Title
        title = Div(
            text="""
            <h1 style="text-align: center; color: #2c3e50;">
                📊 Home vs Away Multi-dimensional Stats - Premier League 21-22
            </h1>
            """,
            width=800,
            height=80,
        )
        
        url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2023/2023-04-04/soccer21-22.csv"
        soccer = pd.read_csv(url)

        # Aggregate sums
        df = pd.DataFrame({
            'team': ['Home', 'Away'],
            'Shots on Target': [soccer['HST'].sum(), soccer['AST'].sum()],
            'Shots': [soccer['HS'].sum(), soccer['AS'].sum()],
            'Corners': [soccer['HC'].sum(), soccer['AC'].sum()],
            'Fouls': [soccer['HF'].sum(), soccer['AF'].sum()],
            'Goals': [soccer['FTHG'].sum(), soccer['FTAG'].sum()]
        })
        
        # Radar chart with mathplotlib
        stats = ['Shots', 'Corners', 'Shots on Target','Fouls','Goals']
        teams = df['team'].tolist()
        
        # Get the radar data
        radar_data = []
        for _, df_row in df.iterrows():
            values = [
                (df_row['Shots'] / df['Shots'].max()) * 100,
                (df_row['Corners'] / df['Corners'].max()) * 100,
                (df_row['Shots on Target'] / df['Shots on Target'].max()) * 100,
                (df_row['Fouls'] / df['Fouls'].max()) * 100,
                (df_row['Goals'] / df['Goals'].max()) * 100
            ]
            radar_data.append(values)
        
        num_vars = len(stats)
        angles = np.linspace(0, 2*np.pi, num_vars, endpoint=False).tolist()
        radar_data = [vals + [vals[0]] for vals in radar_data]
        angles += angles[:1]
        
        colors_radar = ['#1976D2', '#D32F2F']
        
        # Create the figure
        fig = plt.figure(figsize=(6,6))
        ax = plt.subplot(1,1,1, projection='polar')
        
        for i, (team, values) in enumerate(zip(teams, radar_data)):
            ax.plot(angles, values, 'o-', linewidth=2, label=team, color=colors_radar[i])
            ax.fill(angles, values, alpha=0.15, color=colors_radar[i])
        
        # Formatting
        labels = ['Shots', 'Corners', 'Shots on\nTarget','Fouls','Goals']
        ax.set_xticks(angles[:-1])
        ax.set_xticklabels(labels, size=9)
        ax.set_ylim(0, 100)
        ax.set_yticks([20, 40, 60, 80, 100])
        ax.set_yticklabels(['20','40','60','80','100'], size=8)
        ax.set_title('Radar Chart\nHome vs Away Games', fontsize=11, fontweight='bold', pad=20)
        ax.legend(loc='upper right', bbox_to_anchor=(1.3,1.0), fontsize=9)
        ax.grid(True)
        ax.tick_params(axis='x', pad=8)
        
        # Convert Matplotlib figure to Div to display with Bokeh
        buf = BytesIO()
        fig.savefig(buf, format='png', bbox_inches='tight')
        plt.close(fig)
        buf.seek(0)
        img_base64 = base64.b64encode(buf.read()).decode('utf-8')
        img_html = f'<img src="data:image/png;base64,{img_base64}" width="600"/>'
        img_div = Div(text=img_html, width=600, height=600)
        
        # Info Panel
        info = Div(
            text="""
            <div style="background-color: #ecf0f1; padding: 20px; border-radius: 10px;">
                <h3>🎯 Slide Info:</h3>
                <ul>
                    <li>Blue: Home team stats</li>
                    <li>Red: Away team stats</li>
                    <li>Normalized 0-100 for comparison</li>
                    <li>Metrics: Shots, Shots on Target, Corners, Fouls, Goals</li>
                </ul>
            </div>
            """,
            width=400,
            height=250
        )
        
        return layout([[title], [column(img_div), info]])


    def create_slide_2_delta(self):
        """Slide 2: Delta chart for Home vs Away points difference"""

        # Title Div
        title = Div(
            text="""
            <h1 style="text-align: center; color: #2c3e50;">
                📊 Home vs Away Points Difference - Premier League 21-22
            </h1>
            """,
            width=800,
            height=80,
        )

        url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2023/2023-04-04/soccer21-22.csv"
        soccer = pd.read_csv(url)
        soccer["HomePoints"] = soccer["FTR"].map({"H": 3, "D": 1, "A": 0})
        soccer["AwayPoints"] = soccer["FTR"].map({"A": 3, "D": 1, "H": 0})

        home = soccer.groupby("HomeTeam")["HomePoints"].sum().reset_index()
        away = soccer.groupby("AwayTeam")["AwayPoints"].sum().reset_index()
        points = pd.merge(home, away, left_on="HomeTeam", right_on="AwayTeam")
        points["Equipe"] = points["HomeTeam"]
        points["PercentageDifference"] = (points["HomePoints"] - points["AwayPoints"]) / (points["HomePoints"] + points["AwayPoints"])

        # Determine colors
        points["colors"] = np.where(points["PercentageDifference"] >= 0, "green", "red")

        # ColumnDataSource for Bokeh
        source = ColumnDataSource(data=dict(
            Equipe=points["Equipe"],
            PercentageDifference=points["PercentageDifference"],
            colors=points["colors"]
        ))
        
        # Create Figure
        p = figure(
            x_range=points["Equipe"].tolist(),
            width=800,
            height=400,
            title="Percentage Difference in Home vs Away Points – Premier League 2021/2022",
            toolbar_location="above",
            tools="pan,wheel_zoom,box_zoom,reset,save,hover"
        )

        # Add bars
        p.vbar(
            x="Equipe",
            top="PercentageDifference",
            width=0.8,
            color="colors",
            source=source
        )

        # Add horizontal lines at 0 and median
        mean_val = points["PercentageDifference"].median()
        p.line(x=points["Equipe"], y=[0]*len(points), line_color="black", line_width=1)
        p.line(x=points["Equipe"], y=[mean_val]*len(points), line_color="orange", line_width=1)

        # Axis labels
        p.xaxis.major_label_orientation = np.pi/2
        p.yaxis.axis_label = "Home Points % - Away Points %"
        p.xaxis.axis_label = "Teams"

        # Info Panel
        info = Div(
            text="""
            <div style="background-color: #ecf0f1; padding: 20px; border-radius: 10px;">
                <h3>🎯 Slide Info:</h3>
                <ul>
                    <li>Green bars: More points won at home</li>
                    <li>Red bars: More points won away</li>
                    <li>Orange line: Median difference</li>
                    <li>Interactive: Zoom, pan, hover over bars</li>
                </ul>
            </div>
            """,
            width=400,
            height=250
        )

        return layout([[title], [p, info]])


    def create_slide_3_ranking(self):
        """Slide 3: Ranking evolution chart based on real vs alternative points"""
        
        # Title Div
        title = Div(
            text="""
            <h1 style="text-align: center; color: #2c3e50;">
                📈 Evolution of Team Rankings - Premier League 21-22
            </h1>
            """,
            width=800,
            height=80,
        )
        
        url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/refs/heads/main/data/2023/2023-04-04/soccer21-22.csv"
        soccer = pd.read_csv(url)

        # Points real
        soccer["HomePoints"] = soccer["FTR"].map({"H":3,"D":1,"A":0})
        soccer["AwayPoints"] = soccer["FTR"].map({"A":3,"D":1,"H":0})
        home = soccer.groupby("HomeTeam")["HomePoints"].sum().reset_index()
        away = soccer.groupby("AwayTeam")["AwayPoints"].sum().reset_index()
        points = pd.merge(home, away, left_on="HomeTeam", right_on="AwayTeam")
        points["Equipe"] = points["HomeTeam"]
        points["Total"] = points["HomePoints"] + points["AwayPoints"]
        
        # Points alternative
        soccer["HomePoints_alt"] = soccer["FTR"].map({"H":0,"D":0,"A":0})
        soccer["AwayPoints_alt"] = soccer["FTR"].map({"A":3,"D":1,"H":0})
        home_alt = soccer.groupby("HomeTeam")["HomePoints_alt"].sum().reset_index()
        away_alt = soccer.groupby("AwayTeam")["AwayPoints_alt"].sum().reset_index()
        points_alt = pd.merge(home_alt, away_alt, left_on="HomeTeam", right_on="AwayTeam")
        points_alt["Equipe"] = points_alt["HomeTeam"]
        points_alt["Total_alt"] = points_alt["HomePoints_alt"] + points_alt["AwayPoints_alt"]
        
        # Final fusion
        table = pd.merge(points[["Equipe","Total"]], points_alt[["Equipe","Total_alt"]], on="Equipe")
        
        # Rankings
        table["Rank"] = table["Total"].rank(method="min", ascending=False).astype(int)
        table["Rank_alt"] = table["Total_alt"].rank(method="min", ascending=False).astype(int)
        
        # Sort by actual ranking
        table = table.sort_values("Rank").reset_index(drop=True)
        
        # Create a figure with matplotlib
        table["y_pos"] = np.arange(len(table))
        fig, ax = plt.subplots(figsize=(8,9))
        
        for _, table_row in table.iterrows():
            # Color according to change in ranking
            if table_row["Rank_alt"] < table_row["Rank"]:
                color = "green"
                adjust_position = -0.2
            elif table_row["Rank_alt"] > table_row["Rank"]:
                color = "red"
                adjust_position = +0.6
            else:
                color = "gray"
                adjust_position = +0.6
            
            # Arrow
            ax.arrow(table_row["Rank"], table_row["y_pos"], 
                    table_row["Rank_alt"] - table_row["Rank"], 0,
                    head_width=0.3, head_length=0.3,
                    length_includes_head=True, color=color, alpha=0.7)
            
            # Text
            ax.text(table_row["Rank_alt"] + adjust_position, table_row["y_pos"], str(table_row["Rank_alt"]),
                    va="bottom", fontsize=9, color=color)
        
        ax.set_yticks(table["y_pos"])
        ax.set_yticklabels(table["Equipe"])
        
        max_rank = table[["Rank","Rank_alt"]].max().max()
        ax.set_xticks(np.arange(max_rank, 0, -1))
        ax.invert_xaxis()
        ax.invert_yaxis()
        
        ax.set_title("Ranking Evolution with only away points - EPL 2021/2022", fontsize=14)
        ax.set_xlabel("Ranking")
        ax.set_ylabel("Teams")
        
        plt.tight_layout()
        
        # Convert Matplotlib figure to Div to display with Bokeh
        buf = BytesIO()
        fig.savefig(buf, format='png', bbox_inches='tight')
        plt.close(fig)
        buf.seek(0)
        img_base64 = base64.b64encode(buf.read()).decode('utf-8')
        img_html = f'<img src="data:image/png;base64,{img_base64}" width="700"/>'
        img_div = Div(text=img_html, width=700, height=700)
        
        # Info Panel
        info = Div(
            text="""
            <div style="background-color: #ecf0f1; padding: 20px; border-radius: 10px;">
                <h3>🎯 Slide Info:</h3>
                <ul>
                    <li>Green arrows: improvement in ranking</li>
                    <li>Red arrows: worse ranking</li>
                    <li>Gray arrows: no change</li>
                    <li>X-axis inverted: 1 = best ranking</li>
                </ul>
            </div>
            """,
            width=400,
            height=250
        )
        
        return layout([[title], [column(img_div), info]])

    
    def create_slide_4_leagues(self):
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

        # Title Div
        title = Div(
            text="""
            <h1 style="text-align: center; color: #2c3e50;">
                📈 Evolution of Home vs Away points in Europe's Top 5 Leagues
            </h1>
            """,
            width=800,
            height=80,
        )

        # Layout
        return layout([title,[column(row(select, date_slider), p)]])


    def update_slide(self):
        """Update the current slide display

        === CENTRAL UPDATE PATTERN ===
        This method is called whenever slide changes.
        Updates all UI elements to reflect new state.
        """

        # === WIDGET STATE MANAGEMENT ===
        # Disable navigation buttons at boundaries
        # Setting .disabled property grays out button and prevents clicks
        self.prev_button.disabled = self.current_slide == 0
        self.next_button.disabled = self.current_slide == self.total_slides - 1

        # === UPDATING DIV CONTENT ===
        # Changing .text property updates HTML content
        # Bokeh automatically syncs to browser
        self.progress_div.text = self.get_progress_html()

        # === UPDATING SELECT WIDGET ===
        # Setting .value changes selection
        # Must be string matching one of the option values
        self.slide_select.value = str(self.current_slide)

        # === UPDATING LAYOUT CHILDREN ===
        # CRITICAL: This is how to swap content in Bokeh!
        # Layout.children is a list of child elements
        # Replacing the list changes what's displayed
        # Bokeh handles all DOM updates automatically
        self.main_content.children = [self.slides[self.current_slide]]

        # Server-side logging (appears in terminal, not browser)
        print(f"Showing slide {self.current_slide + 1}: {self.get_slide_title(self.current_slide)}")

    def prev_slide(self):
        """Go to previous slide"""
        if self.current_slide > 0:
            self.current_slide -= 1
            self.update_slide()

    def next_slide(self):
        """Go to next slide"""
        if self.current_slide < self.total_slides - 1:
            self.current_slide += 1
            self.update_slide()
        elif self.auto_play:
            # Loop back to beginning in auto-play mode
            self.current_slide = 0
            self.update_slide()

    def go_home(self):
        """Go to first slide"""
        self.current_slide = 0
        self.update_slide()

    def jump_to_slide(self, attr, old, new):
        """Jump to specific slide"""
        self.current_slide = int(new)
        self.update_slide()

    def start_auto_play(self):
        """Start auto-play mode

        === PERIODIC CALLBACKS IN BOKEH ===
        Bokeh can run functions periodically (like setInterval in JavaScript)
        """
        if not self.auto_play:
            self.auto_play = True

            # === ADD_PERIODIC_CALLBACK ===
            # Schedules function to run repeatedly
            # Returns callback ID for later removal
            # Parameters:
            # - callback function (no arguments)
            # - period in milliseconds
            self.auto_play_callback = curdoc().add_periodic_callback(
                self.auto_advance,  # Function to call
                5000,  # Period: 5000ms = 5 seconds
            )

            # Update button appearance to show state
            self.play_button.label = "⏸ Pause"
            self.play_button.button_type = "warning"
            print("Auto-play started")

    def stop_auto_play(self):
        """Stop auto-play mode"""
        if self.auto_play:
            self.auto_play = False

            # === REMOVE_PERIODIC_CALLBACK ===
            # Stop the periodic execution
            # Important: Must remove callbacks to prevent memory leaks
            if self.auto_play_callback:
                curdoc().remove_periodic_callback(self.auto_play_callback)

            # Reset button appearance
            self.play_button.label = "▶ Auto Play"
            self.play_button.button_type = "success"
            print("Auto-play stopped")

    def create_layout(self):
        """Create the main layout

        === LAYOUT HIERARCHY ===
        Bokeh layouts are composable:
        - row() for horizontal arrangement
        - column() for vertical arrangement
        - layout() for grid (nested lists)
        - Spacer() for empty space (not used here)
        """

        # === NAVIGATION BAR ===
        # row() places all navigation elements horizontally
        nav_bar = row(
            self.prev_button,
            self.home_button,
            self.next_button,
            self.slide_select,
            self.progress_div,
        )

        # === MAIN CONTENT AREA ===
        # This will hold the current slide
        # Starting with first slide (index 0)
        self.main_content = column(self.slides[0])

        # === FULL APPLICATION LAYOUT ===
        # Vertical stack:
        # 1. Navigation bar
        # 2. Horizontal rule (separator)
        # 3. Main content (current slide)
        self.layout = column(
            nav_bar,  # Navigation controls
            Div(text="<hr>", width=1200, height=10),  # Visual separator
            self.main_content,  # Slide content
        )

        # Initialize display with first slide
        self.update_slide()


# === BOKEH SERVER APPLICATION ENTRY POINT ===
# This code runs when Bokeh server starts the application

# Create instance of our presentation class
presentation = InteractivePresentation()

# === ADDING TO DOCUMENT ===
# curdoc() returns the current Bokeh document
# This document is synchronized with the browser
# add_root() adds our layout as the root element
# Everything in the layout will be rendered in the browser
curdoc().add_root(presentation.layout)

# === DOCUMENT PROPERTIES ===
# Set browser tab title
curdoc().title = "Interactive Presentation"

# === SERVER-SIDE LOGGING ===
# These print statements appear in the terminal where bokeh serve is running
# Useful for debugging and monitoring server state
# Note: Users won't see these in their browser
print("=" * 50)
print("Interactive Presentation App Started!")
print("=" * 50)
print("Navigate through slides using controls")
print("All visualizations are interactive")
print("Try Auto Play for presentation mode")
print("=" * 50)

# === HOW BOKEH SERVER WORKS ===
# 1. User navigates to http://localhost:5006/06_interactive_presentation
# 2. Bokeh server creates new session for that user
# 3. This Python script runs, creating the document
# 4. Document is serialized and sent to browser
# 5. BokehJS renders the document in browser
# 6. User interactions trigger websocket messages to server
# 7. Server runs Python callbacks
# 8. Document updates are sent back to browser
# 9. BokehJS updates the display
#
# === IMPORTANT NOTES ===
# - Each user gets their own session (isolated state)
# - Python callbacks run on server (can access databases, files, etc.)
# - All data updates happen through ColumnDataSource
# - Layouts can be dynamically modified by changing .children
# - Widget states (.value, .disabled, etc.) auto-sync
# - Server maintains state between callbacks
# - Use print() for server-side debugging
# - Use Div with HTML for user-visible messages
