# Data Visualization

## Assignment Summary

This assignment builds an interactive data storytelling app using Bokeh.
The main script creates a multi-slide presentation focused on football league statistics (season 2021-2022), including comparisons such as home vs away performance, ranking views, and league-level summaries.

## What The Work Does

- Loads and prepares tabular match/statistics data.
- Builds interactive Bokeh figures and controls (buttons, sliders, selectors, hover tooltips).
- Organizes visuals into a slide-like dashboard interface with navigation.
- Combines analytical plots and presentation-style layout in one runnable app.

## Main Files

- `data_viz_bokeh_slides.py`: Bokeh server app containing slide logic and interactive charts.
- `data/`: input data files used by the visualization code.

## Typical Output

- An interactive browser-based presentation served by Bokeh.
- Multiple linked visual views for exploratory analysis and communication.

## Run

From this folder, run:

```bash
bokeh serve data_viz_bokeh_slides.py
```

Then open the local Bokeh URL shown in the terminal.
