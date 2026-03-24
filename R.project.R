library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(triangle)
library(dplyr)

# ============================================================
#  CONSTANTS
# ============================================================
REV0 <- 50; TAX <- 0.21; DA <- 0.04; CAPEX <- 0.08; NWC <- 0.03
RF <- 0.042; MRP <- 0.055; RD <- 0.07; WD <- 0.15; WE <- 0.85
NET_DEBT <- 15; SHARES <- 5

# ============================================================
#  BLOOMBERG MEGA-CSS
# ============================================================
bbg_css <- "
@import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500;600;700&family=Barlow+Condensed:wght@300;400;500;600;700;800;900&display=swap');

:root {
  --bg0:      #000000;
  --bg1:      #080C10;
  --bg2:      #0C1018;
  --bg3:      #111620;
  --bg4:      #161C28;
  --border:   #1F2D3D;
  --border2:  #2A3F55;
  --neon-o:   #FF6B00;
  --neon-y:   #FFD600;
  --neon-g:   #00FF94;
  --neon-b:   #00C8FF;
  --neon-p:   #BF5FFF;
  --neon-r:   #FF3366;
  --neon-c:   #00FFD1;
  --txt:      #E8F0F8;
  --txt2:     #8BA8C4;
  --txt3:     #4A6580;
}

*, *::before, *::after { box-sizing: border-box; }

/* ---- SCANLINE OVERLAY ---- */
body::before {
  content: '';
  position: fixed; top: 0; left: 0; width: 100%; height: 100%;
  background: repeating-linear-gradient(0deg, transparent, transparent 2px, rgba(0,200,255,0.012) 2px, rgba(0,200,255,0.012) 4px);
  pointer-events: none; z-index: 9999;
}

body, .content-wrapper, .right-side, .main-footer {
  background: var(--bg1) !important;
  color: var(--txt) !important;
  font-family: 'IBM Plex Mono', monospace !important;
}

/* ---- HEADER ---- */
.main-header .logo {
  background: var(--bg0) !important;
  border-bottom: 2px solid var(--neon-o) !important;
  font-family: 'Barlow Condensed', sans-serif !important;
  font-weight: 900 !important;
  font-size: 18px !important;
  letter-spacing: 4px !important;
  color: var(--neon-o) !important;
  text-transform: uppercase;
  text-shadow: 0 0 20px rgba(255,107,0,0.8), 0 0 40px rgba(255,107,0,0.4);
}
.main-header .navbar {
  background: var(--bg0) !important;
  border-bottom: 2px solid var(--neon-o) !important;
}
.main-header .navbar .sidebar-toggle { color: var(--neon-o) !important; }

/* ---- TICKER TAPE ---- */
.ticker-wrap {
  width: 100%;
  background: var(--bg0);
  border-bottom: 1px solid var(--border2);
  overflow: hidden;
  padding: 6px 0;
  margin-bottom: 12px;
}
.ticker-inner {
  display: flex;
  gap: 48px;
  animation: ticker 30s linear infinite;
  white-space: nowrap;
  width: max-content;
}
@keyframes ticker {
  from { transform: translateX(0); }
  to   { transform: translateX(-50%); }
}
.tick-item { display: inline-flex; gap: 8px; align-items: center; font-size: 11px; }
.tick-name { color: var(--neon-b); font-weight: 600; letter-spacing: 1px; }
.tick-val  { color: var(--txt); font-weight: 500; }
.tick-up   { color: var(--neon-g); }
.tick-dn   { color: var(--neon-r); }

/* ---- SIDEBAR ---- */
.main-sidebar, .left-side {
  background: var(--bg0) !important;
  border-right: 2px solid var(--border2) !important;
}
.sidebar { background: var(--bg0) !important; padding: 0 !important; }

.sidebar-section {
  padding: 0 14px;
  margin-bottom: 6px;
}
.sidebar-section-title {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-size: 11px !important;
  font-weight: 700 !important;
  letter-spacing: 3px !important;
  text-transform: uppercase !important;
  padding: 10px 0 6px !important;
  border-bottom: 1px solid var(--border) !important;
  margin-bottom: 10px;
}

/* coloured section titles */
.sst-orange { color: var(--neon-o) !important; border-color: var(--neon-o) !important; }
.sst-cyan   { color: var(--neon-b) !important; border-color: var(--neon-b) !important; }
.sst-green  { color: var(--neon-g) !important; border-color: var(--neon-g) !important; }
.sst-purple { color: var(--neon-p) !important; border-color: var(--neon-p) !important; }

/* ---- SLIDERS ---- */
.irs--shiny .irs-bar            { background: var(--neon-o) !important; border: none !important; height: 3px !important; }
.irs--shiny .irs-line           { background: var(--border2) !important; height: 3px !important; }
.irs--shiny .irs-handle         { background: #000 !important; border: 2px solid var(--neon-o) !important; box-shadow: 0 0 8px var(--neon-o) !important; }
.irs--shiny .irs-single         { background: var(--neon-o) !important; color: #000 !important; font-family: 'IBM Plex Mono',monospace !important; font-size: 10px !important; font-weight: 700 !important; }
.irs--shiny .irs-min,
.irs--shiny .irs-max            { color: var(--txt3) !important; background: transparent !important; font-size: 9px !important; }

/* override per-slider neon colors */
.slider-cyan  .irs--shiny .irs-bar    { background: var(--neon-b) !important; }
.slider-cyan  .irs--shiny .irs-handle { border-color: var(--neon-b) !important; box-shadow: 0 0 8px var(--neon-b) !important; }
.slider-cyan  .irs--shiny .irs-single { background: var(--neon-b) !important; }

.slider-green .irs--shiny .irs-bar    { background: var(--neon-g) !important; }
.slider-green .irs--shiny .irs-handle { border-color: var(--neon-g) !important; box-shadow: 0 0 8px var(--neon-g) !important; }
.slider-green .irs--shiny .irs-single { background: var(--neon-g) !important; color: #000 !important; }

.slider-purple .irs--shiny .irs-bar    { background: var(--neon-p) !important; }
.slider-purple .irs--shiny .irs-handle { border-color: var(--neon-p) !important; box-shadow: 0 0 8px var(--neon-p) !important; }
.slider-purple .irs--shiny .irs-single { background: var(--neon-p) !important; }

/* ---- RUN BUTTON ---- */
#run {
  background: var(--neon-o) !important;
  border: none !important;
  border-radius: 3px !important;
  color: #000 !important;
  font-family: 'Barlow Condensed', sans-serif !important;
  font-size: 13px !important;
  font-weight: 900 !important;
  letter-spacing: 1.5px !important;
  text-transform: uppercase !important;
  padding: 14px 4px !important;
  width: 100% !important;
  white-space: nowrap !important;
  overflow: hidden !important;
  text-overflow: ellipsis !important;
  box-shadow: 0 0 24px rgba(255,107,0,0.6), 0 0 48px rgba(255,107,0,0.2) !important;
  transition: all .2s !important;
  margin-top: 10px;
  display: block !important;
}
#run:hover {
  background: #FFD600 !important;
  box-shadow: 0 0 32px rgba(255,214,0,0.7), 0 0 64px rgba(255,214,0,0.3) !important;
  transform: translateY(-2px) !important;
}

/* ---- VALUE BOXES ---- */
.small-box {
  border-radius: 0 !important;
  border-top: 3px solid var(--neon-o) !important;
  border-left: none !important; border-right: none !important; border-bottom: none !important;
  background: var(--bg3) !important;
  position: relative !important;
  overflow: visible !important;
  transition: all .25s !important;
  box-shadow: 0 0 0 1px var(--border), inset 0 1px 0 rgba(255,107,0,0.2) !important;
}
.small-box:hover {
  background: var(--bg4) !important;
  box-shadow: 0 0 0 1px var(--border2), 0 0 30px rgba(255,107,0,0.15) !important;
  transform: translateY(-2px) !important;
}
.small-box h3 {
  font-family: 'IBM Plex Mono', monospace !important;
  font-size: 22px !important;
  font-weight: 700 !important;
  color: var(--neon-y) !important;
  text-shadow: 0 0 12px rgba(255,214,0,0.5) !important;
  letter-spacing: 1px !important;
}
.small-box p {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-size: 12px !important;
  letter-spacing: 2.5px !important;
  text-transform: uppercase !important;
  color: var(--txt2) !important;
  font-weight: 500 !important;
}
.small-box .icon { opacity: 0.08 !important; }

/* per-box accent colours */
.vb-blue   { border-top-color: var(--neon-b) !important; }
.vb-blue   h3 { color: var(--neon-b) !important; text-shadow: 0 0 12px rgba(0,200,255,0.5) !important; }
.vb-green  { border-top-color: var(--neon-g) !important; }
.vb-green  h3 { color: var(--neon-g) !important; text-shadow: 0 0 12px rgba(0,255,148,0.5) !important; }
.vb-orange { border-top-color: var(--neon-o) !important; }
.vb-orange h3 { color: var(--neon-o) !important; text-shadow: 0 0 12px rgba(255,107,0,0.5) !important; }
.vb-purple { border-top-color: var(--neon-p) !important; }
.vb-purple h3 { color: var(--neon-p) !important; text-shadow: 0 0 12px rgba(191,95,255,0.5) !important; }
.vb-red    { border-top-color: var(--neon-r) !important; }
.vb-red    h3 { color: var(--neon-r) !important; text-shadow: 0 0 12px rgba(255,51,102,0.5) !important; }
.vb-cyan   { border-top-color: var(--neon-c) !important; }
.vb-cyan   h3 { color: var(--neon-c) !important; text-shadow: 0 0 12px rgba(0,255,209,0.5) !important; }

/* ---- BOXES ---- */
.box {
  background: var(--bg2) !important;
  border: 1px solid var(--border) !important;
  border-radius: 0 !important;
  box-shadow: none !important;
  color: var(--txt) !important;
}
.box-header {
  border-bottom: 1px solid var(--border2) !important;
  padding: 10px 16px !important;
  background: var(--bg3) !important;
}
.box-title {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-size: 12px !important;
  font-weight: 700 !important;
  letter-spacing: 3px !important;
  text-transform: uppercase !important;
  color: var(--neon-b) !important;
}

/* ---- TAB BOX ---- */
.nav-tabs-custom { background: var(--bg2) !important; border: 1px solid var(--border2) !important; border-radius: 0 !important; }
.nav-tabs-custom > .nav-tabs { border-bottom: 2px solid var(--border2) !important; background: var(--bg0) !important; }
.nav-tabs-custom > .nav-tabs > li > a {
  font-family: 'Barlow Condensed', sans-serif !important;
  font-size: 13px !important; font-weight: 700 !important;
  letter-spacing: 2px !important; text-transform: uppercase !important;
  color: var(--txt3) !important; background: transparent !important;
  border: none !important; padding: 12px 20px !important;
  border-right: 1px solid var(--border) !important;
  transition: all .15s !important;
}
.nav-tabs-custom > .nav-tabs > li.active > a,
.nav-tabs-custom > .nav-tabs > li > a:hover {
  color: var(--neon-o) !important;
  background: rgba(255,107,0,0.08) !important;
  border-bottom: 2px solid var(--neon-o) !important;
  text-shadow: 0 0 10px rgba(255,107,0,0.5) !important;
}
.nav-tabs-custom > .tab-content { background: var(--bg2) !important; padding: 14px !important; }

/* ---- STAT GRID ---- */
.stat-grid {
  display: grid;
  grid-template-columns: repeat(7, 1fr);
  gap: 2px;
  margin-bottom: 14px;
}
.stat-cell {
  background: var(--bg3);
  border: 1px solid var(--border);
  padding: 10px 8px;
  text-align: center;
  position: relative;
  transition: all .2s;
}
.stat-cell:hover { background: var(--bg4); border-color: var(--border2); }
.stat-cell::before {
  content: '';
  position: absolute; top: 0; left: 0; right: 0; height: 2px;
}
.sc-o::before { background: var(--neon-o); }
.sc-y::before { background: var(--neon-y); }
.sc-g::before { background: var(--neon-g); }
.sc-b::before { background: var(--neon-b); }
.sc-p::before { background: var(--neon-p); }
.sc-r::before { background: var(--neon-r); }
.sc-c::before { background: var(--neon-c); }
.stat-val {
  font-family: 'IBM Plex Mono', monospace;
  font-size: 13px; font-weight: 700;
  color: var(--neon-y);
}
.stat-lbl {
  font-family: 'Barlow Condensed', sans-serif;
  font-size: 9px; letter-spacing: 1.8px;
  text-transform: uppercase; color: var(--txt3);
  margin-top: 3px; font-weight: 600;
}

/* ---- SCENARIO MATRIX ---- */
.scen-table { width: 100%; border-collapse: collapse; font-family: 'IBM Plex Mono', monospace; font-size: 11px; }
.scen-table th { background: var(--bg0); color: var(--neon-b); font-size: 9px; letter-spacing: 2px; text-transform: uppercase; padding: 8px 10px; border-bottom: 1px solid var(--border2); text-align: center; }
.scen-table td { padding: 8px 10px; border-bottom: 1px solid var(--border); text-align: center; }
.scen-table tr:hover td { background: rgba(0,200,255,0.04); }
.c-g { color: var(--neon-g) !important; font-weight: 700; }
.c-y { color: var(--neon-y) !important; font-weight: 700; }
.c-r { color: var(--neon-r) !important; font-weight: 700; }
.c-b { color: var(--neon-b) !important; font-weight: 700; }

/* ---- DATATABLE ---- */
.dataTables_wrapper { color: var(--txt) !important; font-family: 'IBM Plex Mono', monospace !important; font-size: 11px !important; }
table.dataTable thead th { background: var(--bg0) !important; color: var(--neon-b) !important; border-bottom: 2px solid var(--neon-b) !important; font-size: 9px !important; letter-spacing: 2px !important; text-transform: uppercase !important; padding: 10px 12px !important; }
table.dataTable tbody tr { background: var(--bg2) !important; }
table.dataTable tbody tr:nth-child(even) { background: var(--bg3) !important; }
table.dataTable tbody tr:hover { background: rgba(0,200,255,0.06) !important; }
table.dataTable tbody td { border-top: 1px solid var(--border) !important; color: var(--txt) !important; }
.dataTables_filter input, .dataTables_length select { background: var(--bg0) !important; border: 1px solid var(--border2) !important; color: var(--txt) !important; border-radius: 2px !important; padding: 4px 8px !important; font-family: 'IBM Plex Mono',monospace !important; font-size: 11px !important; }
.dataTables_info, .dataTables_paginate { color: var(--txt3) !important; font-size: 11px !important; }
.paginate_button { color: var(--txt3) !important; border-radius: 2px !important; }
.paginate_button.current { background: var(--neon-o) !important; color: #000 !important; border: none !important; font-weight: 700 !important; }

/* ---- SCROLLBAR ---- */
::-webkit-scrollbar { width: 5px; height: 5px; }
::-webkit-scrollbar-track { background: var(--bg0); }
::-webkit-scrollbar-thumb { background: var(--border2); }
::-webkit-scrollbar-thumb:hover { background: var(--neon-o); }

/* ---- FOOTER ---- */
.bbg-footer {
  text-align: center; padding: 10px;
  font-family: 'Barlow Condensed', sans-serif;
  font-size: 10px; letter-spacing: 3px;
  color: var(--txt3); text-transform: uppercase;
  border-top: 1px solid var(--border);
  margin-top: 6px;
}
.bbg-footer span { color: var(--neon-o); }
"

# ============================================================
#  HELPERS
# ============================================================
bbg_layout <- function(fig, xtitle = "", ytitle = "") {
  fig %>% layout(
    plot_bgcolor  = "rgba(0,0,0,0)",
    paper_bgcolor = "rgba(0,0,0,0)",
    font  = list(family = "IBM Plex Mono", color = "#8BA8C4", size = 10),
    xaxis = list(title = list(text = xtitle, font = list(size = 10, color = "#4A6580")),
                 gridcolor = "#1F2D3D", zerolinecolor = "#2A3F55",
                 tickfont  = list(family = "IBM Plex Mono", size = 10, color = "#8BA8C4")),
    yaxis = list(title = list(text = ytitle, font = list(size = 10, color = "#4A6580")),
                 gridcolor = "#1F2D3D", zerolinecolor = "#2A3F55",
                 tickfont  = list(family = "IBM Plex Mono", size = 10, color = "#8BA8C4")),
    legend = list(font = list(family = "IBM Plex Mono", color = "#8BA8C4", size = 10),
                  bgcolor = "rgba(0,0,0,0)", bordercolor = "#1F2D3D"),
    hoverlabel = list(bgcolor = "#0C1018", bordercolor = "#2A3F55",
                      font = list(family = "IBM Plex Mono", color = "#E8F0F8", size = 11))
  )
}

neon_vb <- function(val, subtitle, icon_nm, css_class, color_str) {
  valueBox(val, subtitle, icon = icon(icon_nm), color = color_str) %>%
    tagAppendAttributes(class = css_class, .cssSelector = ".small-box")
}

# ============================================================
#  UI
# ============================================================
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = HTML("▮ PROJECT VITALITY")),
  
  dashboardSidebar(width = 230,
                   
                   # ---- CONFIG ----
                   tags$div(class = "sidebar-section",
                            tags$div(class = "sidebar-section-title sst-orange", "Simulation"),
                            div(class = "slider-cyan",
                                sliderInput("runs", "Iterations", 1000, 50000, 10000, step = 1000)
                            )
                   ),
                   # ---- REVENUE ----
                   tags$div(class = "sidebar-section",
                            tags$div(class = "sidebar-section-title sst-cyan", "Revenue Model"),
                            sliderInput("growth", "Growth Mean", 0.05, 0.40, 0.20, step = 0.01),
                            div(class = "slider-green",
                                sliderInput("vol", "Volatility σ", 0.01, 0.15, 0.05, step = 0.005)
                            )
                   ),
                   # ---- DISCOUNT ----
                   tags$div(class = "sidebar-section",
                            tags$div(class = "sidebar-section-title sst-green", "Discount Rate"),
                            div(class = "slider-purple",
                                sliderInput("beta", "Equity Beta", 0.8, 2.5, 1.45, step = 0.05)
                            ),
                            sliderInput("pgr", "Terminal Growth", 0.01, 0.06, 0.03, step = 0.005)
                   ),
                   # ---- CAPITAL ----
                   tags$div(class = "sidebar-section",
                            tags$div(class = "sidebar-section-title sst-purple", "Capital"),
                            sliderInput("net_debt", "Net Debt $M", 0, 60, 15, step = 1),
                            div(class = "slider-green",
                                sliderInput("shares", "Shares (M)", 1, 20, 5, step = 0.5)
                            )
                   ),
                   tags$div(style = "padding: 0 14px 16px;",
                            actionButton("run", "▶ RUN SIMULATION", width = "100%")
                   )
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML(bbg_css))),
    
    # ---- TICKER ----
    uiOutput("ticker_tape"),
    
    # ---- KPI ROW 1 ----
    fluidRow(
      column(3, uiOutput("kpi_mean")),
      column(3, uiOutput("kpi_median")),
      column(3, uiOutput("kpi_range")),
      column(3, uiOutput("kpi_ev"))
    ),
    
    # ---- KPI ROW 2 ----
    fluidRow(
      column(3, uiOutput("kpi_sd")),
      column(3, uiOutput("kpi_wacc")),
      column(3, uiOutput("kpi_upside")),
      column(3, uiOutput("kpi_skew"))
    ),
    
    # ---- PERCENTILE STRIP ----
    uiOutput("pct_strip"),
    
    # ---- MAIN TABS ----
    fluidRow(
      tabBox(width = 12,
             
             # TAB — Distribution
             tabPanel(title = "◈  Distribution",
                      fluidRow(
                        box(width = 7, title = "PRICE DISTRIBUTION HISTOGRAM",
                            plotlyOutput("hist", height = 330)),
                        box(width = 5, title = "CUMULATIVE DENSITY FUNCTION",
                            plotlyOutput("cdf",  height = 330))
                      )
             ),
             
             # TAB — Paths
             tabPanel(title = "⟿  Sim Paths",
                      fluidRow(
                        box(width = 8, title = "MONTE CARLO REVENUE PATHS",
                            plotlyOutput("paths", height = 360)),
                        box(width = 4, title = "MARGIN DISTRIBUTION",
                            plotlyOutput("margin_hist", height = 360))
                      )
             ),
             
             # TAB — Bridge
             tabPanel(title = "⇌  Bridge",
                      fluidRow(
                        box(width = 6, title = "VALUATION WATERFALL",
                            plotlyOutput("bridge", height = 360)),
                        box(width = 6, title = "WACC DECOMPOSITION",
                            plotlyOutput("wacc_pie", height = 360))
                      )
             ),
             
             # TAB — Tornado
             tabPanel(title = "⟳  Sensitivity",
                      fluidRow(
                        box(width = 8, title = "TORNADO — Δ SHARE PRICE",
                            plotlyOutput("tornado", height = 380)),
                        box(width = 4, title = "SCENARIO MATRIX",
                            uiOutput("scen_matrix"))
                      )
             ),
             
             # TAB — Scatter
             tabPanel(title = "⁘  EV vs Price",
                      box(width = 12, title = "ENTERPRISE VALUE vs EQUITY PRICE",
                          plotlyOutput("scatter", height = 400))
             ),
             
             # TAB — Box + Violin
             tabPanel(title = "◫  Distribution Shape",
                      fluidRow(
                        box(width = 6, title = "BOX PLOT — PRICE DISTRIBUTION",
                            plotlyOutput("boxplot", height = 360)),
                        box(width = 6, title = "VIOLIN — WACC vs PRICE",
                            plotlyOutput("violin", height = 360))
                      )
             ),
             
             # TAB — Data
             tabPanel(title = "≡  Raw Data",
                      box(width = 12, DTOutput("table"))
             )
      )
    ),
    
    # ---- FOOTER ----
    tags$div(class = "bbg-footer",
             HTML("PROJECT VITALITY &nbsp;|&nbsp; <span>MONTE CARLO DCF ENGINE v3.0</span> &nbsp;|&nbsp; PROPRIETARY &amp; CONFIDENTIAL")
    )
  )
)

# ============================================================
#  SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ---- SIM ----
  sim <- eventReactive(input$run, {
    n  <- input$runs
    nd <- input$net_debt
    sh <- input$shares
    set.seed(42)
    g_v  <- rnorm(n, input$growth, input$vol)
    m_v  <- rtriangle(n, 0.08, 0.25, 0.15)
    b_v  <- runif(n, max(0.8, input$beta - 0.25), min(2.5, input$beta + 0.25))
    pg_v <- rnorm(n, input$pgr, 0.006)
    ke_v <- RF + b_v * MRP
    wc_v <- WE * ke_v + WD * RD * (1 - TAX)
    price <- numeric(n); ev_v <- numeric(n); fcff_f <- numeric(n)
    for (i in seq_len(n)) {
      rev <- REV0; pv <- 0; fcff <- 0
      for (t in 1:5) {
        rev  <- rev * (1 + g_v[i])
        fcff <- rev * m_v[i] * (1 - TAX) + rev * DA - rev * CAPEX - rev * NWC
        pv   <- pv + fcff / (1 + wc_v[i])^t
      }
      fcff_f[i] <- fcff
      tv <- fcff * (1 + pg_v[i]) / (wc_v[i] - pg_v[i])
      ev_v[i]  <- pv + tv / (1 + wc_v[i])^5
      price[i] <- (ev_v[i] - nd) / sh
    }
    data.frame(price, ev = ev_v, growth = g_v, margin = m_v,
               beta = b_v, pgr = pg_v, wacc = wc_v, fcff = fcff_f)
  }, ignoreNULL = FALSE)
  
  # ---- TICKER ----
  output$ticker_tape <- renderUI({
    req(sim())
    p  <- sim()$price
    items <- list(
      list("VITALITY", paste0("$", round(mean(p), 2)), "+2.4%", TRUE),
      list("MEAN_P",   paste0("$", round(mean(p), 2)), paste0(round((mean(p)/20-1)*100,1),"%"), mean(p)>=20),
      list("EV_AVG",   paste0("$", round(mean(sim()$ev),1),"M"), "+EV", TRUE),
      list("P10",      paste0("$", round(quantile(p,.10),2)), "BEAR", FALSE),
      list("P90",      paste0("$", round(quantile(p,.90),2)), "BULL", TRUE),
      list("WACC",     paste0(round(mean(sim()$wacc)*100,2),"%"), "DISC RATE", FALSE),
      list("SIGMA",    paste0("$", round(sd(p),2)), "STDEV", FALSE),
      list("PROB>20",  paste0(round(mean(p>20)*100,1),"%"), "UPSIDE", mean(p)>50),
      list("MEDIAN",   paste0("$", round(median(p),2)), "50TH PCT", TRUE),
      list("BETA",     round(mean(sim()$beta),2), "SYS RISK", FALSE)
    )
    make_item <- function(nm, val, chg, up) {
      cls <- if (up) "tick-up" else "tick-dn"
      sym <- if (up) "▲" else "▼"
      tags$span(class = "tick-item",
                tags$span(class = "tick-name", nm),
                tags$span(class = "tick-val", val),
                tags$span(class = cls, paste(sym, chg))
      )
    }
    all_items <- lapply(items, function(x) make_item(x[[1]],x[[2]],x[[3]],x[[4]]))
    doubled   <- c(all_items, all_items)  # duplicate for infinite scroll
    tags$div(class = "ticker-wrap",
             tags$div(class = "ticker-inner", doubled)
    )
  })
  
  # ---- KPIs ----
  kpi_box <- function(val, lbl, icon_nm, top_col, txt_col) {
    tags$div(
      style = paste0(
        "background:#111620; border:1px solid #1F2D3D;",
        "border-top:3px solid ", top_col, ";",
        "padding:14px 16px; margin-bottom:10px;",
        "transition: all .2s;"
      ),
      tags$div(style = paste0("font-family:'IBM Plex Mono',monospace; font-size:20px; font-weight:700; color:", txt_col, "; text-shadow: 0 0 14px ", txt_col, "88;"), val),
      tags$div(style = "font-family:'Barlow Condensed',sans-serif; font-size:11px; letter-spacing:2.5px; text-transform:uppercase; color:#4A6580; margin-top:4px; font-weight:600;", lbl)
    )
  }
  
  output$kpi_mean   <- renderUI({ req(sim()); kpi_box(paste0("$",round(mean(sim()$price),2)), "Mean Share Price", "dollar-sign", "#FF6B00", "#FF6B00") })
  output$kpi_median <- renderUI({ req(sim()); kpi_box(paste0("$",round(median(sim()$price),2)), "Median Price", "adjust", "#00C8FF", "#00C8FF") })
  output$kpi_range  <- renderUI({
    req(sim()); p <- sim()$price
    kpi_box(paste0("$",round(quantile(p,.10),1)," – $",round(quantile(p,.90),1)), "P10 / P90 Range", "arrows-alt-h", "#00FF94", "#00FF94")
  })
  output$kpi_ev     <- renderUI({ req(sim()); kpi_box(paste0("$",round(mean(sim()$ev),1),"M"), "Avg Enterprise Value", "building", "#BF5FFF", "#BF5FFF") })
  output$kpi_sd     <- renderUI({ req(sim()); kpi_box(paste0("$",round(sd(sim()$price),2)), "Std Deviation", "chart-line", "#FF3366", "#FF3366") })
  output$kpi_wacc   <- renderUI({ req(sim()); kpi_box(paste0(round(mean(sim()$wacc)*100,2),"%"), "Avg WACC", "percent", "#00FFD1", "#00FFD1") })
  output$kpi_upside <- renderUI({ req(sim()); kpi_box(paste0(round(mean(sim()$price>20)*100,1),"%"), "Prob Price > $20", "arrow-up", "#FFD600", "#FFD600") })
  output$kpi_skew   <- renderUI({
    req(sim()); p <- sim()$price
    sk <- round((mean(p)-median(p))/sd(p), 3)
    col <- if (sk > 0) "#00FF94" else "#FF3366"
    kpi_box(sk, "Price Skewness", "wave-square", col, col)
  })
  
  # ---- PERCENTILE STRIP ----
  output$pct_strip <- renderUI({
    req(sim()); p <- sim()$price
    qs   <- quantile(p, c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99))
    lbls <- c("P1","P5","P10","P25","P50","P75","P90","P95","P99")
    nms  <- c("sc-r","sc-r","sc-r","sc-y","sc-b","sc-g","sc-g","sc-g","sc-g")
    cols <- c("#FF3366","#FF3366","#FF6B00","#FFD600","#00C8FF","#00FF94","#00FF94","#00FFD1","#00FFD1")
    
    cells <- mapply(function(q, l, cls, col) {
      tags$div(class = paste("stat-cell", cls),
               tags$div(class = "stat-val", style = paste0("color:", col), paste0("$", round(q, 2))),
               tags$div(class = "stat-lbl", l)
      )
    }, qs, lbls, nms, cols, SIMPLIFY = FALSE)
    
    tags$div(style = "margin-bottom:12px;",
             tags$div(class = "stat-grid",
                      tags$div(class = "stat-cell sc-o",
                               tags$div(class = "stat-val", style = "color:#FF6B00; font-size:11px;", paste0(input$runs)),
                               tags$div(class = "stat-lbl", "Iterations")
                      ),
                      cells,
                      tags$div(class = "stat-cell sc-p",
                               tags$div(class = "stat-val", style = "color:#BF5FFF; font-size:11px;", paste0("$",round(diff(range(sim()$price)),1))),
                               tags$div(class = "stat-lbl", "Full Range")
                      )
             )
    )
  })
  
  # ---- HISTOGRAM ----
  output$hist <- renderPlotly({
    req(sim()); p <- sim()$price
    mn  <- mean(p); med <- median(p)
    p10 <- quantile(p, 0.10); p90 <- quantile(p, 0.90)
    
    plot_ly(sim(), x = ~price, type = "histogram", nbinsx = 100,
            marker = list(
              color = "rgba(0,200,255,0.35)",
              line  = list(color = "#00C8FF", width = 0.4)
            ), name = "Freq") %>%
      add_lines(x = c(mn,mn), y = c(0, input$runs*0.045),
                line = list(color = "#FF6B00", dash = "solid", width = 2),
                name = paste0("Mean $",round(mn,2))) %>%
      add_lines(x = c(med,med), y = c(0, input$runs*0.045),
                line = list(color = "#FFD600", dash = "dash", width = 2),
                name = paste0("Median $",round(med,2))) %>%
      add_lines(x = c(p10,p10), y = c(0, input$runs*0.025),
                line = list(color = "#FF3366", dash = "dot", width = 1.5),
                name = "P10") %>%
      add_lines(x = c(p90,p90), y = c(0, input$runs*0.025),
                line = list(color = "#00FF94", dash = "dot", width = 1.5),
                name = "P90") %>%
      bbg_layout("Share Price ($)", "Frequency")
  })
  
  # ---- CDF ----
  output$cdf <- renderPlotly({
    req(sim()); x <- sort(sim()$price); y <- seq_along(x)/length(x)
    p10 <- quantile(sim()$price, .10); p90 <- quantile(sim()$price, .90)
    
    plot_ly() %>%
      add_ribbons(x = x, ymin = 0, ymax = y,
                  fillcolor = "rgba(0,200,255,0.06)",
                  line = list(width = 0)) %>%
      add_lines(x = x, y = y, line = list(color = "#00C8FF", width = 2.5), name = "CDF") %>%
      add_lines(x = c(p10,p10), y = c(0,.10),
                line = list(color = "#FF3366", dash = "dot", width = 1.5), name = "P10") %>%
      add_lines(x = c(p90,p90), y = c(0,.90),
                line = list(color = "#00FF94", dash = "dot", width = 1.5), name = "P90") %>%
      add_annotations(x = p10, y = 0.10, text = "P10",
                      font = list(color = "#FF3366", size = 10), showarrow = FALSE, xshift = -18) %>%
      add_annotations(x = p90, y = 0.90, text = "P90",
                      font = list(color = "#00FF94", size = 10), showarrow = FALSE, xshift = 18) %>%
      bbg_layout("Share Price ($)", "Probability")
  })
  
  # ---- PATHS ----
  output$paths <- renderPlotly({
    set.seed(99); n_p <- 60; yrs <- 0:5
    paths <- matrix(REV0, nrow = 6, ncol = n_p)
    for (i in seq_len(n_p)) {
      g <- rnorm(1, input$growth, input$vol)
      for (t in 2:6) paths[t,i] <- paths[t-1,i] * (1 + rnorm(1, g, input$vol*0.5))
    }
    mp <- rowMeans(paths)
    clrs <- colorRampPalette(c("#00C8FF","#BF5FFF","#FF3366"))(n_p)
    
    fig <- plot_ly()
    for (i in seq_len(n_p)) {
      fig <- fig %>% add_lines(x = yrs, y = paths[,i],
                               line = list(color = paste0(clrs[i],"30"), width = 1),
                               showlegend = FALSE, hoverinfo = "skip")
    }
    fig %>%
      add_lines(x = yrs, y = mp,
                line = list(color = "#FF6B00", width = 3, dash = "solid"),
                name = "Mean Path") %>%
      bbg_layout("Year", "Revenue ($M)")
  })
  
  # ---- MARGIN HIST ----
  output$margin_hist <- renderPlotly({
    req(sim())
    plot_ly(sim(), x = ~margin, type = "histogram", nbinsx = 50,
            marker = list(color = "rgba(191,95,255,0.45)",
                          line = list(color = "#BF5FFF", width = 0.5))) %>%
      bbg_layout("EBIT Margin", "Frequency")
  })
  
  # ---- BRIDGE ----
  output$bridge <- renderPlotly({
    req(sim())
    ev  <- mean(sim()$ev); nd <- input$net_debt; eq <- ev - nd
    cats <- c("Enterprise Value", "Less: Net Debt", "Equity Value", "Per Share")
    vals <- c(ev, -nd, eq, eq/input$shares)
    cols <- c("#00C8FF", "#FF3366", "#00FF94", "#FF6B00")
    borders <- c("#00C8FF", "#FF3366", "#00FF94", "#FF6B00")
    
    plot_ly(x = cats, y = vals, type = "bar",
            marker = list(color = paste0(cols, "55"),
                          line = list(color = cols, width = 2)),
            text = paste0(ifelse(vals<0,"-$","$"), round(abs(vals),1), ifelse(abs(vals)>5,"M","")),
            textposition = "outside",
            textfont = list(family = "IBM Plex Mono", size = 12, color = cols)) %>%
      bbg_layout("", "Value ($M)")
  })
  
  # ---- WACC PIE ----
  output$wacc_pie <- renderPlotly({
    req(sim())
    ke_wt <- WE * (RF + mean(sim()$beta) * MRP)
    kd_wt <- WD * RD * (1 - TAX)
    wacc_avg <- ke_wt + kd_wt
    
    plot_ly(labels = c("Equity Cost (Ke·We)", "After-tax Debt (Kd·Wd)"),
            values = c(ke_wt, kd_wt), type = "pie", hole = 0.60,
            marker = list(colors = c("#00C8FF", "#FF6B00"),
                          line = list(color = "#080C10", width = 3)),
            textinfo = "label+percent",
            textfont = list(family = "IBM Plex Mono", size = 10, color = "#E8F0F8")) %>%
      add_annotations(text = paste0(round(wacc_avg*100, 2), "%<br><span style='font-size:10px;color:#4A6580;'>WACC</span>"),
                      x = 0.5, y = 0.5, showarrow = FALSE,
                      font = list(family = "IBM Plex Mono", size = 18, color = "#FF6B00")) %>%
      bbg_layout()
  })
  
  # ---- TORNADO ----
  output$tornado <- renderPlotly({
    req(sim())
    
    nd <- input$net_debt; sh <- input$shares
    g0 <- input$growth;   v0 <- input$vol
    b0 <- input$beta;     p0 <- input$pgr
    
    # Simple deterministic sensitivity — shift one param at a time
    det_price <- function(g, b, pg) {
      ke   <- RF + b * MRP
      wacc <- WE * ke + WD * RD * (1 - TAX)
      rev  <- REV0; pv <- 0; fi <- 0
      for (t in 1:5) {
        rev <- rev * (1 + g)
        fi  <- rev * 0.15 * (1 - TAX) + rev * DA - rev * CAPEX - rev * NWC
        pv  <- pv + fi / (1 + wacc)^t
      }
      gap  <- max(wacc - pg, 0.005)
      ev   <- pv + fi * (1 + pg) / gap / (1 + wacc)^5
      (ev - nd) / sh
    }
    
    base_p <- det_price(g0, b0, p0)
    
    lbls <- c(
      "Growth +5pp", "Growth -5pp",
      "Beta +0.3",   "Beta -0.3",
      "Terminal +1pp","Terminal -1pp"
    )
    raw <- c(
      det_price(g0+.05, b0,      p0),
      det_price(g0-.05, b0,      p0),
      det_price(g0,     b0+0.3,  p0),
      det_price(g0,     b0-0.3,  p0),
      det_price(g0,     b0,      p0+.01),
      det_price(g0,     b0,      p0-.01)
    )
    deltas <- round(as.numeric(raw) - base_p, 2)
    
    ord    <- order(abs(deltas))
    deltas <- deltas[ord]
    lbls   <- lbls[ord]
    
    pos_col <- "#00FF94"; neg_col <- "#FF3366"
    bar_col <- ifelse(deltas >= 0, pos_col, neg_col)
    
    # Build one trace per bar to avoid vector-colour issues
    fig <- plot_ly()
    for (i in seq_along(deltas)) {
      fig <- fig %>% add_bars(
        x            = deltas[i],
        y            = lbls[i],
        orientation  = "h",
        marker       = list(color = paste0(bar_col[i],"55"),
                            line  = list(color = bar_col[i], width = 2)),
        text         = paste0(ifelse(deltas[i] >= 0, "+", ""), deltas[i]),
        textposition = "outside",
        showlegend   = FALSE,
        hoverinfo    = "text",
        hovertext    = paste0(lbls[i], ": ", ifelse(deltas[i]>=0,"+",""), deltas[i])
      )
    }
    
    fig %>%
      bbg_layout("Delta Share Price ($)", "") %>%
      layout(
        bargap = 0.35,
        yaxis  = list(categoryorder = "array", categoryarray = lbls,
                      tickfont = list(color = "#8BA8C4", size = 11)),
        xaxis  = list(zerolinecolor = "#FF6B00", zerolinewidth = 2)
      )
  })
  
  
  # ---- SCENARIO MATRIX ----
  output$scen_matrix <- renderUI({
    req(sim()); p <- sim()$price
    scens <- list(
      list("WORST",  min(p),              "#FF3366", "c-r"),
      list("BEAR",   quantile(p,.10),     "#FF6B00", "c-r"),
      list("BASE",   median(p),           "#00C8FF", "c-b"),
      list("MEAN",   mean(p),             "#FFD600", "c-y"),
      list("BULL",   quantile(p,.90),     "#00FF94", "c-g"),
      list("BEST",   max(p),              "#00FFD1", "c-g")
    )
    rows <- lapply(scens, function(s) {
      pct <- round((s[[2]]/mean(p)-1)*100, 1)
      pct_cls <- if (pct>=0) "c-g" else "c-r"
      tags$tr(
        tags$td(style = "color:#4A6580; text-align:left; font-size:10px; letter-spacing:1.5px;", s[[1]]),
        tags$td(class = s[[4]], paste0("$",round(s[[2]],2))),
        tags$td(class = pct_cls, paste0(ifelse(pct>=0,"+",""),pct,"%"))
      )
    })
    tags$table(class = "scen-table",
               tags$thead(tags$tr(tags$th("Scenario"), tags$th("Price"), tags$th("vs Mean"))),
               tags$tbody(rows)
    )
  })
  
  # ---- SCATTER ----
  output$scatter <- renderPlotly({
    req(sim()); d <- sim()
    idx <- sample(nrow(d), min(4000, nrow(d)))
    d2  <- d[idx,]
    
    plot_ly(d2, x = ~ev, y = ~price, type = "scatter", mode = "markers",
            marker = list(
              color = ~price,
              colorscale = list(c(0,"#FF3366"), c(0.33,"#FF6B00"),
                                c(0.66,"#00C8FF"), c(1,"#00FF94")),
              size = 4, opacity = 0.6, showscale = TRUE,
              colorbar = list(title = "Price $",
                              tickfont = list(family="IBM Plex Mono",size=9,color="#8BA8C4"),
                              titlefont = list(color="#4A6580",size=10))
            ),
            text = ~paste0("EV: $",round(ev,1),"M<br>Price: $",round(price,2)),
            hoverinfo = "text") %>%
      bbg_layout("Enterprise Value ($M)", "Share Price ($)")
  })
  
  # ---- BOX PLOT ----
  output$boxplot <- renderPlotly({
    req(sim())
    plot_ly(y = ~sim()$price, type = "box",
            marker    = list(color = "#FF6B00", size = 3, opacity = 0.5),
            line      = list(color = "#FF6B00"),
            fillcolor = "rgba(255,107,0,0.15)",
            boxpoints = "outliers", name = "Price") %>%
      bbg_layout("", "Share Price ($)")
  })
  
  # ---- VIOLIN ----
  output$violin <- renderPlotly({
    req(sim()); d <- sim()
    d$wacc_grp <- cut(d$wacc, breaks = 3,
                      labels = c("Low WACC","Mid WACC","High WACC"))
    clrs <- c("#00FF94","#FF6B00","#FF3366")
    
    fig <- plot_ly()
    for (i in seq_along(levels(d$wacc_grp))) {
      g   <- levels(d$wacc_grp)[i]
      sub <- d[d$wacc_grp == g,]
      fig <- fig %>% add_trace(
        y = sub$price, type = "violin",
        name = g, side = "both",
        fillcolor = paste0(clrs[i],"33"),
        line = list(color = clrs[i], width = 1.5),
        meanline = list(visible = TRUE, color = clrs[i]),
        points = FALSE
      )
    }
    fig %>% bbg_layout("WACC Bucket", "Share Price ($)")
  })
  
  # ---- DATA TABLE ----
  output$table <- renderDT({
    req(sim()); d <- sim() %>% mutate(across(where(is.numeric), ~round(.x,4)))
    colnames(d) <- c("Share Price ($)","EV ($M)","Growth","Margin","Beta","PGR","WACC","FCFF")
    datatable(d, rownames = FALSE, class = "compact hover",
              options = list(pageLength=15, scrollX=TRUE,
                             columnDefs=list(list(className="dt-center",targets="_all")))) %>%
      formatCurrency(c("Share Price ($)","EV ($M)","FCFF"),"$") %>%
      formatPercentage(c("Growth","Margin","WACC","PGR"), digits=2)
  })
}

shinyApp(ui, server)
