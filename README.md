# monte-carlo-simulation-r
Monte Carlo Simulation in R for probability and risk analysis
This project builds an interactive Monte Carlo DCF simulation dashboard to estimate the intrinsic value of PulsePoint AI.

The model simulates thousands of possible business outcomes and generates a probability distribution of share prices.

## Features

- 5-Year DCF Model
- Monte Carlo simulation
- Revenue growth uncertainty
- Margin distribution
- Beta distribution
- Terminal value calculation
- Enterprise to equity bridge
- Interactive visualization dashboard

## Model Inputs

Revenue: $50M  
Growth: 20%  
EBIT Margin: 15%  
Tax: 21%  
CapEx: 8%  
D&A: 4%  
NWC: 3%

Capital Structure:

Risk-free rate: 4.2%  
Market risk premium: 5.5%  
Beta: 1.45  
Cost of debt: 7%

## Output

The simulation produces:

- Share price distribution
- Mean valuation
- P10 / P90 valuation range
- Enterprise value
- Cash flow projections

## Tech Stack

- R
- Shiny
- Plotly
- Monte Carlo Simulation
- Corporate Finance (DCF)
- 
