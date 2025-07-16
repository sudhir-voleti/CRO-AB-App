# CRO Lab: A Bayesian A/B Testing Shiny App

This Shiny app provides a user-friendly interface for conducting Conversion Rate Optimization (CRO) analysis on A/B/n test data using Bayesian methods. It is designed as a teaching tool for MBA students and a practical utility for marketers to quickly assess experimental results with statistical rigor.

## Key Features

-   **Easy Data Upload:** Upload your own CSV file containing experimental data.
-   **Flexible Column Mapping:** Visually map your data columns to the required inputs (ID, Variant, Outcome, etc.).
-   **Bayesian A
CRO-app/
│
├── app.R               # The main Shiny application script
├── data/                 # Directory for sample data
│   ├── boxbliss_demo.csv
│   └── innovateEcho_clickstream_data.csv
├── README.md             # This file
└── requirements.txt      # List of R package dependencies
