# CRO-AB-Lab: A Bayesian A/B Testing Shiny App

This Shiny app provides a user-friendly interface for conducting Conversion Rate Optimization (CRO) analysis on A/B/n test data using Bayesian methods. It is designed as a teaching tool for MBA students and a practical utility for marketers to quickly assess experimental results with statistical rigor.

## Key Features

-   **Easy Data Upload**: Upload your own CSV file containing experimental data.
-   **Flexible Column Mapping**: Visually map your data columns to the required inputs (ID, Variant, Outcome, etc.).
-   **Bayesian A/B/n Analysis**:
    -   Compares multiple variants against a control.
    -   Uses Bayesian posterior distributions to estimate conversion rates and percentage lift.
    -   Provides 95% credible intervals for robust decision-making.
-   **Segmentation Analysis**: Analyze performance across different user segments (e.g., by device, traffic source) to uncover nuanced insights.
-   **Sequential Stopping**: Monitor the probability of a variant outperforming the control over time to make faster decisions without sacrificing statistical rigor.
-   **What-If Simulator**: Plan future experiments by simulating outcomes with different hypothetical sample sizes.
-   **Downloadable Results**: Export a summary of your analysis to a clean CSV file for reporting.

## Getting Started

### Prerequisites

-   R installed on your system.
-   [RStudio IDE](https://www.rstudio.com/products/rstudio/download/) is highly recommended.

### How to Run the App

1.  **Clone or Download the Repository**:
    ```bash
    git clone <your-repo-url>
    cd CRO-app
    ```

2.  **Open in RStudio**: Open the `app.R` file in RStudio.

3.  **Run the App**: Click the **Run App** button in the RStudio IDE, or run the following command in the R console:
    ```r
    shiny::runApp()
    ```
    The application will automatically check for and install any missing required packages.

## File Structure

The project is organized as follows:

-   `app.R` - The main Shiny application script.
-   `data/` - Directory for sample data files.
    -   `boxbliss_demo.csv`
    -   `innovateEcho_clickstream_data.csv`
-   `README.md` - This file.
-   `requirements.txt` - A list of R package dependencies for documentation.

## Case Study Context

The `data` folder contains two sample datasets that can be downloaded from the app's UI:

-   **`boxbliss_demo.csv`**: A classic e-commerce A/B/C test for a subscription box service, focusing on overall conversion.
-   **`innovateEcho_clickstream_data.csv`**: A B2B clickstream dataset analyzing on-page engagement metrics and conversion funnel progression. This demonstrates the app's flexibility with different column names and business problems.

---
*This application was developed for educational purposes by Prof. Sudhir Voleti, ISB.*
