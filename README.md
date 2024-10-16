# Body fat project of STAT628 group 9

## Repository Overview

This repository contains all the necessary data, code, and resources for analyzing and presenting the body fat prediction application using a Shiny app. The project includes data cleaning, analysis scripts, image outputs, and relevant documentation.

### Directory Structure

-   **data/**: This folder contains the raw and cleaned datasets used in the analysis. Raw data is kept in its original form, while cleaned data is prepared for analysis.

-   **code/**: This folder includes all the code necessary for data cleaning, analysis, and generating visual outputs (such as tables and figures). It also contains the Shiny app code that serves as the base for the body fat prediction app.

-   **image/**: This folder contains figures, images, and tables generated throughout the analysis.

-   **executive_summary.pdf**: A two-page PDF summarizing the findings and key results from the analysis and prediction model.

-   **Body fat.pptx**: The final presentation slides providing an overview of the project, methodology, and results.

### How to Use the Code

1.  **Data Preparation**:
    -   Place the raw dataset(s) in the `data/` folder.
    -   If any cleaning or preprocessing is required, the cleaning scripts inside the `code/` folder will handle it. Make sure to follow the scripts' instructions or modify them as needed.
2.  **Running the Analysis**:
    -   The `code/` folder contains scripts for analyzing the data and generating results, figures, and tables. Run the appropriate scripts sequentially for data cleaning and analysis.
    -   If you need to modify the analysis (e.g., changing variables or models), adjust the R code files accordingly.
3.  **Shiny App**:
    -   The Shiny app for predicting body fat percentage is also located in the `code/` folder. The app allows users to input data and generate predictions based on the models created.

    -   Our app is hosted here, just try it!

        <https://hyang644.shinyapps.io/shiny_app/>
4.  **Viewing Results**:
    -   All graphical outputs, such as figures and tables, will be generated and saved in the `image/` folder. These can be used for reports or presentations.
    -   The final presentation slides and summary PDF are available in the root directory for easy reference.

### Additional Notes

-   **System Requirements**: This project requires R (version 3.6 or above) and the following libraries: `shiny`, `ggplot2`, `corrplot`, `dplyr`, `car`, `fmsb`.
-   **Custom Modifications**: If you wish to modify the models (e.g., adding more predictors or changing the analysis), adjust the code in the `code/` folder accordingly.

### Group Members

Hengyu Yang, Leyan Sun, Tianle Qiu, Yi Ma
