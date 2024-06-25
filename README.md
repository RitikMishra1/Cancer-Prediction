# Cancer Prediction App

This Shiny web application predicts the likelihood of breast cancer, lung cancer, and prostate cancer based on user inputs. The application uses logistic regression models trained on respective datasets to provide predictions.

## Features

- **Breast Cancer Detection**: Predicts the likelihood of breast cancer using parameters such as radius mean, texture mean, perimeter mean, area mean, and more.
- **Lung Cancer Detection**: Predicts the likelihood of lung cancer using parameters such as age, gender, smoking habits, chronic diseases, and more.
- **Prostate Cancer Detection**: Predicts the likelihood of prostate cancer using parameters such as radius, texture, perimeter, area, and more.

## App Structure

The app has three main tabs:
1. **Breast Cancer Detection**
2. **Lung Cancer Detection**
3. **Prostate Cancer Detection**

Each tab provides:
- Information about the selected cancer type.
- An input form for users to enter their data.
- A prediction result based on the user's inputs.
- Explanations for each input parameter.

## Setup and Deployment

### Prerequisites

- R (version 4.2.1 recommended)
- RStudio (optional, but recommended for development)
- Shiny package: `install.packages("shiny")`
- Additional packages: `dplyr`, `ggplot2`

### Files

- `app.R`: Main Shiny application file.
- `Cancer_Data.csv`: Dataset for breast cancer prediction.
- `survey lung cancer.csv`: Dataset for lung cancer prediction.
- `Prostate_Cancer.csv`: Dataset for prostate cancer prediction.

### Running Locally

1. Clone the repository:
   ```sh
   git clone https://github.com/RitikMishra1/Cancer-Prediction.git
   cd Cancer-Prediction
