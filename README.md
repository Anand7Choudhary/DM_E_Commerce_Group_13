# DM_E_Commerce_Group_13

## Welcome, Papa Nikos!

<img src="https://raw.githubusercontent.com/Anand7Choudhary/blog/master/image/DALL%C2%B7E%202024-02-29%2015.52.25%20-%20Transform%20the%20scene_%20The%20programmer%20is%20now%20depicted%20as%20a%20king%2C%20complete%20with%20a%20regal%20crown%20and%20a%20majestic%20robe%2C%20sitting%20on%20a%20throne%20made%20of%20computer%20p.webp" width="400" alt="Welcome Papa Nikos">

# E-Commerce Data Management Project

## Overview

This project simulates a real-world e-commerce data environment, encompassing end-to-end data management from database design to data analysis and reporting. It utilizes SQLite for database management, GitHub Actions for automation, and Quarto with R for data analysis and reporting.

## Table of Contents

- [Database Design and Implementation](#database-design-and-implementation)
- [Data Generation and Management](#data-generation-and-management)
- [GitHub Repository and Workflow Setup](#github-repository-and-workflow-setup)
- [Data Analysis and Reporting with Quarto in R](#data-analysis-and-reporting-with-quarto-in-r)
- [Logging Invalid Data Entries](#logging-invalid-data-entries)

## Database Design and Implementation

### E-R Diagram Design

We designed a detailed E-R diagram for our e-commerce database considering various entities like Products, Customers, Payments etc., with appropriate relationships.

### SQL Database Schema Creation

The E-R diagram was translated into a functional SQL database schema, eliminating data redundancy.

## Data Generation and Management

### Synthetic Data Generation

We used R to generate synthetic data that realistically simulated an e-commerce environment.

### Data Import and Quality Assurance

Scripts were implemented for data import, with checks for data quality and integrity, such as email format validation and referential integrity.

## GitHub Repository and Workflow Setup

Our GitHub repository contains the database file, scripts, and other necessary documents, managed and version-controlled through GitHub.

## GitHub Actions for Continuous Integration

We've set up GitHub Actions to automate data validation, database updates, and basic data analysis tasks. These actions are triggered by specific events like push or pull requests.

## Data Analysis and Reporting with Quarto in R

Advanced data analysis is conducted using R, and comprehensive reporting is done through Quarto, presenting findings suitable for non-technical stakeholders.

## Logging Invalid Data Entries

Each table entity is associated with a log file that stores the list of rows that did not pass validation. These log files are generated and updated via a GitHub Action whenever data is imported or validated.
