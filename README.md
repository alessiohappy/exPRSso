# exPRSso

[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)

A web application for assess the reproducibility of a Polygenic Risk Score (PRS).

## Overview

exPRSso is a web-based tool that allows researchers to reproduce a PRS starting from your genetic data and a published PRS summary. The application provides an intuitive interface for uploading genetic data files and visualizing PRS results.

## Features

- Upload and process MAP and PED files
- Import a published PRS summary
- Calculate weighted and unweighted PRS
- Interactive visualization of results
- Export results in CSV format

## Access

The application is available at: [[exPRSso]](https://alessiohappy.github.io/exPRSso/)

## How to Use

1. Upload your MAP file
2. Upload your PED file
3. Upload your summary statistics file
4. Click "Calculate PRS" to generate results
5. View the visualizations and download results as needed

## Data Requirements

- MAP file format: a variant information file consisting of four columns: chromosome, SNP identifier, position in cM (optional), and base-pair coordinates.
- PED file format: a pedigree file, usually related to a .MAP file. This file consists of 6 initial columns (IID, FID, PID, MID, Sex, Phenotype) and a variable number of columns for the alleles.
- Published PRS summary file format: a file reporting all information related to a published PRS. This should contain three columns: SNP (variant identifier, refseq), A1 (effect allele), and OR (effect).

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Contact

[Alessio Felici/alessio.felici@phd.unipi.it] - For support or any questions on the use of the application.
