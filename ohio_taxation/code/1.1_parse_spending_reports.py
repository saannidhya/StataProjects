"""
Purpose: Parse the spending reports downloaded from Ohio Auditor of State's website
Created by: Saani Rawat
Created on: 05/08/2025

Change Log:
05/08/2025: create the code to parse the spending reports downloaded from the Ohio Auditor of State's website. 
"""


# packages
import os
import pandas as pd
import re

# locations
root = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/"
data_path = root + "data/"
reports_path = root + "data/spending reports/all/"
out_reports_path = os.path.join(reports_path, "parsed/")

#-------------------------------------------------------------------------------------------------------------#
# Test the code on a single file
#-------------------------------------------------------------------------------------------------------------#

# Get the list of all PDF files in the reports_path
pdf_files = [f for f in os.listdir(reports_path) if f.endswith('.pdf')]
print(pdf_files)
fips = sorted(list(set(f.split('_')[0] for f in os.listdir(reports_path) if f.endswith('.pdf'))))

file = out_reports_path + pdf_files[0]
