"""
Purpose: Using Selenium to get all the reports from Ohio Auditor of State's website
Created by: Saani Rawat
Created on: 02/21/2025

Change Log:
02/22/2025: Re-ran the code for last-half-of-the-list due to UC-wide power outage.
"""

# packages
import os
import pandas as pd
import requests
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.common.by import By 
from selenium.webdriver.support.ui import Select
import re
import logging
import sys
import time

# Paths
root = "C:/Users/rawatsa/OneDrive - University of Cincinnati/StataProjects/ohio_taxation/"
data_path = root + "data/"
reports_path = root + "data/spending reports/all/"

#-------------------------------------------------------------------------------------------------------------#
# Code to get township names
#-------------------------------------------------------------------------------------------------------------#

# importing roads_and_census & subdivision names data
stata_file = os.path.join(data_path, "roads_and_census.dta")
df_stata = pd.read_stata(stata_file)
excel_path = os.path.join(data_path, "ohio-only-all-geocodes-2016.xlsx")

# cleaning names data
df_excel = pd.read_excel(excel_path)
df_excel = df_excel[["TENDIGIT_FIPS", "name (note if split between two counties)", "county name", "split_flag"]]
df_excel.columns = ["tendigit_fips", "county_subdivision", "county", "split_flag"]
df_excel.loc[:, 'county_subdivision'] = df_excel.apply(
    lambda row: re.sub(r"(village|city).*", r"\1", row['county_subdivision']).strip() if row['split_flag'] == 1 else row['county_subdivision'],
    axis=1
)
df_excel.loc[:, 'county_subdivision'] = df_excel['county_subdivision'].str.lower().str.replace(r'(village|township|county|city).*', r'\1', regex=True)

# storing tendigit_fips, county_subdivision and county names as a tuple
area_names = list(zip(df_excel['tendigit_fips'], df_excel['county_subdivision'], df_excel['county']))

counter = 0
for fips, subdivision, county in area_names:
    if str(3917553942) in str(fips):
        print(f"Index of {str(3917553942)} ", counter)
    counter += 1

# Get list of all PDF files in the reports_path
pdf_files = sorted(list(set(f.split('_')[0] for f in os.listdir(reports_path) if f.endswith('.pdf'))))
print(pdf_files)
len(pdf_files)

# Get list of all FIPS in the df_excel
fips_list = sorted(list(set(df_excel['tendigit_fips'].tolist())))
len(fips_list)
len(fips_list)- len(pdf_files)


# Find all the elements in fips_list that are not in pdf_files
missing_fips = [fips for fips in fips_list if str(fips) not in pdf_files]
print(f"Missing FIPS: {missing_fips}")
len(missing_fips)

# Subset area_names to only include elements with fips in missing_fips
area_names_ = [area for area in area_names if area[0] in missing_fips]
len(area_names_)

# Remove elements with tendigit_fips ending in five zeros
area_names_2 = [area for area in area_names_ if not str(area[0]).endswith('00000')]
len(area_names_)
len(area_names_2)

# [cosub for _, cosub, _ in area_names_]

#-------------------------------------------------------------------------------------------------------------#
# Selenium Code Setup to Download Reports
#-------------------------------------------------------------------------------------------------------------#

def run_dynamic_downloads(area_names, data_path):
    driver = webdriver.Chrome()
    base_url = "https://ohioauditor.gov/auditsearch/search.aspx"

    for fips, subdivision, county in area_names:
        try:
            entity_type = None
            if "township" in subdivision.lower():
                entity_type = "Township"
            elif "city" in subdivision.lower():
                entity_type = "City"
            elif "village" in subdivision.lower():
                entity_type = "Village"
            else:
                continue

            driver.get(base_url)
            driver.find_element(By.NAME, "txtQueryString").clear()
            search_query = re.sub(r'\b(township|village|city|county)\b', '', subdivision, flags=re.IGNORECASE).strip()
            search_query = re.sub(r'\(.*\)', '', search_query).strip()
            driver.find_element(By.NAME, "txtQueryString").send_keys(search_query)

            entity_dropdown = Select(driver.find_element(By.ID, "ddlEntityType"))
            entity_dropdown.select_by_value(entity_type)

            report_dropdown = Select(driver.find_element(By.ID, "ddlReportType"))
            report_dropdown.select_by_value("Financial Audit")

            county_dropdown = Select(driver.find_element(By.ID, "ddlCounty"))
            county_dropdown.select_by_visible_text(county.capitalize())

            driver.find_element(By.ID, "btnSubmitSearch").click()
            print(f"Searching for {fips}, {subdivision} in {county}")
            print(f"Will look for this in the search results: {search_query.capitalize()}")
            # links = driver.find_elements(By.XPATH, f"//a[contains(text(), '{subdivision.capitalize().split(' ')[0]}')]")
            # links = driver.find_elements(By.XPATH, f"//a[contains(text(), '{' '.join(word.capitalize() for word in search_query.split(' '))}')]")            
            print(f"Found {len(links)} reports for {fips}, {subdivision} in {county}")
            if not links:
                print(f"No reports found for {fips}, {subdivision} in {county}")
                continue

            count = 0
            for link in links:
                count += 1
                link.click()
                report_link = driver.find_element(By.ID, "hlReport")
                report_url = report_link.get_attribute("href")

                file_name = f"{fips}_{subdivision}_{county}_{count}.pdf"
                pdf_path = os.path.join(data_path, file_name)
                response = requests.get(report_url)
                with open(pdf_path, 'wb') as file:
                    file.write(response.content)
                driver.back()

        except:
            print(f"There was an error with {fips}, {subdivision} in {county}.")

    driver.quit()


# run_dynamic_downloads(area_names[0:1], reports_path)

#-------------------------------------------------------------------------------------------------------------#
# DOWNLOAD ALL OHIO AUDITOR OF STATE REPORTS
#-------------------------------------------------------------------------------------------------------------#

LOG_PATH = os.path.join(reports_path, "download_spending_reports4.log")

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s - %(levelname)s - %(message)s",
    handlers=[
        logging.FileHandler(LOG_PATH),
        logging.StreamHandler(sys.stdout)
    ]
)

start_time = time.time()

run_dynamic_downloads(area_names_2, reports_path)

elapsed_time = time.time() - start_time
logging.info(f"Run completed in {elapsed_time:.2f} seconds")

# Note: I think some of these remaining cases are having trouble clicking on the pdf links.
# Need to double check how I am identifying the links and if there is a better way to do it.
def open_new_tabs(area_names, data_path):
    driver = webdriver.Chrome()
    base_url = "https://ohioauditor.gov/auditsearch/search.aspx"
    main_tab = driver.current_window_handle

    for fips, subdivision, county in area_names:
        try:
            entity_type = None
            if "township" in subdivision.lower():
                entity_type = "Township"
            elif "city" in subdivision.lower():
                entity_type = "City"
            elif "village" in subdivision.lower():
                entity_type = "Village"
            else:
                continue

            # Open a new tab for this search
            driver.execute_script("window.open('');")
            new_tab = driver.window_handles[-1]
            driver.switch_to.window(new_tab)
            driver.get(base_url)

            driver.find_element(By.NAME, "txtQueryString").clear()
            search_query = re.sub(
                r'\b(township|village|city|county)\b', '', subdivision, flags=re.IGNORECASE
            ).strip()
            search_query = re.sub(r'\(.*\)', '', search_query).strip()
            driver.find_element(By.NAME, "txtQueryString").send_keys(search_query)

            Select(driver.find_element(By.ID, "ddlEntityType")).select_by_value(entity_type)
            Select(driver.find_element(By.ID, "ddlReportType")).select_by_value("Financial Audit")
            Select(driver.find_element(By.ID, "ddlCounty")).select_by_visible_text(county.capitalize())
            driver.find_element(By.ID, "btnSubmitSearch").click()

            print(f"Searching for {fips}, {subdivision} in {county}")
   
        except Exception as e:
            print(f"There was an error with {fips}, {subdivision} in {county}: {e}")
            if len(driver.window_handles) > 1:
                # If needed, close the current tab and switch back to the main tab
                driver.close()
                driver.switch_to.window(main_tab)

    # Do not close the driver so the Chrome browser and tabs remain open.
    return driver

    # driver.quit()


open_new_tabs(area_names_2[120:], reports_path)