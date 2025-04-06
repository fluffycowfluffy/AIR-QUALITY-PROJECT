# Imports
from urllib.request import urlopen
import re
import pandas as pd
from bs4 import BeautifulSoup
from urllib.request import urlopen
from datetime import datetime, timedelta
from tqdm import tqdm
import numpy as np

# Station Codes
ALL_STATION_CODES = ["ORN06", "ORN08", "ORN03", "BTUM", "ORN05", "AGMS", "VRKT", "ORN01", "KZBG", "TSRT", "ORN07", "RST18", "ORN02", "KUTS", "ORN04"]
TBILISI_STATION_CODES = ["AGMS", "VRKT", "ORN01", "KZBG", "TSRT"]

# Path Strings
BASE_URL = 'https://air.gov.ge/en/reports_page?'
STATIONS_URL = 'station=' + '%2C'.join(TBILISI_STATION_CODES)
DATE_URL = '&report_type=daily&date_from='

# Clean 2017 Filtered Data
interview_data = pd.read_csv('2017_filtered_data.csv')
interview_data['INT_START'] = interview_data['INT_START'].str.split(' ').str[1]
interview_data['INT_START'] = interview_data['INT_START'].str.split(':').str[0]


def run_scraper() -> None:
    '''
    Runs the scraper
    '''
    columns = ['row_id', 'date', 'code', 'hour', 'NO2', 'SO2', 'PM2.5', 'PM10', 'O3', 'CO', 'SO2_Daily', 'PM2.5_Daily', 'PM10_Daily', 'O3_Daily', 'CO_Daily']
    complete_df = pd.DataFrame(columns=columns)

    for row in tqdm(interview_data.index):
        date, start = interview_data.iloc[row][['INT_DATE', 'INT_START']]
        URL = BASE_URL + STATIONS_URL + DATE_URL + date
        html = decodeURL(URL)
        tables = html.find_all('table')
        complete_df = pd.concat([complete_df, 
                                pd.DataFrame(columns=columns, data = readTables(tables, start, row, date))],
                                ignore_index=True)
    complete_df.to_csv('Hourly_Air_Quality_Data_Tbilisi_2017.csv')
    return

def decodeURL(url:str) -> BeautifulSoup:
    '''
    Takes url, returns decoded html
    '''
    html = urlopen(url).read().decode('utf-8')
    return BeautifulSoup(html, 'html.parser')

def readTables(tables:list[str], start:str, index:int, date:str) -> list[list]:
    '''
    Takes in tables and outputs data of interest
    '''
    output = []
    for table in tables:
        code = [table.find('th').get_text().split(',')[1].split(' ')[1]]
        data_table = table.find_all('td')
        start = int(start)
        hourly_table = strip_html(data_table[start*7: start*7+7])
        daily_table = strip_html(data_table[175:180])
        output.append([index, date]+code+hourly_table+daily_table)
    return output

def strip_html(html_table:list)->list:
    '''
    Takes list and returns same list stripped of html
    '''
    for i in range(len(html_table)):
        html_table[i] = html_table[i].get_text()
    return html_table

# Run Scraper
run_scraper()

# Clean Hourly_Air_Quality_Data_Tbilisi_2017
air_data = pd.read_csv('Hourly_Air_Quality_Data_Tbilisi_2017.csv', index_col=0)
air_data.replace('*', np.nan, inplace=True)
air_data[['NO2', 'SO2','PM2.5','PM10','O3','CO','SO2_Daily','PM2.5_Daily','PM10_Daily','O3_Daily','CO_Daily']] = air_data[['NO2', 'SO2','PM2.5','PM10','O3','CO','SO2_Daily','PM2.5_Daily','PM10_Daily','O3_Daily','CO_Daily']].astype(float)

# Create Average Air Quality Dataframe
average_df = air_data.groupby(['row_id', 'date', 'hour'])[['NO2', 'SO2','PM2.5','PM10','O3','CO','SO2_Daily','PM2.5_Daily','PM10_Daily','O3_Daily','CO_Daily']].mean().reset_index()

# Save as csv
average_df.to_csv('Average_Hourly_Air_Quality_Tbilisi_2017.csv')

# Compile with demographic data
complete_df = interview_data.merge(average_df, left_on=interview_data.index, right_on='row_id').drop(columns=['row_id', 'date','INT_START'])
complete_df.to_csv('Complete_Average_Hourly_Tbilisi_2017.csv')
