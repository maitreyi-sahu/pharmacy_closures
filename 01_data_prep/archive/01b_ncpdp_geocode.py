from mapbox import Geocoder
import pandas as pd
import json
from pathlib import Path

import time
startTime = time.time()


token = "pk.eyJ1IjoibWl0YXNhaHUiLCJhIjoiY2xuems2a2N2MHRjazJqcXA3eW82aDJrMSJ9.leG_32BTnCJ2kGP9SH5m3Q"
geocoder = Geocoder(access_token=token)


df = pd.read_csv('ncpdp_cleaned.csv')
df['address'] = df['address_street'] + ',' + df['address_city']  + ',' + df['address_state'] + ',US'

if not Path('ncpdp_cleaned_with_lat_lon.csv').exists():
    df['lat'] = 'no_result'
    df['lon'] = 'no_result'
    df.to_csv('ncpdp_cleaned_with_lat_lon.csv')
else:
    df = pd.read_csv('ncpdp_cleaned_with_lat_lon.csv')



lats = df['lat'].values
lons = df['lon'].values

for i,v in enumerate(df['address'].values):

    if lats[i] == 'no_result':
        print(i)
        print(v)
        response = geocoder.forward(v)
        coords = str(response.json()['features'][0]['center'])
        lon, lat = coords.replace(']', '').replace('[', '').replace(', ', ',').split(',')
        lon, lat = float(lon), float(lat)
        lats[i] = lat
        lons[i] = lon
        print(lat)
        print(lon)

        df['lat'] = lats
        df['lon'] = lons

        df.to_csv('ncpdp_cleaned_with_lat_lon.csv')
        
executionTime = (time.time() - startTime)
print('Execution time in seconds: ' + str(executionTime))
print('DONE')