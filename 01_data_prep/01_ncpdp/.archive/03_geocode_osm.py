from geopy.geocoders import Nominatim
import pandas as pd
import numpy as np
import json
from pathlib import Path

import time
startTime = time.time()


fn = 'ncpdp_cleaned_with_coords_AK_ONLY.csv'
out_fn = 'ncpdp_cleaned_with_coords_AK_ONLY_updated.csv'

df = pd.read_csv(fn)

geolocator = Nominatim(user_agent="ncpdp")

if not Path(out_fn).exists():
    df['lat'] = 'no_result'
    df['lon'] = 'no_result'
    df.to_csv(out_fn, index=False)
else:
    df = pd.read_csv(out_fn)



lats = df['lat'].values
lons = df['lon'].values

for i,v in enumerate(df['address'].values):

    if lats[i] == 'no_result':
        print(i)
        print(v)
        try:
            loc = geolocator.geocode(v)
            lat = np.round(loc.latitude, 5)
            lon = np.round(loc.longitude, 5)
        except:
            print('NO RESULT')
            lat = 'no_result'
            lon = 'no_result'
        lats[i] = lat
        lons[i] = lon
        print(lat)
        print(lon)

        df['lat'] = lats
        df['lon'] = lons

        df.to_csv(out_fn, index=False)
        
executionTime = (time.time() - startTime)
print('Execution time in seconds: ' + str(executionTime))
print('DONE')