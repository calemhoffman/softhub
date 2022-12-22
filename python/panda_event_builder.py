#%% Build Events from Data Stored in a pandas df
import pandas as pd
import numpy as np
from datetime import datetime, timedelta

#%%
#create a panda data frame with time index, columns of tag name value
date_today = datetime.now()
days = pd.date_range(date_today, date_today + timedelta(milliseconds=10), freq='ms')
print(days)
np.random.seed(seed=1111)
data = np.random.randint(1, high=100, size=len(days))
name = ['det1','det1','q1','det1','q2','q3','det1','det1','det1','det1','det1']
df = pd.DataFrame({'time': days, 'tag': name, 'values': data})
df = df.set_index('time')
print(df)

# %% package data into a single event df_evt['time1','time2','q'...'det1'...etc.]
#each row is an event define q1 time as event time (and stop time too)
event = {'q1' : [],'q2' : [],'q3' : [], 'det1': []}
old_time = '2022-12-21 14:14:33.00000' #or 1671632073235940000 in timestamps ns
counter = 0
ave_rate = 0.
for row in df.itertuples():
    print(row.Index)
    if row.Index > pd.Timestamp(old_time):
        if row.tag != 'det1':
            event[row.tag].append(row.values)
        else:
            counter+=1
            ave_rate+=row.values
ave_rate/=counter
event['det1'].append(ave_rate)
print (event)
# %%
