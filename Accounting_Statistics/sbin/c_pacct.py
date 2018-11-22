#!/usr/bin/python

import pandas as pd

df = pd.read_csv('../../sample/pacct-20181026.txt', sep='|', header=None, usecols=[0, 2, 3, 5, 6, 7])

print(df.head(10))
