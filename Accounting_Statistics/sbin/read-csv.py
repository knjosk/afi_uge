#!/usr/bin/python
# -*- coding: utf-8 -*-
import csv
from collections import defaultdict

exceed_list = []

prj_used_f = open('/opt/uge/Accounting_Statistics/etc/prj_used_pm.csv', 'r')
reader = csv.reader(prj_used_f)
for row in reader:
    print float(row[5])
    if float(row[5]) > 0:
        exceed_list.append(row)

print exceed_list
l_exceed_list = len(exceed_list)
message = ""
i = 0
while i < l_exceed_list:
    print  exceed_list[i]
    message += '{0[0]} is exceeded threshold {0[5]} % of limit {0[1]}(hours).\n'.format(exceed_list[i]) 
    i += 1
print message

    

