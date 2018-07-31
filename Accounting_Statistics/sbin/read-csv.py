#!/usr/bin/python
# -*- coding: utf-8 -*-
import csv
from collections import defaultdict
from docopt import docopt

__doc__ = """{f}

Usage:
    {f} <used_file>
    {f} -h | --help

Options:
    -h --help          Show this screen and exit.
""".format(f=__file__)

args = docopt(__doc__)
used_file = args['<used_file>']

exceed_list = []

used_f = open(used_file, 'r')

reader = csv.reader(used_f)
for row in reader:
    if float(row[5]) > 0:
        exceed_list.append(row)

l_exceed_list = len(exceed_list)
body = ""
i = 0
while i < l_exceed_list:

    prj_name = exceed_list[i][0].split("-")[0]
    sys_code = exceed_list[i][0].split("-")[1]

    if sys_code == "s":
        sys_name = "vSMP"
    else:
        sys_name = "PCCL"

    exceeded_p = exceed_list[i][5]

    # print prj_name, sys_name
    # body += '{[0]} is exceeded threshold {0[5]} % of limit {0[1]}(hours).\n'.format(exceed_list[i], prj_name, sys_name)
    body += '{} exceeded the threshold in {} and became {}%.\n'.format(prj_name, sys_name, exceeded_p)
    i += 1
print body
