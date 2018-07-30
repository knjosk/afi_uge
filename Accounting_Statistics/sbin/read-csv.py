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
message = ""
i = 0
while i < l_exceed_list:
    message += '{0[0]} is exceeded threshold {0[5]} % of limit {0[1]}(hours).\n'.format(exceed_list[i])
    i += 1
print message
