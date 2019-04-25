#!/usr/bin/python
# -*- coding: utf-8 -*-
import csv
from collections import defaultdict
from docopt import docopt

__doc__ = """{f}

Usage:
    {f} <input_file>
    {f} -h | --help

Options:
    -h --help          Show this screen and exit.
""".format(f=__file__)

args = docopt(__doc__)
input_file = args['<input_file>']

prj_member_list = []

prj_member_list_f = open('/opt/uge/Accounting_Statistics/etc/prj_member.csv', 'r')

reader = csv.reader(prj_member_list_f, delimiter=',')
for row in reader:
    i = 0
    row_l = len(row)
    if len(row) > 2:
        while i < row_l:
            print row[i] + '_' + row[0] + "-s"
            print row[i] + '_' + row[0] + "-d"
            i = i + 1
