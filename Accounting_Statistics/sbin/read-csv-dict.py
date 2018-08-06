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

user_limit_dict = defaultdict(float)

user_limit_f = open('/opt/uge/Accounting_Statistics/etc/user_limit_py.csv', 'r')

reader = csv.reader(user_limit_f)
header = next(reader)
for row in reader:
    user_limit_dict[row[0]] = float(row[1])

print user_limit_dict


used_f = open(used_file, 'r')

reader = csv.DictReader(used_f)
for row in reader:
    print row
