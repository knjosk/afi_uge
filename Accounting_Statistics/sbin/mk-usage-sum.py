#!/usr/bin/python
# -*- coding: utf-8 -*-
import csv
from collections import defaultdict
from docopt import docopt

__doc__ = """{f}

Usage:
    {f} <used_file> <used_file2>
    {f} -h | --help

Options:
    -h --help          Show this screen and exit.
""".format(f=__file__)

args = docopt(__doc__)
used_file = args['<used_file>']
used_file2 = args['<used_file2>']

exceed_list = []
used_dict = defaultdict(float)

used_f = open(used_file, 'r')

reader = csv.reader(used_f)
for row in reader:
    used_dict[row[0]] = row[1:]

used_f2 = open(used_file2, 'r')
reader = csv.reader(used_f2)
header = next(reader)
for row in reader:
    used_dict[row[0]][1] += float(row[2])
    used_dict[row[0]][2] += float(row[3])
    used_dict[row[0]][3] += float(row[4])
    # print row[0]
    print row[0], used_dict[row[0]]
