#!/usr/bin/python
# -*- coding: utf-8 -*-

__doc__ = """{f}

Usage:
    {f} <qstat_sp_ext_xmlfile> <number_of_waiting_jobs_jsonfile>
    {f} -h | --help

Options:
    -h --help          Show this screen and exit.
""".format(f=__file__)

from docopt import docopt
import datetime
import time
import json
import xmltodict
import sys
from operator import itemgetter
from collections import defaultdict
import collections
import csv

queue_tuple = ('sma.q', 'smb.q', 'aps.q', 'single.q', 'intsmp.q', 'intmpi.q',
               'dmaL.q', 'dmaM.q', 'dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
               'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
               'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')
jc_sma_tuple = ('sma.default', 'sma.A', 'sma.B', 'sma.C', 'sma.D', 'sma.E')
jc_smb_tuple = ('smb.default', 'smb.A', 'smb.B', 'smb.C', 'smb.D', 'smb.E')
jc_dma_tuple = ('dma.default', 'dma.A', 'dma.M' 'dma.L')
jc_aps_tuple = ('aps.default', 'aps.A', 'aps.B', 'aps.C', 'aps.D', 'aps.E')

q_no_group_tuple = ('sma.q', 'smb.q', 'aps.q', 'single.q', 'intsmp.q', 'intmpi.q', 'dmaL.q', 'dmaM.q')
q_dma_group_tuple = ('dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
                     'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
                     'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')
jc_dma_group_tuple = ('dma.default', 'dma.A')


def parse():
    now = datetime.datetime.now()
    args = docopt(__doc__)
    input_filename = args['<qstat_sp_ext_xmlfile>']
    with open(filename, 'r') as f1:
        xmlString = f1.read()

    #--- qstat -s p -u '*' -ext -xml ---
    qstat_s_p_dict = xmltodict.parse(xmlString)

    number_of_waiting_jobs_dict = defaultdict(int)
    number_of_waiting_jobs_dict['Date'] = now.strftime('%Y-%m-%d %H:%M:%S')

    if qstat_s_p_dict["job_info"]["job_info"] != None:

        l_qstat_s_p_dict = len(qstat_s_p_dict['job_info']['job_info']['job_list'])

        number_of_waiting_jobs_dict['Total'] = l_qstat_s_p_dict

        i = 0
        while i < l_qstat_s_p_dict:

            job_class = qstat_s_p_dict['job_info']['job_info']['job_list'][i]['jclass_name']
            owner = qstat_s_p_dict['job_info']['job_info']['job_list'][i]['JB_owner']
            project = qstat_s_p_dict['job_info']['job_info']['job_list'][i]['JB_project']
            slots = qstat_s_p_dict['job_info']['job_info']['job_list'][i]['slots']

            number_of_waiting_jobs_dict[job_class] += 1
            number_of_waiting_jobs_dict[owner] += 1
            number_of_waiting_jobs_dict[project] += 1
            number_of_waiting_jobs_dict['slots_' + slots] += 1

            i += 1

        # print number_of_waiting_jobs_dict

    else:
        print "No Waiting Jobs"

    out = json.JSONEncoder().encode(number_of_waiting_jobs_dict)

    number_of_waiting_jobs_f = open('number_of_waiting_jobs.json', 'a')
    number_of_waiting_jobs_f.write(out + "\n")
    number_of_waiting_jobs_f.close

if __name__ == '__main__':
    parse()
