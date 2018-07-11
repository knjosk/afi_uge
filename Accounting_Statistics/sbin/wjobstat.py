#!/usr/bin/python
# -*- coding: utf-8 -*-

__doc__ = """{f}

Usage:
    {f} <qstat_gc_ext_xmlfile> <qstat_r_xmlfile>
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


def parse():
    now = datetime.datetime.now()
    args = docopt(__doc__)
    filename1 = args['<qstat_gc_ext_xmlfile>']
    with open(filename1, 'r') as f1:
        xmlString = f1.read()
    filename2 = args['<qstat_r_xmlfile>']
    with open(filename2, 'r') as f2:
        xmlString2 = f2.read()

    #--- qstat -g -c -xml ---
    qstat_g_c_dict = xmltodict.parse(xmlString)
    l_qstat_g_c_dict = len(qstat_g_c_dict["job_info"]["cluster_queue_summary"])

    i = 0
    avail_cores_dict = {}
    queue_list = []
    while i < l_qstat_g_c_dict:
        q_name = qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["name"].split(".")[0]
        a_cores = int(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["available"])
        avail_cores_dict[q_name] = a_cores
        queue_list.append(q_name)
        i += 1

    running_job_dict = {}
    for i in queue_list:
        running_job_dict[i] = []

    qstat_r_dict = xmltodict.parse(xmlString2)

    if qstat_r_dict["job_info"]["queue_info"] != None:

        l_queue_info_qstat_r_dict = len(qstat_r_dict["job_info"]["queue_info"]["job_list"])
        if type(qstat_r_dict["job_info"]["queue_info"]["job_list"]) == list:
            i = 0
            while i < l_queue_info_qstat_r_dict:
                running_job_number = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JB_job_number"]
                start_time = datetime.datetime.strptime(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JAT_start_time"], '%Y-%m-%dT%H:%M:%S.%f')
                start_utime = int(time.mktime(start_time.timetuple()))
                exec_queue_name = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["queue_name"].split(".")[0]
                job_jcl = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["jclass_name"]
                used_cores = int(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["slots"])
                elaps = int(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["hard_request"][-2]["#text"])
                end_utime = start_utime + elaps
                end_time = datetime.datetime.fromtimestamp(end_utime)
                running_job_dict[exec_queue_name].append([end_utime, used_cores, end_time, running_job_number])
                i += 1
        else:
            running_job_number = qstat_r_dict["job_info"]["queue_info"]["job_list"]["JB_job_number"]
            start_time = datetime.datetime.strptime(qstat_r_dict["job_info"]["queue_info"]["job_list"]["JAT_start_time"], '%Y-%m-%dT%H:%M:%S.%f')
            start_utime = int(time.mktime(start_time.timetuple()))
            exec_queue_name = qstat_r_dict["job_info"]["queue_info"]["job_list"]["queue_name"].split(".")[0]
            job_jcl = qstat_r_dict["job_info"]["queue_info"]["job_list"]["jclass_name"]
            used_cores = int(qstat_r_dict["job_info"]["queue_info"]["job_list"]["slots"])
            elaps = int(qstat_r_dict["job_info"]["queue_info"]["job_list"]["hard_request"][-2]["#text"])
            end_utime = start_utime + elaps
            end_time = datetime.datetime.fromtimestamp(end_utime)
            running_job_dict[exec_queue_name].append([end_utime, used_cores, end_time, running_job_number])

        for q in queue_list:
            running_job_dict[q].sort(key=itemgetter(0, 3))

        #---

        waiting_job_dict = {}
        for i in queue_list:
            waiting_job_dict[i] = []

        if qstat_r_dict["job_info"]["job_info"] != None:
            l_job_info_qstat_r_dict = len(qstat_r_dict["job_info"]["job_info"]["job_list"])
            if type(qstat_r_dict["job_info"]["job_info"]["job_list"]) == list:
                i = 0
                list2 = []
                while i < l_job_info_qstat_r_dict:
                    waiting_job_number = qstat_r_dict["job_info"]["job_info"]["job_list"][i]["JB_job_number"]
                    waiting_job_prio = float(qstat_r_dict["job_info"]["job_info"]["job_list"][i]["JAT_prio"])
                    waiting_job_name = qstat_r_dict["job_info"]["job_info"]["job_list"][i]["JB_name"]
                    waiting_job_user = qstat_r_dict["job_info"]["job_info"]["job_list"][i]["JB_owner"]
                    waiting_job_jcl = qstat_r_dict["job_info"]["job_info"]["job_list"][i]["jclass_name"]
                    waiting_queue_name = waiting_job_jcl.split(".")[0]
                    waiting_cores = int(qstat_r_dict["job_info"]["job_info"]["job_list"][i]["slots"])
                    waiting_job_elaps = int(qstat_r_dict["job_info"]["job_info"]["job_list"][i]["hard_request"][-2]["#text"])
                    waiting_job_dict[waiting_queue_name].append([waiting_job_number, waiting_job_prio, waiting_job_name, waiting_job_user, waiting_job_jcl, waiting_cores])
                    i += 1
            else:
                waiting_job_number = qstat_r_dict["job_info"]["job_info"]["job_list"]["JB_job_number"]
                waiting_job_prio = float(qstat_r_dict["job_info"]["job_info"]["job_list"]["JAT_prio"])
                waiting_job_name = qstat_r_dict["job_info"]["job_info"]["job_list"]["JB_name"]
                waiting_job_user = qstat_r_dict["job_info"]["job_info"]["job_list"]["JB_owner"]
                waiting_job_jcl = qstat_r_dict["job_info"]["job_info"]["job_list"]["jclass_name"]
                waiting_queue_name = waiting_job_jcl.split(".")[0]
                waiting_cores = int(qstat_r_dict["job_info"]["job_info"]["job_list"]["slots"])
                waiting_job_elaps = int(qstat_r_dict["job_info"]["job_info"]["job_list"]["hard_request"][-2]["#text"])
                waiting_job_dict[waiting_queue_name].append([waiting_job_number, waiting_job_prio, waiting_job_name, waiting_job_user, waiting_job_jcl, waiting_cores])

            for q in queue_list:
                waiting_job_dict[q].sort(key=lambda x: (-x[1], x[0]))

            for q in queue_list:
                number_of_jobs_running_on_queue = len(running_job_dict[q])
                number_of_jobs_waiting_on_queue = len(waiting_job_dict[q])
                i = 0
                j = 0
                while i < number_of_jobs_running_on_queue:
                    avail_cores_dict[q] += running_job_dict[q][i][1]
                    # print avail_cores_dict[q]
                    while j < number_of_jobs_waiting_on_queue:
                        req_cores = waiting_job_dict[q][j][-1]
                        if avail_cores_dict[q] >= req_cores:
                            # print q, waiting_job_dict[q][j], running_job_dict[q][i][2]
                            print('{} {} {} {} {}'.format(*waiting_job_dict[q][j]) + running_job_dict[q][i][2].strftime(' %Y/%m/%d %H:%M:%S'))
                            avail_cores_dict[q] -= req_cores
                            j += 1
                        break
                    i += 1

if __name__ == '__main__':
    parse()
