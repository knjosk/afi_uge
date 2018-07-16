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
from collections import defaultdict
import csv

queue_tuple = ('sma.q', 'smb.q', 'aps.q', 'single.q', 'intsmp.q', 'intmpi.q', 'intsmp.q', 'intmpi.q', 'mpi1.q', 'mpi2.q', 'smp.q', 'smp1.q', 'smp2.q',
               'dmaL.q', 'dmaM.q', 'dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
               'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
               'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')
jc_sma_tuple = ('sma.default', 'sma.A', 'sma.B', 'sma.C', 'sma.D', 'sma.E')
jc_smb_tuple = ('smb.default', 'smb.A', 'smb.B', 'smb.C', 'smb.D', 'smb.E')
jc_dma_tuple = ('dma.default', 'dma.A', 'dma.M' 'dma.L')
jc_aps_tuple = ('aps.default', 'aps.A', 'aps.B', 'aps.C', 'aps.D', 'aps.E')

q_no_group_tuple = ('sma.q', 'smb.q', 'aps.q', 'single.q', 'intsmp.q', 'intmpi.q', 'intsmp.q', 'intmpi.q', 'dmaL.q', 'dmaM.q',
                    'mpi1.q', 'mpi2.q', 'smp.q', 'smp1.q', 'smp2.q')
q_dma_group_tuple = ('dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
                     'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
                     'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')
jc_dma_group_tuple = ('dma.default', 'dma.A')


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
    avail_cores_dict = defaultdict(int)
    load_dict = defaultdict(float)
    used_dict = defaultdict(int)
    resv_dict = defaultdict(int)
    available_dict = defaultdict(int)
    total_dict = defaultdict(int)
    aoacds_dict = defaultdict(int)
    cdsue_dict = defaultdict(int)

    queue_list = []
    while i < l_qstat_g_c_dict:
        q_name = qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["name"]
        a_cores = int(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["available"])
        avail_cores_dict[q_name] = a_cores
        if q_name != "intsmp.q":
            load_dict[q_name] = float(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["load"])
        used_dict[q_name] = int(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["used"])
        resv_dict[q_name] = int(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["resv"])
        available_dict[q_name] = int(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["available"])
        total_dict[q_name] = int(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["total"])
        aoacds_dict[q_name] = int(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["temp_disabled"])
        cdsue_dict[q_name] = int(qstat_g_c_dict["job_info"]["cluster_queue_summary"][i]["manual_intervention"])

        queue_list.append(q_name)
        i += 1

    running_job_dict = {}
    for i in queue_tuple:
        running_job_dict[i] = []
    running_job_dict["dma"] = []

    qstat_r_dict = xmltodict.parse(xmlString2)

    if qstat_r_dict["job_info"]["queue_info"] != None:

        l_queue_info_qstat_r_dict = len(qstat_r_dict["job_info"]["queue_info"]["job_list"])
        #-- 実行中ジョブが1本の場合、listにならないため
        if type(qstat_r_dict["job_info"]["queue_info"]["job_list"]) == list:
            i = 0
            while i < l_queue_info_qstat_r_dict:
                running_job_number = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JB_job_number"]
                prio = float(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JAT_prio"])
                job_name = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JB_name"]
                owner = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JB_owner"]
                state = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["state"]
                start_time = datetime.datetime.strptime(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JAT_start_time"], '%Y-%m-%dT%H:%M:%S.%f')
                start_utime = int(time.mktime(start_time.timetuple()))
                queue_name = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["queue_name"]
                exec_queue_name = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["queue_name"].split("@")[0]
                job_jcl = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["jclass_name"]
                used_cores = int(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["slots"])
                l_hard_request_list = len(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["hard_request"])

                elaps = 0
                j = 0
                while j < l_hard_request_list:
                    if qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["hard_request"][j]["@name"] == "h_rt":
                        elaps = int(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["hard_request"][j]["#text"])
                    j += 1

                end_utime = start_utime + elaps
                end_time = datetime.datetime.fromtimestamp(end_utime)
                running_job_dict[exec_queue_name].append([end_utime, used_cores, end_time, running_job_number, prio, job_name, owner, state, start_time,
                                                          queue_name, job_jcl, exec_queue_name])
                if exec_queue_name in q_dma_group_tuple:
                    running_job_dict["dma"].append([end_utime, used_cores, end_time, running_job_number, prio, job_name, owner, state, start_time,
                                                    queue_name, job_jcl, exec_queue_name])

                i += 1

        else:
            running_job_number = qstat_r_dict["job_info"]["queue_info"]["job_list"]["JB_job_number"]
            prio = float(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JAT_prio"])
            job_name = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JB_name"]
            owner = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["JB_owner"]
            state = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["state"]
            start_time = datetime.datetime.strptime(qstat_r_dict["job_info"]["queue_info"]["job_list"]["JAT_start_time"], '%Y-%m-%dT%H:%M:%S.%f')
            start_utime = int(time.mktime(start_time.timetuple()))
            queue_name = qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["queue_name"]
            exec_queue_name = qstat_r_dict["job_info"]["queue_info"]["job_list"]["queue_name"].split("@")[0]
            job_jcl = qstat_r_dict["job_info"]["queue_info"]["job_list"]["jclass_name"]
            used_cores = int(qstat_r_dict["job_info"]["queue_info"]["job_list"]["slots"])

            elaps = 0
            j = 0
            while j < l_hard_request_list:
                if qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["hard_request"][j]["@name"] == "h_rt":
                    elaps = int(qstat_r_dict["job_info"]["queue_info"]["job_list"][i]["hard_request"][j]["#text"])
                j += 1

            end_utime = start_utime + elaps
            end_time = datetime.datetime.fromtimestamp(end_utime)
            running_job_dict[exec_queue_name].append([end_utime, used_cores, end_time, running_job_number, prio, job_name, owner, state, start_time,
                                                      queue_name, job_jcl, exec_queue_name])
            if exec_queue_name in q_dma_group_tuple:
                running_job_dict["dma"].append([end_utime, used_cores, end_time, running_job_number, prio, job_name, owner, state, start_time,
                                                queue_name, job_jcl, exec_queue_name])

        for q in queue_tuple:
            running_job_dict[q].sort(key=itemgetter(0, 3))
        running_job_dict["dma"].sort(key=itemgetter(0, 3))

        #---

        waiting_job_dict = {}
        for i in queue_tuple:
            waiting_job_dict[i] = []
        waiting_job_dict["dma"] = []

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
                    waiting_queue_name = waiting_job_jcl.split(".")[0] + ".q"
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

                if waiting_job_jcl == "dma.L":
                    waiting_queue_name = "dmaL.q"
                elif waiting_job_jcl == "dma.M":
                    waiting_queue_name = "dmaM.q"
                else:
                    waiting_queue_name = waiting_job_jcl.split(".")[0] + '.q'

                waiting_cores = int(qstat_r_dict["job_info"]["job_info"]["job_list"]["slots"])
                waiting_job_elaps = int(qstat_r_dict["job_info"]["job_info"]["job_list"]["hard_request"][-2]["#text"])
                waiting_job_dict[waiting_queue_name].append([waiting_job_number, waiting_job_prio, waiting_job_name, waiting_job_user, waiting_job_jcl, waiting_cores])

            for q in queue_tuple:
                waiting_job_dict[q].sort(key=lambda x: (-x[1], x[0]))
            waiting_job_dict["dma"].sort(key=lambda x: (-x[1], x[0]))

            for q in queue_tuple:
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

            # -- q = dma --
            q = "dma"
            number_of_jobs_running_on_queue = len(running_job_dict[q])
            number_of_jobs_waiting_on_queue = len(waiting_job_dict[q])
            i = 0
            j = 0
            while i < number_of_jobs_running_on_queue:
                exec_queue_name = running_job_dict[q][i][-1]
                avail_cores_dict[exec_queue_name] += running_job_dict[q][i][1]
                # print avail_cores_dict[q]
                while j < number_of_jobs_waiting_on_queue:
                    req_cores = waiting_job_dict[q][j][-1]
                    if avail_cores_dict[exec_queue_name] >= req_cores:
                        # print q, waiting_job_dict[q][j], running_job_dict[q][i][2]
                        print('{} {} {} {} {}'.format(*waiting_job_dict[q][j]) + running_job_dict[q][i][2].strftime(' %Y/%m/%d %H:%M:%S'))
                        avail_cores_dict[exec_queue_name] -= req_cores
                        j += 1
                    break
                i += 1

if __name__ == '__main__':
    parse()
