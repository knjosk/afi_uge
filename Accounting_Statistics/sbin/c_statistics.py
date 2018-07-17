#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
try:  # rhel5 doesn't have json
    import json
except ImportError:
    import simplejson as json
from collections import defaultdict
import csv

field_tuple = ('qname', 'hostname', 'group', 'owner', 'job_name', 'job_number', 'account', 'priority',
               'submission_time', 'start_time', 'end_time', 'failed', 'exit_status', 'ru_wallclock',
               'ru_utime', 'ru_stime', 'ru_maxrss', 'ru_ixrss', 'ru_ismrss', 'ru_idrss', 'ru_isrss',
               'ru_minflt', 'ru_majflt', 'ru_nswap', 'ru_inblock', 'ru_oublock', 'ru_msgsnd',
               'ru_msgrcv', 'ru_nsignals', 'ru_nvcsw', 'ru_nivcsw', 'project', 'department',
               'granted_pe', 'slots', 'task_number', 'cpu', 'mem', 'io', 'category', 'iow',
               'pe_taskid', 'maxvmem', 'arid', 'ar_submission_time', 'job_class', 'qdel_info',
               'maxrss', 'maxpss', 'submit_host', 'cwd', 'submit_cmd', 'wallclock', 'ioops')

i_qname = i_group = i_owner = i_job_name = i_job_number = i_priority = i_start_time = i_end_time = 0
i_ru_wallclock = i_ru_utime = i_ru_stime = i_project = i_slots = i_cpu = i_mem = i_maxvmem = 0
i_job_class = 0

i_qname = field_tuple.index("qname")
i_group = field_tuple.index("group")
i_owner = field_tuple.index("owner")
i_job_name = field_tuple.index("job_name")
i_job_number = field_tuple.index("job_number")
i_priority = field_tuple.index("priority")
i_start_time = field_tuple.index("start_time")
i_end_time = field_tuple.index("end_time")
i_ru_wallclock = field_tuple.index("ru_wallclock")
i_ru_utime = field_tuple.index("ru_utime")
i_ru_stime = field_tuple.index("ru_stime")
i_project = field_tuple.index("project")
i_slots = field_tuple.index("slots")
i_cpu = field_tuple.index("cpu")
i_mem = field_tuple.index("mem")
i_maxvmem = field_tuple.index("maxvmem")
i_job_class = field_tuple.index("job_class")

CORES = 40

queue_tuple = ('sma.q', 'smb.q', 'aps.q', 'single.q', 'intsmp.q', 'intmpi.q',
               'dmaL.q', 'dmaM.q', 'dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
               'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
               'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')

queue_node_sm_tuple = ('sma.q', 'smb.q', 'aps.q')

queue_node_dm_tuple = ('dmaL.q', 'dmaM.q', 'dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
                       'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
                       'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')

queue_cpu_tuple = ('single.q', 'intsmp.q', 'intmpi.q')
queue_tss_tuple = ('intsmp.q', 'intmpi.q')

nodes_dict = {'sma.default': 2, 'sma.A': 2, 'sma.B': 4, 'sma.C': 8, 'sma.D': 12, 'sma.E': 26,
              'smb.default': 2, 'smb.A': 2, 'smb.B': 4, 'smb.C': 8, 'smb.D': 12, 'smb.E': 26,
              'aps.default': 2, 'aps.A': 2, 'aps.B': 4, 'aps.C': 8, 'aps.D': 12, 'aps.E': 24}

jc_sma_tuple = ('sma.default', 'sma.A', 'sma.B', 'sma.C', 'sma.D', 'sma.E')
jc_smb_tuple = ('smb.default', 'smb.A', 'smb.B', 'smb.C', 'smb.D', 'smb.E')
jc_dma_tuple = ('dma.default', 'dma.A', 'dma.M' 'dma.L')
jc_aps_tuple = ('aps.default', 'aps.A', 'aps.B', 'aps.C', 'aps.D', 'aps.E')

q_no_group_tuple = ('sma.q', 'smb.q', 'aps.q', 'single.q', 'intsmp.q', 'intmpi.q', 'dmaL.q', 'dmaM.q')
q_dma_group_tuple = ('dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
                     'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
                     'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')
jc_dma_group_tuple = ('dma.default', 'dma.A')

queue_cout_dict = defaultdict(int)
group_count_dict = defaultdict(int)
group_queue_count_dict = defaultdict(int)
jc_count_dict = defaultdict(int)
owner_count_dict = defaultdict(int)
prj_count_dict = defaultdict(int)
prj_queue_count_dict = defaultdict(int)

queue_utime_dict = defaultdict(float)
group_utime_dict = defaultdict(float)
group_queue_utime_dict = defaultdict(float)
jc_utime_dict = defaultdict(float)
owner_utime_dict = defaultdict(float)
prj_utime_dict = defaultdict(float)
prj_queue_utime_dict = defaultdict(float)

queue_stime_dict = defaultdict(float)
group_stime_dict = defaultdict(float)
group_queue_stime_dict = defaultdict(float)
jc_stime_dict = defaultdict(float)
owner_stime_dict = defaultdict(float)
prj_stime_dict = defaultdict(float)
prj_queue_stime_dict = defaultdict(float)

queue_cputime_dict = defaultdict(float)
group_cputime_dict = defaultdict(float)
group_queue_cputime_dict = defaultdict(float)
jc_cputime_dict = defaultdict(float)
owner_cputime_dict = defaultdict(float)
prj_cputime_dict = defaultdict(float)
prj_queue_cputime_dict = defaultdict(float)

queue_wallclock_dict = defaultdict(float)
group_wallclock_dict = defaultdict(float)
group_queue_wallclock_dict = defaultdict(float)
jc_wallclock_dict = defaultdict(float)
owner_wallclock_dict = defaultdict(float)
prj_wallclock_dict = defaultdict(float)
prj_queue_wallclock_dict = defaultdict(float)

queue_ocutime_dict = defaultdict(float)
group_ocutime_dict = defaultdict(float)
group_queue_ocutime_dict = defaultdict(float)
jc_ocutime_dict = defaultdict(float)
owner_ocutime_dict = defaultdict(float)
prj_ocutime_dict = defaultdict(float)
prj_queue_ocutime_dict = defaultdict(float)

queue_slots_dict = defaultdict(int)
group_slots_dict = defaultdict(int)
group_queue_slots_dict = defaultdict(int)
jc_slots_dict = defaultdict(int)
owner_slots_dict = defaultdict(int)
prj_slots_dict = defaultdict(int)
prj_queue_slots_dict = defaultdict(int)

jc_maxvmem_dict = defaultdict(float)
jc_avemem_dict = defaultdict(float)

prj_limit_dict = defaultdict(float)
prj_exceeded_list = []
prj_used_list = []

group_limit_dict = defaultdict(float)
group_exceeded_list = []
group_used_list = []

act_group_cputime_dict = defaultdict(float)
act_owner_cputime_dict = defaultdict(float)
act_prj_cputime_dict = defaultdict(float)
act_group_tss_cputime_dict = defaultdict(float)
act_owner_tss_cputime_dict = defaultdict(float)
act_prj_tss_cputime_dict = defaultdict(float)


def calc_accounting(filename, start_utime, end_utime):
    try:
        f = open(filename, 'r')
    except:
        sys.stderr.write("failed to open" + filename + "\n")
        sys.exit(-1)

    limit_f = open('prj_limit_pm.csv', 'r')

    reader = csv.reader(limit_f)
    header = next(reader)
    for row in reader:
        prj_limit_dict[row[0]] = float(row[1])
        # print prj_limit_dict
        # print "row"
        # print row

    # print start_utime
    # print end_utime

    group_limit_f = open('group_limit_pm.csv', 'r')

    reader = csv.reader(group_limit_f)
    header = next(reader)
    for row in reader:
        group_limit_dict[row[0]] = float(row[1])
        # print prj_limit_dict
        # print "row"
        # print row

    # print start_utime
    # print end_utime

    for line in f:
        if line.startswith("#"):
            continue
        else:
            account_data_list = line.split(":")
            nodes = 0
            if start_utime * 1000 <= int(account_data_list[i_start_time]) < end_utime * 1000:
                queue_name = account_data_list[i_qname]
                queue_cout_dict[account_data_list[i_qname]] += 1
                queue_utime_dict[account_data_list[i_qname]] += float(account_data_list[i_ru_utime])
                queue_stime_dict[account_data_list[i_qname]] += float(account_data_list[i_ru_stime])
                queue_cputime_dict[account_data_list[i_qname]] += float(account_data_list[i_cpu])
                queue_wallclock_dict[account_data_list[i_qname]] += float(account_data_list[i_ru_wallclock])
                queue_slots_dict[account_data_list[i_qname]] += int(account_data_list[i_slots])
                if queue_name not in queue_cpu_tuple:
                    if queue_name in queue_node_dm_tuple:
                        nodes = -(-int(account_data_list[i_slots]) // CORES)
                    elif queue_name in queue_node_sm_tuple:
                        nodes = nodes_dict[account_data_list[i_job_class]]
                        queue_ocutime_dict[account_data_list[i_qname]] += float(account_data_list[i_ru_wallclock]) * nodes

                if account_data_list[i_project] == "general":
                    group_count_dict[account_data_list[i_group]] += 1
                    group_utime_dict[account_data_list[i_group]] += float(account_data_list[i_ru_utime])
                    group_stime_dict[account_data_list[i_group]] += float(account_data_list[i_ru_stime])
                    group_cputime_dict[account_data_list[i_group]] += float(account_data_list[i_cpu])
                    group_wallclock_dict[account_data_list[i_group]] += float(account_data_list[i_ru_wallclock])
                    group_slots_dict[account_data_list[i_group]] += int(account_data_list[i_slots])

                    group_queue_count_dict[account_data_list[i_group] + '_' + account_data_list[i_qname]] += 1
                    group_queue_utime_dict[account_data_list[i_group] + '_' + account_data_list[i_qname]] += float(account_data_list[i_ru_utime])
                    group_queue_stime_dict[account_data_list[i_group] + '_' + account_data_list[i_qname]] += float(account_data_list[i_ru_stime])
                    group_queue_cputime_dict[account_data_list[i_group] + '_' + account_data_list[i_qname]] += float(account_data_list[i_cpu])
                    group_queue_wallclock_dict[account_data_list[i_group] + '_' + account_data_list[i_qname]] += float(account_data_list[i_ru_wallclock])
                    group_queue_slots_dict[account_data_list[i_group] + '_' + account_data_list[i_qname]] += int(account_data_list[i_slots])

                    if queue_name not in queue_cpu_tuple:
                        group_ocutime_dict[account_data_list[i_group]] += float(account_data_list[i_ru_wallclock]) * nodes
                        group_queue_ocutime_dict[account_data_list[i_group] + '_' + account_data_list[i_qname]] += float(account_data_list[i_ru_wallclock]) * nodes
                    elif queue_name not in queue_tss_tuple:
                        act_group_cputime_dict[account_data_list[i_group]] += float(account_data_list[i_cpu])
                    else:
                        act_group_tss_cputime_dict[account_data_list[i_group]] += float(account_data_list[i_cpu])

                owner_count_dict[account_data_list[i_owner]] += 1
                owner_utime_dict[account_data_list[i_owner]] += float(account_data_list[i_ru_utime])
                owner_stime_dict[account_data_list[i_owner]] += float(account_data_list[i_ru_stime])
                owner_cputime_dict[account_data_list[i_owner]] += float(account_data_list[i_cpu])
                owner_wallclock_dict[account_data_list[i_owner]] += float(account_data_list[i_ru_wallclock])
                owner_slots_dict[account_data_list[i_owner]] += int(account_data_list[i_slots])

                jc_count_dict[account_data_list[i_job_class]] += 1
                jc_utime_dict[account_data_list[i_job_class]] += float(account_data_list[i_ru_utime])
                jc_stime_dict[account_data_list[i_job_class]] += float(account_data_list[i_ru_stime])
                jc_cputime_dict[account_data_list[i_job_class]] += float(account_data_list[i_cpu])
                jc_wallclock_dict[account_data_list[i_job_class]] += float(account_data_list[i_ru_wallclock])
                jc_slots_dict[account_data_list[i_job_class]] += int(account_data_list[i_slots])
                jc_maxvmem_dict[account_data_list[i_job_class]] = max(jc_maxvmem_dict[account_data_list[i_job_class]], float(account_data_list[i_maxvmem]))
                if float(account_data_list[i_cpu]) == 0.0:
                    jc_avemem_dict[account_data_list[i_job_class]] += 0
                else:
                    jc_avemem_dict[account_data_list[i_job_class]] += float(account_data_list[i_mem]) / float(account_data_list[i_cpu])

                prj_count_dict[account_data_list[i_project]] += 1
                prj_utime_dict[account_data_list[i_project]] += float(account_data_list[i_ru_utime])
                prj_stime_dict[account_data_list[i_project]] += float(account_data_list[i_ru_stime])
                prj_cputime_dict[account_data_list[i_project]] += float(account_data_list[i_cpu])
                prj_wallclock_dict[account_data_list[i_project]] += float(account_data_list[i_ru_wallclock])
                prj_slots_dict[account_data_list[i_project]] += int(account_data_list[i_slots])

                prj_queue_count_dict[account_data_list[i_project] + '_' + account_data_list[i_qname]] += 1
                prj_queue_utime_dict[account_data_list[i_project] + '_' + account_data_list[i_qname]] += float(account_data_list[i_ru_utime])
                prj_queue_stime_dict[account_data_list[i_project] + '_' + account_data_list[i_qname]] += float(account_data_list[i_ru_stime])
                prj_queue_cputime_dict[account_data_list[i_project] + '_' + account_data_list[i_qname]] += float(account_data_list[i_cpu])
                prj_queue_wallclock_dict[account_data_list[i_project] + '_' + account_data_list[i_qname]] += float(account_data_list[i_ru_wallclock])
                prj_queue_slots_dict[account_data_list[i_project] + '_' + account_data_list[i_qname]] += int(account_data_list[i_slots])

                if queue_name not in queue_cpu_tuple:
                    owner_ocutime_dict[account_data_list[i_group]] += float(account_data_list[i_ru_wallclock]) * nodes
                    jc_ocutime_dict[account_data_list[i_job_class]] += float(account_data_list[i_ru_wallclock]) * nodes
                    prj_ocutime_dict[account_data_list[i_project]] += float(account_data_list[i_ru_wallclock]) * nodes
                    prj_queue_ocutime_dict[account_data_list[i_project] + '_' + account_data_list[i_qname]] += float(account_data_list[i_ru_wallclock]) * nodes
                elif queue_name not in queue_tss_tuple:
                    act_prj_cputime_dict[account_data_list[i_project]] += float(account_data_list[i_cpu])
                    act_owner_cputime_dict[account_data_list[i_owner]] += float(account_data_list[i_cpu])
                else:
                    act_prj_tss_cputime_dict[account_data_list[i_project]] += float(account_data_list[i_cpu])
                    act_owner_tss_cputime_dict[account_data_list[i_owner]] += float(account_data_list[i_cpu])

    # make Table 2-3-1 CPU usage per group
    print "--- Table 2.3 Usage per group ---"
    group_list = group_count_dict.keys()
    # print "group_list"
    # print group_list
    group_total_cputime = 0
    for grp in group_list:
        group_total_cputime += group_cputime_dict.get(grp)
    # print queue_tuple
    for grp in group_list:
        print grp,
        for q in queue_tuple:
            key = grp + '_' + q
            print group_queue_utime_dict.get(key, 0), group_queue_stime_dict.get(key, 0), group_queue_cputime_dict.get(key, 0),

        print group_cputime_dict.get(grp, 0),
        print (group_cputime_dict.get(grp, 0) / group_total_cputime) * 100

    print "--- Table 2.4 Usage per project ---"
    prj_list = prj_count_dict.keys()
    prj_total_cputime_dict = 0
    for prj in prj_list:
        prj_total_cputime_dict += prj_cputime_dict.get(prj)
    # print prj_list
    for prj in prj_list:
        print prj,
        for q in queue_tuple:
            key = prj + '_' + q
            print prj_queue_utime_dict.get(key, 0), prj_queue_stime_dict.get(key, 0), prj_queue_cputime_dict.get(key, 0),
        print prj_cputime_dict.get(prj, 0),
        print (prj_cputime_dict.get(prj, 0) / prj_total_cputime_dict) * 100

    print "--- Table 2-8 Usage per job class ---"
    jc_list = jc_count_dict.keys()
    jc_total_cputime_dict = 0
    for jc in jc_list:
        jc_total_cputime_dict += jc_cputime_dict.get(jc)

    # print jc_list, jc_total_cputime_dict
    for key in jc_list:
        print key,
        print jc_count_dict.get(key, 0), jc_utime_dict.get(key, 0), jc_stime_dict.get(key, 0), jc_cputime_dict.get(key, 0),
        print jc_cputime_dict.get(key, 0),
        print jc_ocutime_dict.get(key, 0),
        print jc_slots_dict.get(key, 0) / jc_count_dict.get(key, 0),
        print jc_avemem_dict.get(key, 0) / jc_count_dict.get(key, 0),
        print jc_maxvmem_dict.get(key, 0) / 1000000000

    print "--- Exceeded limit project ---"
    # print "prj_limit_dict"
    # print prj_limit_dict
    for key in prj_limit_dict:
        # print key
        prj_used_list.append([key,
                              prj_limit_dict.get(key, 0),
                              (prj_ocutime_dict.get(key, 0) + act_prj_cputime_dict.get(key, 0) + act_prj_tss_cputime_dict.get(key, 0)) / 60 / 60,
                              (prj_ocutime_dict.get(key, 0) + act_prj_cputime_dict.get(key, 0)) / 60 / 60,
                              (act_prj_tss_cputime_dict.get(key, 0)) / 60 / 60])
        if (prj_limit_dict.get(key, 0) * 60 * 60) <= (prj_ocutime_dict.get(key, 0) + act_prj_cputime_dict.get(key, 0) + act_prj_tss_cputime_dict.get(key, 0)):
            prj_exceeded_list.append(key)
            # print key, prj_limit_dict.get(key,0),  prj_wallclock_dict.get(key,0)
            print prj_exceeded_list

    # print  "prj_used_list"
    # print  prj_used_list
    used_f = open('prj_used_pm.csv', 'w')

    writer = csv.writer(used_f, lineterminator='\n')
    writer.writerows(prj_used_list)

    exceeded_f = open('prj_exceeded_pm.txt', 'w')
    for x in prj_exceeded_list:
        exceeded_f.write(str(x) + "\n")

    print "--- Exceeded limit group ---"
    for key in group_limit_dict:
        # print key
        group_used_list.append([key,
                                group_limit_dict.get(key, 0),
                                (group_ocutime_dict.get(key, 0) + act_group_cputime_dict.get(key, 0) + act_group_tss_cputime_dict.get(key, 0)) / 60 / 60,
                                (group_ocutime_dict.get(key, 0) + act_group_cputime_dict.get(key, 0)) / 60 / 60,
                                (act_group_tss_cputime_dict.get(key, 0)) / 60 / 60])
        if (group_limit_dict.get(key, 0) * 60 * 60) <= (group_ocutime_dict.get(key, 0) + act_group_cputime_dict.get(key, 0) + act_group_tss_cputime_dict.get(key, 0)):
            group_exceeded_list.append(key)
            # print key, prj_limit_dict.get(key,0),  prj_wallclock_dict.get(key,0)
            print group_exceeded_list

    group_used_f = open('group_used_pm.csv', 'w')

    writer = csv.writer(group_used_f, lineterminator='\n')
    writer.writerows(group_used_list)

    group_exceeded_f = open('group_exceeded_pm.txt', 'w')
    for x in group_exceeded_list:
        group_exceeded_f.write(str(x) + "\n")

    exceeded_f.close()
    f.close()
    limit_f.close()
    used_f.close()
    group_limit_f.close()
    group_used_f.close()

__doc__ = """{f}

Usage:
    {f} <fname> [-s | --start <start_time>] [-e | --end <end_time>]
    {f} -h | --help

Options:
    -s --start <START_TIME>  YYYYMMDDhhmmss
    -e --end <END_TIME>      YYYYMMDDhhmmss
    -h --help                Show this screen and exit.
""".format(f=__file__)

from docopt import docopt
import datetime
import time


def parse():
    start_utime = 0
    now = datetime.datetime.now()
    end_utime = int(time.mktime(now.timetuple()))
    args = docopt(__doc__)
    if args['--start']:
        start_time = datetime.datetime.strptime(args['--start'][0], '%Y%m%d%H%M%S')
        start_utime = int(time.mktime(start_time.timetuple()))
    if args['--end']:
        end_time = datetime.datetime.strptime(args['--end'][0], '%Y%m%d%H%M%S')
        end_utime = int(time.mktime(end_time.timetuple()))
    filename = args['<fname>']

    print "start, end"
    print start_utime, end_utime

    calc_accounting(filename, start_utime, end_utime)

if __name__ == '__main__':
    parse()
