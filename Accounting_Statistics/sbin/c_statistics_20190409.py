#!/usr/bin/python
# -*- coding: utf-8 -*-

import sys
try:  # rhel5 doesn't have json
    import json
except ImportError:
    import simplejson as json
from collections import defaultdict
import csv
import re

field_tuple = ('qname', 'hostname', 'group', 'owner', 'job_name', 'job_number', 'account', 'priority', 'submission_time', 'start_time',
               'end_time', 'failed', 'exit_status', 'ru_wallclock', 'ru_utime', 'ru_stime', 'ru_maxrss', 'ru_ixrss', 'ru_ismrss', 'ru_idrss',
               'ru_isrss', 'ru_minflt', 'ru_majflt', 'ru_nswap', 'ru_inblock', 'ru_oublock', 'ru_msgsnd', 'ru_msgrcv', 'ru_nsignals', 'ru_nvcsw',
               'ru_nivcsw', 'project', 'department', 'granted_pe', 'slots', 'task_number', 'cpu', 'mem', 'io', 'category',
               'iow', 'pe_taskid', 'maxvmem', 'arid', 'ar_submission_time', 'job_class', 'qdel_info', 'maxrss', 'maxpss', 'submit_host',
               'cwd', 'submit_cmd', 'wallclock', 'ioops')

idx_qname = field_tuple.index("qname")
idx_group = field_tuple.index("group")
idx_owner = field_tuple.index("owner")
idx_job_name = field_tuple.index("job_name")
idx_job_number = field_tuple.index("job_number")
idx_priority = field_tuple.index("priority")
idx_start_time = field_tuple.index("start_time")
idx_end_time = field_tuple.index("end_time")
idx_ru_wallclock = field_tuple.index("ru_wallclock")
idx_ru_utime = field_tuple.index("ru_utime")
idx_ru_stime = field_tuple.index("ru_stime")
idx_project = field_tuple.index("project")
idx_slots = field_tuple.index("slots")
idx_cpu = field_tuple.index("cpu")
idx_mem = field_tuple.index("mem")
idx_maxvmem = field_tuple.index("maxvmem")
idx_job_class = field_tuple.index("job_class")
idx_granted_pe = field_tuple.index("granted_pe")

CORES = 40
INT_CORES = 36

queue_tuple = ('sma.q', 'smb.q', 'aps.q', 'single.q', 'intsmp.q', 'intmpi.q',
               'dmaL.q', 'dmaM.q', 'dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
               'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
               'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')

queue_node_sm_tuple = ('sma.q', 'smb.q', 'aps.q')

queue_node_dm_tuple = ('dmaL.q', 'dmaM.q', 'dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
                       'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
                       'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')

queue_dma_tuple = ('dmaL.q', 'dmaM.q', 'dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
                   'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
                   'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37', 'single.q', 'share.q')
queue_node_share_tuple = ('single.q', 'share.q')
queue_cpu_tuple = ('intsmp.q', 'intmpi.q')
queue_tss_tuple = ('intsmp.q', 'intmpi.q')

queue_cpu_d_tuple = ()
queue_cpu_s_tuple = ()

queue_tss_d_tuple = ('intmpi.q')
queue_tss_s_tuple = ('intsmp.q')

nodes_dict = {'sma.default': 2, 'sma.A': 2, 'sma.B': 4, 'sma.C': 8, 'sma.D': 12, 'sma.E': 26, 'sma.AS': 1,
              'smb.default': 2, 'smb.A': 2, 'smb.B': 4, 'smb.C': 8, 'smb.D': 12, 'smb.E': 26, 'smb.AS': 1,
              'aps.default': 2, 'aps.A': 2, 'aps.B': 4, 'aps.C': 8, 'aps.D': 12, 'aps.E': 24, 'aps.AS': 1}

q_no_group_tuple = ('sma.q', 'smb.q', 'aps.q', 'single.q', 'intsmp.q', 'intmpi.q', 'dmaL.q', 'dmaM.q', 'share.q')
q_dma_group_tuple = ('dma_01', 'dma_02', 'dma_03', 'dma_04', 'dma_05', 'dma_06', 'dma_07',
                     'dma_08', 'dma_09', 'dma_10', 'dma_11', 'dma_12', 'dma_13', 'dma_14', 'dma_15', 'dma_16',
                     'dma_17', 'dma_18', 'dma_19', 'dma_20', 'dma_21', 'dma_37')

jc_dma_group_tuple = ('dma.default', 'dma.A')

queue_cout_dict = defaultdict(int)
group_count_dict = defaultdict(int)
group_queue_count_dict = defaultdict(int)
jc_count_dict = defaultdict(int)
owner_count_dict = defaultdict(int)
user_count_dict = defaultdict(int)
prj_count_dict = defaultdict(int)
prj_queue_count_dict = defaultdict(int)

queue_utime_dict = defaultdict(float)
group_utime_dict = defaultdict(float)
group_queue_utime_dict = defaultdict(float)
jc_utime_dict = defaultdict(float)
owner_utime_dict = defaultdict(float)
user_utime_dict = defaultdict(float)
prj_utime_dict = defaultdict(float)
prj_queue_utime_dict = defaultdict(float)

queue_stime_dict = defaultdict(float)
group_stime_dict = defaultdict(float)
group_queue_stime_dict = defaultdict(float)
jc_stime_dict = defaultdict(float)
owner_stime_dict = defaultdict(float)
user_stime_dict = defaultdict(float)
prj_stime_dict = defaultdict(float)
prj_queue_stime_dict = defaultdict(float)

queue_cputime_dict = defaultdict(float)
group_cputime_dict = defaultdict(float)
group_queue_cputime_dict = defaultdict(float)
jc_cputime_dict = defaultdict(float)
owner_cputime_dict = defaultdict(float)
user_cputime_dict = defaultdict(float)
prj_cputime_dict = defaultdict(float)
prj_queue_cputime_dict = defaultdict(float)

queue_wallclock_dict = defaultdict(float)
group_wallclock_dict = defaultdict(float)
group_queue_wallclock_dict = defaultdict(float)
jc_wallclock_dict = defaultdict(float)
owner_wallclock_dict = defaultdict(float)
user_wallclock_dict = defaultdict(float)
prj_wallclock_dict = defaultdict(float)
prj_queue_wallclock_dict = defaultdict(float)

queue_ocutime_dict = defaultdict(float)
group_ocutime_dict = defaultdict(float)
group_queue_ocutime_dict = defaultdict(float)
jc_ocutime_dict = defaultdict(float)
owner_ocutime_dict = defaultdict(float)
owner_prj_ocutime_dict = defaultdict(float)
user_ocutime_dict = defaultdict(float)
user_prj_ocutime_dict = defaultdict(float)
prj_ocutime_dict = defaultdict(float)
prj_queue_ocutime_dict = defaultdict(float)

queue_slots_dict = defaultdict(int)
group_slots_dict = defaultdict(int)
group_queue_slots_dict = defaultdict(int)
jc_slots_dict = defaultdict(int)
owner_slots_dict = defaultdict(int)
user_slots_dict = defaultdict(int)
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

user_limit_dict = defaultdict(float)
user_exceeded_list = []
user_used_list = []

act_group_cputime_dict = defaultdict(float)
act_owner_cputime_dict = defaultdict(float)
act_owner_prj_cputime_dict = defaultdict(float)
act_user_cputime_dict = defaultdict(float)
act_user_prj_cputime_dict = defaultdict(float)
act_prj_cputime_dict = defaultdict(float)
act_group_tss_cputime_dict = defaultdict(float)
act_owner_tss_cputime_dict = defaultdict(float)
act_owner_prj_tss_cputime_dict = defaultdict(float)
act_user_tss_cputime_dict = defaultdict(float)
act_user_prj_tss_cputime_dict = defaultdict(float)
act_prj_tss_cputime_dict = defaultdict(float)


def calc_accounting(filename, start_utime, end_utime, POST_FIX):
    try:
        f = open(filename, 'r')
    except:
        sys.stderr.write("failed to open: " + filename + "\n")
        sys.exit(-1)

    calc_pat = r'([+-]?[0-9]+\.?[0-9]*)'
    one_node_pat = r'^NONE|^OpenMP|^smp|.*pslots.*'
    per40_pat = r'.*fillup|^hybrid.*'

    try:
        limit_f = open('/opt/uge/Accounting_Statistics/etc/prj_limit_pm' + POST_FIX, 'r')
    except:
        sys.stderr.write("failed to open: prj_limit_pm." + POST_FIX + "\n")
        sys.exit(-1)

    reader = csv.reader(limit_f)
    header = next(reader)
    for row in reader:
        prj_limit_dict[row[0]] = float(row[1])

    try:
        group_limit_f = open('/opt/uge/Accounting_Statistics/etc/group_limit_py.csv', 'r')
    except:
        sys.stderr.write("failed to open: group_limit_py.csv" + "\n")
        sys.exit(-1)

    reader = csv.reader(group_limit_f)
    header = next(reader)
    for row in reader:
        group_limit_dict[row[0]] = float(row[1])

    try:
        user_limit_f = open('/opt/uge/Accounting_Statistics/etc/user_limit_py.csv', 'r')
    except:
        sys.stderr.write("failed to open: user_limit_py.csv" + "\n")
        sys.exit(-1)

    reader = csv.reader(user_limit_f)
    header = next(reader)
    for row in reader:
        user_limit_dict[row[0]] = float(row[1])

    for line in f:
        if line.startswith("#"):
            continue
        else:
            account_data_list = line.split(":")
            nodes = 0
            pe_l = []
            if start_utime * 1000 <= int(account_data_list[idx_start_time]) < end_utime * 1000:
                queue_name = account_data_list[idx_qname]
                queue_cout_dict[account_data_list[idx_qname]] += 1
                queue_utime_dict[account_data_list[idx_qname]] += float(account_data_list[idx_ru_utime])
                queue_stime_dict[account_data_list[idx_qname]] += float(account_data_list[idx_ru_stime])
                queue_cputime_dict[account_data_list[idx_qname]] += float(account_data_list[idx_cpu])
                queue_wallclock_dict[account_data_list[idx_qname]] += float(account_data_list[idx_ru_wallclock])
                queue_slots_dict[account_data_list[idx_qname]] += int(account_data_list[idx_slots])
                if queue_name not in queue_cpu_tuple:
                    if queue_name in queue_node_dm_tuple:
                        pe_name = account_data_list[idx_granted_pe]
                        if re.search(one_node_pat, pe_name) == None:
                            if re.search(per40_pat, pe_name) == None:
                                pe_l = re.findall(calc_pat, pe_name)
                                alloc_rule = int(pe_l[0])
                                nodes = -(-int(account_data_list[idx_slots]) // alloc_rule)
                            else:
                                nodes = -(-int(account_data_list[idx_slots]) // CORES)
                        else:
                            nodes = 1
                    elif queue_name in queue_node_sm_tuple:
                        nodes = nodes_dict[account_data_list[idx_job_class]]
                        queue_ocutime_dict[account_data_list[idx_qname]] += float(account_data_list[idx_ru_wallclock]) * nodes

                if account_data_list[idx_project] == "general":
                    user_count_dict[account_data_list[idx_owner]] += 1
                    user_utime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_utime])
                    user_stime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_stime])
                    user_cputime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_cpu])
                    user_wallclock_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_wallclock])
                    user_slots_dict[account_data_list[idx_owner]] += int(account_data_list[idx_slots])

                    group_count_dict[account_data_list[idx_group]] += 1
                    group_utime_dict[account_data_list[idx_group]] += float(account_data_list[idx_ru_utime])
                    group_stime_dict[account_data_list[idx_group]] += float(account_data_list[idx_ru_stime])
                    group_cputime_dict[account_data_list[idx_group]] += float(account_data_list[idx_cpu])
                    group_wallclock_dict[account_data_list[idx_group]] += float(account_data_list[idx_ru_wallclock])
                    group_slots_dict[account_data_list[idx_group]] += int(account_data_list[idx_slots])

                    group_queue_count_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] += 1
                    group_queue_utime_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_utime])
                    group_queue_stime_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_stime])
                    group_queue_cputime_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_cpu])
                    group_queue_wallclock_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_wallclock])
                    group_queue_slots_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] += int(account_data_list[idx_slots])

                    if queue_name in queue_node_sm_tuple:
                        user_ocutime_dict[account_data_list[idx_owner] + "-s"] += float(account_data_list[idx_ru_wallclock]) * nodes
                        group_ocutime_dict[account_data_list[idx_group]] += float(account_data_list[idx_ru_wallclock]) * nodes
                        group_queue_ocutime_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_wallclock]) * nodes
                    elif queue_name in queue_node_dm_tuple:
                        user_ocutime_dict[account_data_list[idx_owner] + "-d"] += float(account_data_list[idx_ru_wallclock]) * nodes
                        group_ocutime_dict[account_data_list[idx_group]] += float(account_data_list[idx_ru_wallclock]) * nodes
                        group_queue_ocutime_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_wallclock]) * nodes
                    elif queue_name in queue_node_share_tuple:  # single.q, share.q
                        user_ocutime_dict[account_data_list[idx_owner] + "-d"] += float(account_data_list[idx_ru_wallclock]) * int(account_data_list[idx_slots]) / CORES
                        group_ocutime_dict[account_data_list[idx_group]] += float(account_data_list[idx_ru_wallclock]) * int(account_data_list[idx_slots]) / CORES
                        group_queue_ocutime_dict[account_data_list[idx_group] + '_' + account_data_list[idx_qname]] \
                            += float(account_data_list[idx_ru_wallclock]) * int(account_data_list[idx_slots]) / CORES
                    elif queue_name in queue_cpu_d_tuple:  # no queue
                        act_user_cputime_dict[account_data_list[idx_owner] + "-d"] += float(account_data_list[idx_ru_wallclock]) / CORES
                        act_group_cputime_dict[account_data_list[idx_group] + "-d"] += float(account_data_list[idx_ru_wallclock]) / CORES
                    elif queue_name in queue_cpu_s_tuple:  # no queue
                        act_user_cputime_dict[account_data_list[idx_owner] + "-s"] += float(account_data_list[idx_cpu]) / CORES
                        act_group_cputime_dict[account_data_list[idx_group] + "-s"] += float(account_data_list[idx_cpu]) / CORES
                    elif queue_name in queue_tss_d_tuple:
                        if queue_name == "intmpi.q":  # intmpi.q
                            act_user_tss_cputime_dict[account_data_list[idx_owner] + "-d"] += float(account_data_list[idx_cpu]) / INT_CORES
                            act_group_tss_cputime_dict[account_data_list[idx_group] + "-d"] += float(account_data_list[idx_cpu]) / INT_CORES
                        else:  # no queue
                            act_user_tss_cputime_dict[account_data_list[idx_owner] + "-d"] += float(account_data_list[idx_cpu]) / CORES
                            act_group_tss_cputime_dict[account_data_list[idx_group] + "-d"] += float(account_data_list[idx_cpu]) / CORES
                    elif queue_name in queue_tss_s_tuple:  # intsmp.q
                        act_user_tss_cputime_dict[account_data_list[idx_owner] + "-s"] += float(account_data_list[idx_cpu]) / INT_CORES
                        act_group_tss_cputime_dict[account_data_list[idx_group] + "-s"] += float(account_data_list[idx_cpu]) / INT_CORES

                owner_count_dict[account_data_list[idx_owner]] += 1
                owner_utime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_utime])
                owner_stime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_stime])
                owner_cputime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_cpu])
                owner_wallclock_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_wallclock])
                owner_slots_dict[account_data_list[idx_owner]] += int(account_data_list[idx_slots])

                jc_count_dict[account_data_list[idx_job_class]] += 1
                jc_utime_dict[account_data_list[idx_job_class]] += float(account_data_list[idx_ru_utime])
                jc_stime_dict[account_data_list[idx_job_class]] += float(account_data_list[idx_ru_stime])
                jc_cputime_dict[account_data_list[idx_job_class]] += float(account_data_list[idx_cpu])
                jc_wallclock_dict[account_data_list[idx_job_class]] += float(account_data_list[idx_ru_wallclock])
                jc_slots_dict[account_data_list[idx_job_class]] += int(account_data_list[idx_slots])
                jc_maxvmem_dict[account_data_list[idx_job_class]] = max(jc_maxvmem_dict[account_data_list[idx_job_class]], float(account_data_list[idx_maxvmem]))
                if float(account_data_list[idx_cpu]) == 0.0:
                    jc_avemem_dict[account_data_list[idx_job_class]] += 0
                else:
                    jc_avemem_dict[account_data_list[idx_job_class]] += float(account_data_list[idx_mem]) / float(account_data_list[idx_cpu])

                prj_count_dict[account_data_list[idx_project]] += 1
                prj_utime_dict[account_data_list[idx_project]] += float(account_data_list[idx_ru_utime])
                prj_stime_dict[account_data_list[idx_project]] += float(account_data_list[idx_ru_stime])
                prj_cputime_dict[account_data_list[idx_project]] += float(account_data_list[idx_cpu])
                prj_wallclock_dict[account_data_list[idx_project]] += float(account_data_list[idx_ru_wallclock])
                prj_slots_dict[account_data_list[idx_project]] += int(account_data_list[idx_slots])

                prj_queue_count_dict[account_data_list[idx_project] + '_' + account_data_list[idx_qname]] += 1
                prj_queue_utime_dict[account_data_list[idx_project] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_utime])
                prj_queue_stime_dict[account_data_list[idx_project] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_stime])
                prj_queue_cputime_dict[account_data_list[idx_project] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_cpu])
                prj_queue_wallclock_dict[account_data_list[idx_project] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_wallclock])
                prj_queue_slots_dict[account_data_list[idx_project] + '_' + account_data_list[idx_qname]] += int(account_data_list[idx_slots])

                if queue_name not in queue_cpu_tuple:
                    owner_ocutime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_wallclock]) * nodes
                    owner_prj_ocutime_dict[account_data_list[idx_owner] + "_" + account_data_list[idx_project]] += float(account_data_list[idx_ru_wallclock]) * nodes
                    jc_ocutime_dict[account_data_list[idx_job_class]] += float(account_data_list[idx_ru_wallclock]) * nodes
                    prj_ocutime_dict[account_data_list[idx_project]] += float(account_data_list[idx_ru_wallclock]) * nodes
                    prj_queue_ocutime_dict[account_data_list[idx_project] + '_' + account_data_list[idx_qname]] += float(account_data_list[idx_ru_wallclock]) * nodes
                elif queue_name in queue_node_share_tuple:  # single.q, share.q
                    owner_ocutime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_wallclock]) * int(account_data_list[idx_slots]) / CORES
                    owner_prj_ocutime_dict[account_data_list[idx_owner] + "_" + account_data_list[idx_project]] \
                        += float(account_data_list[idx_ru_wallclock]) * int(account_data_list[idx_slots]) / CORES
                    jc_ocutime_dict[account_data_list[idx_job_class]] += float(account_data_list[idx_ru_wallclock]) * int(account_data_list[idx_slots]) / CORES
                    prj_ocutime_dict[account_data_list[idx_project]] += float(account_data_list[idx_ru_wallclock]) * int(account_data_list[idx_slots]) / CORES
                    prj_queue_ocutime_dict[account_data_list[idx_project] + '_' + account_data_list[idx_qname]] \
                        += float(account_data_list[idx_ru_wallclock]) * int(account_data_list[idx_slots]) / CORES
                elif queue_name not in queue_tss_tuple:  # no queue
                    act_prj_cputime_dict[account_data_list[idx_project]] += float(account_data_list[idx_ru_wallclock]) / CORES
                    act_owner_cputime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_ru_wallclock]) / CORES
                    act_owner_prj_cputime_dict[account_data_list[idx_owner] + "_" + account_data_list[idx_project]] += float(account_data_list[idx_ru_wallclock]) / CORES
                else:  # intsmp.q and intmpi.q
                    act_prj_tss_cputime_dict[account_data_list[idx_project]] += float(account_data_list[idx_cpu]) / INT_CORES
                    act_owner_tss_cputime_dict[account_data_list[idx_owner]] += float(account_data_list[idx_cpu]) / INT_CORES
                    act_owner_prj_tss_cputime_dict[account_data_list[idx_owner] + "_" + account_data_list[idx_project]] += float(account_data_list[idx_cpu]) / INT_CORES

    # make Table 2-3-1 CPU usage per group

    group_out_list = []
    group_list = group_count_dict.keys()
    group_out_list_0 = []

    group_total_cputime = 0
    for grp in group_list:
        group_total_cputime += group_cputime_dict.get(grp)
    for grp in group_list:
        group_out_list0 = []
        dma_utime_total = 0
        dma_stime_total = 0
        dma_cputime_total = 0
        dma_ocutime_total = 0
        for q in queue_node_sm_tuple:
            key = grp + '_' + q
            group_out_list0.extend([group_queue_utime_dict.get(key, 0), group_queue_stime_dict.get(key, 0), group_queue_cputime_dict.get(key, 0)])

        for q in queue_dma_tuple:
            key = grp + '_' + q
            dma_utime_total += group_queue_utime_dict.get(key, 0)
            dma_stime_total += group_queue_stime_dict.get(key, 0)
            dma_cputime_total += group_queue_cputime_dict.get(key, 0)
            dma_ocutime_total += group_queue_ocutime_dict.get(key, 0)

        group_out_list0.extend([dma_utime_total, dma_stime_total, dma_cputime_total])

        for q in queue_tss_tuple:
            key = grp + '_' + q
            group_out_list0.extend([group_queue_utime_dict.get(key, 0), group_queue_stime_dict.get(key, 0), group_queue_cputime_dict.get(key, 0)])

        group_out_list0.extend([group_cputime_dict.get(grp, 0), (group_cputime_dict.get(grp, 0) / group_total_cputime) * 100])
        group_out_list0.insert(0, grp)

        group_out_list.append(group_out_list0)

    # print group_out_list

    grou_out_f = open('/opt/uge/Accounting_Statistics/logs/statistics/group_out' + POST_FIX, 'w')
    writer = csv.writer(grou_out_f, lineterminator='\n')
    writer.writerows(group_out_list)

    # print "--- Table 2.4 Usage per project ---"

    prj_list = prj_count_dict.keys()
    prj_out_list = []
    prj_total_cputime_dict = 0
    for prj in prj_list:
        if prj != "general":
            prj_total_cputime_dict += prj_cputime_dict.get(prj)
    for prj in prj_list:
        if prj != "general":

            prj_out_list0 = []

            dma_utime_total = 0
            dma_stime_total = 0
            dma_cputime_total = 0
            dma_ocutime_total = 0

            for q in queue_node_sm_tuple:
                key = prj + '_' + q
                prj_out_list0.extend([prj_queue_utime_dict.get(key, 0), prj_queue_stime_dict.get(key, 0), prj_queue_cputime_dict.get(key, 0)])

            for q in queue_dma_tuple:
                key = prj + '_' + q
                dma_utime_total += prj_queue_utime_dict.get(key, 0)
                dma_stime_total += prj_queue_stime_dict.get(key, 0)
                dma_cputime_total += prj_queue_cputime_dict.get(key, 0)
                dma_ocutime_total += prj_queue_ocutime_dict.get(key, 0)

            prj_out_list0.extend([dma_utime_total, dma_stime_total, dma_cputime_total])

            for q in queue_tss_tuple:
                key = prj + '_' + q
                prj_out_list0.extend([prj_queue_utime_dict.get(key, 0), prj_queue_stime_dict.get(key, 0), prj_queue_cputime_dict.get(key, 0)])

            prj_out_list0.extend([prj_cputime_dict.get(prj, 0), (prj_cputime_dict.get(prj, 0) / prj_total_cputime_dict) * 100])
            prj_out_list0.insert(0, prj)

            prj_out_list.append(prj_out_list0)

    prj_out_f = open('/opt/uge/Accounting_Statistics/logs/statistics/prj_out' + POST_FIX, 'w')
    writer = csv.writer(prj_out_f, lineterminator='\n')
    writer.writerows(prj_out_list)

    # print "--- Table 2-8 Usage per job class ---"

    jc_out_list = []

    jc_list = jc_count_dict.keys()
    jc_total_cputime_dict = 0
    for jc in jc_list:
        jc_total_cputime_dict += jc_cputime_dict.get(jc)

    for key in jc_list:
        jc_out_list.append([key,
                            jc_count_dict.get(key, 0), jc_utime_dict.get(key, 0), jc_stime_dict.get(key, 0), jc_cputime_dict.get(key, 0),
                            jc_ocutime_dict.get(key, 0),
                            jc_slots_dict.get(key, 0) / jc_count_dict.get(key, 0),
                            jc_avemem_dict.get(key, 0) / jc_count_dict.get(key, 0),
                            jc_maxvmem_dict.get(key, 0) / 1000000000])

    jc_out_f = open('/opt/uge/Accounting_Statistics/logs/statistics/jc_out' + POST_FIX, 'w')
    writer = csv.writer(jc_out_f, lineterminator='\n')
    writer.writerows(jc_out_list)

    # print "--- Exceeded limit project ---"
    for key in prj_limit_dict:
        prj_total_sec = prj_ocutime_dict.get(key, 0) + act_prj_cputime_dict.get(key, 0) + act_prj_tss_cputime_dict.get(key, 0)
        prj_ratio = 0.0
        if prj_limit_dict.get(key, 0) != 0.0:
            prj_ratio = (prj_total_sec / (prj_limit_dict.get(key, 0) * 60 * 60)) * 100

        prj_used_list.append([key,
                              prj_limit_dict.get(key, 0),
                              (prj_ocutime_dict.get(key, 0) + act_prj_cputime_dict.get(key, 0) + act_prj_tss_cputime_dict.get(key, 0)) / 60 / 60,
                              (prj_ocutime_dict.get(key, 0) + act_prj_cputime_dict.get(key, 0)) / 60 / 60,
                              (act_prj_tss_cputime_dict.get(key, 0)) / 60 / 60,
                              '{0:.2f}'.format(prj_ratio)])
        if (prj_limit_dict.get(key, 0) * 60 * 60) < (prj_ocutime_dict.get(key, 0) + act_prj_cputime_dict.get(key, 0) + act_prj_tss_cputime_dict.get(key, 0)):
            prj_exceeded_list.append(key)

    used_f = open('/opt/uge/Accounting_Statistics/logs/accounting/prj_used_pm' + POST_FIX, 'w')

    writer = csv.writer(used_f, lineterminator='\n')
    writer.writerows(prj_used_list)

    exceeded_f = open('/opt/uge/Accounting_Statistics/logs/accounting/prj_exceeded_pm' + POST_FIX, 'w')
    for x in prj_exceeded_list:
        exceeded_f.write(str(x) + "\n")

    # print "--- user usage ---"
    for key in user_limit_dict:
        user_total_sec = user_ocutime_dict.get(key, 0) + act_user_cputime_dict.get(key, 0) + act_user_tss_cputime_dict.get(key, 0)
        user_ratio = 0.0
        if user_limit_dict.get(key, 0) != 0.0:
            user_ratio = (user_total_sec / (user_limit_dict.get(key, 0) * 60 * 60)) * 100
        user_used_list.append([key,
                               user_limit_dict.get(key, 0),
                               (user_ocutime_dict.get(key, 0) + act_user_cputime_dict.get(key, 0) + act_user_tss_cputime_dict.get(key, 0)) / 60 / 60,
                               (user_ocutime_dict.get(key, 0) + act_user_cputime_dict.get(key, 0)) / 60 / 60,
                               (act_user_tss_cputime_dict.get(key, 0)) / 60 / 60,
                               '{0:.2f}'.format(user_ratio)])

    user_used_f = open('/opt/uge/Accounting_Statistics/logs/accounting/user_used_pm' + POST_FIX, 'w')

    # csv: user,limit,total,batch,interactive,ratio
    writer = csv.writer(user_used_f, lineterminator='\n')
    writer.writerows(user_used_list)

    # print "--- Exceeded limit group ---"

    for key in group_limit_dict:
        group_used_list.append([key,
                                group_limit_dict.get(key, 0),
                                (group_ocutime_dict.get(key, 0) + act_group_cputime_dict.get(key, 0) + act_group_tss_cputime_dict.get(key, 0)) / 60 / 60,
                                (group_ocutime_dict.get(key, 0) + act_group_cputime_dict.get(key, 0)) / 60 / 60,
                                (act_group_tss_cputime_dict.get(key, 0)) / 60 / 60])

    group_used_f = open('/opt/uge/Accounting_Statistics/logs/accounting/group_used_pm' + POST_FIX, 'w')

    # csv: group,limit,total,batch,interactive,ratio
    writer = csv.writer(group_used_f, lineterminator='\n')
    writer.writerows(group_used_list)

    used_f = open('/opt/uge/Accounting_Statistics/logs/accounting/prj_used_pm' + POST_FIX, 'w')

    # csv: project,limit,total,batch,interactive,ratio
    writer = csv.writer(used_f, lineterminator='\n')
    writer.writerows(prj_used_list)

    # Project Member's list
    try:
        prj_member_list_f = open('/opt/uge/Accounting_Statistics/etc/prj_member' + POST_FIX, 'r')
    except:
        sys.stderr.write("failed to open: prj_member" + POST_FIX + "\n")
        sys.exit(-1)

    reader = csv.reader(prj_member_list_f)

    prj_member_dict = {}
    prj_member_list = []

    for row in reader:
        i = 1
        row_l = len(row)
        list_of_member = []
        list_of_member_s = []
        list_of_member_d = []
        if row_l > 2:
            while i < row_l:
                list_of_member.append(row[i])
                list_of_member_s.append(row[i] + '_' + row[0] + '-s')
                list_of_member_d.append(row[i] + '_' + row[0] + '-d')
                prj_member_list.append(row[i] + '_' + row[0] + '-s')
                prj_member_list.append(row[i] + '_' + row[0] + '-d')
                i += 1
            prj_member_dict[row[0] + '-s'] = list_of_member_s
            prj_member_dict[row[0] + '-d'] = list_of_member_d
    # print prj_member_dict
    prj_member_used_list = []

    # Project Member's Usages
    prj_member_used_f = open('/opt/uge/Accounting_Statistics/logs/accounting/prj_member_used_pm' + POST_FIX, 'w')
    for prj_member in prj_member_list:
        prj_member_used_list.append([prj_member,
                                     (owner_prj_ocutime_dict.get(prj_member, 0) + act_owner_prj_cputime_dict.get(prj_member, 0) + act_owner_prj_tss_cputime_dict.get(prj_member, 0)) / 60 / 60,
                                     (owner_prj_ocutime_dict.get(prj_member, 0) + act_owner_prj_cputime_dict.get(prj_member, 0)) / 60 / 60,
                                     (act_owner_prj_tss_cputime_dict.get(prj_member, 0)) / 60 / 60])
    # csv: prj_member,total,batch,interactive,ratio
    writer = csv.writer(prj_member_used_f, lineterminator='\n')
    writer.writerows(prj_member_used_list)

    # print owner_prj_ocutime_dict
    # act_owner_prj_cputime_dict
    # act_owner_prj_tss_cputime_dict

    exceeded_f.close()
    f.close()
    limit_f.close()
    used_f.close()
    group_limit_f.close()
    group_used_f.close()

__doc__ = """{f}

Usage:
    {f} <fname> [-s | --start <start_time>] [-e | --end <end_time>] [-p | --post-fix <post_fix>]
    {f} -h | --help

Options:
    -s --start <START_TIME>  YYYYMMDDhhmmss
    -e --end <END_TIME>      YYYYMMDDhhmmss
    -p --post-fix <POST_FIX> YYYYMM
    -h --help                Show this screen and exit.
""".format(f=__file__)

from docopt import docopt
import datetime
import time


def parse():
    start_utime = 0
    now = datetime.datetime.now()
    end_utime = int(time.mktime(now.timetuple()))
    POST_FIX = ".csv"
    args = docopt(__doc__)
    if args['--start']:
        start_time = datetime.datetime.strptime(args['--start'][0], '%Y%m%d%H%M%S')
        start_utime = int(time.mktime(start_time.timetuple()))
    if args['--end']:
        end_time = datetime.datetime.strptime(args['--end'][0], '%Y%m%d%H%M%S')
        end_utime = int(time.mktime(end_time.timetuple()))
    if args['--post-fix']:
        POST_FIX = "." + args['--post-fix'][0]
    filename = args['<fname>']

    calc_accounting(filename, start_utime, end_utime, POST_FIX)

if __name__ == '__main__':

    parse()
