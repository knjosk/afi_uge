#!/usr/bin/python

__doc__ = """{f}

Usage:
    {f} <fname1> <fname2>
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
    filename = args['<fname1>']
    if filename == "-":
        xmlString = sys.stdin.read()
    else:
        with open(filename, 'r') as f:
            xmlString = f.read()

    avail_core_inter = 79
    avail_core_mpi1  = 160
    avail_core_mpi2  = 80
    avail_core_smp   = 715
    avail_core_smp1  = 60
    avail_core_smp2  = 200
    avail_cores = { "inter": 79, "mpi1": 160, "mpi2": 80, "smp": 715, "smp1": 60, "smp2":200 }
    q_list = [u'inter', u'mpi1', u'mpi2', u'smp', u'smp1', u'smp2']
    
    dic = xmltodict.parse(xmlString)
#    print len(dic["job_info"]["job_info"]["job_list"])
    l_dic = len(dic["job_info"]["queue_info"]["job_list"])
#    print l_dic
    i = 0
    list = []
    while i < l_dic:
        job_number = dic["job_info"]["queue_info"]["job_list"][i]["JB_job_number"]
        s_time = datetime.datetime.strptime(dic["job_info"]["queue_info"]["job_list"][i]["JAT_start_time"], '%Y-%m-%dT%H:%M:%S.%f')
        us_time = int(time.mktime(s_time.timetuple()))
        q_name = dic["job_info"]["queue_info"]["job_list"][i]["queue_name"].split(".")[0]
        job_jcl = dic["job_info"]["queue_info"]["job_list"][i]["jclass_name"]
        #q_name = job_jcl.split(".")[0]
        ncores = int(dic["job_info"]["queue_info"]["job_list"][i]["slots"])
        elaps = int(dic["job_info"]["queue_info"]["job_list"][i]["hard_request"][-2]["#text"])
        ue_time = us_time + elaps
        e_time = datetime.datetime.fromtimestamp(ue_time)
        #print job_number, ue_time, e_time, ncores, q_name
        list.append([ q_name, ue_time, ncores, e_time, job_number ])
        #list.sort(key=lambda x:(x[0], x[1]))
        list.sort(key=itemgetter(0,1))
        i += 1
    #print list
    qlist = []
    for l in list:
        print type(l)
        i = 0
        while i < 6:
            qlist[i] = l
            i+=1
        print qlist
        
    l_list = len(list)
    print l_list

    l2_dic = len(dic["job_info"]["job_info"]["job_list"])
#    print l_dic
    i = 0
    list2 = []
    while i < l2_dic:
        wjob_number = dic["job_info"]["job_info"]["job_list"][i]["JB_job_number"]
        wjob_prio = float(dic["job_info"]["job_info"]["job_list"][i]["JAT_prio"])
        wjob_name = dic["job_info"]["job_info"]["job_list"][i]["JB_name"]
        wjob_user = dic["job_info"]["job_info"]["job_list"][i]["JB_owner"]
        wjob_jcl  = dic["job_info"]["job_info"]["job_list"][i]["jclass_name"]
        wq_name = wjob_jcl.split(".")[0]
        wncores = int(dic["job_info"]["job_info"]["job_list"][i]["slots"])
        welaps = int(dic["job_info"]["job_info"]["job_list"][i]["hard_request"][-2]["#text"])
        #print wjob_number, wjob_prio, wjob_name, wjob_user, wjob_jcl, wncores, welaps, wq_name
        list2.append([ wq_name, wjob_number, wjob_prio, wjob_name, wjob_user, wjob_jcl, wncores ])
        list2.sort(key=lambda x:(x[0],-x[2],x[1]))
        #list2.sort(key=itemgetter(0,2,1))
        i += 1
    #print list2
    for l2 in list2:
        print(l2)

    l_list2 = len(list2)
    
    #    print dic["job_info"]["job_info"]["job_list"][0]["hard_request"][2]["#text"]
    i = 0
    j = 0
    while j < l_list:
        print j
        q = list[j][0]
        avail_cores[q] += list[j][2]
        while i < l_list2:
            req_queue = list2[i][0]
            req_cores = list2[i][6]
            print i, q, req_cores, avail_cores[q]
            if avail_cores[q] >= req_cores:
                print q, list2[i], list[j][3]
                avail_cores[q] -= req_cores
                i += 1
            break
        j += 1

        
    jsonString = json.dumps(xmltodict.parse(xmlString), indent=4)
    
if __name__ == '__main__':
    parse()
    
