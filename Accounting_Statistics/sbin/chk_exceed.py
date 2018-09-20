#!/usr/bin/python
# -*- coding: utf-8 -*-

import os.path
import datetime
import smtplib
import codecs
import collections
import tempfile
import unittest
import time
import re
import os
import sys
import datetime
import shutil
import MimeWriter
import mimetools
import base64
import StringIO

from email import Encoders
from email.Utils import formatdate
from email.MIMEBase import MIMEBase
from email.MIMEMultipart import MIMEMultipart
from email.MIMEText import MIMEText

import csv
from collections import defaultdict
from docopt import docopt


def create_body(filename):
    THRESHOLD = 90
    exceed_list = []

    used_f = open(filename, 'r')

    reader = csv.reader(used_f)
    for row in reader:
        if float(row[5]) > THRESHOLD:
            exceed_list.append(row)

    l_exceed_list = len(exceed_list)
    body = ""
    i = 0
    while i < l_exceed_list:

        prj_name = exceed_list[i][0].split("-")[0]
        sys_code = exceed_list[i][0].split("-")[1]

        if sys_code == "s":
            sys_name = "vSMP"
        else:
            sys_name = "PCCL"

        exceeded_p = exceed_list[i][5]

        body += '{} exceeded the threshold in {} and became {}%.\n'.format(prj_name, sys_name, exceeded_p)
        i += 1
    return body


def create_message(from_addr, from_addr_name, to_addr, subject, body):

    cset = 'utf-8'
    msg = MIMEMultipart()
    msg["Subject"] = subject
    msg["From"] = from_addr_name + "<" + from_addr + ">"
    msg["To"] = to_addr
    msg["Date"] = formatdate()
    body = MIMEText(body.encode("utf-8"), 'plain', 'utf-8')
    msg.attach(body)
    return msg


def send(from_addr, to_addrs, msg):
    smtp = smtplib.SMTP("localhost:25")
    smtp.sendmail(from_addr, to_addrs, msg.as_string())
    smtp.close()

__doc__ = """{f}

Usage:
    {f} <used_file>
    {f} -h | --help

Options:
    -h --help          Show this screen and exit.
""".format(f=__file__)

if __name__ == '__main__':

    args = docopt(__doc__)
    filename = args['<used_file>']

    from_addr = "sc-support@ifs.tohoku.ac.jp"
    from_addr_name = "sc-support"
    to_addr = "sc-support@ifs.tohoku.ac.jp"
    # to_addr = "afi-office@ifs.tohoku.ac.jp"
    subject = "Account Report"

    body = create_body(filename)
    if len(body) > 0:
        msg = create_message(from_addr, from_addr_name, to_addr, subject, body)
        send(from_addr, to_addr, msg)
