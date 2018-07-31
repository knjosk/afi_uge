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


def create_body(filename):

    return body


def create_message(from_addr, from_addr_name, to_addr, cc_addr, subject, body):

    cset = 'utf-8'
    msg = MIMEMultipart()
    msg["Subject"] = subject
    msg["From"] = from_addr_name + "<" + from_addr + ">"
    msg["To"] = to_addr
    msg["Cc"] = cc_addr
    msg["Date"] = formatdate()
    body = MIMEText(body.encode("utf-8"), 'plain', 'utf-8')
    msg.attach(body)
    return msg


def send(from_addr, to_addrs, cc_addrs, msg):
    smtp = smtplib.SMTP("localhost:25")
    smtp.sendmail(from_addr, to_addrs, msg.as_string())
    smtp.close()

if __name__ == '__main__':
    from_addr = "Admin@ifs.tohoku.ac.jp"
    from_addr_name = "Administrator"
    # to_addr = "afi-office@ifs.tohoku.ac.jp"
    to_addr = "fjse02@afifep001.ifs.ac.jp"
    subject = "Accout Report"
    filename = "/opt/uge/Accounting_Statistics/etc/prj_limit_pm.csv"

    body = create_body(filename)

    msg = create_message(from_addr, from_addr_name, to_addr, subject, body)
    send(from_addr, to_addr, msg)
