#!/usr/bin/python
# -*- coding: utf-8 -*-

import smtplib

from email.mime.text import MIMEText
from email.header import Header
from email.utils import formatdate


from_address = 'info＠ifs.tohoku.ac.jp'
#to_address = 'afi-office＠ifs.tohoku.ac.jp'
to_address = 'osaki.kenji@jp.fujitsu.com'

charset = 'ISO-2022-JP'
subject = 'Account report'
text = 'メールの本文です'

msg = MIMEText(text, 'plain', charset)
msg['Subject'] = Header(subject, charset)
msg['From'] = from_address
msg['To'] = to_address
msg['Date'] = formatdate(localtime=True)

smtp = smtplib.SMTP('xxx.xx.jp')
smtp.sendmail(from_address, to_address, msg.as_string())
smtp.close()


prj_used_f = open('/opt/uge/Accounting_Statistics/etc/prj_used_pm.csv', 'r')
reader = csv.reader(prj_used_f)
for row in reader:
    prj_used_dict[row[0]] = float(row[1])
    prj_used_dict[row[0]] = float(row[1])


if __name__ == '__main__':
    from_addr = 'spam@example.com'
    to_addr = 'your_email_address@docomo.ne.jp'
    msg = create_deco(from_addr, to_addr)
    send(from_addr, to_addr, msg)
