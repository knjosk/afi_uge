#!/bin/bash

source /etc/profile.d/settings.sh

tmp_b=/tmp/project_$1.before
tmp_a=/tmp/project_$1.after
/opt/uge/8.5.5/bin/lx-amd64/qconf -sprj $1 > $tmp_b
sed -e "s/^oticket.*/oticket $2/" $tmp_b > $tmp_a
/opt/uge/8.5.5/bin/lx-amd64/qconf -Mprj $tmp_a
