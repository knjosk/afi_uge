#!/bin/bash
tmp_b=/tmp/project_$1.before
tmp_a=/tmp/project_$1.after
qconf -sprj $1 > $tmp_b
sed -e "s/^oticket.*/oticket $2/" $tmp_b > $tmp_a
qconf -Mprj $tmp_a
