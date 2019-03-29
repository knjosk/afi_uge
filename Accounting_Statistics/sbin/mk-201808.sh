#!/bin/bash
./c_statistics.py ../../sample/accounting -s 20180806000000 -e 20180901000000 -p 201808
./c_statistics.py ../../sample/accounting -s 20180901000000 -e 20181001000000 -p 201809
./c_statistics.py ../../sample/accounting -s 20181001000000 -e 20181101000000 -p 201810
./c_statistics.py ../../sample/accounting -s 20181101000000 -e 20181201000000 -p 201811
./c_statistics.py ../../sample/accounting -s 20181201000000 -e 20190101000000 -p 201812
./c_statistics.py ../../sample/accounting -s 20190101000000 -e 20190130000000
