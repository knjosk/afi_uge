#!/usr/bin/python
import re

pattern = r'([+-]?[0-9]+\.?[0-9]*)'

text = 'impi8'

print('findall:', re.findall(pattern, text))

lists = re.findall(pattern, text)

print lists
