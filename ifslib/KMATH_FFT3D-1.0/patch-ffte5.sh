#!/bin.sh

if [ -e ffte-5.0 ]; then
	rm ffte
	ln -s ffte-5.0 ffte
	touch ffte/factor.f
fi
