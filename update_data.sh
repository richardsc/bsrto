#!/bin/sh
R --no-save < download_data.R > download_data.out
R --no-save < mc.R > mc.out
R --no-save < icl.R > icl.out
R --no-save < ips.R > ips.out
