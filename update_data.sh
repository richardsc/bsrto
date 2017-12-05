#!/bin/sh
LANG=C.UTF-8 R --no-save < download_data.R > download_data.out
LANG=C.UTF-8 R --no-save < mc.R > mc.out
LANG=C.UTF-8 R --no-save < icl.R > icl.out
LANG=C.UTF-8 R --no-save < met.R > met.out
LANG=C.UTF-8 R --no-save < baro.R > baro.out
LANG=C.UTF-8 R --no-save < ips.R > ips.out
