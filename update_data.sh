#!/bin/sh
R --no-save < download_data.R > download_data.out
R --no-save < mc.R > mc.out
R --no-save < icl.R > icl.out
R --no-save < met.R > met.out
R --no-save < baro.R > baro.out
R --no-save < ips.R > ips.out
R --no-save < pc.R > pc.out
R --no-save < adp.R > adp.out
