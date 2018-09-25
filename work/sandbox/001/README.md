# 2018 pole compass issues

# 01.R

It looks like the PC data are always starting from 0/360, and eventually settling (after about 14-17 seconds) on a stable compass value. The question here is if only the last N points are averaged to get a good compass reading (rather than averaging all of them), what is the best N?

From the produced graphs, it looks like N=3.
