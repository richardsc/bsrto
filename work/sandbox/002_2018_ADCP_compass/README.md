# 002_2018_ADCP_compass

On February 24th 2019, the observatory stopped sending pole compass files. The last file to be received was `19022408.pcm`. It is not known at this time (2019-05-15) what the issue is, but given the age of the pole compass sensors and boards, a failure of some kind is likely.

Here I want to look at a comparison of the ADCP compass readings vs the pole compass readings, for the period when the PC was still reporting data.

On closer look, while there are still files up until Feb 24, it looks like the last *valid* file came in on Feb. 17, which was file `19021704.pcm`. After this file, the files are all quite small (~40 B compared to ~300 B), and look like:

```
02/17/2019 06:07:14
\$IIHDT,0.0,T*22
```

suggesting that something was wrong with the PC itself. I suspect a battery failure, since it continued to only log those single sample files until the 24th and then stopped reporting data.
 
## 01.R

