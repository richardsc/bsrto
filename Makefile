all:

clean:
	-rm *.out *.rda
	-rm adp.000 baro.hpb imm.mc imm_old.mc mcA.mc mcH.mc mcI.mc
	-rm icl/*

clean-icl:
	-rm icl.out icl.rda
	-rm icl/*.png
