CMDS=&synch2; &if -m -a -K 2; &mfs -W 10; &st; &dch; &if -m -a -K 2; &mfs -W 10; 

BENCHMARKS=adder bar div hyp log2 max multiplier sin sqrt square
MLUTS=$(patsubst %,processed/%.mlut,$(BENCHMARKS))
XAGS=$(patsubst %,processed/%-xag.txt,$(BENCHMARKS))

processed/%.mlut: original/%.aig
	abc -c "&r $<; &st; $(CMDS) $(CMDS) $(CMDS) $(CMDS) &w -l $@"

%-xag.txt: %.mlut mlut2xag
	python3 mlut2xag.py $< > $@

mluts: $(MLUTS)
XAGS: $(XAGS)
all: mluts xags

.phony: all mluts xags
