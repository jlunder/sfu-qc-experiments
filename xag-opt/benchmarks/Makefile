CMDS=&synch2; &if -m -a -K 2; &mfs -W 10; &st; &dch; &if -m -a -K 2; &mfs -W 10; 
processed/%.mlut: original/%.aig
	abc -c "&r $<; &st; $(CMDS) $(CMDS) $(CMDS) $(CMDS) &w -l $@"

%.xagb: %.mlut mlut2xag.py
	python3 mlut2xag.py $< > $@

BENCHMARKS=adder bar div hyp log2 max multiplier sin sqrt square
MLUTS=$(patsubst %,processed/%.mlut,$(BENCHMARKS))
XAGBS=$(patsubst %,processed/%.xagb,$(BENCHMARKS))

mluts: $(MLUTS)
xagbs: $(XAGBS)
all: mluts xags

.phony: all mluts xags
