CMDS=&synch2; &if -m -a -K 2; &mfs -W 10; &st; &dch; &if -m -a -K 2; &mfs -W 10; 
processed/%.mlut: original/%.aig
	abc -c "&r $<; &st; $(CMDS) $(CMDS) $(CMDS) $(CMDS) &w -l $@"

# %-xag.txt: %.mlut mlut2xag.py
# 	python3 mlut2xag.py $< > $@

%.hs:
	python3 mlut2xag.py $< > $@

MLUTS=$(patsubst %,processed/%.mlut,adder bar div hyp log2 max multiplier sin sqrt square)
XAGS=$(patsubst %,processed/%.hs,Adder Bar Div Hyp Log2 Max Multiplier Sin Sqrt Square)

processed/Adder.hs: processed/adder.mlut mlut2xag.py
processed/Bar.hs: processed/bar.mlut mlut2xag.py
processed/Div.hs: processed/div.mlut mlut2xag.py
processed/Hyp.hs: processed/hyp.mlut mlut2xag.py
processed/Log2.hs: processed/log2.mlut mlut2xag.py
processed/Max.hs: processed/max.mlut mlut2xag.py
processed/Multiplier.hs: processed/multiplier.mlut mlut2xag.py
processed/Sin.hs: processed/sin.mlut mlut2xag.py
processed/Sqrt.hs: processed/sqrt.mlut mlut2xag.py
processed/Square.hs: processed/square.mlut mlut2xag.py

mluts: $(MLUTS)
xags: $(XAGS)
all: mluts xags

.phony: all mluts xags
