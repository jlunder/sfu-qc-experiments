for X in adder bar div hyp log2 max multiplier sin sqrt square; do
  #abc -o processed/pp-$X -c "&r original/$X; &st; &synch2; &if -m -a -K 2; &mfs -W 10; &st; &dch; &if -m -a -K 2; &mfs -W 10"
  CMDS="&synch2; &if -m -a -K 2; &mfs -W 10; &st; &dch; &if -m -a -K 2; &mfs -W 10;"
  abc -c "&r original/$X.aig; &st; $CMDS $CMDS $CMDS $CMDS &w -l processed/$X.mlut" # &w -p processed/$X.v"
  python3 mlut2xag.py processed/$X.mlut > processed/$X-xag.txt
done
