SHELL  = /bin/sh

YO     = Output.your
YC     = Output.correct
YO1    = Output.your1
YC1    = Output.correct1
YO2    = Output.your2
YC2    = Output.correct2
YO3    = Output.your3
YC3    = Output.correct3

# note that make will always retranslate all needed .java files,
# but that's OK since translation for this program is quick.
# (That assumes no junk .java files in this directory.)
# (That's unlike using make for C++ where one usually specifies dependencies;
#  we won't bother trying since translation is quick.)
#
# (be sure to put class X in file X.java)

# the -Xlint checks for a bunch of bad things.
# must use -Xlint; mustn't have any warnings.
# except for the provided Main.java (which has some redundant casts)

# if you have non-standard ls or grep, you'll need to give pathnames:
## NotMainJava = `/bin/ls -1 *.java | /bin/grep -v -w Main.java`
NotMainJava = `ls -1 *.java | grep -v -w Main.java`

# instead of "echo" below, could instead use here
#    NotMainJava := $(shell /bin/ls -1 *.java | /bin/grep -v -w Main.java)
# but that mightn't be supported by all makes.

trans:
	javac        Main.java
	@# make the output a bit nicer looking (i.e., expand $(NotMainJava))
	@echo javac -Xlint $(NotMainJava)
	@javac -Xlint $(NotMainJava)

# invoke via "make run" to run and compare output with correct output.
run: trans
	java Main > $(YO) 2>&1
	diff $(YC) $(YO)
	-if java Main ConstantIt > $(YO1) 2>&1; then echo "wrong exit status"; else echo "OK"; fi
	diff $(YC1) $(YO1)
	-if java Main DeltaIt > $(YO2) 2>&1; then echo "wrong exit status"; else echo "OK"; fi
	diff $(YC2) $(YO2)
	-if java Main JumbleIt > $(YO3) 2>&1; then echo "wrong exit status"; else echo "OK"; fi
	diff $(YC3) $(YO3)

# like "make run", but use a nicer diff.
runv: trans
	java Main > $(YO) 2>&1
	cmp -s $(YC) $(YO) || tkdiff $(YC) $(YO)
	-if java Main ConstantIt > $(YO1) 2>&1; then echo "wrong exit status"; else echo "OK"; fi
	cmp -s $(YC1) $(YO1) || tkdiff $(YC1) $(YO1)
	-if java Main DeltaIt > $(YO2) 2>&1; then echo "wrong exit status"; else echo "OK"; fi
	cmp -s $(YC2) $(YO2) || tkdiff $(YC2) $(YO2)
	-if java Main JumbleIt > $(YO3) 2>&1; then echo "wrong exit status"; else echo "OK"; fi
	cmp -s $(YC3) $(YO3) || tkdiff $(YC3) $(YO3)

# invoke via "make clean".
clean:
	/bin/rm -f *.class *~ $(YO) $(YO1) $(YO2) $(YO3)

# just do `make remake' instead of `make clean; make'
remake:	clean trans

# just do `make rerun' instead of `make remake; make run'
rerun:	remake run
