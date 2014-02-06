SHELL  = /bin/sh

YO     = Output.your
YC     = Output.correct

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

# like "make run", but use a nicer diff.
runv: trans
	java Main > $(YO) 2>&1
	cmp -s $(YC) $(YO) || tkdiff $(YC) $(YO)

# invoke via "make clean".
clean:
	/bin/rm -f *.class *~ $(YO)

# just do `make remake' instead of `make clean; make'
remake:	clean trans

# just do `make rerun' instead of `make remake; make run'
rerun:	remake run
