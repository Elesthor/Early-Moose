DIRS = ./:parser/:player/

SRCPARSER = $(wildcard parser/*.scala)
SRCPLAYER = $(wildcard player/*.scala)
SRCMAIN = $(wildcard *.scala)
SRC = $(SRCPARSER) $(SRCPLAYER) $(SRCMAIN)

CLASSPARSER = $(SRCPARSER:.scala=.class)
CLASSPLAYER = $(SRCPLAYER:.scala=.class)
CLASSMAIN = $(SRCMAIN:.scala=.class)
CLASS = $(CLASSPARSER) $(CLASSPLAYER) $(CLASSMAIN)

compile: $(CLASS)

parser/Parser.class: parser/Input.class parser/Process.class parser/Term.class player/Channel.class
parser/Process.class: parser/Term.class player/Channel.class
player/Interpretor.class: $(CLASSPARSER) player/Channel.class
Application.class: $(CLASSPARSER) $(CLASSPLAYER)

%.class: %.scala
	scalac -d $(shell dirname $*) -classpath $(DIRS) $*.scala

clean:
	@rm -f *.class parser/*.class player/*.class $(SRC:.scala=.scala~)
