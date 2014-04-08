DIRS = ./:parser/:player/:cyphers/

SRCPARSER = $(wildcard parser/*.scala)
SRCPLAYER = $(wildcard player/*.scala)
SRCCYPHERS = $(wildcard cyphers/*.scala)
SRCMAIN = $(wildcard *.scala)
SRC = $(SRCPARSER) $(SRCPLAYER) $(SRCMAIN) $(SRCCYPHERS)

CLASSPARSER = $(SRCPARSER:.scala=.class)
CLASSPLAYER = $(SRCPLAYER:.scala=.class)
CLASSCYPHERS = $(SRCCYPHERS:.scala=.class)
CLASSMAIN = $(SRCMAIN:.scala=.class)
CLASS = $(CLASSPARSER) $(CLASSPLAYER) $(CLASSMAIN) $(CLASSCYPHERS)

all: $(CLASS)
cyphers: $(CLASSCYPHERS)

parser/Parser.class: parser/Input.class parser/Process.class parser/Term.class
parser/Process.class: parser/Term.class
player/Interpretor.class: $(CLASSPARSER) player/Channel.class
CYPHERS = $(filter-out cyphers/CryptoSystem.class cypher/TestCyphers.scala, $(CLASSCYPHERS))
$(CYPHERS): cyphers/CryptoSystem.class
cyphers/ElGamal.class: cyphers/Group.class
#cyphers/TestCyphers.class: $(CYPHERS)
# TODO : package
Aplipcation.class: $(CLASSPARSER) $(CLASSPLAYER)


%.class: %.scala
	scalac -d $(shell dirname $*) -classpath $(DIRS) $*.scala

clean:
	@rm -f *.class parser/*.class player/*.class cyphers/*.class $(SRC:.scala=.scala~)
