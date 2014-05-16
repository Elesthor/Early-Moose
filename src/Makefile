DIRS = ./:parser/:player/:cyphers/

SRCPARSER = $(wildcard parser/*.scala)
SRCPLAYER = $(wildcard player/*.scala)
SRCCYPHERS = $(filter-out cyphers/CryptoSystem.scala cyphers/TestCyphers.scala cyphers/BigIntUtils.scala cyphers/NetworkTools.scala, $(wildcard cyphers/*.scala))
SRCMAIN = $(wildcard *.scala)
SRC = $(SRCPARSER) $(SRCPLAYER) $(SRCMAIN) $(SRCCYPHERS)

CLASSPARSER = $(SRCPARSER:.scala=.class)
CLASSPLAYER = $(SRCPLAYER:.scala=.class)
CLASSCYPHERS = $(SRCCYPHERS:.scala=.class)
CLASSMAIN = $(SRCMAIN:.scala=.class)
CLASS = $(CLASSPARSER) $(CLASSPLAYER) $(CLASSMAIN) $(CLASSCYPHERS) cyphers/TestCyphers.class

all: $(CLASS)
cyphers: $(CLASSCYPHERS) cyphers/TestCyphers.class

parser/Parser.class: parser/Input.class parser/Process.class parser/Term.class
parser/Term.class: cyphers/perso/utils/NetworkTools.class
parser/Process.class: parser/Term.class
player/Interpretor.class: $(CLASSCYPHERS) $(CLASSPARSER) player/Channel.class

cyphers/perso/utils/NetworkTools.class:cyphers/NetworkTools.scala
	scalac -d cyphers -classpath $(DIRS) cyphers/NetworkTools.scala
cyphers/perso/utils/BigIntUtils.class:cyphers/BigIntUtils.scala
	scalac -d cyphers -classpath $(DIRS) cyphers/BigIntUtils.scala
$(SRCCYPHERS): cyphers/CryptoSystem.class cyphers/perso/utils/BigIntUtils.class cyphers/perso/utils/NetworkTools.class
cyphers/ElGamal.class: cyphers/Group.class
cyphers/TestCyphers.class: $(SRCCYPHERS)
cyphers/CryptoSystem.class: cyphers/perso/utils/NetworkTools.class
Application.class: $(CLASSCYPHERS) $(CLASSPARSER) $(CLASSPLAYER)


%.class: %.scala
	scalac -d $(shell dirname $*) -classpath $(DIRS) $*.scala

clean:
	@rm -f *.class parser/*.class player/*.class cyphers/*.class $(SRC:.scala=.scala~)
	@rm -f cyphers/NetworkTools.scala~ cyphers/BigIntUtils.scala~ cyphers/TestCyphers.scala~
	@rm -fr cyphers/perso

