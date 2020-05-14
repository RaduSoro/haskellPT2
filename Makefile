# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#   

# Files that need to be generated from other files
DEPEND += Tokens.hs Parser.hs Lib.hs

# When "make" is invoked with no arguments, we build an executable 
#  after building everything that it depends on
all: $(DEPEND) myinterpreter

# Build an executable for Toy interpreter
myinterpreter: $(DEPEND) myinterpreter.hs
	ghc myinterpreter.hs

# Generate ML files from a parser definition file
Parser.hs : Parser.y
	@rm -f Parser.hs
	happy Parser.y
	@chmod -w Parser.hs

# Generate ML files from a lexer definition file

Tokens.hs : Tokens.x

	@rm -f Tokens.hs
	alex Tokens.x
	@chmod -w Tokens.hs

# Clean up the directory
clean::
	rm -rf Tokens.hs Parser.hs *.hi *.o *.info


