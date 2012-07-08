#----------------------------------------------------------------------
# File Makefile	
# Written by Chris Frisz	
# 	
# Created 10 Jan 2012	
# Last modified 10 Jan 2012	
# 	
# This Makefile is intended for use with CSCI-P423 and runs the the
# load_and_test.ss file. It may be extended to do other things as you
# may need.
#----------------------------------------------------------------------

#-- Variables --#
SC=petite
HS=ghci

HS_FLAGS=-v0

SCRIPT_DIR=scripts

SC_FILE=load_and_test.ss
HS_FILE=LoadAndTest.hs
HS_EXE=LoadAndTest
CG_FILE=compile_grammars.ss

SRC_GRAMMAR=source-grammar.ss

#-- Rules --#

# The main point of this file is to run the tests
all : grammars

# Run the testing on the compiler
grammars : $(SRC_GRAMMAR) GrammarCompiler
	@mkdir -p Framework{,Hs}/GenGrammars
	$(SC) --script $(SCRIPT_DIR)/$(CG_FILE) "$(SRC_GRAMMAR)"

scheme :
	@$(SC) $(SCRIPT_DIR)/$(SC_FILE)

haskell :
	@$(HS) $(HS_FLAGS) $(SCRIPT_DIR)/$(HS_FILE)

clean :
	rm -f t.s t
#	find . -name "*.o" -exec rm -f {} \;
#	find . -name "*.hi" -exec rm -f {} \;

.PHONY: scheme haskell grammars clean
