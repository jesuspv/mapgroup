###############################################################################
# Constants
###############################################################################

APP_NAME=mapgroup

SANITY_FLAGS=-Wall -Werror
OPTIMIZATION_FLAGS=-O2 -funfolding-use-threshold=16 -optc-O3 -threaded
RUNTIME_FLAGS=-rtsopts
PATH_FLAGS=-isrc -odir obj -hidir obj

###############################################################################
# Generic Rules
###############################################################################

.PHONY: all bin clean dist

all: bin

bin: bin/$(APP_NAME)

clean:
	rm -rf bin obj src/*.{o,hi}

dist: clean
	tar czvf ../$(APP_NAME).tgz --exclude=.git --exclude=*.tgz ../$(APP_NAME)*

###############################################################################
# Specific Rules
###############################################################################

bin/$(APP_NAME): src/*.hs
	@mkdir -p bin
	ghc $(SANITY_FLAGS) $(OPTIMIZATION_FLAGS) $(RUNTIME_FLAGS) $(PATH_FLAGS) --make src/Main.hs -o $@
