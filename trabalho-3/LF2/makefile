GHC = stack ghc -- --make
GHC_FLAGS = -no-keep-o-files -no-keep-hi-files

%.exe:
	${GHC} -o $@ ${GHC_FLAGS} ${@:.exe=.hs}

all: Interpret.exe InterpretOld.exe TestLF.exe

.PHONY: all
