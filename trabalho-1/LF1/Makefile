GHC = stack ghc -- --make
GHC_FLAGS = -odir ${TMP_DIR} -hidir ${TMP_DIR}

TMP_DIR = tmp

%.exe:
	${GHC} -o $@ ${GHC_FLAGS} ${@:.exe=.hs}
	rd /s /q tmp 

all: Interpret.exe TestLF.exe

.PHONY: all
