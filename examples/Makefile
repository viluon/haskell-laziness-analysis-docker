
SRC_DIR    = src
TARGET_DIR = target
GHC_FLAGS  = -fplugin=TracingPlugin -O0

.PHONY: all clean

all: ${TARGET_DIR}/control-flow ${TARGET_DIR}/difficult ${TARGET_DIR}/foo ${TARGET_DIR}/qsort ${TARGET_DIR}/sieve ${TARGET_DIR}/test-lazy ${TARGET_DIR}/test-qsort

clean:
	rm -r ${TARGET_DIR}

${TARGET_DIR}:
	mkdir ${TARGET_DIR}

${TARGET_DIR}/%: ${SRC_DIR}/%.hs ${TARGET_DIR}
	ghc ${GHC_FLAGS} -outputdir ${TARGET_DIR} -o ${TARGET_DIR}/$* $<
