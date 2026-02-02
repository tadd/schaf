BIN = schaf
SRC != ls src/*.erb
TARGET = $(SRC:src/%.erb=%)
N ?= 2

all: benchmark

target: $(TARGET)

benchmark: benchmark.bash $(TARGET)
	./$< $(BIN) $(N)

%.scm: src/%.scm.erb
	erb -T - $< > $@

clean:
	rm -f $(TARGET)

.PHONY: all target benchmark clean
