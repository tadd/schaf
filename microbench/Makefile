BIN = schaf
SRC != ls src/*.erb
TARGET = $(SRC:src/%.erb=%)

all: benchmark

target: $(TARGET)

benchmark: benchmark.bash $(TARGET)
	./$< $(BIN)

%.scm: src/%.scm.erb
	erb -T - $< > $@

clean:
	rm -f $(TARGET)

.PHONY: all target benchmark clean
