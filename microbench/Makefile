BIN=../schaf
SRC!=ls src/*.erb
TARGET=$(SRC:src/%.erb=%)
NLOOP=5
BENCHFLAGS=-TM

all: benchmark

target: $(TARGET)

benchmark: $(TARGET)
	@for t in $^; do \
	    echo $$t:; \
	    for i in `seq ${NLOOP}`; do \
	        $(BIN) $(BENCHFLAGS) $(TARGET); \
	    done \
	done

%.scm: src/%.scm.erb
	erb -T - $< > $@

clean:
	rm -f $(TARGET)

.PHONY: all target benchmark clean
