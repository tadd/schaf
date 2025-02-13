CC=gcc
OPTFLAGS=-O0 -ggdb3
CFLAGS=-std=gnu17 -Wall -Wextra $(OPTFLAGS) $(XCFLAGS)
LIBS=-lm
ANALYZER=-fanalyzer
SANITIZER=-fsanitize=undefined #,address
TIMEOUT=timeout 2

OBJ_COMMON=schaf.o utils.o
OBJ=$(OBJ_COMMON) main.o
OBJ_TEST=$(OBJ_COMMON) basic-test.o

all: schaf test

schaf: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

basic-test: $(OBJ_TEST)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS) -lcriterion

test: test-c test-scheme

test-c: basic-test
	$(TIMEOUT) ./$<
test-c-san: basic-test-san
	$(TIMEOUT) ./$<

test-scheme: schaf
	$(TIMEOUT) ./$< test.scm
test-scheme-san: schaf-san
	$(TIMEOUT) ./$< test.scm

clean:
	rm -f schaf basic-test *-san *.o

analyze: $(OBJ:.o=.analyzer)

sanitize: schaf-san test-san
test-san: test-c-san test-scheme-san

schaf-san: $(OBJ:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS)

basic-test-san: $(OBJ_TEST:.o=.san.o)
	$(CC) $(CFLAGS) $(SANITIZER) -o $@ $^ $(LIBS) -lcriterion

%.o: %.c
	$(CC) $(CFLAGS) -c $<

%.s: %.c
	$(CC) $(CFLAGS) -S -fverbose-asm -c $<

%.analyzer: %.c
	$(CC) $(CFLAGS) $(ANALYZER) -c $< -o /dev/null

%.san.o: %.c
	$(CC) $(CFLAGS) $(SANITIZER) -c $< -o $@

microbench: schaf
	@$(MAKE) -C $@

basic-test.o: schaf.h
main.o: schaf.h utils.h
schaf.o: schaf.h utils.h
utils.o: utils.h

.PHONY: all clean test test-c test-scheme analyze sanitize \
	test-san test-c-san test-scheme-san \
	microbench
