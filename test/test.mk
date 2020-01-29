ifeq ($(OS), Windows_NT)
	RMRF = RMDIR /S /Q $1
	MKDIR = $(if $(wildcard $1),,MKDIR $1)
else
	MKDIR = mkdir -p $1
	RMRF = rm -rf $1
endif

NAME = $(notdir $(CURDIR))

ICAS = $(realpath ../../vm/ic) $(realpath ../../bin/as.input)
ICLD = $(realpath ../../vm/ic) $(realpath ../../bin/ld.input)

prep:
	$(RMRF) output
	$(MKDIR) output

output/%.input: output/%.o
	echo .$$ | cat $^ - | $(ICLD) > $@ 2> /dev/null || true
	diff $(notdir $@) $@ > /dev/null 2> /dev/null || diff $(notdir $@) $@

output/%.a: output/%.o
	echo .L | cat - $^ > $@ || true
	diff $(notdir $@) $@ > /dev/null 2> /dev/null || diff $(notdir $@) $@

output/%.o: %.s
	$(ICAS) < $< > $@ 2> /dev/null || true
	diff $(notdir $@) $@ > /dev/null 2> /dev/null || diff $(notdir $@) $@

clean:
	$(RMRF) output

.PHONY: prep clean
