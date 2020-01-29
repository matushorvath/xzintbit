NAME = $(notdir $(CURDIR))

ICAS = $(realpath ../../vm/ic) $(realpath ../../bin/as.input)
ICLD = $(realpath ../../vm/ic) $(realpath ../../bin/ld.input)

prep:
	rm -rf output
	mkdir -p output

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
	rm -rf output

.PHONY: prep clean
