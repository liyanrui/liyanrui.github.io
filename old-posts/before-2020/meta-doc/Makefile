m4_files = $(wildcard *.m4)
objects = $(m4_files:.m4=.o)
all: $(objects)
%.o: %.m4
	./m4tomkd $<
	cp $@ ../posts/$*.md
clean:
	rm *.o
