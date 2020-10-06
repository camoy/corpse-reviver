ARTIFACT = ../corpse-reviver-artifact
SRCS = ../corpse-reviver/scribblings/*.scrbl \
       $(ARTIFACT)/scribblings/*.scrbl \
       ../corpse-reviver-benchmark/scribblings/*.scrbl
DEST = .
SCRIBBLE = raco scribble

all: $(SRCS)
	mv $(ARTIFACT)/data $(ARTIFACT)/empty_data
	mv $(ARTIFACT)/author_data $(ARTIFACT)/data
	$(SCRIBBLE) ++xref-in setup/xref load-collections-xref \
	            --redirect-main http://docs.racket-lang.org/ \
	            --dest $(DEST) \
	            --html $^
	cp corpse-reviver-artifact.html index.html
	mv $(ARTIFACT)/data $(ARTIFACT)/author_data 
	mv $(ARTIFACT)/empty_data $(ARTIFACT)/data

clean:
	rm $(DEST)/*.html $(DEST)/*.css $(DEST)/*.js $(DEST)/*.svg 

.PHONY: all clean
