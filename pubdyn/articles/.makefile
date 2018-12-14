PANDOC = pandoc --toc --toc-depth=4

TARGETS = index.html hello-world.html

default : $(TARGETS)

index.html : .index.md .makefile
	$(PANDOC) -s $< -o $@ -c ../transform.css -Mtitle=Index

hello-world.html : .hello-world.md .makefile
	$(PANDOC) -s $< -o $@ -c ../transform.css -Mtitle='Hello World'

clean :
	rm -f $(TARGETS)
