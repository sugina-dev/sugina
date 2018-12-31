PANDOC := pandoc --toc --toc-depth=4 -c ../transform.css
INDEX := index.csv

default : clean_original_contents_csv $(patsubst .%.md,write_into_new_csv_%,$(wildcard .*.md))

clean_original_contents_csv :
	$(RM) $(INDEX)

write_into_new_csv_% : .%.md %.html
	TITLE="`grep -Po -m 1 '(?<=# ).+' $<`" && echo "$*,$$TITLE" >> $(INDEX)

.PRECIOUS: %.html
%.html : .%.md Makefile
	TITLE="`grep -Po -m 1 '(?<=# ).+' $<`" && $(PANDOC) -s $< -o $@ -Mtitle="$$TITLE"

clean :
	$(RM) *.html
