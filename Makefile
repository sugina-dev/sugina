default : pubdyn/articles/.makefile
	cd pubdyn/articles && $(MAKE) -f .makefile

bootstrap :
	stack build
	stack runhaskell -- -Wall -Werror utils/emplacedb.hs
