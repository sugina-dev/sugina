default : pubdyn/static/.makefile
	cd pubdyn/static && $(MAKE) -f .makefile

bootstrap :
	stack build
	stack runhaskell -- -Wall -Werror utils/emplacedb.hs
