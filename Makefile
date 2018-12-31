bootstrap :
	stack build
	stack runhaskell -- -Wall -Werror utils/emplacedb.hs
