bootstrap :
	stack build
	stack runhaskell -- utils/emplacedb.hs --Wall -Werror
