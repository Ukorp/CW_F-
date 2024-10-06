fun fact (x) \
    if (x = 1) \
        1
    / else \
	if (x = 0) \
		1
	/ else \
        	x * fact(x - 1)
    /
/

printint fact(8)