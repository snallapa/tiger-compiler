sml _driver.sml $1
cat sysspim.s | cat - $1.s > temp && mv temp $1.s
cat runtime.s | cat - $1.s > temp && mv temp $1.s
