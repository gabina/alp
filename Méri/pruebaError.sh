#!/bin/bash

for i in {1..3}
	do		
			((j=i+1))
			echo "PRUEBA "$i
			cabal run -- -f ../Tests/Error/test$i.txt $j
	done
