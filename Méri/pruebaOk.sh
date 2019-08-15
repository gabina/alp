#!/bin/bash

for i in {1..6}
	do		
			((j=i+1))
			echo "PRUEBA "$i
			cabal run -- -f ../Tests/Ok/ok$i.txt $j
	done
