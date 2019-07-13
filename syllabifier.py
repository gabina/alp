# coding=utf-8


#Conjuntos útiles
V = {'a','e','i','o','u','á','é','í','ó','ú'} #vocales
C = {'b','c','d','f','g','h','j','k','l','m','n','ñ','p','q','r','s','t','v','w','x','y','z'} #consonantes
Vs = {'a','e','o'} #vocales fuertes
Vw = {'i','u'} #vocales débiles
Vwa = {'í','ú'} #vocales débiles tildadas
LR = {'l','r'}


#argumento: toma el string a separar en sílabas
def syllabifier(S):

	N = ""
	T = ""

	for i in range(0,len(S)-1):
		print("Letra",S[i])
		
		#corrobora hiatos
		if ((((S[i] in Vs) or (S[i] in Vwa)) and (S[i+1] in Vs)) or ((S[i] in V) and (S[i+1] in Vwa))):
			N = N + T + S[i] + "-"
			T = S[i+1]
			print("Primer if")
			continue
		
		#consonante + vocal	
		if ((S[i] in C) and (S[i+1] in V)):
			print("Segundo if")
			if (( S[i] in LR) and (S[i-1] in C)):
				print("Tercer if")
				if i>1:
					print("Cuarto if")
					print(T)
					N = N + T + "-"
					T = S[i-1] + S[i]
				else:
					T = T + S[i]
					
			else:
				N = N + T + "-"
				T = S[i]
		else:
			T = T + S[i]
		
		print("T: ",T)
	print(N+T+S[i+1])
	
# Llamamos a la función definida arriba
syllabifier("creían")
