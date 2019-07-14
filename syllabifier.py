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

	#N es el texto dividido en sílabas
	N = ""
	#T es la sílaba hasta el momento
	T = ""

	for i in range(0,len(S)-1):
		print("Letra",S[i])
		
		#corrobora hiatos. En este caso tiene que separar la sílaba
		if ((((S[i] in Vs) or (S[i] in Vwa)) and (S[i+1] in Vs)) or ((S[i] in V) and (S[i+1] in Vwa))):
			print("Primer if")			
			N = N + T + S[i] + "-"
			#T = S[i+1]
			T = ""			
			print("T: ",T)
			print(" ")
			continue
		
		#consonante + vocal	
		if ((S[i] in C) and (S[i+1] in V)):
			print("Segundo if")
			if (( S[i] in LR) and (S[i-1] in C)):
				print("Tercer if")
				if i>1:
					print("Cuarto if")
					N = N + T + "-"
					T = S[i-1] + S[i]
				else:
					print("Cuarto else")
					T = T + S[i]
					
			else:
				print("Tercer else")
				N = N + T + "-"
				T = S[i]
		else:

			print("Segundo else")
			T = T + S[i]
		
		print("T: ",T)
		print(" ")
	print(N+T+S[i+1])
	
# Llamamos a la función definida arriba
syllabifier("En la escuela secundarias claramente tuve una materia, cuyo nombre nunca supe, de la que solo recuerdo dos cosas. Una es que le decíamos Tirapelli porque ese era el apellido del docente. La otra es la primera clase. Vimos un documental sobre la obsolescencia programada. En una escena mostraban una habitación completamente venida abajo, en donde lo único que había sobrevivido era una lamparita encendida. Había quedado prendida por no sé cuántos años, pero muchos más de los que una lamparita de ahora puede. En otra escena mostraban un camino con dos camiones. Uno estaba atado al otro mediante un par de medias cancanes. Alguien se subía al primero y lo aceleraba. El resultado era que los dos camiones desaparecían de la escena, unidos por el invencible par de medias cancanes. Y al final de la clase el profesor nos iluminó con, posiblemente, la mejor frase que dijo durante todo el cursado, y quizás, su vida: telgopor es la forma corta de tela gomosa porosa. Desde ese día eliminé a telgopor de la lista de palabras que pronuncio rápido porque no estoy segura de cómo son. Eso. Hay lamparitas que no se queman y cancanes que no se rasgan.")
