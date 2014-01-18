para probar el programa:

swipl
['proyecto2.pl'].
jugar.
s.
jugada(VALOR1,VALOR2,VALOR3,VALOR4).


no cambia en nada si se coloca s. o n. cuando se pregunta si se desea
jugar contra la maquina.

el comando 
printTableroActual. 
imprime el estado actual del tablero.

Cosas que se pueden hacer hasta ahora:
mover fichas del jugador 2 hacia abajo, 1 y 2 casillas.
enviar mensaje de jugada invalida para numeros mayores a 9 y menores a 1,
para constantes (k,j,jhon...)

para mover 2 casillas es necesario comer. el jugador 2 no esta implementando
aun. para probar comer, se debe mover una ficha del jugador1 hasta una posicion
en la que amenace a otra ficha del jugador 2, borrar manualmente la ficha que bloquea
el movimiento usando el comando retract(tieneFicha(jugador2,peon(coordX,coordY))).
y seguidamente hacer la jugada de comida con la ficha. 

mostrar numero de fichas de cada jugador: totalFichas(jugador1,X).
totalFichas(jugador2,X).
