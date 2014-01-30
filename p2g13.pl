%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%             CHECKERS                 %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                              %
% Realizado por:                                                               %
%       Oswaldo Jiménez 10-10368                                               %
%       Rebeca Machado 10-10406                                                %
%                                                                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------- DEFINICIONES -----------------------------------%

% Definición de jugadores
jugadores(jugador1).
jugadores(jugador2).


% Definición de una ficha "peon"
peon(X,Y) :-
    integer(X), integer(Y).

    
% Definición de una ficha "rey"
rey(X,Y) :-
    integer(X), integer(Y).

% Definiciones de hechos dinámicos
definirTieneFichaFunctor :-
    (dynamic tieneFicha/2).
    
definirComio :-
    (dynamic comio/0).
    
definirComer :-
    (dynamic comer/2).
    
%------------------------------------------------------------------------------%

% inicializarFichas
% 
% Inicializaciones
inicializarFichas :-
    definirTieneFichaFunctor,
    definirComio,
    definirComer,

    %% se definen las fichas en el tablero para el jugador 1
    assert(tieneFicha(jugador1,peon(2,1))),
    assert(tieneFicha(jugador1,peon(1,2))),
    assert(tieneFicha(jugador1,peon(2,3))),
    assert(tieneFicha(jugador1,peon(1,4))),
    assert(tieneFicha(jugador1,peon(2,5))),
    assert(tieneFicha(jugador1,peon(1,6))),
    assert(tieneFicha(jugador1,peon(2,7))),
    assert(tieneFicha(jugador1,peon(1,8))),
    assert(tieneFicha(jugador1,peon(3,2))),
    assert(tieneFicha(jugador1,peon(3,4))),
    assert(tieneFicha(jugador1,peon(3,6))),
    assert(tieneFicha(jugador1,peon(3,8))),

    assert(tieneFicha(jugador2,peon(6,1))),
    assert(tieneFicha(jugador2,peon(6,3))),
    assert(tieneFicha(jugador2,peon(6,5))),
    assert(tieneFicha(jugador2,peon(6,7))),
    assert(tieneFicha(jugador2,peon(7,2))),
    assert(tieneFicha(jugador2,peon(7,4))),
    assert(tieneFicha(jugador2,peon(7,6))),
    assert(tieneFicha(jugador2,peon(7,8))),
    assert(tieneFicha(jugador2,peon(8,1))),
    assert(tieneFicha(jugador2,peon(8,3))),
    assert(tieneFicha(jugador2,peon(8,5))),
    assert(tieneFicha(jugador2,peon(8,7))),

    assert(totalFichas(jugador1,12)),
    assert(totalFichas(jugador2,12)).

    
%-------------------------------- IMPRESIONES ---------------------------------%

% espacioLineaInicial
% 
% Procedimiento auxiliar, impresion de un elemnto mas una cantidad de espacios
% acorde a la dimension del tablero
espacioLineaInicial(Elem):-
    write(Elem),
    write('       ').
    

% printCasilla2
% 
% Impresion de una casilla. Revisa a que tipo de ficha corresponde la casilla
% ocupada. La impresion ademas depende del jugador poseedor de la ficha
printCasilla2(Fila,Columna):-
        
    write(' '),
    write('|  '),
    % caso en el que la ficha existe, es un peon, le pertenece al jugador 1
    (tieneFicha(jugador1,peon(Fila,Columna))->
        write('>'),
        write('  |'),!
    );
    % caso en el que la ficha existe, es un rey, le pertenece al jugador 1
    (tieneFicha(jugador1,rey(Fila,Columna))->
        write('>>'),
        write(' |'),!
    );
    % caso en el que la ficha existe, es un peon, le pertenece al jugador 2
     (tieneFicha(jugador2,peon(Fila,Columna))->
        write('<'),
        write('  |'),!
    );
    % caso en el que la ficha existe, es un rey, le pertenece al jugador 2
     (tieneFicha(jugador2,rey(Fila,Columna))->
        write('<<'),
        write(' |'),!
    );
    write('   |').
    

% printColumnas
% 
% procedimiento auxiliar de la funcion printTableroActual   
% for j=1..8 do 
printColumnas(Fila,Columna):-
   % if columna!=9
   (Columna=\=9  ->
        printCasilla2(Fila,Columna),
        ColumnaActual is Columna+1,
        printColumnas(Fila,ColumnaActual)
   );
   % if columna ==9 skip
   Columna=:=9.
   
   
% printFila
% 
% procedimiento auxiliar de la funcion printTableroActual    
% for i=1..8 do
%   for j=1..8 do
printFila(Fila):-
    
   % if fila!=9
    (Fila=\=9 ->
       write(Fila),
       printColumnas(Fila,1),nl,
       FilaActual is Fila+1,
       printFila(FilaActual)
    );
    
    % if fila ==9 skip
   Fila=:=9. 
    
    
% printTableroActual
% 
% Impresion del estado del tablero al momento de efectuar la llamada    
printTableroActual:-
    write('     '),
    % impresion los numeros de la parte superior del tablero
    % desde el 1 hasta el 8
    maplist(espacioLineaInicial,[1,2,3,4,5,6,7,8]),nl,
    % printFila implementa el recorrido de una matrz
    % a modo de for iterativo
    printFila(1).

%------------------------------------------------------------------------------%


%--------------------------------- PREDICADOS ---------------------------------%

% obtenerContrincante
% 
% Retorna true si JugadorContrincante es el adversario actual
% de Jugador
obtenerContrincante(Jugador,JugadorContrincante):-
    jugadores(Jugador),
    jugadores(JugadorContrincante),
    not(JugadorContrincante = Jugador).
  
  
  
% decrementarNumeroFichas
% 
% decrementa el numero de fichas del jugador pasado como parametro
% en una unidad
decrementarNumeroFichas(Jugador):-
    % se captura en la variable NumeroFichas
    % el numero de fichas actual del jugador 
    totalFichas(Jugador,NumeroFichas),
    FichasActuales is (NumeroFichas-1),
    % se actualiza el numero de fichas del jugador
    retract(totalFichas(Jugador,NumeroFichas)),
    assert(totalFichas(Jugador,FichasActuales)).

   
   
% eliminarFicha
% 
% True si se elimina la ficha encontrada en la casilla (X,Y), pertenenciente al 
% jugador pasado como parametro de entrada
eliminarFicha(Jugador,X,Y) :-
    ( (retract(tieneFicha(Jugador, peon(X,Y))), !);
      (retract(tieneFicha(Jugador,rey(X,Y)))) ),
    decrementarNumeroFichas(Jugador).
    

    
% cambiarPeonPorRey
% 
% True si se reemplaza la ficha peon ubicada en las coordenadas X,Y pasadas como
% parametro, por un rey.
cambiarPeonPorRey(Jugador,X,Y):-
    retract(tieneFicha(Jugador,peon(X,Y))),
    assert(tieneFicha(Jugador,rey(X,Y))),!.    
    
    
    
% reubicarFicha
% 
%True si la ficha correspondiente a un peon pertenece al jugador1. Se cambia
%dicho peon por un rey de llegar a una casilla en la fila 8  
reubicarFicha(jugador1,peon(X_viejo,Y_viejo),8,Y_nuevo):-    
    retract(tieneFicha(jugador1,peon(X_viejo,Y_viejo))),
    assert(tieneFicha(jugador1,peon(8,Y_nuevo))),
    cambiarPeonPorRey(jugador1,8,Y_nuevo),!.

%True si la ficha correspondiente a un peon pertenece al jugador2. Se cambia
%dicho peon por un rey de llegar a una casilla en la fila 1    
reubicarFicha(jugador2,peon(X_viejo,Y_viejo),1,Y_nuevo):-    
    retract(tieneFicha(jugador2,peon(X_viejo,Y_viejo))),
    assert(tieneFicha(jugador2,peon(1,Y_nuevo))),
    cambiarPeonPorRey(jugador2,1,Y_nuevo),!.
  
% reubica la ficha dada en la casilla correspondiente a las coordenadas 
% X_vieja,Y_vieja a la casilla destino denotada por X_nuevo, Y_nuevo
reubicarFicha(Jugador,FunctorViejo,X_nuevo,Y_nuevo):-
    % obtenemos una copia del functor pasado como parametro, garantizando
    % la preservacion del valor
    CopiaFunctorViejo = FunctorViejo,
    % se colocan en la variables FunctorRecuperado el functor
    % de la estructura pasada como parametro, (i.e. peon/2, rey/2)
    FunctorViejo =.. [FunctorRecuperado|_],
    % se reconstruye la estructura, empleando el functor de entrada
    % en conjunto con los nuevos atomos (coordenadas de destino)
    FunctorNuevo =.. [FunctorRecuperado,X_nuevo,Y_nuevo],
    % actualizacion de la base del conocimiento
    retract(tieneFicha(Jugador,CopiaFunctorViejo)),
    assert(tieneFicha(Jugador,FunctorNuevo)),!.
    
   
   
% movimientoHaciaAbajo
% 
% dadas dos coordenadas correspondientes a las filas (X)
% indica si el movimiento es hacia abajo
movimientoHaciaAbajo(X_viejo,X_nuevo):-     
    % para que el movimiento sea hacia abajo
    % forzosamente X_nuevo debe exceder a X_viejo
    (X_viejo<X_nuevo).
  
  
  
% movimientoHaciaArriba
% 
% dadas dos coordenadas correspondientes a las filas (X)
% indica si el movimiento es hacia arriba
movimientoHaciaArriba(X_viejo,X_nuevo):-     
    % para que el movimiento sea hacia arriba
    % forzosamente X_nuevo debe ser superior a X_viejo
    (X_viejo>X_nuevo).
    

    
% movimientoEsDiagonal
% 
% Acierta si el movimiento es diagonal
movimientoEsDiagonal(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    CoordenadaX is abs(X_viejo-X_nuevo),
    CoordenadaY is abs(Y_viejo-Y_nuevo),
    (CoordenadaX=:=CoordenadaY).
     
     
     
% casillaOcupada
% 
% indica si existe una ficha ocupando la casilla indicada
% por las coordenadas X,Y de entrada
casillaOcupada(X,Y):-
   ( (tieneFicha(_,peon(X,Y))),!; 
     (tieneFicha(_,rey(X,Y))),!
    ).
  
  
  
% coordenadasValidas
% 
% verifica que los valores de entrada correspondan a valores validos
% de acuerdo a las dimensiones del Tablero (8x8)
coordenadasValidas(X1,Y1,X2,Y2):-
    % las coordenadas son invalidas directamente si no son numeros enteros
    (
       ((integer(X1)),(integer(X2)),(integer(Y1)),(integer(Y2)))
    ),!,
    
    % evalua verdadero si alguna de las coordenadas es superior a 8
    % o inferior a 1
    (  
       ( ((X1=<8),(Y1=<8),(X2=<8),(Y2=<8)), 
         ((X1>=1),(Y1>=1),(X2>=1),(Y2>=1))  )
    ).

    
    
% noHayObstaculo
% 
%noHayObstaculo es un predicado que consta de 4 reglas. cada una tiene como
%proposito monitorear la direccion del rey. Retorna true si no existen
%casillas que obstaculicen el camino entre la casillad e origen y la casilla
%destino
   
%caso en el que el rey sube al noroeste
noHayObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    (X_viejo>X_nuevo),
    (Y_viejo>Y_nuevo),
    %write('noroeste'),
    caminoLibre(X_viejo,Y_viejo,X_nuevo,Y_nuevo,-,-),!.

%caso en el que el rey sube al noreste
noHayObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    (X_viejo>X_nuevo),
    (Y_viejo<Y_nuevo),
    %write('noreste'),
    caminoLibre(X_viejo,Y_viejo,X_nuevo,Y_nuevo,-,+),!.
    
%caso en el que el rey baja al sureste
noHayObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    (X_viejo<X_nuevo),
    (Y_viejo<Y_nuevo),
   % write('sureste'),
    caminoLibre(X_viejo,Y_viejo,X_nuevo,Y_nuevo,+,+),!.    
    
%caso en el que el rey baja al suroeste
noHayObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    (X_viejo<X_nuevo),
    (Y_viejo>Y_nuevo),
    %write('suroeste'),
    caminoLibre(X_viejo,Y_viejo,X_nuevo,Y_nuevo,+,-),!.

    
    
% caminoLibre
% 
%Se detiene en cuanto se hayan inspeccionado todas las fichas desde la casilla
%de origen hasta la de destino        
caminoLibre(X,Y,X,Y,_,_):-!. 

%El caso general de caminoLibre revisa que la casilla encontrada
%en (X_viejo OpX 1) (Y_viejo OpY 1) no este ocupada. OpX y OPy 
%pueden ser '+' o '-' dependiendo de la direccion de desplazamiento
%indicada por la regla noHayObstaculo correspondiente
caminoLibre(X_viejo,Y_viejo,X_nuevo,Y_nuevo,OpX,OpY):-
    K =.. [OpX,X_viejo,1],
    NewX is K,
    Z =.. [OpY,Y_viejo,1],
    NewY is Z,
    not(casillaOcupada(NewX,NewY)),
    caminoLibre(NewX,NewY,X_nuevo,Y_nuevo,OpX,OpY).
    
    

% obtenerIntermedias
% 
%dados dos pares de coordenadas X,Y correspondientes a una casilla de origen
%y a una de destino respectivamente, retorna true si X_intermedia
%y Y_intermedia corresponden a las coordenadas de la casilla ubicada
%entre las dos primeras.    
obtenerIntermedias(X_viejo,Y_viejo,X_nuevo,Y_nuevo,X_intermedia,Y_intermedia):-
    X_intermedia is ((X_viejo+X_nuevo)/2),
    Y_intermedia is ((Y_viejo+Y_nuevo)/2).

    

% puedeDesplazarse
% 
%True si el desplazamiento a computar es de exactamente 1 casilla,
%siendo ademas un peon quien la efectua, verificando que la casilla de destino
%no este ocupada
puedeDesplazarse(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo,1,1):-
    obtenerJugador(Jugador),
    not(casillaOcupada(X_nuevo,Y_nuevo)),
    reubicarFicha(Jugador,peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo),
    nl,!.

%True si el desplazamiento a computar es de exactamente 2 casillas,
%siendo ademas un peon quien la efectua, verificando que dicho peon
%cumpla con las condiciones impuestas por el predicado puedeComer.
puedeDesplazarse(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo,2,2):-
    obtenerJugador(Jugador),
    puedeComer(Jugador,X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    obtenerContrincante(Jugador,JugadorContrincante),
    obtenerIntermedias(X_viejo,Y_viejo,X_nuevo,Y_nuevo,X_intermedia,
                       Y_intermedia),
    eliminarFicha(JugadorContrincante,X_intermedia,Y_intermedia),
    assert(comio),
    reubicarFicha(Jugador,peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo),
    nl.
   
%True si el desplazamiento a computar es de exactamente 2 casillas,
%siendo ademas un rey quien la efectua, verificando que dicho rey   
%cumpla con las condiciones impuestas por el predicado puedeComer. 
puedeDesplazarse(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo,2,2):-
    obtenerJugador(Jugador),
    puedeComer(Jugador,X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    obtenerContrincante(Jugador,JugadorContrincante),
    obtenerIntermedias(X_viejo,Y_viejo,X_nuevo,Y_nuevo,X_intermedia,
                       Y_intermedia),
    eliminarFicha(JugadorContrincante,X_intermedia,Y_intermedia),
    assert(comio),
    reubicarFicha(Jugador,rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo),
    nl,!.

%True si el desplazamiento a computar es ejecutado por un rey,
%sin importar la cantidad de pasos que este desee moverse, siempre y cuando
%no exista un obstaculo entre la casilla de origen y la de destino  
puedeDesplazarse(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo,_,_):-
    noHayObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    obtenerJugador(Jugador),
    reubicarFicha(Jugador,rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo),!.

%True si el desplazamiento a computar es ejecutado por un rey,
%sin importar la cantidad de pasos que este desee moverse, siempre
%y cuando cumpla con las condiciones de comida de un rey y no haya
%comido previamente (El rey solo puede comer a distancia en el primer 
%movimiento de su turno
puedeDesplazarse(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo,_,_):-
    %el rey solo puede comer largo la primera jugada, esto es
    %en las jugadas consecutivas no podra comer largo
    not(comio),
    obtenerJugador(Jugador),
    puedeComer(Jugador,rey,X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2),
    obtenerContrincante(Jugador,JugadorContrincante),
    eliminarFicha(JugadorContrincante,XResult1,YResult2),
    assert(comio),
    reubicarFicha(Jugador,rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo).


    
% puedeComer
% 
% retorna true de existir coordenadas de destino que permitan a la ficha
% posicionada en X_viejo, Y_viejo saltar por encima de dicha ficha.
puedeComer(Jugador,X_viejo,Y_viejo,X_nuevo,Y_nuevo) :-

    %valores validos de instanciacion para las variables implicadas
    %en el backtracking
    member(X_nuevo,[1,2,3,4,5,6,7,8]), 
    member(Y_nuevo,[1,2,3,4,5,6,7,8]),
    X_intermedia is ((X_viejo+X_nuevo)/2),
    Y_intermedia is ((Y_viejo+Y_nuevo)/2),
    obtenerContrincante(Jugador,JugadorContrincante),
    %debe existir una  ficha contrincante, rey o peon, en la posicion
    %intermedia entre la casilla origen y la casilla destino.
    ((tieneFicha(JugadorContrincante,peon(X_intermedia,Y_intermedia)));
     (tieneFicha(JugadorContrincante,rey(X_intermedia,Y_intermedia)))
    ),
    %la casilla de destino no esta ocupada
    not(casillaOcupada(X_nuevo,Y_nuevo)).
    
%Se calculan las coordenadas del primer obstaculo. (estas no pueden coincidir
% con las coordenadas de desitno, por lo que forzosamente esta entre el rey
%y el destino). Se verifica que no haya algun otro obstaculo entre esta ficha
%y el destino. Solo asi, la comida sera posible.  
puedeComer(Jugador,rey,X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2):-
    not(casillaOcupada(X_nuevo,Y_nuevo)),
    obtenerPrimerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2),
    obtenerContrincante(Jugador,JugadorContrincante),
    ( (tieneFicha(JugadorContrincante,peon(XResult1,YResult2)));
      (tieneFicha(JugadorContrincante,rey(XResult1,YResult2)))
    ),
    noHayObstaculo(XResult1,YResult2,X_nuevo,Y_nuevo).
    
    
    
% puedeComerADistancia
% 
%True si la ficha correspondiente a un rey cumple con las condiciones
%de comida a distancia  
puedeComerADistancia(Jugador,X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    member(X_viejo,[1,2,3,4,5,6,7,8]), 
    member(Y_viejo,[1,2,3,4,5,6,7,8]),
    member(X_nuevo,[1,2,3,4,5,6,7,8]), 
    member(Y_nuevo,[1,2,3,4,5,6,7,8]),
    tieneFicha(Jugador,rey(X_viejo,Y_viejo)),
    puedeComer(Jugador,rey,X_viejo,Y_viejo,X_nuevo,Y_nuevo,_,_),!.
    
    
    
% puedeComerDirecto
% 
%True si X_viejo,Y_viejo,X_nuevo,Y_nuevo corresponden a coordendas
%validas de origen y de destino respectivamente, que cumplen con 
%la condiciones establecidas por el predicado puedeComer
puedeComerDirecto(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    member(X_viejo,[1,2,3,4,5,6,7,8]), 
    member(Y_viejo,[1,2,3,4,5,6,7,8]),
    puedeComer(X_viejo,Y_viejo,X_nuevo,Y_nuevo),!.
    
    
    
% obtenerPrimerObstaculo
% 
%metodologia de accion similar al predicado noHayObstaculo. Retorna
%true si XResult1 y YResult2 corresponde a la casilla donde se encuentra
%la primera ficha que bloquea el paso entre las coordenadas X_viejo,Y_viejo
%de origen y las coordenadas X_nuevo Y_nuevo de destino.
obtenerPrimerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2):-
    (X_viejo>X_nuevo),
    (Y_viejo>Y_nuevo),
    %write('noroeste'),
    primerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2,-,-),!.

obtenerPrimerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2):-
    (X_viejo>X_nuevo),
    (Y_viejo<Y_nuevo),
    %write('noreste'),
    primerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2,-,+),!.

obtenerPrimerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2):-
    (X_viejo<X_nuevo),
    (Y_viejo<Y_nuevo),
    %write('sureste'),
    primerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2,+,+),!.

obtenerPrimerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2):-
    (X_viejo<X_nuevo),
    (Y_viejo>Y_nuevo),
    %write('suroeste'),
    primerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2,+,-),!.

    
    
% primerObstaculo
% 
%Las llamadas a primerObstaculo garantizan que al menos hay un obstaculo
%intermedio, haciendo uso del predicado noHayObstaculo.
primerObstaculo(X,Y,X,Y,_,_,_,_):-fail.

%el caso general del predicado primerObstaculo encuentra los valores
%XResult1 YResult2 correspondientes a la primera ficha obstaculo buscada
primerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2,OpX,OpY):-
    K =.. [OpX,X_viejo,1],
    NewX is K,
    Z =.. [OpY,Y_viejo,1],
    NewY is Z,
    casillaOcupada(NewX,NewY),!,
    %la casilla siguiente debe coincidir con la casilla a donde se quiere saltar
    M =.. [OpX,NewX,1],
    N =.. [OpY,NewY,1],
    MyLast is M,
    NyLast is N,
    (X_nuevo = MyLast),
    (Y_nuevo = NyLast),
    (XResult1 = NewX),
    (YResult2 = NewY).

%Tercera regla del predicado primerObstaculo en el que aun no se encuentra la 
%casilla bloqueante. Gracias al cut ! sabemso que este caso asume
%not(casillaOcupada)
primerObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo,XResult1,YResult2,OpX,OpY):-
    K =.. [OpX,X_viejo,1],
    NewX is K,
    Z =.. [OpY,Y_viejo,1],
    NewY is Z,
    primerObstaculo(NewX,NewY,X_nuevo,Y_nuevo,XResult1,YResult2,OpX,OpY).
 
 
 
% movimientoValido
% 
%Caso en el que la ficha corresponde a un peon del jugador1. En esste caso
%un movimiento valido tentativo debe tener direccion hacia abajo    
movimientoValido(jugador1,peon,X_viejo,_,X_nuevo,_):-
    movimientoHaciaAbajo(X_viejo,X_nuevo).

%Caso en el que la ficha corresponde a un peon del jugador2. En este caso
%un movimiento valido tentativo debe tener direccion hacia arriba
movimientoValido(jugador2,peon,X_viejo,_,X_nuevo,_):-
    movimientoHaciaArriba(X_viejo,X_nuevo).
    
%Caso en el que la ficha corresponde a un rey, independientemente del jugador
%poseedor. En este caso un movimiento valido tentativo debe ser como minimo
%diagonal.    
movimientoValido(_,rey,X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    movimientoEsDiagonal(X_viejo,Y_viejo,X_nuevo,Y_nuevo).    
    
    
    
% calcularDiferencia
% 
%True si DiferenciaX, DiferenciaY contiene la diferencia entre
%los valores X-viejo,X_nuevo y Y_viejo,Y_nuevo respectivamente   
calcularDiferencia(X_viejo,Y_viejo,X_nuevo,Y_nuevo,DiferenciaX,DiferenciaY):-
    DiferenciaX is X_viejo-X_nuevo,
    DiferenciaY is Y_viejo-Y_nuevo.

    
    
% desplazar
% 
%True si la solicitud de desplazamiento es efectuada por un peon, siempre y cuando
%se cumplan con las condiciones impuestas por los predicados movimientoValido
%y puedeDesplazarse
desplazar(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo):-
    obtenerJugador(Jugador),
    movimientoValido(Jugador,peon,X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    calcularDiferencia(X_viejo,Y_viejo,X_nuevo,Y_nuevo,DiferenciaX,DiferenciaY),
    ModuloDiferenciaX is abs(DiferenciaX),
    ModuloDiferenciaY is abs(DiferenciaY),
    %write(ModuloDiferenciaX),
    %write(ModuloDiferenciaY).
    puedeDesplazarse(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo,ModuloDiferenciaX,
                          ModuloDiferenciaY).
   
%True si la solicitud de desplazamiento es efectuada por un rey, siempre y cuando
%se cumplan con las condiciones impuestas por los predicados movimientoValido
%y puedeDesplazarse    
desplazar(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo):-
    obtenerJugador(Jugador),
    movimientoValido(Jugador,rey,X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    calcularDiferencia(X_viejo,Y_viejo,X_nuevo,Y_nuevo,DiferenciaX,DiferenciaY),
    ModuloDiferenciaX is abs(DiferenciaX),
    ModuloDiferenciaY is abs(DiferenciaY),
    puedeDesplazarse(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo,ModuloDiferenciaX,
                     ModuloDiferenciaY).
                     


% obtenerJugada
% 
%Obtiene una jugada valida para la maquina, basada en aquellos valores
%dentro del rango del tablero permitido que correspondan a una jugada de
%procesamiento valido
obtenerJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    member(X_viejo,[1,2,3,4,5,6,7,8]), 
    member(Y_viejo,[1,2,3,4,5,6,7,8]),
    member(X_nuevo,[1,2,3,4,5,6,7,8]), 
    member(Y_nuevo,[1,2,3,4,5,6,7,8]),
    procesarJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo),!.
    

    
% procesarJugada
% 
%True si el jugador posee una ficha peon correspondiente a las coordenadas 
%de origen X_viejo,Y_viejo, siempre y cuando esta cumpla con las condiciones
%impuestas por el predicado desplazar
procesarJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    obtenerJugador(Jugador),
    tieneFicha(Jugador,peon(X_viejo,Y_viejo)),
    desplazar(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo).

%True si el jugador posee una ficha rey correspondiente a las coordenadas
%de origen X_viejo,Y_viejo, siempre y cuando esta cumpla con las condiciones
%impuestas por el predicado desplazar
procesarJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    obtenerJugador(Jugador),
    tieneFicha(Jugador,rey(X_viejo,Y_viejo)),
    desplazar(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo).
    
    
    
% turnoMaquina
% 
% Acierta si es el turno de la máquina
turnoMaquina:-
    obtenerJugador(Jugador),
    (Jugador=jugador2),
    (vsMaquina).
    


% procesarOpcion
% 
% Recibe la opción de jugar contra la máquina o no
procesarOpcion:-
    retract(jugador2),
    writeln('Desea jugar contra la maquina(s/n)?'),
    read(Opcion), not(var(Opcion)),
    (Opcion = s -> !;
    (Opcion = n -> retract(vsMaquina), !;
    (write('Opcion Invalida.'), nl,
    procesarOpcion))).

    
    
% imprimirTurno
% 
% Imprime el jugador al que le toca jugar
imprimirTurno :-
    jugador1 -> writeln('\nJuega jugador 1\n'),!;
    jugador2 -> writeln('\nJuega jugador 2\n').
 
 

% cambiarTurno
% 
% Cambia el turno del jugador
cambiarTurno :-
    member(J1,[jugador1,jugador2]), 
    member(J2,[jugador1,jugador2]),
    not(J1 = J2),
    retract(J1), assert(J2).
    
    
    
% obtenerJugador
% 
% Acierta si X es el jugador de turno actual
obtenerJugador(X) :-
    jugador1 -> X = jugador1,!;
    jugador2 -> X = jugador2.    
    
    
    
% ganador
% 
% Acierta si Jugador gana, cuando su contrincante no tiene más fichas en el
% tablero
ganador(Jugador) :-
   obtenerContrincante(Jugador,JugadorContrincante),
   totalFichas(JugadorContrincante,0).

   

% puedeComer
% 
% Esta serie de predicados aciertan cuando una determinada ficha tiene la
% posibilidad de comer, dependiendo de si es un peón o un rey.
puedeComer(X,Y,X1,Y1) :-
    obtenerJugador(Jugador),
    ((tieneFicha(Jugador, peon(X,Y)), puedeComer(peon(X,Y),X1,Y1), !);
    (tieneFicha(Jugador, rey(X,Y)), puedeComer(rey(X,Y),X1,Y1))).
    
% Caso de un peón del jugador1 (hacia abajo)
puedeComer(peon(X,Y),X1,Y1) :-
    jugador1, 
    X1 is X+2,
    puedeComer(jugador1,X,Y,X1,Y1),
    abs(Y1-Y) =:= 2.

% Caso de un peón del jugador2 (hacia arriba)
puedeComer(peon(X,Y),X1,Y1) :-
    jugador2, 
    X1 is X-2,
    puedeComer(jugador2,X,Y,X1,Y1),
    abs(Y1-Y) =:= 2.
    
% Caso de un rey, que puede comer en cualquier dirección
puedeComer(rey(X,Y),X1,Y1) :-
    obtenerJugador(Jugador),
    puedeComer(Jugador,X,Y,X1,Y1),
    abs(X1-X) =:= 2,
    abs(Y1-Y) =:= 2.

    

% verificarTranca
% 
% Acierta cuando el jugador de turno no puede realizar ninguna jugada, haciendo
% que su contrincante gane automáticamente. Depende de los siguientes predicados
verificarTranca :-
    obtenerJugador(Jugador),
    not(tieneJugada(_,_,_,_)),
    obtenerContrincante(Jugador, JC),
    write('\n¡Ha ganado '), write(JC), write('!\n\n'), 
    halt.
  
  
    
% tieneJugada
% 
% Este predicado unifica con posibles jugadas que tenga el jugador actual,
% utilizando todos los predicados siguientes (aquellos con sufijo '2').
% Funcionan igual que los predicados de movimiento de fichas pero sin realizar
% acciones de modificación de la base de datos (assert y retract) para no
% afectar el estado del juego actual.
tieneJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo) :-
    member(X_viejo,[1,2,3,4,5,6,7,8]), 
    member(Y_viejo,[1,2,3,4,5,6,7,8]),
    member(X_nuevo,[1,2,3,4,5,6,7,8]), 
    member(Y_nuevo,[1,2,3,4,5,6,7,8]),
    procesarJugada2(X_viejo,Y_viejo,X_nuevo,Y_nuevo).
    
% ver procesarJugada
procesarJugada2(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    obtenerJugador(Jugador),
    tieneFicha(Jugador,peon(X_viejo,Y_viejo)),
    %procesarPeon(Jugador,X_viejo,Y_viejo,X_nuevo,Y_nuevo),!.
    desplazar2(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo).
    
procesarJugada2(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    obtenerJugador(Jugador),
    tieneFicha(Jugador,rey(X_viejo,Y_viejo)),
    %procesarPeon(Jugador,X_viejo,Y_viejo,X_nuevo,Y_nuevo),!.
    desplazar2(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo).

% ver desplazar
desplazar2(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo) :-
    obtenerJugador(Jugador),
    movimientoValido(Jugador,peon,X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    calcularDiferencia(X_viejo,Y_viejo,X_nuevo,Y_nuevo,DiferenciaX,DiferenciaY),
    ModuloDiferenciaX is abs(DiferenciaX),
    ModuloDiferenciaY is abs(DiferenciaY),
    puedeDesplazarse2(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo,ModuloDiferenciaX,
                          ModuloDiferenciaY).
                          
desplazar2(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo) :-
    obtenerJugador(Jugador),
    movimientoValido(Jugador,rey,X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    calcularDiferencia(X_viejo,Y_viejo,X_nuevo,Y_nuevo,DiferenciaX,DiferenciaY),
    ModuloDiferenciaX is abs(DiferenciaX),
    ModuloDiferenciaY is abs(DiferenciaY),
    puedeDesplazarse2(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo,ModuloDiferenciaX,
                     ModuloDiferenciaY).
   
% ver puedeDesplazarse
puedeDesplazarse2(peon(_,_),X_nuevo,Y_nuevo,1,1):-
    not(casillaOcupada(X_nuevo,Y_nuevo)).
   
puedeDesplazarse2(peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo,2,2):-
    obtenerJugador(Jugador),
    puedeComer(Jugador,X_viejo,Y_viejo,X_nuevo,Y_nuevo).
    
puedeDesplazarse2(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo,2,2):-
    obtenerJugador(Jugador),
    puedeComer(Jugador,X_viejo,Y_viejo,X_nuevo,Y_nuevo).
   
puedeDesplazarse2(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo,_,_):-
    noHayObstaculo(X_viejo,Y_viejo,X_nuevo,Y_nuevo).
    
puedeDesplazarse2(rey(X_viejo,Y_viejo),X_nuevo,Y_nuevo,_,_):-
    %el rey solo puede comer largo la primera jugada, esto es
    %en las jugadas consecutivas no podra comer largo
    not(comio),
    not(casillaOcupada(X_nuevo,Y_nuevo)),
    obtenerJugador(Jugador),
    puedeComer(Jugador,rey,X_viejo,Y_viejo,X_nuevo,Y_nuevo,_,_).
   
%------------------------------------------------------------------------------%

    
%----------------------------- PREDICADOS PRINCIPALES -------------------------%
    

%True si las coordenadas de origen y destino son validas, y ademas
%esta configuracion de movimiento corresponde a una jugadanormal o a 
%una consecutiva
jugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo) :-
    coordenadasValidas(X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    ((jugadaConsecutiva(X_viejo,Y_viejo,X_nuevo,Y_nuevo), !);
    (jugadaNormal(X_viejo,Y_viejo,X_nuevo,Y_nuevo))).
  

  
% jugadaNormal
% 
% Una jugada normal unifica con coordenadas donde no se hace un salto
% consecutivo
jugadaNormal(X_viejo,Y_viejo,X_nuevo,Y_nuevo) :-
    not(comer(_,_)),
    procesarJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    printTableroActual,
    obtenerJugador(Jugador),
    ( (ganador(Jugador), write('\n¡Ha ganado '), write(Jugador), write('!\n\n'),
       halt); ! ),
    ( (comio, retract(comio), puedeComer(X_nuevo,Y_nuevo,_,_),
        assert(comer(X_nuevo,Y_nuevo)), !);
      (cambiarTurno, (verificarTranca; !)) ),
    imprimirTurno,
    %caso en el que el turno es de la maquina, se computa la primera jugada
    %valida
    ( (turnoMaquina,jugadaMaquina);!),!.
    
    
    
% jugadaConsecutiva
% 
% Una jugada consecutiva unifica con coordenadas donde la ficha a moverse acaba
% de realizar un salto y puede realizar otro de forma consecutiva
jugadaConsecutiva(X_viejo,Y_viejo,X_nuevo,Y_nuevo) :-
    comer(X_viejo,Y_viejo),
    retract(comer(X_viejo,Y_viejo)),
    puedeComer(X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    procesarJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    printTableroActual,
    obtenerJugador(Jugador),
    ( (ganador(Jugador), write('\n¡Ha ganado '), write(Jugador), write('!\n\n'),
       halt); ! ),
    ( (comio, retract(comio), puedeComer(X_nuevo,Y_nuevo,_,_),
        assert(comer(X_nuevo,Y_nuevo)), !);
      (cambiarTurno, (verificarTranca; !)) ),
    imprimirTurno,
    %caso en el que el turno es de la maquina, se computa la primera jugada
    %valida
    ( (turnoMaquina,jugadaMaquina);!),!.



% jugadaMaquina
% 
% Acierta cuando se realiza una jugada normal de la máquina
jugadaMaquina:-
    jugadaNormalMaquina.

    

% jugadaNormalMaquina
% 
% Unifica con una jugada normal de la máquina. ver jugadaNormal
jugadaNormalMaquina:-
    obtenerJugador(Jugador),
    %la maquina primero revisa si puede comer.
    (
      ((puedeComerDirecto(X_viejo,Y_viejo,X_nuevo,Y_nuevo),
        procesarJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo)),!);
      
       (obtenerJugada(_,_,X_nuevo,Y_nuevo),!) ),
    
    printTableroActual,
    ( (ganador(Jugador), write('\n¡Ha ganado '), write(Jugador), write('!\n\n'),
       halt); ! ),
    ( (comio, retract(comio), puedeComer(X_nuevo,Y_nuevo,_,_),
        assert(comer(X_nuevo,Y_nuevo)), !);
      (cambiarTurno, !) ),
    imprimirTurno,
    ((comer(_,_),jugadaConsecutivaMaquina);!).
    
    
    
% jugadaConsecutivaMaquina
% 
% Unifica con una jugada consecutiva de la máquina. ver jugadaConsecutiva
jugadaConsecutivaMaquina:- 
    %se obtienen los valores de origen de la maquina   X_viejo Y_viejo
    comer(X_viejo,Y_viejo),
    retract(comer(X_viejo,Y_viejo)),
    %se obtienen los valores de destino de la maquina X_nuevo Y_nuevo
    puedeComer(X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    %se procesa la jugada con todos los valores calculados
    procesarJugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo),
    printTableroActual,
    obtenerJugador(Jugador),
    ( (ganador(Jugador), write('\n¡Ha ganado '), write(Jugador), write('!\n\n'),
       halt); ! ),
    ( (comio, retract(comio), puedeComer(X_nuevo,Y_nuevo,_,_),
        assert(comer(X_nuevo,Y_nuevo)), !);
      (cambiarTurno, !) ),
    imprimirTurno,
    ((comer(_,_),jugadaConsecutivaMaquina);!).
    
    
    
% Predicado principal
jugar :-
    assert(jugador1),
    assert(jugador2),
    assert(vsMaquina),
    procesarOpcion,
    inicializarFichas,
    writeln('Comenzo el juego\n'),
    printTableroActual,
    imprimirTurno.
    
%------------------------------------------------------------------------------%