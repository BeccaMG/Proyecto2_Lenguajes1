estupefacto(andres).
deidad(fernando).
deidad(arturo).
deidad(juan).


tableroValido([[],[],[],[],[],[],[],[]]).
tableroValido([[_|RestRow1],[_|RestRow2],[_|RestRow3],[_|RestRow4],
[_|RestRow5],[_|RestRow6],[_|RestRow7],[_|RestRow8]]):-
    write(RestRow6),
    tableroValido([RestRow1,RestRow2,RestRow3,RestRow4,RestRow5,RestRow6,RestRow7,
RestRow8]).
    
printRow([]).    
printRow([Row|RestRow]):-
    write(Row),
    write(' '),
    printRow(RestRow).   

printTablero([]).    
printTablero([Row|resto]):-
    write(Row),nl,
    write(resto).
% printTablero([Row|restoTablero]):-
%    printRow(Row),
%    printTablero(restoTablero).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%             INTENTONA                 %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% printTablero([[1,5,8,9,3,6,9,2],[6,7,8,6,5,4,6,7],[8,8,7,6,4,5,9,3],[7,6,4,6,7,9,2,8],[6,6,5,4,5,6,7,8],[9,6,5,4,5,2,3,6],[6,6,5,6,6,7,8,6],[7,6,7,6,7,6,7,8]],0).

% se utiliza 0 a modo de bandera por la condicion del maplist/3, que fuerza
% dos listas a tener el mismo tamanio

% agregarFicha(0,_).
  agregarFicha(Fila,Columna):-
    assert(peon(Fila,Columna)).


agregarFichasColumnas_2_4_6_8(Fila):-
    
    % Filas del jugador 1
    agregarFicha(Fila,2),
    agregarFicha(Fila,4),
    agregarFicha(Fila,6),
    agregarFicha(Fila,8).

agregarFichasColumnas_1_3_5_7(Fila):-
    % Filas del jugador 1
    agregarFicha(Fila,1),
    agregarFicha(Fila,3),
    agregarFicha(Fila,5),
    agregarFicha(Fila,7).

definirPeonFunctor:-
    % se define dinamicamente el functor peon, con 2 parmetros de entrada
    dynamic peon/2. 

definirReyFunctor:-
    % se define dinamicamente el functor rey, con 2 parmetros de entrada
    dynamic rey/2.

definirTieneFichaFunctor:-
    % se define dinamicamente el functor de fichas de jugador, con 1 parametro de entrada
    (dynamic tieneFicha/2).
intentona:-
    % en las filas 1,3,6 & 8 se agregran fichas
    % en las columnas 2, 4 6 & 8
    maplist(agregarFicha,[1,3,6,8],[2,4,6,8]),
    
    % en las filas 2 & 7 se agregran fichas
    % en las columnas 1,3 5 & 7
    maplist(agregarFicha,[2,7,0,0],[1,3,5,7]).
inicializarFichas:-
    definirPeonFunctor,
    definirReyFunctor,
    definirTieneFichaFunctor,
    % inicializando fichas del jugador uno 

    agregarFichasColumnas_2_4_6_8(1),
    agregarFichasColumnas_2_4_6_8(3),
    agregarFichasColumnas_2_4_6_8(7),
    agregarFichasColumnas_1_3_5_7(2),
    agregarFichasColumnas_1_3_5_7(6),
    agregarFichasColumnas_1_3_5_7(8),


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





%%%% Procedimiento auxiliar, impresion de una casilla %%%%%%%%%%%%%

printCasilla(Elem):-
    write(' '),
    write('|  '),
    (Elem=:=6 ->
      write('>'),
      write('  |')
    ,!);
    % gracias al cut !, este write solo se evalua si 
    % Elem =\= 6 
    write('   |').

%%%%%% Procedimiento auxiliar, impresion de un elemnto mas espacio %%%%%%
espacioLineaInicial(Elem):-
    write(Elem),
    write('       ').


%%%%%%% Primera aproximacion Impresion de Tablero %%%%%%%%%%%%
printTablero([],_).
printTablero([Row|RestRow],RowNum):-
    (RowNum=:=0 ->
        write('      '),
        maplist(espacioLineaInicial,[1,2,3,4,5,6,7,8]),nl,
        RowNum1 is RowNum+1,
        printTablero(RestRow, RowNum1)
    ),!;
    (RowNum<9 ->
        write(RowNum),write(' '),
        RowNum1 is RowNum+1,
        maplist(printCasilla,Row),nl,
        printTablero(RestRow,RowNum1)
    ).
    
printCasilla2(Fila,Columna):-
        
    write(' '),
    write('|  '),
    % caso en el que la ficha existe, es un peon, le pertenece al jugador 1
    (tieneFicha(jugador1,peon(Fila,Columna))->
        write('<'),
        write('  |'),!
    );
    % caso en el que la ficha existe, es un rey, le pertenece al jugador 1
    (tieneFicha(jugador1,rey(Fila,Columna))->
        write('<<'),
        write(' |'),!
    );
    % caso en el que la ficha existe, es un peon, le pertenece al jugador 2
     (tieneFicha(jugador2,peon(Fila,Columna))->
        write('>'),
        write('  |'),!
    );
    % caso en el que la ficha existe, es un rey, le pertenece al jugador 2
     (tieneFicha(jugador2,rey(Fila,Columna))->
        write('>>'),
        write(' |'),!
    );
    write('   |').
    

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
    
%%% Impresion del estado del tablero al momento de efectuar la llamada    
printTableroActual:-
    write('     '),
    % impresion los numeros de la parte superior del tablero
    % desde el 1 hasta el 8
    maplist(espacioLineaInicial,[1,2,3,4,5,6,7,8]),nl,
    % printFila implementa el recorrido de una matrz
    % a modo de for
    printFila(1).
     

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

% elimina la ficha encontrada en la casilla (X,Y), pertenenciente al jugador
% pasado como parametro de entrada
eliminarFicha(Jugador,X,Y):-
    retract(tieneFicha(Jugador,peon(X,Y))),
    decrementarNumeroFichas(Jugador).
    
% reemplaza la ficha peon ubicada en las coordenadas X,Y pasadas como
% parametro, por un rey.
cambiarPeonPorRey(Jugador,X,Y):-
    retract(tieneFicha(Jugador,peon(X,Y))),
    assert(tieneFicha(Jugador,rey(X,Y))),!.
    
% reubica la ficha dada en la casilla correspondiente a las coordenadas 
% X_vieja,Y_vieja a la casilla destino denotada por X_nueva, Y_nueva
reubicarFicha(Jugador,FunctorViejo,X_nueva,Y_nueva):-

    % obtenemos una copia del functor pasado como parametro, garantizando
    % la preservacion del valor
    CopiaFunctorViejo = FunctorViejo,
    
    % se colocan en la variables FunctorRecuperado el functor
    % de la estructura pasada como parametro, (i.e. peon/2, rey/2)
    FunctorViejo =.. [FunctorRecuperado|_],
    
    % se reconstruye la estructura, empleando el functor de entrada
    % en conjunto con los nuevos atomos (coordenadas de destino)
    FunctorNuevo =.. [FunctorRecuperado,X_nueva,Y_nueva],
    
    % actualizacion de la base del conocimiento
    retract(tieneFicha(Jugador,CopiaFunctorViejo)),
    assert(tieneFicha(Jugador,FunctorNuevo)),!.
    
    
    
% dadas dos coordenadas correspondientes a las filas (X)
% indica si el movimiento es hacia abajo
movimientoHaciaAbajo(X_viejo,X_nuevo):-     
    % para que el movimiento sea hacia abajo
    % forzosamente X_nuevo debe exceder a X_viejo
    (X_viejo<X_nuevo).
     
% dadas dos coordenadas correspondientes a las filas (X)
% indica si el movimiento es hacia arriba
movimientoHaciaArriba(X_viejo,X_nuevo):-     
    % para que el movimiento sea hacia arriba
    % forzosamente X_nuevo debe ser superior a X_viejo
    (X_viejo>X_nuevo).     
     
% indica si existe una ficha ocupando la casilla indicada
% por las coordenadas X,Y de entrada
casillaOcupada(X,Y):-
   ( (tieneFicha(_,peon(X,Y))),!; 
     (tieneFicha(_,rey(X,Y))),!
    ).
     
% verifica que los valores de entrada correspondan a valores validos
% de acuerdo a las dimensiones del Tablero (8x8)
coordenadasInvalidas(X1,Y1,X2,Y2):-
    % las coordenadas son invalidas directamente si no son numeros enteros
    (
       (not(integer(X1));not(integer(X2));not(integer(Y1));not(integer(Y2))),!
    );
    
    % evalua verdadero si alguna de las coordenadas es superior a 8
    % o inferior a 1
    (  
       ( ((X1>8);(Y1>8);(X2>8);(Y2>8)),! ; 
         ((X1<1),!;(Y1<1),!;(X2<1),!;(Y2<1))  )
    ).

jugada(X_viejo,Y_viejo,X_nuevo,Y_nuevo):-
    write('Movimiento jugador 1'),nl,
    % se verifica que los valores insertados correspondan a valores validos
    % de acuerdo a las dimensiones del Tablero (8x8)
    (  
       (coordenadasInvalidas(X_viejo,Y_viejo,X_nuevo,Y_nuevo)) ->
            % el movimiento es invalido si alguna coordenada es superior a 8
            % o menor que 1
            write('Movimiento Invalido'),!
    );
    (  
       % caso en el que la ficha existe, es un peon, le pertenece al jugador 1
       % si es peon, le pertenece a jugador 1 el movimiento debe ser obligatoria
       % mente hacia abajo
       ( (tieneFicha(jugador1,peon(X_viejo,Y_viejo)),
          movimientoHaciaAbajo(X_viejo,X_nuevo))->
            DiferenciaX is X_viejo-X_nuevo,
            DiferenciaY is Y_viejo-Y_nuevo,
            ModuloDiferenciaX is abs(DiferenciaX),
            ModuloDiferenciaY is abs(DiferenciaY),
            write(ModuloDiferenciaX),
            write(ModuloDiferenciaY),
            % caso en el que el valor absoluto de las diferencias en ambas
            % coordenadas es 1. Es el caso en el que el jugador solicito
            % efectuar un movimiento de distancia 1. Solo es posible
            % si la casilla de destino corresponde a una casilla vacia, dentro
            % de los limites del tablero.
            (
                ( ((ModuloDiferenciaX=:=1),(ModuloDiferenciaY=:=1)) ->
                  (
                      % se verifica que la casilla a la que se va a saltar no este
                      % ocupada por alguna otra ficha                
                      (  
                         (casillaOcupada(X_nuevo,Y_nuevo)) ->
                             
                              % si la casilla esta ocupada por alguna otra ficha
                              % el movimiento es declarado invalido
                              write('casilla ocupada'),
                              write('Movimiento Invalido'),!
                      );
                      % else caso en el que la casilla de destino no esta vacia
                      % se reubica satisfactoriamente la ficha hacia su destino
                      reubicarFicha(jugador1,peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo),
                      write('movimiento de 1 paso'),!
                   ) % fin del if ((ModuloDiferenciaX=:=1),(ModuloDiferenciaY=:=1)) 
                );
                % caso en el que el valor absoluto de las diferencias en ambas
                % coordenadas es 2. Es el caso en el que el jugador solicito
                % efectuar un movimiento de distancia 2. Solo es posible
                % si el jugador saltara sobre una ficha adversaria, siendo la casilla
                % de destino una casilla vacia, dentro de los limites del tablero.
                ( ((ModuloDiferenciaX=:=2),(ModuloDiferenciaY=:=2)) ->
                  ( 
                      
                      % X_intermedia & Y_intermedia corresponden a las coordenadas
                      % de la casilla corresponde a aquella intermedia entre
                      % la casilla de movimiento de origen & la de destino
                      X_intermedia is ((X_viejo+X_nuevo)/2),
                      Y_intermedia is ((Y_viejo+Y_nuevo)/2),
                      % caso en el que existe una ficha sobre la cual
                      % se puede saltar
                      ( (tieneFicha(jugador2,peon(X_intermedia,Y_intermedia))) ->
                        (
                              % se verifica que la casilla a la que se va a saltar no este
                              % ocupada por alguna otra ficha  
                              (  
                                 (casillaOcupada(X_nuevo,Y_nuevo)) ->
                                     
                                      % si la casilla esta ocupada por alguna otra ficha
                                      % el movimiento es declarado invalido
                                      write('casilla ocupada'),
                                      write('Movimiento Invalido'),!
                              );
                              % en este punto, la casilla destino esta libre 
                              % ademas existe una ficha intermedia enemiga sobre
                              % la cual saltar
                              
                              % se borra la ficha adversaria
                              eliminarFicha(jugador2,X_intermedia,Y_intermedia),
                              
                              % se reubica satisfactoriamente la ficha hacia su destino
                              reubicarFicha(jugador1,peon(X_viejo,Y_viejo),X_nuevo,Y_nuevo),
                              write('movimiento de 2 pasos'),!
                         )
                      ); % fin del if tieneFicha(jugador2,X_intermedia,Y_intermedia)
                      % caso en el que no hay ficha sobre la cual saltar
                      write(' no hay por donde saltar'),
                      write('Movimiento Invalido'),!
                      
                  )
                ) % fin del if ((ModuloDiferenciaX=:=1),(ModuloDiferenciaY=:=1)) 
            );
            write('no pase'),!
        );
        % caso en el que la ficha existe, es un rey, le pertenece al jugador 1
        (tieneFicha(jugador1,rey(X_viejo,Y_viejo)) ->
                retract(tieneFicha(jugador1,rey(X_viejo,Y_viejo))),
                assert(tieneFicha(jugador1,rey(X_nuevo,Y_nuevo))),!
        )
    );
    write('sali'),
    write('Movimiento Invalido'),nl.

doA :- writeln('DoA').
doB :- writeln('DoB').
doC :- writeln('DoC').

procesarOpcion:-
    writeln('Desea jugar contra la maquina(s/n)?'),
    read(Opcion),
    (Opcion = s -> doA,!;
    (Opcion = n -> doB,!;
    (write('Opcion Invalida.'),nl,
    procesarOpcion))).


% funcion principal
jugar:-
    procesarOpcion,
    inicializarFichas,
    writeln('Comenzo el juego'),
    printTableroActual.

