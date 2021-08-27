
%alumno(nombre, sangre, caracteristicas, odiariaEstarEn)
mago(harry, mestiza, [coraje, amistoso, orgullo, inteligencia], slytherin).
mago(draco, pura, [inteligencia, orgullo], hufflepuff).
mago(hermione, impura, [inteligencia, orgullo, responsabilidad], ninguna).
mago(ron, mestiza, [responsabilidad, coraje], slytherin).

caracteristicaParaElegirCasa(gryffindor, coraje).
caracteristicaParaElegirCasa(slytherin, orgullo).
caracteristicaParaElegirCasa(slytherin, inteligencia).
caracteristicaParaElegirCasa(ravenclaw, inteligencia).
caracteristicaParaElegirCasa(ravenclaw, responsabilidad).
caracteristicaParaElegirCasa(hufflepuff, amistoso).

casa(Casa) :-
    caracteristicaParaElegirCasa(Casa, _).

% --------------- PUNTO 1 ---------------

permiteEntrar(Casa, Mago) :-
    casa(Casa),
    mago(Mago, _, _, _),
    Casa \= slytherin.

permiteEntrar(slytherin, Mago) :-
    mago(Mago, Sangre, _, _),
    Sangre \= impura.

% --------------- PUNTO 2 ---------------

tieneCaracterApropiado(Casa, Mago) :-
    casa(Casa),
    mago(Mago, _, CaracteristicasMago, _),
    forall(caracteristicaParaElegirCasa(Casa, UnaCaracteristica), member(UnaCaracteristica, CaracteristicasMago)).

% --------------- PUNTO 3 --------------- 

casaAdecuada(gryffindor, hermione).

casaAdecuada(Casa, Mago) :-
    tieneCaracterApropiado(Casa, Mago),
    permiteEntrar(Casa, Mago),
    not(odiariaEstarEn(Mago, Casa)).

odiariaEstarEn(Mago, UnaCasa) :-
    mago(Mago, _, _, UnaCasa).

% --------------- PUNTO 4 --------------- 

cadenaDeAmistades(Magos) :-
    todosSonAmistosos(Magos),
    todosEstanEnLaMismaCasa(Magos).

todosSonAmistosos(Magos) :-
    forall(member(Mago, Magos), esAmistoso(Mago)).

esAmistoso(Mago) :-
    mago(Mago, _, CaracteristicasMago, _),
    member(amistoso, CaracteristicasMago).

todosEstanEnLaMismaCasa(Magos) :-
    forall(dosMagosDistintos(Mago, OtroMago, Magos), compartenCasa(Mago, OtroMago)).

dosMagosDistintos(UnMago, OtroMago, Magos) :-
    member(UnMago, Magos), 
    member(OtroMago, Magos), 
    UnMago \= OtroMago.

compartenCasa(UnMago, OtroMago) :-
    casaAdecuada(Casa, UnMago),
    casaAdecuada(Casa, OtroMago),
    UnMago \= OtroMago.

% --------------- PARTE 2 --------------- 

accion(harry, andarFueraDeLaCama).
accion(hermione, tercerPiso).
accion(hermione, seccionRestringidaDeBiblioteca).
accion(harry, bosque).
accion(harry, tercerPiso).
accion(draco, mazmorras).
accion(ron, ganarAjedrez).
accion(hermione, salvarAmigos).
accion(harry, ganarleAVoldemort).
accion(luna, salvarAmigos).

accion(hermione, pregunta(dondeSeEncuentraUnBezoar, snape)).
accion(hermione, pregunta(comoHacerLevitarUnaPluma, flitwick)).
accion(luna, pregunta(comoHacerLevitarUnaPluma, flitwick)).

lugarProhibido(tercerPiso, 75).
lugarProhibido(seccionRestringidaDeBiblioteca, 10).
lugarProhibido(bosque, 50).

malaAccion(andarFueraDeLaCama, 50).
malaAccion(Accion, Puntaje) :-
    lugarProhibido(Accion, Puntaje).

buenaAccion(ganarAjedrez, 50).
buenaAccion(salvarAmigos, 50).
buenaAccion(ganarleAVoldemort, 60).

buenaAccion(pregunta(Pregunta, Profesor), PuntajeDefinitivo) :-
    puntosPorPregunta(Pregunta, Puntaje),
    calcularPuntaje(Profesor, Puntaje, PuntajeDefinitivo).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

% --------------- PUNTO 1 A --------------- 

buenAlumno(Mago) :-
    accion(Mago, _),
    forall(accion(Mago, Accion), not(malaAccion(Accion, _))).

% --------------- PUNTO 1 B --------------- 

accionRecurrente(Accion) :-
    accion(UnMago, Accion),
    accion(OtroMago, Accion),
    UnMago \= OtroMago.

% --------------- PUNTO 2 --------------- 

puntajeTotal(Casa, PuntajeTotal) :-
    casa(Casa),
    findall(Puntaje, puntajePorMago(_, Casa, Puntaje), Puntajes),
    sum_list(Puntajes, PuntajeTotal).

puntajePorMago(Mago, Casa, Puntaje) :-
    esDe(Mago, Casa),
    accion(Mago, Accion),
    puntajePorAccion(Accion, Puntaje).

puntajePorAccion(Accion, Puntaje) :-
    buenaAccion(Accion, Puntaje).

puntajePorAccion(Accion, Puntaje) :-
    malaAccion(Accion, UnPuntaje),
    Puntaje is UnPuntaje * (-1).

% --------------- PUNTO 3 --------------- 

casaGanadora(UnaCasa) :-
    casa(UnaCasa),
    forall(puntajeDeDosCasas(UnaCasa, _, PuntajeCasa1, PuntajeCasa2), PuntajeCasa1 > PuntajeCasa2).

puntajeDeDosCasas(UnaCasa, OtraCasa, PuntajeCasa1, PuntajeCasa2) :-
    casa(OtraCasa),
    puntajeTotal(UnaCasa, PuntajeCasa1),
    puntajeTotal(OtraCasa, PuntajeCasa2),
    UnaCasa \= OtraCasa.

% --------------- PUNTO 4 --------------- 

calcularPuntaje(Profesor, Puntaje, Puntaje) :-
    Profesor \= snape.

calcularPuntaje(snape, Puntaje, PuntajeDefinitivo) :-
    PuntajeDefinitivo is Puntaje / 2.

puntosPorPregunta(dondeSeEncuentraUnBezoar, 20).
puntosPorPregunta(comoHacerLevitarUnaPluma, 25).