% Mi Cursada Universitaria

% Base de conocimiento Materias
% materia(nombre, cantidadHorasSemanales).
materia(analisis_Matematico_I,5).
materia(algebra_y_Geometria_Analitica,5).
materia(matematica_Discreta,3).
materia(sistemas_y_Organizaciones,3).
materia(algoritmo_y_Estructuras_de_Datos,5).
materia(arquitectura_de_Computadoras,4).
materia(ingenieria_y_Sociedad,2).
materia(quimica,3).
materia(fisica_I,5).
materia(analisis_Matematico_II,5).
materia(probabilidad_y_Estadistica,3).
materia(analisis_de_Sistemas,6).
materia(sintaxis_y_Semantica_de_los_Lenguajes,4).
materia(paradigmas_de_Programacion,4).
materia(sistemas_de_Representacion,3).
materia(sistemas_Operativos,4).
materia(disenio_de_Sistemas,6).
materia(ingles_I,2).
materia(fisica_II,5).
materia(matematica_Superior,4).
materia(gestion_de_Datos,4).
materia(legislacion,2).
materia(economia,3).
materia(ingles_II,2).
materia(redes_de_Informacion,4).
materia(administracion_de_Recursos,6).
materia(investigacion_Operativa,5).
materia(simulacion,4).
materia(ingenieria_de_Software,3).
materia(teoria_de_Control,3).
materia(comunicaciones,4).
materia(simulacion,4).
materia(proyecto_Final,6).
materia(inteligencia_Artificial,3).


% esIntegradora(materia).
esIntegradora(sistemas_y_Organizaciones).
esIntegradora(analisis_de_Sistemas).
esIntegradora(disenio_de_Sistemas).
esIntegradora(administracion_de_Recursos).
esIntegradora(proyecto_Final).


% esCorrelativaDe(materia,materiaCorrelativa).
esCorrelativaDe(analisis_de_Sistemas,sistemas_y_Organizaciones).
esCorrelativaDe(analisis_de_Sistemas,algoritmos_y_Estructuras_de_Datos).
esCorrelativaDe(analisis_Matematico_II,analisis_Matematico_I).
esCorrelativaDe(analisis_Matematico_II,algebra_y_Geometria_Analitica).
esCorrelativaDe(sintaxis_y_Semantica_de_los_Lenguajes,matematica_Discreta).
esCorrelativaDe(sintaxis_y_Semantica_de_los_Lenguajes,algoritmos_y_Estructuras_de_Datos).
esCorrelativaDe(paradigmas_de_Programacion,matematica_Discreta).
esCorrelativaDe(paradigmas_de_Programacion,algoritmos_y_Estructuras_de_Datos).
esCorrelativaDe(probabilidad_y_Estadistica,analisis_Matematico_I).
esCorrelativaDe(probabilidad_y_Estadistica,algebra_y_Geometria_Analitica).
esCorrelativaDe(disenio_de_Sistemas,analisis_de_Sistemas).
esCorrelativaDe(disenio_de_Sistemas,paradigmas_de_Programacion).
esCorrelativaDe(sistemas_Operativos,matematica_Discreta).
esCorrelativaDe(sistemas_Operativos,algoritmos_y_Estructuras_de_Datos).
esCorrelativaDe(sistemas_Operativos,arquitectura_de_Computadoras).
esCorrelativaDe(fisica_II,analisis_Matematico_I).
esCorrelativaDe(fisica_II,fisica_I).
esCorrelativaDe(economia,analisis_de_Sistemas).
esCorrelativaDe(gestion_de_Datos,analisis_de_Sistemas).
esCorrelativaDe(gestion_de_Datos,paradigmas_de_Programacion).
esCorrelativaDe(gestion_de_Datos,sintaxis_y_Semantica_de_los_Lenguajes).
esCorrelativaDe(ingles_II,ingles_I).
esCorrelativaDe(matematica_Superior,analisis_Matematico_II).
esCorrelativaDe(legislacion,analisis_de_Sistemas).
esCorrelativaDe(legislacion,ingenieria_y_Sociedad).
esCorrelativaDe(administracion_de_Recursos,disenio_de_Sistemas).
esCorrelativaDe(administracion_de_Recursos,sistemas_Operativos).
esCorrelativaDe(administracion_de_Recursos,economia).
esCorrelativaDe(ingenieria_de_Software,probabilidad_y_Estadistica).
esCorrelativaDe(ingenieria_de_Software,disenio_de_Sistemas).
esCorrelativaDe(ingenieria_de_Software,gestion_de_Datos).
esCorrelativaDe(teoria_de_Control,quimica).
esCorrelativaDe(teoria_de_Control,matematica_Superior).
esCorrelativaDe(comunicaciones,arquitectura_de_Computadoras).
esCorrelativaDe(comunicaciones,analisis_Matematico_II).
esCorrelativaDe(comunicaciones,fisica_II).
esCorrelativaDe(redes_de_Informacion,sistemas_Operativos).
esCorrelativaDe(redes_de_Informacion,comunicaciones).
esCorrelativaDe(investigacion_Operativa,probabilidad_y_Estadistica).
esCorrelativaDe(investigacion_Operativa,matematica_Superior).
esCorrelativaDe(simulacion,probabilidad_y_Estadistica).
esCorrelativaDe(simulacion,matematica_Superior).
esCorrelativaDe(inteligencia_Artificial,investigacion_Operativa).
esCorrelativaDe(inteligencia_Artificial,simulacion).
esCorrelativaDe(administracion_Gerencial,administracion_de_Recursos).
esCorrelativaDe(administracion_Gerencial,investigacion_Operativa).
esCorrelativaDe(sistemas_de_Gestion,administracion_de_Recursos).
esCorrelativaDe(sistemas_de_Gestion,investigacion_Operativa).
esCorrelativaDe(sistemas_de_Gestion,simulacion).
esCorrelativaDe(proyecto_Final,legislacion).
esCorrelativaDe(proyecto_Final,administracion_de_Recursos).
esCorrelativaDe(proyecto_Final,redes_de_Informacion).
esCorrelativaDe(proyecto_Final,ingenieria_de_Software).


% Base de conocimientos
% Para estudiante con muchas recursadas
cursada(pepe, quimica, 2).
cursada(pepe, quimica, 3).
cursada(pepe, quimica, 4).
cursada(pepe, quimica, 5).
cursada(pepe, fisica_I, 5).
cursada(pepe, fisica_I, 2).
% Los Estudiantes
% cursada(Estudiante, Materia, Nota).
cursada(vero, Materia, 8) :-
    esInicial(Materia).
cursada(alan, sistemas_y_Organizaciones, 6).
cursada(alan, analisis_Matematico_I, 6).
cursada(alan, analisis_de_Sistemas, 2).
cursada(alan, analisis_de_Sistemas, 9).
cursada(alan, fisica_I, 2).
% Para estudiante con algunas recursadas
cursada(nelson, quimica, 2).
cursada(nelson, quimica, 3).
cursada(nelson, fisica_I, 2).
cursada(nelson, fisica_I, 10).
% Para estudiante veraniego
cursada(milhouse, quimica, 6).
cursada(milhouse, fisica_I, 6).
cursada(milhouse, matematica_Discreta, 2).
cursada(milhouse, matematica_Discreta, 8).
% Para estudiante atr
cursada(lisa, quimica, 10).
cursada(lisa, fisica_I, 10).



% Modalidades
% modalidad(estudiante(Estudiante),anual(Anio)).
% modalidad(estudiante(Estudiante),cuatrimestral(Anio,NroDeCustrimestre)).
% modalidad(estudiante(Estudiante),verano(AnioCalendario)).
modalidad(vero, sistemas_y_Organizaciones, anual(2014)).
modalidad(juan, sistemas_y_Organizaciones, anual(2015)).
modalidad(juan, quimica, cuatrimestral(2015,1)).
modalidad(juan, quimica, cuatrimestral(2015,2)).
modalidad(juan, fisica_I, deVerano(2016)).

modalidad(pepe, quimica, anual(2016)).
modalidad(pepe, quimica, cuatrimestral(2017, 1)).
modalidad(pepe, quimica, cuatrimestral(2017, 2)).
modalidad(pepe, quimica, anual(2018)).
modalidad(pepe, fisica_I, verano(2018)).
modalidad(pepe, fisica_I, anual(2018)).

modalidad(nelson, quimica, anual(2016)).
modalidad(nelson, quimica, cuatrimestral(2017, 2)).
modalidad(nelson, fisica_I, anual(2017)).
modalidad(nelson, fisica_I, cuatrimestral(2018, 1)).

modalidad(milhouse, quimica, anual(2016)).
modalidad(milhouse, fisica_I, verano(2017)).
modalidad(milhouse, matematica_Discreta, anual(2017)).
modalidad(milhouse, matematica_Discreta, verano(2018)).

modalidad(lisa, quimica, cuatrimestral(2016, 1)).
modalidad(lisa, fisica_I, cuatrimestral(2016, 2)).

% final(Estudiante, Final).
% funtores
% libre(Materia, Nota)
% normal(Materia, Nota)
final(vero, libre(ingles_II, 10)).

final(alan, normal(sistemas_y_Organizaciones, 4)).
final(alan, libre(ingles_I, 2)).

% Punto 1
esPesada(Materia) :-
    materia(Materia, 6),
    esIntegradora(Materia).
  
esPesada(Materia) :-
    materia(Materia,Horas),
    Horas>=4,
    not(esIntegradora(Materia)).


% Test
% Caso de prueba saber si una materia es pesada.

:-begin_tests(esPesada).

test(integradora_de_6_horas_es_pesada,nondet):-
    esPesada(proyecto_Final).

test(integradora_pero_no_es_pesada,fail):-
    esPesada(sistemas_y_Organizaciones).

test(materia_con_cursada_larga_es_pesada):-
    esPesada(sistemas_Operativos).

test(materia_que_no_es_pesada,fail):-
    esPesada(ingles_I).

:- end_tests(esPesada).


% Punto 2
% a)
esInicial(Materia) :-
    materia(Materia,_),
    not(esCorrelativaDe(Materia,_)).
  

% Test
% Caso de prueba saber si una materia es inicial.
% Las materias iniciales de la carrera son (11): 

:-begin_tests(esInicial).


test(todas_las_materias_iniciales, set(Materia == [algebra_y_Geometria_Analitica,algoritmo_y_Estructuras_de_Datos,analisis_Matematico_I,arquitectura_de_Computadoras,fisica_I,ingenieria_y_Sociedad,ingles_I,matematica_Discreta,quimica,sistemas_de_Representacion,sistemas_y_Organizaciones])) :-
	esInicial(Materia).

test(una_materia_no_es_inicial,fail):-
    esInicial(proyecto_Final).

:- end_tests(esInicial).


%b)
% materias necesarias para cursar una materia
necesariaParaCursar(Materia,Correlativa) :-
    esCorrelativaDe(Materia,OtraMateria),
    necesariaParaCursar(OtraMateria,Correlativa).
  
% Caso Base
necesariaParaCursar(Materia,Correlativa) :- esCorrelativaDe(Materia,Correlativa).

% Test
% Caso de prueba saber si una mateira tiene otras necesarias para cursarla.

:-begin_tests(necesariaParaCursar).

test(se_necesita_alguna_materia_para_cursar_otra,fail):-
    necesariaParaCursar(matematica_Discreta, _).

test(materia_necesaria_para_cursar_otra_de_forma_directa):-
    necesariaParaCursar(fisica_II,fisica_I).

test(materia_necesaria_indirecta_para_cursar_administracion_Gerencial,nondet):-
        necesariaParaCursar(administracion_Gerencial, sistemas_y_Organizaciones).

test(materias_necesarias_para_cursar_otra_materia, 
    set(MateriaNecesaria==[analisis_Matematico_I,algebra_y_Geometria_Analitica,analisis_Matematico_II,quimica,matematica_Superior])):-
    necesariaParaCursar(teoria_de_Control,MateriaNecesaria).

:- end_tests(necesariaParaCursar).

  
%c)
  % materias que habilita una materia
  % Materia = materia habilitada
  habilita(Correlativa,Materia) :- 
    necesariaParaCursar(Materia,Correlativa).


% Test
% Caso de prueba saber si una mateira habilita a otras a cursar.

:-begin_tests(habilita).
test(materias_que_habilitan_a_una_materia, set(Materia == [redes_de_Informacion,proyecto_Final])):-
    habilita(comunicaciones, Materia).
:- end_tests(habilita).

% 1
% materiaCursada(Estudiante, Materia).
materiaCursada(Estudiante, Materia) :-
    cursada(Estudiante, Materia, Nota),
    Nota >= 6.

materiaCursada(Estudiante, Materia) :-
    final(Estudiante, Final),
    finalLibreAprobadoDe(Materia, Final).


% finalLibreAprobadoDe(Materia, Final).
finalLibreAprobadoDe(Materia, libre(Materia, Nota)) :-
    aprobado(Nota).




% 2
% materiaAprobada(Estudiante, Materia).
materiaAprobada(Estudiante, Materia) :-
    promociono(Estudiante, Materia).

materiaAprobada(Estudiante, Materia) :-
    aproboFinal(Estudiante, Materia).


% promociono(Estudiante, Materia).
promociono(Estudiante, Materia) :-
    cursada(Estudiante, Materia, Nota),
    Nota > 7.


% aproboFinal(Estudiante, Materia).
aproboFinal(Estudiante, Materia) :-
    final(Estudiante, Final),
    finalAprobadoDe(Materia, Final).

/*
% finalAprobadoDe(Final, Materia).
finalAprobadoDe(Materia, Final) :-
    finalLibreAprobadoDe(Materia, Final).

finalAprobadoDe(Materia, Final) :-
    finalNormalAprobadoDe(Materia, Final).


% finalNormalAprobadoDe(Materia, Final).
finalNormalAprobadoDe(Materia, normal(Materia, Nota)) :-
    Nota >= 6.
*/
% finalAprobadoDe(Final, Materia).
finalAprobadoDe(normal(Materia, Nota), Materia) :-
    aprobado(Nota).

finalAprobadoDe(libre(Materia, Nota), Materia) :-
    aprobado(Nota).


aprobado(Nota) :-
    Nota>=6.


% estudiante(Estudiante).
estudiante(Estudiante) :-
    cursada(Estudiante, _, _).
estudiante(Estudiante) :-
    modalidad(Estudiante, _, _).

% ----------------------------------------------------------
% Test Los Estudiantes
% ----------------------------------------------------------
:- begin_tests(materiaCursada).
test(esta_cursada_por_haberla_aprobado, nondet) :-
	materiaCursada(vero, matematica_Discreta).

test(esta_cursada_por_haberla_aprobado_libre) :-
	materiaCursada(vero, ingles_II).


test(no_esta_cursada_por_haber_desaprobado, fail) :-
	materiaCursada(alan, fisica_I).

test(no_esta_cursada_por_nunca_haberla_cursado, fail) :-
	materiaCursada(vero, proyecto_Final).

test(no_esta_cursada_por_haber_desaprobado_el_final_libre, fail) :-
	materiaCursada(alan, ingles_I).


test(todas_las_materias_que_tiene_cursadas, set(Materia == [sistemas_y_Organizaciones, analisis_Matematico_I, analisis_de_Sistemas])) :-
	materiaCursada(alan, Materia).

:- end_tests(materiaCursada).



:- begin_tests(materiaAprobada).
test(esta_aprobada_por_haberla_promocionado, nondet) :-
	materiaAprobada(vero, quimica).


test(no_esta_aprobada_por_haber_desaprobado_final, fail) :-
	materiaAprobada(alan, ingles_I).

test(no_esta_aprobada_por_nunca_haberla_cursado_ni_rendido_final, fail) :-
	materiaAprobada(vero, proyecto_Final).

test(no_esta_aprobada_solamente_la_tiene_cursada, fail) :-
	materiaAprobada(alan, analisis_Matematico_I).

:- end_tests(materiaAprobada).




enQueAnioCurso(Estudiante,MateriaCursada,Anio):-
    modalidad(Estudiante,MateriaCursada,anual(Anio)).

enQueAnioCurso(Estudiante,MateriaCursada,Anio):-
    modalidad(Estudiante,MateriaCursada,cuatrimestral(Anio,_)).

enQueAnioCurso(Estudiante,MateriaCursada,AnioQueCurso):-
    modalidad(Estudiante,MateriaCursada,deVerano(Anio)),
    AnioQueCurso  is Anio -  1.


/*
materiaQueRecurso(Estudiante,Materia):-
    estudiante(Estudiante),
    materia(Materia, _),
    cuantasVecesRecursoUnaMateria(Estudiante,Materia,Cantidad),
    Cantidad > 1.

cuantasVecesRecursoUnaMateria(Estudiante,Materia,CantidadDeVecesRecursada):-
    findall(Materia, modalidad(Estudiante,Materia,_), MismaMateriaRecursada),
    length(MismaMateriaRecursada, CantidadDeVecesRecursada).
*/    
materiaQueRecurso(Estudiante,Materia):-
    estudiante(Estudiante),
    materia(Materia, _),
    materiaRecursada(Estudiante,Materia).


materiaRecursada(Estudiante,Materia) :-
    modalidad(Estudiante,Materia, UnaModalidad),
    modalidad(Estudiante,Materia, OtraModalidad),
    UnaModalidad \= OtraModalidad.


:- begin_tests(enQueAnioCurso).
test(cursada_en_el_2015_de_forma_anual,nondet):-
    enQueAnioCurso(juan,sistemas_y_Organizaciones,2015).
test(cursada_en_el_2015_de_forma_cuatrimestral,set(Anio==[2015,2015])):-
    enQueAnioCurso(juan,quimica,Anio).
test(cursada_en_el_2015_en_el_curso_del_verano_siguiente,nondet):-
    enQueAnioCurso(juan,quimica,2015).
:- end_tests(enQueAnioCurso). 

:- begin_tests(materiaQueRecurso).
test(juan_solamente_recurso_quimica,set(Materia==[quimica])):-
    materiaQueRecurso(juan,Materia).
test(vero_no_recurso_ninguna_materia,fail):-
    materiaQueRecurso(vero,_).
:- end_tests(materiaQueRecurso). 


%Punto 6
%a
esElSiguiente(Anio,OtroAnio):-
	Anio is OtroAnio - 1.

cursoInmediatamente(Estudiante,Materia) :-
	modalidad(Estudiante,Materia,Cursada),
	modalidad(Estudiante,Materia,Cursada2),
    Cursada \= Cursada2,
	sonInmediatas(Cursada,Cursada2).

sonInmediatas(anual(Anio1), anual(Anio2)) :- esElSiguiente(Anio1,Anio2).

sonInmediatas(anual(Anio1), cuatrimestral(Anio2,1)) :-
	esElSiguiente(Anio1,Anio2).

sonInmediatas(cuatrimestral(Anio,1), cuatrimestral(Anio,2)).


sonInmediatas(cuatrimestral(Anio1,2), anual(Anio2)) :-
	esElSiguiente(Anio1,Anio2).


sonInmediatas(verano(Anio1), anual(Anio1)).
sonInmediatas(verano(Anio1), cuatrimestral(1,Anio1)).



perfil(sinDescanso,Estudiante):-
	estudiante(Estudiante),
	forall(materiaQueRecurso(Estudiante,Materia), cursoInmediatamente(Estudiante,Materia)).

perfil(invictus,Estudiante):-
	estudiante(Estudiante),
	not(materiaQueRecurso(Estudiante,_)).

perfil(repechaje,Estudiante):-
    desaproboAlgunaAnual(Estudiante,Materia,Anio),
    aproboEnElPrimerCuatrimestreDelAnioSiguiente(Estudiante,Materia,Anio).

perfil(buenasCursadas,Estudiante):-
    estudiante(Estudiante),
    promocionoTodasLasCursadas(Estudiante).

perfil(seLoQueHicisteElVeranoPasado,Estudiante):-
    estudiante(Estudiante),
    forall(enQueAnioCurso(Estudiante,_,Ano),considerarAnioLectivoDelVerano(Estudiante,Ano)).


:- begin_tests(estudiantes_sinDescanso).

    %test(cumple_sinDescanso, set(Estudiante == [vero])):-
    test(cumple_sinDescanso_juan,nondet):-
        perfil(sinDescanso,juan).

    %test(no_cumple_sinDescanso, set(Estudiante == [vero]),fail):-
    test(no_cumple_sinDescanso,nondet,fail):-
        perfil(sinDescanso,nelson).
    

:- end_tests(estudiantes_sinDescanso).


%test b
:- begin_tests(estudiantes_invictus).

        test(cumple_invictus, set(Estudiante == [alan,lisa,vero])):-
            perfil(invictus,Estudiante).

        test(no_cumple_invictus, set(Estudiante == [pepo]),fail):-
            perfil(invictus,Estudiante).

:- end_tests(estudiantes_invictus).


% Punto 6 (c,d,e)
desaproboAlgunaAnual(Estudiante,Materia,Ano):-
    modalidad(Estudiante,Materia,anual(Ano)),
    cursada(Estudiante, Materia, Nota),
    Nota < 6.

aproboEnElPrimerCuatrimestreDelAnioSiguiente(Estudiante,Materia,Anio):-
    AnoSiguiente is Anio + 1,
    modalidad(Estudiante,Materia,cuatrimestral(AnoSiguiente,1)),
    materiaAprobada(Estudiante, Materia).


promocionoTodasLasCursadas(Estudiante):-
    forall(cursada(Estudiante, Materia,_),promociono(Estudiante, Materia)).




considerarAnioLectivoDelVerano(Estudiante,Anio):-
    AnioCalendario is Anio + 1,
    modalidad(Estudiante,_,verano(AnioCalendario)).


% Tests Punto 6 (c,d,e)


:- begin_tests(repechaje).
test(nelson_tuvo_repechaje,nondet):-
    perfil(repechaje,nelson).
test(vero_no_tuvo_repechaje,fail):-
    perfil(repechaje,vero).
:- end_tests(repechaje). 


:- begin_tests(buenasCursadas).
test(lisa_tuvo_buenas_cursadas,nondet):-
    perfil(buenasCursadas,lisa).
test(nelson_no_tuvo_buenas_cursadas,nondet,fail):-
    perfil(buenasCursadas,nelson).
:- end_tests(buenasCursadas). 


:- begin_tests(seLoQueHicisteElVeranoPasado).
test(milhouse_no_la_paso_bien_durante_los_veranos,nondet):-
    perfil(seLoQueHicisteElVeranoPasado,milhouse).
test(lisa_la_paso_bien_durante_los_veranos,nondet,fail):-
    perfil(seLoQueHicisteElVeranoPasado,lisa).
:- end_tests(seLoQueHicisteElVeranoPasado). 


% 7
% unicoPerfil(Estudiante).
/*
unicoPerfil(Estudiante) :-
    estudiante(Estudiante),
    cuantosPerfilesTiene(Estudiante,1).    

cuantosPerfilesTiene(Estudiante,Cantidad):-
    findall(Perfil, perfil(Perfil, Estudiante), Perfiles),
    length(Perfiles, Cantidad).
*/
unicoPerfil(Estudiante) :-
    perfil(_, Estudiante),
    not(tieneVariosPerfiles(Estudiante)).


tieneVariosPerfiles(Estudiante) :-
    perfil(UnPerfil, Estudiante),
    perfil(OtroPerfil, Estudiante),
    UnPerfil\=OtroPerfil.


:- begin_tests(unicoPerfil).
test(tiene_unico_perfil, nondet) :-
	unicoPerfil(nelson).

test(no_tiene_unico_perfil, fail) :-
	unicoPerfil(lisa). 

test(todos_los_estudiantes_que_tiene_un_unico_perfil, set(Estudiante == [nelson,milhouse,pepe])) :-
	unicoPerfil(Estudiante). 

:- end_tests(unicoPerfil).


%8
desempenioAcademico(Estudiante, Promedio):-
    cursada(Estudiante,_,_),
    findall(Indice, indiceDeCursada(Estudiante,_, Indice), Indices),
    sum_list(Indices, TotalDeIndices),
    length(Indices, Cantidad),
    Promedio is TotalDeIndices // Cantidad.
    
/*
desempenioAcademico(Estudiante,Materia,Indice):-
    cursada(Estudiante,Materia,Nota),
    modalidad(Estudiante,Materia,Epoca),
    indice(Nota,Epoca,Indice).
*/

indiceDeCursada(Estudiante, Materia, Indice) :-
    cursada(Estudiante, Materia, Nota),
    modalidad(Estudiante,Materia,Cursada),
    indice(Nota,Cursada,Indice).


indice(Nota,anual(_),Nota).

indice(Nota,cuatrimestral(_,Cuatrimestre),Indice):- Indice is Nota - Cuatrimestre.
  
indice(Nota,verano(Anio),Indice):-
    puntajeCursoDeVerano(Anio,Nota,Indice).

% puntajeCursoDeVerano(Anio, Nota, indice)
puntajeCursoDeVerano(Anio, _, 5):-
    esPar(Anio).
  
puntajeCursoDeVerano(Anio, Nota, Nota):-
    not(esPar(Anio)).


esPar(Numero):- 0 is Numero mod 2.



:- begin_tests(desempenioAcademico).
test(el_desempenio_de_pepe,nondet) :-
	desempenioAcademico(pepe,3). % Desempeño de pepe 3

test(el_desempenio_de_nelson,nondet) :-
	desempenioAcademico(nelson,3). % Desempeño de nelson 3,25

test(el_desempenio_de_milhouse,nondet) :-
	desempenioAcademico(milhouse,5). % Desempeño de milhouse

test(el_desempenio_de_lisa,nondet) :-
	desempenioAcademico(lisa,8).% Desempeño de lisa 8,5

:- end_tests(desempenioAcademico).
%asd

