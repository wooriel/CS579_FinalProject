:- module(person,[isWoman/1, isMan/1, age/2, haveBicycle/1, haveCar/1, loves/2, workload/2, hadMeal/2, youngest/2]).

/*:- use_module(filename, [list of methods to bring])*/

person(amy).
person(bob).
person(chris).
person(david).
person(ellie).
person(fred).
person(george).
person(hanna).
person(iris).
person(jay).
person(kay).


isWoman(amy).
isWoman(ellie).
isWoman(hanna).
isWoman(iris).
isWoman(kay).


isMan(X) :- \+ isWoman(X).


age(amy, 18).
age(bob, 24).
age(chris, 20).
age(david, 22).
age(ellie, 23).
age(fred, 24).
age(george, 19).
age(hanna, 21).
age(iris, 18).
age(jay, 20).
age(kay, 18).


/* both: amy, bob, chris */
/* only bicycle: david, ellie, fred, george */
/* only car: hanna, iris */
/* neither: jay, kay */
haveBicycle(amy).
haveBicycle(bob).
haveBicycle(chris).
haveBicycle(david).
haveBicycle(ellie).
haveBicycle(fred).
haveBicycle(george).


haveCar(amy).
haveCar(hanna).
haveCar(iris).

/* food categories based on countary */
/* extend: flour-based food(분식), fastfood, steamed, soup */
/* preferences */
/* amy: k, a, bob: c, v, chris: k, c, david: a, j, ellie: k, v, fred: c, i
 * george: a, v, hanna: i, t, iris: a, i, jay: j, t, kay: k, j */
loves(amy, korean).
loves(chris, korean).
loves(ellie, korean).
loves(kay, korean).

loves(bob, chinese).
loves(chris, chinese).
loves(fred, chinese).

loves(amy, american).
loves(david, american).
loves(hanna, american).

loves(david, japanese).
loves(iris, japanese).
loves(jay, japanese).
loves(kay, japanese).

loves(ellie, vietnamese).
loves(george, vietnamese).

loves(fred, indian).
loves(hanna, indian).

loves(bob, italian).
loves(george, italian).
loves(iris, italian).
loves(jay, italian).


/* workload: low, middle, high */
workload(amy, low).
workload(bob, low).
workload(chris, low).
workload(david, middle).
workload(ellie, middle).
workload(fred, middle).
workload(george, middle).
workload(hanna, middle).
workload(iris, high).
workload(jay, high).
workload(kay, high).


/* if they had meal, it removes from the category */
hadMeal(amy, chinese).
hadMeal(bob, vietnamese).
hadMeal(david, korean).
hadMeal(fred, chinese).
hadMeal(iris, american).
hadMeal(kay, indian).


/* gets the list of names, and returns the names of youngest members */
/* L: list of names
 * Ages: list of ages that are queried
 * Minage: minimum age(single number)
 * Return the names with the minimum age */
youngest(L,X) :-
    findage(L, Ages),
    min_list(Ages, Minage),
    findname(L, [Minage], X).

/** helper function of findage **/
helpfindage([], L, L).
helpfindage([H|T], L, Res) :-
    age(H, Age),
    helpfindage(T, [Age|L], Res).

/* gets the list of name and returns corresponding age
 * finding in opposite direction does not work */
findage([H|T], Ages) :-
    helpfindage([H|T], [], Ages).

/** helper function of namesformultage **/
/* gets corresponding names of single age */
namesforsingage(Age, ResNames) :-
    findall(CondName, age(CondName, Age), ResNames).

/** helper function of findname **/
/* gets corresponding names of list of ages*/
namesformultage([], ResNames, ResNames).
namesformultage([H|T], Interlist, ResNames) :-
    findall(CondName, age(CondName, H), CondNames),
    append(Interlist, CondNames, NewInterList),
    namesformultage(T, NewInterList, ResNames).

/* get names from ages (Condnames), and remove names that are not initially in the input name list */
findname(Names, Ages, ResNames) :-
    namesformultage(Ages, [], CondNames),
    ord_intersect(CondNames, Names, ResNames).