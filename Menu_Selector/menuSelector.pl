:- module(menuSelector, [haveTime/2, numBike/3, carAvailable/1, whichMethods/3, voteCategory/2, votingList/2, collectHadMeal/3, collectPref/3]).

:- use_module(person,[isWoman/1, isMan/1, age/2, haveBicycle/1, haveCar/1, loves/2, workload/2, hadMeal/2, youngest/2]).
:- use_module(restaurant,[distance/2, category/2, canOrder/1, hasHall/1]).

:- use_module(library(dialect/hprolog)).

/*:- dynamic haveMeeting/1.*/

haveMeeting(B) :- B.
    
/* the main method */
menuSelector(L, Res) :-
    voteCategory(L, Categories),
    getDist(L, Dist),
    getRestaurants(Categories, Dist, Res).

/* 3. Getting Restaurants */
getRestaurants(Categories, Dist, Res) :-
    findMulCatRest(Categories, [], Restaurants),
    filterDist(Restaurants, Dist, [], Res).

/*filterDist([], _, Res, Res).
filterDist(Restaurants, Dist, Inter, Res) :-
    (   member(near, Dist) ->
            saveOneDist(Restaurants, near, [], SavedRestaurants),
            append(Inter, SavedRestaurants, NewInter),
            ord_subtract(Restaurant, SavedRestaurants, RemainingRests),
            ord_subtract(Dist, near, RemainDists),
            filterDist(Restaurants, RemainDists, NewInter, Res)
    ;   member(middle, Dist) ->
            saveOneDist(Restaurants, middle, [], SavedRestaurants),
            append(Inter, SavedRestaurants, NewInter),
            ord_subtract(Restaurant, SavedRestaurants, RemainingRests),
            ord_subtract(Dist, middle, RemainDists),
            filterDist(RemainingRests, RemainDists, NewInter, Res)
    ;   saveOneDist(Restaurants, far, [], SavedRestaurants),
        append(Inter, SavedRestaurants, NewInter),
        ord_subtract(Restaurant, SavedRestaurants, RemainingRests),
        ord_subtract(Dist, far, RemainDists),
        filterDist(RemainingRests, RemainDists, NewInter, Res)
    ).*/


filterDist([], _, Res, Res).
filterDist(Restaurants, [H|T], Inter, Res) :-
    (   H == near ->
            saveOneDist(Restaurants, near, [], SavedRestaurants),
            append(Inter, SavedRestaurants, NewInter),
            /*ord_subtract(Restaurant, SavedRestaurants, RemainingRests),
            ord_subtract(Dist, near, RemainDists),*/
            filterDist(Restaurants, T, NewInter, Res)
    ;   H == middle ->
            saveOneDist(Restaurants, middle, [], SavedRestaurants),
            append(Inter, SavedRestaurants, NewInter),
            /*ord_subtract(Restaurant, SavedRestaurants, RemainingRests),
            ord_subtract(Dist, middle, RemainDists),*/
            filterDist(RemainingRests, T, NewInter, Res)
    ;   saveOneDist(Restaurants, far, [], SavedRestaurants),
        append(Inter, SavedRestaurants, NewInter),
        /*ord_subtract(Restaurant, SavedRestaurants, RemainingRests),
        ord_subtract(Dist, far, RemainDists),*/
        filterDist(RemainingRests, T, NewInter, Res)
    ).


saveOneDist([], Dist, L, L).
saveOneDist([H|T], Dist, Inter, Res) :-
    (   distance(H, Dist) ->
            saveOneDist(T, Dist, [H|Inter], Res)
    ;   saveOneDist(T, Dist, Inter, Res)
    ).

/* Finds restaurants in multiple categories */

findMulCatRest([], L, L).
findMulCatRest([H|T], Inter, Res) :-
    findOneCatRest(H, SubRestaurants),
    append(Inter, SubRestaurants, NewInter),
    findMulCatRest(T, NewInter, Res).   

/* Find restaurants in one category */
findOneCatRest(Cat, Res) :-
    findall(Restaurant, category(Restaurant, Cat), Res).


/* 2. Get available distances */
getDist(L, Res) :-
    whichMethods(L, [], Methods),
    haveTime(L, Time),
    addDist(Methods, Time, Res).
    /*list_to_set(DupRes, Res).*/


/* Was to use when multiple timeslot exists - not required*/
/* addMulDist([], [], L, L).
addMulDist(Methods, [H|T], Inter, Res) :-
    addDist(Methods, H, [], SubRes),
    append(SubRes, Inter, NewInter).
    addMulDist(Methods, T, NewInter, Res). */


/* H2 is the time */
addDist(Methods, H2, Res) :-
    (   member(car, Methods) ->
            (   H2 == noTime ->
                    Res = [near, middle]
            ;   Res = [near, middle, far]
            ), !
    ;   member(bicycle, Methods) ->
            (   H2 == noTime ->
                    Res = [near]
            ;   H2 == moderateTime ->
                    Res = [near, middle]
            ;   Res = [near, middle, far]
            ), !
    ;   (   H2 == muchTime ->
                Res = [near, middle]
            ;   Res = [near]
        )
    ).

    /*(   H == walk ->
            (   H2 == muchTime ->
                    append([middle], Inter, NewInter)
                    addDist(T, H2, [near|NewInter], Res)
                ;   addDist(T, H2, [near|Inter], Res)
            )

    ;   H == bike ->
            (   H2 == muchTime ->
                    append([far], Inter, NewInter)
                    addDist(T, H2, [middle|NewInter], Res)
            ;   H2 == moderateTime ->
                    addDist(T, H2, [middle|Inter], Res)
            ;   addDist(T, H2, [near|Inter], Res)
            )
    ;   
            (   H2 == noTime ->
                    addDist(T, H2, [middle|Inter], Res)
            ;   addDist(T, H2, [far|Inter], Res)
        )
    ).*/

/* 2-1. Using name of the list, retrieve the workload. */
/* make into a set and select the worst workload */
haveTime(L, Res) :-
    findWorkload(L, [], Workloads),
    list_to_set(Workloads, Loads),
    ( member(high, Loads) ->
        Res = noTime
        ;
        member(middle, Loads) ->
            Res = moderateTime
            ;
            Res = muchTime
    ).

/** helper function for haveTime **/
findWorkload([], Res, Res).
findWorkload([H|T], Inter, Res) :-
    workload(H, W),
    findWorkload(T, [W|Inter], Res).


/* 2-2. transportations */
whichMethods(L, Inter, Res) :-
    ( haveMeeting(false) ->
        Res = [order], !
        ;
        Inter1 = [walk|Inter]
    ),
    ( bikeAvailable(L) ->
        Inter2 = [bicycle|Inter1]
        ;
        Inter2 = Inter1
    ),
    ( carAvailable(L) ->
        Inter3 = [car|Inter2]
        ;
        Inter3 = Inter2
    ),
    Res = Inter3.
    

/** helper function of whichMethods **/
/* test if bike is available */
/* If there same number of students as 'number of bike' */
bikeAvailable(L) :-
    numBike(L, 0, NumBike),
    length(L, NumPeople),
    NumBike =:= NumPeople.

/** helper function of bikeAvailable **/
/* Returns the number of bikes */
numBike([], Res, Res).
numBike([H|T], Inter, Res) :-
    ( haveBicycle(H) ->
        NewInter is Inter + 1,
        numBike(T, NewInter, Res)
        ;
        numBike(T, Inter, Res)
    ).


/** helper function of whichMethods **/
/* test if car is available */
/* If there are students less or equal than the 'number of car' * 4, students can drive */
carAvailable(L) :-
    numCar(L, 0, Numcar),
    Maxnum is Numcar * 4,
    length(L, NumPeople),
    Maxnum >= NumPeople.

/** helper function of carAvailable **/
/* Returns the number of cars */
numCar([], Res, Res).
numCar([H|T], Inter, Res) :-
    ( haveCar(H) ->
        NewInter is Inter + 1,
        numCar(T, NewInter, Res)
        ;
        numCar(T, Inter, Res)
    ).



/* 1. Get categories */
/* helpVoteCategory(Votes, Prefs, Occurs, Res). */
voteCategory(L, Res) :-
    votingList(L, Votes),
    helpVoteCategory(Votes, [], [], Res).

helpVoteCategory([], Prefs, Occurs, Res) :-
    max_list(Occurs, Maxnum),
    findPref(Prefs, Occurs, Maxnum, [], Res).
helpVoteCategory([H|T], Prefs, Occurs, Res) :-
    occurance(H, [H|T], 0, Occur),
    list_difference_eq([H|T], [H], FirstPrefRemList),
    helpVoteCategory(FirstPrefRemList, [H|Prefs], [Occur|Occurs], Res).


/* finds the occurance of the X on list and save the result on Res */
occurance(X, [], Res, Res).
occurance(X, [X|T], Init, Res) :-
    NewInit is Init+1,
    occurance(X, T, NewInit, Res).
occurance(X, [_|T], Init, Res) :-
    occurance(X, T, Init, Res).

/* In Res, store the pref that have occured maximum times
 * first array: pref, second array: occurance, Maxnum: maximum occurance,
 * fourth: intermediate rep (usually input []), fifth: Resulting prefs */
findPref([], [], Maxnum, L, L).
findPref([H|T], [H2|T2], Maxnum, Inter, Res) :-
    ( H2 =:= Maxnum ->
        findPref(T, T2, Maxnum, [H|Inter], Res)
        ;
        findPref(T, T2, Maxnum, Inter, Res)
    ).

/* subtrace eaten meal from preference */
votingList(L, Votes) :-
    collectHadMeal(L, [], Avoid),
    collectPref(L, [], Pref),
    removeMultFirst(Pref, Avoid, Votes).

/* ord_subtract could be used instead */
/* removes the first occurance of list of terms from the L(list) */
removeMultFirst(L, [], L).
removeMultFirst(L, [H|T], Res) :-
    removeFirst(H, L, NewL),
    removeMultFirst(NewL, T, Res).

/* remove the term first occurance from the list */
removeFirst(_, [], []).
removeFirst(ToRem, [ToRem|T], T) :- !.
removeFirst(ToRem, [H|T], [H|Res]) :-
    removeFirst(ToRem, T, Res).

/* collects eaten meal */
collectHadMeal([], L, L).
collectHadMeal([H|T], Inter, Res) :-
    ( hadMeal(H, X) ->
        collectHadMeal(T, [X|Inter], Res)
        ;
        collectHadMeal(T, Inter, Res)
    ). 

/* collects all preference that student has using loves */
collectPref([], L, L).
collectPref([H|T], Inter, Res) :-
    findall(Pref, loves(H, Pref), Prefs),
    append(Inter, Prefs, NewPrefs),
    collectPref(T, NewPrefs, Res).
