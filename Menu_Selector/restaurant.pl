:- module(restaurant,[distance/2, category/2, canOrder/1, hasHall/1]).


/* restaurant: name*/
restaurant(dongmatgol). /* korean, american */
restaurant(subway). /* american, near */
restaurant(lotteria). /* american, near */
restaurant(littleHanoi). /* vietnamese, near */
restaurant(wellchai). /* chinese, near */
restaurant(camto). /* american, near */

restaurant(manbouno). /* italian, middle */
restaurant(bespoke). /* italian, middle */
restaurant(garimSodam). /* italian, middle */
restaurant(gyeongbokgung). /* korean, middle */

restaurant(malgm). /* indian, middle, */
restaurant('1117meal'). /* japanese, middle */
restaurant(berisinjuku). /* japanese, middle */
restaurant(masterOfSsam). /* korean, middle */

restaurant(hackbob). /* japanese, middle, order */
restaurant(bonjuk). /* korean, middle, order/hall */
restaurant(momsTouch). /* american, middle, order/hall, burgers */

restaurant(suspiciousPig). /* korean, far, order, samgyupsal(pork), shoulder(pork) */
restaurant('24hoursDaewangseong'). /* chinese, far, order, jajangmyeon(pork), jjampong(pork) */
restaurant(orientalgate). /* vietnamese, far, order, pho, padthai */
restaurant(thiwanpung). /* chinese, far, order/hall gogi-mando(pork), shrimp-mando, stirFry-rice */
restaurant(sangmuChobab). /* japanese, far, order/hall chobapset, salmonchobap */


/* distance: near, middle, far*/
distance(dongmatgol, near).
distance(subway, near).
distance(lotteria, near).
distance(littleHanoi, near).
distance(wellchai, near).
distance(camto, near).

distance(manbouno, middle).
distance(bespoke, middle).
distance(garimSodam, middle).
distance(gyeongbokgung, middle).
distance(malgm, middle).
distance('1117meal', middle).
distance(berisinjuku, middle).
distance(masterOfSsam, middle).
distance(momsTouch, middle).
distance(hackbob, middle).
distance(bonjuk, middle).

distance(suspiciousPig, far).
distance('24hoursDaewangseong', far).
distance(orientalgate, far).
distance(thiwanpung, far).
distance(sangmuChobab, far).


/* category */
category(dongmatgol, korean).
category(gyeongbokgung, korean).
category(masterOfSsam, korean).
category(bonjuk, korean).
category(suspiciousPig, korean).

category(dongmatgol, american).
category(subway, american).
category(lotteria, american).
category(camto, american).

category(manbouno, italian).
category(bespoke, italian).
category(garimSodam, italian).

category('1117meal', japanese).
category(berisinjuku, japanese).
category(hackbob, japanese).
category(sangmuChobab, japanese).

category(wellchai, chinese).
category('24hoursDaewangseong', chinese).

category(littleHanoi, vietnamese).
category(orientalgate, vietnamese).

category(malgm, indian).


/*isKorean(dongmatgol).
isKorean(gyeongbokgung).
isKorean(masterOfSsam).
isKorean(bonjuk).
isKorean(suspiciousPig).

isAmerican(dongmatgol).
isAmerican(subway).
isAmerican(lotteria).
isAmerican(camto).

isItalian(manbouno).
isItalian(bespoke).
isItalian(garimSodam).

isJapanese('1117meal').
isJapanese(berisinjuku).
isJapanese(hackbob).
isJapanese(sangmuChobab).

isChinese(wellchai).
isChinese('24hoursDaewangseong').

isVietnamese(littleHanoi).
isVietnamese(orientalgate).

isIndian(malgm).*/


/* canOrder */
canOrder(hackbob).
canOrder(bonjuk).
canOrder(momsTouch).
canOrder(suspiciousPig).
canOrder('24hoursDaewangseong').
canOrder(orientalgate).
canOrder(thiwanpung).
canOrder(sangmuChobab).


/* hasHall */
hasHall(momsTouch).
hasHall(thiwanpung).
hasHall(sangmuChobab).
hasHall(dongmatgol).
hasHall(subway).
hasHall(lotteria).
hasHall(littleHanoi).
hasHall(wellchai).
hasHall(camto).
hasHall(manbouno).
hasHall(bespoke).
hasHall(garimSodam).
hasHall(gyeongbokgung).
hasHall(malgm).
hasHall('1117meal').
hasHall(berisinjuku).
hasHall(masterOfSsam).
hasHall(bonjuk).


/* mainmenu */
/*menu().
menu().*/
