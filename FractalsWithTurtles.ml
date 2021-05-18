#load "graphics.cma";;
open Graphics;;

open_graph "999x999";;

type fractal = (int*int) list;;

type tortue = {mutable x : float ;  (*abscisse*)
                mutable y : float ; (*ordonnée*)
                mutable alpha : float ; (*en radians évidemment*)
                mutable color : Graphics.color};; (*couleur*)

(*implémentation des fonctions de base de la tortue*)    

let pi=4.*.atan 1.;;
let s2=(2.)**(1./.2.);;

let init a b theta c = 
    {x=a ; y=b ; alpha=theta ; color=c};;

let recule (t:tortue) a b =
    t.x <- t.x -. a;
    t.y <- t.y -. b;;

let avance (t:tortue) l =
    set_color t.color;
    moveto (int_of_float t.x) (int_of_float t.y);
    let newX = t.x +. l*.(cos t.alpha)
    and newY = t.y +. l*.(sin t.alpha) in
        t.x <- newX;
        t.y <- newY;
        lineto (int_of_float newX) (int_of_float newY);;
        

let tourne_gauche (t:tortue) theta =
    t.alpha <- t.alpha +. theta;;

let tourne_droite (t:tortue) theta =
    t.alpha <- t.alpha -. theta;;

(*quelques fractales*)

let rec triangle_koch (t:tortue) n l =
    if n=0 then avance t l
    else(
        triangle_koch t (n-1) (l/.3.);
        tourne_gauche t (pi/.3.);
        triangle_koch t (n-1) (l/.3.);
        tourne_droite t (2.*.pi/.3.);
        triangle_koch t (n-1) (l/.3.);
        tourne_gauche t (pi/.3.);
        triangle_koch t (n-1) (l/.3.);
    );;

let rec rectangles (t:tortue) n l =
    if n=0 then avance t l
    else(
        rectangles t (n-1) (l/.3.);
        tourne_gauche t (pi/.2.);
        rectangles t (n-1) (l/.4.);
        tourne_droite t (pi/.2.);
        rectangles t (n-1) (l/.3.);
        tourne_droite t (pi/.2.);
        rectangles t (n-1) (l/.4.);
        tourne_gauche t (pi/.2.);
        rectangles t (n-1) (l/.3.);
    );;

let rec forme1 (t:tortue) n l =
    if n=0 then avance t l
    else(
        forme1 t (n-1) (l/.4.);
        tourne_gauche t (pi/.2.);
        forme1 t (n-1) (l/.4.);
        tourne_droite t (pi/.2. +. atan (3./.5.));
        forme1 t (n-1) (58.*.l/.120.);
        tourne_gauche t (atan (3./.5.));
        forme1 t (n-1) (l/.4.);
    );;

let rec forme2 (t:tortue) n l =
    if n=0 then avance t l
    else(
        forme2 t (n-1) (l/.4.);
        tourne_gauche t (pi/.2.);
        forme2 t (n-1) (l/.4.);
        tourne_droite t (pi/.4.);
        forme2 t (n-1) (l*.s2/.4.);
        tourne_droite t (pi/.2.);
        forme2 t (n-1) (l*.s2/.4.);
        tourne_droite t (pi/.4.);
        forme2 t (n-1) (l/.4.);
        tourne_gauche t (pi/.2.);
        forme2 t (n-1) (l/.4.);
    );;
        
        
clear_graph ();;
let t=init 0. 200. 0. green in triangle_koch t 9 999.;;
let t=init 0. 600. 0. blue in rectangles t 5 999.;;
let t=init 0. 0. 0. black in forme1 t 4 999.;;
let t=init 0. 400. 0. magenta in forme2 t 6 999.;;
let t=init 0. 200. 0. black in penis t 1 999.;;
let t=init 0. 400. 0. black in nazi t 5 999.;;
