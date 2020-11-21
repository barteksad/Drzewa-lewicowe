(* Zadanie 2 - Drzewa lewicowe *)
(* Autor: Bartek Sadlej *)

exception Empty;;

type 'a queue  = 
    Pusta |
    Korzen of 'a queue * 'a * 'a queue * int;; 
    (* lewy syn, ten element, prayw syn, długośc ścierzki do najbliższego liścia po prawej *)

let empty = 
    Pusta;;

let is_empty = 
    function
    | Pusta -> true 
    | Korzen _ -> false

(* funkcja pomocnicza biorąca Korzeń i zwracająca jego elementy jako krotke *)
let unpack = 
    function
    | Korzen(lq,value,rq,rlp) -> lq,value,rq,rlp
    | Pusta -> assert false;;

let rec join l r = 
    match l,r with
    | Pusta, Pusta -> Pusta (*  Puste nie ma co łączyć*)
    | Pusta, Korzen _ -> r (*  Puste  i kolejka zwracamy kolejke*)
    | Korzen _, Pusta -> l (*  Puste  i kolejka zwracamy kolejke*)

    (* ------------ Łączenie nie pustych ------------ *)
    (* chcemy żeby wartośc w korzeniu lewej była <= niż w kożeniu prawej *)
    (* jeśli tak nie jest wywołujemy join od prawej i lewej, wtedy się zamienia *)

    (* 1. Jeśli w lewej jest jedne element to jako lewe poddrzewo lewej ustawiamy prawą i załatwione *)
    | Korzen (lleft,lval,lright,lrlp),
      Korzen (rleft,rval,rright,rrlp) 
      when lval <= rval && lleft = Pusta && lright = Pusta->
        Korzen(r,lval,Pusta,0)

    (* 2. Lewa ma jedno lub dwa poddrzewo *)
    | Korzen (lleft,lval,lright,lrlp),
      Korzen (rleft,rval,rright,rrlp) 
      when lval <= rval ->

        (* nowe_lewe_rlp = długość najkrótszej prawostronnej ścieżki do liścia w lewym poddrzewie lewej *)
        let _,_,_,nowe_lewe_rlp = unpack lleft in

        (* Łączymy prawe poddrzewo lewej z prawą *)

        (* jeśli ścieżka w nowej kolejce jest dłuższa niż w lewym poddrzewie lewj *)
        (* to zamieniammy te elementy ze sobą żeby zachować lewicowość *)
        let Korzen(npleft,npval,npright,nowe_prawe_rlp)  = join lright r 
        in
        if nowe_prawe_rlp <= nowe_lewe_rlp 
          then Korzen(lleft,lval,Korzen(npleft,npval,npright,nowe_prawe_rlp),nowe_prawe_rlp +1)
        else
          Korzen(Korzen(npleft,npval,npright,nowe_prawe_rlp),lval,lleft,nowe_lewe_rlp+1)
      
    (* Wartość w korzeniu lewej jest większa niż w prawej *)
    (* więc wywołujemy join od prawej jako lewa i lewej jako prawa*)
    | Korzen (lleft,lval,lright,lrlp),
      Korzen (rleft,rval,rright,rrlp) ->
      join r l;;

let add e q = 
  join (Korzen(Pusta,e,Pusta,0)) q;;


let delete_min q = 
  match q with
  | Pusta -> raise Empty
  | _ -> let (l,n,r,rlp) = unpack q in n,(join l r);;
    

        

