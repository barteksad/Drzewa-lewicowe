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

let unpack = 
    function
    | Korzen(lq,value,rq,rlp) -> lq,value,rq,rlp
    | Pusta -> assert false;;

let rec join l r = 
    match l,r with
    | Pusta, Pusta -> Pusta
    | Pusta, Korzen _ -> r
    | Korzen _, Pusta -> l


    | Korzen (lleft,lval,lright,lrlp),
      Korzen (rleft,rval,rright,rrlp) 
      when lval <= rval && lleft = Pusta && lright = Pusta->
        Korzen(r,lval,Pusta,0)

    | Korzen (lleft,lval,lright,lrlp),
      Korzen (rleft,rval,rright,rrlp) 
      when lval <= rval ->
        let _,_,_,nowe_lewe_rlp = unpack lleft in
        if lright = Pusta
          then  
            if rrlp <= nowe_lewe_rlp 
              then Korzen(lleft,lval,r,rrlp)
            else Korzen(r,lval,lleft,nowe_lewe_rlp+1)
        else
        let Korzen(npleft,npval,npright,nrlp)  = join lright r 
        in
        if nrlp <= nowe_lewe_rlp 
          then Korzen(lleft,lval,Korzen(npleft,npval,npright,nrlp),nrlp +1)
        else
          Korzen(Korzen(npleft,npval,npright,nrlp),lval,lleft,nowe_lewe_rlp+1)
      
    (* b) tak samo tylko zamieniamy lewą z prawą *)
    | Korzen (lleft,lval,lright,lrlp),
      Korzen (rleft,rval,rright,rrlp) ->
      (* let a = print_int (3) in *)
      join r l;;

let add e q = 
  join (Korzen(Pusta,e,Pusta,0)) q;;


let delete_min q = 
  match q with
  | Pusta -> raise Empty
  | _ -> let (l,n,r,rlp) = unpack q in n,(join l r);;
    

        

