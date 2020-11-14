type 'a node = 
    Node of 'a*int;; 
     (* wartość, ile takich elementów *)

exception Empty;;

type 'a queue  = 
    Pusta |
    Korzen of 'a queue * 'a node * 'a queue * int;; 
    (* lewy syn, ten element, prayw syn, długośc ścierzki do najbliższego liścia po prawej *)

let empty = 
    Pusta;;

let is_empty = 
    function
    | Pusta -> true
    | Korzen _ -> false

let unpack = 
    function
    | Korzen(lq,n,rq,rlp) -> lq,n,rq,rlp
    | Pusta -> assert false;;

let rec join l r = 
    match l,r with
    | Pusta, Pusta -> Pusta
    | Pusta, Korzen _ -> r
    | Korzen _, Pusta -> l
        
    (*1. Dodajemy jeden element który już jest, zwiększamy tylko count  *)
    | Korzen (lleft,Node(lval,lcount),lright,lrlp),
      Korzen (rleft,Node(rval,rcount),rright,rrlp) 
      when lval = rval -> 
        if lleft = Pusta && lright = Pusta
          then Korzen(rleft,Node(rval,rcount+1),rright,rrlp)
        else Korzen(lleft,Node(lval,lcount+1),lright,lrlp)

    (*2. Dodajemy zupełnie nowy element *)
            
    (* a) Korzeń lewej mniejszy od korzenia prawej *)
    (*2.1 a) Lewa nie ma prawego ani lewego syna, dołączamy r jako lewego syna i aktualizujemy l.rpl = r.rpl + 1  *)
    | Korzen (lleft,Node(lval,lcount),lright,lrlp),
      Korzen (rleft,Node(rval,rcount),rright,rrlp) 
      when lval < rval && lleft = Pusta && lright = Pusta->
        Korzen(r,Node(lval,lcount),Pusta,0)

    (*2.2 a) Lewa ma lewego albo prawego syna  *)
    | Korzen (lleft,Node(lval,lcount),lright,lrlp),
      Korzen (rleft,Node(rval,rcount),rright,rrlp) 
      when lval < rval ->
        let _,_,_,nowe_lewe_rlp = unpack lleft in
        let Korzen(npleft,npnode,npright,nrlp)  = join lright r 
        in
        if nrlp <= nowe_lewe_rlp 
          then Korzen(lleft,Node(lval,lcount),Korzen(npleft,npnode,npright,nrlp),nrlp +1)
        else
          Korzen(Korzen(npleft,npnode,npright,nrlp),Node(lval,lcount),lleft,nowe_lewe_rlp+1)
      
    (* b) tak samo tylko zamieniamy lewą z prawą *)
    | Korzen (lleft,Node(lval,lcount),lright,lrlp),
      Korzen (rleft,Node(rval,rcount),rright,rrlp) ->
      (* let a = print_int (3) in *)
      join r l;;

let add e q = 
  join (Korzen(Pusta,Node(e,1),Pusta,0)) q;;


let delete_min q = 
  match q with
  | Pusta -> raise Empty
  | _ -> let (l,n,r,rlp) = unpack q in
    match n with
    | Node(value,1) -> value, (join l r)
    | Node(value,greater_than_one) -> value, Korzen(l,Node(value,greater_than_one-1),r,rlp);; 

        

