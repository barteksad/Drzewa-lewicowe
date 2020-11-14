type node = 
    Node of int*int;; 
     (* wartość, ile takich elementów *)

type queue  = 
    Pusta |
    Korzen of queue * node * queue * int;; 
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
        Korzen(rleft,Node(rval,rcount+1),rright,rrlp)

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
        if nrlp < nowe_lewe_rlp 
          then Korzen(lleft,Node(lval,lcount),Korzen(npleft,npnode,npright,nrlp),nrlp +1)
        else
          Korzen(Korzen(npleft,npnode,npright,nrlp),Node(lval,lcount),lleft,nowe_lewe_rlp+1)
      
    (* b) tak samo tylko zamieniamy lewą z prawą *)
    | Korzen (lleft,Node(lval,lcount),lright,lrlp),
      Korzen (rleft,Node(rval,rcount),rright,rrlp) ->
      join r l;;




        

