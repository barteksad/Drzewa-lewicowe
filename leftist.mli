<pre><div class="text_to_html">type &#039;a queue
(** Typ złączalnej kolejki priorytetowej *)

val empty : &#039;a queue
(** Pusta kolejka priorytetowa *)

val add : &#039;a -&gt; &#039;a queue -&gt; &#039;a queue
(** [add e q] zwraca kolejkę powstałą z dołączenia elementu [e] 
    do kolejki [q] *)

exception Empty
(** Wyjątek podnoszony przez [delete_min] gdy kolejka jest pusta *)

val delete_min : &#039;a queue -&gt; &#039;a * &#039;a queue
(** Dla niepustej kolejki [q], [delete_min q] zwraca parę [(e,q&#039;)] gdzie [e]
    jest elementem minimalnym kolejki [q] a [q&#039;] to [q] bez elementu [e].
    Jeśli [q] jest puste podnosi wyjątek [Empty]. *)

val join : &#039;a queue -&gt; &#039;a queue -&gt; &#039;a queue
(** [join q1 q2] zwraca złączenie kolejek [q1] i [q2] *)

val is_empty : &#039;a queue -&gt; bool
(** Zwraca [true] jeśli dana kolejka jest pusta. W przeciwnym razie [false] *)

</div></pre>