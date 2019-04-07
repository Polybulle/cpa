(* usage => for_each_edge f acc in
     f : a -> int -> int -> a
     acc : a
     in : in_channel
 *)

type graph = {
    name : string;
    node_n : int;
    edge_n : int;
    mutable inchan : in_channel option; 
  }
           
let eu_emails = {
    name = "email.txt";
    node_n = 1005;
    edge_n = 25571;
    inchan = None;
  }

let amazon = {
    name = "amazon.txt";
    node_n = 334863;
    edge_n = 925872;
    inchan = None;
  }

let livejournal = {
    name = "lj.txt";
    node_n = 3997962;
    edge_n = 34681189;
    inchan = None;
  }

let orkut = {
    name = "orkut.txt";
    node_n = 3072441;
    edge_n = 117185083;
    inchan = None;    
  }

let friendster = {
    name = "friendster.txt";
    node_n = 65608366;
    edge_n = 1806067135;
    inchan = None;
  }

          
let rec next_line inchan =
  let s = input_line inchan in
  if s.[0] = '#'
  then next_line inchan
  else Scanf.sscanf s "%d %d" (fun x y -> (x,y))

let inchan graph =
  match graph.inchan with
  | Some i -> i
  | None ->
     let i = open_in ("graphs_good/" ^ graph.name) in
     graph.inchan <- Some i; i
  
let next_edge graph = next_line (inchan graph)

                             
let fold_edge action acc0 graph =
  let acc = ref acc0 in
  try while true do
        acc := action !acc (next_edge graph)
      done;
      (* n'arrive jamais *) !acc
  with End_of_file -> !acc;;
                    
  
type edgelist = (int * int) array

type adjacency = bool array array 

type adjarray = {
    nodes : int;
    edges : int;
    cdegs : int array;
    adj : int array;
  }
                    
let edgelist_of_graph graph =
  let inchan = open_in  ("graphs_good/" ^ graph.name) in
  let get _ = Scanf.sscanf (input_line inchan) "%d %d" (fun x y -> x,y) in
  Array.init (graph.node_n + 1) get

let adjacency_of_graph graph =
  let a = Array.make ((graph.node_n+1)*(graph.node_n+1)) 0 in
  let doit () (n,m) = a.((n*(graph.node_n+1) + m)) <- 1 in
  fold_edge doit () graph; a


(* Les noeuds, de 1 à nodes dans les fichier, vont de 1 à nodes dans la structure,
 le premier élément de cdegs étant ignoré *)
let adjarray_of_graph graph =
  let cd = ref 0
  and cur_node = ref 1
  and cdegs = Array.make (graph.node_n + 2) 0
  and adj = Array.make (graph.edge_n * 2) 0 in
  let doit () =
    try
      while true do
        let (x,y) = next_edge graph in
        while !cur_node <> x do
          cdegs.(!cur_node) <- !cd;
          incr cur_node
        done;
        adj.(!cd) <- y;
        incr cd;
      done
    with End_of_file ->
      while !cur_node != graph.node_n + 1 do
          cdegs.(!cur_node) <- !cd;
          incr cur_node
      done 
  in
  doit ();
  { nodes = graph.node_n;
    edges = graph.edge_n;
    cdegs = cdegs;
    adj = adj;
  }

let neighbors adjar node =
  let inf = adjar.cdegs.(node)
  and sup = adjar.cdegs.(node+1) in
  Array.sub adjar.adj inf (sup - inf)

let degree adjar node = adjar.cdegs.(node+1) - adjar.cdegs.(node)

let bfs adja node action ret =
  let waitlist = ref [node]
  and marks = Array.make (adja.nodes+1) false in
  let add_nodes n =
    if not marks.(n) then begin
        waitlist := n :: !waitlist;
      end;
  in
  marks.(node) <- true;
  while !waitlist != [] do
    let cur = List.hd !waitlist in
    waitlist := List.tl !waitlist;
    action cur;
    Array.iter add_nodes (neighbors adja cur)
  done;
  Printf.printf "Done with bfs\n\n";
  ret ()
                      
let connected_comp g =
  let ccs = Array.make (g.nodes+1) 0 
  and cur_cc = ref 0
  and histogram = ref [] in
  let do_one node cc =
    let cur_cc_size = ref 0 in
    let add_to_cur_cc n = ccs.(n) <- !cur_cc; incr cur_cc_size in
    let ret () = () in
    if cc == 0
    then begin
        bfs g node add_to_cur_cc ret;
        histogram := (!cur_cc, !cur_cc_size) :: !histogram
      end
  in snd (Array.iteri do_one ccs; (ccs, !histogram))
     
let biggest_cc_id histogram =
  let largest big_cc cc =
    if snd big_cc > snd cc then big_cc else cc
  in List.fold_left largest (0,0) histogram

let get_cc ccs id =
  let doit node cc = if cc == id
                     then print_int node
  in Array.iteri doit ccs

let triangles_with u v g success fail ret =
  if v <= u then ()
  else let i = ref g.cdegs.(u)
       and j = ref g.cdegs.(v) in
       while !i < g.cdegs.(u+1) && g.adj.(!i) < u
       do incr i done;
       while !j < g.cdegs.(v+1) && g.adj.(!j) < u
       do incr j done;
       while !i < g.cdegs.(u+1)
          && !j < g.cdegs.(v+1)
          && g.adj.(!i) < v
          && g.adj.(!j) < v
       do
         if g.adj.(!i) < g.adj.(!j) then begin
             incr i;
             fail (u, g.adj.(!i), v);
           end
         else if g.adj.(!j) < g.adj.(!i) then begin
             incr j;
             fail (u, g.adj.(!j), v)
           end
         else
           success (u,g.adj.(!i),v)
       done;
       ret ()

let triangles g =
  let success (u,v,w) = Printf.printf "(%d, %d, %d)\n" u v w
  and fail _ = Printf.printf "Fail\n"
  and ret () = () 
  and adja = adjarray_of_graph g in
  let doit () (u,v) = triangles_with u v adja success fail ret in
  fold_edge doit () g
  
let () =
  let a = adjarray_of_graph livejournal in
  let cc = connected_comp a in
  let (id, size) = biggest_cc_id cc in
  Printf.printf "lcc: %d, size: %d\n" id size
