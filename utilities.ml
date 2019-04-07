(* usage => for_each_edge f acc in
     f : a -> int -> int -> a
     acc : a
     in : in_channel
 *)

type graph = {
    name : string;
    nodes_n : int;
    edges_n : int;
    max_node : int;
    mutable inchan : in_channel option; 
  }
           
let eu_emails = {
    name = "email-Eu-core";
    nodes_n = 1005;
    edges_n = 25571;
    max_node = 1004;
    inchan = None;
  }

let amazon = {
    name = "com-amazon.ungraph";
    nodes_n = 334863;
    edges_n = 925872;
    max_node = 548551;
    inchan = None;
  }

let livejournal = {
    name = "com-lj.ungraph";
    nodes_n = 3997962;
    edges_n = 34681189;
    max_node = 4036537;
    inchan = None;
  }

let orkut = {
    name = "com-orkut.ungraph";
    nodes_n = 3072441;
    edges_n = 117185083;
    max_node = 3072626;
    inchan = None;    
  }

let friendster = {
    name = "com-friendster.ungraph";
    nodes_n = 65608366;
    edges_n = 1806067135;
    max_node = 124836179;
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
     let i = open_in ("graphs/" ^ graph.name ^ ".clean.txt") in
     graph.inchan <- Some i; i
  
let next_edge graph = next_line (inchan graph)
  
let inchan_dedup graph =
  match graph.inchan with
  | Some i -> i
  | None ->
     let i = open_in ("graphs/" ^ graph.name ^ ".dedup.txt") in
     graph.inchan <- Some i; i
            
let rec next_edge_dedup graph = next_line (inchan_dedup graph)
          
 
                              
let fold_edge action acc0 graph =
  let acc = ref acc0 in
  try while true do
        acc := action !acc (next_edge graph)
      done;
      (* n'arrive jamais *) !acc
  with End_of_file -> !acc;;
       
let max_node graph =
  fold_edge (fun acc (n,m) -> max acc (max n m)) (-1) graph
    
let degrees graph =
  let acc = Array.make (graph.max_node + 1) 0 and
      doit a (n,m) =
        a.(n) <- a.(n) + 1;
        a.(m) <- a.(m) + 1;
        a in
  fold_edge doit acc graph

let print_degrees graph =
  let outchan  = open_out ("graphs/" ^ graph.name ^ ".degrees.txt") in
  let print_one node deg = output_string outchan 
                             (string_of_int node ^ " " ^ string_of_int deg ^ "\n") in
  Array.iteri print_one (degrees graph)

let get_degrees graph =
  let inchan = open_in  ("graphs/" ^ graph.name ^ ".degrees.txt") in
  let get _ = Scanf.sscanf (input_line inchan) "%d %d" (fun x y -> y) in
  Array.init (graph.max_node + 1) get
  
let special_quantity graph =
  let degs = get_degrees graph in
  let doit q (n,m) = q + degs.(n) * degs.(m) in
  fold_edge doit 0 graph


let fold_line action acc0 inchan =
  let acc = ref acc0 in
  try while true do
        acc := action !acc (next_line inchan)
      done;
      (* n'arrive jamais *) !acc
  with End_of_file -> !acc;;
  
let make_index graph = 
  let inchan = open_in ("graphs/" ^ graph.name ^ ".degrees.sorted.txt") in
  let a = Array.make (graph.max_node+1) 0 in
  let doit new_node (node, deg) = a.(node) <- new_node; new_node + 1 in
  ignore (fold_line doit 1 inchan); a
      
let reindex index file =
  let inchan = open_in (file ^ ".txt")
  and outchan = open_out (file ^ ".reindexed.txt") in
  try
    while true do
      let (n,m) = next_line inchan in
      if index.(n) != 0 && index.(m) != 0 then
        Printf.fprintf outchan "%d %d" index.(n) index.(m)
    done
  with End_of_file -> ()

let () = print_int (special_quantity friendster)
