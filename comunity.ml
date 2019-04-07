open Projet

let () = Random.self_init ()

let random_permutation size = 
  let p = Array.init size (fun n -> n) in
  for i = 0 to size - 2 do
    let j = i + Random.int (size - i) in
    let temp = p.(i) in
    p.(i) <- p.(j);
    p.(j) <- temp
  done;
  p

let benchmark emitter size cluster_size p q =
  for i = 1 to size - 1 do
    for j = i + 1 to size do
      let prob =
        if i/cluster_size = j/cluster_size then q else p
      in if Random.float 1.0 < prob
         then emitter (i,j)
    done
  done
    
let make_benchmarks () =
  let emit outchan (i,j) = Printf.fprintf outchan "%d %d\n" i j in
  for i = 1 to 5 do
    for j = 1 to 5 do
      let q = i in     (* en vrai, q = i / 6 *)
      let p = i * j in (* en vrai, p = i*j / 36 *)
      let name = Printf.sprintf "benchmark_%d_%d.txt" i j in
      let outchan = open_out name in
      benchmark (emit outchan) 400 100 (float_of_int (p/36)) (float_of_int (q/6))
    done
  done

  
type 'a multiset = ('a * int) list

let empty : 'a multiset = []

let rec add ms x = match ms with
  | [] -> [(x, 1)]
  | (y, n) :: tl when y = x -> (y, n+1) :: tl
  | (y, n) :: tl            -> (y, n)   :: add tl x

let majority =
  let rec aux cur score = function
    | [] -> cur
    | (el, n) :: tl ->
       if n > cur
       then aux el n tl
       else aux cur score tl
  in aux (-1) (-1)
                             
let propagate adja labels node =
  let lbls = Array.map (Array.get labels) (neighbors adja node) in
  let histogram = Array.fold_left add empty lbls in
  let new_lbl = majority histogram in
  let lbl = labels.(node) in
  labels.(node) <- new_lbl;
  lbl = new_lbl
  
let label_propagation adja =
  let labels = Array.init adja.nodes (fun n -> n) in
  let is_done = ref false in
  while not !is_done do
    is_done := true;
    let p = random_permutation adja.nodes in
    for i = 0 to adja.nodes-1 do
      is_done := propagate adja labels p.(i)
    done
  done;
  labels
     
let output_labels labels name =
  let outchan = open_out name in
  let print_one n lbl =
    Printf.fprintf outchan "%d %d\n" n lbl in
  Array.iteri print_one labels
