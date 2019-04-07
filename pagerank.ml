open Projet

let multiply adja p =
  let aux n =
    let x = ref 0.0 in
    for i = adja.cdegs.(n) to adja.cdegs.(n+1) - 1 do
      x := !x +. p.(i)
    done; !x
  in Array.init (Array.length p) aux
   
let pagerank adja alpha iter_n =
  let p = ref (Array.make adja.nodes (1.0 /. float_of_int adja.nodes)) in
  let hole_correction sum i x =
    let k = (1.0 -. sum) /. (float_of_int adja.nodes) in
    !p.(i) <- x +. k in
  let aux sum i x =
    let y = (1.0 -. alpha) *. x  +. alpha *. (1.0 /. float_of_int adja.nodes) in
    sum := !sum +. y;
    !p.(i) <- y in
  for _ = 0 to iter_n do
    p := multiply adja !p;
    let sum = ref 0.0 in
    Array.iteri (aux sum) !p;
    Array.iteri (hole_correction !sum) !p;
  done;
  !p
                                            
                                            
                                            
