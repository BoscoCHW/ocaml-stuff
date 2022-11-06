
(* Acceptance function: 
   if e1 < e2, return natural e ^ ((e1 -. e2) /. t)
   else return 1  *)
let acceptance e1 e2 t: float = 
  if e1 < e2 then exp((e1 -. e2) /. t)
  else 1.

(* Reduces t by a factor at the end of every interval *)
let next_t_and_cd t factor count_down interval = 
  if count_down = 0 then (t *. factor, interval)
  else (t, count_down - 1)

(* Run the annealing process *)
let run (s, energy , next) (t, factor, interval) numsteps = 
  (* given the current state (s), temperature (t), 
     countdown to next temperature decrease (count_down) 
     and the remaining steps (rem_steps),
     running annealing
     *)
  let rec run' s t count_down rem_steps = 
    if rem_steps = 0 then s
    else 
      let t', cd = next_t_and_cd t factor count_down interval in 
      let next_s = next s in 
      if acceptance (energy s) (energy next_s) t >= Random.float 1. then 
        run' next_s t' cd (rem_steps - 1)
      else run' s t' cd (rem_steps - 1)
  in
  run' s t interval numsteps 