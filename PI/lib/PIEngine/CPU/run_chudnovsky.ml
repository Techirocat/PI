let file = "pi.txt"

let () =

  Gc.set { (Gc.get ()) with space_overhead = 40 };
  
  let digits =
    if Array.length Sys.argv > 1 then 
      int_of_string Sys.argv.(1) 
    else 
      1000
  in
  Printf.printf "=== PiEngine Computation ===\n";
  Printf.printf "Target digits: %d\n" digits;
  Printf.printf "Output to: %s\n\n" file;
  Printf.printf "Starting...\n";

  let t = Pi_cpu.Utils.start () in
  let pi = Pi_cpu.Chudnovsky.compute_pi digits in
  Pi_cpu.Utils.stop t;

  Out_channel.with_open_bin file (fun oc -> output_string oc pi);


  Printf.printf "=== Time Results ==\n";
  Printf.printf "Total time: %.6f seconds\n" (Pi_cpu.Utils.elapsed_seconds t)