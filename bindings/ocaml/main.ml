open Keystone
open Keystone.T

let _ =
  match (ks_open KS_ARCH_X86 KS_MODE_64) with
  | KSOpenSucc engine ->
     begin
       match (ks_asm engine "add eax, ecx" 0) with
       | ASMSuccess asm -> ignore (ks_close engine); List.iter (fun c -> Printf.printf "%02x" (int_of_char c)) asm
       | ASMError s -> Printf.printf "ERROR: %s\n" s; ignore (ks_close engine)
     end
  | KSOpenError e -> Printf.printf "ERROR: %s\n"  e 

  
