open Keystone


let _ =
  Printf.printf "IS: %b\n" (Keystone.ks_arch_supported KS_ARCH_ARM)
