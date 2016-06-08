open Keystone
open Keystone.T

let _ =
  let (e,engine) = ks_open KS_ARCH_X86 KS_MODE_64 in
  let i = ks_asm engine "add eax, ecx" 0 in
  let err = ks_close engine in
  let s = ks_strerror KS_ERR_VERSION in
  Printf.printf "%s\n" s;
  ()
