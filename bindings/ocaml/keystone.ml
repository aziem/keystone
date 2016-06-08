module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

open Ctypes
open Foreign

type ks_mode =
   | KS_MODE_LITTLE_ENDIAN
   | KS_MODE_BIG_ENDIAN
   | KS_MODE_ARM
   | KS_MODE_THUMB
   | KS_MODE_V8
   | KS_MODE_MICRO
   | KS_MODE_MIPS3
   | KS_MODE_MIPS32R6
   | KS_MODE_MIPS32
   | KS_MODE_MIPS64
   | KS_MODE_16
   | KS_MODE_32
   | KS_MODE_64
   | KS_MODE_PPC32
   | KS_MODE_PPC64
   | KS_MODE_QPX
   | KS_MODE_SPARC32
   | KS_MODE_SPARC64
   | KS_MODE_V9 

let int_of_mode = function
  | KS_MODE_LITTLE_ENDIAN -> T.ks_mode_little_endian
  | KS_MODE_BIG_ENDIAN -> T.ks_mode_big_endian
  | KS_MODE_ARM -> T.ks_mode_arm
                     
   | KS_MODE_THUMB -> T.ks_mode_thumb
   | KS_MODE_V8 -> T.ks_mode_v8
   | KS_MODE_MICRO -> T.ks_mode_micro
   | KS_MODE_MIPS3 -> T.ks_mode_mips3
   | KS_MODE_MIPS32R6 -> T.ks_mode_mips32r6
   | KS_MODE_MIPS32 -> T.ks_mode_mips32
   | KS_MODE_MIPS64 -> T.ks_mode_mips64
   | KS_MODE_16 -> T.ks_mode_16
   | KS_MODE_32 -> T.ks_mode_32
   | KS_MODE_64 -> T.ks_mode_64
   | KS_MODE_PPC32 -> T.ks_mode_ppc32
   | KS_MODE_PPC64 -> T.ks_mode_ppc64
   | KS_MODE_QPX -> T.ks_mode_qpx
   | KS_MODE_SPARC32 -> T.ks_mode_sparc32
   | KS_MODE_SPARC64 -> T.ks_mode_sparc64
   | KS_MODE_V9 -> T.ks_mode_v9

    
let arch_of_ks_arch = function
  | T.KS_ARCH_ARM -> T.ks_arch_arm
  | T.KS_ARCH_ARM64 -> T.ks_arch_arm64
  | T.KS_ARCH_MIPS -> T.ks_arch_mips
  | T.KS_ARCH_X86 -> T.ks_arch_x86
  | T.KS_ARCH_PPC -> T.ks_arch_ppc
  | T.KS_ARCH_SPARC -> T.ks_arch_sparc
  | T.KS_ARCH_SYSTEMZ -> T.ks_arch_systemz
  | T.KS_ARCH_HEXAGON -> T.ks_arch_hexagon
  | T.KS_ARCH_MAX -> T.ks_arch_max





                     
                    
                     

let ks_arch_supported arch =
  B.ks_arch_supported (arch_of_ks_arch arch)

let ks_version i j =
  B.ks_version (allocate int i) (allocate int j)

type ks_engine
type ks_t = ks_engine structure ptr 
let ks_engine : ks_engine structure typ = structure "ks_engine"
              
let ks_open_ = foreign "ks_open" (T.ks_arch @-> int64_t @-> (ptr (ptr ks_engine)) @-> returning int64_t)

let ks_close_ = foreign "ks_close" (ptr ks_engine @-> returning int64_t) 

let ks_err_ = foreign "ks_errno" (ptr ks_engine @-> returning int64_t)
                        
let ks_open arch mode  =
  let engine = allocate_n ~count:1 ((ptr ks_engine)) in
  let err = ks_open_ arch (int_of_mode mode) engine in
  (err, !@ engine)
    
let ks_close engine = ks_close_ engine 

let ks_errno engine = ks_err_ engine

let ks_free_ = foreign "ks_free" (ptr void @-> returning void) 


let ks_asm_ = foreign "ks_asm" (ptr ks_engine @-> string @-> int64_t @-> ptr (ptr char) @-> ptr size_t @-> ptr size_t @-> returning int)


let char_array_as_string a =
    let len = Array.length a in 
    let b = Buffer.create len in 
    try 
      for i = 0 to len -1 do 
        let c = Array.get a i in 
        if c = '\x00' then raise Exit else Buffer.add_char b c
      done;
      Buffer.contents b 
    with Exit -> Buffer.contents b

let ks_strerror_ = foreign "ks_strerror" (T.ks_err @-> returning (ptr char))
                                 
let ks_strerror err =
  let c = ks_strerror_ err in
  let s = coerce (ptr char) string c in
  s

                           
let ks_asm engine str addr =
  let addr1 = Unsigned.UInt64.of_int addr in
  let addr2 = Unsigned.UInt64.to_int64 addr1 in
  let encoding = allocate_n ~count:1 (ptr char) in 
  let encoding_size = allocate size_t (Unsigned.Size_t.of_int 0) in
  let stat_count = allocate size_t (Unsigned.Size_t.of_int 0) in
  let i = ks_asm_ engine str addr2 encoding encoding_size stat_count  in
  let f = CArray.from_ptr (!@ encoding) (Unsigned.Size_t.to_int (!@ encoding_size)) in
  let f' = CArray.to_list f in
  (*List.iter (fun c -> Printf.printf " %02x " (Char.code c); flush stdout) f'; *)
  ks_free_ (to_voidp (!@ encoding));
  f'
