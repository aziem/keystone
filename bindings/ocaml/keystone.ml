(*module B = Ffi_bindings.Bindings(Ffi_generated) *)
module T = Ffi_bindings.Types(Ffi_generated_types)

open Ctypes
open Foreign


type ks_engine
type ks_t = ks_engine structure ptr

let ks_engine : ks_engine structure typ = structure "ks_engine"
                      
type asm_result =
  | ASMSuccess of char list
  | ASMError of string
    
 
type ks_open_result =
  | KSOpenSucc of ks_t
  | KSOpenError of string

let ks_arch_supported_ = foreign "ks_arch_supported" (T.ks_arch @-> returning bool)

let ks_version_ = foreign "ks_version" (ptr int @-> ptr int @-> returning int)
                                                   
let ks_open_ = foreign "ks_open" (T.ks_arch @-> T.ks_mode @-> (ptr (ptr ks_engine)) @-> returning T.ks_err)
                       
let ks_close_ = foreign "ks_close" (ptr ks_engine @-> returning int64_t) 

let ks_err_ = foreign "ks_errno" (ptr ks_engine @-> returning T.ks_err)

let ks_option_ = foreign "ks_option" (ptr ks_engine @-> T.ks_opt_type @-> T.ks_opt_value @-> returning T.ks_err)

let ks_strerror_ = foreign "ks_strerror" (T.ks_err @-> returning (ptr char))

let ks_free_ = foreign "ks_free" (ptr void @-> returning void) 

let ks_asm_ = foreign "ks_asm" (ptr ks_engine @-> string @-> int64_t @-> ptr (ptr char) @-> ptr size_t @-> ptr size_t @-> returning int)
 

let ks_arch_supported arch =
  ks_arch_supported_ arch

let ks_version i j =
  ks_version_ (allocate int i) (allocate int j)

let ks_strerror err =
  coerce (ptr char) string (ks_strerror_ err) 

let ks_open arch mode  =
    let engine = allocate_n ~count:1 ((ptr ks_engine)) in
    match (ks_open_ arch mode engine) with
    | T.KS_ERR_OK -> KSOpenSucc (!@ engine)
    | _ as err -> KSOpenError(ks_strerror err)
    
let ks_close engine = ks_close_ engine 

let ks_errno engine = ks_err_ engine
                         
let ks_asm engine str addr =
  let addr1 = Unsigned.UInt64.of_int addr in
  let addr2 = Unsigned.UInt64.to_int64 addr1 in
  let encoding = allocate_n ~count:1 (ptr char) in 
  let encoding_size = allocate size_t (Unsigned.Size_t.of_int 0) in
  let stat_count = allocate size_t (Unsigned.Size_t.of_int 0) in
  match (ks_asm_ engine str addr2 encoding encoding_size stat_count) with
  | 0 -> begin
      let f = CArray.from_ptr (!@ encoding) (Unsigned.Size_t.to_int (!@ encoding_size)) in
      let f' = CArray.to_list f in
      ks_free_ (to_voidp (!@ encoding));
      ASMSuccess f'
    end
  | _ -> let err = ks_errno engine in
         ASMError(ks_strerror err)
         
          
