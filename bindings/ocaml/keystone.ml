module B = Ffi_bindings.Bindings(Ffi_generated)
module KSTypes = B.T
open Ctypes
open Foreign

open KSTypes
open B

type asm_result =
  | ASMSuccess of string
  | ASMError of string
    
 
type ks_open_result =
  | KSOpenSucc of ks_struct structure ptr
  | KSOpenError of string

                     
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
  let encoding = allocate_n ~count:1 (ptr uchar) in 
  let encoding_size = allocate size_t (Unsigned.Size_t.of_int 0) in
  let stat_count = allocate size_t (Unsigned.Size_t.of_int 0) in
  match (ks_asm_ engine str addr2 encoding encoding_size stat_count) with
  | 0 -> begin
      let f = CArray.from_ptr (!@ encoding) (Unsigned.Size_t.to_int (!@ encoding_size)) in
      let f' = CArray.to_list f in
      let f'' = Array.of_list f' in
      ks_free_ (to_voidp (!@ encoding));
      let str = List.fold_left (fun str c -> let t = Printf.sprintf "%x " (Unsigned.UChar.to_int c) in str^t) "" f' in
      ASMSuccess str
    end
  | _ -> let err = ks_errno engine in
         ASMError(ks_strerror err)
         
          
