module B = Ffi_bindings.Bindings(Ffi_generated)
(*module Types = B.T *)
open Ctypes
open Foreign


 


  








module Types = B.T
(* module Types = (B.T : TYPES with *)
(*                   type ks_opt_type = B.T.ks_opt_type and *)
(*                   type ks_struct = B.T.ks_struct and *)
(*                   type ks_engine = B.T.ks_engine and *)
(*                   type ks_t = B.T.ks_t and *)
(*                   type ks_error = B.T.ks_error and *)
(*                   type ks_opt_value = B.T.ks_opt_value and *)
(*                   type ks_arch = B.T.ks_arch and *)
(*                   type ks_mode = B.T.ks_mode  *)
(*                ) *)





       



       
type asm_result =
  | ASMSuccess of (int array) * int * int
  | ASMError of string

type ks_open_result =
  | KSOpenSucc of Types.ks_struct structure ptr
  | KSOpenError of string


let ks_arch_supported arch =
  B.ks_arch_supported_ arch

let ks_version major minor =
  let major = Unsigned.UInt.of_int major in
  let minor = Unsigned.UInt.of_int minor in
  let u = B.ks_version_ (allocate uint major) (allocate uint minor) in
  Unsigned.UInt.to_int u
                      
(* TODO: better handling of error value *)
let ks_option engine opttype optvalue =
  match opttype with
  | Types.KS_OPT_SYNTAX -> B.ks_option_ engine Types.KS_OPT_SYNTAX optvalue 
                        
let ks_strerror err =
  B.ks_strerror_ err

let ks_open arch ?(endian=Types.KS_MODE_LITTLE_ENDIAN) mode =
  let mode =
    match endian with
    | Types.KS_MODE_BIG_ENDIAN ->
       begin
         let m = Ffi_generated_types.constant (Types.string_of_ks_mode Types.KS_MODE_BIG_ENDIAN) int64_t in
         let m' = Ffi_generated_types.constant (Types.string_of_ks_mode mode) int64_t in
         Int64.add m m'
         
       end
    | Types.KS_MODE_LITTLE_ENDIAN -> Ffi_generated_types.constant (Types.string_of_ks_mode mode) int64_t
    | _ -> assert false (* TODO: better error handling here *)
  in
  let engine = allocate_n ~count:1 ((ptr Types.ks_engine)) in
  
  match (B.ks_open_ arch mode engine) with
  | Types.KS_ERR_OK -> KSOpenSucc (!@ engine)
  | _ as err -> KSOpenError(ks_strerror err)
                           

let ks_close engine = B.ks_close_ engine 

let ks_errno engine = B.ks_err_ engine


let ks_asm engine str addr =
  let addr1 = Unsigned.UInt64.of_int addr in
  let addr2 = Unsigned.UInt64.to_int64 addr1 in
  let encoding = allocate_n ~count:1 (ptr uchar) in 
  let encoding_size = allocate size_t (Unsigned.Size_t.of_int 0) in
  let stat_count = allocate size_t (Unsigned.Size_t.of_int 0) in
  match (B.ks_asm_ engine str addr2 encoding encoding_size stat_count) with
  | 0 -> begin
      let f = CArray.from_ptr (!@ encoding) (Unsigned.Size_t.to_int (!@ encoding_size)) in
      let f' = CArray.to_list f in
      let f'' = Array.of_list f' in
      let c' = Array.map (fun c -> Unsigned.UChar.to_int c) f'' in
      B.ks_free_ (to_voidp (!@ encoding));
      ASMSuccess(c', (Unsigned.Size_t.to_int (!@ encoding_size)), (Unsigned.Size_t.to_int (!@ stat_count)))
    end
  | _ -> let err = ks_errno engine in
         ASMError(ks_strerror err)
         
let asm_array_to_string a =
  Array.fold_left (fun str c -> let t = Printf.sprintf "%02x " c in str^t) "" a 