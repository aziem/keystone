module B = Ffi_bindings.Bindings(Ffi_generated)
module Types = B.T
open Ctypes
open Foreign

open Types
open B


(** Result of assembling instructions Succesful assembly results in
    ASMSuccess with an int array contained the assembled instructions,
    the size of the array of instructions, and a the number of
    instructions encoded.
 *)
type asm_result =
  | ASMSuccess of (int array) * int * int
  | ASMError of string

(** Result of opening an assembly engine. 
    KSOpenSucc indicates success and contains (a C pointer) to the engine. 
    KSOpenError indicates an error and contains the error string.  
*)
type ks_open_result =
  | KSOpenSucc of ks_struct structure ptr
  | KSOpenError of string


(** Determin if the given architecture is supported. True if the
    architecture is supported, false otherwise *)
let ks_arch_supported arch =
  ks_arch_supported_ arch

(** Returns a combined API version *)                     
let ks_version major minor =
  let major = Unsigned.UInt.of_int major in
  let minor = Unsigned.UInt.of_int minor in
  let u = ks_version_ (allocate uint major) (allocate uint minor) in
  Unsigned.UInt.to_int u
                      
(** Set an option for a Keystone engine instance. 
    Takes an engine instance, an option type and an option value. 

    Returns KS_ERR_OK on success or another value on failure. 
*)
(* TODO: better handling of error value *)
let ks_option engine opttype optvalue =
  match opttype with
  | KS_OPT_SYNTAX -> ks_option_ engine KS_OPT_SYNTAX optvalue 
                        
let ks_strerror err =
  ks_strerror_ err

(** Create a new instance of Keystone engine. 
    arch: is architecture type, mode is hardware mode. 
    
    Returns a Keystone engine 
*)
let ks_open arch ?(endian=KS_MODE_LITTLE_ENDIAN) mode =
  let mode =
    match endian with
    | KS_MODE_BIG_ENDIAN ->
       begin
         let m = Ffi_generated_types.constant (string_of_ks_mode KS_MODE_BIG_ENDIAN) int64_t in
         let m' = Ffi_generated_types.constant (string_of_ks_mode mode) int64_t in
         Int64.add m m'
         
       end
    | KS_MODE_LITTLE_ENDIAN -> Ffi_generated_types.constant (string_of_ks_mode mode) int64_t
    | _ -> assert false (* TODO: better error handling here *)
  in
  let engine = allocate_n ~count:1 ((ptr ks_engine)) in
  
  match (ks_open_ arch mode engine) with
  | T.KS_ERR_OK -> KSOpenSucc (!@ engine)
  | _ as err -> KSOpenError(ks_strerror err)
                           

(** Close the Keystone engine instance *)
let ks_close engine = ks_close_ engine 

(** Given a Keystone engine instance, returns the last error number on
    some API function failure. *)
let ks_errno engine = ks_err_ engine


(** Assemble a string: takes a Keystone engine instance, a string to
assember and a start address to assemble from. 

On success returns ASMSuccess with an array of the encoded input
assembly string, the size of the encoding and the number of statements
successfully processed.
 *)
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
      let c' = Array.map (fun c -> Unsigned.UChar.to_int c) f'' in
      ks_free_ (to_voidp (!@ encoding));
      ASMSuccess(c', (Unsigned.Size_t.to_int (!@ encoding_size)), (Unsigned.Size_t.to_int (!@ stat_count)))
    end
  | _ -> let err = ks_errno engine in
         ASMError(ks_strerror err)
         
(** Convert an array containing an encoding of an assembly string and
returns a string representation. *)
let asm_array_to_string a =
  Array.fold_left (fun str c -> let t = Printf.sprintf "%02x " c in str^t) "" a 
