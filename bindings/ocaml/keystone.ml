module B = Ffi_bindings.Bindings(Ffi_generated)
(*module Types = B.T *)
open Ctypes
open Foreign


module type TYPES =
  sig
    (** Test doc *)
    type ks_struct

    val ks_struct : ks_struct Ctypes.structure Ffi_generated_types.typ

    type ks_engine

    val ks_engine : ks_struct Ctypes.structure Ffi_generated_types.typ

    type ks_t

    val ks_t : ks_struct Ctypes.structure Ctypes_static.ptr Ffi_generated_types.typ

    (** Architecture type *)
    type ks_arch =
      | KS_ARCH_ARM
      | KS_ARCH_ARM64
      | KS_ARCH_MIPS
      | KS_ARCH_X86
      | KS_ARCH_PPC
      | KS_ARCH_SPARC
      | KS_ARCH_SYSTEMZ
      | KS_ARCH_HEXAGON
      | KS_ARCH_MAX


   type ks_error =
     |  KS_ERR_OK
     | KS_ERR_NOMEM
     | KS_ERR_ARCH
     | KS_ERR_HANDLE
     | KS_ERR_MODE
     | KS_ERR_VERSION
     | KS_ERR_OPT_INVALID
     | KS_ERR_ASM_EXPR_TOKEN
     | KS_ERR_ASM_DIRECTIVE_VALUE_RANGE
     | KS_ERR_ASM_DIRECTIVE_ID
     | KS_ERR_ASM_DIRECTIVE_TOKEN
     | KS_ERR_ASM_DIRECTIVE_STR
     | KS_ERR_ASM_DIRECTIVE_COMMA
     | KS_ERR_ASM_DIRECTIVE_RELOC_NAME
     | KS_ERR_ASM_DIRECTIVE_RELOC_TOKEN
     | KS_ERR_ASM_DIRECTIVE_FPOINT
     | KS_ERR_ASM_DIRECTIVE_UNKNOWN
     | KS_ERR_ASM_VARIANT_INVALID
     | KS_ERR_ASM_DIRECTIVE_EQU
     | KS_ERR_ASM_EXPR_BRACKET
     | KS_ERR_ASM_SYMBOL_MODIFIER
     | KS_ERR_ASM_SYMBOL_REDEFINED
     | KS_ERR_ASM_SYMBOL_MISSING
     | KS_ERR_ASM_RPAREN
     | KS_ERR_ASM_STAT_TOKEN
     | KS_ERR_ASM_UNSUPPORTED
     | KS_ERR_ASM_MACRO_TOKEN
     | KS_ERR_ASM_MACRO_PAREN
     | KS_ERR_ASM_MACRO_EQU
     | KS_ERR_ASM_MACRO_ARGS
     | KS_ERR_ASM_MACRO_LEVELS_EXCEED
     | KS_ERR_ASM_MACRO_STR
     | KS_ERR_ASM_ESC_BACKSLASH
     | KS_ERR_ASM_ESC_OCTAL
     | KS_ERR_ASM_ESC_SEQUENCE
     | KS_ERR_ASM_ESC_STR
     | KS_ERR_ASM_TOKEN_INVALID
     | KS_ERR_ASM_INSN_UNSUPPORTED
     | KS_ERR_ASM_FIXUP_INVALID
     | KS_ERR_ASM_LABEL_INVALID
     | KS_ERR_ASM_FRAGMENT_INVALID
     | KS_ERR_ASM_INVALIDOPERAND
     | KS_ERR_ASM_MISSINGFEATURE
     | KS_ERR_ASM_MNEMONICFAIL


   type ks_mode =
          KS_MODE_ARM
        | KS_MODE_BIG_ENDIAN
        | KS_MODE_LITTLE_ENDIAN
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


   type ks_opt_type = KS_OPT_SYNTAX


   type ks_opt_value =
          KS_OPT_SYNTAX_INTEL
        | KS_OPT_SYNTAX_ATT
        | KS_OPT_SYNTAX_NASM
        | KS_OPT_SYNTAX_MASM
        | KS_OPT_SYNTAX_GAS

      val string_of_ks_error : ks_error -> string

      val string_of_ks_mode : ks_mode -> string

  end
  


  









module Types = (B.T : TYPES with
                  type ks_opt_type = B.T.ks_opt_type and
                  type ks_struct = B.T.ks_struct and
                  type ks_engine = B.T.ks_engine and
                  type ks_t = B.T.ks_t and
                  type ks_error = B.T.ks_error and
                  type ks_opt_value = B.T.ks_opt_value and
                  type ks_arch = B.T.ks_arch and
                  type ks_mode = B.T.ks_mode 
               )





       



       
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
  | KSOpenSucc of Types.ks_struct structure ptr
  | KSOpenError of string


(** Determin if the given architecture is supported. True if the
    architecture is supported, false otherwise *)
let ks_arch_supported arch =
  B.ks_arch_supported_ arch

(** Returns a combined API version *)                     
let ks_version major minor =
  let major = Unsigned.UInt.of_int major in
  let minor = Unsigned.UInt.of_int minor in
  let u = B.ks_version_ (allocate uint major) (allocate uint minor) in
  Unsigned.UInt.to_int u
                      
(** Set an option for a Keystone engine instance. 
    Takes an engine instance, an option type and an option value. 

    Returns KS_ERR_OK on success or another value on failure. 
*)
(* TODO: better handling of error value *)
let ks_option engine opttype optvalue =
  match opttype with
  | Types.KS_OPT_SYNTAX -> B.ks_option_ engine Types.KS_OPT_SYNTAX optvalue 
                        
let ks_strerror err =
  B.ks_strerror_ err

(** Create a new instance of Keystone engine. 
    arch: is architecture type, mode is hardware mode. 
    
    Returns a Keystone engine 
*)
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
                           

(** Close the Keystone engine instance *)
let ks_close engine = B.ks_close_ engine 

(** Given a Keystone engine instance, returns the last error number on
    some API function failure. *)
let ks_errno engine = B.ks_err_ engine


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
         
(** Convert an array containing an encoding of an assembly string and
returns a string representation. *)
let asm_array_to_string a =
  Array.fold_left (fun str c -> let t = Printf.sprintf "%02x " c in str^t) "" a 
