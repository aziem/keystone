open Ctypes
module Types (F: Cstubs.Types.TYPE) =
  struct

    open F
 
    (* type ks_arch = *)
    (*   | KS_ARCH_ARM  *)
    (*   | KS_ARCH_ARM64 *)
    (*   | KS_ARCH_MIPS *)
    (*   | KS_ARCH_X86 *)
    (*   | KS_ARCH_PPC *)
    (*   | KS_ARCH_SPARC *)
    (*   | KS_ARCH_SYSTEMZ *)
    (*   | KS_ARCH_HEXAGON *)
    (*   | KS_ARCH_MAX *)
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
  | KS_ERR_OK
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


  let ks_err_ok = constant "KS_ERR_OK" int64_t
  let ks_err_nomem = constant "KS_ERR_NOMEM" int64_t
  let ks_err_arch = constant "KS_ERR_ARCH" int64_t
  let ks_err_handle = constant "KS_ERR_HANDLE" int64_t
  let ks_err_mode = constant "KS_ERR_MODE" int64_t
  let ks_err_version = constant "KS_ERR_VERSION" int64_t
  let ks_err_opt_invalid = constant "KS_ERR_OPT_INVALID" int64_t
  let ks_err_asm_expr_token = constant "KS_ERR_ASM_EXPR_TOKEN" int64_t
  let ks_err_asm_directive_value_range = constant "KS_ERR_ASM_DIRECTIVE_VALUE_RANGE" int64_t
  let ks_err_asm_directive_id = constant "KS_ERR_ASM_DIRECTIVE_ID" int64_t
  let ks_err_asm_directive_token = constant "KS_ERR_ASM_DIRECTIVE_TOKEN" int64_t
  let ks_err_asm_directive_str = constant "KS_ERR_ASM_DIRECTIVE_STR" int64_t
  let ks_err_asm_directive_comma = constant "KS_ERR_ASM_DIRECTIVE_COMMA" int64_t
  let ks_err_asm_directive_reloc_name = constant "KS_ERR_ASM_DIRECTIVE_RELOC_NAME" int64_t
  let ks_err_asm_directive_reloc_token = constant "KS_ERR_ASM_DIRECTIVE_RELOC_TOKEN" int64_t
  let ks_err_asm_directive_fpoint = constant "KS_ERR_ASM_DIRECTIVE_FPOINT" int64_t
  let ks_err_asm_directive_unknown = constant "KS_ERR_ASM_DIRECTIVE_UNKNOWN" int64_t
  let ks_err_asm_variant_invalid = constant "KS_ERR_ASM_VARIANT_INVALID" int64_t
  let ks_err_asm_directive_equ = constant "KS_ERR_ASM_DIRECTIVE_EQU" int64_t
  let ks_err_asm_expr_bracket = constant "KS_ERR_ASM_EXPR_BRACKET" int64_t
  let ks_err_asm_symbol_modifier = constant "KS_ERR_ASM_SYMBOL_MODIFIER" int64_t
  let ks_err_asm_symbol_redefined = constant "KS_ERR_ASM_SYMBOL_REDEFINED" int64_t
  let ks_err_asm_symbol_missing = constant "KS_ERR_ASM_SYMBOL_MISSING" int64_t
  let ks_err_asm_rparen = constant "KS_ERR_ASM_RPAREN" int64_t
  let ks_err_asm_stat_token = constant "KS_ERR_ASM_STAT_TOKEN" int64_t
  let ks_err_asm_unsupported = constant "KS_ERR_ASM_UNSUPPORTED" int64_t
  let ks_err_asm_macro_token = constant "KS_ERR_ASM_MACRO_TOKEN" int64_t
  let ks_err_asm_macro_paren = constant "KS_ERR_ASM_MACRO_PAREN" int64_t
  let ks_err_asm_macro_equ = constant "KS_ERR_ASM_MACRO_EQU" int64_t
  let ks_err_asm_macro_args = constant "KS_ERR_ASM_MACRO_ARGS" int64_t
  let ks_err_asm_macro_levels_exceed = constant "KS_ERR_ASM_MACRO_LEVELS_EXCEED" int64_t
  let ks_err_asm_macro_str = constant "KS_ERR_ASM_MACRO_STR" int64_t
  let ks_err_asm_esc_backslash = constant "KS_ERR_ASM_ESC_BACKSLASH" int64_t
  let ks_err_asm_esc_octal = constant "KS_ERR_ASM_ESC_OCTAL" int64_t
  let ks_err_asm_esc_sequence = constant "KS_ERR_ASM_ESC_SEQUENCE" int64_t
  let ks_err_asm_esc_str  = constant "KS_ERR_ASM_ESC_STR"  int64_t
  let ks_err_asm_token_invalid = constant "KS_ERR_ASM_TOKEN_INVALID" int64_t
  let ks_err_asm_insn_unsupported = constant "KS_ERR_ASM_INSN_UNSUPPORTED" int64_t
  let ks_err_asm_fixup_invalid  = constant "KS_ERR_ASM_FIXUP_INVALID"  int64_t
  let ks_err_asm_label_invalid  = constant "KS_ERR_ASM_LABEL_INVALID"  int64_t
  let ks_err_asm_fragment_invalid = constant "KS_ERR_ASM_FRAGMENT_INVALID" int64_t
  let ks_err_asm_invalidoperand = constant "KS_ERR_ASM_INVALIDOPERAND" int64_t
  let ks_err_asm_missingfeature = constant "KS_ERR_ASM_MISSINGFEATURE" int64_t
  let ks_err_asm_mnemonicfail  = constant "KS_ERR_ASM_MNEMONICFAIL"  int64_t


  let ks_err = enum "ks_err" [
                      KS_ERR_OK, ks_err_ok;
                      KS_ERR_NOMEM, ks_err_nomem;
                      KS_ERR_ARCH,   ks_err_arch;
                      KS_ERR_HANDLE,   ks_err_handle;
                      KS_ERR_MODE,   ks_err_mode;
                      KS_ERR_VERSION,   ks_err_version;
                      KS_ERR_OPT_INVALID,   ks_err_opt_invalid;
                      KS_ERR_ASM_EXPR_TOKEN,   ks_err_asm_expr_token;
                      KS_ERR_ASM_DIRECTIVE_VALUE_RANGE,   ks_err_asm_directive_value_range;
                      KS_ERR_ASM_DIRECTIVE_ID,   ks_err_asm_directive_id;
                      KS_ERR_ASM_DIRECTIVE_TOKEN,   ks_err_asm_directive_token;
                      KS_ERR_ASM_DIRECTIVE_STR,   ks_err_asm_directive_str;
                      KS_ERR_ASM_DIRECTIVE_COMMA,   ks_err_asm_directive_comma;
                      KS_ERR_ASM_DIRECTIVE_RELOC_NAME,   ks_err_asm_directive_reloc_name;
                      KS_ERR_ASM_DIRECTIVE_RELOC_TOKEN,   ks_err_asm_directive_reloc_token;
                      KS_ERR_ASM_DIRECTIVE_FPOINT,   ks_err_asm_directive_fpoint;
                      KS_ERR_ASM_DIRECTIVE_UNKNOWN,   ks_err_asm_directive_unknown;
                      KS_ERR_ASM_VARIANT_INVALID,   ks_err_asm_variant_invalid;
                      KS_ERR_ASM_DIRECTIVE_EQU,   ks_err_asm_directive_equ;
                      KS_ERR_ASM_EXPR_BRACKET,   ks_err_asm_expr_bracket;
                      KS_ERR_ASM_SYMBOL_MODIFIER,   ks_err_asm_symbol_modifier;
                      KS_ERR_ASM_SYMBOL_REDEFINED,   ks_err_asm_symbol_redefined;
                      KS_ERR_ASM_SYMBOL_MISSING,   ks_err_asm_symbol_missing;
                      KS_ERR_ASM_RPAREN,   ks_err_asm_rparen;
                      KS_ERR_ASM_STAT_TOKEN,   ks_err_asm_stat_token;
                      KS_ERR_ASM_UNSUPPORTED,   ks_err_asm_unsupported;
                      KS_ERR_ASM_MACRO_TOKEN,   ks_err_asm_macro_token;
                      KS_ERR_ASM_MACRO_PAREN,   ks_err_asm_macro_paren;
                      KS_ERR_ASM_MACRO_EQU,   ks_err_asm_macro_equ;
                      KS_ERR_ASM_MACRO_ARGS,   ks_err_asm_macro_args;
                      KS_ERR_ASM_MACRO_LEVELS_EXCEED,   ks_err_asm_macro_levels_exceed;
                      KS_ERR_ASM_MACRO_STR,   ks_err_asm_macro_str;
                      KS_ERR_ASM_ESC_BACKSLASH,   ks_err_asm_esc_backslash;
                      KS_ERR_ASM_ESC_OCTAL,   ks_err_asm_esc_octal;
                      KS_ERR_ASM_ESC_SEQUENCE,   ks_err_asm_esc_sequence;
                      KS_ERR_ASM_ESC_STR ,  ks_err_asm_esc_str ;
                      KS_ERR_ASM_TOKEN_INVALID,   ks_err_asm_token_invalid;
                      KS_ERR_ASM_INSN_UNSUPPORTED,   ks_err_asm_insn_unsupported;
                      KS_ERR_ASM_FIXUP_INVALID ,   ks_err_asm_fixup_invalid ;
                      KS_ERR_ASM_LABEL_INVALID ,   ks_err_asm_label_invalid ;
                      KS_ERR_ASM_FRAGMENT_INVALID,   ks_err_asm_fragment_invalid;
                      KS_ERR_ASM_INVALIDOPERAND,   ks_err_asm_invalidoperand;
                      KS_ERR_ASM_MISSINGFEATURE,   ks_err_asm_missingfeature;
                      KS_ERR_ASM_MNEMONICFAIL ,   ks_err_asm_mnemonicfail 
                    ]




    let constant_test = 1

    let ks_arch_arm =  constant "KS_ARCH_ARM" int64_t
    let ks_arch_arm64 =  constant "KS_ARCH_ARM64"  int64_t
    let ks_arch_mips =  constant "KS_ARCH_MIPS" int64_t
    let ks_arch_x86 =  constant "KS_ARCH_X86" int64_t
    let ks_arch_ppc =  constant "KS_ARCH_PPC" int64_t
    let ks_arch_sparc =  constant "KS_ARCH_SPARC" int64_t
    let ks_arch_systemz =  constant "KS_ARCH_SYSTEMZ" int64_t
    let ks_arch_hexagon =  constant "KS_ARCH_HEXAGON" int64_t
    let ks_arch_max =  constant "KS_ARCH_MAX" int64_t

    let ks_arch = enum "ks_arch" [
                         KS_ARCH_ARM, ks_arch_arm;
                         KS_ARCH_ARM64, ks_arch_arm64;
                         KS_ARCH_MIPS, ks_arch_mips;
                         KS_ARCH_X86, ks_arch_x86;
                         KS_ARCH_PPC, ks_arch_ppc;
                         KS_ARCH_SPARC, ks_arch_sparc;
                         KS_ARCH_SYSTEMZ, ks_arch_systemz;
                         KS_ARCH_HEXAGON, ks_arch_hexagon;
                         KS_ARCH_MAX, ks_arch_max
                       ]
                                

    let ks_api_major = constant "KS_API_MAJOR" int
    let ks_api_minor = constant "KS_API_MINOR" int



    let ks_err_ok = constant "KS_ERR_OK" int64_t
    let ks_err_nomem = constant "KS_ERR_NOMEM" int64_t
    let ks_err_arch = constant "KS_ERR_ARCH" int64_t
    let ks_err_handle = constant "KS_ERR_HANDLE" int64_t
    let ks_err_mode = constant "KS_ERR_MODE" int64_t
    let ks_err_version = constant "KS_ERR_VERSION" int64_t
    let ks_err_opt_invalid = constant "KS_ERR_OPT_INVALID" int64_t
    let ks_err_asm_expr_token = constant "KS_ERR_ASM_EXPR_TOKEN" int64_t
    let ks_err_asm_directive_value_range = constant "KS_ERR_ASM_DIRECTIVE_VALUE_RANGE" int64_t
    let ks_err_asm_directive_id = constant "KS_ERR_ASM_DIRECTIVE_ID" int64_t
    let ks_err_asm_directive_token = constant "KS_ERR_ASM_DIRECTIVE_TOKEN" int64_t
    let ks_err_asm_directive_str = constant "KS_ERR_ASM_DIRECTIVE_STR" int64_t
    let ks_err_asm_directive_comma = constant "KS_ERR_ASM_DIRECTIVE_COMMA" int64_t
    let ks_err_asm_directive_reloc_name = constant "KS_ERR_ASM_DIRECTIVE_RELOC_NAME" int64_t
    let ks_err_asm_directive_reloc_token = constant "KS_ERR_ASM_DIRECTIVE_RELOC_TOKEN" int64_t
    let ks_err_asm_directive_fpoint = constant "KS_ERR_ASM_DIRECTIVE_FPOINT" int64_t
    let ks_err_asm_directive_unknown = constant "KS_ERR_ASM_DIRECTIVE_UNKNOWN" int64_t
    let ks_err_asm_variant_invalid = constant "KS_ERR_ASM_VARIANT_INVALID" int64_t
    let ks_err_asm_directive_equ = constant "KS_ERR_ASM_DIRECTIVE_EQU" int64_t
    let ks_err_asm_expr_bracket = constant "KS_ERR_ASM_EXPR_BRACKET" int64_t
    let ks_err_asm_symbol_modifier = constant "KS_ERR_ASM_SYMBOL_MODIFIER" int64_t
    let ks_err_asm_symbol_redefined = constant "KS_ERR_ASM_SYMBOL_REDEFINED" int64_t
    let ks_err_asm_symbol_missing = constant "KS_ERR_ASM_SYMBOL_MISSING" int64_t
    let ks_err_asm_rparen = constant "KS_ERR_ASM_RPAREN" int64_t
    let ks_err_asm_stat_token = constant "KS_ERR_ASM_STAT_TOKEN" int64_t
    let ks_err_asm_unsupported = constant "KS_ERR_ASM_UNSUPPORTED" int64_t
    let ks_err_asm_macro_token = constant "KS_ERR_ASM_MACRO_TOKEN" int64_t
    let ks_err_asm_macro_paren = constant "KS_ERR_ASM_MACRO_PAREN" int64_t
    let ks_err_asm_macro_equ = constant "KS_ERR_ASM_MACRO_EQU"  int64_t
    let ks_err_asm_macro_args = constant "KS_ERR_ASM_MACRO_ARGS"  int64_t
    let ks_err_asm_macro_levels_exceed = constant "KS_ERR_ASM_MACRO_LEVELS_EXCEED" int64_t
    let ks_err_asm_macro_str = constant "KS_ERR_ASM_MACRO_STR" int64_t
    let ks_err_asm_esc_backslash = constant "KS_ERR_ASM_ESC_BACKSLASH" int64_t
    let ks_err_asm_esc_octal = constant "KS_ERR_ASM_ESC_OCTAL"  int64_t
    let ks_err_asm_esc_sequence = constant "KS_ERR_ASM_ESC_SEQUENCE" int64_t
    let ks_err_asm_esc_str = constant "KS_ERR_ASM_ESC_STR" int64_t
    let ks_err_asm_token_invalid = constant "KS_ERR_ASM_TOKEN_INVALID" int64_t
    let ks_err_asm_insn_unsupported = constant "KS_ERR_ASM_INSN_UNSUPPORTED" int64_t
    let ks_err_asm_fixup_invalid = constant "KS_ERR_ASM_FIXUP_INVALID" int64_t
    let ks_err_asm_label_invalid = constant "KS_ERR_ASM_LABEL_INVALID" int64_t
    let ks_err_asm_fragment_invalid = constant "KS_ERR_ASM_FRAGMENT_INVALID" int64_t
    let ks_err_asm_invalidoperand = constant "KS_ERR_ASM_INVALIDOPERAND" int64_t
    let ks_err_asm_missingfeature = constant "KS_ERR_ASM_MISSINGFEATURE" int64_t
    let ks_err_asm_mnemonicfail = constant "KS_ERR_ASM_MNEMONICFAIL" int64_t


                                           
   let ks_mode_little_endian = constant "KS_MODE_LITTLE_ENDIAN" int64_t 
   let ks_mode_big_endian  = constant "KS_MODE_BIG_ENDIAN" int64_t 
   let ks_mode_arm = constant "KS_MODE_ARM" int64_t
   let ks_mode_thumb  = constant "KS_MODE_THUMB" int64_t
   let ks_mode_v8 = constant "KS_MODE_V8" int64_t
   let ks_mode_micro = constant "KS_MODE_MICRO" int64_t
   let ks_mode_mips3 = constant "KS_MODE_MIPS3" int64_t
   let ks_mode_mips32r6 = constant "KS_MODE_MIPS32R6" int64_t
   let ks_mode_mips32 = constant "KS_MODE_MIPS32" int64_t
   let ks_mode_mips64 = constant "KS_MODE_MIPS64" int64_t
   let ks_mode_16 = constant "KS_MODE_16" int64_t
   let ks_mode_32 = constant "KS_MODE_32" int64_t
   let ks_mode_64 = constant "KS_MODE_64" int64_t
   let ks_mode_ppc32 = constant "KS_MODE_PPC32" int64_t
   let ks_mode_ppc64 = constant "KS_MODE_PPC64" int64_t
   let ks_mode_qpx = constant "KS_MODE_QPX" int64_t
   let ks_mode_sparc32 = constant "KS_MODE_SPARC32" int64_t
   let ks_mode_sparc64 = constant "KS_MODE_SPARC64" int64_t
   let ks_mode_v9 = constant "KS_MODE_V9" int64_t

                                           


  end
       
module Bindings (F: Cstubs.FOREIGN)  =
  struct
    open F

    
    let ks_arch_supported = foreign "ks_arch_supported" (int64_t @-> returning bool)

    let ks_version = foreign "ks_version" (ptr int @-> ptr int @-> returning int)


  end
