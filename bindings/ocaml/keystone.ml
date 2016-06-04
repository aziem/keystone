module B = Ffi_bindings.Bindings(Ffi_generated)
module T = Ffi_bindings.Types(Ffi_generated_types)

open Ctypes

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

let arch_of_ks_arch = function
  | KS_ARCH_ARM -> T.ks_arch_arm
  | KS_ARCH_ARM64 -> T.ks_arch_arm64
  | KS_ARCH_MIPS -> T.ks_arch_mips
  | KS_ARCH_X86 -> T.ks_arch_x86
  | KS_ARCH_PPC -> T.ks_arch_ppc
  | KS_ARCH_SPARC -> T.ks_arch_sparc
  | KS_ARCH_SYSTEMZ -> T.ks_arch_systemz
  | KS_ARCH_HEXAGON -> T.ks_arch_hexagon
  | KS_ARCH_MAX -> T.ks_arch_max
      

let ks_arch_supported arch =
  B.ks_arch_supported (arch_of_ks_arch arch)
 
