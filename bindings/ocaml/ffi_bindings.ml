open Ctypes
module Types (F: Cstubs.Types.TYPE) =
  struct

    open F
 
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
    let ks_api_major = constant "KS_API_MAJOR" int
    let ks_api_minor = constant "KS_API_MINOR" int
  end
       
module Bindings (F: Cstubs.FOREIGN)  =
  struct
    open F
    let ks_arch_supported = foreign "ks_arch_supported" (int64_t @-> returning bool) 
  end
