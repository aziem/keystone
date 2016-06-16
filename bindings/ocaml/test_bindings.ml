open Keystone
open Keystone.Types

let test_ks arch mode ?(syntax=KS_OPT_SYNTAX_INTEL) ?(endian=KS_MODE_LITTLE_ENDIAN) asm  =
  match (ks_open arch ~endian:endian mode) with
  | KSOpenSucc engine ->
     begin
       ignore(ks_option engine KS_OPT_SYNTAX syntax);
       match (ks_asm engine asm 0) with
       | ASMSuccess(asm',encsize,stat) ->
          Printf.printf "%s = %s\nAssembled: %i bytes, %i statements\n \n" asm (asm_array_to_string asm') encsize stat;
          ignore(ks_close engine)

       | ASMError s -> Printf.printf "ERROR: failed on ks_asm with: %s\n" s
     end

  | KSOpenError e -> Printf.printf "ERROR: failed on ks_open: %s\n" e

  
let _ =
  test_ks KS_ARCH_X86 KS_MODE_16 "add eax, ecx";
  test_ks KS_ARCH_X86 KS_MODE_32 "add eax, ecx";
  test_ks KS_ARCH_X86 KS_MODE_64 "add rax, rcx";
  test_ks KS_ARCH_X86 KS_MODE_32 ~syntax:KS_OPT_SYNTAX_ATT "add %ecx, %eax";
  test_ks KS_ARCH_X86 KS_MODE_64 ~syntax:KS_OPT_SYNTAX_ATT "add %rcx, %rax";

  test_ks KS_ARCH_ARM KS_MODE_ARM  "sub r1, r2, r5";
  test_ks KS_ARCH_ARM KS_MODE_ARM  "sub r1, r2, r5";
  test_ks KS_ARCH_ARM KS_MODE_ARM ~endian:KS_MODE_BIG_ENDIAN "sub r1, r2, r5";
  test_ks KS_ARCH_ARM KS_MODE_THUMB "movs r4, #0xf0";
  test_ks KS_ARCH_ARM KS_MODE_THUMB ~endian:KS_MODE_BIG_ENDIAN "movs r4, #0xf0";

  test_ks KS_ARCH_ARM64 KS_MODE_LITTLE_ENDIAN "ldr w1, [sp, #0x8]";

  test_ks KS_ARCH_HEXAGON KS_MODE_BIG_ENDIAN "v23.w=vavg(v11.w,v2.w):rnd";

  test_ks KS_ARCH_MIPS KS_MODE_MIPS32 "and $9, $6, $7";
  test_ks KS_ARCH_MIPS KS_MODE_MIPS32 ~endian:KS_MODE_BIG_ENDIAN "and $9, $6, $7";

  test_ks KS_ARCH_MIPS KS_MODE_MIPS64 "and $9, $6, $7";
  test_ks KS_ARCH_MIPS KS_MODE_MIPS64 ~endian:KS_MODE_BIG_ENDIAN "and $9, $6, $7";

  test_ks KS_ARCH_PPC KS_MODE_PPC32 ~endian:KS_MODE_BIG_ENDIAN "add 1,2,3";
  test_ks KS_ARCH_PPC KS_MODE_PPC64 "add 1,2,3";
  test_ks KS_ARCH_PPC KS_MODE_PPC64 ~endian:KS_MODE_BIG_ENDIAN "add 1,2,3";

  test_ks KS_ARCH_SPARC KS_MODE_SPARC32 "add %g1, %g2, %g3";
  test_ks KS_ARCH_SPARC KS_MODE_SPARC32 ~endian:KS_MODE_BIG_ENDIAN "add %g1, %g2, %g3";

  test_ks KS_ARCH_SYSTEMZ KS_MODE_BIG_ENDIAN "a %r0, 4092(%r15, %r1)";
  ()
    
