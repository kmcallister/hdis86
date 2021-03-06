{-# OPTIONS_HADDOCK
    hide #-}
{-# LANGUAGE
    DeriveDataTypeable #-}
module Hdis86.Internal.Opcode where
-- generated by gen_opcode.sh, do not exit

import Data.Typeable ( Typeable )
import Data.Data ( Data )

data Opcode
 = I3dnow
 | Iaaa
 | Iaad
 | Iaam
 | Iaas
 | Iadc
 | Iadd
 | Iaddpd
 | Iaddps
 | Iaddsd
 | Iaddss
 | Iaddsubpd
 | Iaddsubps
 | Iand
 | Iandpd
 | Iandps
 | Iandnpd
 | Iandnps
 | Iarpl
 | Imovsxd
 | Ibound
 | Ibsf
 | Ibsr
 | Ibswap
 | Ibt
 | Ibtc
 | Ibtr
 | Ibts
 | Icall
 | Icbw
 | Icwde
 | Icdqe
 | Iclc
 | Icld
 | Iclflush
 | Iclgi
 | Icli
 | Iclts
 | Icmc
 | Icmovo
 | Icmovno
 | Icmovb
 | Icmovae
 | Icmovz
 | Icmovnz
 | Icmovbe
 | Icmova
 | Icmovs
 | Icmovns
 | Icmovp
 | Icmovnp
 | Icmovl
 | Icmovge
 | Icmovle
 | Icmovg
 | Icmp
 | Icmppd
 | Icmpps
 | Icmpsb
 | Icmpsw
 | Icmpsd
 | Icmpsq
 | Icmpss
 | Icmpxchg
 | Icmpxchg8b
 | Icomisd
 | Icomiss
 | Icpuid
 | Icvtdq2pd
 | Icvtdq2ps
 | Icvtpd2dq
 | Icvtpd2pi
 | Icvtpd2ps
 | Icvtpi2ps
 | Icvtpi2pd
 | Icvtps2dq
 | Icvtps2pi
 | Icvtps2pd
 | Icvtsd2si
 | Icvtsd2ss
 | Icvtsi2ss
 | Icvtss2si
 | Icvtss2sd
 | Icvttpd2pi
 | Icvttpd2dq
 | Icvttps2dq
 | Icvttps2pi
 | Icvttsd2si
 | Icvtsi2sd
 | Icvttss2si
 | Icwd
 | Icdq
 | Icqo
 | Idaa
 | Idas
 | Idec
 | Idiv
 | Idivpd
 | Idivps
 | Idivsd
 | Idivss
 | Iemms
 | Ienter
 | If2xm1
 | Ifabs
 | Ifadd
 | Ifaddp
 | Ifbld
 | Ifbstp
 | Ifchs
 | Ifclex
 | Ifcmovb
 | Ifcmove
 | Ifcmovbe
 | Ifcmovu
 | Ifcmovnb
 | Ifcmovne
 | Ifcmovnbe
 | Ifcmovnu
 | Ifucomi
 | Ifcom
 | Ifcom2
 | Ifcomp3
 | Ifcomi
 | Ifucomip
 | Ifcomip
 | Ifcomp
 | Ifcomp5
 | Ifcompp
 | Ifcos
 | Ifdecstp
 | Ifdiv
 | Ifdivp
 | Ifdivr
 | Ifdivrp
 | Ifemms
 | Iffree
 | Iffreep
 | Ificom
 | Ificomp
 | Ifild
 | Ifncstp
 | Ifninit
 | Ifiadd
 | Ifidivr
 | Ifidiv
 | Ifisub
 | Ifisubr
 | Ifist
 | Ifistp
 | Ifisttp
 | Ifld
 | Ifld1
 | Ifldl2t
 | Ifldl2e
 | Ifldlpi
 | Ifldlg2
 | Ifldln2
 | Ifldz
 | Ifldcw
 | Ifldenv
 | Ifmul
 | Ifmulp
 | Ifimul
 | Ifnop
 | Ifpatan
 | Ifprem
 | Ifprem1
 | Ifptan
 | Ifrndint
 | Ifrstor
 | Ifnsave
 | Ifscale
 | Ifsin
 | Ifsincos
 | Ifsqrt
 | Ifstp
 | Ifstp1
 | Ifstp8
 | Ifstp9
 | Ifst
 | Ifnstcw
 | Ifnstenv
 | Ifnstsw
 | Ifsub
 | Ifsubp
 | Ifsubr
 | Ifsubrp
 | Iftst
 | Ifucom
 | Ifucomp
 | Ifucompp
 | Ifxam
 | Ifxch
 | Ifxch4
 | Ifxch7
 | Ifxrstor
 | Ifxsave
 | Ifpxtract
 | Ifyl2x
 | Ifyl2xp1
 | Ihaddpd
 | Ihaddps
 | Ihlt
 | Ihsubpd
 | Ihsubps
 | Iidiv
 | Iin
 | Iimul
 | Iinc
 | Iinsb
 | Iinsw
 | Iinsd
 | Iint1
 | Iint3
 | Iint
 | Iinto
 | Iinvd
 | Iinvlpg
 | Iinvlpga
 | Iiretw
 | Iiretd
 | Iiretq
 | Ijo
 | Ijno
 | Ijb
 | Ijae
 | Ijz
 | Ijnz
 | Ijbe
 | Ija
 | Ijs
 | Ijns
 | Ijp
 | Ijnp
 | Ijl
 | Ijge
 | Ijle
 | Ijg
 | Ijcxz
 | Ijecxz
 | Ijrcxz
 | Ijmp
 | Ilahf
 | Ilar
 | Ilddqu
 | Ildmxcsr
 | Ilds
 | Ilea
 | Iles
 | Ilfs
 | Ilgs
 | Ilidt
 | Ilss
 | Ileave
 | Ilfence
 | Ilgdt
 | Illdt
 | Ilmsw
 | Ilock
 | Ilodsb
 | Ilodsw
 | Ilodsd
 | Ilodsq
 | Iloopnz
 | Iloope
 | Iloop
 | Ilsl
 | Iltr
 | Imaskmovq
 | Imaxpd
 | Imaxps
 | Imaxsd
 | Imaxss
 | Imfence
 | Iminpd
 | Iminps
 | Iminsd
 | Iminss
 | Imonitor
 | Imov
 | Imovapd
 | Imovaps
 | Imovd
 | Imovddup
 | Imovdqa
 | Imovdqu
 | Imovdq2q
 | Imovhpd
 | Imovhps
 | Imovlhps
 | Imovlpd
 | Imovlps
 | Imovhlps
 | Imovmskpd
 | Imovmskps
 | Imovntdq
 | Imovnti
 | Imovntpd
 | Imovntps
 | Imovntq
 | Imovq
 | Imovqa
 | Imovq2dq
 | Imovsb
 | Imovsw
 | Imovsd
 | Imovsq
 | Imovsldup
 | Imovshdup
 | Imovss
 | Imovsx
 | Imovupd
 | Imovups
 | Imovzx
 | Imul
 | Imulpd
 | Imulps
 | Imulsd
 | Imulss
 | Imwait
 | Ineg
 | Inop
 | Inot
 | Ior
 | Iorpd
 | Iorps
 | Iout
 | Ioutsb
 | Ioutsw
 | Ioutsd
 | Ioutsq
 | Ipacksswb
 | Ipackssdw
 | Ipackuswb
 | Ipaddb
 | Ipaddw
 | Ipaddq
 | Ipaddsb
 | Ipaddsw
 | Ipaddusb
 | Ipaddusw
 | Ipand
 | Ipandn
 | Ipause
 | Ipavgb
 | Ipavgw
 | Ipcmpeqb
 | Ipcmpeqw
 | Ipcmpeqd
 | Ipcmpgtb
 | Ipcmpgtw
 | Ipcmpgtd
 | Ipextrw
 | Ipinsrw
 | Ipmaddwd
 | Ipmaxsw
 | Ipmaxub
 | Ipminsw
 | Ipminub
 | Ipmovmskb
 | Ipmulhuw
 | Ipmulhw
 | Ipmullw
 | Ipmuludq
 | Ipop
 | Ipopa
 | Ipopad
 | Ipopfw
 | Ipopfd
 | Ipopfq
 | Ipor
 | Iprefetch
 | Iprefetchnta
 | Iprefetcht0
 | Iprefetcht1
 | Iprefetcht2
 | Ipsadbw
 | Ipshufd
 | Ipshufhw
 | Ipshuflw
 | Ipshufw
 | Ipslldq
 | Ipsllw
 | Ipslld
 | Ipsllq
 | Ipsraw
 | Ipsrad
 | Ipsrlw
 | Ipsrld
 | Ipsrlq
 | Ipsrldq
 | Ipsubb
 | Ipsubw
 | Ipsubd
 | Ipsubq
 | Ipsubsb
 | Ipsubsw
 | Ipsubusb
 | Ipsubusw
 | Ipunpckhbw
 | Ipunpckhwd
 | Ipunpckhdq
 | Ipunpckhqdq
 | Ipunpcklbw
 | Ipunpcklwd
 | Ipunpckldq
 | Ipunpcklqdq
 | Ipi2fw
 | Ipi2fd
 | Ipf2iw
 | Ipf2id
 | Ipfnacc
 | Ipfpnacc
 | Ipfcmpge
 | Ipfmin
 | Ipfrcp
 | Ipfrsqrt
 | Ipfsub
 | Ipfadd
 | Ipfcmpgt
 | Ipfmax
 | Ipfrcpit1
 | Ipfrspit1
 | Ipfsubr
 | Ipfacc
 | Ipfcmpeq
 | Ipfmul
 | Ipfrcpit2
 | Ipmulhrw
 | Ipswapd
 | Ipavgusb
 | Ipush
 | Ipusha
 | Ipushad
 | Ipushfw
 | Ipushfd
 | Ipushfq
 | Ipxor
 | Ircl
 | Ircr
 | Irol
 | Iror
 | Ircpps
 | Ircpss
 | Irdmsr
 | Irdpmc
 | Irdtsc
 | Irdtscp
 | Irepne
 | Irep
 | Iret
 | Iretf
 | Irsm
 | Irsqrtps
 | Irsqrtss
 | Isahf
 | Isal
 | Isalc
 | Isar
 | Ishl
 | Ishr
 | Isbb
 | Iscasb
 | Iscasw
 | Iscasd
 | Iscasq
 | Iseto
 | Isetno
 | Isetb
 | Isetnb
 | Isetz
 | Isetnz
 | Isetbe
 | Iseta
 | Isets
 | Isetns
 | Isetp
 | Isetnp
 | Isetl
 | Isetge
 | Isetle
 | Isetg
 | Isfence
 | Isgdt
 | Ishld
 | Ishrd
 | Ishufpd
 | Ishufps
 | Isidt
 | Isldt
 | Ismsw
 | Isqrtps
 | Isqrtpd
 | Isqrtsd
 | Isqrtss
 | Istc
 | Istd
 | Istgi
 | Isti
 | Iskinit
 | Istmxcsr
 | Istosb
 | Istosw
 | Istosd
 | Istosq
 | Istr
 | Isub
 | Isubpd
 | Isubps
 | Isubsd
 | Isubss
 | Iswapgs
 | Isyscall
 | Isysenter
 | Isysexit
 | Isysret
 | Itest
 | Iucomisd
 | Iucomiss
 | Iud2
 | Iunpckhpd
 | Iunpckhps
 | Iunpcklps
 | Iunpcklpd
 | Iverr
 | Iverw
 | Ivmcall
 | Ivmclear
 | Ivmxon
 | Ivmptrld
 | Ivmptrst
 | Ivmresume
 | Ivmxoff
 | Ivmrun
 | Ivmmcall
 | Ivmload
 | Ivmsave
 | Iwait
 | Iwbinvd
 | Iwrmsr
 | Ixadd
 | Ixchg
 | Ixlatb
 | Ixor
 | Ixorpd
 | Ixorps
 | Idb
 | Iinvalid
 | Id3vil
 | Ina
 | Igrp_reg
 | Igrp_rm
 | Igrp_vendor
 | Igrp_x87
 | Igrp_mode
 | Igrp_osize
 | Igrp_asize
 | Igrp_mod
 | Inone
 deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Data)
