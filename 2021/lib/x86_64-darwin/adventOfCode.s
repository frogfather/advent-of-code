# Begin asmlist al_begin

.section __DWARF,__debug_line,regular,debug
Ldebug_linesection0:
Ldebug_line0:

.section __DWARF,__debug_abbrev,regular,debug
Ldebug_abbrevsection0:
Ldebug_abbrev0:

.text
L_DEBUGSTART_$P$ADVENTOFCODE:
# End asmlist al_begin
# Begin asmlist al_procedures

.text
	.align 3
.globl	_main
_main:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-32(%rsp),%rsp
	movl	%edi,-8(%rbp)
	movq	%rsi,-16(%rbp)
	movq	%rdx,-24(%rbp)
	movq	%rdx,%rax
	movq	-16(%rbp),%rsi
	movl	-8(%rbp),%edi
	movq	%rax,%rdx
	call	_FPC_SYSTEMMAIN
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt1:
Ll1:

.text
	.align 3
.globl	_PASCALMAIN
_PASCALMAIN:
Ll2:
	pushq	%rbp
	movq	%rsp,%rbp
	call	fpc_initializeunits
Ll3:
	movq	_TC_$FORMS_$$_REQUIREDERIVEDFORMRESOURCE@GOTPCREL(%rip),%rax
	movb	$1,(%rax)
Ll4:
	movq	_TC_$FORMS_$$_APPLICATION@GOTPCREL(%rip),%rax
	movq	(%rax),%rax
	movb	$1,568(%rax)
Ll5:
	movq	_TC_$FORMS_$$_APPLICATION@GOTPCREL(%rip),%rax
	movq	(%rax),%rdi
	movq	_TC_$FORMS_$$_APPLICATION@GOTPCREL(%rip),%rax
	movq	(%rax),%rax
	movq	(%rax),%rax
	call	*488(%rax)
Ll6:
	movq	_U_$ADVENT_$$_MAINFORM@GOTPCREL(%rip),%rdx
	movq	_TC_$FORMS_$$_APPLICATION@GOTPCREL(%rip),%rax
	movq	(%rax),%rdi
	movq	_VMT_$ADVENT_$$_TMAINFORM@GOTPCREL(%rip),%rsi
	call	_FORMS$_$TAPPLICATION_$__$$_CREATEFORM$TCOMPONENTCLASS$formal
Ll7:
	movq	_U_$PAINTBOX_$$_PAINTBOXFORM@GOTPCREL(%rip),%rdx
	movq	_TC_$FORMS_$$_APPLICATION@GOTPCREL(%rip),%rax
	movq	(%rax),%rdi
	movq	_VMT_$PAINTBOX_$$_TPAINTBOXFORM@GOTPCREL(%rip),%rsi
	call	_FORMS$_$TAPPLICATION_$__$$_CREATEFORM$TCOMPONENTCLASS$formal
Ll8:
	movq	_TC_$FORMS_$$_APPLICATION@GOTPCREL(%rip),%rax
	movq	(%rax),%rdi
	call	_FORMS$_$TAPPLICATION_$__$$_RUN
Ll9:
	call	fpc_do_exit
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt2:
Ll10:
# End asmlist al_procedures
# Begin asmlist al_globals

.data
	.align 3
.globl	INITFINAL
INITFINAL:
	.quad	77,0
	.quad	_INIT$_$SYSTEM
	.quad	0
	.quad	_INIT$_$LNFODWRF
	.quad	_FINALIZE$_$LNFODWRF
	.quad	_INIT$_$FPINTRES
	.quad	0,0
	.quad	_FINALIZE$_$OBJPAS
	.quad	_INIT$_$UNIX
	.quad	_FINALIZE$_$UNIX
	.quad	_INIT$_$DL
	.quad	0
	.quad	_INIT$_$DYNLIBS
	.quad	0
	.quad	_INIT$_$UNIXCP
	.quad	_FINALIZE$_$UNIXCP
	.quad	_INIT$_$CWSTRING
	.quad	_FINALIZE$_$CWSTRING
	.quad	_INIT$_$SYSUTILS
	.quad	_FINALIZE$_$SYSUTILS
	.quad	_INIT$_$TYPINFO
	.quad	_FINALIZE$_$TYPINFO
	.quad	_INIT$_$CLASSES
	.quad	_FINALIZE$_$CLASSES
	.quad	_INIT$_$FPIMAGE
	.quad	_FINALIZE$_$FPIMAGE
	.quad	_INIT$_$FPCADDS
	.quad	0,0
	.quad	_FINALIZE$_$GETTEXT
	.quad	_INIT$_$MACPAS
	.quad	0
	.quad	_INIT$_$LAZUTF8
	.quad	_FINALIZE$_$LAZUTF8
	.quad	_INIT$_$LCLTYPE
	.quad	_FINALIZE$_$LCLTYPE
	.quad	_INIT$_$LAZ_AVL_TREE
	.quad	_FINALIZE$_$LAZ_AVL_TREE
	.quad	_INIT$_$LAZFILEUTILS
	.quad	_FINALIZE$_$LAZFILEUTILS
	.quad	0
	.quad	_FINALIZE$_$LAZLOGGERBASE
	.quad	0
	.quad	_FINALIZE$_$LAZTRACER
	.quad	_INIT$_$LCLPROC
	.quad	_FINALIZE$_$LCLPROC
	.quad	_INIT$_$GRAPHTYPE
	.quad	0
	.quad	_INIT$_$LCLPLATFORMDEF
	.quad	_FINALIZE$_$LCLPLATFORMDEF
	.quad	_INIT$_$FPIMGCMN
	.quad	0
	.quad	_INIT$_$FPREADBMP
	.quad	0
	.quad	_INIT$_$FPWRITEBMP
	.quad	0
	.quad	_INIT$_$FPREADPNG
	.quad	0
	.quad	_INIT$_$FPWRITEPNG
	.quad	0
	.quad	_INIT$_$FPREADTIFF
	.quad	0
	.quad	_INIT$_$FPWRITETIFF
	.quad	0
	.quad	_INIT$_$LCLVERSION
	.quad	0
	.quad	_INIT$_$FPCANVAS
	.quad	_FINALIZE$_$FPCANVAS
	.quad	_INIT$_$FPREADPNM
	.quad	0
	.quad	_INIT$_$FPWRITEPNM
	.quad	0
	.quad	_INIT$_$FPREADJPEG
	.quad	0
	.quad	_INIT$_$FPWRITEJPEG
	.quad	0
	.quad	_INIT$_$FPREADGIF
	.quad	0
	.quad	_INIT$_$VARIANTS
	.quad	_FINALIZE$_$VARIANTS
	.quad	_INIT$_$LRESOURCES
	.quad	_FINALIZE$_$LRESOURCES
	.quad	_INIT$_$FILEUTIL
	.quad	_FINALIZE$_$FILEUTIL
	.quad	_INIT$_$PROCESS
	.quad	_FINALIZE$_$PROCESS
	.quad	_INIT$_$THEMES
	.quad	_FINALIZE$_$THEMES
	.quad	_INIT$_$LCLINTF
	.quad	_FINALIZE$_$LCLINTF
	.quad	_INIT$_$GRAPHICS
	.quad	_FINALIZE$_$GRAPHICS
	.quad	_INIT$_$INTFGRAPHICS
	.quad	0,0
	.quad	_FINALIZE$_$WSLCLCLASSES
	.quad	_INIT$_$FPPIXLCANV
	.quad	_FINALIZE$_$FPPIXLCANV
	.quad	_INIT$_$RTTIUTILS
	.quad	_FINALIZE$_$RTTIUTILS
	.quad	_INIT$_$ACTNLIST
	.quad	0
	.quad	_INIT$_$LAZLOGGER
	.quad	0,0
	.quad	_FINALIZE$_$WSMENUS
	.quad	_INIT$_$CLIPBRD
	.quad	_FINALIZE$_$CLIPBRD
	.quad	_INIT$_$FORMS
	.quad	_FINALIZE$_$FORMS
	.quad	_INIT$_$MENUS
	.quad	_FINALIZE$_$MENUS
	.quad	_INIT$_$IMGLIST
	.quad	0
	.quad	_INIT$_$CONTROLS
	.quad	_FINALIZE$_$CONTROLS
	.quad	_INIT$_$STDCTRLS
	.quad	0
	.quad	_INIT$_$EXTCTRLS
	.quad	_FINALIZE$_$EXTCTRLS
	.quad	_INIT$_$BUTTONS
	.quad	_FINALIZE$_$BUTTONS
	.quad	_INIT$_$COMCTRLS
	.quad	_FINALIZE$_$COMCTRLS
	.quad	0
	.quad	_FINALIZE$_$CUSTOMDRAWNDRAWERS
	.quad	_INIT$_$CUSTOMDRAWN_COMMON
	.quad	0
	.quad	_INIT$_$CUSTOMDRAWN_MAC
	.quad	0,0
	.quad	_FINALIZE$_$COCOACARET
	.quad	_INIT$_$COCOAWSCLIPBOARD
	.quad	0
	.quad	_INIT$_$LCLTASKDIALOG
	.quad	_FINALIZE$_$LCLTASKDIALOG
	.quad	_INIT$_$DIALOGS
	.quad	_FINALIZE$_$DIALOGS
	.quad	_INIT$_$COCOAWSCOMMON
	.quad	_FINALIZE$_$COCOAWSCOMMON
	.quad	0
	.quad	_FINALIZE$_$COCOAINT
	.quad	_INIT$_$INTERFACES
	.quad	_FINALIZE$_$INTERFACES
	.quad	_INIT$_$FPJSON
	.quad	_FINALIZE$_$FPJSON
	.quad	_INIT$_$ARRAYUTILS
	.quad	_FINALIZE$_$ARRAYUTILS
	.quad	_INIT$_$DATEUTILS
	.quad	_FINALIZE$_$DATEUTILS
	.quad	_INIT$_$AOCUTILS
	.quad	_FINALIZE$_$AOCUTILS
	.quad	_INIT$_$ADVENT
	.quad	_FINALIZE$_$ADVENT

.data
	.align 3
.globl	FPC_THREADVARTABLES
FPC_THREADVARTABLES:
	.long	3
	.quad	_THREADVARLIST_$SYSTEM$indirect
	.quad	_THREADVARLIST_$CWSTRING$indirect
	.quad	_THREADVARLIST_$CLASSES$indirect

.const_data
	.align 3
.globl	FPC_RESOURCESTRINGTABLES
FPC_RESOURCESTRINGTABLES:
	.quad	15
	.quad	_RESSTR_$MATH_$$_START$indirect
	.quad	_RESSTR_$MATH_$$_END$indirect
	.quad	_RESSTR_$SYSCONST_$$_START$indirect
	.quad	_RESSTR_$SYSCONST_$$_END$indirect
	.quad	_RESSTR_$TYPINFO_$$_START$indirect
	.quad	_RESSTR_$TYPINFO_$$_END$indirect
	.quad	_RESSTR_$RTLCONSTS_$$_START$indirect
	.quad	_RESSTR_$RTLCONSTS_$$_END$indirect
	.quad	_RESSTR_$STRUTILS_$$_START$indirect
	.quad	_RESSTR_$STRUTILS_$$_END$indirect
	.quad	_RESSTR_$LCLSTRCONSTS_$$_START$indirect
	.quad	_RESSTR_$LCLSTRCONSTS_$$_END$indirect
	.quad	_RESSTR_$LAZUTILSSTRCONSTS_$$_START$indirect
	.quad	_RESSTR_$LAZUTILSSTRCONSTS_$$_END$indirect
	.quad	_RESSTR_$ZSTREAM_$$_START$indirect
	.quad	_RESSTR_$ZSTREAM_$$_END$indirect
	.quad	_RESSTR_$ZBASE_$$_START$indirect
	.quad	_RESSTR_$ZBASE_$$_END$indirect
	.quad	_RESSTR_$CONTNRS_$$_START$indirect
	.quad	_RESSTR_$CONTNRS_$$_END$indirect
	.quad	_RESSTR_$VARIANTS_$$_START$indirect
	.quad	_RESSTR_$VARIANTS_$$_END$indirect
	.quad	_RESSTR_$SYNCOBJS_$$_START$indirect
	.quad	_RESSTR_$SYNCOBJS_$$_END$indirect
	.quad	_RESSTR_$PROCESS_$$_START$indirect
	.quad	_RESSTR_$PROCESS_$$_END$indirect
	.quad	_RESSTR_$CUSTAPP_$$_START$indirect
	.quad	_RESSTR_$CUSTAPP_$$_END$indirect
	.quad	_RESSTR_$FPJSON_$$_START$indirect
	.quad	_RESSTR_$FPJSON_$$_END$indirect

.data
	.align 3
.globl	FPC_WIDEINITTABLES
FPC_WIDEINITTABLES:
	.quad	0

.data
	.align 3
.globl	FPC_RESSTRINITTABLES
FPC_RESSTRINITTABLES:
	.quad	0

.section __TEXT, .fpc, regular, no_dead_strip
	.align 4
.reference __fpc_ident
__fpc_ident:
	.ascii	"FPC 3.2.0 [2020/05/31] for x86_64 - Darwin"

.data
	.align 3
.globl	__stklen
__stklen:
	.quad	262144

.data
	.align 3
.globl	__heapsize
__heapsize:
	.quad	0

.data
	.align 3
.globl	__fpc_valgrind
__fpc_valgrind:
	.byte	0

.const_data
	.align 3
.globl	FPC_RESLOCATION
FPC_RESLOCATION:
	.quad	FPC_RESSYMBOL
# End asmlist al_globals
# Begin asmlist al_dwarf_info

.section __DWARF,__debug_info,regular,debug
Ldebug_info0:
	.long	L$set$1
	.set L$set$1,Ledebug_info0-Lf1
Lf1:
	.short	3
	.long	L$set$2
	.set L$set$2,Ldebug_abbrev0-Ldebug_abbrevsection0
	.byte	8
	.byte	1
	.ascii	"adventOfCode.lpr\000"
	.ascii	"Free Pascal 3.2.0 2020/05/31\000"
	.ascii	"/Users/cloudsoft/Code/advent-of-code/2021/\000"
	.byte	9
	.byte	3
	.long	L$set$3
	.set L$set$3,Ldebug_line0-Ldebug_linesection0
	.quad	L_DEBUGSTART_$P$ADVENTOFCODE
	.quad	L_DEBUGEND_$P$ADVENTOFCODE
# Syms - Begin Staticsymtable
# Symbol SYSTEM
# Symbol LNFODWRF
# Symbol FPINTRES
# Symbol OBJPAS
# Symbol INTERFACES
# Symbol FORMS
# Symbol ADVENT
# Symbol BINGOCARD
# Symbol VENTMAP
# Symbol ARRAYUTILS
# Symbol ANYSORT
# Symbol PAINTBOX
# Symbol OCTOPUS
# Symbol ORIGAMI
# Symbol POLYMER
# Symbol CHITON
# Symbol ADVENTOFCODE
# Symbol main
# Symbol __FPC_IMPL_EXTERNAL_REDIRECT__FPC_SYSTEMMAIN
# Symbol PASCALMAIN
# Symbol SYSINIT
# Syms - End Staticsymtable
# Procdef $main(const LongInt;const ^^Char;const ^^Char); CDecl;
	.byte	2
	.ascii	"$main\000"
	.byte	1
	.byte	1
	.quad	_main
	.quad	Lt1
# Symbol ARGC
	.byte	3
	.ascii	"ARGC\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$4
	.set L$set$4,La1-Ldebug_info0
# Symbol ARGV
	.byte	3
	.ascii	"ARGV\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$5
	.set L$set$5,La3-Ldebug_info0
# Symbol ARGP
	.byte	3
	.ascii	"ARGP\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$6
	.set L$set$6,La3-Ldebug_info0
	.byte	0
# Procdef $PASCALMAIN; Register;
	.byte	4
	.ascii	"$PASCALMAIN\000"
	.byte	1
	.byte	65
	.byte	1
	.quad	_PASCALMAIN
	.quad	Lt2
	.byte	0
# Defs - Begin unit SYSTEM has index 1
# Definition LongInt
La1:
	.byte	5
	.ascii	"LongInt\000"
	.long	L$set$7
	.set L$set$7,La5-Ldebug_info0
La5:
	.byte	6
	.ascii	"LongInt\000"
	.byte	5
	.byte	4
La2:
	.byte	7
	.long	L$set$8
	.set L$set$8,La1-Ldebug_info0
# Defs - End unit SYSTEM has index 1
# Defs - Begin unit STRINGS has index 4
# Defs - End unit STRINGS has index 4
# Defs - Begin unit EXEINFO has index 3
# Defs - End unit EXEINFO has index 3
# Defs - Begin unit LNFODWRF has index 2
# Defs - End unit LNFODWRF has index 2
# Defs - Begin unit FPINTRES has index 5
# Defs - End unit FPINTRES has index 5
# Defs - Begin unit OBJPAS has index 6
# Defs - End unit OBJPAS has index 6
# Defs - Begin unit UNIXTYPE has index 24
# Defs - End unit UNIXTYPE has index 24
# Defs - Begin unit SYSCTL has index 28
# Defs - End unit SYSCTL has index 28
# Defs - Begin unit BASEUNIX has index 21
# Defs - End unit BASEUNIX has index 21
# Defs - Begin unit CTYPES has index 22
# Defs - End unit CTYPES has index 22
# Defs - Begin unit UNIXUTIL has index 29
# Defs - End unit UNIXUTIL has index 29
# Defs - Begin unit UNIX has index 23
# Defs - End unit UNIX has index 23
# Defs - Begin unit INITC has index 25
# Defs - End unit INITC has index 25
# Defs - Begin unit DL has index 30
# Defs - End unit DL has index 30
# Defs - Begin unit DYNLIBS has index 26
# Defs - End unit DYNLIBS has index 26
# Defs - Begin unit UNIXCP has index 27
# Defs - End unit UNIXCP has index 27
# Defs - Begin unit CWSTRING has index 19
# Defs - End unit CWSTRING has index 19
# Defs - Begin unit ERRORS has index 46
# Defs - End unit ERRORS has index 46
# Defs - Begin unit SYSCONST has index 47
# Defs - End unit SYSCONST has index 47
# Defs - Begin unit SYSUTILS has index 33
# Defs - End unit SYSUTILS has index 33
# Defs - Begin unit MATH has index 34
# Defs - End unit MATH has index 34
# Defs - Begin unit TYPES has index 31
# Defs - End unit TYPES has index 31
# Defs - Begin unit RTLCONSTS has index 49
# Defs - End unit RTLCONSTS has index 49
# Defs - Begin unit TYPINFO has index 48
# Defs - End unit TYPINFO has index 48
# Defs - Begin unit CLASSES has index 32
# Defs - End unit CLASSES has index 32
# Defs - Begin unit FPIMAGE has index 35
# Defs - End unit FPIMAGE has index 35
# Defs - Begin unit FPCADDS has index 50
# Defs - End unit FPCADDS has index 50
# Defs - Begin unit STRUTILS has index 51
# Defs - End unit STRUTILS has index 51
# Defs - Begin unit GETTEXT has index 52
# Defs - End unit GETTEXT has index 52
# Defs - Begin unit MACPAS has index 54
# Defs - End unit MACPAS has index 54
# Defs - Begin unit BLOCKRTL has index 55
# Defs - End unit BLOCKRTL has index 55
# Defs - Begin unit CONDITIONALMACROS has index 58
# Defs - End unit CONDITIONALMACROS has index 58
# Defs - Begin unit MACTYPES has index 56
# Defs - End unit MACTYPES has index 56
# Defs - Begin unit COREAUDIOTYPES has index 57
# Defs - End unit COREAUDIOTYPES has index 57
# Defs - Begin unit MACOSALL has index 53
# Defs - End unit MACOSALL has index 53
# Defs - Begin unit LAZUTF8 has index 36
# Defs - End unit LAZUTF8 has index 36
# Defs - Begin unit FGL has index 59
# Defs - End unit FGL has index 59
# Defs - Begin unit INTEGERLIST has index 37
# Defs - End unit INTEGERLIST has index 37
# Defs - Begin unit LCLTYPE has index 38
# Defs - End unit LCLTYPE has index 38
# Defs - Begin unit LAZ_AVL_TREE has index 60
# Defs - End unit LAZ_AVL_TREE has index 60
# Defs - Begin unit LAZUTILSSTRCONSTS has index 68
# Defs - End unit LAZUTILSSTRCONSTS has index 68
# Defs - Begin unit LAZFILEUTILS has index 61
# Defs - End unit LAZFILEUTILS has index 61
# Defs - Begin unit LAZUTILITIES has index 62
# Defs - End unit LAZUTILITIES has index 62
# Defs - Begin unit LAZMETHODLIST has index 63
# Defs - End unit LAZMETHODLIST has index 63
# Defs - Begin unit LAZUTF8CLASSES has index 64
# Defs - End unit LAZUTF8CLASSES has index 64
# Defs - Begin unit LAZCLASSES has index 69
# Defs - End unit LAZCLASSES has index 69
# Defs - Begin unit LAZLOGGERBASE has index 65
# Defs - End unit LAZLOGGERBASE has index 65
# Defs - Begin unit LAZTRACER has index 66
# Defs - End unit LAZTRACER has index 66
# Defs - Begin unit LCLSTRCONSTS has index 67
# Defs - End unit LCLSTRCONSTS has index 67
# Defs - Begin unit LCLPROC has index 39
# Defs - End unit LCLPROC has index 39
# Defs - Begin unit GRAPHTYPE has index 42
# Defs - End unit GRAPHTYPE has index 42
# Defs - Begin unit LMESSAGES has index 40
# Defs - End unit LMESSAGES has index 40
# Defs - Begin unit LCLPLATFORMDEF has index 41
# Defs - End unit LCLPLATFORMDEF has index 41
# Defs - Begin unit GRAPHMATH has index 43
# Defs - End unit GRAPHMATH has index 43
# Defs - Begin unit FPIMGCMN has index 80
# Defs - End unit FPIMGCMN has index 80
# Defs - Begin unit BMPCOMN has index 72
# Defs - End unit BMPCOMN has index 72
# Defs - Begin unit FPREADBMP has index 70
# Defs - End unit FPREADBMP has index 70
# Defs - Begin unit FPWRITEBMP has index 71
# Defs - End unit FPWRITEBMP has index 71
# Defs - Begin unit PNGCOMN has index 81
# Defs - End unit PNGCOMN has index 81
# Defs - Begin unit ZBASE has index 83
# Defs - End unit ZBASE has index 83
# Defs - Begin unit CRC has index 85
# Defs - End unit CRC has index 85
# Defs - Begin unit TREES has index 88
# Defs - End unit TREES has index 88
# Defs - Begin unit ADLER has index 89
# Defs - End unit ADLER has index 89
# Defs - Begin unit ZDEFLATE has index 86
# Defs - End unit ZDEFLATE has index 86
# Defs - Begin unit INFUTIL has index 91
# Defs - End unit INFUTIL has index 91
# Defs - Begin unit INFFAST has index 94
# Defs - End unit INFFAST has index 94
# Defs - Begin unit INFCODES has index 92
# Defs - End unit INFCODES has index 92
# Defs - Begin unit INFTREES has index 93
# Defs - End unit INFTREES has index 93
# Defs - Begin unit INFBLOCK has index 90
# Defs - End unit INFBLOCK has index 90
# Defs - Begin unit ZINFLATE has index 87
# Defs - End unit ZINFLATE has index 87
# Defs - Begin unit GZIO has index 84
# Defs - End unit GZIO has index 84
# Defs - Begin unit ZSTREAM has index 82
# Defs - End unit ZSTREAM has index 82
# Defs - Begin unit FPREADPNG has index 73
# Defs - End unit FPREADPNG has index 73
# Defs - Begin unit FPWRITEPNG has index 74
# Defs - End unit FPWRITEPNG has index 74
# Defs - Begin unit FPTIFFCMN has index 77
# Defs - End unit FPTIFFCMN has index 77
# Defs - Begin unit FPREADTIFF has index 75
# Defs - End unit FPREADTIFF has index 75
# Defs - Begin unit FPWRITETIFF has index 76
# Defs - End unit FPWRITETIFF has index 76
# Defs - Begin unit LAZVERSION has index 95
# Defs - End unit LAZVERSION has index 95
# Defs - Begin unit LCLVERSION has index 78
# Defs - End unit LCLVERSION has index 78
# Defs - Begin unit ICNSTYPES has index 79
# Defs - End unit ICNSTYPES has index 79
# Defs - Begin unit CONTNRS has index 98
# Defs - End unit CONTNRS has index 98
# Defs - Begin unit CLIPPING has index 108
# Defs - End unit CLIPPING has index 108
# Defs - Begin unit FPCANVAS has index 99
# Defs - End unit FPCANVAS has index 99
# Defs - Begin unit FPREADPNM has index 100
# Defs - End unit FPREADPNM has index 100
# Defs - Begin unit FPWRITEPNM has index 101
# Defs - End unit FPWRITEPNM has index 101
# Defs - Begin unit JDEFERR has index 114
# Defs - End unit JDEFERR has index 114
# Defs - Begin unit JMORECFG has index 113
# Defs - End unit JMORECFG has index 113
# Defs - Begin unit JPEGLIB has index 109
# Defs - End unit JPEGLIB has index 109
# Defs - Begin unit JINCLUDE has index 115
# Defs - End unit JINCLUDE has index 115
# Defs - Begin unit JCOMAPI has index 120
# Defs - End unit JCOMAPI has index 120
# Defs - Begin unit JERROR has index 116
# Defs - End unit JERROR has index 116
# Defs - Begin unit JUTILS has index 121
# Defs - End unit JUTILS has index 121
# Defs - Begin unit JMEMNOBS has index 122
# Defs - End unit JMEMNOBS has index 122
# Defs - Begin unit JMEMMGR has index 117
# Defs - End unit JMEMMGR has index 117
# Defs - Begin unit JDMARKER has index 118
# Defs - End unit JDMARKER has index 118
# Defs - Begin unit JDINPUT has index 119
# Defs - End unit JDINPUT has index 119
# Defs - Begin unit JDAPIMIN has index 110
# Defs - End unit JDAPIMIN has index 110
# Defs - Begin unit JDATASRC has index 111
# Defs - End unit JDATASRC has index 111
# Defs - Begin unit JDCOLOR has index 124
# Defs - End unit JDCOLOR has index 124
# Defs - Begin unit JDSAMPLE has index 125
# Defs - End unit JDSAMPLE has index 125
# Defs - Begin unit JDPOSTCT has index 126
# Defs - End unit JDPOSTCT has index 126
# Defs - Begin unit JDCT has index 135
# Defs - End unit JDCT has index 135
# Defs - Begin unit JIDCTFST has index 136
# Defs - End unit JIDCTFST has index 136
# Defs - Begin unit JIDCTINT has index 137
# Defs - End unit JIDCTINT has index 137
# Defs - Begin unit JIDCTFLT has index 138
# Defs - End unit JIDCTFLT has index 138
# Defs - Begin unit JIDCTRED has index 139
# Defs - End unit JIDCTRED has index 139
# Defs - Begin unit JDDCTMGR has index 127
# Defs - End unit JDDCTMGR has index 127
# Defs - Begin unit JDHUFF has index 129
# Defs - End unit JDHUFF has index 129
# Defs - Begin unit JDPHUFF has index 128
# Defs - End unit JDPHUFF has index 128
# Defs - Begin unit JDCOEFCT has index 130
# Defs - End unit JDCOEFCT has index 130
# Defs - Begin unit JQUANT2 has index 133
# Defs - End unit JQUANT2 has index 133
# Defs - Begin unit JDMAINCT has index 131
# Defs - End unit JDMAINCT has index 131
# Defs - Begin unit JQUANT1 has index 132
# Defs - End unit JQUANT1 has index 132
# Defs - Begin unit JDMERGE has index 134
# Defs - End unit JDMERGE has index 134
# Defs - Begin unit JDMASTER has index 123
# Defs - End unit JDMASTER has index 123
# Defs - Begin unit JDAPISTD has index 112
# Defs - End unit JDAPISTD has index 112
# Defs - Begin unit FPREADJPEG has index 102
# Defs - End unit FPREADJPEG has index 102
# Defs - Begin unit JCMARKER has index 145
# Defs - End unit JCMARKER has index 145
# Defs - Begin unit JCAPIMIN has index 141
# Defs - End unit JCAPIMIN has index 141
# Defs - Begin unit JCHUFF has index 147
# Defs - End unit JCHUFF has index 147
# Defs - Begin unit JCPHUFF has index 146
# Defs - End unit JCPHUFF has index 146
# Defs - Begin unit JCMASTER has index 148
# Defs - End unit JCMASTER has index 148
# Defs - Begin unit JCCOLOR has index 149
# Defs - End unit JCCOLOR has index 149
# Defs - Begin unit JCSAMPLE has index 150
# Defs - End unit JCSAMPLE has index 150
# Defs - Begin unit JCPREPCT has index 151
# Defs - End unit JCPREPCT has index 151
# Defs - Begin unit JFDCTINT has index 155
# Defs - End unit JFDCTINT has index 155
# Defs - Begin unit JFDCTFST has index 156
# Defs - End unit JFDCTFST has index 156
# Defs - Begin unit JFDCTFLT has index 157
# Defs - End unit JFDCTFLT has index 157
# Defs - Begin unit JCDCTMGR has index 152
# Defs - End unit JCDCTMGR has index 152
# Defs - Begin unit JCCOEFCT has index 153
# Defs - End unit JCCOEFCT has index 153
# Defs - Begin unit JCMAINCT has index 154
# Defs - End unit JCMAINCT has index 154
# Defs - Begin unit JCINIT has index 144
# Defs - End unit JCINIT has index 144
# Defs - Begin unit JCAPISTD has index 140
# Defs - End unit JCAPISTD has index 140
# Defs - Begin unit JDATADST has index 142
# Defs - End unit JDATADST has index 142
# Defs - Begin unit JCPARAM has index 143
# Defs - End unit JCPARAM has index 143
# Defs - Begin unit FPWRITEJPEG has index 103
# Defs - End unit FPWRITEJPEG has index 103
# Defs - Begin unit FPREADGIF has index 104
# Defs - End unit FPREADGIF has index 104
# Defs - Begin unit VARUTILS has index 161
# Defs - End unit VARUTILS has index 161
# Defs - Begin unit VARIANTS has index 158
# Defs - End unit VARIANTS has index 158
# Defs - Begin unit LAZDBGLOG has index 163
# Defs - End unit LAZDBGLOG has index 163
# Defs - Begin unit AVGLVLTREE has index 162
# Defs - End unit AVGLVLTREE has index 162
# Defs - Begin unit LAZCONFIGSTORAGE has index 159
# Defs - End unit LAZCONFIGSTORAGE has index 159
# Defs - Begin unit DYNQUEUE has index 160
# Defs - End unit DYNQUEUE has index 160
# Defs - Begin unit LRESOURCES has index 105
# Defs - End unit LRESOURCES has index 105
# Defs - Begin unit WSREFERENCES has index 107
# Defs - End unit WSREFERENCES has index 107
# Defs - Begin unit SYNCOBJS has index 164
# Defs - End unit SYNCOBJS has index 164
# Defs - Begin unit LCLRESCACHE has index 106
# Defs - End unit LCLRESCACHE has index 106
# Defs - Begin unit OBJC has index 165
# Defs - End unit OBJC has index 165
# Defs - Begin unit OBJCBASE has index 166
# Defs - End unit OBJCBASE has index 166
# Defs - Begin unit DEFINEDCLASSESFOUNDATION has index 172
# Defs - End unit DEFINEDCLASSESFOUNDATION has index 172
# Defs - Begin unit DEFINEDCLASSESCOREIMAGE has index 173
# Defs - End unit DEFINEDCLASSESCOREIMAGE has index 173
# Defs - Begin unit DEFINEDCLASSESQUARTZCORE has index 174
# Defs - End unit DEFINEDCLASSESQUARTZCORE has index 174
# Defs - Begin unit DEFINEDCLASSESCOREDATA has index 175
# Defs - End unit DEFINEDCLASSESCOREDATA has index 175
# Defs - Begin unit DEFINEDCLASSESAPPKIT has index 176
# Defs - End unit DEFINEDCLASSESAPPKIT has index 176
# Defs - Begin unit COCOAALL has index 167
# Defs - End unit COCOAALL has index 167
# Defs - Begin unit MASKS has index 178
# Defs - End unit MASKS has index 178
# Defs - Begin unit FILEUTIL has index 168
# Defs - End unit FILEUTIL has index 168
# Defs - Begin unit TERMIO has index 181
# Defs - End unit TERMIO has index 181
# Defs - Begin unit PIPES has index 180
# Defs - End unit PIPES has index 180
# Defs - Begin unit PROCESS has index 179
# Defs - End unit PROCESS has index 179
# Defs - Begin unit UTF8PROCESS has index 169
# Defs - End unit UTF8PROCESS has index 169
# Defs - Begin unit LAZSYSUTILS has index 170
# Defs - End unit LAZSYSUTILS has index 170
# Defs - Begin unit MAPS has index 171
# Defs - End unit MAPS has index 171
# Defs - Begin unit LCLINTF has index 97
# Defs - End unit LCLINTF has index 97
# Defs - Begin unit GRAPHICS has index 96
# Defs - End unit GRAPHICS has index 96
# Defs - Begin unit INTFGRAPHICS has index 44
# Defs - End unit INTFGRAPHICS has index 44
# Defs - Begin unit TMSCHEMA has index 177
# Defs - End unit TMSCHEMA has index 177
# Defs - Begin unit THEMES has index 45
# Defs - End unit THEMES has index 45
# Defs - Begin unit INTERFACEBASE has index 20
# Defs - End unit INTERFACEBASE has index 20
# Defs - Begin unit CFBASE has index 199
# Defs - End unit CFBASE has index 199
# Defs - Begin unit CFDICTIONARY has index 200
# Defs - End unit CFDICTIONARY has index 200
# Defs - Begin unit CGBASE has index 201
# Defs - End unit CGBASE has index 201
# Defs - Begin unit CGGEOMETRY has index 198
# Defs - End unit CGGEOMETRY has index 198
# Defs - Begin unit COCOA_EXTRA has index 186
# Defs - End unit COCOA_EXTRA has index 186
# Defs - Begin unit WSLCLCLASSES has index 203
# Defs - End unit WSLCLCLASSES has index 203
# Defs - Begin unit LCLCLASSES has index 202
# Defs - End unit LCLCLASSES has index 202
# Defs - Begin unit COCOAUTILS has index 184
# Defs - End unit COCOAUTILS has index 184
# Defs - Begin unit PIXTOOLS has index 212
# Defs - End unit PIXTOOLS has index 212
# Defs - Begin unit ELLIPSES has index 213
# Defs - End unit ELLIPSES has index 213
# Defs - Begin unit FPPIXLCANV has index 211
# Defs - End unit FPPIXLCANV has index 211
# Defs - Begin unit FPIMGCANV has index 210
# Defs - End unit FPIMGCANV has index 210
# Defs - Begin unit ACTNLIST has index 208
# Defs - End unit ACTNLIST has index 208
# Defs - Begin unit WSFACTORY has index 216
# Defs - End unit WSFACTORY has index 216
# Defs - Begin unit LAZSTRINGUTILS has index 220
# Defs - End unit LAZSTRINGUTILS has index 220
# Defs - Begin unit LAZLOGGER has index 219
# Defs - End unit LAZLOGGER has index 219
# Defs - Begin unit WSMENUS has index 218
# Defs - End unit WSMENUS has index 218
# Defs - Begin unit SINGLEINSTANCE has index 225
# Defs - End unit SINGLEINSTANCE has index 225
# Defs - Begin unit CUSTAPP has index 221
# Defs - End unit CUSTAPP has index 221
# Defs - Begin unit CUSTOMTIMER has index 222
# Defs - End unit CUSTOMTIMER has index 222
# Defs - Begin unit FASTHTMLPARSER has index 226
# Defs - End unit FASTHTMLPARSER has index 226
# Defs - Begin unit CLIPBRD has index 223
# Defs - End unit CLIPBRD has index 223
# Defs - Begin unit HELPINTFS has index 224
# Defs - End unit HELPINTFS has index 224
# Defs - Begin unit UITYPES has index 209
# Defs - End unit UITYPES has index 209
# Defs - Begin unit WSCONTROLS has index 227
# Defs - End unit WSCONTROLS has index 227
# Defs - Begin unit WSFORMS has index 228
# Defs - End unit WSFORMS has index 228
# Defs - Begin unit FORMS has index 8
# Defs - End unit FORMS has index 8
# Defs - Begin unit MENUS has index 196
# Defs - End unit MENUS has index 196
# Defs - Begin unit WSPROC has index 215
# Defs - End unit WSPROC has index 215
# Defs - Begin unit WSIMGLIST has index 214
# Defs - End unit WSIMGLIST has index 214
# Defs - Begin unit IMGLIST has index 206
# Defs - End unit IMGLIST has index 206
# Defs - Begin unit RTTIUTILS has index 217
# Defs - End unit RTTIUTILS has index 217
# Defs - Begin unit PROPERTYSTORAGE has index 207
# Defs - End unit PROPERTYSTORAGE has index 207
# Defs - Begin unit CONTROLS has index 195
# Defs - End unit CONTROLS has index 195
# Defs - Begin unit TEXTSTRINGS has index 234
# Defs - End unit TEXTSTRINGS has index 234
# Defs - Begin unit EXTENDEDSTRINGS has index 235
# Defs - End unit EXTENDEDSTRINGS has index 235
# Defs - Begin unit WSSTDCTRLS has index 236
# Defs - End unit WSSTDCTRLS has index 236
# Defs - Begin unit STDCTRLS has index 231
# Defs - End unit STDCTRLS has index 231
# Defs - Begin unit POPUPNOTIFIER has index 237
# Defs - End unit POPUPNOTIFIER has index 237
# Defs - Begin unit WSEXTCTRLS has index 238
# Defs - End unit WSEXTCTRLS has index 238
# Defs - Begin unit EXTCTRLS has index 230
# Defs - End unit EXTCTRLS has index 230
# Defs - Begin unit TOOLWIN has index 232
# Defs - End unit TOOLWIN has index 232
# Defs - Begin unit IMAGELISTCACHE has index 239
# Defs - End unit IMAGELISTCACHE has index 239
# Defs - Begin unit WSBUTTONS has index 240
# Defs - End unit WSBUTTONS has index 240
# Defs - Begin unit BUTTONS has index 233
# Defs - End unit BUTTONS has index 233
# Defs - Begin unit WSTOOLWIN has index 242
# Defs - End unit WSTOOLWIN has index 242
# Defs - Begin unit WSCOMCTRLS has index 241
# Defs - End unit WSCOMCTRLS has index 241
# Defs - Begin unit COMCTRLS has index 229
# Defs - End unit COMCTRLS has index 229
# Defs - Begin unit CUSTOMDRAWNDRAWERS has index 204
# Defs - End unit CUSTOMDRAWNDRAWERS has index 204
# Defs - Begin unit CUSTOMDRAWN_COMMON has index 243
# Defs - End unit CUSTOMDRAWN_COMMON has index 243
# Defs - Begin unit CUSTOMDRAWN_MAC has index 205
# Defs - End unit CUSTOMDRAWN_MAC has index 205
# Defs - Begin unit COCOAGDIOBJECTS has index 185
# Defs - End unit COCOAGDIOBJECTS has index 185
# Defs - Begin unit COCOAPRIVATE has index 183
# Defs - End unit COCOAPRIVATE has index 183
# Defs - Begin unit MESSAGES has index 244
# Defs - End unit MESSAGES has index 244
# Defs - Begin unit LCLMESSAGEGLUE has index 194
# Defs - End unit LCLMESSAGEGLUE has index 194
# Defs - Begin unit COCOACARET has index 245
# Defs - End unit COCOACARET has index 245
# Defs - Begin unit COCOASCROLLERS has index 190
# Defs - End unit COCOASCROLLERS has index 190
# Defs - Begin unit COCOAWSCOMMON has index 193
# Defs - End unit COCOAWSCOMMON has index 193
# Defs - Begin unit COCOAWSMENUS has index 187
# Defs - End unit COCOAWSMENUS has index 187
# Defs - Begin unit COCOABUTTONS has index 247
# Defs - End unit COCOABUTTONS has index 247
# Defs - Begin unit COCOATHEMES has index 249
# Defs - End unit COCOATHEMES has index 249
# Defs - Begin unit COCOATABLES has index 248
# Defs - End unit COCOATABLES has index 248
# Defs - Begin unit COCOATEXTEDITS has index 192
# Defs - End unit COCOATEXTEDITS has index 192
# Defs - Begin unit COCOAWSSTDCTRLS has index 246
# Defs - End unit COCOAWSSTDCTRLS has index 246
# Defs - Begin unit COCOAWINDOWS has index 189
# Defs - End unit COCOAWINDOWS has index 189
# Defs - Begin unit COCOAWSFORMS has index 188
# Defs - End unit COCOAWSFORMS has index 188
# Defs - Begin unit COCOAWSCLIPBOARD has index 191
# Defs - End unit COCOAWSCLIPBOARD has index 191
# Defs - Begin unit WSSPIN has index 262
# Defs - End unit WSSPIN has index 262
# Defs - Begin unit SPIN has index 250
# Defs - End unit SPIN has index 250
# Defs - Begin unit WSPAIRSPLITTER has index 263
# Defs - End unit WSPAIRSPLITTER has index 263
# Defs - Begin unit PAIRSPLITTER has index 251
# Defs - End unit PAIRSPLITTER has index 251
# Defs - Begin unit BUTTONPANEL has index 264
# Defs - End unit BUTTONPANEL has index 264
# Defs - Begin unit LCLTASKDIALOG has index 265
# Defs - End unit LCLTASKDIALOG has index 265
# Defs - Begin unit WSDIALOGS has index 266
# Defs - End unit WSDIALOGS has index 266
# Defs - Begin unit DIALOGS has index 252
# Defs - End unit DIALOGS has index 252
# Defs - Begin unit WSCHECKLST has index 267
# Defs - End unit WSCHECKLST has index 267
# Defs - Begin unit CHECKLST has index 253
# Defs - End unit CHECKLST has index 253
# Defs - Begin unit WSCALENDAR has index 268
# Defs - End unit WSCALENDAR has index 268
# Defs - Begin unit CALENDAR has index 254
# Defs - End unit CALENDAR has index 254
# Defs - Begin unit COCOAWSBUTTONS has index 255
# Defs - End unit COCOAWSBUTTONS has index 255
# Defs - Begin unit COCOAWSEXTCTRLS has index 256
# Defs - End unit COCOAWSEXTCTRLS has index 256
# Defs - Begin unit COCOATABCONTROLS has index 269
# Defs - End unit COCOATABCONTROLS has index 269
# Defs - Begin unit COCOAWSCOMCTRLS has index 257
# Defs - End unit COCOAWSCOMCTRLS has index 257
# Defs - Begin unit COCOAWSDIALOGS has index 258
# Defs - End unit COCOAWSDIALOGS has index 258
# Defs - Begin unit COCOAWSSPIN has index 259
# Defs - End unit COCOAWSSPIN has index 259
# Defs - Begin unit COCOAWSCHECKLST has index 260
# Defs - End unit COCOAWSCHECKLST has index 260
# Defs - Begin unit COCOADATEPICKER has index 270
# Defs - End unit COCOADATEPICKER has index 270
# Defs - Begin unit COCOAWSDATEPICKER has index 261
# Defs - End unit COCOAWSDATEPICKER has index 261
# Defs - Begin unit COCOAWSFACTORY has index 197
# Defs - End unit COCOAWSFACTORY has index 197
# Defs - Begin unit COCOAINT has index 182
# Defs - End unit COCOAINT has index 182
# Defs - Begin unit INTERFACES has index 7
# Defs - End unit INTERFACES has index 7
# Defs - Begin unit FPJSON has index 273
# Defs - End unit FPJSON has index 273
# Defs - Begin unit ANYSORT has index 13
# Defs - End unit ANYSORT has index 13
# Defs - Begin unit ARRAYUTILS has index 12
# Defs - End unit ARRAYUTILS has index 12
# Defs - Begin unit FILEUTILITIES has index 274
# Defs - End unit FILEUTILITIES has index 274
# Defs - Begin unit BINGOCARD has index 10
# Defs - End unit BINGOCARD has index 10
# Defs - Begin unit VENTMAP has index 11
# Defs - End unit VENTMAP has index 11
# Defs - Begin unit DATEUTILS has index 271
# Defs - End unit DATEUTILS has index 271
# Defs - Begin unit AOCUTILS has index 272
# Defs - End unit AOCUTILS has index 272
# Defs - Begin unit PAINTBOX has index 14
# Defs - End unit PAINTBOX has index 14
# Defs - Begin unit OCTOPUS has index 15
# Defs - End unit OCTOPUS has index 15
# Defs - Begin unit ORIGAMI has index 16
# Defs - End unit ORIGAMI has index 16
# Defs - Begin unit POLYMER has index 17
# Defs - End unit POLYMER has index 17
# Defs - Begin unit CHITON has index 18
# Defs - End unit CHITON has index 18
# Defs - Begin unit ADVENT has index 9
# Defs - End unit ADVENT has index 9
# Defs - Begin unit SYSINIT has index 275
# Defs - End unit SYSINIT has index 275
# Defs - Begin Staticsymtable
# Definition ^^Char
La3:
	.byte	8
	.long	L$set$9
	.set L$set$9,La6-Ldebug_info0
La4:
	.byte	7
	.long	L$set$10
	.set L$set$10,La3-Ldebug_info0
# Defs - End Staticsymtable
# Definition ^Char
La6:
	.byte	5
	.ascii	"$char_pointer\000"
	.long	L$set$11
	.set L$set$11,La8-Ldebug_info0
La8:
	.byte	8
	.long	L$set$12
	.set L$set$12,La9-Ldebug_info0
La7:
	.byte	7
	.long	L$set$13
	.set L$set$13,La6-Ldebug_info0
# Definition Char
La9:
	.byte	5
	.ascii	"Char\000"
	.long	L$set$14
	.set L$set$14,La11-Ldebug_info0
La11:
	.byte	6
	.ascii	"Char\000"
	.byte	8
	.byte	1
La10:
	.byte	7
	.long	L$set$15
	.set L$set$15,La9-Ldebug_info0
	.byte	0
Ledebug_info0:
# End asmlist al_dwarf_info
# Begin asmlist al_dwarf_abbrev

.section __DWARF,__debug_abbrev,regular,debug
# Abbrev 1
	.byte	1
	.byte	17
	.byte	1
	.byte	3
	.byte	8
	.byte	37
	.byte	8
	.byte	27
	.byte	8
	.byte	19
	.byte	11
	.byte	66
	.byte	11
	.byte	16
	.byte	6
	.byte	17
	.byte	1
	.byte	18
	.byte	1
	.byte	0
	.byte	0
# Abbrev 2
	.byte	2
	.byte	46
	.byte	1
	.byte	3
	.byte	8
	.byte	39
	.byte	12
	.byte	63
	.byte	12
	.byte	17
	.byte	1
	.byte	18
	.byte	1
	.byte	0
	.byte	0
# Abbrev 3
	.byte	3
	.byte	5
	.byte	0
	.byte	3
	.byte	8
	.byte	2
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 4
	.byte	4
	.byte	46
	.byte	1
	.byte	3
	.byte	8
	.byte	39
	.byte	12
	.byte	54
	.byte	11
	.byte	63
	.byte	12
	.byte	17
	.byte	1
	.byte	18
	.byte	1
	.byte	0
	.byte	0
# Abbrev 5
	.byte	5
	.byte	22
	.byte	0
	.byte	3
	.byte	8
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 6
	.byte	6
	.byte	36
	.byte	0
	.byte	3
	.byte	8
	.byte	62
	.byte	11
	.byte	11
	.byte	11
	.byte	0
	.byte	0
# Abbrev 7
	.byte	7
	.byte	16
	.byte	0
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 8
	.byte	8
	.byte	15
	.byte	0
	.byte	73
	.byte	19
	.byte	0
	.byte	0
	.byte	0
# End asmlist al_dwarf_abbrev
# Begin asmlist al_dwarf_line

.section __DWARF,__debug_line,regular,debug
# === header start ===
	.long	L$set$16
	.set L$set$16,Ledebug_line0-Lf2
Lf2:
	.short	3
	.long	L$set$17
	.set L$set$17,Lehdebug_line0-Lf3
Lf3:
	.byte	1
	.byte	1
	.byte	1
	.byte	255
	.byte	13
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	1
# include_directories
	.byte	0
# file_names
	.ascii	"adventOfCode.lpr\000"
	.byte	0
	.byte	0
	.byte	0
	.byte	0
Lehdebug_line0:
# === header end ===
# function: _main
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll1
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _PASCALMAIN
# [15:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll2
	.byte	5
	.byte	1
	.byte	26
# [16:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll3
	.byte	5
	.byte	3
	.byte	13
# [17:21]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll4
	.byte	5
	.byte	21
	.byte	13
# [18:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll5
	.byte	5
	.byte	3
	.byte	13
# [19:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll6
	.byte	13
# [20:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll7
	.byte	13
# [21:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll8
	.byte	13
# [22:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll9
	.byte	5
	.byte	1
	.byte	13
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll10
	.byte	0
	.byte	1
	.byte	1
# ###################
Ledebug_line0:
# End asmlist al_dwarf_line
# Begin asmlist al_end

.text
L_DEBUGEND_$P$ADVENTOFCODE:
# End asmlist al_end
	.subsections_via_symbols

