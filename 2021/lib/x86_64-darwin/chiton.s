# Begin asmlist al_begin

.section __DWARF,__debug_line,regular,debug
Ldebug_linesection0:
Ldebug_line0:

.section __DWARF,__debug_abbrev,regular,debug
Ldebug_abbrevsection0:
Ldebug_abbrev0:

.text
L_DEBUGSTART_$CHITON:
# End asmlist al_begin
# Begin asmlist al_procedures

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_CREATE$TSTRINGARRAY$$TROUTEFINDER
_CHITON$_$TROUTEFINDER_$__$$_CREATE$TSTRINGARRAY$$TROUTEFINDER:
Ll1:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-256(%rsp),%rsp
	movq	%rbx,-256(%rbp)
	movq	%rdi,-24(%rbp)
	movq	%rsi,-16(%rbp)
	movq	%rdx,-8(%rbp)
	movq	%rdx,%rdi
	call	fpc_dynarray_incr_ref
Ll2:
	movq	$0,-248(%rbp)
	movq	$0,-232(%rbp)
	cmpq	$1,-16(%rbp)
	jne	Lj6
	movq	-24(%rbp),%rax
	movq	-24(%rbp),%rdx
	movq	%rax,%rdi
	call	*104(%rdx)
	movq	%rax,-24(%rbp)
Lj6:
	cmpq	$0,-24(%rbp)
	je	Lj3
	leaq	-56(%rbp),%rdx
	leaq	-120(%rbp),%rsi
	movl	$1,%edi
	call	fpc_pushexceptaddr
	movq	%rax,%rdi
	call	fpc_setjmp
	movslq	%eax,%rdx
	movq	%rdx,-128(%rbp)
	testl	%eax,%eax
	jne	Lj13
	movq	$-1,-32(%rbp)
	leaq	-152(%rbp),%rdx
	leaq	-216(%rbp),%rsi
	movl	$1,%edi
	call	fpc_pushexceptaddr
	movq	%rax,%rdi
	call	fpc_setjmp
	movslq	%eax,%rdx
	movq	%rdx,-224(%rbp)
	testl	%eax,%eax
	jne	Lj15
Ll3:
	movq	_RTTI_$ARRAYUTILS_$$_T2DINTMAP@GOTPCREL(%rip),%rbx
	movq	_RTTI_$ARRAYUTILS_$$_T2DINTMAP@GOTPCREL(%rip),%rsi
	leaq	-232(%rbp),%rdi
	call	fpc_dynarray_clear
	movq	$0,-240(%rbp)
	movq	_RTTI_$ARRAYUTILS_$$_T2DINTMAP@GOTPCREL(%rip),%rsi
	leaq	-240(%rbp),%rcx
	movl	$1,%edx
	leaq	-232(%rbp),%rdi
	call	fpc_dynarray_setlength
	movq	-232(%rbp),%rsi
	movq	-24(%rbp),%rax
	leaq	24(%rax),%rdi
	movq	%rbx,%rdx
	call	fpc_dynarray_assign
Ll4:
	movq	_INIT_$CHITON_$$_TQUEUE@GOTPCREL(%rip),%rbx
	movq	_INIT_$CHITON_$$_TQUEUE@GOTPCREL(%rip),%rsi
	leaq	-248(%rbp),%rdi
	call	fpc_dynarray_clear
	movq	$0,-240(%rbp)
	movq	_INIT_$CHITON_$$_TQUEUE@GOTPCREL(%rip),%rsi
	leaq	-240(%rbp),%rcx
	movl	$1,%edx
	leaq	-248(%rbp),%rdi
	call	fpc_dynarray_setlength
	movq	-248(%rbp),%rsi
	movq	-24(%rbp),%rax
	leaq	32(%rax),%rdi
	movq	%rbx,%rdx
	call	fpc_dynarray_assign
Ll5:
	movq	-8(%rbp),%rsi
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_INITIALIZEMAPANDQUEUE$TSTRINGARRAY
Lj15:
Ll6:
	call	fpc_popaddrstack
Ll7:
	movq	_INIT_$CHITON_$$_TQUEUE@GOTPCREL(%rip),%rsi
	leaq	-248(%rbp),%rdi
	call	fpc_dynarray_clear
	movq	_RTTI_$ARRAYUTILS_$$_T2DINTMAP@GOTPCREL(%rip),%rsi
	leaq	-232(%rbp),%rdi
	call	fpc_dynarray_clear
	movq	_RTTI_$SYSUTILS_$$_TSTRINGARRAY@GOTPCREL(%rip),%rsi
	leaq	-8(%rbp),%rdi
	call	fpc_dynarray_clear
Ll8:
	movq	-224(%rbp),%rax
	testq	%rax,%rax
	je	Lj14
	call	fpc_reraise
Lj14:
Ll9:
	movq	$1,-32(%rbp)
	cmpq	$0,-24(%rbp)
	je	Lj17
	cmpq	$0,-16(%rbp)
	je	Lj17
	movq	-24(%rbp),%rdi
	movq	-24(%rbp),%rax
	movq	(%rax),%rax
	call	*136(%rax)
Lj17:
Lj13:
Ll10:
	call	fpc_popaddrstack
	movq	-128(%rbp),%rax
	testq	%rax,%rax
	je	Lj11
	leaq	-152(%rbp),%rdx
	leaq	-224(%rbp),%rsi
	movl	$1,%edi
	call	fpc_pushexceptaddr
	movq	%rax,%rdi
	call	fpc_setjmp
	movslq	%eax,%rdx
	movq	%rdx,-240(%rbp)
	testl	%eax,%eax
	jne	Lj19
Ll11:
	cmpq	$0,-16(%rbp)
	je	Lj21
	movq	-32(%rbp),%rsi
	movq	-24(%rbp),%rdi
	movq	-24(%rbp),%rax
	movq	(%rax),%rax
	call	*96(%rax)
Lj21:
	call	fpc_popaddrstack
	call	fpc_reraise
Lj19:
Ll12:
	call	fpc_popaddrstack
	movq	-240(%rbp),%rax
	testq	%rax,%rax
	je	Lj22
	call	fpc_raise_nested
Lj22:
	call	fpc_doneexception
Lj11:
Lj3:
Ll13:
	movq	-24(%rbp),%rax
	movq	-256(%rbp),%rbx
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt11:
Ll14:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_INITIALIZEMAPANDQUEUE$TSTRINGARRAY
_CHITON$_$TROUTEFINDER_$__$$_INITIALIZEMAPANDQUEUE$TSTRINGARRAY:
Ll15:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-192(%rsp),%rsp
	movq	%rbx,-184(%rbp)
	movq	%r12,-176(%rbp)
	movq	%rdi,-16(%rbp)
	movq	%rsi,-8(%rbp)
	movq	%rsi,%rdi
	call	fpc_dynarray_incr_ref
Ll16:
	movq	$0,-40(%rbp)
	movq	$0,-168(%rbp)
	leaq	-72(%rbp),%rdx
	leaq	-136(%rbp),%rsi
	movl	$1,%edi
	call	fpc_pushexceptaddr
	movq	%rax,%rdi
	call	fpc_setjmp
	movslq	%eax,%rdx
	movq	%rdx,-144(%rbp)
	testl	%eax,%eax
	jne	Lj26
Ll17:
	movq	-8(%rbp),%rax
	testq	%rax,%rax
	je	Lj27
	movq	-8(%rax),%rax
	addq	$1,%rax
Lj27:
	movl	%eax,-24(%rbp)
Ll18:
	testl	%eax,%eax
	jng	Lj29
	movq	-8(%rbp),%rax
	movq	(%rax),%rax
	testq	%rax,%rax
	je	Lj30
	movq	-8(%rax),%rax
Lj30:
	movl	%eax,-20(%rbp)
Lj29:
Ll19:
	movslq	-24(%rbp),%rax
	movq	%rax,-152(%rbp)
	movslq	-20(%rbp),%rax
	movq	%rax,-160(%rbp)
	movq	_RTTI_$ARRAYUTILS_$$_T2DINTMAP@GOTPCREL(%rip),%rsi
	movq	-16(%rbp),%rax
	leaq	24(%rax),%rdi
	leaq	-160(%rbp),%rcx
	movl	$2,%edx
	call	fpc_dynarray_setlength
Ll20:
	movl	-24(%rbp),%eax
	leal	-1(%eax),%ebx
	testl	%ebx,%ebx
	jnge	Lj32
	movl	$-1,-32(%rbp)
	.align 2
Lj33:
	addl	$1,-32(%rbp)
Ll21:
	movq	-8(%rbp),%rax
	movslq	-32(%rbp),%rdx
	movq	(%rax,%rdx,8),%rsi
	leaq	-40(%rbp),%rdi
	call	fpc_ansistr_assign
Ll22:
	movl	-20(%rbp),%eax
	leal	-1(%eax),%r12d
	testl	%r12d,%r12d
	jnge	Lj37
	movl	$-1,-28(%rbp)
	.align 2
Lj38:
	addl	$1,-28(%rbp)
Ll23:
	leaq	-168(%rbp),%rdi
	call	fpc_ansistr_decr_ref
	movslq	-28(%rbp),%rdx
	leaq	-40(%rbp),%rdi
	movl	$1,%ecx
	leaq	-168(%rbp),%rsi
	call	_SYSUTILS$_$TSTRINGHELPER_$__$$_SUBSTRING$INT64$INT64$$ANSISTRING
	movq	-168(%rbp),%rdi
	call	_SYSUTILS_$$_STRTOINT$ANSISTRING$$LONGINT
	movq	-16(%rbp),%rdx
	movq	24(%rdx),%rdx
	movslq	-28(%rbp),%rcx
	movq	(%rdx,%rcx,8),%rdx
	movslq	-32(%rbp),%rcx
	movl	%eax,(%rdx,%rcx,4)
Ll24:
	movl	-28(%rbp),%eax
	movl	%eax,-48(%rbp)
Ll25:
	movl	-32(%rbp),%eax
	movl	%eax,-44(%rbp)
Ll26:
	movq	-16(%rbp),%rax
	movq	32(%rax),%rax
	testq	%rax,%rax
	je	Lj41
	movq	-8(%rax),%rax
	addq	$1,%rax
Lj41:
	addq	$1,%rax
	movq	%rax,-152(%rbp)
	movq	_INIT_$CHITON_$$_TQUEUE@GOTPCREL(%rip),%rsi
	movq	-16(%rbp),%rax
	leaq	32(%rax),%rdi
	leaq	-152(%rbp),%rcx
	movl	$1,%edx
	call	fpc_dynarray_setlength
Ll27:
	movq	-16(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETMAPDIMENSIONS$$TPOINT
	movq	%rax,%rcx
	movq	-48(%rbp),%rdx
	movl	$1,%esi
	movq	_VMT_$CHITON_$$_TQUEUEENTRY@GOTPCREL(%rip),%rdi
	call	_CHITON$_$TQUEUEENTRY_$__$$_CREATE$TPOINT$TPOINT$$TQUEUEENTRY
	movq	-16(%rbp),%rdx
	movq	32(%rdx),%rcx
	movq	-16(%rbp),%rdx
	movq	32(%rdx),%rdx
	testq	%rdx,%rdx
	je	Lj42
	movq	-8(%rdx),%rdx
	addq	$1,%rdx
Lj42:
	subq	$1,%rdx
	movq	%rax,(%rcx,%rdx,8)
Ll28:
	cmpl	-28(%rbp),%r12d
	jnle	Lj38
Lj37:
Ll29:
	cmpl	-32(%rbp),%ebx
	jnle	Lj33
Lj32:
Lj26:
Ll30:
	call	fpc_popaddrstack
Ll31:
	leaq	-168(%rbp),%rdi
	call	fpc_ansistr_decr_ref
	movq	_RTTI_$SYSUTILS_$$_TSTRINGARRAY@GOTPCREL(%rip),%rsi
	leaq	-8(%rbp),%rdi
	call	fpc_dynarray_clear
	leaq	-40(%rbp),%rdi
	call	fpc_ansistr_decr_ref
Ll32:
	movq	-144(%rbp),%rax
	testq	%rax,%rax
	je	Lj25
	call	fpc_reraise
Lj25:
Ll33:
	movq	-184(%rbp),%rbx
	movq	-176(%rbp),%r12
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt2:
Ll34:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_GETQUEUELENGTH$$LONGINT
_CHITON$_$TROUTEFINDER_$__$$_GETQUEUELENGTH$$LONGINT:
Ll35:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-16(%rsp),%rsp
	movq	%rdi,-8(%rbp)
Ll36:
	movq	32(%rdi),%rax
	testq	%rax,%rax
	je	Lj45
	movq	-8(%rax),%rax
	addq	$1,%rax
Lj45:
Ll37:
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt3:
Ll38:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_GETMAPDIMENSIONS$$TPOINT
_CHITON$_$TROUTEFINDER_$__$$_GETMAPDIMENSIONS$$TPOINT:
Ll39:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-16(%rsp),%rsp
	movq	%rdi,-8(%rbp)
Ll40:
	movq	24(%rdi),%rax
	testq	%rax,%rax
	je	Lj48
	movq	-8(%rax),%rax
	addq	$1,%rax
Lj48:
	movl	%eax,-16(%rbp)
Ll41:
	movq	-8(%rbp),%rax
	movq	24(%rax),%rax
	testq	%rax,%rax
	je	Lj49
	movq	-8(%rax),%rax
	addq	$1,%rax
Lj49:
	testq	%rax,%rax
	jng	Lj51
	movq	-8(%rbp),%rax
	movq	24(%rax),%rax
	movq	(%rax),%rax
	testq	%rax,%rax
	je	Lj52
	movq	-8(%rax),%rax
	addq	$1,%rax
Lj52:
	movl	%eax,-12(%rbp)
	jmp	Lj53
Lj51:
	movl	$0,-12(%rbp)
Lj53:
Ll42:
	movq	-16(%rbp),%rax
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt4:
Ll43:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_PROCESSNODE$TQUEUEENTRY$TQUEUEENTRY
_CHITON$_$TROUTEFINDER_$__$$_PROCESSNODE$TQUEUEENTRY$TQUEUEENTRY:
Ll44:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-64(%rsp),%rsp
	movq	%rdi,-24(%rbp)
	movq	%rsi,-8(%rbp)
	movq	%rdx,-16(%rbp)
Ll45:
	movq	-8(%rbp),%rax
	movb	$1,45(%rax)
Ll46:
	movq	-8(%rbp),%rax
	cmpq	-16(%rbp),%rax
	jne	Lj57
Ll47:
	movq	-24(%rbp),%rdx
	movq	-8(%rbp),%rax
	movl	36(%rax),%eax
	movl	%eax,40(%rdx)
	jmp	Lj58
Lj57:
Ll48:
	movl	$-2,-52(%rbp)
	.align 2
Lj59:
	addl	$1,-52(%rbp)
Ll49:
	movl	$-2,-56(%rbp)
	.align 2
Lj62:
	addl	$1,-56(%rbp)
Ll50:
	movq	-8(%rbp),%rax
	movl	28(%rax),%eax
	addl	-52(%rbp),%eax
	movl	%eax,-48(%rbp)
Ll51:
	movq	-8(%rbp),%rax
	movl	32(%rax),%eax
	addl	-56(%rbp),%eax
	movl	%eax,-44(%rbp)
Ll52:
	movq	-8(%rbp),%rax
	movl	28(%rax),%eax
	cmpl	-48(%rbp),%eax
	je	Lj65
Ll53:
	movq	-8(%rbp),%rax
	movl	32(%rax),%eax
	cmpl	-44(%rbp),%eax
	jne	Lj67
Lj65:
Ll54:
	movq	-8(%rbp),%rax
	movl	28(%rax),%eax
	cmpl	-48(%rbp),%eax
	jne	Lj68
	movq	-8(%rbp),%rax
	movl	32(%rax),%eax
	cmpl	-44(%rbp),%eax
	je	Lj67
Lj68:
Ll55:
	movq	-48(%rbp),%rsi
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRY$TPOINT$$TQUEUEENTRY
	movq	%rax,-32(%rbp)
Ll56:
	testq	%rax,%rax
	je	Lj73
Ll57:
	movq	-32(%rbp),%rax
	cmpb	$0,45(%rax)
	jne	Lj73
Ll58:
	movq	-48(%rbp),%rcx
	movq	-8(%rbp),%rdx
	movq	-32(%rbp),%rsi
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_UPDATERISK$TQUEUEENTRY$TQUEUEENTRY$TPOINT
Lj73:
Lj67:
Ll59:
	cmpl	$1,-56(%rbp)
	jnge	Lj62
Ll60:
	cmpl	$1,-52(%rbp)
	jnge	Lj59
Ll61:
	movq	-8(%rbp),%rsi
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_REMOVEFROMQUEUE$TQUEUEENTRY
Ll62:
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETLEASTRISKYQUEUEENTRY$$TQUEUEENTRY
	movq	%rax,-40(%rbp)
Ll63:
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETLEASTRISKYQUEUEENTRY$$TQUEUEENTRY
	movq	%rax,%rsi
	movq	-16(%rbp),%rdx
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_PROCESSNODE$TQUEUEENTRY$TQUEUEENTRY
Lj58:
Ll64:
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt5:
Ll65:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_FINDSHORTESTPATH$TPOINT$TPOINT
_CHITON$_$TROUTEFINDER_$__$$_FINDSHORTESTPATH$TPOINT$TPOINT:
Ll66:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-48(%rsp),%rsp
	movq	%rdi,-24(%rbp)
	movq	%rsi,-8(%rbp)
	movq	%rdx,-16(%rbp)
Ll67:
	movq	-8(%rbp),%rax
	movq	-24(%rbp),%rdi
	movq	%rax,%rsi
	call	_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRY$TPOINT$$TQUEUEENTRY
	movq	%rax,-32(%rbp)
Ll68:
	movq	-16(%rbp),%rsi
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRY$TPOINT$$TQUEUEENTRY
	movq	%rax,-40(%rbp)
Ll69:
	cmpq	$0,-32(%rbp)
	je	Lj75
	cmpq	$0,-40(%rbp)
	je	Lj75
Ll70:
	movq	-40(%rbp),%rdx
	movq	-32(%rbp),%rsi
	movq	-24(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_PROCESSNODE$TQUEUEENTRY$TQUEUEENTRY
Lj75:
Ll71:
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt12:
Ll72:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRY$TPOINT$$TQUEUEENTRY
_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRY$TPOINT$$TQUEUEENTRY:
Ll73:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-32(%rsp),%rsp
	movq	%rdi,-16(%rbp)
	movq	%rsi,-8(%rbp)
Ll74:
	movq	$0,-24(%rbp)
Ll75:
	movq	-16(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETMAPDIMENSIONS$$TPOINT
	movl	-8(%rbp),%edx
	cmpl	%eax,%edx
	jge	Lj80
	movq	-16(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETMAPDIMENSIONS$$TPOINT
	movl	-4(%rbp),%edx
	sarq	$32,%rax
	cmpl	%eax,%edx
	jge	Lj80
Ll76:
	cmpl	$0,-8(%rbp)
	jl	Lj80
	cmpl	$0,-4(%rbp)
	jl	Lj80
Ll77:
	movq	-16(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETMAPDIMENSIONS$$TPOINT
	movl	-4(%rbp),%edx
	imull	%edx,%eax
	addl	-8(%rbp),%eax
	movl	%eax,-28(%rbp)
Ll78:
	movq	-16(%rbp),%rax
	movq	32(%rax),%rax
	testq	%rax,%rax
	je	Lj87
	movq	-8(%rax),%rax
	addq	$1,%rax
Lj87:
	subq	$1,%rax
	testl	%eax,%eax
	jnge	Lj89
	movl	$-1,-32(%rbp)
	.align 2
Lj90:
	addl	$1,-32(%rbp)
Ll79:
	movq	-16(%rbp),%rdx
	movq	32(%rdx),%rcx
	movslq	-32(%rbp),%rdx
	movq	(%rcx,%rdx,8),%rdx
	movl	24(%rdx),%edx
	cmpl	-28(%rbp),%edx
	jne	Lj94
Ll80:
	movq	-16(%rbp),%rdx
	movq	32(%rdx),%rdx
	movslq	-32(%rbp),%rcx
	movq	(%rdx,%rcx,8),%rdx
	movq	%rdx,-24(%rbp)
Ll81:
	jmp	Lj80
Lj94:
Ll82:
	cmpl	-32(%rbp),%eax
	jnle	Lj90
Lj89:
Lj80:
Ll83:
	movq	-24(%rbp),%rax
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt10:
Ll84:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRYPOSITION$TQUEUEENTRY$$LONGINT
_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRYPOSITION$TQUEUEENTRY$$LONGINT:
Ll85:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-32(%rsp),%rsp
	movq	%rdi,-16(%rbp)
	movq	%rsi,-8(%rbp)
Ll86:
	movl	$-1,-20(%rbp)
Ll87:
	movq	-16(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETQUEUELENGTH$$LONGINT
	subl	$1,%eax
	testl	%eax,%eax
	jnge	Lj98
	movl	$-1,-24(%rbp)
	.align 2
Lj99:
	addl	$1,-24(%rbp)
Ll88:
	movq	-16(%rbp),%rdx
	movq	32(%rdx),%rdx
	movslq	-24(%rbp),%rcx
	movq	(%rdx,%rcx,8),%rdx
	cmpq	-8(%rbp),%rdx
	jne	Lj103
Ll89:
	movl	-24(%rbp),%edx
	movl	%edx,-20(%rbp)
Ll90:
	jmp	Lj95
Lj103:
Ll91:
	cmpl	-24(%rbp),%eax
	jnle	Lj99
Lj98:
Lj95:
Ll92:
	movl	-20(%rbp),%eax
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt6:
Ll93:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_UPDATERISK$TQUEUEENTRY$TQUEUEENTRY$TPOINT
_CHITON$_$TROUTEFINDER_$__$$_UPDATERISK$TQUEUEENTRY$TQUEUEENTRY$TPOINT:
Ll94:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-48(%rsp),%rsp
	movq	%rdi,-32(%rbp)
	movq	%rsi,-8(%rbp)
	movq	%rdx,-16(%rbp)
	movq	%rcx,-24(%rbp)
Ll95:
	movq	-32(%rbp),%rax
	movq	24(%rax),%rax
	movslq	-24(%rbp),%rdx
	movq	(%rax,%rdx,8),%rax
	movslq	-20(%rbp),%rdx
	movq	-16(%rbp),%rcx
	movl	(%rax,%rdx,4),%edx
	movl	36(%rcx),%eax
	addl	%edx,%eax
	movl	%eax,-36(%rbp)
Ll96:
	movq	-8(%rbp),%rax
	cmpb	$0,44(%rax)
	jne	Lj106
	movq	-8(%rbp),%rax
	movl	36(%rax),%eax
	cmpl	-36(%rbp),%eax
	jng	Lj108
Lj106:
Ll97:
	movq	-8(%rbp),%rax
	movb	$0,44(%rax)
Ll98:
	movq	-8(%rbp),%rax
	movl	-36(%rbp),%edx
	movl	%edx,36(%rax)
Ll99:
	movq	-8(%rbp),%rdx
	movq	-16(%rbp),%rax
	movl	24(%rax),%eax
	movl	%eax,40(%rdx)
Lj108:
Ll100:
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt7:
Ll101:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_REMOVEFROMQUEUE$TQUEUEENTRY
_CHITON$_$TROUTEFINDER_$__$$_REMOVEFROMQUEUE$TQUEUEENTRY:
Ll102:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-176(%rsp),%rsp
	movq	%rbx,-168(%rbp)
	movq	%r12,-160(%rbp)
	movq	%rdi,-16(%rbp)
	movq	%rsi,-8(%rbp)
Ll103:
	movq	$0,-144(%rbp)
	movq	$0,-136(%rbp)
	leaq	-56(%rbp),%rdx
	leaq	-120(%rbp),%rsi
	movl	$1,%edi
	call	fpc_pushexceptaddr
	movq	%rax,%rdi
	call	fpc_setjmp
	movslq	%eax,%rdx
	movq	%rdx,-128(%rbp)
	testl	%eax,%eax
	jne	Lj112
Ll104:
	movq	-16(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETQUEUELENGTH$$LONGINT
	movl	%eax,-28(%rbp)
Ll105:
	call	fpc_get_output
	movq	%rax,%rbx
	leaq	-136(%rbp),%rdi
	call	fpc_ansistr_decr_ref
	leaq	-144(%rbp),%rdi
	call	fpc_ansistr_decr_ref
	leaq	-144(%rbp),%r12
	movq	%r12,%rdi
	call	fpc_ansistr_decr_ref
	movq	%r12,%rdx
	movslq	-28(%rbp),%rdi
	xorl	%ecx,%ecx
	movq	$-1,%rsi
	call	fpc_ansistr_sint
	movq	-144(%rbp),%rdx
	movl	$65535,%ecx
	leaq	_$$fpclocal$_ld1+24(%rip),%rsi
	leaq	-136(%rbp),%rdi
	call	fpc_ansistr_concat
	movq	-136(%rbp),%rdx
	movq	%rbx,%rsi
	xorl	%edi,%edi
	call	fpc_write_text_ansistr
	call	fpc_iocheck
	movq	%rbx,%rdi
	call	fpc_writeln_end
	call	fpc_iocheck
Ll106:
	movq	-8(%rbp),%rsi
	movq	-16(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRYPOSITION$TQUEUEENTRY$$LONGINT
	movl	%eax,-24(%rbp)
Ll107:
	cmpl	$-1,%eax
	jng	Lj114
Ll108:
	movl	-28(%rbp),%eax
	subl	$1,%eax
	cmpl	-24(%rbp),%eax
	jng	Lj116
Ll109:
	movl	-28(%rbp),%eax
	subl	$2,%eax
	cmpl	-24(%rbp),%eax
	jnge	Lj118
	movl	-24(%rbp),%edx
	subl	$1,%edx
	movl	%edx,-20(%rbp)
	.align 2
Lj119:
	addl	$1,-20(%rbp)
Ll110:
	movq	-16(%rbp),%rdx
	movq	32(%rdx),%rdi
	movslq	-20(%rbp),%r8
	movq	-16(%rbp),%rdx
	movq	32(%rdx),%rsi
	movslq	-20(%rbp),%rcx
	movq	8(%rdi,%r8,8),%rdx
	movq	%rdx,(%rsi,%rcx,8)
Ll111:
	cmpl	-20(%rbp),%eax
	jnle	Lj119
Lj118:
Lj116:
Ll112:
	movq	-16(%rbp),%rax
	movq	32(%rax),%rax
	testq	%rax,%rax
	je	Lj122
	movq	-8(%rax),%rax
	addq	$1,%rax
Lj122:
	subq	$1,%rax
	movq	%rax,-152(%rbp)
	movq	_INIT_$CHITON_$$_TQUEUE@GOTPCREL(%rip),%rsi
	movq	-16(%rbp),%rax
	leaq	32(%rax),%rdi
	leaq	-152(%rbp),%rcx
	movl	$1,%edx
	call	fpc_dynarray_setlength
Lj114:
Lj112:
Ll113:
	call	fpc_popaddrstack
Ll114:
	leaq	-144(%rbp),%rdi
	call	fpc_ansistr_decr_ref
	leaq	-136(%rbp),%rdi
	call	fpc_ansistr_decr_ref
Ll115:
	movq	-128(%rbp),%rax
	testq	%rax,%rax
	je	Lj111
	call	fpc_reraise
Lj111:
Ll116:
	movq	-168(%rbp),%rbx
	movq	-160(%rbp),%r12
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt8:
Ll117:

.text
	.align 3
.globl	_CHITON$_$TROUTEFINDER_$__$$_GETLEASTRISKYQUEUEENTRY$$TQUEUEENTRY
_CHITON$_$TROUTEFINDER_$__$$_GETLEASTRISKYQUEUEENTRY$$TQUEUEENTRY:
Ll118:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-32(%rsp),%rsp
	movq	%rdi,-8(%rbp)
Ll119:
	movb	$0,-32(%rbp)
Ll120:
	movq	$0,-16(%rbp)
Ll121:
	movl	$-1,-24(%rbp)
Ll122:
	movl	$-1,-28(%rbp)
Ll123:
	movq	-8(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETQUEUELENGTH$$LONGINT
	testl	%eax,%eax
	je	Lj123
Ll124:
	movq	-8(%rbp),%rdi
	call	_CHITON$_$TROUTEFINDER_$__$$_GETQUEUELENGTH$$LONGINT
	subl	$1,%eax
	testl	%eax,%eax
	jnge	Lj128
	movl	$-1,-20(%rbp)
	.align 2
Lj129:
	addl	$1,-20(%rbp)
Ll125:
	movq	-8(%rbp),%rdx
	movq	32(%rdx),%rdx
	movslq	-20(%rbp),%rcx
	movq	(%rdx,%rcx,8),%rdx
	cmpb	$0,44(%rdx)
	jne	Lj133
Ll126:
	movq	-8(%rbp),%rdx
	movq	32(%rdx),%rdx
	movslq	-20(%rbp),%rcx
	movq	(%rdx,%rcx,8),%rdx
	cmpb	$0,45(%rdx)
	jne	Lj133
Ll127:
	cmpb	$0,-32(%rbp)
	je	Lj135
	movq	-8(%rbp),%rdx
	movq	32(%rdx),%rdx
	movslq	-20(%rbp),%rcx
	movq	(%rdx,%rcx,8),%rdx
	movl	36(%rdx),%edx
	cmpl	-24(%rbp),%edx
	jnl	Lj133
Lj135:
Ll128:
	movb	$1,-32(%rbp)
Ll129:
	movq	-8(%rbp),%rdx
	movq	32(%rdx),%rdx
	movslq	-20(%rbp),%rcx
	movq	(%rdx,%rcx,8),%rdx
	movl	36(%rdx),%edx
	movl	%edx,-24(%rbp)
Ll130:
	movl	-20(%rbp),%edx
	movl	%edx,-28(%rbp)
Lj133:
Ll131:
	cmpl	-20(%rbp),%eax
	jnle	Lj129
Lj128:
Ll132:
	cmpl	$-1,-28(%rbp)
	jng	Lj140
	movq	-8(%rbp),%rax
	movq	32(%rax),%rdx
	movslq	-28(%rbp),%rax
	movq	(%rdx,%rax,8),%rax
	movq	%rax,-16(%rbp)
Lj140:
Lj123:
Ll133:
	movq	-16(%rbp),%rax
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt9:
Ll134:

.text
	.align 3
.globl	_CHITON$_$TQUEUEENTRY_$__$$_CREATE$TPOINT$TPOINT$$TQUEUEENTRY
_CHITON$_$TQUEUEENTRY_$__$$_CREATE$TPOINT$TPOINT$$TQUEUEENTRY:
Ll135:
	pushq	%rbp
	movq	%rsp,%rbp
	leaq	-240(%rsp),%rsp
	movq	%rdi,-32(%rbp)
	movq	%rsi,-24(%rbp)
	movq	%rdx,-8(%rbp)
	movq	%rcx,-16(%rbp)
Ll136:
	cmpq	$1,-24(%rbp)
	jne	Lj144
	movq	-32(%rbp),%rax
	movq	-32(%rbp),%rdx
	movq	%rax,%rdi
	call	*104(%rdx)
	movq	%rax,-32(%rbp)
Lj144:
	cmpq	$0,-32(%rbp)
	je	Lj141
	leaq	-64(%rbp),%rdx
	leaq	-128(%rbp),%rsi
	movl	$1,%edi
	call	fpc_pushexceptaddr
	movq	%rax,%rdi
	call	fpc_setjmp
	movslq	%eax,%rdx
	movq	%rdx,-136(%rbp)
	testl	%eax,%eax
	jne	Lj151
	movq	$-1,-40(%rbp)
Ll137:
	movq	-32(%rbp),%rax
	movb	$1,44(%rax)
Ll138:
	movl	-4(%rbp),%edx
	movl	-16(%rbp),%eax
	imull	%edx,%eax
	addl	-8(%rbp),%eax
	movq	-32(%rbp),%rdx
	movl	%eax,24(%rdx)
Ll139:
	movq	-32(%rbp),%rax
	movq	-8(%rbp),%rdx
	movq	%rdx,28(%rax)
Ll140:
	movq	-32(%rbp),%rax
	movb	$0,45(%rax)
Ll141:
	movq	-32(%rbp),%rax
	movl	$-1,40(%rax)
Ll142:
	movq	-32(%rbp),%rax
	movl	$0,36(%rax)
Ll143:
	movq	$1,-40(%rbp)
	cmpq	$0,-32(%rbp)
	je	Lj154
	cmpq	$0,-24(%rbp)
	je	Lj154
	movq	-32(%rbp),%rdi
	movq	-32(%rbp),%rax
	movq	(%rax),%rax
	call	*136(%rax)
Lj154:
Lj151:
Ll144:
	call	fpc_popaddrstack
	movq	-136(%rbp),%rax
	testq	%rax,%rax
	je	Lj149
	leaq	-160(%rbp),%rdx
	leaq	-224(%rbp),%rsi
	movl	$1,%edi
	call	fpc_pushexceptaddr
	movq	%rax,%rdi
	call	fpc_setjmp
	movslq	%eax,%rdx
	movq	%rdx,-232(%rbp)
	testl	%eax,%eax
	jne	Lj156
Ll145:
	cmpq	$0,-24(%rbp)
	je	Lj158
	movq	-40(%rbp),%rsi
	movq	-32(%rbp),%rdi
	movq	-32(%rbp),%rax
	movq	(%rax),%rax
	call	*96(%rax)
Lj158:
	call	fpc_popaddrstack
	call	fpc_reraise
Lj156:
Ll146:
	call	fpc_popaddrstack
	movq	-232(%rbp),%rax
	testq	%rax,%rax
	je	Lj159
	call	fpc_raise_nested
Lj159:
	call	fpc_doneexception
Lj149:
Lj141:
Ll147:
	movq	-32(%rbp),%rax
	movq	%rbp,%rsp
	popq	%rbp
	ret
Lt1:
Ll148:
# End asmlist al_procedures
# Begin asmlist al_globals

.const_data
	.align 3
.globl	_VMT_$CHITON_$$_TQUEUEENTRY
_VMT_$CHITON_$$_TQUEUEENTRY:
	.quad	48,-48
	.quad	_VMT_$SYSTEM_$$_TINTERFACEDOBJECT$indirect
	.quad	_$$fpclocal$_ld2
	.quad	0,0,0
	.quad	_RTTI_$CHITON_$$_TQUEUEENTRY
	.quad	0,0
	.quad	Ld3
	.quad	0
	.quad	_SYSTEM$_$TINTERFACEDOBJECT_$__$$_DESTROY
	.quad	_SYSTEM$_$TINTERFACEDOBJECT_$__$$_NEWINSTANCE$$TOBJECT
	.quad	_SYSTEM$_$TOBJECT_$__$$_FREEINSTANCE
	.quad	_SYSTEM$_$TOBJECT_$__$$_SAFECALLEXCEPTION$TOBJECT$POINTER$$HRESULT
	.quad	_SYSTEM$_$TOBJECT_$__$$_DEFAULTHANDLER$formal
	.quad	_SYSTEM$_$TINTERFACEDOBJECT_$__$$_AFTERCONSTRUCTION
	.quad	_SYSTEM$_$TINTERFACEDOBJECT_$__$$_BEFOREDESTRUCTION
	.quad	_SYSTEM$_$TOBJECT_$__$$_DEFAULTHANDLERSTR$formal
	.quad	_SYSTEM$_$TOBJECT_$__$$_DISPATCH$formal
	.quad	_SYSTEM$_$TOBJECT_$__$$_DISPATCHSTR$formal
	.quad	_SYSTEM$_$TOBJECT_$__$$_EQUALS$TOBJECT$$BOOLEAN
	.quad	_SYSTEM$_$TOBJECT_$__$$_GETHASHCODE$$INT64
	.quad	_SYSTEM$_$TOBJECT_$__$$_TOSTRING$$ANSISTRING
	.quad	0

.const_data
	.align 3
.globl	_VMT_$CHITON_$$_TROUTEFINDER
_VMT_$CHITON_$$_TROUTEFINDER:
	.quad	48,-48
	.quad	_VMT_$SYSTEM_$$_TINTERFACEDOBJECT$indirect
	.quad	_$$fpclocal$_ld4
	.quad	0,0,0
	.quad	_RTTI_$CHITON_$$_TROUTEFINDER
	.quad	_INIT_$CHITON_$$_TROUTEFINDER
	.quad	0
	.quad	Ld5
	.quad	0
	.quad	_SYSTEM$_$TINTERFACEDOBJECT_$__$$_DESTROY
	.quad	_SYSTEM$_$TINTERFACEDOBJECT_$__$$_NEWINSTANCE$$TOBJECT
	.quad	_SYSTEM$_$TOBJECT_$__$$_FREEINSTANCE
	.quad	_SYSTEM$_$TOBJECT_$__$$_SAFECALLEXCEPTION$TOBJECT$POINTER$$HRESULT
	.quad	_SYSTEM$_$TOBJECT_$__$$_DEFAULTHANDLER$formal
	.quad	_SYSTEM$_$TINTERFACEDOBJECT_$__$$_AFTERCONSTRUCTION
	.quad	_SYSTEM$_$TINTERFACEDOBJECT_$__$$_BEFOREDESTRUCTION
	.quad	_SYSTEM$_$TOBJECT_$__$$_DEFAULTHANDLERSTR$formal
	.quad	_SYSTEM$_$TOBJECT_$__$$_DISPATCH$formal
	.quad	_SYSTEM$_$TOBJECT_$__$$_DISPATCHSTR$formal
	.quad	_SYSTEM$_$TOBJECT_$__$$_EQUALS$TOBJECT$$BOOLEAN
	.quad	_SYSTEM$_$TOBJECT_$__$$_GETHASHCODE$$INT64
	.quad	_SYSTEM$_$TOBJECT_$__$$_TOSTRING$$ANSISTRING
	.quad	0
# End asmlist al_globals
# Begin asmlist al_const

.const_data
	.align 3
_$$fpclocal$_ld2:
	.byte	11
	.ascii	"TQueueEntry"

.const_data
	.align 3
Ld3:
	.quad	0

.const_data
	.align 3
_$$fpclocal$_ld4:
	.byte	12
	.ascii	"TRouteFinder"

.const_data
	.align 3
Ld5:
	.quad	0
# End asmlist al_const
# Begin asmlist al_typedconsts

.const
	.align 3
_$$fpclocal$_ld1:
	.short	0,1
	.long	0
	.quad	-1,13
	.ascii	"queue length \000"
# End asmlist al_typedconsts
# Begin asmlist al_rtti

.const_data
	.align 3
.globl	_INIT_$CHITON_$$_TQUEUEENTRY
_INIT_$CHITON_$$_TQUEUEENTRY:
	.byte	15,11
	.ascii	"TQueueEntry"
	.quad	0
	.long	8
	.quad	0,0
	.long	0

.const_data
	.align 3
.globl	_RTTI_$CHITON_$$_TQUEUEENTRY
_RTTI_$CHITON_$$_TQUEUEENTRY:
	.byte	15,11
	.ascii	"TQueueEntry"
	.quad	_VMT_$CHITON_$$_TQUEUEENTRY
	.quad	_RTTI_$SYSTEM_$$_TINTERFACEDOBJECT$indirect
	.short	0
	.byte	6
	.ascii	"chiton"
	.short	0

.const_data
	.align 3
.globl	_INIT_$CHITON_$$_TQUEUE
_INIT_$CHITON_$$_TQUEUE:
	.byte	21,6
	.ascii	"TQueue"
	.quad	8
	.quad	_INIT_$CHITON_$$_TQUEUEENTRY$indirect
	.long	-1
	.quad	0
	.byte	6
	.ascii	"chiton"

.const_data
	.align 3
.globl	_RTTI_$CHITON_$$_TQUEUE
_RTTI_$CHITON_$$_TQUEUE:
	.byte	21,6
	.ascii	"TQueue"
	.quad	8
	.quad	_RTTI_$CHITON_$$_TQUEUEENTRY$indirect
	.long	-1
	.quad	0
	.byte	6
	.ascii	"chiton"

.const_data
	.align 3
.globl	_INIT_$CHITON_$$_TROUTEFINDER
_INIT_$CHITON_$$_TROUTEFINDER:
	.byte	15,12
	.ascii	"TRouteFinder"
	.quad	0
	.long	8
	.quad	0,0
	.long	2
	.quad	_RTTI_$ARRAYUTILS_$$_T2DINTMAP$indirect
	.quad	24
	.quad	_INIT_$CHITON_$$_TQUEUE$indirect
	.quad	32

.const_data
	.align 3
.globl	_RTTI_$CHITON_$$_TROUTEFINDER
_RTTI_$CHITON_$$_TROUTEFINDER:
	.byte	15,12
	.ascii	"TRouteFinder"
	.quad	_VMT_$CHITON_$$_TROUTEFINDER
	.quad	_RTTI_$SYSTEM_$$_TINTERFACEDOBJECT$indirect
	.short	0
	.byte	6
	.ascii	"chiton"
	.short	0
# End asmlist al_rtti
# Begin asmlist al_indirectglobals

.const_data
	.align 3
.globl	_VMT_$CHITON_$$_TQUEUEENTRY$indirect
_VMT_$CHITON_$$_TQUEUEENTRY$indirect:
	.quad	_VMT_$CHITON_$$_TQUEUEENTRY

.const_data
	.align 3
.globl	_VMT_$CHITON_$$_TROUTEFINDER$indirect
_VMT_$CHITON_$$_TROUTEFINDER$indirect:
	.quad	_VMT_$CHITON_$$_TROUTEFINDER

.const_data
	.align 3
.globl	_INIT_$CHITON_$$_TQUEUEENTRY$indirect
_INIT_$CHITON_$$_TQUEUEENTRY$indirect:
	.quad	_INIT_$CHITON_$$_TQUEUEENTRY

.const_data
	.align 3
.globl	_RTTI_$CHITON_$$_TQUEUEENTRY$indirect
_RTTI_$CHITON_$$_TQUEUEENTRY$indirect:
	.quad	_RTTI_$CHITON_$$_TQUEUEENTRY

.const_data
	.align 3
.globl	_INIT_$CHITON_$$_TQUEUE$indirect
_INIT_$CHITON_$$_TQUEUE$indirect:
	.quad	_INIT_$CHITON_$$_TQUEUE

.const_data
	.align 3
.globl	_RTTI_$CHITON_$$_TQUEUE$indirect
_RTTI_$CHITON_$$_TQUEUE$indirect:
	.quad	_RTTI_$CHITON_$$_TQUEUE

.const_data
	.align 3
.globl	_INIT_$CHITON_$$_TROUTEFINDER$indirect
_INIT_$CHITON_$$_TROUTEFINDER$indirect:
	.quad	_INIT_$CHITON_$$_TROUTEFINDER

.const_data
	.align 3
.globl	_RTTI_$CHITON_$$_TROUTEFINDER$indirect
_RTTI_$CHITON_$$_TROUTEFINDER$indirect:
	.quad	_RTTI_$CHITON_$$_TROUTEFINDER
# End asmlist al_indirectglobals
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
	.ascii	"chiton.pas\000"
	.ascii	"Free Pascal 3.2.0 2020/05/31\000"
	.ascii	"/Users/cloudsoft/Code/advent-of-code/2021/\000"
	.byte	9
	.byte	3
	.long	L$set$3
	.set L$set$3,Ldebug_line0-Ldebug_linesection0
	.quad	L_DEBUGSTART_$CHITON
	.quad	L_DEBUGEND_$CHITON
# Syms - Begin unit CHITON has index 18
# Symbol CHITON
# Symbol SYSTEM
# Symbol OBJPAS
# Symbol CLASSES
# Symbol SYSUTILS
# Symbol ARRAYUTILS
# Symbol TQUEUEENTRY
# Symbol vmtdef$TQUEUEENTRY
# Symbol TQUEUE
# Symbol TROUTEFINDER
# Symbol vmtdef$TROUTEFINDER
# Syms - End unit CHITON has index 18
# Syms - Begin Staticsymtable
# Symbol _CHITON_$$_init$
# Symbol ansistrrec13
# Symbol rttidef$_INIT_$CHITON_$$_TQUEUEENTRY
# Symbol rttidef$_RTTI_$CHITON_$$_TQUEUEENTRY
# Symbol rttidef$_INIT_$CHITON_$$_TQUEUE
# Symbol rtti_dyn_array$
# Symbol rttidef$_RTTI_$CHITON_$$_TQUEUE
# Symbol rttidef$_INIT_$CHITON_$$_TROUTEFINDER
# Symbol rttidef$_RTTI_$CHITON_$$_TROUTEFINDER
# Syms - End Staticsymtable
# Defs - Begin unit SYSTEM has index 1
# Defs - End unit SYSTEM has index 1
# Defs - Begin unit OBJPAS has index 6
# Defs - End unit OBJPAS has index 6
# Defs - Begin unit UNIXTYPE has index 24
# Defs - End unit UNIXTYPE has index 24
# Defs - Begin unit SYSCTL has index 28
# Defs - End unit SYSCTL has index 28
# Defs - Begin unit BASEUNIX has index 21
# Defs - End unit BASEUNIX has index 21
# Defs - Begin unit UNIXUTIL has index 29
# Defs - End unit UNIXUTIL has index 29
# Defs - Begin unit UNIX has index 23
# Defs - End unit UNIX has index 23
# Defs - Begin unit ERRORS has index 46
# Defs - End unit ERRORS has index 46
# Defs - Begin unit SYSCONST has index 47
# Defs - End unit SYSCONST has index 47
# Defs - Begin unit CTYPES has index 22
# Defs - End unit CTYPES has index 22
# Defs - Begin unit INITC has index 25
# Defs - End unit INITC has index 25
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
# Defs - Begin unit ANYSORT has index 13
# Defs - End unit ANYSORT has index 13
# Defs - Begin unit CONTNRS has index 98
# Defs - End unit CONTNRS has index 98
# Defs - Begin unit LAZ_AVL_TREE has index 60
# Defs - End unit LAZ_AVL_TREE has index 60
# Defs - Begin unit FPIMAGE has index 35
# Defs - End unit FPIMAGE has index 35
# Defs - Begin unit CLIPPING has index 108
# Defs - End unit CLIPPING has index 108
# Defs - Begin unit FPCANVAS has index 99
# Defs - End unit FPCANVAS has index 99
# Defs - Begin unit FPIMGCMN has index 80
# Defs - End unit FPIMGCMN has index 80
# Defs - Begin unit BMPCOMN has index 72
# Defs - End unit BMPCOMN has index 72
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
# Defs - Begin unit FPWRITEPNG has index 74
# Defs - End unit FPWRITEPNG has index 74
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
# Defs - Begin unit FPTIFFCMN has index 77
# Defs - End unit FPTIFFCMN has index 77
# Defs - Begin unit FPREADTIFF has index 75
# Defs - End unit FPREADTIFF has index 75
# Defs - Begin unit FPREADGIF has index 104
# Defs - End unit FPREADGIF has index 104
# Defs - Begin unit FPCADDS has index 50
# Defs - End unit FPCADDS has index 50
# Defs - Begin unit DL has index 30
# Defs - End unit DL has index 30
# Defs - Begin unit DYNLIBS has index 26
# Defs - End unit DYNLIBS has index 26
# Defs - Begin unit UNIXCP has index 27
# Defs - End unit UNIXCP has index 27
# Defs - Begin unit CWSTRING has index 19
# Defs - End unit CWSTRING has index 19
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
# Defs - Begin unit LAZUTILSSTRCONSTS has index 68
# Defs - End unit LAZUTILSSTRCONSTS has index 68
# Defs - Begin unit LAZFILEUTILS has index 61
# Defs - End unit LAZFILEUTILS has index 61
# Defs - Begin unit LAZUTF8CLASSES has index 64
# Defs - End unit LAZUTF8CLASSES has index 64
# Defs - Begin unit LAZMETHODLIST has index 63
# Defs - End unit LAZMETHODLIST has index 63
# Defs - Begin unit LAZCLASSES has index 69
# Defs - End unit LAZCLASSES has index 69
# Defs - Begin unit LAZLOGGERBASE has index 65
# Defs - End unit LAZLOGGERBASE has index 65
# Defs - Begin unit LAZUTILITIES has index 62
# Defs - End unit LAZUTILITIES has index 62
# Defs - Begin unit LAZVERSION has index 95
# Defs - End unit LAZVERSION has index 95
# Defs - Begin unit LCLVERSION has index 78
# Defs - End unit LCLVERSION has index 78
# Defs - Begin unit LCLSTRCONSTS has index 67
# Defs - End unit LCLSTRCONSTS has index 67
# Defs - Begin unit LCLTYPE has index 38
# Defs - End unit LCLTYPE has index 38
# Defs - Begin unit LAZTRACER has index 66
# Defs - End unit LAZTRACER has index 66
# Defs - Begin unit LCLPROC has index 39
# Defs - End unit LCLPROC has index 39
# Defs - Begin unit GRAPHTYPE has index 42
# Defs - End unit GRAPHTYPE has index 42
# Defs - Begin unit LMESSAGES has index 40
# Defs - End unit LMESSAGES has index 40
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
# Defs - Begin unit FPREADBMP has index 70
# Defs - End unit FPREADBMP has index 70
# Defs - Begin unit FPREADPNG has index 73
# Defs - End unit FPREADPNG has index 73
# Defs - Begin unit FPWRITETIFF has index 76
# Defs - End unit FPWRITETIFF has index 76
# Defs - Begin unit ICNSTYPES has index 79
# Defs - End unit ICNSTYPES has index 79
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
# Defs - Begin unit FGL has index 59
# Defs - End unit FGL has index 59
# Defs - Begin unit INTEGERLIST has index 37
# Defs - End unit INTEGERLIST has index 37
# Defs - Begin unit LCLPLATFORMDEF has index 41
# Defs - End unit LCLPLATFORMDEF has index 41
# Defs - Begin unit GRAPHMATH has index 43
# Defs - End unit GRAPHMATH has index 43
# Defs - Begin unit TMSCHEMA has index 177
# Defs - End unit TMSCHEMA has index 177
# Defs - Begin unit THEMES has index 45
# Defs - End unit THEMES has index 45
# Defs - Begin unit INTERFACEBASE has index 20
# Defs - End unit INTERFACEBASE has index 20
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
# Defs - Begin unit INTFGRAPHICS has index 44
# Defs - End unit INTFGRAPHICS has index 44
# Defs - Begin unit GRAPHICS has index 96
# Defs - End unit GRAPHICS has index 96
# Defs - Begin unit ARRAYUTILS has index 12
# Defs - End unit ARRAYUTILS has index 12
# Defs - Begin unit CHITON has index 18
# Definition TQueueEntry
La1:
	.byte	2
	.ascii	"TQueueEntry\000"
	.long	L$set$4
	.set L$set$4,La29-Ldebug_info0
La29:
	.byte	3
	.long	L$set$5
	.set L$set$5,La3-Ldebug_info0
La3:
	.byte	4
	.ascii	"TQueueEntry\000"
	.byte	48
	.byte	5
	.byte	1
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$6
	.set L$set$6,La32-Ldebug_info0
	.byte	6
	.ascii	"fNodeId\000"
	.byte	2
	.byte	35
	.byte	24
	.byte	3
	.long	L$set$7
	.set L$set$7,La33-Ldebug_info0
	.byte	6
	.ascii	"fNodePos\000"
	.byte	2
	.byte	35
	.byte	28
	.byte	3
	.long	L$set$8
	.set L$set$8,La35-Ldebug_info0
	.byte	6
	.ascii	"fRisk\000"
	.byte	2
	.byte	35
	.byte	36
	.byte	3
	.long	L$set$9
	.set L$set$9,La33-Ldebug_info0
	.byte	6
	.ascii	"fSource\000"
	.byte	2
	.byte	35
	.byte	40
	.byte	3
	.long	L$set$10
	.set L$set$10,La33-Ldebug_info0
	.byte	6
	.ascii	"fInfinity\000"
	.byte	2
	.byte	35
	.byte	44
	.byte	3
	.long	L$set$11
	.set L$set$11,La37-Ldebug_info0
	.byte	6
	.ascii	"fVisited\000"
	.byte	2
	.byte	35
	.byte	45
	.byte	3
	.long	L$set$12
	.set L$set$12,La37-Ldebug_info0
	.byte	6
	.ascii	"nodeId\000"
	.byte	2
	.byte	35
	.byte	24
	.byte	3
	.long	L$set$13
	.set L$set$13,La33-Ldebug_info0
	.byte	6
	.ascii	"nodePos\000"
	.byte	2
	.byte	35
	.byte	28
	.byte	3
	.long	L$set$14
	.set L$set$14,La35-Ldebug_info0
	.byte	6
	.ascii	"risk\000"
	.byte	2
	.byte	35
	.byte	36
	.byte	3
	.long	L$set$15
	.set L$set$15,La33-Ldebug_info0
	.byte	6
	.ascii	"source\000"
	.byte	2
	.byte	35
	.byte	40
	.byte	3
	.long	L$set$16
	.set L$set$16,La33-Ldebug_info0
	.byte	6
	.ascii	"infinity\000"
	.byte	2
	.byte	35
	.byte	44
	.byte	3
	.long	L$set$17
	.set L$set$17,La37-Ldebug_info0
	.byte	6
	.ascii	"visited\000"
	.byte	2
	.byte	35
	.byte	45
	.byte	3
	.long	L$set$18
	.set L$set$18,La37-Ldebug_info0
# Procdef constructor create(<TQueueEntry>;<Class Of TQueueEntry>;TPoint;TPoint);
	.byte	7
	.ascii	"create\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$19
	.set L$set$19,La1-Ldebug_info0
	.quad	_CHITON$_$TQUEUEENTRY_$__$$_CREATE$TPOINT$TPOINT$$TQUEUEENTRY
	.quad	Lt1
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	96
	.byte	1
	.long	L$set$20
	.set L$set$20,La1-Ldebug_info0
# Symbol vmt
	.byte	9
	.ascii	"$vmt\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$21
	.set L$set$21,La39-Ldebug_info0
# Symbol POSITION
	.byte	9
	.ascii	"position\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$22
	.set L$set$22,La35-Ldebug_info0
# Symbol MAPDIMENSIONS
	.byte	9
	.ascii	"mapDimensions\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$23
	.set L$set$23,La35-Ldebug_info0
# Symbol vmt_afterconstruction_local
	.byte	10
	.ascii	"$vmt_afterconstruction_local\000"
	.byte	2
	.byte	118
	.byte	88
	.long	L$set$24
	.set L$set$24,La41-Ldebug_info0
	.byte	0
	.byte	0
La2:
	.byte	11
	.long	L$set$25
	.set L$set$25,La1-Ldebug_info0
# Definition <record type>
La4:
	.byte	2
	.ascii	"$vmtdef$TQUEUEENTRY\000"
	.long	L$set$26
	.set L$set$26,La43-Ldebug_info0
La43:
	.byte	12
	.ascii	"$VMTDEF$TQUEUEENTRY\000"
	.byte	208,1
	.byte	0
La5:
	.byte	11
	.long	L$set$27
	.set L$set$27,La4-Ldebug_info0
# Definition TQueue
La6:
	.byte	2
	.ascii	"TQueue\000"
	.long	L$set$28
	.set L$set$28,La44-Ldebug_info0
La44:
	.byte	13
	.ascii	"TQueue\000"
	.byte	2
	.byte	151
	.byte	6
	.long	L$set$29
	.set L$set$29,La1-Ldebug_info0
	.byte	14
	.byte	8
	.byte	0
	.byte	14
	.byte	151
	.byte	6
	.byte	18
	.byte	40
	.short	5
	.byte	9
	.byte	255
	.byte	47
	.short	3
	.byte	56
	.byte	28
	.byte	6
	.long	L$set$30
	.set L$set$30,La33-Ldebug_info0
	.byte	0
La7:
	.byte	11
	.long	L$set$31
	.set L$set$31,La6-Ldebug_info0
# Definition TRouteFinder
La8:
	.byte	2
	.ascii	"TRouteFinder\000"
	.long	L$set$32
	.set L$set$32,La45-Ldebug_info0
La45:
	.byte	3
	.long	L$set$33
	.set L$set$33,La10-Ldebug_info0
La10:
	.byte	4
	.ascii	"TRouteFinder\000"
	.byte	48
	.byte	5
	.byte	1
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$34
	.set L$set$34,La32-Ldebug_info0
	.byte	6
	.ascii	"fMap\000"
	.byte	2
	.byte	35
	.byte	24
	.byte	3
	.long	L$set$35
	.set L$set$35,La46-Ldebug_info0
	.byte	6
	.ascii	"fQueue\000"
	.byte	2
	.byte	35
	.byte	32
	.byte	3
	.long	L$set$36
	.set L$set$36,La6-Ldebug_info0
	.byte	6
	.ascii	"fShortest\000"
	.byte	2
	.byte	35
	.byte	40
	.byte	3
	.long	L$set$37
	.set L$set$37,La33-Ldebug_info0
	.byte	6
	.ascii	"shortest\000"
	.byte	2
	.byte	35
	.byte	40
	.byte	3
	.long	L$set$38
	.set L$set$38,La33-Ldebug_info0
# Procdef initializeMapAndQueue(<TRouteFinder>;TStringArray);
	.byte	15
	.ascii	"initializeMapAndQueue\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.quad	_CHITON$_$TROUTEFINDER_$__$$_INITIALIZEMAPANDQUEUE$TSTRINGARRAY
	.quad	Lt2
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	112
	.byte	1
	.long	L$set$39
	.set L$set$39,La8-Ldebug_info0
# Symbol PUZZLEINPUT
	.byte	9
	.ascii	"puzzleInput\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$40
	.set L$set$40,La48-Ldebug_info0
# Symbol PUZZLEWIDTH
	.byte	10
	.ascii	"puzzleWidth\000"
	.byte	2
	.byte	118
	.byte	108
	.long	L$set$41
	.set L$set$41,La33-Ldebug_info0
# Symbol PUZZLEHEIGHT
	.byte	10
	.ascii	"puzzleHeight\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$42
	.set L$set$42,La33-Ldebug_info0
# Symbol X
	.byte	10
	.ascii	"x\000"
	.byte	2
	.byte	118
	.byte	100
	.long	L$set$43
	.set L$set$43,La33-Ldebug_info0
# Symbol Y
	.byte	10
	.ascii	"y\000"
	.byte	2
	.byte	118
	.byte	96
	.long	L$set$44
	.set L$set$44,La33-Ldebug_info0
# Symbol SLINE
	.byte	10
	.ascii	"sLine\000"
	.byte	2
	.byte	118
	.byte	88
	.long	L$set$45
	.set L$set$45,La50-Ldebug_info0
# Symbol NODEPOSITION
	.byte	10
	.ascii	"nodePosition\000"
	.byte	2
	.byte	118
	.byte	80
	.long	L$set$46
	.set L$set$46,La35-Ldebug_info0
	.byte	0
# Procdef getQueueLength(<TRouteFinder>):LongInt;
	.byte	16
	.ascii	"getQueueLength\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.long	L$set$47
	.set L$set$47,La33-Ldebug_info0
	.quad	_CHITON$_$TROUTEFINDER_$__$$_GETQUEUELENGTH$$LONGINT
	.quad	Lt3
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	120
	.byte	1
	.long	L$set$48
	.set L$set$48,La8-Ldebug_info0
# Symbol result
	.byte	10
	.ascii	"$result\000"
	.byte	2
	.byte	118
	.byte	116
	.long	L$set$49
	.set L$set$49,La33-Ldebug_info0
# Symbol GETQUEUELENGTH
	.byte	10
	.ascii	"GETQUEUELENGTH\000"
	.byte	2
	.byte	118
	.byte	116
	.long	L$set$50
	.set L$set$50,La33-Ldebug_info0
# Symbol RESULT
	.byte	10
	.ascii	"RESULT\000"
	.byte	2
	.byte	118
	.byte	116
	.long	L$set$51
	.set L$set$51,La33-Ldebug_info0
	.byte	0
# Procdef getMapDimensions(<TRouteFinder>):<record type>;
	.byte	16
	.ascii	"getMapDimensions\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.long	L$set$52
	.set L$set$52,La35-Ldebug_info0
	.quad	_CHITON$_$TROUTEFINDER_$__$$_GETMAPDIMENSIONS$$TPOINT
	.quad	Lt4
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	120
	.byte	1
	.long	L$set$53
	.set L$set$53,La8-Ldebug_info0
# Symbol result
	.byte	10
	.ascii	"$result\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$54
	.set L$set$54,La35-Ldebug_info0
# Symbol GETMAPDIMENSIONS
	.byte	10
	.ascii	"GETMAPDIMENSIONS\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$55
	.set L$set$55,La35-Ldebug_info0
# Symbol RESULT
	.byte	10
	.ascii	"RESULT\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$56
	.set L$set$56,La35-Ldebug_info0
	.byte	0
# Procdef processNode(<TRouteFinder>;TQueueEntry;TQueueEntry);
	.byte	15
	.ascii	"processNode\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.quad	_CHITON$_$TROUTEFINDER_$__$$_PROCESSNODE$TQUEUEENTRY$TQUEUEENTRY
	.quad	Lt5
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	104
	.byte	1
	.long	L$set$57
	.set L$set$57,La8-Ldebug_info0
# Symbol STARTENTRY
	.byte	9
	.ascii	"startEntry\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$58
	.set L$set$58,La1-Ldebug_info0
# Symbol ENDENTRY
	.byte	9
	.ascii	"endEntry\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$59
	.set L$set$59,La1-Ldebug_info0
# Symbol VISITINGQUEUEENTRY
	.byte	10
	.ascii	"visitingQueueEntry\000"
	.byte	2
	.byte	118
	.byte	96
	.long	L$set$60
	.set L$set$60,La1-Ldebug_info0
# Symbol LEASTRQENTRY
	.byte	10
	.ascii	"leastRQEntry\000"
	.byte	2
	.byte	118
	.byte	88
	.long	L$set$61
	.set L$set$61,La1-Ldebug_info0
# Symbol VISITINGPOINT
	.byte	10
	.ascii	"visitingPoint\000"
	.byte	2
	.byte	118
	.byte	80
	.long	L$set$62
	.set L$set$62,La35-Ldebug_info0
# Symbol XOFFSET
	.byte	10
	.ascii	"xOffset\000"
	.byte	2
	.byte	118
	.byte	76
	.long	L$set$63
	.set L$set$63,La33-Ldebug_info0
# Symbol YOFFSET
	.byte	10
	.ascii	"yOffset\000"
	.byte	2
	.byte	118
	.byte	72
	.long	L$set$64
	.set L$set$64,La33-Ldebug_info0
	.byte	0
# Procdef findQueueEntryPosition(<TRouteFinder>;TQueueEntry):LongInt;
	.byte	16
	.ascii	"findQueueEntryPosition\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.long	L$set$65
	.set L$set$65,La33-Ldebug_info0
	.quad	_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRYPOSITION$TQUEUEENTRY$$LONGINT
	.quad	Lt6
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	112
	.byte	1
	.long	L$set$66
	.set L$set$66,La8-Ldebug_info0
# Symbol QUEUEENTRY
	.byte	9
	.ascii	"queueEntry\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$67
	.set L$set$67,La1-Ldebug_info0
# Symbol result
	.byte	10
	.ascii	"$result\000"
	.byte	2
	.byte	118
	.byte	108
	.long	L$set$68
	.set L$set$68,La33-Ldebug_info0
# Symbol FINDQUEUEENTRYPOSITION
	.byte	10
	.ascii	"FINDQUEUEENTRYPOSITION\000"
	.byte	2
	.byte	118
	.byte	108
	.long	L$set$69
	.set L$set$69,La33-Ldebug_info0
# Symbol RESULT
	.byte	10
	.ascii	"RESULT\000"
	.byte	2
	.byte	118
	.byte	108
	.long	L$set$70
	.set L$set$70,La33-Ldebug_info0
# Symbol INDEX
	.byte	10
	.ascii	"index\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$71
	.set L$set$71,La33-Ldebug_info0
	.byte	0
# Procdef updateRisk(<TRouteFinder>;TQueueEntry;TQueueEntry;TPoint);
	.byte	15
	.ascii	"updateRisk\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.quad	_CHITON$_$TROUTEFINDER_$__$$_UPDATERISK$TQUEUEENTRY$TQUEUEENTRY$TPOINT
	.quad	Lt7
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	96
	.byte	1
	.long	L$set$72
	.set L$set$72,La8-Ldebug_info0
# Symbol QUEUEENTRY
	.byte	9
	.ascii	"queueEntry\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$73
	.set L$set$73,La1-Ldebug_info0
# Symbol SOURCEENTRY
	.byte	9
	.ascii	"sourceEntry\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$74
	.set L$set$74,La1-Ldebug_info0
# Symbol POSITION
	.byte	9
	.ascii	"position\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$75
	.set L$set$75,La35-Ldebug_info0
# Symbol RISKTOVISITINGPOINT
	.byte	10
	.ascii	"riskToVisitingPoint\000"
	.byte	2
	.byte	118
	.byte	92
	.long	L$set$76
	.set L$set$76,La33-Ldebug_info0
	.byte	0
# Procdef removeFromQueue(<TRouteFinder>;TQueueEntry);
	.byte	15
	.ascii	"removeFromQueue\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.quad	_CHITON$_$TROUTEFINDER_$__$$_REMOVEFROMQUEUE$TQUEUEENTRY
	.quad	Lt8
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	112
	.byte	1
	.long	L$set$77
	.set L$set$77,La8-Ldebug_info0
# Symbol QUEUEENTRY
	.byte	9
	.ascii	"queueEntry\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$78
	.set L$set$78,La1-Ldebug_info0
# Symbol INDEX
	.byte	10
	.ascii	"index\000"
	.byte	2
	.byte	118
	.byte	108
	.long	L$set$79
	.set L$set$79,La33-Ldebug_info0
# Symbol DELETEINDEX
	.byte	10
	.ascii	"deleteIndex\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$80
	.set L$set$80,La33-Ldebug_info0
# Symbol LENGTHOFQUEUE
	.byte	10
	.ascii	"lengthOfQueue\000"
	.byte	2
	.byte	118
	.byte	100
	.long	L$set$81
	.set L$set$81,La33-Ldebug_info0
	.byte	0
# Procdef getLeastRiskyQueueEntry(<TRouteFinder>):TQueueEntry;
	.byte	16
	.ascii	"getLeastRiskyQueueEntry\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.long	L$set$82
	.set L$set$82,La1-Ldebug_info0
	.quad	_CHITON$_$TROUTEFINDER_$__$$_GETLEASTRISKYQUEUEENTRY$$TQUEUEENTRY
	.quad	Lt9
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	120
	.byte	1
	.long	L$set$83
	.set L$set$83,La8-Ldebug_info0
# Symbol result
	.byte	10
	.ascii	"$result\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$84
	.set L$set$84,La1-Ldebug_info0
# Symbol GETLEASTRISKYQUEUEENTRY
	.byte	10
	.ascii	"GETLEASTRISKYQUEUEENTRY\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$85
	.set L$set$85,La1-Ldebug_info0
# Symbol RESULT
	.byte	10
	.ascii	"RESULT\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$86
	.set L$set$86,La1-Ldebug_info0
# Symbol INDEX
	.byte	10
	.ascii	"index\000"
	.byte	2
	.byte	118
	.byte	108
	.long	L$set$87
	.set L$set$87,La33-Ldebug_info0
# Symbol MINRISK
	.byte	10
	.ascii	"minRisk\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$88
	.set L$set$88,La33-Ldebug_info0
# Symbol MINRISKINDEX
	.byte	10
	.ascii	"minRiskIndex\000"
	.byte	2
	.byte	118
	.byte	100
	.long	L$set$89
	.set L$set$89,La33-Ldebug_info0
# Symbol FIRSTFOUND
	.byte	10
	.ascii	"firstFound\000"
	.byte	2
	.byte	118
	.byte	96
	.long	L$set$90
	.set L$set$90,La37-Ldebug_info0
	.byte	0
# Procdef findQueueEntry(<TRouteFinder>;TPoint):TQueueEntry;
	.byte	16
	.ascii	"findQueueEntry\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.long	L$set$91
	.set L$set$91,La1-Ldebug_info0
	.quad	_CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRY$TPOINT$$TQUEUEENTRY
	.quad	Lt10
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	112
	.byte	1
	.long	L$set$92
	.set L$set$92,La8-Ldebug_info0
# Symbol ATPOINT
	.byte	9
	.ascii	"atPoint\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$93
	.set L$set$93,La35-Ldebug_info0
# Symbol result
	.byte	10
	.ascii	"$result\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$94
	.set L$set$94,La1-Ldebug_info0
# Symbol FINDQUEUEENTRY
	.byte	10
	.ascii	"FINDQUEUEENTRY\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$95
	.set L$set$95,La1-Ldebug_info0
# Symbol RESULT
	.byte	10
	.ascii	"RESULT\000"
	.byte	2
	.byte	118
	.byte	104
	.long	L$set$96
	.set L$set$96,La1-Ldebug_info0
# Symbol IDOFENTRY
	.byte	10
	.ascii	"idOfEntry\000"
	.byte	2
	.byte	118
	.byte	100
	.long	L$set$97
	.set L$set$97,La33-Ldebug_info0
# Symbol INDEX
	.byte	10
	.ascii	"index\000"
	.byte	2
	.byte	118
	.byte	96
	.long	L$set$98
	.set L$set$98,La33-Ldebug_info0
	.byte	0
# Procdef constructor create(<TRouteFinder>;<Class Of TRouteFinder>;TStringArray);
	.byte	7
	.ascii	"create\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$99
	.set L$set$99,La8-Ldebug_info0
	.quad	_CHITON$_$TROUTEFINDER_$__$$_CREATE$TSTRINGARRAY$$TROUTEFINDER
	.quad	Lt11
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	104
	.byte	1
	.long	L$set$100
	.set L$set$100,La8-Ldebug_info0
# Symbol vmt
	.byte	9
	.ascii	"$vmt\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$101
	.set L$set$101,La52-Ldebug_info0
# Symbol PUZZLEINPUT
	.byte	9
	.ascii	"puzzleInput\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$102
	.set L$set$102,La48-Ldebug_info0
# Symbol vmt_afterconstruction_local
	.byte	10
	.ascii	"$vmt_afterconstruction_local\000"
	.byte	2
	.byte	118
	.byte	96
	.long	L$set$103
	.set L$set$103,La41-Ldebug_info0
	.byte	0
# Procdef findShortestPath(<TRouteFinder>;TPoint;TPoint);
	.byte	17
	.ascii	"findShortestPath\000"
	.byte	1
	.byte	65
	.byte	1
	.quad	_CHITON$_$TROUTEFINDER_$__$$_FINDSHORTESTPATH$TPOINT$TPOINT
	.quad	Lt12
# Symbol this
	.byte	8
	.ascii	"this\000"
	.byte	2
	.byte	118
	.byte	104
	.byte	1
	.long	L$set$104
	.set L$set$104,La8-Ldebug_info0
# Symbol STARTPOINT
	.byte	9
	.ascii	"startPoint\000"
	.byte	2
	.byte	118
	.byte	120
	.long	L$set$105
	.set L$set$105,La35-Ldebug_info0
# Symbol ENDPOINT
	.byte	9
	.ascii	"endPoint\000"
	.byte	2
	.byte	118
	.byte	112
	.long	L$set$106
	.set L$set$106,La35-Ldebug_info0
# Symbol STARTENTRY
	.byte	10
	.ascii	"startEntry\000"
	.byte	2
	.byte	118
	.byte	96
	.long	L$set$107
	.set L$set$107,La1-Ldebug_info0
# Symbol ENDENTRY
	.byte	10
	.ascii	"endEntry\000"
	.byte	2
	.byte	118
	.byte	88
	.long	L$set$108
	.set L$set$108,La1-Ldebug_info0
	.byte	0
	.byte	0
La9:
	.byte	11
	.long	L$set$109
	.set L$set$109,La8-Ldebug_info0
# Definition <record type>
La11:
	.byte	2
	.ascii	"$vmtdef$TROUTEFINDER\000"
	.long	L$set$110
	.set L$set$110,La54-Ldebug_info0
La54:
	.byte	12
	.ascii	"$VMTDEF$TROUTEFINDER\000"
	.byte	208,1
	.byte	0
La12:
	.byte	11
	.long	L$set$111
	.set L$set$111,La11-Ldebug_info0
# Defs - End unit CHITON has index 18
# Defs - Begin Staticsymtable
# Definition <record type>
La13:
	.byte	2
	.ascii	"$ansistrrec13\000"
	.long	L$set$112
	.set L$set$112,La55-Ldebug_info0
La55:
	.byte	12
	.ascii	"$ANSISTRREC13\000"
	.byte	38
	.byte	0
La14:
	.byte	11
	.long	L$set$113
	.set L$set$113,La13-Ldebug_info0
# Definition <record type>
La15:
	.byte	2
	.ascii	"$rttidef$_INIT_$CHITON_$$_TQUEUEENTRY\000"
	.long	L$set$114
	.set L$set$114,La56-Ldebug_info0
La56:
	.byte	12
	.ascii	"$RTTIDEF$_INIT_$CHITON_$$_TQUEUEENTRY\000"
	.byte	45
	.byte	0
La16:
	.byte	11
	.long	L$set$115
	.set L$set$115,La15-Ldebug_info0
# Definition <record type>
La17:
	.byte	2
	.ascii	"$rttidef$_RTTI_$CHITON_$$_TQUEUEENTRY\000"
	.long	L$set$116
	.set L$set$116,La57-Ldebug_info0
La57:
	.byte	12
	.ascii	"$RTTIDEF$_RTTI_$CHITON_$$_TQUEUEENTRY\000"
	.byte	40
	.byte	0
La18:
	.byte	11
	.long	L$set$117
	.set L$set$117,La17-Ldebug_info0
# Definition <record type>
La19:
	.byte	2
	.ascii	"$rttidef$_INIT_$CHITON_$$_TQUEUE\000"
	.long	L$set$118
	.set L$set$118,La58-Ldebug_info0
La58:
	.byte	12
	.ascii	"$RTTIDEF$_INIT_$CHITON_$$_TQUEUE\000"
	.byte	43
	.byte	0
La20:
	.byte	11
	.long	L$set$119
	.set L$set$119,La19-Ldebug_info0
# Definition <record type>
La21:
	.byte	2
	.ascii	"$rtti_dyn_array$\000"
	.long	L$set$120
	.set L$set$120,La59-Ldebug_info0
La59:
	.byte	12
	.ascii	"$RTTI_DYN_ARRAY$\000"
	.byte	35
	.byte	0
La22:
	.byte	11
	.long	L$set$121
	.set L$set$121,La21-Ldebug_info0
# Definition <record type>
La23:
	.byte	2
	.ascii	"$rttidef$_RTTI_$CHITON_$$_TQUEUE\000"
	.long	L$set$122
	.set L$set$122,La60-Ldebug_info0
La60:
	.byte	12
	.ascii	"$RTTIDEF$_RTTI_$CHITON_$$_TQUEUE\000"
	.byte	43
	.byte	0
La24:
	.byte	11
	.long	L$set$123
	.set L$set$123,La23-Ldebug_info0
# Definition <record type>
La25:
	.byte	2
	.ascii	"$rttidef$_INIT_$CHITON_$$_TROUTEFINDER\000"
	.long	L$set$124
	.set L$set$124,La61-Ldebug_info0
La61:
	.byte	12
	.ascii	"$RTTIDEF$_INIT_$CHITON_$$_TROUTEFINDER\000"
	.byte	78
	.byte	0
La26:
	.byte	11
	.long	L$set$125
	.set L$set$125,La25-Ldebug_info0
# Definition <record type>
La27:
	.byte	2
	.ascii	"$rttidef$_RTTI_$CHITON_$$_TROUTEFINDER\000"
	.long	L$set$126
	.set L$set$126,La62-Ldebug_info0
La62:
	.byte	12
	.ascii	"$RTTIDEF$_RTTI_$CHITON_$$_TROUTEFINDER\000"
	.byte	41
	.byte	0
La28:
	.byte	11
	.long	L$set$127
	.set L$set$127,La27-Ldebug_info0
# Defs - End Staticsymtable
# Definition TInterfacedObject
La30:
	.byte	2
	.ascii	"TInterfacedObject\000"
	.long	L$set$128
	.set L$set$128,La63-Ldebug_info0
La63:
	.byte	3
	.long	L$set$129
	.set L$set$129,La32-Ldebug_info0
La32:
	.byte	4
	.ascii	"TInterfacedObject\000"
	.byte	24
	.byte	5
	.byte	1
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$130
	.set L$set$130,La66-Ldebug_info0
	.byte	6
	.ascii	"frefcount\000"
	.byte	2
	.byte	35
	.byte	8
	.byte	2
	.long	L$set$131
	.set L$set$131,La33-Ldebug_info0
	.byte	6
	.ascii	"FDestroyCount\000"
	.byte	2
	.byte	35
	.byte	12
	.byte	2
	.long	L$set$132
	.set L$set$132,La33-Ldebug_info0
	.byte	6
	.ascii	"RefCount\000"
	.byte	2
	.byte	35
	.byte	8
	.byte	2
	.long	L$set$133
	.set L$set$133,La33-Ldebug_info0
# Procdef QueryInterface(<TInterfacedObject>;constref TGuid;out <Formal type>):LongInt; CDecl;
	.byte	18
	.ascii	"QueryInterface\000"
	.byte	1
	.byte	1
	.byte	2
	.long	L$set$134
	.set L$set$134,La33-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$135
	.set L$set$135,La30-Ldebug_info0
# Symbol IID
	.byte	20
	.ascii	"iid\000"
	.long	L$set$136
	.set L$set$136,La67-Ldebug_info0
# Symbol OBJ
	.byte	20
	.ascii	"obj\000"
	.long	L$set$137
	.set L$set$137,La69-Ldebug_info0
	.byte	0
# Procdef _AddRef(<TInterfacedObject>):LongInt; CDecl;
	.byte	18
	.ascii	"_AddRef\000"
	.byte	1
	.byte	1
	.byte	2
	.long	L$set$138
	.set L$set$138,La33-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$139
	.set L$set$139,La30-Ldebug_info0
	.byte	0
# Procdef _Release(<TInterfacedObject>):LongInt; CDecl;
	.byte	18
	.ascii	"_Release\000"
	.byte	1
	.byte	1
	.byte	2
	.long	L$set$140
	.set L$set$140,La33-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$141
	.set L$set$141,La30-Ldebug_info0
	.byte	0
# Procdef destructor destroy(<TInterfacedObject>;<Class Of TInterfacedObject>);
	.byte	21
	.ascii	"destroy\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	4
	.byte	6
	.byte	16
	.byte	96
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$142
	.set L$set$142,La30-Ldebug_info0
# Symbol vmt
	.byte	20
	.ascii	"$vmt\000"
	.long	L$set$143
	.set L$set$143,La71-Ldebug_info0
	.byte	0
# Procdef AfterConstruction(<TInterfacedObject>);
	.byte	21
	.ascii	"AfterConstruction\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	136,1
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$144
	.set L$set$144,La30-Ldebug_info0
	.byte	0
# Procdef BeforeDestruction(<TInterfacedObject>);
	.byte	21
	.ascii	"BeforeDestruction\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	144,1
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$145
	.set L$set$145,La30-Ldebug_info0
	.byte	0
# Procdef class NewInstance(<Class Of TInterfacedObject>):TObject;
	.byte	22
	.ascii	"NewInstance\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	4
	.byte	6
	.byte	16
	.byte	104
	.byte	34
	.long	L$set$146
	.set L$set$146,La64-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$147
	.set L$set$147,La73-Ldebug_info0
	.byte	0
	.byte	0
La31:
	.byte	11
	.long	L$set$148
	.set L$set$148,La30-Ldebug_info0
# Definition LongInt
La33:
	.byte	2
	.ascii	"LongInt\000"
	.long	L$set$149
	.set L$set$149,La75-Ldebug_info0
La75:
	.byte	23
	.ascii	"LongInt\000"
	.byte	5
	.byte	4
La34:
	.byte	11
	.long	L$set$150
	.set L$set$150,La33-Ldebug_info0
# Definition TPoint
La35:
	.byte	2
	.ascii	"TPoint\000"
	.long	L$set$151
	.set L$set$151,La76-Ldebug_info0
La76:
	.byte	12
	.ascii	"TPOINT\000"
	.byte	8
	.byte	24
	.ascii	"X\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$152
	.set L$set$152,La33-Ldebug_info0
	.byte	24
	.ascii	"Y\000"
	.byte	2
	.byte	35
	.byte	4
	.long	L$set$153
	.set L$set$153,La33-Ldebug_info0
# Procdef constructor Create(<var TPoint>;LongInt;LongInt);
	.byte	25
	.ascii	"Create\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$154
	.set L$set$154,La35-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$155
	.set L$set$155,La35-Ldebug_info0
# Symbol AX
	.byte	20
	.ascii	"ax\000"
	.long	L$set$156
	.set L$set$156,La33-Ldebug_info0
# Symbol AY
	.byte	20
	.ascii	"ay\000"
	.long	L$set$157
	.set L$set$157,La33-Ldebug_info0
	.byte	0
# Procdef constructor Create(<var TPoint>;TPoint);
	.byte	25
	.ascii	"Create\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$158
	.set L$set$158,La35-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$159
	.set L$set$159,La35-Ldebug_info0
# Symbol APT
	.byte	20
	.ascii	"apt\000"
	.long	L$set$160
	.set L$set$160,La35-Ldebug_info0
	.byte	0
# Procdef class Zero:<record type>; Static;
	.byte	25
	.ascii	"Zero\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$161
	.set L$set$161,La35-Ldebug_info0
	.byte	0
# Procdef Add(<var TPoint>;const TPoint):<record type>;
	.byte	25
	.ascii	"Add\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$162
	.set L$set$162,La35-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$163
	.set L$set$163,La35-Ldebug_info0
# Symbol APT
	.byte	20
	.ascii	"apt\000"
	.long	L$set$164
	.set L$set$164,La35-Ldebug_info0
	.byte	0
# Procdef Distance(<var TPoint>;const TPoint):Extended;
	.byte	25
	.ascii	"Distance\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$165
	.set L$set$165,La77-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$166
	.set L$set$166,La35-Ldebug_info0
# Symbol APT
	.byte	20
	.ascii	"apt\000"
	.long	L$set$167
	.set L$set$167,La35-Ldebug_info0
	.byte	0
# Procdef IsZero(<var TPoint>):Boolean;
	.byte	25
	.ascii	"IsZero\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$168
	.set L$set$168,La37-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$169
	.set L$set$169,La35-Ldebug_info0
	.byte	0
# Procdef Subtract(<var TPoint>;const TPoint):<record type>;
	.byte	25
	.ascii	"Subtract\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$170
	.set L$set$170,La35-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$171
	.set L$set$171,La35-Ldebug_info0
# Symbol APT
	.byte	20
	.ascii	"apt\000"
	.long	L$set$172
	.set L$set$172,La35-Ldebug_info0
	.byte	0
# Procdef SetLocation(<var TPoint>;const TPoint);
	.byte	26
	.ascii	"SetLocation\000"
	.byte	1
	.byte	65
	.byte	1
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$173
	.set L$set$173,La35-Ldebug_info0
# Symbol APT
	.byte	20
	.ascii	"apt\000"
	.long	L$set$174
	.set L$set$174,La35-Ldebug_info0
	.byte	0
# Procdef SetLocation(<var TPoint>;LongInt;LongInt);
	.byte	26
	.ascii	"SetLocation\000"
	.byte	1
	.byte	65
	.byte	1
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$175
	.set L$set$175,La35-Ldebug_info0
# Symbol AX
	.byte	20
	.ascii	"ax\000"
	.long	L$set$176
	.set L$set$176,La33-Ldebug_info0
# Symbol AY
	.byte	20
	.ascii	"ay\000"
	.long	L$set$177
	.set L$set$177,La33-Ldebug_info0
	.byte	0
# Procdef Offset(<var TPoint>;const TPoint);
	.byte	26
	.ascii	"Offset\000"
	.byte	1
	.byte	65
	.byte	1
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$178
	.set L$set$178,La35-Ldebug_info0
# Symbol APT
	.byte	20
	.ascii	"apt\000"
	.long	L$set$179
	.set L$set$179,La35-Ldebug_info0
	.byte	0
# Procdef Offset(<var TPoint>;LongInt;LongInt);
	.byte	26
	.ascii	"Offset\000"
	.byte	1
	.byte	65
	.byte	1
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$180
	.set L$set$180,La35-Ldebug_info0
# Symbol DX
	.byte	20
	.ascii	"dx\000"
	.long	L$set$181
	.set L$set$181,La33-Ldebug_info0
# Symbol DY
	.byte	20
	.ascii	"dy\000"
	.long	L$set$182
	.set L$set$182,La33-Ldebug_info0
	.byte	0
# Procdef class PointInCircle(const TPoint;const TPoint;const LongInt):Boolean; Static;
	.byte	25
	.ascii	"PointInCircle\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$183
	.set L$set$183,La37-Ldebug_info0
# Symbol APT
	.byte	20
	.ascii	"apt\000"
	.long	L$set$184
	.set L$set$184,La35-Ldebug_info0
# Symbol ACENTER
	.byte	20
	.ascii	"acenter\000"
	.long	L$set$185
	.set L$set$185,La35-Ldebug_info0
# Symbol ARADIUS
	.byte	20
	.ascii	"aradius\000"
	.long	L$set$186
	.set L$set$186,La33-Ldebug_info0
	.byte	0
# Procdef operator =(const TPoint;const TPoint):Boolean; Static;
	.byte	25
	.ascii	"$equal\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$187
	.set L$set$187,La37-Ldebug_info0
# Symbol APT1
	.byte	20
	.ascii	"apt1\000"
	.long	L$set$188
	.set L$set$188,La35-Ldebug_info0
# Symbol APT2
	.byte	20
	.ascii	"apt2\000"
	.long	L$set$189
	.set L$set$189,La35-Ldebug_info0
	.byte	0
# Procdef operator <>(const TPoint;const TPoint):Boolean; Static;
	.byte	25
	.ascii	"$not_equal\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$190
	.set L$set$190,La37-Ldebug_info0
# Symbol APT1
	.byte	20
	.ascii	"apt1\000"
	.long	L$set$191
	.set L$set$191,La35-Ldebug_info0
# Symbol APT2
	.byte	20
	.ascii	"apt2\000"
	.long	L$set$192
	.set L$set$192,La35-Ldebug_info0
	.byte	0
# Procdef operator +(const TPoint;const TPoint):<record type>; Static;
	.byte	25
	.ascii	"$plus\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$193
	.set L$set$193,La35-Ldebug_info0
# Symbol APT1
	.byte	20
	.ascii	"apt1\000"
	.long	L$set$194
	.set L$set$194,La35-Ldebug_info0
# Symbol APT2
	.byte	20
	.ascii	"apt2\000"
	.long	L$set$195
	.set L$set$195,La35-Ldebug_info0
	.byte	0
# Procdef operator -(const TPoint;const TPoint):<record type>; Static;
	.byte	25
	.ascii	"$minus\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$196
	.set L$set$196,La35-Ldebug_info0
# Symbol APT1
	.byte	20
	.ascii	"apt1\000"
	.long	L$set$197
	.set L$set$197,La35-Ldebug_info0
# Symbol APT2
	.byte	20
	.ascii	"apt2\000"
	.long	L$set$198
	.set L$set$198,La35-Ldebug_info0
	.byte	0
# Procdef operator :=(const TSmallPoint):<record type>; Static;
	.byte	25
	.ascii	"$assign\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$199
	.set L$set$199,La35-Ldebug_info0
# Symbol ASPT
	.byte	20
	.ascii	"aspt\000"
	.long	L$set$200
	.set L$set$200,La79-Ldebug_info0
	.byte	0
# Procdef operator explicit(const TPoint):<record type>; Static;
	.byte	25
	.ascii	"$explicit\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$201
	.set L$set$201,La79-Ldebug_info0
# Symbol APT
	.byte	20
	.ascii	"apt\000"
	.long	L$set$202
	.set L$set$202,La35-Ldebug_info0
	.byte	0
	.byte	0
La36:
	.byte	11
	.long	L$set$203
	.set L$set$203,La35-Ldebug_info0
# Definition Boolean
La37:
	.byte	2
	.ascii	"Boolean\000"
	.long	L$set$204
	.set L$set$204,La81-Ldebug_info0
La81:
	.byte	23
	.ascii	"Boolean\000"
	.byte	2
	.byte	1
La38:
	.byte	11
	.long	L$set$205
	.set L$set$205,La37-Ldebug_info0
# Definition TQueueEntry.Class Of TQueueEntry
La39:
	.byte	3
	.long	L$set$206
	.set L$set$206,La82-Ldebug_info0
La40:
	.byte	11
	.long	L$set$207
	.set L$set$207,La39-Ldebug_info0
# Definition Int64
La41:
	.byte	2
	.ascii	"Int64\000"
	.long	L$set$208
	.set L$set$208,La84-Ldebug_info0
La84:
	.byte	23
	.ascii	"Int64\000"
	.byte	5
	.byte	8
La42:
	.byte	11
	.long	L$set$209
	.set L$set$209,La41-Ldebug_info0
# Definition T2DIntMap
La46:
	.byte	2
	.ascii	"T2DIntMap\000"
	.long	L$set$210
	.set L$set$210,La85-Ldebug_info0
La85:
	.byte	13
	.ascii	"T2DIntMap\000"
	.byte	2
	.byte	151
	.byte	6
	.long	L$set$211
	.set L$set$211,La86-Ldebug_info0
	.byte	14
	.byte	8
	.byte	0
	.byte	14
	.byte	151
	.byte	6
	.byte	18
	.byte	40
	.short	5
	.byte	9
	.byte	255
	.byte	47
	.short	3
	.byte	56
	.byte	28
	.byte	6
	.long	L$set$212
	.set L$set$212,La33-Ldebug_info0
	.byte	0
La47:
	.byte	11
	.long	L$set$213
	.set L$set$213,La46-Ldebug_info0
# Definition TStringArray
La48:
	.byte	2
	.ascii	"TStringArray\000"
	.long	L$set$214
	.set L$set$214,La88-Ldebug_info0
La88:
	.byte	13
	.ascii	"TStringArray\000"
	.byte	2
	.byte	151
	.byte	6
	.long	L$set$215
	.set L$set$215,La50-Ldebug_info0
	.byte	14
	.byte	8
	.byte	0
	.byte	14
	.byte	151
	.byte	6
	.byte	18
	.byte	40
	.short	5
	.byte	9
	.byte	255
	.byte	47
	.short	3
	.byte	56
	.byte	28
	.byte	6
	.long	L$set$216
	.set L$set$216,La33-Ldebug_info0
	.byte	0
La49:
	.byte	11
	.long	L$set$217
	.set L$set$217,La48-Ldebug_info0
# Definition AnsiString
La50:
	.byte	2
	.ascii	"AnsiString\000"
	.long	L$set$218
	.set L$set$218,La89-Ldebug_info0
La89:
	.byte	13
	.ascii	"AnsiString\000"
	.byte	2
	.byte	151
	.byte	6
	.long	L$set$219
	.set L$set$219,La90-Ldebug_info0
	.byte	27
	.byte	1
	.byte	13
	.byte	151
	.byte	6
	.byte	18
	.byte	40
	.short	4
	.byte	48
	.byte	47
	.short	3
	.byte	56
	.byte	28
	.byte	6
	.byte	0
La51:
	.byte	11
	.long	L$set$220
	.set L$set$220,La50-Ldebug_info0
# Definition TRouteFinder.Class Of TRouteFinder
La52:
	.byte	3
	.long	L$set$221
	.set L$set$221,La82-Ldebug_info0
La53:
	.byte	11
	.long	L$set$222
	.set L$set$222,La52-Ldebug_info0
# Definition TObject
La64:
	.byte	2
	.ascii	"TObject\000"
	.long	L$set$223
	.set L$set$223,La92-Ldebug_info0
La92:
	.byte	3
	.long	L$set$224
	.set L$set$224,La66-Ldebug_info0
La66:
	.byte	4
	.ascii	"TObject\000"
	.byte	8
	.byte	28
	.byte	1
	.ascii	"_vptr$TOBJECT\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$225
	.set L$set$225,La93-Ldebug_info0
# Procdef constructor Create(<TObject>;<Class Of TObject>);
	.byte	25
	.ascii	"Create\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$226
	.set L$set$226,La64-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$227
	.set L$set$227,La64-Ldebug_info0
# Symbol vmt
	.byte	20
	.ascii	"$vmt\000"
	.long	L$set$228
	.set L$set$228,La95-Ldebug_info0
	.byte	0
# Procdef destructor Destroy(<TObject>;<Class Of TObject>);
	.byte	21
	.ascii	"Destroy\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	4
	.byte	6
	.byte	16
	.byte	96
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$229
	.set L$set$229,La64-Ldebug_info0
# Symbol vmt
	.byte	20
	.ascii	"$vmt\000"
	.long	L$set$230
	.set L$set$230,La97-Ldebug_info0
	.byte	0
# Procdef class newinstance(<Class Of TObject>):TObject;
	.byte	22
	.ascii	"newinstance\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	4
	.byte	6
	.byte	16
	.byte	104
	.byte	34
	.long	L$set$231
	.set L$set$231,La64-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$232
	.set L$set$232,La99-Ldebug_info0
	.byte	0
# Procdef FreeInstance(<TObject>);
	.byte	21
	.ascii	"FreeInstance\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	4
	.byte	6
	.byte	16
	.byte	112
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$233
	.set L$set$233,La64-Ldebug_info0
	.byte	0
# Procdef SafeCallException(<TObject>;TObject;Pointer):LongInt;
	.byte	22
	.ascii	"SafeCallException\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	4
	.byte	6
	.byte	16
	.byte	120
	.byte	34
	.long	L$set$234
	.set L$set$234,La101-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$235
	.set L$set$235,La64-Ldebug_info0
# Symbol EXCEPTOBJECT
	.byte	20
	.ascii	"exceptobject\000"
	.long	L$set$236
	.set L$set$236,La64-Ldebug_info0
# Symbol EXCEPTADDR
	.byte	20
	.ascii	"exceptaddr\000"
	.long	L$set$237
	.set L$set$237,La93-Ldebug_info0
	.byte	0
# Procdef DefaultHandler(<TObject>;var <Formal type>);
	.byte	21
	.ascii	"DefaultHandler\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	128,1
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$238
	.set L$set$238,La64-Ldebug_info0
# Symbol MESSAGE
	.byte	20
	.ascii	"message\000"
	.long	L$set$239
	.set L$set$239,La69-Ldebug_info0
	.byte	0
# Procdef Free(<TObject>);
	.byte	26
	.ascii	"Free\000"
	.byte	1
	.byte	65
	.byte	1
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$240
	.set L$set$240,La64-Ldebug_info0
	.byte	0
# Procdef class InitInstance(<Class Of TObject>;Pointer):TObject;
	.byte	25
	.ascii	"InitInstance\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$241
	.set L$set$241,La64-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$242
	.set L$set$242,La103-Ldebug_info0
# Symbol INSTANCE
	.byte	20
	.ascii	"instance\000"
	.long	L$set$243
	.set L$set$243,La93-Ldebug_info0
	.byte	0
# Procdef CleanupInstance(<TObject>);
	.byte	26
	.ascii	"CleanupInstance\000"
	.byte	1
	.byte	65
	.byte	1
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$244
	.set L$set$244,La64-Ldebug_info0
	.byte	0
# Procdef class ClassType(<Class Of TObject>):Class Of TObject;
	.byte	25
	.ascii	"ClassType\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$245
	.set L$set$245,La105-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$246
	.set L$set$246,La107-Ldebug_info0
	.byte	0
# Procdef class ClassInfo(<Class Of TObject>):^untyped;
	.byte	25
	.ascii	"ClassInfo\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$247
	.set L$set$247,La93-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$248
	.set L$set$248,La109-Ldebug_info0
	.byte	0
# Procdef class ClassName(<Class Of TObject>;<var ShortString>):ShortString;
	.byte	25
	.ascii	"ClassName\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$249
	.set L$set$249,La111-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$250
	.set L$set$250,La113-Ldebug_info0
# Symbol result
	.byte	29
	.ascii	"$result\000"
	.long	L$set$251
	.set L$set$251,La111-Ldebug_info0
	.byte	0
# Procdef class ClassNameIs(<Class Of TObject>;const ShortString):Boolean;
	.byte	25
	.ascii	"ClassNameIs\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$252
	.set L$set$252,La37-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$253
	.set L$set$253,La115-Ldebug_info0
# Symbol NAME
	.byte	20
	.ascii	"name\000"
	.long	L$set$254
	.set L$set$254,La111-Ldebug_info0
	.byte	0
# Procdef class ClassParent(<Class Of TObject>):Class Of TObject;
	.byte	25
	.ascii	"ClassParent\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$255
	.set L$set$255,La105-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$256
	.set L$set$256,La117-Ldebug_info0
	.byte	0
# Procdef class InstanceSize(<Class Of TObject>):Int64;
	.byte	25
	.ascii	"InstanceSize\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$257
	.set L$set$257,La41-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$258
	.set L$set$258,La119-Ldebug_info0
	.byte	0
# Procdef class InheritsFrom(<Class Of TObject>;TClass):Boolean;
	.byte	25
	.ascii	"InheritsFrom\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$259
	.set L$set$259,La37-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$260
	.set L$set$260,La121-Ldebug_info0
# Symbol ACLASS
	.byte	20
	.ascii	"aclass\000"
	.long	L$set$261
	.set L$set$261,La105-Ldebug_info0
	.byte	0
# Procdef class StringMessageTable(<Class Of TObject>):^TStringMessageTable;
	.byte	25
	.ascii	"StringMessageTable\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$262
	.set L$set$262,La123-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$263
	.set L$set$263,La125-Ldebug_info0
	.byte	0
# Procdef class MethodAddress(<Class Of TObject>;const ShortString):^untyped;
	.byte	25
	.ascii	"MethodAddress\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$264
	.set L$set$264,La93-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$265
	.set L$set$265,La127-Ldebug_info0
# Symbol NAME
	.byte	20
	.ascii	"name\000"
	.long	L$set$266
	.set L$set$266,La111-Ldebug_info0
	.byte	0
# Procdef class MethodName(<Class Of TObject>;<var ShortString>;Pointer):ShortString;
	.byte	25
	.ascii	"MethodName\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$267
	.set L$set$267,La111-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$268
	.set L$set$268,La129-Ldebug_info0
# Symbol result
	.byte	29
	.ascii	"$result\000"
	.long	L$set$269
	.set L$set$269,La111-Ldebug_info0
# Symbol ADDRESS
	.byte	20
	.ascii	"address\000"
	.long	L$set$270
	.set L$set$270,La93-Ldebug_info0
	.byte	0
# Procdef FieldAddress(<TObject>;const ShortString):^untyped;
	.byte	25
	.ascii	"FieldAddress\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$271
	.set L$set$271,La93-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$272
	.set L$set$272,La64-Ldebug_info0
# Symbol NAME
	.byte	20
	.ascii	"name\000"
	.long	L$set$273
	.set L$set$273,La111-Ldebug_info0
	.byte	0
# Procdef AfterConstruction(<TObject>);
	.byte	21
	.ascii	"AfterConstruction\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	136,1
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$274
	.set L$set$274,La64-Ldebug_info0
	.byte	0
# Procdef BeforeDestruction(<TObject>);
	.byte	21
	.ascii	"BeforeDestruction\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	144,1
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$275
	.set L$set$275,La64-Ldebug_info0
	.byte	0
# Procdef DefaultHandlerStr(<TObject>;var <Formal type>);
	.byte	21
	.ascii	"DefaultHandlerStr\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	152,1
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$276
	.set L$set$276,La64-Ldebug_info0
# Symbol MESSAGE
	.byte	20
	.ascii	"message\000"
	.long	L$set$277
	.set L$set$277,La69-Ldebug_info0
	.byte	0
# Procdef Dispatch(<TObject>;var <Formal type>);
	.byte	21
	.ascii	"Dispatch\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	160,1
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$278
	.set L$set$278,La64-Ldebug_info0
# Symbol MESSAGE
	.byte	20
	.ascii	"message\000"
	.long	L$set$279
	.set L$set$279,La69-Ldebug_info0
	.byte	0
# Procdef DispatchStr(<TObject>;var <Formal type>);
	.byte	21
	.ascii	"DispatchStr\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	168,1
	.byte	34
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$280
	.set L$set$280,La64-Ldebug_info0
# Symbol MESSAGE
	.byte	20
	.ascii	"message\000"
	.long	L$set$281
	.set L$set$281,La69-Ldebug_info0
	.byte	0
# Procdef GetInterface(<TObject>;const TGuid;out <Formal type>):Boolean;
	.byte	25
	.ascii	"GetInterface\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$282
	.set L$set$282,La37-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$283
	.set L$set$283,La64-Ldebug_info0
# Symbol IID
	.byte	20
	.ascii	"iid\000"
	.long	L$set$284
	.set L$set$284,La67-Ldebug_info0
# Symbol OBJ
	.byte	20
	.ascii	"obj\000"
	.long	L$set$285
	.set L$set$285,La69-Ldebug_info0
	.byte	0
# Procdef GetInterface(<TObject>;const ShortString;out <Formal type>):Boolean;
	.byte	25
	.ascii	"GetInterface\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$286
	.set L$set$286,La37-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$287
	.set L$set$287,La64-Ldebug_info0
# Symbol IIDSTR
	.byte	20
	.ascii	"iidstr\000"
	.long	L$set$288
	.set L$set$288,La111-Ldebug_info0
# Symbol OBJ
	.byte	20
	.ascii	"obj\000"
	.long	L$set$289
	.set L$set$289,La69-Ldebug_info0
	.byte	0
# Procdef GetInterfaceByStr(<TObject>;const ShortString;out <Formal type>):Boolean;
	.byte	25
	.ascii	"GetInterfaceByStr\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$290
	.set L$set$290,La37-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$291
	.set L$set$291,La64-Ldebug_info0
# Symbol IIDSTR
	.byte	20
	.ascii	"iidstr\000"
	.long	L$set$292
	.set L$set$292,La111-Ldebug_info0
# Symbol OBJ
	.byte	20
	.ascii	"obj\000"
	.long	L$set$293
	.set L$set$293,La69-Ldebug_info0
	.byte	0
# Procdef GetInterfaceWeak(<TObject>;const TGuid;out <Formal type>):Boolean;
	.byte	25
	.ascii	"GetInterfaceWeak\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$294
	.set L$set$294,La37-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$295
	.set L$set$295,La64-Ldebug_info0
# Symbol IID
	.byte	20
	.ascii	"iid\000"
	.long	L$set$296
	.set L$set$296,La67-Ldebug_info0
# Symbol OBJ
	.byte	20
	.ascii	"obj\000"
	.long	L$set$297
	.set L$set$297,La69-Ldebug_info0
	.byte	0
# Procdef class GetInterfaceEntry(<Class Of TObject>;const TGuid):^tinterfaceentry;
	.byte	25
	.ascii	"GetInterfaceEntry\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$298
	.set L$set$298,La131-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$299
	.set L$set$299,La133-Ldebug_info0
# Symbol IID
	.byte	20
	.ascii	"iid\000"
	.long	L$set$300
	.set L$set$300,La67-Ldebug_info0
	.byte	0
# Procdef class GetInterfaceEntryByStr(<Class Of TObject>;const ShortString):^tinterfaceentry;
	.byte	25
	.ascii	"GetInterfaceEntryByStr\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$301
	.set L$set$301,La131-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$302
	.set L$set$302,La135-Ldebug_info0
# Symbol IIDSTR
	.byte	20
	.ascii	"iidstr\000"
	.long	L$set$303
	.set L$set$303,La111-Ldebug_info0
	.byte	0
# Procdef class GetInterfaceTable(<Class Of TObject>):^tinterfacetable;
	.byte	25
	.ascii	"GetInterfaceTable\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$304
	.set L$set$304,La137-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$305
	.set L$set$305,La139-Ldebug_info0
	.byte	0
# Procdef class UnitName(<Class Of TObject>;<var AnsiString>):AnsiString;
	.byte	25
	.ascii	"UnitName\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$306
	.set L$set$306,La50-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$307
	.set L$set$307,La141-Ldebug_info0
# Symbol result
	.byte	29
	.ascii	"$result\000"
	.long	L$set$308
	.set L$set$308,La50-Ldebug_info0
	.byte	0
# Procdef class QualifiedClassName(<Class Of TObject>;<var AnsiString>):AnsiString;
	.byte	25
	.ascii	"QualifiedClassName\000"
	.byte	1
	.byte	65
	.byte	1
	.long	L$set$309
	.set L$set$309,La50-Ldebug_info0
# Symbol self
	.byte	19
	.ascii	"self\000"
	.byte	1
	.long	L$set$310
	.set L$set$310,La143-Ldebug_info0
# Symbol result
	.byte	29
	.ascii	"$result\000"
	.long	L$set$311
	.set L$set$311,La50-Ldebug_info0
	.byte	0
# Procdef Equals(<TObject>;TObject):Boolean;
	.byte	22
	.ascii	"Equals\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	176,1
	.byte	34
	.long	L$set$312
	.set L$set$312,La37-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$313
	.set L$set$313,La64-Ldebug_info0
# Symbol OBJ
	.byte	20
	.ascii	"Obj\000"
	.long	L$set$314
	.set L$set$314,La64-Ldebug_info0
	.byte	0
# Procdef GetHashCode(<TObject>):Int64;
	.byte	22
	.ascii	"GetHashCode\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	184,1
	.byte	34
	.long	L$set$315
	.set L$set$315,La41-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$316
	.set L$set$316,La64-Ldebug_info0
	.byte	0
# Procdef ToString(<TObject>;<var AnsiString>):AnsiString;
	.byte	22
	.ascii	"ToString\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	1
	.byte	5
	.byte	6
	.byte	16
	.byte	192,1
	.byte	34
	.long	L$set$317
	.set L$set$317,La50-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$318
	.set L$set$318,La64-Ldebug_info0
# Symbol result
	.byte	29
	.ascii	"$result\000"
	.long	L$set$319
	.set L$set$319,La50-Ldebug_info0
	.byte	0
	.byte	0
La65:
	.byte	11
	.long	L$set$320
	.set L$set$320,La64-Ldebug_info0
# Definition TGuid
La67:
	.byte	2
	.ascii	"TGuid\000"
	.long	L$set$321
	.set L$set$321,La145-Ldebug_info0
La145:
	.byte	12
	.ascii	"TGUID\000"
	.byte	16
	.byte	24
	.ascii	"Data1\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$322
	.set L$set$322,La146-Ldebug_info0
	.byte	24
	.ascii	"Data2\000"
	.byte	2
	.byte	35
	.byte	4
	.long	L$set$323
	.set L$set$323,La148-Ldebug_info0
	.byte	24
	.ascii	"Data3\000"
	.byte	2
	.byte	35
	.byte	6
	.long	L$set$324
	.set L$set$324,La148-Ldebug_info0
	.byte	24
	.ascii	"Data4\000"
	.byte	2
	.byte	35
	.byte	8
	.long	L$set$325
	.set L$set$325,La150-Ldebug_info0
	.byte	24
	.ascii	"D1\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$326
	.set L$set$326,La146-Ldebug_info0
	.byte	24
	.ascii	"D2\000"
	.byte	2
	.byte	35
	.byte	4
	.long	L$set$327
	.set L$set$327,La148-Ldebug_info0
	.byte	24
	.ascii	"D3\000"
	.byte	2
	.byte	35
	.byte	6
	.long	L$set$328
	.set L$set$328,La148-Ldebug_info0
	.byte	24
	.ascii	"D4\000"
	.byte	2
	.byte	35
	.byte	8
	.long	L$set$329
	.set L$set$329,La152-Ldebug_info0
	.byte	24
	.ascii	"time_low\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$330
	.set L$set$330,La146-Ldebug_info0
	.byte	24
	.ascii	"time_mid\000"
	.byte	2
	.byte	35
	.byte	4
	.long	L$set$331
	.set L$set$331,La148-Ldebug_info0
	.byte	24
	.ascii	"time_hi_and_version\000"
	.byte	2
	.byte	35
	.byte	6
	.long	L$set$332
	.set L$set$332,La148-Ldebug_info0
	.byte	24
	.ascii	"clock_seq_hi_and_reserved\000"
	.byte	2
	.byte	35
	.byte	8
	.long	L$set$333
	.set L$set$333,La154-Ldebug_info0
	.byte	24
	.ascii	"clock_seq_low\000"
	.byte	2
	.byte	35
	.byte	9
	.long	L$set$334
	.set L$set$334,La154-Ldebug_info0
	.byte	24
	.ascii	"node\000"
	.byte	2
	.byte	35
	.byte	10
	.long	L$set$335
	.set L$set$335,La156-Ldebug_info0
	.byte	0
La68:
	.byte	11
	.long	L$set$336
	.set L$set$336,La67-Ldebug_info0
# Definition <Formal type>
La69:
	.byte	2
	.ascii	"$formal\000"
	.long	L$set$337
	.set L$set$337,La158-Ldebug_info0
La158:
	.byte	30
La70:
	.byte	11
	.long	L$set$338
	.set L$set$338,La69-Ldebug_info0
# Definition TInterfacedObject.Class Of TInterfacedObject
La71:
	.byte	3
	.long	L$set$339
	.set L$set$339,La82-Ldebug_info0
La72:
	.byte	11
	.long	L$set$340
	.set L$set$340,La71-Ldebug_info0
# Definition TInterfacedObject.Class Of TInterfacedObject
La73:
	.byte	3
	.long	L$set$341
	.set L$set$341,La82-Ldebug_info0
La74:
	.byte	11
	.long	L$set$342
	.set L$set$342,La73-Ldebug_info0
# Definition Extended
La77:
	.byte	2
	.ascii	"Extended\000"
	.long	L$set$343
	.set L$set$343,La159-Ldebug_info0
La159:
	.byte	23
	.ascii	"Extended\000"
	.byte	4
	.byte	10
La78:
	.byte	11
	.long	L$set$344
	.set L$set$344,La77-Ldebug_info0
# Definition TSmallPoint
La79:
	.byte	2
	.ascii	"TSmallPoint\000"
	.long	L$set$345
	.set L$set$345,La160-Ldebug_info0
La160:
	.byte	12
	.ascii	"TSMALLPOINT\000"
	.byte	4
	.byte	24
	.ascii	"X\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$346
	.set L$set$346,La161-Ldebug_info0
	.byte	24
	.ascii	"Y\000"
	.byte	2
	.byte	35
	.byte	2
	.long	L$set$347
	.set L$set$347,La161-Ldebug_info0
	.byte	0
La80:
	.byte	11
	.long	L$set$348
	.set L$set$348,La79-Ldebug_info0
# Definition <record type>
La82:
	.byte	2
	.ascii	"$__vtbl_ptr_type\000"
	.long	L$set$349
	.set L$set$349,La163-Ldebug_info0
La163:
	.byte	31
	.byte	40
	.byte	0
La83:
	.byte	11
	.long	L$set$350
	.set L$set$350,La82-Ldebug_info0
# Definition {Dynamic} Array Of LongInt
La86:
	.byte	32
	.byte	2
	.byte	151
	.byte	6
	.long	L$set$351
	.set L$set$351,La33-Ldebug_info0
	.byte	14
	.byte	4
	.byte	0
	.byte	14
	.byte	151
	.byte	6
	.byte	18
	.byte	40
	.short	5
	.byte	9
	.byte	255
	.byte	47
	.short	3
	.byte	56
	.byte	28
	.byte	6
	.long	L$set$352
	.set L$set$352,La33-Ldebug_info0
	.byte	0
La87:
	.byte	11
	.long	L$set$353
	.set L$set$353,La86-Ldebug_info0
# Definition Char
La90:
	.byte	2
	.ascii	"Char\000"
	.long	L$set$354
	.set L$set$354,La164-Ldebug_info0
La164:
	.byte	23
	.ascii	"Char\000"
	.byte	8
	.byte	1
La91:
	.byte	11
	.long	L$set$355
	.set L$set$355,La90-Ldebug_info0
# Definition Pointer
La93:
	.byte	2
	.ascii	"Pointer\000"
	.long	L$set$356
	.set L$set$356,La165-Ldebug_info0
La165:
	.byte	33
La94:
	.byte	11
	.long	L$set$357
	.set L$set$357,La93-Ldebug_info0
# Definition TObject.Class Of TObject
La95:
	.byte	3
	.long	L$set$358
	.set L$set$358,La82-Ldebug_info0
La96:
	.byte	11
	.long	L$set$359
	.set L$set$359,La95-Ldebug_info0
# Definition TObject.Class Of TObject
La97:
	.byte	3
	.long	L$set$360
	.set L$set$360,La82-Ldebug_info0
La98:
	.byte	11
	.long	L$set$361
	.set L$set$361,La97-Ldebug_info0
# Definition TObject.Class Of TObject
La99:
	.byte	3
	.long	L$set$362
	.set L$set$362,La82-Ldebug_info0
La100:
	.byte	11
	.long	L$set$363
	.set L$set$363,La99-Ldebug_info0
# Definition HRESULT
La101:
	.byte	2
	.ascii	"HRESULT\000"
	.long	L$set$364
	.set L$set$364,La166-Ldebug_info0
La166:
	.byte	23
	.ascii	"HRESULT\000"
	.byte	5
	.byte	4
La102:
	.byte	11
	.long	L$set$365
	.set L$set$365,La101-Ldebug_info0
# Definition TObject.Class Of TObject
La103:
	.byte	3
	.long	L$set$366
	.set L$set$366,La82-Ldebug_info0
La104:
	.byte	11
	.long	L$set$367
	.set L$set$367,La103-Ldebug_info0
# Definition TClass
La105:
	.byte	2
	.ascii	"TClass\000"
	.long	L$set$368
	.set L$set$368,La167-Ldebug_info0
La167:
	.byte	3
	.long	L$set$369
	.set L$set$369,La82-Ldebug_info0
La106:
	.byte	11
	.long	L$set$370
	.set L$set$370,La105-Ldebug_info0
# Definition TObject.Class Of TObject
La107:
	.byte	3
	.long	L$set$371
	.set L$set$371,La82-Ldebug_info0
La108:
	.byte	11
	.long	L$set$372
	.set L$set$372,La107-Ldebug_info0
# Definition TObject.Class Of TObject
La109:
	.byte	3
	.long	L$set$373
	.set L$set$373,La82-Ldebug_info0
La110:
	.byte	11
	.long	L$set$374
	.set L$set$374,La109-Ldebug_info0
# Definition ShortString
La111:
	.byte	2
	.ascii	"ShortString\000"
	.long	L$set$375
	.set L$set$375,La168-Ldebug_info0
La168:
	.byte	13
	.ascii	"ShortString\000"
	.byte	3
	.byte	151
	.byte	49
	.byte	34
	.long	L$set$376
	.set L$set$376,La90-Ldebug_info0
	.byte	27
	.byte	1
	.byte	3
	.byte	151
	.byte	148
	.byte	1
	.byte	0
La112:
	.byte	11
	.long	L$set$377
	.set L$set$377,La111-Ldebug_info0
# Definition TObject.Class Of TObject
La113:
	.byte	3
	.long	L$set$378
	.set L$set$378,La82-Ldebug_info0
La114:
	.byte	11
	.long	L$set$379
	.set L$set$379,La113-Ldebug_info0
# Definition TObject.Class Of TObject
La115:
	.byte	3
	.long	L$set$380
	.set L$set$380,La82-Ldebug_info0
La116:
	.byte	11
	.long	L$set$381
	.set L$set$381,La115-Ldebug_info0
# Definition TObject.Class Of TObject
La117:
	.byte	3
	.long	L$set$382
	.set L$set$382,La82-Ldebug_info0
La118:
	.byte	11
	.long	L$set$383
	.set L$set$383,La117-Ldebug_info0
# Definition TObject.Class Of TObject
La119:
	.byte	3
	.long	L$set$384
	.set L$set$384,La82-Ldebug_info0
La120:
	.byte	11
	.long	L$set$385
	.set L$set$385,La119-Ldebug_info0
# Definition TObject.Class Of TObject
La121:
	.byte	3
	.long	L$set$386
	.set L$set$386,La82-Ldebug_info0
La122:
	.byte	11
	.long	L$set$387
	.set L$set$387,La121-Ldebug_info0
# Definition pstringmessagetable
La123:
	.byte	2
	.ascii	"pstringmessagetable\000"
	.long	L$set$388
	.set L$set$388,La169-Ldebug_info0
La169:
	.byte	3
	.long	L$set$389
	.set L$set$389,La170-Ldebug_info0
La124:
	.byte	11
	.long	L$set$390
	.set L$set$390,La123-Ldebug_info0
# Definition TObject.Class Of TObject
La125:
	.byte	3
	.long	L$set$391
	.set L$set$391,La82-Ldebug_info0
La126:
	.byte	11
	.long	L$set$392
	.set L$set$392,La125-Ldebug_info0
# Definition TObject.Class Of TObject
La127:
	.byte	3
	.long	L$set$393
	.set L$set$393,La82-Ldebug_info0
La128:
	.byte	11
	.long	L$set$394
	.set L$set$394,La127-Ldebug_info0
# Definition TObject.Class Of TObject
La129:
	.byte	3
	.long	L$set$395
	.set L$set$395,La82-Ldebug_info0
La130:
	.byte	11
	.long	L$set$396
	.set L$set$396,La129-Ldebug_info0
# Definition pinterfaceentry
La131:
	.byte	2
	.ascii	"pinterfaceentry\000"
	.long	L$set$397
	.set L$set$397,La172-Ldebug_info0
La172:
	.byte	3
	.long	L$set$398
	.set L$set$398,La173-Ldebug_info0
La132:
	.byte	11
	.long	L$set$399
	.set L$set$399,La131-Ldebug_info0
# Definition TObject.Class Of TObject
La133:
	.byte	3
	.long	L$set$400
	.set L$set$400,La82-Ldebug_info0
La134:
	.byte	11
	.long	L$set$401
	.set L$set$401,La133-Ldebug_info0
# Definition TObject.Class Of TObject
La135:
	.byte	3
	.long	L$set$402
	.set L$set$402,La82-Ldebug_info0
La136:
	.byte	11
	.long	L$set$403
	.set L$set$403,La135-Ldebug_info0
# Definition pinterfacetable
La137:
	.byte	2
	.ascii	"pinterfacetable\000"
	.long	L$set$404
	.set L$set$404,La175-Ldebug_info0
La175:
	.byte	3
	.long	L$set$405
	.set L$set$405,La176-Ldebug_info0
La138:
	.byte	11
	.long	L$set$406
	.set L$set$406,La137-Ldebug_info0
# Definition TObject.Class Of TObject
La139:
	.byte	3
	.long	L$set$407
	.set L$set$407,La82-Ldebug_info0
La140:
	.byte	11
	.long	L$set$408
	.set L$set$408,La139-Ldebug_info0
# Definition TObject.Class Of TObject
La141:
	.byte	3
	.long	L$set$409
	.set L$set$409,La82-Ldebug_info0
La142:
	.byte	11
	.long	L$set$410
	.set L$set$410,La141-Ldebug_info0
# Definition TObject.Class Of TObject
La143:
	.byte	3
	.long	L$set$411
	.set L$set$411,La82-Ldebug_info0
La144:
	.byte	11
	.long	L$set$412
	.set L$set$412,La143-Ldebug_info0
# Definition LongWord
La146:
	.byte	2
	.ascii	"LongWord\000"
	.long	L$set$413
	.set L$set$413,La178-Ldebug_info0
La178:
	.byte	23
	.ascii	"LongWord\000"
	.byte	7
	.byte	4
La147:
	.byte	11
	.long	L$set$414
	.set L$set$414,La146-Ldebug_info0
# Definition Word
La148:
	.byte	2
	.ascii	"Word\000"
	.long	L$set$415
	.set L$set$415,La179-Ldebug_info0
La179:
	.byte	23
	.ascii	"Word\000"
	.byte	7
	.byte	2
La149:
	.byte	11
	.long	L$set$416
	.set L$set$416,La148-Ldebug_info0
# Definition TGuid.Array[0..7] Of Byte
La150:
	.byte	34
	.byte	8
	.long	L$set$417
	.set L$set$417,La154-Ldebug_info0
	.byte	35
	.byte	0
	.byte	7
	.byte	1
	.long	L$set$418
	.set L$set$418,La180-Ldebug_info0
	.byte	0
La151:
	.byte	11
	.long	L$set$419
	.set L$set$419,La150-Ldebug_info0
# Definition TGuid.Array[0..7] Of Byte
La152:
	.byte	34
	.byte	8
	.long	L$set$420
	.set L$set$420,La154-Ldebug_info0
	.byte	35
	.byte	0
	.byte	7
	.byte	1
	.long	L$set$421
	.set L$set$421,La180-Ldebug_info0
	.byte	0
La153:
	.byte	11
	.long	L$set$422
	.set L$set$422,La152-Ldebug_info0
# Definition Byte
La154:
	.byte	2
	.ascii	"Byte\000"
	.long	L$set$423
	.set L$set$423,La182-Ldebug_info0
La182:
	.byte	23
	.ascii	"Byte\000"
	.byte	7
	.byte	1
La155:
	.byte	11
	.long	L$set$424
	.set L$set$424,La154-Ldebug_info0
# Definition TGuid.Array[0..5] Of Byte
La156:
	.byte	34
	.byte	6
	.long	L$set$425
	.set L$set$425,La154-Ldebug_info0
	.byte	35
	.byte	0
	.byte	5
	.byte	1
	.long	L$set$426
	.set L$set$426,La180-Ldebug_info0
	.byte	0
La157:
	.byte	11
	.long	L$set$427
	.set L$set$427,La156-Ldebug_info0
# Definition SmallInt
La161:
	.byte	2
	.ascii	"SmallInt\000"
	.long	L$set$428
	.set L$set$428,La183-Ldebug_info0
La183:
	.byte	23
	.ascii	"SmallInt\000"
	.byte	5
	.byte	2
La162:
	.byte	11
	.long	L$set$429
	.set L$set$429,La161-Ldebug_info0
# Definition TStringMessageTable
La170:
	.byte	2
	.ascii	"TStringMessageTable\000"
	.long	L$set$430
	.set L$set$430,La184-Ldebug_info0
La184:
	.byte	12
	.ascii	"TSTRINGMESSAGETABLE\000"
	.byte	24
	.byte	24
	.ascii	"count\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$431
	.set L$set$431,La33-Ldebug_info0
	.byte	24
	.ascii	"msgstrtable\000"
	.byte	2
	.byte	35
	.byte	8
	.long	L$set$432
	.set L$set$432,La185-Ldebug_info0
	.byte	0
La171:
	.byte	11
	.long	L$set$433
	.set L$set$433,La170-Ldebug_info0
# Definition tinterfaceentry
La173:
	.byte	2
	.ascii	"tinterfaceentry\000"
	.long	L$set$434
	.set L$set$434,La187-Ldebug_info0
La187:
	.byte	12
	.ascii	"TINTERFACEENTRY\000"
	.byte	40
	.byte	24
	.ascii	"IIDRef\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$435
	.set L$set$435,La188-Ldebug_info0
	.byte	24
	.ascii	"VTable\000"
	.byte	2
	.byte	35
	.byte	8
	.long	L$set$436
	.set L$set$436,La93-Ldebug_info0
	.byte	24
	.ascii	"IOffset\000"
	.byte	2
	.byte	35
	.byte	16
	.long	L$set$437
	.set L$set$437,La190-Ldebug_info0
	.byte	24
	.ascii	"IOffsetAsCodePtr\000"
	.byte	2
	.byte	35
	.byte	16
	.long	L$set$438
	.set L$set$438,La93-Ldebug_info0
	.byte	24
	.ascii	"IIDStrRef\000"
	.byte	2
	.byte	35
	.byte	24
	.long	L$set$439
	.set L$set$439,La192-Ldebug_info0
	.byte	24
	.ascii	"IType\000"
	.byte	2
	.byte	35
	.byte	32
	.long	L$set$440
	.set L$set$440,La194-Ldebug_info0
# Procdef GetIID(<var tinterfaceentry>):^TGuid;
	.byte	36
	.ascii	"GetIID\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.long	L$set$441
	.set L$set$441,La196-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$442
	.set L$set$442,La173-Ldebug_info0
	.byte	0
# Procdef GetIIDStr(<var tinterfaceentry>):^ShortString;
	.byte	36
	.ascii	"GetIIDStr\000"
	.byte	1
	.byte	65
	.byte	1
	.byte	3
	.long	L$set$443
	.set L$set$443,La198-Ldebug_info0
# Symbol this
	.byte	19
	.ascii	"this\000"
	.byte	1
	.long	L$set$444
	.set L$set$444,La173-Ldebug_info0
	.byte	0
	.byte	0
La174:
	.byte	11
	.long	L$set$445
	.set L$set$445,La173-Ldebug_info0
# Definition tinterfacetable
La176:
	.byte	2
	.ascii	"tinterfacetable\000"
	.long	L$set$446
	.set L$set$446,La200-Ldebug_info0
La200:
	.byte	12
	.ascii	"TINTERFACETABLE\000"
	.byte	48
	.byte	24
	.ascii	"EntryCount\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$447
	.set L$set$447,La190-Ldebug_info0
	.byte	24
	.ascii	"Entries\000"
	.byte	2
	.byte	35
	.byte	8
	.long	L$set$448
	.set L$set$448,La201-Ldebug_info0
	.byte	0
La177:
	.byte	11
	.long	L$set$449
	.set L$set$449,La176-Ldebug_info0
# Definition ShortInt
La180:
	.byte	2
	.ascii	"ShortInt\000"
	.long	L$set$450
	.set L$set$450,La203-Ldebug_info0
La203:
	.byte	23
	.ascii	"ShortInt\000"
	.byte	5
	.byte	1
La181:
	.byte	11
	.long	L$set$451
	.set L$set$451,La180-Ldebug_info0
# Definition TStringMessageTable.Array[0..0] Of TMsgStrTable
La185:
	.byte	34
	.byte	16
	.long	L$set$452
	.set L$set$452,La204-Ldebug_info0
	.byte	35
	.byte	0
	.byte	0
	.byte	16
	.long	L$set$453
	.set L$set$453,La180-Ldebug_info0
	.byte	0
La186:
	.byte	11
	.long	L$set$454
	.set L$set$454,La185-Ldebug_info0
# Definition tinterfaceentry.^PGuid
La188:
	.byte	3
	.long	L$set$455
	.set L$set$455,La196-Ldebug_info0
La189:
	.byte	11
	.long	L$set$456
	.set L$set$456,La188-Ldebug_info0
# Definition QWord
La190:
	.byte	2
	.ascii	"QWord\000"
	.long	L$set$457
	.set L$set$457,La206-Ldebug_info0
La206:
	.byte	23
	.ascii	"QWord\000"
	.byte	7
	.byte	8
La191:
	.byte	11
	.long	L$set$458
	.set L$set$458,La190-Ldebug_info0
# Definition tinterfaceentry.^PShortString
La192:
	.byte	3
	.long	L$set$459
	.set L$set$459,La198-Ldebug_info0
La193:
	.byte	11
	.long	L$set$460
	.set L$set$460,La192-Ldebug_info0
# Definition tinterfaceentrytype
La194:
	.byte	2
	.ascii	"tinterfaceentrytype\000"
	.long	L$set$461
	.set L$set$461,La207-Ldebug_info0
La207:
	.byte	37
	.ascii	"tinterfaceentrytype\000"
	.byte	4
	.byte	38
	.ascii	"etStandard\000"
	.long	0
	.byte	38
	.ascii	"etVirtualMethodResult\000"
	.long	1
	.byte	38
	.ascii	"etStaticMethodResult\000"
	.long	2
	.byte	38
	.ascii	"etFieldValue\000"
	.long	3
	.byte	38
	.ascii	"etVirtualMethodClass\000"
	.long	4
	.byte	38
	.ascii	"etStaticMethodClass\000"
	.long	5
	.byte	38
	.ascii	"etFieldValueClass\000"
	.long	6
	.byte	0
La195:
	.byte	11
	.long	L$set$462
	.set L$set$462,La194-Ldebug_info0
# Definition PGuid
La196:
	.byte	2
	.ascii	"PGuid\000"
	.long	L$set$463
	.set L$set$463,La208-Ldebug_info0
La208:
	.byte	3
	.long	L$set$464
	.set L$set$464,La67-Ldebug_info0
La197:
	.byte	11
	.long	L$set$465
	.set L$set$465,La196-Ldebug_info0
# Definition PShortString
La198:
	.byte	2
	.ascii	"PShortString\000"
	.long	L$set$466
	.set L$set$466,La209-Ldebug_info0
La209:
	.byte	3
	.long	L$set$467
	.set L$set$467,La111-Ldebug_info0
La199:
	.byte	11
	.long	L$set$468
	.set L$set$468,La198-Ldebug_info0
# Definition tinterfacetable.Array[0..0] Of tinterfaceentry
La201:
	.byte	34
	.byte	40
	.long	L$set$469
	.set L$set$469,La173-Ldebug_info0
	.byte	35
	.byte	0
	.byte	0
	.byte	40
	.long	L$set$470
	.set L$set$470,La180-Ldebug_info0
	.byte	0
La202:
	.byte	11
	.long	L$set$471
	.set L$set$471,La201-Ldebug_info0
# Definition TMsgStrTable
La204:
	.byte	2
	.ascii	"TMsgStrTable\000"
	.long	L$set$472
	.set L$set$472,La210-Ldebug_info0
La210:
	.byte	12
	.ascii	"TMSGSTRTABLE\000"
	.byte	16
	.byte	24
	.ascii	"name\000"
	.byte	2
	.byte	35
	.byte	0
	.long	L$set$473
	.set L$set$473,La198-Ldebug_info0
	.byte	24
	.ascii	"method\000"
	.byte	2
	.byte	35
	.byte	8
	.long	L$set$474
	.set L$set$474,La93-Ldebug_info0
	.byte	0
La205:
	.byte	11
	.long	L$set$475
	.set L$set$475,La204-Ldebug_info0
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
	.byte	22
	.byte	0
	.byte	3
	.byte	8
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 3
	.byte	3
	.byte	15
	.byte	0
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 4
	.byte	4
	.byte	2
	.byte	1
	.byte	3
	.byte	8
	.byte	11
	.byte	15
	.byte	0
	.byte	0
# Abbrev 5
	.byte	5
	.byte	28
	.byte	0
	.byte	50
	.byte	11
	.byte	56
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 6
	.byte	6
	.byte	13
	.byte	0
	.byte	3
	.byte	8
	.byte	56
	.byte	10
	.byte	50
	.byte	11
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 7
	.byte	7
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
	.byte	73
	.byte	19
	.byte	17
	.byte	1
	.byte	18
	.byte	1
	.byte	0
	.byte	0
# Abbrev 8
	.byte	8
	.byte	5
	.byte	0
	.byte	3
	.byte	8
	.byte	2
	.byte	10
	.byte	52
	.byte	12
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 9
	.byte	9
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
# Abbrev 10
	.byte	10
	.byte	52
	.byte	0
	.byte	3
	.byte	8
	.byte	2
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 11
	.byte	11
	.byte	16
	.byte	0
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 12
	.byte	12
	.byte	19
	.byte	1
	.byte	3
	.byte	8
	.byte	11
	.byte	15
	.byte	0
	.byte	0
# Abbrev 13
	.byte	13
	.byte	1
	.byte	1
	.byte	3
	.byte	8
	.byte	80
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 14
	.byte	14
	.byte	33
	.byte	0
	.byte	81
	.byte	15
	.byte	34
	.byte	15
	.byte	47
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 15
	.byte	15
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
	.byte	50
	.byte	11
	.byte	17
	.byte	1
	.byte	18
	.byte	1
	.byte	0
	.byte	0
# Abbrev 16
	.byte	16
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
	.byte	50
	.byte	11
	.byte	73
	.byte	19
	.byte	17
	.byte	1
	.byte	18
	.byte	1
	.byte	0
	.byte	0
# Abbrev 17
	.byte	17
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
# Abbrev 18
	.byte	18
	.byte	46
	.byte	1
	.byte	3
	.byte	8
	.byte	39
	.byte	12
	.byte	63
	.byte	12
	.byte	50
	.byte	11
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 19
	.byte	19
	.byte	5
	.byte	0
	.byte	3
	.byte	8
	.byte	52
	.byte	12
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 20
	.byte	20
	.byte	5
	.byte	0
	.byte	3
	.byte	8
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 21
	.byte	21
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
	.byte	76
	.byte	11
	.byte	77
	.byte	10
	.byte	0
	.byte	0
# Abbrev 22
	.byte	22
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
	.byte	76
	.byte	11
	.byte	77
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 23
	.byte	23
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
# Abbrev 24
	.byte	24
	.byte	13
	.byte	0
	.byte	3
	.byte	8
	.byte	56
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 25
	.byte	25
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
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 26
	.byte	26
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
	.byte	0
	.byte	0
# Abbrev 27
	.byte	27
	.byte	33
	.byte	0
	.byte	34
	.byte	15
	.byte	47
	.byte	10
	.byte	0
	.byte	0
# Abbrev 28
	.byte	28
	.byte	13
	.byte	0
	.byte	52
	.byte	12
	.byte	3
	.byte	8
	.byte	56
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 29
	.byte	29
	.byte	52
	.byte	0
	.byte	3
	.byte	8
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 30
	.byte	30
	.byte	59
	.byte	0
	.byte	0
	.byte	0
# Abbrev 31
	.byte	31
	.byte	19
	.byte	1
	.byte	11
	.byte	15
	.byte	0
	.byte	0
# Abbrev 32
	.byte	32
	.byte	1
	.byte	1
	.byte	80
	.byte	10
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 33
	.byte	33
	.byte	15
	.byte	0
	.byte	0
	.byte	0
# Abbrev 34
	.byte	34
	.byte	1
	.byte	1
	.byte	11
	.byte	15
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 35
	.byte	35
	.byte	33
	.byte	0
	.byte	34
	.byte	13
	.byte	47
	.byte	13
	.byte	81
	.byte	15
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 36
	.byte	36
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
	.byte	50
	.byte	11
	.byte	73
	.byte	19
	.byte	0
	.byte	0
# Abbrev 37
	.byte	37
	.byte	4
	.byte	1
	.byte	3
	.byte	8
	.byte	11
	.byte	11
	.byte	0
	.byte	0
# Abbrev 38
	.byte	38
	.byte	40
	.byte	0
	.byte	3
	.byte	8
	.byte	28
	.byte	6
	.byte	0
	.byte	0
	.byte	0
# End asmlist al_dwarf_abbrev
# Begin asmlist al_dwarf_line

.section __DWARF,__debug_line,regular,debug
# === header start ===
	.long	L$set$476
	.set L$set$476,Ledebug_line0-Lf2
Lf2:
	.short	3
	.long	L$set$477
	.set L$set$477,Lehdebug_line0-Lf3
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
	.ascii	"chiton.pas\000"
	.byte	0
	.byte	0
	.byte	0
	.byte	0
Lehdebug_line0:
# === header end ===
# function: _CHITON$_$TROUTEFINDER_$__$$_CREATE$TSTRINGARRAY$$TROUTEFINDER
# [63:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll1
	.byte	5
	.byte	1
	.byte	74
# [63:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll2
	.byte	1
# [64:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll3
	.byte	5
	.byte	3
	.byte	13
# [65:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll4
	.byte	13
# [66:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll5
	.byte	13
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll6
	.byte	5
	.byte	1
	.byte	6
	.byte	3
	.byte	190,127
	.byte	1
# [67:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll7
	.byte	6
	.byte	79
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll8
	.byte	6
	.byte	3
	.byte	189,127
	.byte	1
# [67:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll9
	.byte	6
	.byte	79
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll10
	.byte	6
	.byte	3
	.byte	189,127
	.byte	1
# [67:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll11
	.byte	6
	.byte	79
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll12
	.byte	6
	.byte	3
	.byte	189,127
	.byte	1
# [67:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll13
	.byte	6
	.byte	79
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll14
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_INITIALIZEMAPANDQUEUE$TSTRINGARRAY
# [74:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll15
	.byte	5
	.byte	1
	.byte	85
# [74:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll16
	.byte	1
# [75:17]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll17
	.byte	5
	.byte	17
	.byte	13
# [76:19]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll18
	.byte	5
	.byte	19
	.byte	13
# [77:42]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll19
	.byte	5
	.byte	42
	.byte	13
# [78:15]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll20
	.byte	5
	.byte	15
	.byte	13
# [80:23]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll21
	.byte	5
	.byte	23
	.byte	14
# [81:17]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll22
	.byte	5
	.byte	17
	.byte	13
# [83:24]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll23
	.byte	5
	.byte	24
	.byte	14
# [84:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll24
	.byte	5
	.byte	7
	.byte	13
# [85:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll25
	.byte	13
# [86:31]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll26
	.byte	5
	.byte	31
	.byte	13
# [87:82]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll27
	.byte	5
	.byte	82
	.byte	13
# [81:5]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll28
	.byte	5
	.byte	5
	.byte	3
	.byte	122
	.byte	1
# [78:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll29
	.byte	5
	.byte	3
	.byte	3
	.byte	125
	.byte	1
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll30
	.byte	5
	.byte	1
	.byte	6
	.byte	3
	.byte	178,127
	.byte	1
# [90:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll31
	.byte	6
	.byte	102
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll32
	.byte	6
	.byte	3
	.byte	166,127
	.byte	1
# [90:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll33
	.byte	6
	.byte	102
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll34
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_GETQUEUELENGTH$$LONGINT
# [93:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll35
	.byte	5
	.byte	1
	.byte	104
# [94:11]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll36
	.byte	5
	.byte	11
	.byte	13
# [95:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll37
	.byte	5
	.byte	1
	.byte	13
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll38
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_GETMAPDIMENSIONS$$TPOINT
# [98:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll39
	.byte	5
	.byte	1
	.byte	109
# [99:13]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll40
	.byte	5
	.byte	13
	.byte	13
# [100:14]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll41
	.byte	5
	.byte	14
	.byte	13
# [101:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll42
	.byte	5
	.byte	1
	.byte	13
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll43
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_PROCESSNODE$TQUEUEENTRY$TQUEUEENTRY
# [108:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll44
	.byte	5
	.byte	1
	.byte	119
# [109:21]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll45
	.byte	5
	.byte	21
	.byte	13
# [110:17]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll46
	.byte	5
	.byte	17
	.byte	13
# [111:10]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll47
	.byte	5
	.byte	10
	.byte	13
# [114:5]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll48
	.byte	5
	.byte	5
	.byte	15
# [115:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll49
	.byte	5
	.byte	7
	.byte	13
# [117:46]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll50
	.byte	5
	.byte	46
	.byte	14
# [118:46]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll51
	.byte	13
# [119:50]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll52
	.byte	5
	.byte	50
	.byte	13
# [120:51]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll53
	.byte	5
	.byte	51
	.byte	13
# [121:57]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll54
	.byte	5
	.byte	57
	.byte	13
# [123:32]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll55
	.byte	5
	.byte	32
	.byte	14
# [124:14]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll56
	.byte	5
	.byte	14
	.byte	13
# [125:34]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll57
	.byte	5
	.byte	34
	.byte	13
# [126:18]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll58
	.byte	5
	.byte	18
	.byte	13
# [115:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll59
	.byte	5
	.byte	7
	.byte	3
	.byte	117
	.byte	1
# [114:5]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll60
	.byte	5
	.byte	5
	.byte	3
	.byte	127
	.byte	1
# [129:5]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll61
	.byte	27
# [133:19]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll62
	.byte	5
	.byte	19
	.byte	16
# [134:37]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll63
	.byte	5
	.byte	37
	.byte	13
# [136:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll64
	.byte	5
	.byte	1
	.byte	14
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll65
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_FINDSHORTESTPATH$TPOINT$TPOINT
# [141:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll66
	.byte	5
	.byte	1
	.byte	152
# [142:15]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll67
	.byte	5
	.byte	15
	.byte	13
# [143:13]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll68
	.byte	5
	.byte	13
	.byte	13
# [144:6]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll69
	.byte	5
	.byte	6
	.byte	13
# [145:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll70
	.byte	5
	.byte	3
	.byte	13
# [146:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll71
	.byte	5
	.byte	1
	.byte	13
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll72
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRY$TPOINT$$TQUEUEENTRY
# [152:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll73
	.byte	5
	.byte	1
	.byte	163
# [153:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll74
	.byte	5
	.byte	3
	.byte	13
# [155:20]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll75
	.byte	5
	.byte	20
	.byte	14
# [156:6]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll76
	.byte	5
	.byte	6
	.byte	13
# [158:28]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll77
	.byte	5
	.byte	28
	.byte	14
# [159:31]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll78
	.byte	5
	.byte	31
	.byte	13
# [161:8]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll79
	.byte	5
	.byte	8
	.byte	14
# [163:17]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll80
	.byte	5
	.byte	17
	.byte	14
# [164:9]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll81
	.byte	5
	.byte	9
	.byte	13
# [159:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll82
	.byte	5
	.byte	3
	.byte	3
	.byte	123
	.byte	1
# [167:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll83
	.byte	5
	.byte	1
	.byte	20
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll84
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_FINDQUEUEENTRYPOSITION$TQUEUEENTRY$$LONGINT
# [172:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll85
	.byte	5
	.byte	1
	.byte	183
# [173:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll86
	.byte	5
	.byte	3
	.byte	13
# [174:24]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll87
	.byte	5
	.byte	24
	.byte	13
# [176:8]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll88
	.byte	5
	.byte	8
	.byte	14
# [178:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll89
	.byte	5
	.byte	7
	.byte	14
# [179:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll90
	.byte	13
# [174:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll91
	.byte	5
	.byte	3
	.byte	3
	.byte	123
	.byte	1
# [182:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll92
	.byte	5
	.byte	1
	.byte	20
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll93
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_UPDATERISK$TQUEUEENTRY$TQUEUEENTRY$TPOINT
# [187:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll94
	.byte	5
	.byte	1
	.byte	198
# [188:44]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll95
	.byte	5
	.byte	44
	.byte	13
# [189:16]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll96
	.byte	5
	.byte	16
	.byte	13
# [192:26]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll97
	.byte	5
	.byte	26
	.byte	15
# [193:22]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll98
	.byte	5
	.byte	22
	.byte	13
# [194:24]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll99
	.byte	5
	.byte	24
	.byte	13
# [196:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll100
	.byte	5
	.byte	1
	.byte	14
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll101
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_REMOVEFROMQUEUE$TQUEUEENTRY
# [201:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll102
	.byte	5
	.byte	1
	.byte	212
# [201:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll103
	.byte	1
# [202:18]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll104
	.byte	5
	.byte	18
	.byte	13
# [203:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll105
	.byte	5
	.byte	3
	.byte	13
# [204:16]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll106
	.byte	5
	.byte	16
	.byte	13
# [205:18]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll107
	.byte	5
	.byte	18
	.byte	13
# [207:23]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll108
	.byte	5
	.byte	23
	.byte	14
# [209:38]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll109
	.byte	5
	.byte	38
	.byte	14
# [210:24]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll110
	.byte	5
	.byte	24
	.byte	13
# [209:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll111
	.byte	5
	.byte	7
	.byte	3
	.byte	127
	.byte	1
# [212:30]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll112
	.byte	5
	.byte	30
	.byte	15
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll113
	.byte	5
	.byte	1
	.byte	6
	.byte	3
	.byte	172,126
	.byte	1
# [214:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll114
	.byte	6
	.byte	226
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll115
	.byte	6
	.byte	3
	.byte	170,126
	.byte	1
# [214:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll116
	.byte	6
	.byte	226
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll117
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TROUTEFINDER_$__$$_GETLEASTRISKYQUEUEENTRY$$TQUEUEENTRY
# [222:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll118
	.byte	5
	.byte	1
	.byte	233
# [223:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll119
	.byte	5
	.byte	3
	.byte	13
# [224:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll120
	.byte	13
# [225:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll121
	.byte	13
# [226:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll122
	.byte	13
# [227:6]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll123
	.byte	5
	.byte	6
	.byte	13
# [228:24]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll124
	.byte	5
	.byte	24
	.byte	13
# [229:13]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll125
	.byte	5
	.byte	13
	.byte	13
# [230:14]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll126
	.byte	5
	.byte	14
	.byte	13
# [231:10]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll127
	.byte	5
	.byte	10
	.byte	13
# [233:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll128
	.byte	5
	.byte	7
	.byte	14
# [234:16]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll129
	.byte	5
	.byte	16
	.byte	13
# [235:7]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll130
	.byte	5
	.byte	7
	.byte	13
# [228:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll131
	.byte	5
	.byte	3
	.byte	3
	.byte	121
	.byte	1
# [237:19]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll132
	.byte	5
	.byte	19
	.byte	21
# [238:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll133
	.byte	5
	.byte	1
	.byte	13
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll134
	.byte	0
	.byte	1
	.byte	1
# ###################
# function: _CHITON$_$TQUEUEENTRY_$__$$_CREATE$TPOINT$TPOINT$$TQUEUEENTRY
# [245:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll135
	.byte	5
	.byte	1
	.byte	3
	.byte	244,1
	.byte	1
# [245:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll136
	.byte	1
# [246:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll137
	.byte	5
	.byte	3
	.byte	13
# [247:13]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll138
	.byte	5
	.byte	13
	.byte	13
# [248:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll139
	.byte	5
	.byte	3
	.byte	13
# [249:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll140
	.byte	13
# [250:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll141
	.byte	13
# [251:3]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll142
	.byte	13
# [252:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll143
	.byte	5
	.byte	1
	.byte	13
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll144
	.byte	6
	.byte	3
	.byte	132,126
	.byte	1
# [252:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll145
	.byte	6
	.byte	3
	.byte	252,1
	.byte	1
# [0:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll146
	.byte	6
	.byte	3
	.byte	132,126
	.byte	1
# [252:1]
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll147
	.byte	6
	.byte	3
	.byte	252,1
	.byte	1
	.byte	0
	.byte	9
	.byte	2
	.quad	Ll148
	.byte	0
	.byte	1
	.byte	1
# ###################
Ledebug_line0:
# End asmlist al_dwarf_line
# Begin asmlist al_end

.text
L_DEBUGEND_$CHITON:
# End asmlist al_end
	.subsections_via_symbols

