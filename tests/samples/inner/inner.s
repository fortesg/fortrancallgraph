	.file	"inner.f90"
	.text
.Ltext0:
	.globl	__inner_MOD_a
	.bss
	.align 4
	.type	__inner_MOD_a, @object
	.size	__inner_MOD_a, 4
__inner_MOD_a:
	.zero	4
	.globl	__inner_MOD_ai1
	.align 4
	.type	__inner_MOD_ai1, @object
	.size	__inner_MOD_ai1, 4
__inner_MOD_ai1:
	.zero	4
	.globl	__inner_MOD_ai2
	.align 4
	.type	__inner_MOD_ai2, @object
	.size	__inner_MOD_ai2, 4
__inner_MOD_ai2:
	.zero	4
	.globl	__inner_MOD_aia
	.align 4
	.type	__inner_MOD_aia, @object
	.size	__inner_MOD_aia, 4
__inner_MOD_aia:
	.zero	4
	.globl	__inner_MOD_b
	.align 4
	.type	__inner_MOD_b, @object
	.size	__inner_MOD_b, 4
__inner_MOD_b:
	.zero	4
	.globl	__inner_MOD_bi1
	.align 4
	.type	__inner_MOD_bi1, @object
	.size	__inner_MOD_bi1, 4
__inner_MOD_bi1:
	.zero	4
	.globl	__inner_MOD_bi2
	.align 4
	.type	__inner_MOD_bi2, @object
	.size	__inner_MOD_bi2, 4
__inner_MOD_bi2:
	.zero	4
	.globl	__inner_MOD_bib
	.align 4
	.type	__inner_MOD_bib, @object
	.size	__inner_MOD_bib, 4
__inner_MOD_bib:
	.zero	4
	.globl	__inner_MOD_c
	.align 4
	.type	__inner_MOD_c, @object
	.size	__inner_MOD_c, 4
__inner_MOD_c:
	.zero	4
	.section	.rodata
.LC0:
	.string	"inner.f90"
	.text
	.globl	__inner_MOD_i1
	.type	__inner_MOD_i1, @function
__inner_MOD_i1:
.LFB0:
	.file 1 "inner.f90"
	.loc 1 89 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$480, %rsp
.LBB2:
	.loc 1 91 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -472(%rbp)
	movl	$91, -464(%rbp)
	movl	$128, -480(%rbp)
	movl	$6, -476(%rbp)
	leaq	-480(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-480(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_c(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-480(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE2:
	.loc 1 93 0
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE0:
	.size	__inner_MOD_i1, .-__inner_MOD_i1
	.type	ib.3514, @function
ib.3514:
.LFB2:
	.loc 1 80 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$496, %rsp
	movq	%r10, -488(%rbp)
.LBB3:
	.loc 1 82 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -472(%rbp)
	movl	$82, -464(%rbp)
	movl	$128, -480(%rbp)
	movl	$6, -476(%rbp)
	leaq	-480(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-480(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_bib(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-480(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE3:
	.loc 1 84 0
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE2:
	.size	ib.3514, .-ib.3514
	.globl	__inner_MOD_sb
	.type	__inner_MOD_sb, @function
__inner_MOD_sb:
.LFB1:
	.loc 1 55 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$496, %rsp
	.loc 1 55 0
	movq	$0, -8(%rbp)
.LBB4:
	.loc 1 57 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -488(%rbp)
	movl	$57, -480(%rbp)
	movl	$128, -496(%rbp)
	movl	$6, -492(%rbp)
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-496(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_b(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE4:
	.loc 1 59 0
	leaq	-8(%rbp), %rax
	movq	%rax, %r10
	call	i1.3518
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE1:
	.size	__inner_MOD_sb, .-__inner_MOD_sb
	.type	i2.3516, @function
i2.3516:
.LFB3:
	.loc 1 72 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$504, %rsp
	.cfi_offset 3, -24
	movq	%r10, %rbx
	movq	%r10, -504(%rbp)
.LBB5:
	.loc 1 74 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -488(%rbp)
	movl	$74, -480(%rbp)
	movl	$128, -496(%rbp)
	movl	$6, -492(%rbp)
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-496(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_bi2(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE5:
	.loc 1 75 0
	movq	%rbx, %r10
	call	ib.3514
	.loc 1 77 0
	nop
	addq	$504, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE3:
	.size	i2.3516, .-i2.3516
	.type	i1.3518, @function
i1.3518:
.LFB4:
	.loc 1 64 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$504, %rsp
	.cfi_offset 3, -24
	movq	%r10, %rbx
	movq	%r10, -504(%rbp)
.LBB6:
	.loc 1 66 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -488(%rbp)
	movl	$66, -480(%rbp)
	movl	$128, -496(%rbp)
	movl	$6, -492(%rbp)
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-496(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_bi1(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE6:
	.loc 1 67 0
	movq	%rbx, %r10
	call	i2.3516
	.loc 1 69 0
	nop
	addq	$504, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE4:
	.size	i1.3518, .-i1.3518
	.type	ia.3524, @function
ia.3524:
.LFB6:
	.loc 1 45 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$496, %rsp
	movq	%r10, -488(%rbp)
.LBB7:
	.loc 1 47 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -472(%rbp)
	movl	$47, -464(%rbp)
	movl	$128, -480(%rbp)
	movl	$6, -476(%rbp)
	leaq	-480(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-480(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_aia(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-480(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE7:
	.loc 1 49 0
	nop
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE6:
	.size	ia.3524, .-ia.3524
	.globl	__inner_MOD_sa
	.type	__inner_MOD_sa, @function
__inner_MOD_sa:
.LFB5:
	.loc 1 20 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	subq	$496, %rsp
	.loc 1 20 0
	movq	$0, -8(%rbp)
.LBB8:
	.loc 1 22 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -488(%rbp)
	movl	$22, -480(%rbp)
	movl	$128, -496(%rbp)
	movl	$6, -492(%rbp)
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-496(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_a(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE8:
	.loc 1 24 0
	leaq	-8(%rbp), %rax
	movq	%rax, %r10
	call	i1.3528
	leave
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE5:
	.size	__inner_MOD_sa, .-__inner_MOD_sa
	.type	i2.3526, @function
i2.3526:
.LFB7:
	.loc 1 37 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$504, %rsp
	.cfi_offset 3, -24
	movq	%r10, %rbx
	movq	%r10, -504(%rbp)
.LBB9:
	.loc 1 39 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -488(%rbp)
	movl	$39, -480(%rbp)
	movl	$128, -496(%rbp)
	movl	$6, -492(%rbp)
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-496(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_ai2(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE9:
	.loc 1 40 0
	movq	%rbx, %r10
	call	ia.3524
	.loc 1 42 0
	nop
	addq	$504, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE7:
	.size	i2.3526, .-i2.3526
	.type	i1.3528, @function
i1.3528:
.LFB8:
	.loc 1 29 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	pushq	%rbx
	subq	$504, %rsp
	.cfi_offset 3, -24
	movq	%r10, %rbx
	movq	%r10, -504(%rbp)
.LBB10:
	.loc 1 31 0
	leaq	.LC0(%rip), %rax
	movq	%rax, -488(%rbp)
	movl	$31, -480(%rbp)
	movl	$128, -496(%rbp)
	movl	$6, -492(%rbp)
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write@PLT
	leaq	-496(%rbp), %rax
	movl	$4, %edx
	leaq	__inner_MOD_ai1(%rip), %rsi
	movq	%rax, %rdi
	call	_gfortran_transfer_integer_write@PLT
	leaq	-496(%rbp), %rax
	movq	%rax, %rdi
	call	_gfortran_st_write_done@PLT
.LBE10:
	.loc 1 32 0
	movq	%rbx, %r10
	call	i2.3526
	.loc 1 34 0
	nop
	addq	$504, %rsp
	popq	%rbx
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE8:
	.size	i1.3528, .-i1.3528
	.globl	__inner_MOD_s0
	.type	__inner_MOD_s0, @function
__inner_MOD_s0:
.LFB9:
	.loc 1 11 0
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register 6
	.loc 1 13 0
	call	__inner_MOD_sa
	.loc 1 14 0
	call	__inner_MOD_sb
	.loc 1 15 0
	call	__inner_MOD_i1
	.loc 1 17 0
	nop
	popq	%rbp
	.cfi_def_cfa 7, 8
	ret
	.cfi_endproc
.LFE9:
	.size	__inner_MOD_s0, .-__inner_MOD_s0
.Letext0:
	.section	.debug_info,"",@progbits
.Ldebug_info0:
	.long	0x2ff
	.value	0x4
	.long	.Ldebug_abbrev0
	.byte	0x8
	.uleb128 0x1
	.long	.LASF11
	.byte	0xe
	.byte	0x2
	.long	.LASF12
	.long	.LASF13
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.long	.Ldebug_line0
	.uleb128 0x2
	.long	.LASF14
	.byte	0x1
	.byte	0x1
	.long	0x2fb
	.uleb128 0x3
	.string	"a"
	.byte	0x1
	.byte	0x5
	.long	.LASF0
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_a
	.uleb128 0x3
	.string	"ai1"
	.byte	0x1
	.byte	0x5
	.long	.LASF1
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_ai1
	.uleb128 0x3
	.string	"ai2"
	.byte	0x1
	.byte	0x5
	.long	.LASF2
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_ai2
	.uleb128 0x3
	.string	"aia"
	.byte	0x1
	.byte	0x5
	.long	.LASF3
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_aia
	.uleb128 0x3
	.string	"b"
	.byte	0x1
	.byte	0x5
	.long	.LASF4
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_b
	.uleb128 0x3
	.string	"bi1"
	.byte	0x1
	.byte	0x5
	.long	.LASF5
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_bi1
	.uleb128 0x3
	.string	"bi2"
	.byte	0x1
	.byte	0x5
	.long	.LASF6
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_bi2
	.uleb128 0x3
	.string	"bib"
	.byte	0x1
	.byte	0x5
	.long	.LASF7
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_bib
	.uleb128 0x3
	.string	"c"
	.byte	0x1
	.byte	0x5
	.long	.LASF8
	.long	0x2fb
	.uleb128 0x9
	.byte	0x3
	.quad	__inner_MOD_c
	.uleb128 0x4
	.string	"s0"
	.byte	0x1
	.byte	0xb
	.long	.LASF15
	.quad	.LFB9
	.quad	.LFE9-.LFB9
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x5
	.string	"sa"
	.byte	0x1
	.byte	0x14
	.long	.LASF9
	.quad	.LFB5
	.quad	.LFE5-.LFB5
	.uleb128 0x1
	.byte	0x9c
	.long	0x1fe
	.uleb128 0x6
	.string	"ia"
	.byte	0x1
	.byte	0x2d
	.quad	.LFB6
	.quad	.LFE6-.LFB6
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x5
	.byte	0x91
	.sleb128 -504
	.byte	0x6
	.byte	0x6
	.long	0x184
	.uleb128 0x7
	.quad	.LBB7
	.quad	.LBE7-.LBB7
	.byte	0
	.uleb128 0x6
	.string	"i2"
	.byte	0x1
	.byte	0x25
	.quad	.LFB7
	.quad	.LFE7-.LFB7
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x5
	.byte	0x91
	.sleb128 -520
	.byte	0x6
	.byte	0x6
	.long	0x1b8
	.uleb128 0x7
	.quad	.LBB9
	.quad	.LBE9-.LBB9
	.byte	0
	.uleb128 0x6
	.string	"i1"
	.byte	0x1
	.byte	0x1d
	.quad	.LFB8
	.quad	.LFE8-.LFB8
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x5
	.byte	0x91
	.sleb128 -520
	.byte	0x6
	.byte	0x6
	.long	0x1ec
	.uleb128 0x7
	.quad	.LBB10
	.quad	.LBE10-.LBB10
	.byte	0
	.uleb128 0x7
	.quad	.LBB8
	.quad	.LBE8-.LBB8
	.byte	0
	.uleb128 0x5
	.string	"sb"
	.byte	0x1
	.byte	0x37
	.long	.LASF10
	.quad	.LFB1
	.quad	.LFE1-.LFB1
	.uleb128 0x1
	.byte	0x9c
	.long	0x2cc
	.uleb128 0x6
	.string	"ib"
	.byte	0x1
	.byte	0x50
	.quad	.LFB2
	.quad	.LFE2-.LFB2
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x5
	.byte	0x91
	.sleb128 -504
	.byte	0x6
	.byte	0x6
	.long	0x252
	.uleb128 0x7
	.quad	.LBB3
	.quad	.LBE3-.LBB3
	.byte	0
	.uleb128 0x6
	.string	"i2"
	.byte	0x1
	.byte	0x48
	.quad	.LFB3
	.quad	.LFE3-.LFB3
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x5
	.byte	0x91
	.sleb128 -520
	.byte	0x6
	.byte	0x6
	.long	0x286
	.uleb128 0x7
	.quad	.LBB5
	.quad	.LBE5-.LBB5
	.byte	0
	.uleb128 0x6
	.string	"i1"
	.byte	0x1
	.byte	0x40
	.quad	.LFB4
	.quad	.LFE4-.LFB4
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x5
	.byte	0x91
	.sleb128 -520
	.byte	0x6
	.byte	0x6
	.long	0x2ba
	.uleb128 0x7
	.quad	.LBB6
	.quad	.LBE6-.LBB6
	.byte	0
	.uleb128 0x7
	.quad	.LBB4
	.quad	.LBE4-.LBB4
	.byte	0
	.uleb128 0x8
	.string	"i1"
	.byte	0x1
	.byte	0x59
	.long	.LASF16
	.quad	.LFB0
	.quad	.LFE0-.LFB0
	.uleb128 0x1
	.byte	0x9c
	.uleb128 0x7
	.quad	.LBB2
	.quad	.LBE2-.LBB2
	.byte	0
	.byte	0
	.uleb128 0x9
	.byte	0x4
	.byte	0x5
	.long	.LASF17
	.byte	0
	.section	.debug_abbrev,"",@progbits
.Ldebug_abbrev0:
	.uleb128 0x1
	.uleb128 0x11
	.byte	0x1
	.uleb128 0x25
	.uleb128 0xe
	.uleb128 0x13
	.uleb128 0xb
	.uleb128 0x42
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x1b
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x10
	.uleb128 0x17
	.byte	0
	.byte	0
	.uleb128 0x2
	.uleb128 0x1e
	.byte	0x1
	.uleb128 0x3
	.uleb128 0xe
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x3
	.uleb128 0x34
	.byte	0
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x6e
	.uleb128 0xe
	.uleb128 0x49
	.uleb128 0x13
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x2
	.uleb128 0x18
	.byte	0
	.byte	0
	.uleb128 0x4
	.uleb128 0x2e
	.byte	0
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x6e
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2116
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x5
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x6e
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2116
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x6
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x48
	.uleb128 0x18
	.uleb128 0x2116
	.uleb128 0x19
	.uleb128 0x1
	.uleb128 0x13
	.byte	0
	.byte	0
	.uleb128 0x7
	.uleb128 0xb
	.byte	0
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.byte	0
	.byte	0
	.uleb128 0x8
	.uleb128 0x2e
	.byte	0x1
	.uleb128 0x3f
	.uleb128 0x19
	.uleb128 0x3
	.uleb128 0x8
	.uleb128 0x3a
	.uleb128 0xb
	.uleb128 0x3b
	.uleb128 0xb
	.uleb128 0x6e
	.uleb128 0xe
	.uleb128 0x11
	.uleb128 0x1
	.uleb128 0x12
	.uleb128 0x7
	.uleb128 0x40
	.uleb128 0x18
	.uleb128 0x2116
	.uleb128 0x19
	.byte	0
	.byte	0
	.uleb128 0x9
	.uleb128 0x24
	.byte	0
	.uleb128 0xb
	.uleb128 0xb
	.uleb128 0x3e
	.uleb128 0xb
	.uleb128 0x3
	.uleb128 0xe
	.byte	0
	.byte	0
	.byte	0
	.section	.debug_aranges,"",@progbits
	.long	0x2c
	.value	0x2
	.long	.Ldebug_info0
	.byte	0x8
	.byte	0
	.value	0
	.value	0
	.quad	.Ltext0
	.quad	.Letext0-.Ltext0
	.quad	0
	.quad	0
	.section	.debug_line,"",@progbits
.Ldebug_line0:
	.section	.debug_str,"MS",@progbits,1
.LASF11:
	.string	"GNU Fortran2008 7.2.0 -mtune=generic -march=x86-64 -g -fintrinsic-modules-path /usr/lib/gcc/x86_64-linux-gnu/7/finclude"
.LASF7:
	.string	"__inner_MOD_bib"
.LASF10:
	.string	"__inner_MOD_sb"
.LASF13:
	.string	"/home/christian/workspace/fortrancallgraph/tests/samples/inner"
.LASF5:
	.string	"__inner_MOD_bi1"
.LASF6:
	.string	"__inner_MOD_bi2"
.LASF9:
	.string	"__inner_MOD_sa"
.LASF12:
	.string	"inner.f90"
.LASF3:
	.string	"__inner_MOD_aia"
.LASF15:
	.string	"__inner_MOD_s0"
.LASF16:
	.string	"__inner_MOD_i1"
.LASF1:
	.string	"__inner_MOD_ai1"
.LASF2:
	.string	"__inner_MOD_ai2"
.LASF0:
	.string	"__inner_MOD_a"
.LASF4:
	.string	"__inner_MOD_b"
.LASF8:
	.string	"__inner_MOD_c"
.LASF17:
	.string	"integer(kind=4)"
.LASF14:
	.string	"inner"
	.ident	"GCC: (Ubuntu 7.2.0-8ubuntu3) 7.2.0"
	.section	.note.GNU-stack,"",@progbits
