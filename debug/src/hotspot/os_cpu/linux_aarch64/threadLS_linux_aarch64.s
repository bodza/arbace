        // JavaThread::aarch64_get_thread_helper()
        //
        // Return the current thread pointer in x0.
        // Clobber x1, flags.
        // All other registers are preserved,

	.global	_ZN10JavaThread25aarch64_get_thread_helperEv
	.type	_ZN10JavaThread25aarch64_get_thread_helperEv, %function

_ZN10JavaThread25aarch64_get_thread_helperEv:
	stp x29, x30, [sp, -16]!
	adrp x0, :tlsdesc:_ZN6Thread12_thr_currentE
	ldr x1, [x0, #:tlsdesc_lo12:_ZN6Thread12_thr_currentE]
	add x0, x0, :tlsdesc_lo12:_ZN6Thread12_thr_currentE
	.tlsdesccall _ZN6Thread12_thr_currentE
	blr x1
	mrs x1, tpidr_el0
	add x0, x1, x0
	ldr x0, [x0]
	ldp x29, x30, [sp], 16
	ret

	.size _ZN10JavaThread25aarch64_get_thread_helperEv, .-_ZN10JavaThread25aarch64_get_thread_helperEv
