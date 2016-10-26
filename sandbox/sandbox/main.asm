;
; sandbox.asm
;
; Created: 10/26/2016 2:54:21 AM
; Author : edwar
;


; Replace with your application code

.nolist
.include "m16def.inc"
.list

init:
	;init stack
	ldi r16, LOW(RAMEND)
	out SPL, r16
	ldi r16, HIGH(RAMEND)
	out SPH, r16

start:
    ldi r16, 1
	call var_delay;
	rjmp start

var_Delay:
	push r16
	in r25, SREG
	push r25
	ldi r17, 25
	outer_loop:
		dec r17
		nop
		brne outer_loop
		dec r16
		brne outer_loop
	pop r25
	out SREG, r25
	pop r16
	ret
