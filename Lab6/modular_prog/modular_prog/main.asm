;
; update_display_image.asm
;
; Created: 10/26/2016 2:00:03 PM
; Author : yashj
;
;input: from the given circuitory, we are getting
;a hex value from PD0-PD3 as well as PD7 for the 
;flag for the DFF
;output: using the hex_2_7_seg subroutine, table 
;lookup is implemented to convert hex value from 
;the switch bank into its corresponding 7seg value
;this value is outputted to PB0-PB7 and the 2 displays
;are muxed using PC6-PC7

.nolist
.include "m16def.inc"
.list

.def dig0_seg = r0
.def dig1_seg = r1

.cseg
reset:
    ;Configure port B as an output port
    ldi r16, $FF        ;load r16 with all 1s
    out DDRB, r16       ;port B - all bits configured as outputs

    ;Configure port C as an output port
    sbi DDRC, 7			;PINC7 configured as output
    sbi DDRC, 6			;PINC6 configured as output

   ;Configure port D as an input/Output port
    ldi r16, $40        ;load r16 with all 0s
    out DDRD, r16       ;port D - all bits configured as inputs except PD6 - output
    ldi r16, $0F		;enable pull-up resistors for PD0-PD3 by outputting 1's
	out PORTD, r16		;make sure the DFF output is initially cleared
	ldi r16, $4F        ;output 1 to PD6 to clr' input of DFF
	out PORTD, r16      ;output 01001111, output 1 to clr'

    ;Inital delay value
    ldi r20, 1         ; 32

    ;Initialize stack pointer to allow subroutine calls
    ldi r16, LOW(RAMEND)    ;load low byte of stack pointer    
    out SPL, r16
    ldi r16, HIGH(RAMEND)   ;load high byte of stack pointer
    out SPH, r16

main_loop:
    in r16, PIND        ;input switch values
    andi r16, $0f       ;force ms nibble to 0
	cbi PORTC, 6        ;turn ON digit 0
	out PORTB, r0		;updated value is in dig 0
    call on_delay       ;timeout digit ON time
	sbi PORTC, 6
    cbi PORTC, 7		;turn ON digit 1
	out PORTB, r1		;old value in dig0 is pushed to dig1
    call off_delay      ;timeout digit OFF time
	sbi PORTC, 7
wf1:
    sbis PIND, 7		;skip to subroutine if PIND7 is set
	rjmp main_loop
	call update_display_image              
 
 clear_DFF:
	cbi PORTD, 6		;Clear flip-flop
	sbi PORTD, 6		;output a 1 to clear'
	rjmp main_loop
    ;Table of segment values to display digits 0 - F
hextable: .db $01, $4F, $12, $06, $4C, $24, $60, $0F, $00, $0C, $08, $00, $31, $01, $30, $38

on_delay:   ;delay for ATmega16 @ 1MHz
off_delay:
    mov r17, r20		; load outer loop count
outer_loop:
    ldi r16, 1         ; 40 inner loop is about 123 us (called twice per period)
inner_loop:
    dec r16
    brne inner_loop
    dec r17
    brne outer_loop
    ret

;***************************************************************************
;*
;* "update_display_image" - Update Display Image
;*
;* Description:
;* Copies the image of the segment pattern for dig0 stored in r0 (dig0_seg)
;* to the image of the segment pattern for dig1 stored in r1 (dig1_seg).
;* Then copies the segment pattern in r16 to r0. This effectively, shifts
;* what is displayed one digit to the left when the multiplexed display is
;* updated.
;*
;* Author: “authors’ names”
;* Version: 0.0
;* Last updated: 10/09/16
;* Target:
;* Number of words:
;* Number of cycles:
;* Low registers modified: r0, r1 - new segment patterns for dig0 and dig1
;* High registers modified: none
;*
;* Parameters:
;* r16 - new segment pattern for dig0
;*
;* Returns:
;* r0, r1 - updated with new segment patterns for dig0 and dig1
;*
;* Notes:
;* Uses def and undef directives to provide aliases for r0 and r1
;* .def dig0_seg = r0;image of segment pattern for digit 0
;* .def dig1_seg = r1;image of segment pattern for digit 1
;***************************************************************************

update_display_image:
	in r25, SREG	
	push r25		;push r25 to stack(SREG)
	push r18		;offset for ZH
	mov dig1_seg, dig0_seg		;move dig0 to dig1
    ldi ZH, high (hextable * 2)    ;set Z to point to start of table
    ldi ZL, low (hextable * 2)
    ldi r18, $00                ;add offset to Z pointer
    add ZL, r16
    adc ZH, r18
    lpm r16, Z                  ;load byte from table pointed to by Z
	mov dig0_seg, r16
	pop r18			;pop r18 off the stack and restore the previous value
	pop r25			;pop r25 off back 
	out SREG, r25   ;restore r25 to value before subroutine
    ret				;jump back to read switches again

;***************************************************************************
;*
;* Title: Modular Program
;* Author: Edward Wang/Yash Jain
;* Version: 1.0
;* Last updated: 10/26/15
;* Target: ATmega16
;*
;* DESCRIPTION
;* This system inputs a hex code from a 4-position DIP switch each time a
;* pushbutton is pressed. The hex digit previously displayed on the right
;* digit of a two-digit seven-segment display is displayed the left digit of
;* the display. The newly entered hex value is displayed on the right digit.
;*
;* Ports:
;* PortB:
;* PB6 - PB0 outputs, segments a through g of common anode seven-seg display
;*
;* PortC:
;* PC7 output, digit driver 1 control
;* PC6 output, digit driver 0 control
;* PC5 - PC2 JTAG interface, do not modify (IMPORTANT)
;*
;* PortD:
;* PD7 input, Q output data valid flag flip-flop
;* PD6 output, /CLR data valid flag flip-flop
;* PD3 - PD0 input, from 4-position DIP switch
;*
;* VERSION HISTORY
;* 1.0 Original version
;***************************************************************************
modular_prog:
	in r25, SREG
	push r25
	push r16		;push curr value of r16
	sbis PIND, 7	;wait for button press
	rjmp modular_prog
	in r16, PIND
	andi r16, $0F	;get lower nibble from switch bank
	ldi ZL, LOW(hextable*2)
	ldi ZH, HIGH(hextable*2)
	ldi r18, $00	
	add ZL, r16		;add offset
	adc ZH, r18
	lpm r16, Z		;get new value of r16
	pop r17		;push previous value to r17
	lsl r17			;shift r17 to msb
	lsl r17
	lsl r17
	lsl r17
	pop r25		;restore SREG value
	out SREG, r25

	;display dig0
	out PORTB, r16
	sbi PORTC, 6
	call var_delay
	cbi PORTC, 6
	;display dig1
	out PORTB, r17
	sbi PORTC, 7
	call var_delay
	cbi PORTC, 7

	;clear DFF
	cbi PIND, 6
	sbi PIND, 6
ret