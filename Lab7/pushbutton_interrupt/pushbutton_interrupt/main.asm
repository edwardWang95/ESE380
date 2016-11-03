;******************************************
;*
;* Title:           Pushbutton Interrupt
;* Author:          Edward Wang/ Yash Jain
;* Version:         1.0
;* Last updated:    11/02/16
;* Target:          ATmega16 @ 1MHz
;*
;* DESCRIPTION
;* Handles the interrupts from a pushbutton
/* when inputted from PD3 as well as 
   grabs an input from the dip switch on
   pins PA0 - PA3. Performs the necessary
   table lookup and grabs the appropriate
   hex to 7 segment display value. This value
   is then push to display 0 while its 
   original value is pushed onto display 1. 
   When there is no interrupt, main_loop   
   loops themux_display method to display 
   alternatively display 0 and display1 with
   a delay.
*/
;*
;* VERSION HISTORY
;* 1.0 Original version
;**********************************************
.nolist
.include "m16def.inc"
.list

.equ QCLR   = 0     
;flip-flop asynchronous clear input connected to PD0

.def dig0_seg = r0
.def dig1_seg = r1
.def digit_ON = r2

.cseg

reset:
	.org RESET
		rjmp start ;program starts 
	.org INT1addr  ;INT0 interrupt vector
		rjmp pushbutton_isr
start:
    ;Configure port B as an output port
    ldi r16, $FF        ;load r16 with all 1s
    out DDRB, r16       
	;port B - all bits configured as outputs
	ldi r16, $00    ;initial count is 0
    out PORTB, r16

    ;Configure port C as an output port
    sbi DDRC, 7	;PINC7 configured as output
    sbi DDRC, 6	;PINC6 configured as output

	//Configure PORTD
	ldi r16, $01
	out DDRD, r16
	//reset DFF
	cbi PORTD, 0
	sbi PORTD, 0

	//Configure PORTA
	ldi r16, $40
	out DDRA, r16
	ldi r16, $FF
	out PORTA, r16

    ;Inital delay value
    ldi r20, 1        ; 32

	;Initial value of digit_ON is 1
	ldi r16, 1
	add digit_ON, r16


    ;Initialize stack pointer to allow 
	;subroutine calls
    ldi r16, LOW(RAMEND)    ;load low 
	;byte of stack pointer    
    out SPL, r16
    ldi r16, HIGH(RAMEND)   ;load high 
	;byte of stack pointer
    out SPH, r16

	/*
	ISC00/01 - manage trigger events
			   for INT0
	ISC10/11 - manage trigger events
			   for INT1
	00 - low level
	01 - any logic
	10 - falling edge
	11 - rising edge
	*/
	ldi r16, (1 << ISC11) | (1 << ISC10)    
	//MCUCR: MCU Control Reg
	//contains config for ISCXX
    out MCUCR, r16      
	;rising edge at INT1 requests interrupt
    ldi r16, 1<<INT1    
	//GICR: General Interrupt Control Reg
	//contains interrupt enable bits 
	//for INT0/1/2
    out GICR, r16

	//set global interrupt enable
    sei     

main_loop:
	call mux_display
	rjmp main_loop


;****************************************
;* 
;* "pushbutton_isr" - Push button interrupt
;*
;* Description:at pushbutton interrupt,
;* get input from DIP switch, perform 
;* table lookup from hextable and updates
;* display values.
;*
;* Author:                  Edward Wang/
;*							Yash Jain
;* Version:					1.0
;* Last updated:            0.1
;* Target:                  ATmega16@1MHz
;* Number of words:			12
;* Number of cycles:        21
;* Low registers modified:  ro, r1
;* High registers modified: none
;*
;*
;*
;*******************************************
    ;INT0 interrupt service routine
pushbutton_isr:
    push r16            ;save r16
    in r16, SREG        ;save SREG
    push r16

	//call get_input subroutine
	in r16, PINA     
	;input switch values
    andi r16, $0F       
	;force ms nibble to 0
	call hex_2_7seg

	;generate a negative pulse on pin 0 
	;of port D
    cbi PORTD, QCLR     
	;to clear the flip-flop
    sbi PORTD, QCLR     
	 
	;pop items in stack
	pop r16
	out SREG, r16
	pop r16
	reti

 
;Table of segment values to display digits 0 - F
hextable: .db $01, $4F, $12, $06, $4C, $24, $60, $0F, $00, $0C, $08, $00, $31, $01, $30, $38

;**************************************
;*************************************
;*
;* "hex_2_7seg" - 
;Hexadecimal to Seven-Segment Table Lookup
;*
;* Description:
;* Uses table lookup to convert a 
;hexadecimal value passed in r16 to the
;* seven-segment pattern required to 
;display the hexadecimal value on a
;* common anode display. The seven-segment 
;pattern is returned in r16.
;*
;* Author: Edward Wang, Yash Jain
;* Version: 1.0
;* Last updated: 10/26/16
;* Target: ATmega16 @1Mhz
;* Number of words: 18
;* Number of cycles: 18
;* Low registers modified: none
;* High registers modified: r16
;*
;* Parameters:
;* r16 - right justified hexadecimal value 
;to convert
;*
;* Returns:
;* r16 - seven-segment pattern
;*
;* Notes:
;* Values in the table are for a common 
;anode display. Complement these
;* values when using a common cathode display
;*
;****************************************
;***********************************
hex_2_7seg:		
	;push r25 to stack(SREG)
	push r18		
	;push r18
    ldi ZH, high (hextable * 2)    
	;set Z to point to start of table
    ldi ZL, low (hextable * 2)
    ldi r18, $00                
	;add offset to Z pointer
    add ZL, r16
    adc ZH, r18
    lpm r16, Z                  
	;load byte from table pointed to by Z
	call update_display_image
	pop r18			  
    ret				
	;jump back to read switches again


;**********************************************
;*****************************
;*
;* "update_display_image" - 
;Update Display Image
;*
;* Description:
;* Copies the image of the segment pattern 
;for dig0 stored in r0 (dig0_seg)
;* to the image of the segment pattern for 
;dig1 stored in r1 (dig1_seg).
;* Then copies the segment pattern in 
;r16 to r0. This effectively, shifts
;* what is displayed one digit to the left 
;when the multiplexed display is
;* updated.
;*
;* Author: Yash Jain/ Edward Wang
;* Version: 1.0
;* Last updated: 10/26/16
;* Target: ATmega16 @1Mhz
;* Number of words: 2
;* Number of cycles: 2
;* Low registers modified: r0, r1 - new 
;segment patterns for dig0 and dig1
;* High registers modified: none
;*
;* Parameters:
;* r16 - new segment pattern for dig0
;*
;* Returns:
;* r0, r1 - updated with new segment patterns 
;for dig0 and dig1
;*
;* Notes:
;* Uses def and undef directives to provide 
;aliases for r0 and r1
;* .def dig0_seg = r0
;image of segment pattern for digit 0
;* .def dig1_seg = r1
;image of segment pattern for digit 1
;*****************************************
;**********************************
update_display_image:
	mov dig1_seg, dig0_seg		
	;move dig0 to dig1
	mov dig0_seg, r16
		;call mux_display
    ret				
	;jump back to main_loop


;*********************************
;******************************************
;*
;* "mux_display" - 
;Multiplexes Two-Digit Common Anode 
;Seven-Segment Display
;*
;* Description:
;* Each time this subroutine is called, 
;it turns OFF the previous
;* digit and turns ON the next digit of 
;a two-digit seven segment display.
;* The segment values to be displayed 
;are taken from registers r1 and r0
;* for digits dig1 and dig0, respectively
;. The subroutine maintains a digit
;* counter (r2) indicating which digit is 
;currently being displayed.
;*
;* To keep each digit ON for a longer time
; requires a separate delay
;* subroutine.
;*
;* Author: Yash Jain, Edward Wang
;* Version: 0.0
;* Last updated: 10/26/16
;* Target: ATmega16 @1 Mhz
;* Number of words:
;* Number of cycles:
;* Low registers modified: r2
;* High registers modified: none
;*
;* Parameters: The segment values to be 
;displayed are passed in r0 - r2
;* r0 - dig0_seg
;* r1 - dig1_seg
;*
;* Returns:
;* r2 - digit_ON, increments r2 to select 
;next digit turned ON
;*
;* Notes: 0s turn ON digits and 0s turn ON 
;segments
;* The segments are a through g at PB6 
;through PB0 respectively.
;* The digit driver pins are PC7 and PC6 
;for digits dig1 and dig0
;*
;* Uses def and undef directives to 
;provides aliases for
;* r0, r1, and r2
;* .def dig0_seg = r0;image of segment 
;pattern for digit 0
;* .def dig1_seg = r1;image of segment 
;pattern for digit 1
;* .def digit_ON = r2;lsb indicates 
;digit that is ON
;*
;*****************************************
;**********************************
mux_display:
	push r16
	in r16, SREG
	push r16
	push r18
	push r26
	inc digit_ON
	mov r18, digit_ON
	andi r18, $01 ;lsb is preserved
	sbrc r18, 0   ;test if lsb=0
	call display0
	sbrs r18, 0   ;test if lsb=1
	call display1 
	pop r26
	pop r18
	pop r16
	out SREG, r16
	pop r16
	ret
display0:
	out PORTB, dig0_seg
	cbi PORTC, 7
	sbi PORTC, 6
	;ldi r18, 5
	ldi r16, 100
	call var_delay
	ret
display1:
	out PORTB, dig1_seg
	cbi PORTC, 6
	sbi PORTC, 7
	;ldi r18, 5
	ldi r16, 100
	call var_delay
	ret

;**************************************************
;*************************
;*
;* "var_delay" - Variable Delay - 0.1 ms increments
;*
;* Description:
;* Delays for a time equal to r16 * 0.1 ms when 
;ATmega16 clocked at 1 MHz
;*
;* Author: “authors’ names”
;* Version: 1.0
;* Last updated: 10/09/16
;* Target: ATmega16 @ 1 MHz
;* Number of words:
;* Number of cycles: ~100 * r16
;* Low registers modified: none
;* High registers modified: none
;*
;* Parameters:
;* r16 - outer loop control variable
;*
;* Returns:
;* delay of 0.1ms * r16
;*
;* Notes:
;* Delay is designed for ATmega16 with 1 MHz 
;clock
;*
;*************************************************
;**************************

var_delay:		; load outer loop count
outer_loop:
    ldi r26, 32  ;about 100 us 
inner_loop:
    dec r26
    brne inner_loop
    dec r16
    brne outer_loop
    ret
