PORTB = $6000                   ; I/O data register B
PORTA = $6001                   ; I/O data register A
DDRB = $6002                    ; Data direction register B
DDRA = $6003                    ; Data direction register A

E  = %00001000
RW = %00000100
RS = %00000010


  .org $8000


reset:
  ;; Initialize the stack pointer
  ldx #$ff
  txs

  jsr lcd_init
  ;; Set 4-bit 2-row mode, using the secondary font (non-jp)
  lda #%00101000
  jsr lcd_instruction
  ;; Clear display
  lda #%00000001
  jsr lcd_instruction
  ;; Turn on display and cursor
  lda #%00001110
  jsr lcd_instruction
  ;; Character entry mode set
  lda #%00000110
  jsr lcd_instruction
  ;; Write "i8\sum\pi :)"
  lda #%01101001
  jsr lcd_print_char
  lda #%00111000
  jsr lcd_print_char
  lda #%11110110
  jsr lcd_print_char
  lda #%11110111
  jsr lcd_print_char
  lda #%00100000
  jsr lcd_print_char
  lda #%11000010
  jsr lcd_print_char


eofspin:
  jmp eofspin


lcd_init:
  ;; Set data direction to all outputs for I/O register B
  lda #%11111111
  sta DDRB
  ;; Set 4-bit operation mode
  jsr lcd_wait
  lda #%00100000
  sta PORTB
  ora #E
  sta PORTB
  and #(~E)
  sta PORTB
  rts


  ;; Only uses the A and Y registers
lcd_wait:
  lda #%00001111
  sta DDRB
lcdbusy:
  lda #RW
  sta PORTB
  lda #(RW | E)
  sta PORTB
  ldy PORTB
  ;; Ignore the second nibble
  lda #RW
  sta PORTB
  lda #(RW | E)
  sta PORTB
  ;; Check if the busy flag is set
  tya
  and #%10000000
  bne lcdbusy

  lda #RW
  sta PORTB
  lda #%11111111  ; Set port B to all output
  sta DDRB
  rts


  ;; The A register is the instruction parameter
lcd_instruction:
  ;; Wait for the busy flag
  tax
  jsr lcd_wait
  ;; Send MSB
  txa
  and #%11110000                ; Clear RS/RW/E bits
  sta PORTB
  ora #E                        ; Set E bit to send instruction
  sta PORTB
  and #(~E)
  sta PORTB
  txa
  ;; Send LSB
  rol
  rol
  rol
  rol
  and #%11110000
  sta PORTB
  ora #E
  sta PORTB
  and #(~E)
  sta PORTB
  rts


  ;; The A register is the data character parameter
lcd_print_char:
  ;; Wait for the busy flag
  tax
  jsr lcd_wait
  ;; Send MSB
  txa
  and #%11110000                ; Clear RS/RW/E bits
  sta PORTB
  ora #(RS | E)                 ; Set E bit and RS to send data
  sta PORTB
  and #(~(RS | E))
  sta PORTB
  txa
  ;; Send LSB
  rol
  rol
  rol
  rol
  and #%11110000
  sta PORTB
  ora #(RS | E)
  sta PORTB
  and #(~(RS | E))
  sta PORTB
  rts


  .org $fffc
  .word reset
  .word $0000
