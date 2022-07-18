PORTB = $6000
PORTA = $6001
DDRB = $6002
DDRA = $6003

E  = %10000000
RW = %01000000
RS = %00100000


  .org $8000


reset:
  ldx #$ff
  txs

  lda #%11111111
  sta DDRB
  lda #%11100000
  sta DDRA

  ;; Set 4-bit operation mode
  jsr lcd_wait
  lda #%00100000
  sta PORTB
  lda #0
  sta PORTA
  lda #E
  sta PORTA
  lda #0
  sta PORTA
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


  ;; Only uses the A and Y registers
lcd_wait:
  lda #%00000000                 ; Set port B to all input
  sta DDRB
lcdbusy:
  lda #RW
  sta PORTA
  lda #(RW | E)
  sta PORTA
  ldy PORTB
  ;; Ignore the second nibble
  lda #RW
  sta PORTA
  lda #(RW | E)
  sta PORTA
  ;; Check if the busy flag is set
  tya
  and #%10000000
  bne lcdbusy

  lda #RW
  sta PORTA
  lda #%11111111  ; Set port B to all output
  sta DDRB
  rts


lcd_instruction:
  ;; Wait for the busy flag
  tax
  jsr lcd_wait
  ;; Send MSB
  stx PORTB
  lda #0                        ; Clear RS/RW/E bits
  sta PORTA
  lda #E                        ; Set E bit to send instruction
  sta PORTA
  lda #0                        ; Clear RS/RW/E bits
  sta PORTA
  txa
  ;; Send LSB
  rol
  rol
  rol
  rol
  sta PORTB
  lda #0                        ; Clear RS/RW/E bits
  sta PORTA
  lda #E                        ; Set E bit to send instruction
  sta PORTA
  lda #0                        ; Clear RS/RW/E bits
  sta PORTA
  rts


lcd_print_char:
  ;; Wait for the busy flag
  tax
  jsr lcd_wait
  ;; Send MSB
  stx PORTB
  lda #RS                       ; Clear RS/RW/E bits
  sta PORTA
  lda #(RS | E)                 ; Set E bit to send instruction
  sta PORTA
  lda #RS                       ; Clear RS/RW/E bits
  sta PORTA
  txa
  ;; Send LSB
  rol
  rol
  rol
  rol
  sta PORTB
  lda #RS                       ; Clear RS/RW/E bits
  sta PORTA
  lda #(RS | E)                 ; Set E bit to send instruction
  sta PORTA
  lda #RS                       ; Clear RS/RW/E bits
  sta PORTA
  rts


  .org $fffc
  .word reset
  .word $0000
