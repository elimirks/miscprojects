  .include "base.s"
  .org $8000

main:
  lda #"f"
  jsr lcd_print_char
  lda #"i"
  jsr lcd_print_char
  lda #"b"
  jsr lcd_print_char
  lda #"("
  jsr lcd_print_char
  lda VIA_PORTA
  and #$0f
  pha
  jsr lcd_print_hex_nibble
  lda #")"
  jsr lcd_print_char
  lda #"="
  jsr lcd_print_char

  pla
  jsr fib_itr
  jsr lcd_print_hex
  rts


  ;; Iterative fibonacci function.
  ;; Expects register A as the input. Returns via register A
  ;; Uses zero page addresses $0000-$0001
  ;; Undefined behavior for $e and $f
fib_itr:
  ;; General case
  tay       ; Store the current n value at register Y, safe and warm
  ;; Initially, $0000-$0001 = [0,1] = [n-1, n]
  ldx #0
  stx $0000 ; 2 numbers ago
  ldx #1
  stx $0001 ; 1 number ago
fib_loop:
  ;; Condition check: Have we iterated all the way through N yet?
  tya
  cmp #0
  beq fib_itr_end
  dey
  ;; Arithmetic fun
  lda $0000
  clc
  adc $0001
  tax       ; Transfer result to register X as a temporary
  lda $0001 ; Move $0001 to $0000, aka n-1 to n-2
  sta $0000
  stx $0001 ; Move result (n) to n-1
  jmp fib_loop
fib_itr_end:
  lda $0000 ; Result is stored at the "previous" zero page address
  rts
