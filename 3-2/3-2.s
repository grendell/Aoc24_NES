.segment "HEADER"
; https://www.nesdev.org/wiki/INES
  .byte $4e, $45, $53, $1a ; iNES header identifier
  .byte $02                ; 2x 16KB PRG code
  .byte $01                ; 1x  8KB CHR data
  .byte $00                ; mapper 0 and horizontal mirroring
  .byte $00                ; mapper 0

.segment "VECTORS"
  .addr nmi, reset, 0

.segment "ZEROPAGE"
BCD_LEN = 5

  input_ptr: .res 2

  mul_enabled: .res 1

  multiplier: .res 2
  multiplicand: .res 2
  product: .res 4
  scratch: .res 2

  total: .res 4
  bcd_total: .res BCD_LEN

.segment "RODATA"
input_start:
.include "../inputs/3.input"
input_end:

.include "system.inc"
.include "chars.inc"

.segment "CODE"
.proc reset
  ; https://www.nesdev.org/wiki/Init_code
  sei                    ; ignore IRQs
  cld                    ; disable decimal mode
  ldx #$40
  stx APU_FRAME_COUNTER  ; disable APU frame IRQ
  ldx #$ff
  txs                    ; Set up stack
  inx                    ; now X = 0
  stx PPU_CTRL           ; disable NMI
  stx PPU_MASK           ; disable rendering
  stx DMC_FREQ           ; disable DMC IRQs

  ; clear vblank flag
  bit PPU_STATUS

  ; wait for first vblank
: bit PPU_STATUS
  bpl :-

  ; initialize cpu variables
  lda #0
  ldx #0
: sta $00, x
  inx
  bne :-

  lda #1
  sta mul_enabled

  ; wait for second vblank
: bit PPU_STATUS
  bpl :-

  ; initialize ppu
  jsr init_palettes
  jsr init_nametables

  jsr solve
  jsr present

  ; enable NMI and select pattern tables
  lda #%10000000
  sta PPU_CTRL

forever:
  jmp forever
.endproc

.proc nmi
  ; clear vblank flag
  bit PPU_STATUS

  ; show backgrounds
  lda #%00001000
  sta PPU_MASK

  ; update background scroll position
  lda #0
  sta PPU_SCROLL
  sta PPU_SCROLL

  rti
.endproc

.proc init_palettes
  ; set ppu address to palette entries ($3f00)
  lda #$3f
  sta PPU_ADDR
  lda #0
  sta PPU_ADDR

  ; loop through each palette entry, 32 total
  ldx #0
: lda palettes, x
  sta PPU_DATA
  inx
  cpx #32
  bne :-

  rts
.endproc

.proc init_nametables
  ; set ppu address to first nametable ($2000)
  lda #$20
  sta PPU_ADDR
  lda #0
  sta PPU_ADDR

  ; set next 2048 bytes (nametables + attributes) to 0
  ldx #0
  ldy #8
: sta PPU_DATA
  inx
  bne :-

  dey
  bne :-

  rts
.endproc

.proc solve
  lda #<input_start
  sta input_ptr
  lda #>input_start
  sta input_ptr + 1

loop:
  lda input_ptr + 1
  cmp #>input_end
  bcc :+

  lda input_ptr
  cmp #<input_end
  bcc :+

  rts

: lda mul_enabled
  beq disabled

enabled:
  jsr check_for_dont

  lda mul_enabled
  beq loop

  jsr check_for_mul
  jmp loop

disabled:
  jsr check_for_do
  jmp loop
.endproc

.proc check_for_do
  ldy #0

  ; "do()"
  lda (input_ptr), y
  cmp #'d'
  beq :+

  jsr progress_ptr
  rts

: iny
  lda (input_ptr), y
  cmp #'o'
  beq :+

  jsr progress_ptr
  rts

: iny
  lda (input_ptr), y
  cmp #'('
  beq :+

  jsr progress_ptr
  rts

: iny
  lda (input_ptr), y
  cmp #')'
  beq found

  jsr progress_ptr
  rts

found:
  lda #1
  sta mul_enabled

  tya
  sec
  adc input_ptr
  sta input_ptr

  bcc :+

  inc input_ptr + 1

: rts
.endproc

.proc check_for_dont
  ldy #0

  ; "don't()"
  lda (input_ptr), y
  cmp #'d'
  beq :+

  rts

: iny
  lda (input_ptr), y
  cmp #'o'
  beq :+

  rts

: iny
  lda (input_ptr), y
  cmp #'n'
  beq :+

  rts

: iny
  lda (input_ptr), y
  cmp #$27 ; apostrophe
  beq :+

  rts

: iny
  lda (input_ptr), y
  cmp #'t'
  beq :+

  rts

: iny
  lda (input_ptr), y
  cmp #'('
  beq :+

  rts

: iny
  lda (input_ptr), y
  cmp #')'
  beq found

  rts

found:
  lda #0
  sta mul_enabled

  tya
  sec
  adc input_ptr
  sta input_ptr

  bcc :+

  inc input_ptr + 1

: rts
.endproc

.proc check_for_mul
  ldy #0

  ; "mul("
  lda (input_ptr), y
  cmp #'m'
  beq :+

  jsr progress_ptr
  rts

: iny
  lda (input_ptr), y
  cmp #'u'
  beq :+

  jsr progress_ptr
  rts

: iny
  lda (input_ptr), y
  cmp #'l'
  beq :+

  jsr progress_ptr
  rts

: iny
  lda (input_ptr), y
  cmp #'('
  beq :+

  jsr progress_ptr
  rts

  ; 1 - 3 digit multiplier
: lda #0
  sta multiplier
  sta multiplier + 1

  iny
  lda (input_ptr), y
  jsr is_digit
  bcs :+

  jsr progress_ptr
  rts

: and #$f
  sta multiplier

  iny
  lda (input_ptr), y
  jsr is_digit
  bcs :+

  cmp #','
  beq after_comma

  jsr progress_ptr
  rts

: jsr multiplier_mul10

  lda (input_ptr), y
  and #$f
  clc
  adc multiplier
  sta multiplier

  bcc :+

  inc multiplier + 1

: iny
  lda (input_ptr), y
  jsr is_digit
  bcs :+

  cmp #','
  beq after_comma

  jsr progress_ptr
  rts

: jsr multiplier_mul10

  lda (input_ptr), y
  and #$f
  clc
  adc multiplier
  sta multiplier

  bcc :+

  inc multiplier + 1

: iny
  lda (input_ptr), y
  cmp #','
  beq after_comma

  jsr progress_ptr
  rts

after_comma:
  ; 1 - 3 digit multiplicand
  lda #0
  sta multiplicand
  sta multiplicand + 1

  iny
  lda (input_ptr), y
  jsr is_digit
  bcs :+

  jsr progress_ptr
  rts

: and #$f
  sta multiplicand

  iny
  lda (input_ptr), y
  jsr is_digit
  bcs :+

  cmp #')'
  beq found

  jsr progress_ptr
  rts

: jsr multiplicand_mul10

  lda (input_ptr), y
  and #$f
  clc
  adc multiplicand
  sta multiplicand

  bcc :+

  inc multiplicand + 1

: iny
  lda (input_ptr), y
  jsr is_digit
  bcs :+

  cmp #')'
  beq found

  jsr progress_ptr
  rts

: jsr multiplicand_mul10

  lda (input_ptr), y
  and #$f
  clc
  adc multiplicand
  sta multiplicand

  bcc :+

  inc multiplicand + 1

: iny
  lda (input_ptr), y
  cmp #')'
  beq found

  jsr progress_ptr
  rts

found:
  jsr mul16
  jsr add_product_to_total

  tya
  ldy #0

  sec
  adc input_ptr
  sta input_ptr

  bcc :+

  inc input_ptr + 1

: rts
.endproc

.proc progress_ptr
  inc input_ptr
  bne :+

  inc input_ptr + 1

: rts
.endproc

.proc is_digit
; checks a, sets c

  cmp #'0'
  bcc fail

  cmp #'9' + 1
  bcs fail

pass:
  sec
  rts

fail:
  clc
  rts
.endproc

.proc multiplier_mul10
; 8 * multiplier + 2 * multiplier

  asl multiplier
  rol multiplier + 1

  lda multiplier
  sta scratch
  lda multiplier + 1
  sta scratch + 1

  asl multiplier
  rol multiplier + 1
  asl multiplier
  rol multiplier + 1

  lda multiplier
  clc
  adc scratch
  sta multiplier

  lda multiplier + 1
  adc scratch + 1
  sta multiplier + 1

  rts
.endproc

.proc multiplicand_mul10
; 8 * multiplicand + 2 * multiplicand

  asl multiplicand
  rol multiplicand + 1

  lda multiplicand
  sta scratch
  lda multiplicand + 1
  sta scratch + 1

  asl multiplicand
  rol multiplicand + 1
  asl multiplicand
  rol multiplicand + 1

  lda multiplicand
  clc
  adc scratch
  sta multiplicand

  lda multiplicand + 1
  adc scratch + 1
  sta multiplicand + 1

  rts
.endproc

.proc mul16
; https://codebase64.org/doku.php?id=base:16bit_multiplication_32-bit_product

  lda #0
  sta product + 2
  sta product + 3

  ldx #16
shift:
  lsr multiplier + 1
  ror multiplier
  bcc rotate

  lda product + 2
  clc
  adc multiplicand
  sta product + 2

  lda product + 3
  adc multiplicand + 1

rotate:
  ror
  sta product + 3
  ror product + 2
  ror product + 1
  ror product

  dex
  bne shift

  rts
.endproc

.proc add_product_to_total
  lda product
  clc
  adc total
  sta total

  lda product + 1
  adc total + 1
  sta total + 1

  lda product + 2
  adc total + 2
  sta total + 2

  lda product + 3
  adc total + 3
  sta total + 3

  rts
.endproc

.proc present
  jsr calc_bcd

  lda #$20
  sta PPU_ADDR
  lda #$21
  sta PPU_ADDR

  ldy #BCD_LEN - 1
: lda bcd_total, y
  bne non_zero

  dey
  bpl :-

zero:
  lda #$10
  sta PPU_DATA

  rts

non_zero:
  lda bcd_total, y
  lsr
  lsr
  lsr
  lsr

  beq :+

  ora #$10
  sta PPU_DATA

: lda bcd_total, y
  and #$f
  ora #$10
  sta PPU_DATA

  dey
  bmi done

: lda bcd_total, y
  lsr
  lsr
  lsr
  lsr

  ora #$10
  sta PPU_DATA

  lda bcd_total, y
  and #$f
  ora #$10
  sta PPU_DATA

  dey
  bpl :-

done:
  rts
.endproc

.proc calc_bcd
; https://en.wikipedia.org/wiki/Double_dabble

  ldy #32
iterate:
  ldx #BCD_LEN - 1

check:
  lda bcd_total, x
  lsr
  lsr
  lsr
  lsr

  cmp #5
  bcc :+

  clc
  adc #3

  asl
  asl
  asl
  asl
  sta scratch

  lda bcd_total, x
  and #$f
  ora scratch
  sta bcd_total, x

: lda bcd_total, x
  and #$f

  cmp #5
  bcc :+

  clc
  adc #3
  sta scratch

  lda bcd_total, x
  and #$f0
  ora scratch
  sta bcd_total, x

: dex
  bpl check

shift:
  asl total
  rol total + 1
  rol total + 2
  rol total + 3

  rol bcd_total
  rol bcd_total + 1
  rol bcd_total + 2
  rol bcd_total + 3
  rol bcd_total + 4

  dey
  bne iterate

  rts
.endproc

palettes:
; https://www.nesdev.org/wiki/PPU_palettes
  ; background palettes
  .byte $0f, $20, $0f, $0f
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f

  ; sprite palettes
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f
  .byte $0f, $0f, $0f, $0f