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
.enum
  UNDECIDED = 0
  INCREASING = 1
  DECREASING = 2
.endenum

DELTA_LIMIT = 3
BCD_LEN = 3

  input_ptr: .res 2

  last: .res 1
  next: .res 1
  scratch:
  direction: .res 1
  num_safe: .res 2
  bcd_safe: .res BCD_LEN

.segment "RODATA"
input_start:
.include "../inputs/2.input"
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

  ldy #0

outer_loop:
  lda input_ptr + 1
  cmp #>input_end
  bcc :+

  lda input_ptr
  cmp #<input_end
  bcc :+

  rts

: lda #UNDECIDED
  sta direction

  lda (input_ptr), y
  and #$f
  sta next

  jsr progress_ptr

  lda (input_ptr), y
  cmp #' '
  beq inner_loop

  jsr next_mul10

  lda (input_ptr), y
  and #$f
  adc next
  sta next

  jsr progress_ptr

inner_loop:
  lda (input_ptr), y
  bne not_done

inc_safe:
  inc num_safe
  bne :+

  inc num_safe + 1

: jsr progress_ptr
  jmp outer_loop

not_done:
  lda next
  sta last

  jsr progress_ptr

  lda (input_ptr), y
  and #$f
  sta next

  jsr progress_ptr

  lda (input_ptr), y
  beq :+
  cmp #' '
  beq :+

  jsr next_mul10

  lda (input_ptr), y
  and #$f
  adc next
  sta next

  jsr progress_ptr

: lda direction
  cmp #UNDECIDED
  bne compare

set_direction:
  lda last
  cmp next
  bcc set_increasing
  bne set_decreasing

set_equal:
  jsr skip_line
  jmp outer_loop

set_increasing:
  lda #INCREASING
  sta direction
  jmp compare

set_decreasing:
  lda #DECREASING
  sta direction

compare:
  lda last
  cmp next
  bcc confirm_increasing
  bne confirm_decreasing

skip:
  jsr skip_line
  jmp outer_loop

confirm_increasing:
  lda direction
  cmp #INCREASING
  bne skip

  lda next
  sec
  sbc last
  cmp #DELTA_LIMIT + 1
  bcs skip

  jmp inner_loop

confirm_decreasing:
  lda direction
  cmp #DECREASING
  bne skip

  lda last
  sec
  sbc next
  cmp #DELTA_LIMIT + 1
  bcs skip

  jmp inner_loop
.endproc

.proc progress_ptr
  inc input_ptr
  bne :+

  inc input_ptr + 1

: rts
.endproc

.proc skip_line
: lda (input_ptr), y
  beq done

  jsr progress_ptr
  jmp :-

done:
  jmp progress_ptr
.endproc

.proc next_mul10
  ; 8 * next + next + next
  lda next
  asl
  asl
  asl

  clc
  adc next
  adc next

  sta next

  rts
.endproc

.proc present
  jsr calc_bcd

  lda #$20
  sta PPU_ADDR
  lda #$21
  sta PPU_ADDR

  ldy #BCD_LEN - 1
: lda bcd_safe, y
  bne non_zero

  dey
  bpl :-

zero:
  lda #$10
  sta PPU_DATA

  rts

non_zero:
  lda bcd_safe, y
  lsr
  lsr
  lsr
  lsr

  beq :+

  ora #$10
  sta PPU_DATA

: lda bcd_safe, y
  and #$f
  ora #$10
  sta PPU_DATA

  dey
  bmi done

: lda bcd_safe, y
  lsr
  lsr
  lsr
  lsr

  ora #$10
  sta PPU_DATA

  lda bcd_safe, y
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

  ldy #16
iterate:
  ldx #BCD_LEN - 1

check:
  lda bcd_safe, x
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

  lda bcd_safe, x
  and #$f
  ora scratch
  sta bcd_safe, x

: lda bcd_safe, x
  and #$f

  cmp #5
  bcc :+

  clc
  adc #3
  sta scratch

  lda bcd_safe, x
  and #$f0
  ora scratch
  sta bcd_safe, x

: dex
  bpl check

shift:
  asl num_safe
  rol num_safe + 1

  rol bcd_safe
  rol bcd_safe + 1
  rol bcd_safe + 2

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