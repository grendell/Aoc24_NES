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
  line_ptr: .res 2

  last: .res 1
  next: .res 1
  scratch:
  direction: .res 1
  num_safe: .res 2
  bcd_safe: .res BCD_LEN

  num_levels: .res 1
  curr_index: .res 1
  skip_index: .res 1

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

; 1 iteration per report
outer_loop:
  lda input_ptr + 1
  cmp #>input_end
  bcc :+

  lda input_ptr
  cmp #<input_end
  bcc :+

  rts

: lda input_ptr
  sta line_ptr
  lda input_ptr + 1
  sta line_ptr + 1

  lda #0
  sta skip_index

  lda #1
  sta num_levels

  ldy #0

: lda (input_ptr), y
  beq after_count

  iny
  cmp #' '
  bne :-

  inc num_levels
  jmp :-

after_count:
  ldy #0

; 1 iteration per skip_index
skip_loop:
  lda line_ptr
  sta input_ptr
  lda line_ptr + 1
  sta input_ptr + 1

  lda #0
  sta curr_index

  lda #UNDECIDED
  sta direction

  jsr enforce_skip

  lda (input_ptr), y
  beq inc_safe

  lda (input_ptr), y
  and #$f
  sta next

  jsr progress_ptr

  lda (input_ptr), y
  cmp #' '
  beq skip_space

  jsr next_mul10

  lda (input_ptr), y
  and #$f
  adc next
  sta next

  jsr progress_ptr

skip_space:
  jsr progress_ptr

inner_loop:
  jsr enforce_skip

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

  lda (input_ptr), y
  and #$f
  sta next

  jsr progress_ptr

  lda (input_ptr), y
  beq after_progression
  cmp #' '
  beq :+

  jsr next_mul10

  lda (input_ptr), y
  and #$f
  adc next
  sta next

  jsr progress_ptr

  lda (input_ptr), y
  beq after_progression

: jsr progress_ptr

after_progression:
  lda direction
  cmp #UNDECIDED
  bne compare

set_direction:
  lda last
  cmp next
  bcc set_increasing
  bne set_decreasing

set_equal:
  jmp skip

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
  lda skip_index
  cmp num_levels
  bne reset_line

  jsr skip_line
  jmp outer_loop

reset_line:
  inc skip_index
  jmp skip_loop

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

.proc enforce_skip
  lda curr_index
  cmp skip_index
  beq :+

  inc curr_index
  rts

: lda (input_ptr), y
  beq done
  cmp #' '
  beq done

  jsr progress_ptr
  jmp :-

done:
  inc curr_index

  lda (input_ptr), y
  beq :+

  jmp progress_ptr

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