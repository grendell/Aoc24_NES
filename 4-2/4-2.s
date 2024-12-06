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
NUM_PATTERNS = 4
BCD_LEN = 3

.enum
  M_TL = 1 << 7
  S_TL = 1 << 6
  M_TR = 1 << 5
  S_TR = 1 << 4
  M_BL = 1 << 3
  S_BL = 1 << 2
  M_BR = 1 << 1
  S_BR = 1 << 0
.endenum

  input_ptr: .res 2
  search_ptr: .res 2

  width: .res 1
  height: .res 1

  pos_x: .res 1
  pos_y: .res 1
  delta: .res 1
  pattern: .res 1

  scratch: .res 1
  total: .res 3
  bcd_total: .res BCD_LEN

.segment "RODATA"
input_start:
.include "../inputs/4.input"
input_end:

valid_patterns:
  .byte M_TL | S_BR | M_TR | S_BL
  .byte M_TL | S_BR | S_TR | M_BL
  .byte S_TL | M_BR | M_TR | S_BL
  .byte S_TL | M_BR | S_TR | M_BL

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

  jsr find_dimens
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

.proc find_dimens
  lda #<input_start
  sta input_ptr
  lda #>input_start
  sta input_ptr + 1

  lda #0
  sta width
  sta height

  ldy #0

width_loop:
  lda (input_ptr), y
  beq found_width

  iny
  jmp width_loop

found_width:
  sty width

height_loop:
  lda input_ptr + 1
  cmp #>input_end
  bcc :+

  lda input_ptr
  cmp #<input_end
  bcc :+

  dec height
  rts

: inc height

  lda input_ptr
  clc
  adc width
  sta input_ptr
  bcc :+

  inc input_ptr + 1

: jmp height_loop

rts
.endproc

.proc solve
  lda #<input_start
  sta input_ptr
  lda #>input_start
  sta input_ptr + 1

  ldy #0

loop:
  lda input_ptr + 1
  cmp #>input_end
  bcc :+

  lda input_ptr
  cmp #<input_end
  bcc :+

  rts

: lda (input_ptr), y
  cmp #'A'
  bne progress

  jsr check_pattern

progress:
  jsr progress_ptr
  jmp loop
.endproc

.proc check_pattern
  ; pos_x >= 1 => continue
  lda pos_x
  cmp #1
  bcs :+

  rts

  ; width - 2 >= pos_x => continue
: lda width
  sec
  sbc #2
  cmp pos_x
  bcs :+

  rts

; pos_y >= 1 => continue
: lda pos_y
  cmp #1
  bcs :+

  rts

  ; height - 2 >= pos_y => continue
: lda height
  sec
  sbc #2
  cmp pos_y
  bcs :+

  rts

: lda #0
  sta pattern

top_left:
  lda width
  clc
  adc #2 ; diagonal due to null terminator
  sta delta

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  sec
  sbc delta
  sta search_ptr
  bcs :+

  dec search_ptr + 1

: lda (search_ptr), y
  cmp #'M'
  bne :+

  lda pattern
  ora #M_TL
  sta pattern
  jmp top_right

: lda (search_ptr), y
  cmp #'S'
  bne :+

  lda pattern
  ora #S_TL
  sta pattern
  jmp top_right

  ; no top-left match
: rts

top_right:
  lda width
  sta delta ; diagonal due to null terminator

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  sec
  sbc delta
  sta search_ptr
  bcs :+

  dec search_ptr + 1

: lda (search_ptr), y
  cmp #'M'
  bne :+

  lda pattern
  ora #M_TR
  sta pattern
  jmp bottom_left

: lda (search_ptr), y
  cmp #'S'
  bne :+

  lda pattern
  ora #S_TR
  sta pattern
  jmp bottom_left

  ; no top-right match
: rts

bottom_left:
  lda width
  sta delta ; diagonal due to null terminator

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  clc
  adc delta
  sta search_ptr
  bcc :+

  inc search_ptr + 1

: lda (search_ptr), y
  cmp #'M'
  bne :+

  lda pattern
  ora #M_BL
  sta pattern
  jmp bottom_right

: lda (search_ptr), y
  cmp #'S'
  bne :+

  lda pattern
  ora #S_BL
  sta pattern
  jmp bottom_right

  ; no bottom-left match
: rts

bottom_right:
  lda width
  clc
  adc #2 ; diagonal due to null terminator
  sta delta

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  clc
  adc delta
  sta search_ptr
  bcc :+

  inc search_ptr + 1

: lda (search_ptr), y
  cmp #'M'
  bne :+

  lda pattern
  ora #M_BR
  sta pattern
  jmp check

: lda (search_ptr), y
  cmp #'S'
  bne :+

  lda pattern
  ora #S_BR
  sta pattern
  jmp check

  ; no bottom-right match
: rts

check:
  ldx #NUM_PATTERNS - 1
  lda pattern
: cmp valid_patterns, x
  beq found

  dex
  bpl :-

  ; no matches
  rts

found:
  jmp inc_total
.endproc

.proc progress_ptr
  ldy #0

  inc input_ptr
  bne :+

  inc input_ptr + 1

: lda (input_ptr), y
  beq wrap

  inc pos_x
  rts

wrap:
  inc input_ptr
  bne :+

  inc input_ptr + 1

: sty pos_x
  inc pos_y

  rts
.endproc

.proc inc_total
  inc total
  bne done

  inc total + 1
  bne done

  inc total + 2

done:
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

  ldy #24
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

  rol bcd_total
  rol bcd_total + 1
  rol bcd_total + 2

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