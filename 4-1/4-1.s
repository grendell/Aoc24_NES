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
SEARCH_LEN = 4
BCD_LEN = 3

  input_ptr: .res 2
  search_ptr: .res 2

  width: .res 1
  height: .res 1

  pos_x: .res 1
  pos_y: .res 1
  delta: .res 1

  scratch: .res 1
  total: .res 3
  bcd_total: .res BCD_LEN

.segment "RODATA"
input_start:
.include "../inputs/4.input"
input_end:

target:
  .asciiz "XMAS"

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
  cmp target
  bne progress

  jsr check_up
  jsr check_upright
  jsr check_right
  jsr check_downright
  jsr check_down
  jsr check_downleft
  jsr check_left
  jsr check_upleft

progress:
  jsr progress_ptr
  jmp loop
.endproc

.proc check_up
  ; pos_y >= search_len - 1 => continue
  lda pos_y
  cmp #SEARCH_LEN - 1
  bcs :+

  rts

: lda width
  clc
  adc #1 ; skip null terminator
  sta delta

  ldx #1

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  sec
  sbc delta
  sta search_ptr
  bcs loop

  dec search_ptr + 1

loop:
  lda (search_ptr), y
  cmp target, x
  bne done

  lda search_ptr
  sec
  sbc delta
  sta search_ptr
  bcs :+

  dec search_ptr + 1

: inx
  cpx #SEARCH_LEN
  bcc loop

  jmp inc_total

done:
  rts
.endproc

.proc check_upright
  ; width - search_len >= pos_x => continue
  lda width
  sec
  sbc #SEARCH_LEN
  cmp pos_x
  bcs :+

  rts

  ; pos_y >= search_len - 1 => continue
  lda pos_y
  cmp #SEARCH_LEN - 1
  bcs :+

  rts

: lda width
  sta delta ; diagonal due to null terminator

  ldx #1

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  sec
  sbc delta
  sta search_ptr
  bcs loop

  dec search_ptr + 1

loop:
  lda (search_ptr), y
  cmp target, x
  bne done

  lda search_ptr
  sec
  sbc delta
  sta search_ptr
  bcs :+

  dec search_ptr + 1

: inx
  cpx #SEARCH_LEN
  bcc loop

  jmp inc_total

done:
  rts
.endproc

.proc check_right
  ; width - search_len >= pos_x => continue
  lda width
  sec
  sbc #SEARCH_LEN
  cmp pos_x
  bcs :+

  rts

: lda #1
  sta delta

  ldx #1

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  clc
  adc delta
  sta search_ptr
  bcc loop

  inc search_ptr + 1

loop:
  lda (search_ptr), y
  cmp target, x
  bne done

  lda search_ptr
  clc
  adc delta
  sta search_ptr
  bcc :+

  inc search_ptr + 1

: inx
  cpx #SEARCH_LEN
  bcc loop

  jmp inc_total

done:
  rts
.endproc

.proc check_downright
  ; width - search_len >= pos_x => continue
  lda width
  sec
  sbc #SEARCH_LEN
  cmp pos_x
  bcs :+

  rts

  ; height - search_len >= y => continue
: lda height
  sec
  sbc #SEARCH_LEN
  cmp pos_y
  bcs :+

  rts

: lda width
  clc
  adc #2 ; skip null terminator + 1
  sta delta

  ldx #1

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  clc
  adc delta
  sta search_ptr
  bcc loop

  inc search_ptr + 1

loop:
  lda (search_ptr), y
  cmp target, x
  bne done

  lda search_ptr
  clc
  adc delta
  sta search_ptr
  bcc :+

  inc search_ptr + 1

: inx
  cpx #SEARCH_LEN
  bcc loop

  jmp inc_total

done:
  rts
.endproc

.proc check_down
  ; height - search_len >= y => continue
  lda height
  sec
  sbc #SEARCH_LEN
  cmp pos_y
  bcs :+

  rts

: lda width
  clc
  adc #1 ; skip null terminator
  sta delta

  ldx #1

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  clc
  adc delta
  sta search_ptr
  bcc loop

  inc search_ptr + 1

loop:
  lda (search_ptr), y
  cmp target, x
  bne done

  lda search_ptr
  clc
  adc delta
  sta search_ptr
  bcc :+

  inc search_ptr + 1

: inx
  cpx #SEARCH_LEN
  bcc loop

  jmp inc_total

done:
  rts
.endproc

.proc check_downleft
  ; pos_x >= search_len - 1 => continue
  lda pos_x
  cmp #SEARCH_LEN - 1
  bcs :+

  rts

  ; height - search_len >= y => continue
: lda height
  sec
  sbc #SEARCH_LEN
  cmp pos_y
  bcs :+

  rts

: lda width
  sta delta ; diagonal due to null terminator

  ldx #1

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  clc
  adc delta
  sta search_ptr
  bcc loop

  inc search_ptr + 1

loop:
  lda (search_ptr), y
  cmp target, x
  bne done

  lda search_ptr
  clc
  adc delta
  sta search_ptr
  bcc :+

  inc search_ptr + 1

: inx
  cpx #SEARCH_LEN
  bcc loop

  jmp inc_total

done:
  rts
.endproc

.proc check_left
  ; pos_x >= search_len - 1 => continue
  lda pos_x
  cmp #SEARCH_LEN - 1
  bcs :+

  rts

: lda #1
  sta delta

  ldx #1

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  sec
  sbc delta
  sta search_ptr
  bcs loop

  dec search_ptr + 1

loop:
  lda (search_ptr), y
  cmp target, x
  bne done

  lda search_ptr
  sec
  sbc delta
  sta search_ptr
  bcs :+

  dec search_ptr + 1

: inx
  cpx #SEARCH_LEN
  bcc loop

  jmp inc_total

done:
  rts
.endproc

.proc check_upleft
  ; pos_x >= search_len - 1 => continue
  lda pos_x
  cmp #SEARCH_LEN - 1
  bcs :+

  rts

  ; pos_y >= search_len - 1 => continue
: lda pos_y
  cmp #SEARCH_LEN - 1
  bcs :+

  rts

: lda width
  clc
  adc #2 ; diagonal due to null terminator
  sta delta

  ldx #1

  lda input_ptr + 1
  sta search_ptr + 1

  lda input_ptr
  sec
  sbc delta
  sta search_ptr
  bcs loop

  dec search_ptr + 1

loop:
  lda (search_ptr), y
  cmp target, x
  bne done

  lda search_ptr
  sec
  sbc delta
  sta search_ptr
  bcs :+

  dec search_ptr + 1

: inx
  cpx #SEARCH_LEN
  bcc loop

  jmp inc_total

done:
  rts
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