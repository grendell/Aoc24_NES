.segment "HEADER"
; https://www.nesdev.org/wiki/INES
  .byte $4e, $45, $53, $1a ; iNES header identifier
  .byte $01                ; 1x 16KB PRG code
  .byte $01                ; 1x  8KB CHR data
  .byte $00                ; mapper 0 and horizontal mirroring
  .byte $00                ; mapper 0

.segment "VECTORS"
  .addr nmi, reset, 0

.segment "ZEROPAGE"
INPUT_LEN = 5
INPUT_LEFT_OFFSET = 0
INPUT_SPACE = 3
INPUT_RIGHT_OFFSET = INPUT_LEN + INPUT_SPACE
SCORE_LEN = 11 ; 1000 * 1000 * 99999

  left_ptr: .res 2
  right_ptr: .res 2

  padded_score: .res SCORE_LEN - INPUT_LEN
  score: .res INPUT_LEN
  total_score: .res SCORE_LEN

.segment "RODATA"
input_start:
.include "../inputs/1.input"
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
  sta left_ptr
  lda #>input_start
  sta left_ptr + 1

outer_loop:
  lda #<(input_start + INPUT_RIGHT_OFFSET)
  sta right_ptr
  lda #>(input_start + INPUT_RIGHT_OFFSET)
  sta right_ptr + 1

  ldy #INPUT_LEN - 1
: lda (left_ptr), y
  and #$f
  sta score, y

  dey
  bpl :-

inner_loop:
  ldy #INPUT_LEN - 1
: lda (left_ptr), y
  cmp (right_ptr), y
  bne progress_right

  dey
  bpl :-

add_to_total:
  clc
  ldy #SCORE_LEN - 1
: lda padded_score, y
  adc total_score, y
  cmp #10
  bcc @store

@carry:
  sbc #10
  sec

@store:
  sta total_score, y

  dey
  bpl :-

progress_right:
  lda right_ptr
  clc
  adc #INPUT_LEN + INPUT_SPACE + INPUT_LEN + 1
  sta right_ptr

  bcc check_right_end

  inc right_ptr + 1

check_right_end:
  lda right_ptr + 1
  cmp #>input_end
  bcc inner_loop

  lda right_ptr
  cmp #<input_end
  bcc inner_loop

progress_left:
  lda left_ptr
  clc
  adc #INPUT_LEN + INPUT_SPACE + INPUT_LEN + 1
  sta left_ptr

  bcc check_left_end

  inc left_ptr + 1

check_left_end:
  lda left_ptr + 1
  cmp #>input_end
  bcc outer_loop

  lda left_ptr
  cmp #<input_end
  bcc outer_loop

  rts
.endproc

.proc present
  lda #$20
  sta PPU_ADDR
  lda #$21
  sta PPU_ADDR

  ldy #0
: lda total_score, y
  bne non_zero

  iny
  cpy #SCORE_LEN
  bcc :-

zero:
  lda #$10
  sta PPU_DATA

  rts

non_zero:
: lda total_score, y
  ora #$10
  sta PPU_DATA

  iny
  cpy #SCORE_LEN
  bcc :-

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