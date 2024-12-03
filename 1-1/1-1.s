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
DISTANCE_LEN = INPUT_LEN + 4

  input_ptr: .res 2

  next_left: .res INPUT_LEN
  left_remaining: .res 1
  next_right: .res INPUT_LEN
  right_remaining: .res 1
  last_left: .res INPUT_LEN
  last_right: .res INPUT_LEN

  padded_distance: .res DISTANCE_LEN - INPUT_LEN
  distance: .res INPUT_LEN
  total_distance: .res DISTANCE_LEN

.segment "RODATA"
input_start:
.include "1.input"
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
loop:
  jsr find_next
  lda next_left
  cmp #$ff
  beq done

  ldy #0
: lda next_left, y
  cmp next_right, y
  bcc right_minus_left
  bne left_minus_right

  iny
  cpy #INPUT_LEN
  bcc :-

left_minus_right:
  sec
  ldy #INPUT_LEN - 1
: lda next_left, y
  sbc next_right, y
  bcs @store

@borrow:
  adc #10
  clc

@store:
  sta distance, y

  dey
  bpl :-

  jmp add_to_total

right_minus_left:
  sec
  ldy #INPUT_LEN - 1
: lda next_right, y
  sbc next_left, y
  bcs @store

@borrow:
  adc #10
  clc

@store:
  sta distance, y

  dey
  bpl :-

  jmp add_to_total

add_to_total:
  clc
  ldy #DISTANCE_LEN - 1
: lda padded_distance, y
  adc total_distance, y
  cmp #10
  bcc @store

@carry:
  sbc #10
  sec

@store:
  sta total_distance, y

  dey
  bpl :-

  jmp loop

done:
  rts
.endproc

.proc find_next
find_left:
  lda left_remaining
  beq init_left_search

  dec left_remaining
  jmp find_right

init_left_search:
  ldy #INPUT_LEN - 1
: lda next_left, y
  sta last_left, y

  lda #$ff
  sta next_left, y

  dey
  bpl :-

  lda #<input_start
  sta input_ptr
  lda #>input_start
  sta input_ptr + 1

consider_left_1:
  ; if input < last, progress
  ldy #0
: lda (input_ptr), y
  cmp last_left, y
  bcc progress_left
  bne consider_left_2

  iny
  cpy #INPUT_LEN
  bcc :-

consider_left_2:
  ; if input == last, progress
  ldy #INPUT_LEN - 1
: lda (input_ptr), y
  cmp last_left, y
  bne consider_left_3

  dey
  bpl :-

  jmp progress_left

consider_left_3:
  ; if input < next, accept
  ldy #0
: lda (input_ptr), y
  cmp next_left, y
  bcc accept_left
  bne progress_left

  iny
  cpy #INPUT_LEN
  bcc :-

  jmp progress_left

accept_left:
  ldy #INPUT_LEN - 1
: lda (input_ptr), y
  sta next_left, y
  dey
  bpl :-

progress_left:
  lda input_ptr
  clc
  adc #INPUT_LEN + INPUT_SPACE + INPUT_LEN + 1
  sta input_ptr

  bcc check_left_end

  inc input_ptr + 1

check_left_end:
  lda input_ptr + 1
  cmp #>input_end
  bcc consider_left_1

  lda input_ptr
  cmp #<input_end
  bcc consider_left_1

  jsr count_next_left
  dec left_remaining

find_right:
  lda right_remaining
  beq init_right_search

  dec right_remaining
  rts

init_right_search:
  ldy #INPUT_LEN - 1
: lda next_right, y
  sta last_right, y

  lda #$ff
  sta next_right, y

  dey
  bpl :-

  lda #<(input_start + INPUT_RIGHT_OFFSET)
  sta input_ptr
  lda #>(input_start + INPUT_RIGHT_OFFSET)
  sta input_ptr + 1

consider_right_1:
  ; if input < last, progress
  ldy #0
: lda (input_ptr), y
  cmp last_right, y
  bcc progress_right
  bne consider_right_2

  iny
  cpy #INPUT_LEN
  bcc :-

consider_right_2:
  ; if input == last, progress
  ldy #INPUT_LEN - 1
: lda (input_ptr), y
  cmp last_right, y
  bne consider_right_3

  dey
  bpl :-

  jmp progress_right

consider_right_3:
  ; if input < next, accept
  ldy #0
: lda (input_ptr), y
  cmp next_right, y
  bcc accept_right
  bne progress_right

  iny
  cpy #INPUT_LEN
  bcc :-

  jmp progress_right

accept_right:
  ldy #INPUT_LEN - 1
: lda (input_ptr), y
  sta next_right, y
  dey
  bpl :-

progress_right:
  lda input_ptr
  clc
  adc #INPUT_LEN + INPUT_SPACE + INPUT_LEN + 1
  sta input_ptr

  bcc check_right_end

  inc input_ptr + 1

check_right_end:
  lda input_ptr + 1
  cmp #>input_end
  bcc consider_right_1

  lda input_ptr
  cmp #<input_end
  bcc consider_right_1

  jsr count_next_right
  dec right_remaining

  rts
.endproc

.proc count_next_left
  lda #0
  sta left_remaining

  lda #<input_start
  sta input_ptr
  lda #>input_start
  sta input_ptr + 1

consider_left:
  ldy #INPUT_LEN - 1
: lda (input_ptr), y
  cmp next_left, y
  bne progress_left
  dey
  bpl :-

  inc left_remaining

progress_left:
  lda input_ptr
  clc
  adc #INPUT_LEN + INPUT_SPACE + INPUT_LEN + 1
  sta input_ptr

  bcc check_left_end

  inc input_ptr + 1

check_left_end:
  lda input_ptr + 1
  cmp #>input_end
  bcc consider_left

  lda input_ptr
  cmp #<input_end
  bcc consider_left

  rts
.endproc

.proc count_next_right
  lda #0
  sta right_remaining

  lda #<(input_start + INPUT_RIGHT_OFFSET)
  sta input_ptr
  lda #>(input_start + INPUT_RIGHT_OFFSET)
  sta input_ptr + 1

consider_right:
  ldy #INPUT_LEN - 1
: lda (input_ptr), y
  cmp next_right, y
  bne progress_right
  dey
  bpl :-

  inc right_remaining

progress_right:
  lda input_ptr
  clc
  adc #INPUT_LEN + INPUT_SPACE + INPUT_LEN + 1
  sta input_ptr

  bcc check_right_end

  inc input_ptr + 1

check_right_end:
  lda input_ptr + 1
  cmp #>input_end
  bcc consider_right

  lda input_ptr
  cmp #<input_end
  bcc consider_right

  rts
.endproc

.proc present
  lda #$20
  sta PPU_ADDR
  lda #$21
  sta PPU_ADDR

  ldy #0
: lda total_distance, y
  bne non_zero

  iny
  cpy #DISTANCE_LEN
  bcc :-

zero:
  lda #$10
  sta PPU_DATA

  rts

non_zero:
: lda total_distance, y
  ora #$10
  sta PPU_DATA

  iny
  cpy #DISTANCE_LEN
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