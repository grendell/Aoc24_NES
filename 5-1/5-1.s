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
RULE_LEN = 6
BCD_LEN = 3

  rules_ptr: .res 2
  update_ptr: .res 2

  page: .res 1
  before: .res 1
  after: .res 1
  check: .res 1
  middle: .res 1

  length: .res 1
  scratch: .res 1

  total: .res 2
  bcd_total: .res BCD_LEN

.segment "RODATA"
input_start:
.include "../inputs/5.input"
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
  sta update_ptr
  lda #>input_start
  sta update_ptr + 1

  ldy #0

find_updates:
  lda (update_ptr), y
  beq found_blank

  lda update_ptr
  clc
  adc #RULE_LEN
  sta update_ptr
  bcc find_updates

  inc update_ptr + 1
  jmp find_updates

found_blank:
  inc update_ptr
  bne update_loop

  inc update_ptr + 1

update_loop:
  lda update_ptr + 1
  cmp #>input_end
  bcc :+

  lda update_ptr
  cmp #<input_end
  bcc :+

  rts

  ; skip the first entry
: ldy #2
  lda #1
  sta length

page_loop:
  lda (update_ptr), y
  beq found_update

  iny ; skip comma
  inc length
  lda (update_ptr), y
  and #$f
  jsr mul10
  sta page

  iny
  lda (update_ptr), y
  and #$f
  clc
  adc page
  sta page
  iny

  jmp enforce_rules

found_update:
  iny ; skip null terminator
  tya
  pha

  jsr add_middle_to_total

  pla
  clc
  adc update_ptr
  sta update_ptr
  bcc update_loop

  inc update_ptr + 1
  jmp update_loop

enforce_rules:
  tya
  pha

  lda #<input_start
  sta rules_ptr
  lda #>input_start
  sta rules_ptr + 1

rule_loop:
  ldy #0
  lda (rules_ptr), y
  beq progress_page

  and #$f
  jsr mul10
  sta before

  iny
  lda (rules_ptr), y
  and #$f
  clc
  adc before

  cmp page
  bne progress_rule

  sta before

  iny ; second digit
  iny ; pipe

  lda (rules_ptr), y
  and #$f
  jsr mul10
  sta after

  iny
  lda (rules_ptr), y
  and #$f
  clc
  adc after
  sta after

  ldy #0

check_loop:
  lda (update_ptr), y
  and #$f
  jsr mul10
  sta check

  iny
  lda (update_ptr), y
  and #$f
  clc
  adc check
  sta check

  cmp before
  beq progress_rule

  cmp after
  beq progress_update

  iny ; second digit
  iny ; comma
  jmp check_loop

progress_rule:
  lda rules_ptr
  clc
  adc #RULE_LEN
  sta rules_ptr
  bcc rule_loop

  inc rules_ptr + 1
  jmp rule_loop

progress_page:
  pla
  tay
  jmp page_loop

progress_update:
  pla
  tay

: lda (update_ptr), y
  beq next_update

  iny
  jmp :-

next_update:
  iny
  tya
  clc
  adc update_ptr
  sta update_ptr

  bcc :+

  inc update_ptr + 1

: jmp update_loop
.endproc

.proc mul10
  ; * 2
  asl
  sta scratch

  ; * 8
  asl
  asl

  clc
  adc scratch

  rts
.endproc

.proc add_middle_to_total
  ; offset = 3 * (length >> 1)
  lsr length
  clc
  lda length
  adc length
  adc length
  tay

  lda (update_ptr), y
  and #$f
  jsr mul10
  sta middle

  iny
  lda (update_ptr), y
  and #$f
  clc
  adc middle

  clc
  adc total
  sta total
  bcc :+

  inc total + 1

: rts
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

  ldy #16
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