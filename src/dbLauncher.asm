; sverx's homebrew launcher 0.0.1 ---
; works (well, should!) with db_elec's homebrew cart

.memorymap
defaultslot 0
slotsize $4000 ; ROM
slot 0 $0000
slotsize $2000 ; RAM
slot 1 $c000
.endme

.rombankmap
bankstotal 1
banksize $4000
banks 1
.endro

.bank 0 slot 0
.org $0000                      ; this goes at ROM address 0 (boot)
.section "Startup" force
  ; (no mapper/interrupt mode initializazion needed here!)
  di                            ; disable interrupt
  ld sp,$dff0                   ; set stack pointer at end of RAM
  jp main                       ; run main
.ends

.org $0038
.section "Interrupt handler" force
  reti                          ; return from interrupt  (stub)
.ends

.org $0066
.section "Pause handler" force
  retn                          ; return from NMI (stub)
.ends

.section "bankshift table" free
; table where bankshift values for each game are stored
; the 1st game starts at 1
; the 2nd after the 1st and so on...
bankshift_table:
  ; ******** fill in the correct values! ********
  .db  1,5,9,13
.ends

.section "main" free
main:
  ; copy the launcher code to RAM
  ld hl,launcher
  ld de,launcher_RAM
  ld bc,launcher_end-launcher
  ldir

loop:
  in a,($dc)           ; read pad
  cpl                  ; A = ~A

  cp $01               ; UP=first game selected
  jr nz,+
  ld hl,bankshift_table
  ld a,(hl)
  jp launcher_RAM

+:cp $02               ; DOWN=second game selected
  jr nz,+
  ld hl,bankshift_table+1
  ld a,(hl)
  jp launcher_RAM

+:cp $04               ; LEFT=third game selected
  jr nz,+
  ld hl,bankshift_table+2
  ld a,(hl)
  jp launcher_RAM

+:cp $08               ; RIGHT=fourth game selected
  jr nz,+
  ld hl,bankshift_table+3
  ld a,(hl)
  jp launcher_RAM

+:jp loop
.ends

.section "launcher" free
; this is the code that launch the homebrew (needs to be copied to RAM!)
; (input) A = bankshift value to set
launcher:
  ; write 'magic sequence' to activate bank shifting
  ; ($40,$C1,$82 - also maps banks 0,1,2)
  ld de,$FFFD
  ld hl,launcher_RAM+(magic_sequence-launcher)
  ld bc,$0003
  ldir

  ; calculate register value from bank number
  ld b,a        ; save bank number
  .rept 3       ; A SHL 3
    rlca
  .endr
  and $60       ; keep bits 6,5
  or b          ; B is less or equal to $0F
  and $63       ; mask everything but bits 6,5,1,0

  ; write value to bankshifting register
  ld hl,$FFFC
  ld (hl),a

  jp $0000       ; start game!  (back to ROM 0 "bankshifted")

magic_sequence:
  .db $40,$C1,$82
launcher_end:
.ends

.ramsection "RAM" slot 1
  ; launcher_RAM         dsb launcher_end-launcher   ;  here's where the lancher copy goes
  ;                      can't make this here work :|
  launcher_RAM         dsb 256   ;  here's where the lancher copy goes
  ;                      256 bytes are surely enough :)
.ends
