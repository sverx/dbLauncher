; sverx's homebrew launcher
; works (well, should!) with db_elec's homebrew cart

.memorymap
defaultslot 0
slotsize $4000 ; ROM (16 KB)
slot 0 $0000
slotsize $2000 ; RAM (8 KB)
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
  reti                          ; return from interrupt (stub)
.ends

.org $0066
.section "Pause handler" force
  retn                          ; return from NMI (stub)
.ends

.SDSCTAG 0.01, "dbLauncher","Boot ROM to launch homebrew ROMs in db-elec's SMS cartridge","2014/SVERX"
; since 2014-10-16 it's working properly even with 16KB ROMs,
; adding SDSC header (homebrew) and SEGA ROM header ("TMR SEGA"
; signature, ROM checksum and correct ROMtype/ROMsize byte)

.section "bankshift table" free
; table where bankshift values for each game are stored
; the 1st game starts at 1 (well, as long as this 'boot' ROM is 16KB only)
; the 2nd after the 1st and so on...
bankshift_table:
  ; ******** fill in the correct values HERE! ********
  .db  1,9,17,25
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
  ld a,(bankshift_table)
  jp launcher_RAM

+:cp $02               ; DOWN=second game selected
  jr nz,+
  ld a,(bankshift_table+1)
  jp launcher_RAM

+:cp $04               ; LEFT=third game selected
  jr nz,+
  ld a,(bankshift_table+2)
  jp launcher_RAM

+:cp $08               ; RIGHT=fourth game selected
  jr nz,+
  ld a,(bankshift_table+3)
  jp launcher_RAM

+:jp loop
.ends

.section "launcher" free
; this is the code that launch the homebrew (needs to be executed from RAM!)
; (input) A = bankshift value to set
launcher:
  ; write 'magic sequence' to activate bank shifting
  ; ($40,$C1,$82 - also maps banks 0,1,2)
  ld de,$FFFD
  ld hl,launcher_RAM+(magic_sequence-launcher)
  ld bc,$0003
  ldir

  ; calculate register value from bank number
  or $C0        ; set the 7th and 8th bit

  ; write the value to the bankshifting register
  ld ($FFFC),a

  ; start game!  (back to ROM 0 "bankshifted")
  jp $0000

magic_sequence:
  .db $40,$C1,$82
launcher_end:
.ends

.ramsection "RAM" slot 1
  ; launcher_RAM         dsb launcher_end-launcher   ;  here's where the lancher copy goes
  ;                      can't make this here work :|
  launcher_RAM         dsb 256 ;  here's where the lancher copy goes
  ;                      256 bytes are surely enough :)
.ends
