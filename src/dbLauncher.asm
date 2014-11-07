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
  di                            ; disable interrupt
  im 1                          ; interrupt mode 1 (this won't change)
  ld sp, $dff0                  ; set stack pointer at end of RAM
  jp main                       ; run main
                                ; (no mapper mode initializazion needed here!)
.ends

.org $0038
.section "Interrupt handler" force
  push af
    in a,(VDPStatusPort)        ; read port to satisfy interrupt
    ld a,$01                    ; the only interrupt enabled is VBlank so...
    ld (VBlankFlag),a           ;   ... write down that it actually happened
  pop af
  ei                            ; enable interrupt (that were disabled by the IRQ call)
  reti                          ; return from interrupt
.ends

.org $0066
.section "Pause handler" force  ; no PAUSE support in the launcher
  retn                          ; return from NMI (stub)
.ends

.SDSCTAG 0.11, "dbLauncher","Boot ROM to launch homebrew ROMs in db-elec's SMS cartridge","2014/SVERX"
; since 2014-10-16 WLA-DX it's working properly even with 16KB ROMs,
; adding SDSC header (homebrew) and SEGA ROM header ("TMR SEGA"
; signature, ROM checksum and correct ROMtype/ROMsize byte)

.include "base.inc"
.include "vdp.inc"
.include "PSGaiden_tile_decomp.inc"

; ******** ASSETS ********
.section "assets" free
multicart_tiles:
.incbin "inc/multicart (tiles).psgcompr"
multicart_tilemap:
.incbin "inc/multicart (tilemap).bin"
multicart_palette:
.incbin "inc/multicart (palette).bin"
arrow_tiles:
.incbin "inc/arrow (tiles).psgcompr"
arrow_palette:
.incbin "inc/arrow (palette).bin"
.ends


.section "bankshift table" free
; table where bankshift values for each game are stored
; the 1st game starts at 1 (well, as long as this 'boot' ROM is 16KB only)
; the 2nd after the 1st and so on...
bankshift_table:
  ; ******** fill in the correct values HERE! ********
  .db  1,9,17,25
.ends

.section "turnOnVideo" free
turnOnVideo:
  ; enable sprites 8x16, turn on video
  SETVDPREGSANDRET VDPReg1_Mode|Reg1_BigSprites|Reg1_Show|Reg1_FrameInt
.ends

.section "turnOffVideo" free
turnOffVideo:
  ; disable sprites 8x16, turn off video
  SETVDPREGSANDRET VDPReg1_Mode|$00
.ends

.section "waitForVBlank" free
waitForVBlank:
  xor a
  ld (VBlankFlag),a     ; reset VBlank - we are waiting for the next one
-:ld a,(VBlankFlag)
  or a
  jr z,-
  xor a
  ld (VBlankFlag),a
  ret
.ends

.section "main" free
main:
  ; copy the launcher code to RAM
  ld hl,launcher
  ld de,launcher_RAM
  ld bc,launcher_end-launcher
  ldir
  
  ; set VDP to default Mode4 values
  call SetMode4Default
  
  ; load assets (tiles/tilemap/palette) in VRAM
  ld hl,multicart_tiles
  ld de,$0000|$4000              ; from VRAM $0000
  call PSGaiden_tile_decompr

  ld hl,arrow_tiles
  ld de,$2000|$4000              ; from VRAM $2000 (sprites)
  call PSGaiden_tile_decompr

  SetVDPAddress $3800|$4000
  ld hl,multicart_tilemap
  ld c,VDPDataPort
  ld b,0                         ; 0 = 256 bytes
  .rept 6                        ; 6 times 256 bytes = 32*24*2 = whole map
    otir
  .endr
  
  ld hl,arrow_palette
  call WriteSpritePalette

  ld hl,multicart_palette
  call WriteSpritePalette_zerocolor

  ld hl,multicart_palette
  call WriteBGPalette
  
  ; set sprite info
  SetVDPAddress $3F00|$4000
  ld c,VDPDataPort
  ld a,110                 ; sprite Y
  out (c),a
  out (c),a
  ld a,$D0                 ; no more sprites
  out (c),a

  SetVDPAddress $3F00+128|$4000
  ld c,VDPDataPort
  ld a,128-8               ; sprite X
  out (c),a
  ld a,0                   ; tile 0
  out (c),a
  ld a,128-8+8             ; sprite X
  out (c),a
  ld a,2                   ; tile 0
  out (c),a

  call turnOnVideo
  ei
  
  xor a
  ld (WhichGame),a         ; selected game = 0
  
loop:
  call waitForVBlank       ; wait until VBlank starts

  ; update sprite
  SetVDPAddress $3F00+128+1|$4000
  ld c,VDPDataPort
  ld a,(WhichGame)
  ASLA 2
  out (c),a

  SetVDPAddress $3F00+128+3|$4000
  ld c,VDPDataPort
  ld a,(WhichGame)
  ASLA 2
  inc a
  inc a
  out (c),a

  in a,($dc)           ; read pad
  cpl                  ; A = ~A

  cp $01               ; UP=first game selected
  jr nz,+
  ld a,0
  ld (WhichGame),a

+:cp $02               ; DOWN=second game selected
  jr nz,+
  ld a,1
  ld (WhichGame),a

+:cp $04               ; LEFT=third game selected
  jr nz,+
  ld a,2
  ld (WhichGame),a

+:cp $08               ; RIGHT=fourth game selected
  jr nz,+
  ld a,3
  ld (WhichGame),a
  
+:cp $10               ; START!
  jr nz,+
  call turnOffVideo
  ld a,(WhichGame)
  ld hl,bankshift_table
  ADDHLA
  ld a,(hl)
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
  VBlankFlag      db      ;  the flag for VBlank
  WhichGame       db      ;  which game
  launcher_RAM    dsb 256 ;  here's where the launcher copy goes
                          ;  256 bytes are surely enough :)
.ends
