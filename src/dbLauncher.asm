; sverx's homebrew launcher
; works (...well, let's see!) with db_elec's homebrew cart

; memory map / rom banks / etc
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

; you shouldn't modify anything above this line, theoretically...
; *********************************************************************************

.define NUMBER_OF_GAMES                2     ; change this if needed!

.section "cover assets" free
cover_assets:
  ; ONE line for EACH game - each line has 4 pointers: tiles(*), tilemap, palette, UNUSED
  ; (*) tiles are PSGaiden compressed
  .dw game1_tiles, game1_tilemap, game1_palette, $0000
  .dw game2_tiles, game2_tilemap, game2_palette, $0000

  ; put the assets wherever you want ;)
  game1_tiles:
  .incbin "inc/game1 (tiles).psgcompr"
  game1_tilemap:
  .incbin "inc/game1 (tilemap).bin"
  game1_palette:
  .incbin "inc/game1 (palette).bin"

  game2_tiles:
  .incbin "inc/game2 (tiles).psgcompr"
  game2_tilemap:
  .incbin "inc/game2 (tilemap).bin"
  game2_palette:
  .incbin "inc/game2 (palette).bin"
.ends

.section "bankshift table" free
; table where bankshift values for each game are stored
; the 1st game starts at 1 (well, as long as this 'boot' ROM is 16KB only)
; the 2nd after the 1st and so on...
bankshift_table:
  .db  1,9,17,25            ; ******** fill in the correct values HERE! ********
.ends

; modify SDSCTAG with the details of your collection! :)
.SDSCTAG 0.15, "dbLauncher","Boot ROM to launch homebrew ROMs in db-elec's SMS cartridge","2014/SVERX"
; since 2014-10-16 WLA-DX it's working properly even with 16KB ROMs,
; adding SDSC header (homebrew) and SEGA ROM header ("TMR SEGA"
; signature, ROM checksum and correct ROMtype/ROMsize byte)


; *********************************************************************************
; you shouldn't modify anything below this line, theoretically...

.org $0000                      ; this goes at ROM address 0 (boot)
.section "Startup" force
  di                            ; disable interrupt
  im 1                          ; interrupt mode 1 (this won't change)
  ld sp, $dff0                  ; set stack pointer at end of RAM
  xor a
  ld ($FFFD),a                  ; set BANK 0 to $00 - enough for 16KB
  jp main                       ; run main
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

.include "base.inc"
.include "vdp.inc"
.include "PSGaiden_tile_decomp.inc"

.macro SetVDPAddress_SAFE args addr
  ; - interrupt safe (DI/EI around setting VRAM address)
  ld a,<addr
  di
  out (VDPControlPort),a
  ld a,>addr
  out (VDPControlPort),a
  ei
.endm

; ******** fixed ASSETS ********
.section "assets" free
  headfoot_tiles:
  .incbin "inc/headfoot (tiles).psgcompr"
  headfoot_tilemap:
  .include "inc/headfoot (tilemap).inc"
  headfoot_palette:
  .incbin "inc/headfoot (palette).bin"
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

.section "ZeroBGPalette_SAFE" free
ZeroBGPalette_SAFE:
  ; zero out BG palette (VRAM safe)
  SetVDPAddress_SAFE BackgroundPaletteAddress
  ld c,VDPDataPort
  xor a
  ld b,16
-:out (c),a              ; 12
  nop                    ;  4
  djnz -                 ; 13
  ret                    ;   = 29 = SAFE
.ends

.section "WriteBGPalette_SAFE" free
WriteBGPalette_SAFE:
  ; writes provided (hl) to the BG palette (VRAM safe)
  SetVDPAddress_SAFE BackgroundPaletteAddress
  ld c,VDPDataPort
  ld b,16
-:outi                    ; 16
  jr nz,-                 ; 12 = 28 = SAFE
  ret
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

.macro OTIR_SAFE
-:outi                   ; 16                 16
  jr nz,-                ; 12 = 28 = SAFE
                         ;                     7
  nop                    ;                     4 = 27 = SAFE
.endm

.section "setcover" free
  ; sets the selected game cover, using "WhichGame"
setcover:
  call ZeroBGPalette_SAFE

  ld hl,cover_assets             ; tiles of game
  ld a,(WhichGame)
  ASLA 3
  ADDHLA
  ld a,(hl)
  inc hl
  ld h,(hl)
  ld l,a
  ld de,$0000|$4000              ; from VRAM $0000
  call PSGaiden_tile_decompr

;  call waitForVBlank       ; wait until VBlank starts

  SetVDPAddress_SAFE $3800+32*2|$4000 ; second line from top
  ld hl,cover_assets+2          ; tilemap of game
  ld a,(WhichGame)
  ASLA 3
  ADDHLA
  ld a,(hl)
  inc hl
  ld h,(hl)
  ld l,a
  ld c,VDPDataPort

                                 ; write tilemap
  ld b,0                         ; 0 = 256 bytes
  OTIR_SAFE
  OTIR_SAFE
  OTIR_SAFE
  OTIR_SAFE
  OTIR_SAFE
  ld b,256-32*2*2
  OTIR_SAFE

  ld hl,cover_assets+4           ; palette of game
  ld a,(WhichGame)
  ASLA 3
  ADDHLA
  ld a,(hl)
  inc hl
  ld h,(hl)
  ld l,a
  jp WriteBGPalette_SAFE   ; TO DO      ; tail call optimizazion
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
  ld hl,headfoot_tiles
  ld de,$3000|$4000              ; from VRAM $3000          /* FIX ME */
  call PSGaiden_tile_decompr
  
  SetVDPAddress_SAFE $3800|$4000      ; top of screen
  ld hl,headfoot_tilemap
  ld c,VDPDataPort
  ld b,32*2
  otir                           ; copy 64 bytes (one row)

  SetVDPAddress_SAFE $3800+23*32*2|$4000   ; bottom of screen
  ld hl,headfoot_tilemap+32*2
  ld c,VDPDataPort
  ld b,32*2
  otir                           ; copy 64 bytes (one row)
  
  ld hl,headfoot_palette
  call WriteSpritePalette
  
  ; set sprite info (no sprites please!)
  SetVDPAddress_SAFE $3F00|$4000
  ld c,VDPDataPort
  ld a,$D0
  out (c),a
  
  call turnOnVideo
  ei

  xor a
  ld (WhichGame),a         ; selected game = 0

  call setcover

_loop:
  call waitForVBlank       ; wait until VBlank starts
  
  in a,($dc)           ; read pad
  cpl                  ; A = ~A
  ld b,a               ; B = current pad status
  ld hl,PadStatus
  ld a,(hl)            ; A = previous pad status
  cpl                  ; A = ~A
  and b                ; A = current pad status AND NOT PadStatus (so ones are only where it was 0 before)
  ld (hl),b            ; save new pad status

+:cp $04               ; LEFT=decrease game selected (wrapping)
  jr nz,+
  ld a,(WhichGame)
  sub 1                ; dec a doesn't set carry...
  jp nc,_nowrapleft
  ld a,NUMBER_OF_GAMES-1   ; select LAST game
_nowrapleft:
  ld (WhichGame),a
  call setcover
  jp _loop

+:cp $08               ; RIGHT=increase game selected (wrapping)
  jr nz,+
  ld a,(WhichGame)
  inc a
  cp NUMBER_OF_GAMES
  jr nz,_nowraprigth
  xor a                ; select FIRST game
_nowraprigth:
  ld (WhichGame),a
  call setcover
  jp _loop

+:cp $10               ; START!
  jr nz,+
  call turnOffVideo
  ld a,(WhichGame)
  ld hl,bankshift_table
  ADDHLA
  ld a,(hl)
  jp launcher_RAM

+:jp _loop
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
  PadStatus       db      ;  the pad status
  WhichGame       db      ;  which game
  launcher_RAM    dsb 256 ;  here's where the launcher copy goes
                          ;  256 bytes are surely enough :)
.ends
