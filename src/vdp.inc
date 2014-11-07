;
;  VDP - according to http://www.smspower.org/Development/VDPRegisters
;

;  VDP I/O ports
.define VDPVCounter         $7e ; Vcounter (r/o)
.define VDPDataPort         $be ; VDP Data Port (w/o)
.define VDPStatusPort       $bf ; VDP Status (r)
.define VDPControlPort      $bf ; VDP Control Port (w)

; VDPControlPort operational modes
.define readVRAM            $00 ; to read from VRAM
.define writeVRAM           $40 ; to write to VRAM
.define writeVDPregister    $80 ; to write to VDP register
.define writeCRAM           $c0 ; to write to CRAM

; the SMS VDP registers and the relative flags:
.define VDPReg0_Mode        (writeVDPregister|$00)<<8   ; the mode register (first part)
.define Reg0_DisableSync    $01                         ; disable sync
.define Reg0_Mode4ExtraHigh $02                         ; enable extra high mode (SMS2)
.define Reg0_Mode4          $04                         ; enable Mode4
.define Reg0_ShiftSprites   $08                         ; shift sprites left 8 pixels
.define Reg0_EnableLineInt  $10                         ; enables line interrupt
.define Reg0_MaskFirstCol   $20                         ; mask column 0 with overscan color
.define Reg0_DisableHScroll $40                         ; disable horizontal scrolling for rows 0-1
.define Reg0_DisableVScroll $80                         ; disable vertical scrolling for columns 24-31

.define VDPReg1_Mode        (writeVDPregister|$01)<<8   ; the mode register (second part)
.define Reg1_ZoomSprites    $01                         ; sprite pixels are doubled in size
.define Reg1_BigSprites     $02                         ; sprites are 16x16 (TMS9918) or 8x16 (Mode 4)
.define Reg1_240lines       $08                         ; selects 240 lines screen for Mode 4 (SMS2)
.define Reg1_224lines       $10                         ; selects 224 lines screen for Mode 4 (SMS2)
.define Reg1_FrameInt       $20                         ; frame interrupt enable
.define Reg1_Show           $40                         ; if not set, display will blank

.define VDPReg2_NameTabAddr (writeVDPregister|$02)<<8   ; Name Table Address
; valid values for 256x192
.define Reg2_Addr_0000      (($0000>>10)|%11110001)
.define Reg2_Addr_0800      (($0800>>10)|%11110001)
.define Reg2_Addr_1000      (($1000>>10)|%11110001)
.define Reg2_Addr_1800      (($1800>>10)|%11110001)
.define Reg2_Addr_2000      (($2000>>10)|%11110001)
.define Reg2_Addr_2800      (($2800>>10)|%11110001)
.define Reg2_Addr_3000      (($3000>>10)|%11110001)
.define Reg2_Addr_3800      (($3800>>10)|%11110001)
; valid values for 256x224 (PAL/NTSC) and 256x240 (PAL only)
.define Reg2_Addr_0700      (($0700>>10)|%11110011)
.define Reg2_Addr_1700      (($1700>>10)|%11110011)
.define Reg2_Addr_2700      (($2700>>10)|%11110011)
.define Reg2_Addr_3700      (($3700>>10)|%11110011)

.define VDPReg3_ColTabAddr  (writeVDPregister|$03)<<8   ; Color Table Address [no effect]
.define VDPReg4_BGPatAddr   (writeVDPregister|$04)<<8   ; Background Pattern Address [no effect]
.define VDPReg5_SATAddr     (writeVDPregister|$05)<<8   ; Sprite Attribute Table Address

.define Reg5_Addr_0000      (($0000>>7)|%10000001)
; there are 64 possible options here in between...
.define Reg5_Addr_3f00      (($3f00>>7)|%10000001)

.define VDPReg6_SprPatAddr  (writeVDPregister|$06)<<8   ; Sprite Pattern Address
.define Reg6_Addr_0000      (($0000>>11)|%11111011)
.define Reg6_Addr_2000      (($2000>>11)|%11111011)

.define VDPReg7_BDCol       (writeVDPregister|$07)<<8   ; Overscan/Backdrop Color
.define VDPReg8_ScrollX     (writeVDPregister|$08)<<8   ; X Scroll (background)
.define VDPReg9_ScrollY     (writeVDPregister|$09)<<8   ; Y Scroll (background)
.define VDPRegA_LineCnt     (writeVDPregister|$0A)<<8   ; Line counter

.define BackgroundPaletteAddress  (writeCRAM<<8)|$00
.define SpritePaletteAddress      (writeCRAM<<8)|$10

/*
.macro SETVDPREGS
  ; TO DO - write comments here
  ld hl,_LocalDataBegin
  ld b,_LocalDataEnd-_LocalDataBegin
  ld c,VDPControlPort
  jr +
_LocalDataBegin:
  .rept 16
    .if NARGS != 0
      .dw \1
      .shift
    .endif
  .endr
_LocalDataEnd:
+:otir
.endm
*/

.macro SETVDPREGSANDRET
  ; write all the provided words to VDP registers
  ; it doesn't use DI/EI
  ld hl,_LocalDataBegin
  ld b,_LocalDataEnd-_LocalDataBegin
  ld c,VDPControlPort
;  di
  otir
;  ei
  ret
_LocalDataBegin:
  .rept 16
    .if NARGS != 0
      .dw \1
      .shift
    .endif
  .endr
_LocalDataEnd:
.endm

.macro SetVDPAddress args addr
  ; TO DO - write comments here
  ld a,<addr
  out (VDPControlPort),a
  ld a,>addr
  out (VDPControlPort),a
.endm

.section "SetMode4Default: Initialise VDP to Mode 4 with default values" free
  ; TO DO - write comments here
SetMode4Default:
  SETVDPREGSANDRET  VDPReg1_Mode|$00, VDPReg0_Mode|Reg0_Mode4, VDPReg2_NameTabAddr|Reg2_Addr_3800, VDPReg3_ColTabAddr|$FF, VDPReg4_BGPatAddr|$FF, VDPReg5_SATAddr|Reg5_Addr_3f00, VDPReg6_SprPatAddr|Reg6_Addr_2000, VDPReg7_BDCol|$00, VDPReg8_ScrollX|$00, VDPReg9_ScrollY|$00, VDPRegA_LineCnt|$FF
.ends

.section "WriteSpritePalette" free
WriteSpritePalette:
  ; writes provided (hl) to the sprite palette
  ; TO DO - write other comments here
  SetVDPAddress SpritePaletteAddress
  ld c,VDPDataPort
  FastOtir 16
  ret
.ends

.section "WriteSpritePalette zero color" free
WriteSpritePalette_zerocolor:
  ; writes provided (hl) to the sprite palette entry 0
  SetVDPAddress SpritePaletteAddress
  ld c,VDPDataPort
  outi
  ret
.ends

.section "WriteBGPalette" free
WriteBGPalette:
  ; writes provided (hl) to the BG palette
  ; TO DO - write other comments here
  SetVDPAddress BackgroundPaletteAddress
  ld c,VDPDataPort
  FastOtir 16
  ret
.ends
