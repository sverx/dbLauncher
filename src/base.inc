;
; very common code - should NOT require any prerequisites
;

.macro ASLA args shift
  ; this is ARITHMETIC SHIFT LEFT A by 'shift' bits using ADD A,A since it's more efficient than SLA A
  ; cycles taken = 4*shift
  ; affects A, Flags
  .rept shift
    add a,a
  .endr
.endm

.macro LSRA args shift
  ; this is LOGICAL SHIFT RIGHT A by 'shift' bits using RRCA, more efficient than using SRA for shift>1
  ; cycles taken = 4*(shift+1)
  ; affects A, Flags trashed
  .rept shift
    rrca
  .endr
  and $FF>>shift
.endm

.macro ASLHL args shift
  ; this is ARITHMETIC SHIFT LEFT HL by 'shift' bits using ADD HL,HL
  ; cycles taken = 11*shift
  ; affects HL, Flags unaffected
  .rept shift
    add hl,hl
  .endr
.endm

.macro RRA args shift
  ; this is ROTATE RIGHT A by 'shift' bits
  ; cycles taken = 4*shift
  ; affects A, Flags
  .rept shift
    rrca
  .endr
.endm

.macro ADDHLA
  ; this is HL=HL+A, since ADD HL,A doesn't exist...
  ; cycles taken = 20
  ; affects HL and A, Flags trashed
  add a,l
  ld l,a
  adc a,h
  sub l
  ld h,a
.endm

.section "Div and Mod" free
; courtesy of http://www.smspower.org/Development/DivMod
; Integer divides D by E
; Result in D, remainder in A
; Clobbers F, B
DivMod:
  xor a
  ld b,8
-:sla d
  rla
  cp e
  jr c,+
  sub e
  inc d
+:djnz -
  ret
.ends

;  =====================
;  macro FastOtir - from Maxim's tutorial/examples
;  =====================
.define FastOtirBlockSize 256

.macro FastOtir args count
.ifgr count FastOtirBlockSize
  call outiblock
  FastOtir count-FastOtirBlockSize
.else
  call outiblock+(FastOtirBlockSize-count)*2
.endif
.endm

.section "OUTI block" free
outiblock:
.rept FastOtirBlockSize
  outi
.endr
  ret
.ends
