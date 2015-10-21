pro rmsynthesis, stokes_q, $
                 stokes_u, $
                 lambdasq, $
                 phi, $
                 WEIGHT=weight_in, $
                 PROGRESS=progress, $
                 FORLOOPS=forloops, $
                 DOUBLE=double, $
                 fdf_cube
;+
; NAME:
;       RMSYNTHESIS
;
; PURPOSE:
;       Perform rotation measure synthesis on linearly polarized
;       Stokes paramaters.
;
; CALLING SEQUENCE:
;       RMSYNTHESIS, stokes_q, stokes_u, lambdasq, phi, fdf_cube [,
;                    WEIGHT=vector][, /PROGRESS]
;
; INPUTS:
;       STOKES_Q - the Stokes Q data; a vector or 2D or 3D array of
;                  floating point values in spectral order (spectral
;                  dimension comes first).
;
;       STOKES_U - the Stokes U data;  a vector or 2D or 3D array of
;                  floating point values in spectral order (spectral
;                  dimension comes first).  Must have the same dimensions
;                  and size as STOKES_Q.
;
;       LAMBDASQ - the lambda-squared sampling of the Stokes measurements;
;                  a floating point vector in units of inverse meters
;                  squared; must have same length as the first dimension of
;                  the Stokes cubes.
;
;       PHI - the Faraday depth; a floating point vector in units of
;             radians per meter-squared.
;
; KEYWORD PARAMETERS:
;       WEIGHT = the weight function (a.k.a. the sampling function) for the
;                complex polarized surface brightness; floating point vector
;                that must have the same length as the LAMBDASQ array.  If
;                not set, a uniform weighting is assumed.
;
;       /DOUBLE - by default, FDF_CUBE is returned as a single-precision
;                 complex array; set this keyword to force it to be
;                 returned in double-precision.  If your input STOKES_Q and
;                 STOKES_U are large, you might not have enough memory to
;                 allocate a double-precision complex FDF_CUBE.
;
;       /PROGRESS - print the progress of the synthesis to the terminal.
;
;       /FORLOOPS - use for loops instead of vectorization; the code uses
;                   vectorization by default and will provide a performance
;                   advantage for NLAMBDA<~1000.  If NLAMBDA>1000 then you
;                   should set /FORLOOPS.  There should be a way to
;                   automatically estimate when we should use one scheme
;                   and not the other, but we haven't gone this far yet.
;
; OUTPUTS:
;       FDF_CUBE - the Faraday dispersion function; a single-precision
;                  complex array.  If the inputs STOKES_Q cube is of size
;                  [NFREQ,NX,NY] and the input PHI vector has length NPHI,
;                  then FDF_CUBE has the dimensions [NPHI,NX,NY].
;
; RESTRICTIONS:
;       All arithmetic is done in double precision.  The Faraday
;       dispersion function is returned as a single-precision complex
;       cube. As such, there is a limit to how large each of the input
;       cubes can be based on your system's physical memory. ELABORATE.
;
; PROCEDURE:
;       Algorithm from Brentjens & de Bruyn 2005, A&A, 441, 1217.
;
; NOTES:
;       Assumes that data that are blanked are flagged with IEEE NaN.
;
;       If masking is not uniform, i.e., if 3 channels are flagged in one
;       spectrum, but 5 in another, then this routine needs to be rewritten
;       to deal with a weight cube.
;
;       Assumes that if a whole frequency plane is masked out, that
;       it hasn't been passed in (that it's been removed from the cube
;       altogether) before being passed into this routine.
;
; BENCHMARKS:
;       Vectorization offers a performance advantage over using for loops
;       up to some product of NPHI*NLAMBDA ~ 2 million.  We need to do more
;       benchmark tests, but here are some numbers...
;
;           NX    NY NLAMBDA  NPHI     FOR-LOOPS    VECTORIZED
;            1     1      10  1001        0.0025        0.0016
;            1     1     100  1001        0.0145        0.0086
;            1     1    1000  1001        0.1335        0.1007
;            1     1    2048  1001        0.2726        0.2220
;            1     1   32768  1001        4.3525        4.5573
;
;          100    10     100  1001       15.1596        1.6133
;
;       In the future, when we have spectrometers like CABB that have >2048
;       channels, we need to switch to for loops because vectorization
;       will give a performance penalty.
;
;       N.B. - the number for my mac are quite different...
;           NX    NY NLAMBDA  NPHI     FOR-LOOPS    VECTORIZED
;            1     1      10  1001        0.0024        0.0022
;            1     1     100  1001        0.0119        0.0179
;            1     1    1000  1001        0.1305        0.1715
;            1     1    2048  1001        0.2430        0.3298
;
;           10    10     100  1001        1.0486        0.8038
;
; EXAMPLE:
;       Your telescope gives you a Stokes Q cube and Stokes U cube, most
;       likely arranged in "image order" [longitude,latitude,frequency].
;       RM Synthesis works in the spectral dimension, so we transpose the
;       cubes to "spectral order" [frequency,longitude,latitude]:
;
;       stokes_q = transpose(stokes_q,[2,0,1])
;       stokes_u = transpose(stokes_u,[2,0,1])
;
;       Now we're ready to run RMSYNTHESIS...
;
;       rmsynthesis, stokes_q, stokes_u, lambdasq, phi, fdf_cube
;
;       The Faraday dispersion function cube returned from RMSYNTHESIS is
;       in spectral order.  If you want to save it somewhere, you're
;       probably going to want back in image order, so transpose it
;       while you're transposing the Stokes Q and U cubes back to
;       image order:
;
;       stokes_q = transpose(stokes_q,[1,2,0])
;       stokes_u = transpose(stokes_u,[1,2,0])
;       fdf_cube = transpose(fdf_cube,[1,2,0])
;
; MODIFICATION HISTORY:
;	Written by Tim Robishaw, USyd  08 Jun 2010
;-

icomp = dcomplex(0,1)

; GET NUMBER OF FREQUENCY CHANNELS...
nlambdasq = N_elements(lambdasq)

; FIND POSITIONS THAT HAVE BEEN MASKED OUT IN THE STOKES Q OR U CUBES...
;mask = finite(stokes_q) AND finite(stokes_u)
;blank = total(mask,1) eq 0.0

; CHECK FOR EXISTENCE OF WEIGHT KEYWORD...
nweight = N_elements(weight_in)
if (nweight eq 0) then begin
   weight = dblarr(nlambdasq)+1.0
   ;weight = double(mask)
endif else begin
   weight = weight_in
   ; MAKE SURE LAMBDASQ AND WEIGHT ARRAYS ARE THE SAME SIZE...
   if (nweight ne nlambdasq) $
      then message, 'LAMBDASQ and WEIGHT vectors must have the same size.'
endelse

; GET THE SIZE OF THE INPUT STOKES CUBES...
szq = size(stokes_q)
szu = size(stokes_u)

; SANITY CHECK THAT Q AND U ARE SAME SIZE...
if (array_equal(szq[1:szq[0]],szu[1:szu[0]]) eq 0) then $
   message, "Stokes Q and U arrays MUST be the same size."

; NOW MAKE SURE THEY'RE IN SPECTRAL ORDER...
if (szq[1] ne nlambdasq) then $
   message, "Stokes Q and U arrays MUST be in spectral order "+$
            "with frequency axis in first dimension."

; HOW DO WE HANDLE INPUT ARRAYS OF VARIOUS DIMENSIONS...
case szq[0] of
   ; GIVE US A 1D INPUT AND WE GIVE BACK A [1,1,NPHI] ARRAY...
   1 : begin
      nx = 1
      ny = 1
   end
   ; GIVE US A 2D INPUT AND WE GIVE BACK A [1,NY,NPHI] ARRAY...
   2 : begin
      nx = 1
      ny = szq[2]
   end
   ; GIVE US A 3D INPUT AND WE GIVE BACK A [NX,NY,NPHI] ARRAY...   
   3 : begin
      nx = szq[2]
      ny = szq[3]
   end
   ; WE DON'T WANT >3D INPUTS...
   else : message, "Don't know what to do with a "+strtrim(szq[0],2)+$
                   "D Stokes array."
endcase

; HAVE WE ASKED TO DO THE MATH IN DOUBLE PRECISION...
double = keyword_set(DOUBLE)

; HOW MANY FARADAY DEPTHS DO WE HAVE...
nphi = N_elements(phi)

; INITIALIZE THE FARADAY DISPERSION FUNCTION CUBE...
fdf_cube = double $
           ? dcomplexarr(nphi,nx,ny) $
           : complexarr(nphi,nx,ny)

; BdB EQUATIONS (24) AND (38) GIVE THE INVERSE SUM OF THE WEIGHTS...
K = 1d0 / total(weight,DOUBLE=double)

; GET THE MEAN OF THE LAMBDA-SQUARED DISTRIBUTION...
; THIS IS EQUATION (32) OF BdB05...
lambda0sq = K * total(weight * lambdasq,DOUBLE=double)

if keyword_set(FORLOOPS) then goto, for_loops

; MINIMIZE THE NUMBER OF INNER-LOOP OPERATIONS BY DEFINING THE ARGUMENT OF
; THE EXPONENTIAL TERM IN BdB05 EQUATIONS (25) AND (36) FOR THE FARADAY
; DISPERSION FUNCTION...
arg = exp(-2d0 * icomp * phi ## (lambdasq - lambda0sq))

;^^^ AT SOME POINT, MAKING AN [NPHI,NLAMBDASQ] ARRAY IS GOING TO CAUSE A
; PERFORMANCE PENALTY (ESPECIALLY SINCE WE DO IT AGAIN ***INSIDE*** THE FOR
; LOOP) WHEN COMPARED TO THE FOR LOOP METHOD...

; DO THE SYNTHESIS AT EACH PIXEL IN THE IMAGE...
for j = 0, ny-1 do begin
   for i = 0, nx-1 do begin

      ; DEFINE THE OBSERVED POLARIZED SURFACE BRIGHTNESS...
      ; BdB05 EQUATIONS (8) AND (14)...
      p = (weight * dcomplex(stokes_q[*,i,j],stokes_u[*,i,j])) # rebin([1.0],nphi)
      
      ; CALCULATE THE FARADAY DISPERSION FUNCTION...
      ; BdB05 EQUATIONS (25) AND (36)...
      ; WEIGHT BY K AFTER THE LOOP TO MINIMIZE IN-LOOP OPERATIONS...
      ; ^^^ that makes NO sense
      ;fdf_cube[0,i,j] = total(p * arg, 1, /NAN, DOUBLE=double)
      fdf_cube[0,i,j] = K * total(p * arg, 1, /NAN, DOUBLE=double)

      ; FREE MEMORY USED TO STORE P...
      ; IT'S FASTER TO DO THIS THAN TO USE A TEMPORARY() CALL ABOVE...
      p = 0b

   endfor

   ; GIVE US A PROGRESS REPORT IF WE'VE ASKED FOR ONE...
   if keyword_set(PROGRESS) then $
      print, 100*float(j)/(ny-1), format='($,"Progress: ",I4,"%",%"\R")'
endfor

return

;=================================================================
; HERE WE AVOID VECTORIZING AND JUST USE A FOR LOOP...

for_loops:

; MINIMIZE THE NUMBER OF INNER-LOOP OPERATIONS BY DEFINING THE ARGUMENTS OF
; THE FOURIER TRANSFORM BEFORE WE ENTER THE LOOP...
arg = -2d0 * icomp * (lambdasq - lambda0sq)

; DO THE SYNTHESIS AT EACH PIXEL IN THE IMAGE...
for j = 0, ny-1 do begin
   for i = 0, nx-1 do begin

      ; DEFINE THE WEIGHTED COMPLEX POLARIZED INTENSITY...
      ; BdB05 EQUATION (14)....
      wp = weight * dcomplex(stokes_q[*,i,j],stokes_u[*,i,j])

      ; CALCULATE THE FARADAY DISPERSION FUNCTION...
      ; BdB05 EQUATIONS (25) AND (36)...
      ; WEIGHT BY K AFTER THE LOOP TO MINIMIZE IN-LOOP OPERATIONS...
      for p = 0, nphi-1 do $
         fdf_cube[p,i,j] = total(wp * exp(arg * phi[p]),/DOUBLE,/NAN) 

   endfor
   ; GIVE US A PROGRESS REPORT IF WE'VE ASKED FOR ONE...
   if keyword_set(PROGRESS) then $
      print, 100*float(j)/(ny-1), format='($,"Progress: ",I4,"%",%"\R")'
endfor

; WEIGHT THE FARADAY DISPERSION FUNCTION...
fdf_cube = K * temporary(fdf_cube)

end ; rmsynthesis
