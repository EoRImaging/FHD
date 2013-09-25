; docformat = 'rst'
;+
; An alternative to IDL's SORT function that employs a radix-sort algorithm
; (http://en.wikipedia.org/wiki/Radix_sort). This algorithm is stable but
; slower than the quicksort algorithm (http://en.wikipedia.org/wiki/Quicksort)
; used in IDL's SORT function.
;
; :author:
; 	Atle Borsholm, VIS, 2012
; 	MODIFIED IS 9/2013 to be compatible with idl 7 
;-

;+
; Handle special case for string arrays
;
; Note that this is very slow compared to "SORT" in most cases.
; It is however a stable sort. Calling "strmid" repeatedly is
; probably the biggest problem.
;-
function radix_sort_string_type, data, index=index
   compile_opt idl2, logical_predicate
   
   len=strlen(data)
   maxlen=max(len)
   sorted=data
   radix=256 ; fixed to byte for each character
   for i=maxlen-1,0,-1 do begin
      ; use a nice feature of IDL: byte('') = [0b]
      digit=byte(strmid(sorted,i,1))
      h=histogram(digit, reverse_indices=ri)
      sorted=sorted[ri[radix+1:*]]
      if n_elements(index) then index=index[ri[radix+1:*]]
   endfor
   return, sorted
end

;+
; Radix sort
;-
function radix_sort, data, radix=radix, index=index
   compile_opt idl2, logical_predicate
   
   ; default radix if not specified
   if ~keyword_set(radix) then radix=256
   radix=long64(radix)
   
   ; index output is requested
   if arg_present(index) then begin
      index=lindgen(n_elements(data))
   endif
   
   ; support signed ints
   code=size(data,/type)
   case code of
      2: sorted=uint(data) + ishft(1us, 15) ; +/- are equivalent here
      3: sorted=ulong(data) + ishft(1ul, 31)
      14: sorted=ulong64(data) + ishft(1ull, 63)
      5: begin
         sorted=ulong64(data,0,n_elements(data))
         ; IEEE does not use 2's complement for negative numbers
         ; so, flip the bits for the negative numbers with this trick
         ; this may not be 100% IEEE compliant sorting, but works
         ; well for finite numbers at least
         sorted xor= ((sorted and ishft(1ull,63)) ne 0) * (not ishft(1ull,63))
         sorted+=ishft(1ull, 63)
      end
      4: begin
         sorted=ulong(data,0,n_elements(data))
         sorted xor= ((sorted and ishft(1ul,31)) ne 0) * (not ishft(1ul,31))
         sorted+=ishft(1ul, 31)
      end
      7: return, radix_sort_string_type(data, index=index)
      else: sorted=data
   endcase
   
   mx=max(sorted, min=mn)
   
   ; implement a slight speed improvement for small data ranges
   rng=mx-mn
   if rng lt mn/radix then begin
      sorted-=mn
      mx-=mn
   endif
   
   factor=1ull
   while mx gt 0 do begin
      mx/=radix
      rem=sorted/factor
      digit=rem mod radix
      factor=factor*radix
      h=histogram(digit, min=0, max=radix-1, binsize=1, $
         reverse_indices=ri)
      sorted=sorted[ri[radix+1:*]]
      if arg_present(index) then index=index[ri[radix+1:*]]
   endwhile
   
   ; convert back slight optimization
   if rng lt mn/radix then begin
      sorted+=mn
   endif
   
   ; convert back in case of signed
   case code of
      2: sorted=fix(sorted - ishft(1us, 15)) ; could use + or -, same result
      3: sorted=long(sorted - ishft(1ul, 31))
      14: sorted=long64(sorted - ishft(1ull, 63))
      5: begin
         sorted-=ishft(1ull, 63)
         sorted xor= ((sorted and ishft(1ull,63)) ne 0) * (not ishft(1ull,63))
         sorted=double(sorted, 0, n_elements(data))
      end
      4: begin
         sorted-=ishft(1ul, 31)
         sorted xor= ((sorted and ishft(1ul,31)) ne 0) * (not ishft(1ul,31))
         sorted=float(sorted, 0, n_elements(data))
      end
      else:
   endcase
   
   return, sorted
end
