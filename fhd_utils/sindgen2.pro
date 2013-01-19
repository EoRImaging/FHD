FUNCTION sindgen2,d1,d2,d3,d4,d5,d6,d7,d8,characters=characters
;simple program to return a string array of indices with leading zeroes
ndim=N_params()
d_arr=fltarr(8)+1
SWITCH ndim OF
    8:d_arr[7]=d8
    7:d_arr[6]=d7
    6:d_arr[5]=d6
    5:d_arr[4]=d5
    4:d_arr[3]=d4
    3:d_arr[2]=d3
    2:d_arr[1]=d2
    1:d_arr[0]=d1
ENDSWITCH
ntot=product(d_arr)
characters_use=Ceil(Alog10(ntot))
IF Keyword_Set(characters) THEN characters_use=characters>characters_use
;format_string=Strcompress(String(format='("(I",I,")")',characters_use),/remove_all)
;result=String(format=format_string,lindgen(ntot))

result=strarr(ntot)
zero_format=Strcompress(String(format='("(",I,"(I1))")',characters_use),/remove)
result[0]=string(format=zero_format,fltarr(characters_use))
FOR i=1.,ntot-1 DO BEGIN
    char=Ceil(Alog10(i+1))>1.
    IF char LT characters_use THEN BEGIN
        zero_format=Strcompress(String(format='("(",I,"(I1))")',characters_use-char))
        result[i]=string(format=zero_format,fltarr(characters_use-char))+Strcompress(String(Floor(i)),/remove)
    ENDIF ELSE result[i]=Strcompress(String(Floor(i)),/remove)
ENDFOR

result=Reform(Reform(result,d_arr[0],d_arr[1],d_arr[2],d_arr[3],d_arr[4],d_arr[5],d_arr[6],d_arr[7]))
RETURN,result
END