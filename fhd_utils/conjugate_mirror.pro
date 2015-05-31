FUNCTION conjugate_mirror,image

type=size(image,/type) ;check if complex
IF type GE 6 THEN $
    conj_mirror_image=Conj(Shift(Reverse(Reverse(image,1),2),1,1)) $
ELSE conj_mirror_image=Shift(Reverse(Reverse(image,1),2),1,1)

RETURN,conj_mirror_image
END

