FUNCTION size_type,data
;returns a string instead of a type code, otherwise same as =size(data,/type)

code=size(data,/type)
CASE code OF
    1:type_name='Byte'
    2:type_name='Integer'
    3:type_name='Long Integer'
    4:type_name='Float'
    5:type_name='Double'
    6:type_name='Complex'
    7:type_name='String'
    8:type_name='Structure'
    9:type_name='Double Complex'
    10:type_name='Pointer'
    11:type_name='Object reference'
    12:type_name='Unsigned Integer'
    13:type_name='Unsigned Long Integer'
    14:type_name='Long 64 bit Integer'
    15:type_name='Unsigned Long 64 bit Integer'
    ELSE:type_name='Undefined'
ENDCASE
RETURN,type_name
END