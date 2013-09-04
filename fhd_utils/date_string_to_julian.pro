FUNCTION date_string_to_julian,date_string

;format is YYYY-MM-DDTHH:MM:SS
year=Float(Strmid(date_string,0,4))
month=Float(Strmid(date_string,5,2))
day=Float(Strmid(date_string,8,2))
hour=Float(Strmid(date_string,11,2))
minute=Float(Strmid(date_string,14,2))
second=Float(Strmid(date_string,17,2))
jd=julday(month,day,year,hour,minute,second)

RETURN,jd
END