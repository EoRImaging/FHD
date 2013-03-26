FUNCTION hash_name_gen,user=user,time=time,machine=machine

time=StrCompress(Strn(Long(Systime(1))),/remove_all)
login=get_login_info()
hash=''
IF Keyword_Set(user) THEN BEGIN
    user=StrCompress(login.user_name,/remove_all)
    hash+=user+'_'
ENDIF
IF Keyword_Set(machine) THEN BEGIN
    machine=StrCompress(login.machine_name,/remove_all)
    hash+=machine+'_'
ENDIF

hash+=time

RETURN,hash
END