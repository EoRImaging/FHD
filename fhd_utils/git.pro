PRO git,command,args=args,message=message,result=result,error=error
IF N_Elements(command) EQ 0 THEN command='describe'

fhd_repo_path=rootdir('fhd')
pushd,fhd_repo_path

IF N_Elements(args) EQ 0 THEN args=''
IF Keyword_Set(message) THEN args+=' -m ' ELSE message=''


command_use='git '+command+args
IF Keyword_Set(message) THEN command_use+='"'+message+'"'
SPAWN, command_use,result,error

IF ~Arg_present(result) THEN print,'Result: ',result
IF Keyword_Set(error) THEN print,'Error code calling GIT: ',error


popd
END