PRO git,command,args=args,message=message,result=result,error=error,project_name=project_name,repo_path=repo_path
;NOTE: On Windows, this program requires that you include the path to the 'bin' directory of your git installation in your PATH environment variable (Control Panel->System->Advanced System Settings->Environment Variables)
IF N_Elements(command) EQ 0 THEN command='describe'
IF N_Elements(project_name) EQ 0 THEN project_name='fhd' 
IF N_Elements(repo_path) EQ 0 THEN repo_path=rootdir(project_name)
pushd,repo_path

IF N_Elements(args) EQ 0 THEN args=''
IF Keyword_Set(message) THEN args+=' -m ' ELSE message=''

command_use='git '+command+' '+args
IF Keyword_Set(message) THEN command_use+='"'+message+'"'
SPAWN, command_use,result,error

IF ~Arg_present(result) THEN print,'Result: ',result
IF Keyword_Set(error) THEN print,'Error code calling GIT: ',error

popd
END