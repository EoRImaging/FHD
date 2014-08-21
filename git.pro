PRO git,command,args=args,message=message,result=result,error=error,repo_path=repo_path
;NOTE: On Windows, this program requires that you include the path to the 'bin' directory of your git installation in your PATH environment variable (Control Panel->System->Advanced System Settings->Environment Variables)

;; NOTE: On Mac the libcurl.4.dylib library included with the idl distribution may be picked up by git rather than the system version resulting in an error like this:
;;dyld: Library not loaded: /usr/lib/libcurl.4.dylib
;;  Referenced from: /System/Library/PrivateFrameworks/DiskImages.framework/Versions/A/DiskImages
;;  Reason: Incompatible library version: DiskImages requires version 7.0.0 or later, but libcurl.4.dylib provides version 5.0.0
;;git: error: unable to locate xcodebuild, please make sure the path to the Xcode folder is set correctly!
;;git: error: You can set the path to the Xcode folder using /usr/bin/xcode-select -switch
;; to fix this either delete libcurl.4*.dylib from bin/bin.darwin.x86_64 (or similar) directory of the idl distribution (tested by Bryna)
;; or change the dylib environmental variable (export DYLD_LIBRARY_PATH=/opt/local/lib:${DYLD_LIBRARY_PATH}) (suggested by Google)


IF N_Elements(command) EQ 0 THEN command='describe'
IF N_Elements(repo_path) EQ 0 THEN BEGIN
    error=1
    print,"ERROR: repository path must be specified: repo_path"
ENDIF
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