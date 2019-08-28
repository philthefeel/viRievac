


# OSroot ------------------------------------------------------------------



OSroot = function(OS=get_os()){
  if(OS=='osx' & grepl('Air',system('system_profiler SPHardwareDataType | grep "Model Identifier"',intern=T))) path = '/Users/user/Documents/Irsicaixa/' # Mac Dan
  if(OS=='osx' & !grepl('Air',system('system_profiler SPHardwareDataType | grep "Model Identifier"',intern=T))) path = '/Volumes/virievac/DAN/' # Mac Dan
  # if(OS=='windows') path = 'Z:/' # PC?
  # if(OS=='linux') path = '/run/user/868803867/gvfs/smb-share:server=irsi-clufs,share=virievac/DAN/' # Server
  return(path)
}
