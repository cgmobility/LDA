lda_install <- function(){
  replicate(
    3,
    KeyboardSimulator::keybd.press(button = 'backspace')
  )
  Sys.sleep(.3)
  text <- "if('LDA' %in% installed.packages() == FALSE){
  remotes::install_github('cgmobility/LDA')
}"
  writeClipboard(text)
  keybd.press('Ctrl',hold = T);keybd.press('v');keybd.release('Ctrl')
  return(NULL)
}
