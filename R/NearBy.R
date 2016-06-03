Inside<-function(p_start,p_end,low_values,high_values,error) {
  return((p_start-high_values<=error & p_start-high_values>0)|
           (low_values-p_end<=error & low_values-p_end>0))
}
