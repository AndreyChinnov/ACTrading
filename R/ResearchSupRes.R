#Function that researches the Support and Resistant levels

ResearchSupRes<-function(OHLC_Data, Delta, Error=0) {
  OHLC_Data$Bullish<- Cl(OHLC_Data)>=Op(OHLC_Data)
  MaxVal<-max(Hi(OHLC_Data))+Delta
  MinVal<-min(Lo(OHLC_Data))-Delta
  p<-MinVal
  Intervals<-NULL
  DF<-NULL
  while(p+Delta<MaxVal) {
    Intervals<-c(Intervals,paste(as.character(p),as.character(p+Delta),sep="-"))
    WhichInside<-Inside(p,Lo(OHLC_Data),Hi(OHLC_Data))|Inside(p+Delta,Lo(OHLC_Data),Hi(OHLC_Data))
    WhichNearBy<- NearBy(p,p+Delta,Lo(OHLC_Data),Hi(OHLC_Data),Error)
    N_cand<-sum(WhichInside)
    #OHLC_Data[WhichInside]
    N_cand_B<-sum(OHLC_Data[WhichInside]$Bullish)


    # WhichReverse<-(Inside(Cl(OHLC_Data),p,p+Delta)&!Inside(Op(OHLC_Data),p,p+Delta))|
    #   (Inside(Op(OHLC_Data),p,p+Delta)&!Inside(Cl(OHLC_Data),p,p+Delta))
    WhichReverse<- !((Cl(OHLC_Data)>p+Delta & Op(OHLC_Data)<p) |
                       (Cl(OHLC_Data)<p & Op(OHLC_Data)>p+Delta))& WhichInside

    N_rev<-sum(WhichReverse)
    N_rev_B<-sum(OHLC_Data[WhichReverse]$Bullish)
    
    WhichReverseNearBy<- !((Cl(OHLC_Data)>p+Delta & Op(OHLC_Data)<p) |
                       (Cl(OHLC_Data)<p & Op(OHLC_Data)>p+Delta))& WhichInside & WhichNearBy
    
    N_rev_NearBy<-sum(WhichReverseNearBy)
    N_rev_all<-N_rev+N_rev_NearBy
    
    p_value<-(2*p+Delta)/2
    p_start<-p
    p_end<-p+Delta
    newDF<-data.frame(p_start,p_end,p_value, N_cand, N_cand_B, N_rev, N_rev_B, N_rev_NearBy, N_rev_all)
    row.names(newDF)<-paste(as.character(p),as.character(p+Delta),sep="-")
    DF<-rbind(DF,newDF)
    p<-p+Delta
  }
  return(DF)
}
