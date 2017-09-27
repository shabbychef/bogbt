
The file calm_sentiment_data.csv contains Twitter sentiment data for the
'calm' sentiment, aligned with returns of the DJI (the Dow Jones Index)
and GSPC (the S&P 500 index). The fields consist of:

 * date : the date of the observed Twitter sentiment data. Presumably
   the data are observable by 23:59:59 of the given date, though the
   the time zone is unclear.
 * tone : the tone sentiment series.
 * calm : the calm sentiment series.
 * tone_Z10 : the normalized tone variable, using k=10. This is essentially a
   centered Z-score of tone over 21 observations.
 * tone_Z1 : the normalized tone variable, using k=1. This is essentially a
   centered Z-score of tone over 3 observations.
 * DJI  : the closing value of the DJI index for the given day. Will take value
   of NA when the market is closed. Early closes are treated as normal market
   days. The DJI data are source from Yahoo finance via the quantmod package.
 * DJI_volume_k  : the volume of the DJI index for the given day, in thousands.
   This may be useful for data QA, for example.
   The volume is NA when the market is closed.
 * DJI_forward_return  : this is a one market period relative 'return' of the
   DJI index. A value of 0.01, for example, corresponds to a 1% increase in
   the DJI index. This is a _forward_ return, meaning it is the return from
   the close of the given day to the close of the next market day, and
   could be approximately captured by a long holder in the index.
   (Were that possible; an index is not an ETF.) The return is NA when the 
   market is closed.
 * GSPC  : the closing value of the GSPC index for the given day. Will take value
   of NA when the market is closed. Early closes are treated as normal market
   days. The GSPC data are source from Yahoo finance via the quantmod package.
 * GSPC_volume_k  : the volume of the GSPC index for the given day, in thousands.
 * GSPC_forward_return  : the one market period relative 'return' of the
   GSPC index. This is a forward_return, meaning it is the return from
   the close of the given day to the close of the next market day.
   The return is NA when the market is closed.

