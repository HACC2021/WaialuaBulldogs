# having trouble getting the timestamp out of our kml file
# also not rendering kml file from xml to data frame nicely
# so here we are using powershell to strip out tags (see line22)
# then write a bar delimited file so we can pull that into R and 
# plot our coordinates against the timestamps... the left column in the resulting file is
# the timestamps are 'epoch time' but we will sort by epoch time then group by minutes then plot 
#                 $fname = 'C:\r_stuff\ping_report_204283.kml'

$fname = 'C:\r_stuff\ping_report_2043336.kml'
gci $fname |%{

    gc $_ |%{
        if($_ -match 'Unix_Timestamp_of_Observation_Point'){
            $_ | out-file C:\r_stuff\ping_report_2043336_scrubbed.txt -Append
        }
        if($_ -match 'coordinates'){
            $_ | out-file C:\r_stuff\ping_report_2043336_scrubbed.txt -Append
        }

}}

gc 'C:\r_stuff\ping_report_2043336_scrubbed.txt' |%{
    if($_ -match 'Unix_Timestamp_of_Observation_Point'){
        $timestamp = $_.trim() 
        $timestamp = $timestamp -replace "<[^>]*?>",''
    }
    if($_ -match 'coordinates'){
        $coords = $_.trim() 
        $coords = $coords -replace "<[^>]*?>",''
        $timestamp+'|'+$coords | out-file C:\r_stuff\ping_2043336_clean.txt -Append
    }
}
