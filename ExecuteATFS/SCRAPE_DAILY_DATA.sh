# the first columns in the instrument MUST be url_name, symbol, platform, buy_holding_cost, sell_holding_cost

scrape_dir=$1
instruments_file=$2
fx_dir=$3
fx_file=$4

today=`date +"%Y%m%d"`
today_dash=`date +"%Y-%m-%d"`

### Download cash contracts from UK CMC markets
# iterate over the instruments file, in the csv format (without quotes)  "symbol url", "two letter code", "buy holding cost", "sell holding cost", "class". Here we only need the first three
while IFS=, read -r w1 w2 w3 w4 w5 w6; do
  # create symbol dir if does not exists
  if [ ! -d "$scrape_dir/$w2" ]; then mkdir $scrape_dir/$w2; fi
  # ignore other platforms than CMC
  if [ "$w3" != "CMC" ]; then continue; fi
  echo $w1 $w2 $w3
  f=$w1
  # we scrape the main symbol page, where we can fine the apicode and the apikey
  scrape=_scrape.$f.html
  wget -q "https://www.cmcmarkets.com/en/instruments/"$f -O $scrape;
  apicode=`grep api_code $scrape | sed -e 's/<input type="hidden" id="api_code" value="//' -e 's/"\/>//'`
  apikey=`grep api_key $scrape | sed -e 's/<input type="hidden" id="api_key" value="//' -e 's/">//'`
  # download the correspong "hidden" data
  wget -q "https://ws.cmcmarkets.com/instruments/prices/$apicode/DAY/3?key=$apikey" -O "_day."$f.txt
  # extract the relevant infos, which are the Date and the Close price from the previous 2 files
  sed -e 's/},{/\n/g' -e 's/"[chlot]"://g' -e 's/\[{//g'  -e 's/}\]//g' -e  's/T/ /g' -e  's/Z//g' -e 's/"//g' "_day."$f.txt  | cut -f 1,5 -d ","  | awk -F "," '{print $2","$1}'  > $today.$f.day
  # download the "hidden" json file, where we can find the holding costs
  wget "https://ws.cmcmarkets.com/json/instruments/"$apicode"_gb.json" -q -O "_"$f.json
  grep .yearlyPercentageBuy.*tradingHours "_"$f.json  -o | sed -e 's/},"tradingHours//' -e 's/"[^"]*"://g' -e 's/"//g' | awk -F ","  -v d=$today_dash ' function abs(v) {return v < 0 ? -v : v}  {if($2 == "CHARGED") b=-abs($1); else b=abs($1); if($4 == "CHARGED") s=-abs($3); else s=abs($3); print d,",",b,",",s}'  > $today.$f.holding_cost 
  # remove temp file and move everything to the correspoing symbol folder
  rm  "_day."$f.txt  _scrape.$f.html "_"$f.json 
  mv $today.$f.day $scrape_dir/$w2/$today.$w2.intraday  
  mv $today.$f.holding_cost $scrape_dir/$w2/$today.$w2.holding_cost
done <<< `tail -n+2 $instruments_file` 


### Download specific instrument from yahoo
start_date=1009843200
end_date=`date +"%s"`
while IFS=, read -r w1 w2 w3 w4 w5 w6 w7 w8 w9 w10; do
  if [ ! -d "$scrape_dir/$w2" ]; then mkdir $scrape_dir/$w2; fi
  if [ "$w3" != "yahoo" ]; then continue; fi
  echo $w1 $w2 $w3
  f=$w1
  scrape_tmp=_scrape.$f.html
  curl -sS "https://query1.finance.yahoo.com/v7/finance/download/$f?period1="$start_date"&period2="$end_date"&interval=1d&events=history&includeAdjustedClose=true" > $scrape_tmp
  sed -i 's/null/NA/g' $scrape_tmp
  cut -f 1,6 -d "," $scrape_tmp | tail -n +2 > $scrape_dir/$w2/$today.$w2.intraday # not really intraday, but we call it like this in compatibility with above CMC data
  head -n 2 $scrape_dir/$w2/$today.$w2.intraday > $scrape_dir/$w2/$today.$w2.weekly # fake weekly data
  echo "$today_dash,$w4,$w5" > $scrape_dir/$w2/$today.$w2.holding_cost
  rm $scrape_tmp
done <<< `tail -n+2 $instruments_file` 

## Download FX rates
start_date=1654117608
end_date=`date +"%s"`
while IFS=, read -r fx; do
  echo $fx
  scrape_tmp=_scrape.$fx.html
  curl -sS "https://query1.finance.yahoo.com/v7/finance/download/$fx=X?period1="$start_date"&period2="$end_date"&interval=1d&events=history&includeAdjustedClose=true" > $scrape_tmp
  sed -i 's/null/NA/g' $scrape_tmp
  cut -f 1,6 -d "," $scrape_tmp  > $fx_dir/$fx.csv
  rm $scrape_tmp
done < $fx_file


