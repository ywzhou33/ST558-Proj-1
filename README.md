This Project is to access data sets from the One Call API and perform data exploration and graphical summaries. 

Connects to the One Call API.  

The One Call API provides the following weather data for any geographical coordinates:  
 - Current weather  
 - Minute forecast for 1 hour  
 - Hourly forecast for 48 hours  
 - Daily forecast for 7 days  
 - National weather alerts  
 - Historical weather data for the previous 5 days  

These data can be accessed by defining the following parameters in the URL.

Parameters:

 - `lat` `(required)` - Latitude; geographical coordinates that have values between -180 and 180.  
 - `lon` `(required)` - Longitude; geographical coordinates that have values between -180 and 180.  
 - `appid` `(required)` - Your unique API key  
 - `exclude` `(optional)` - You can use it to exclude some parts from the data, available values are `current`, `minutely`, `hourly`, `daily`, `alerts`, should be a comma-delimited list (without spaces).  
 - `units` `(optional)` - Units of measurement. `standard` (Kelvin & meter/sec), `metric` (Celsius & meter/sec) and `imperial` (Fahrenheit & miles/hour) units are available. If you do not use the `units` parameter, `standard` units will be applied by default.  
 - `lang` `(optinal)` - You can use this parameter to get the output in your language.  
 
 