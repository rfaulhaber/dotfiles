#!/usr/bin/env nu

def main [--latitude: number, --longitude: number] {
    http get $"https://api.weather.gov/points/($latitude),($longitude)"
        | from json
        | get properties.forecastHourly
        | http get $in
        | from json
        | get properties.periods
        | filter { |period|
            let start = ($period.startTime | into datetime)
            let end = ($period.endTime | into datetime)

            let now = (date now)

            ( $start <= $now ) and ( $end >= $now )
        }
        | first
        | $"($in.shortForecast) ($in.temperature)($in.temperatureUnit)°"
}
