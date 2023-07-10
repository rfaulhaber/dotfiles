#!/usr/bin/env nu

def main [--latitude: number, --longitude: number] {
    let location = {
        latitude: $latitude,
        longitude: $longitude
    };

    let hourly = ($location
        | format "https://api.weather.gov/points/{latitude},{longitude}"
        | http get $in
        | from json
        | get properties.forecastHourly
        | http get $in
        | from json)

    let current = ($hourly.properties.periods
        | filter { |period|
            let start = ($period.startTime | into datetime)
            let end = ($period.endTime | into datetime)

            let now = (date now)

            ( $start <= $now ) and ( $end >= $now )
        })

    echo $current | first | format "{shortForecast} {temperature}{temperatureUnit}°"
}
