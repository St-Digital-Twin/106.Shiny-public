'use strict';

var CITY_QUERY_URL_PREFIX = 'http://db-valera.dtwin.ru:4003/location?long_name=ilike.'

var app = {};

app.addResultRows = function (rows) {
    var rowsString = '';
    var rowsLength = rows.length;

    if (rowsLength > 0) {
        for (var i = 0; i < rowsLength; i++) {
            rowsString += app.buildResultRowString(rows[i]);
        }
    } else {
        rowsString = '<tr><td colspan="4">Results not found</td></tr>';
    }

    document.getElementById('results-table-body').innerHTML = rowsString;
    app.showElement('results-table');
}

app.buildResultRowString = function (row) {
    return  '<tr>' +
                '<td>' + row.long_name + '</td>' +
                '<td>' + row.oktmo + '</td>' +
                '<td>' + row.okato + '</td>' +
                '<td>' + row.long_name_eng + '</td>' +
            '</tr>';
}

app.showElement = function (id) {
    document.getElementById(id).classList.remove('hidden');
}

app.queryCity = function (cityName) {
    var queryURL = CITY_QUERY_URL_PREFIX + '*' + cityName + '*';

    fetch(queryURL)
        .then(function(response) {
            return response.json();
        })
        .then(function (j) {
            console.log(j);
            app.addResultRows(j);
        })
}

app.searchClick = function () {
    var city = document.getElementById('city-input').value;

    app.queryCity(city);
}
