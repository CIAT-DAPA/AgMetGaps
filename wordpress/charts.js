    google.charts.load('current', { packages: ['corechart', 'bar'] });

    var font_name = 'Open Sans';
    var font_size = 12;
    var number_format = '##0.00'

google.charts.setOnLoadCallback(drawRegionWA);

    function drawRegionWA() {
        var gdata = google.visualization.arrayToDataTable(region_WA);

        var formatter = new google.visualization.NumberFormat(
            { pattern: number_format });

        formatter.format(gdata, 1);
        formatter.format(gdata, 2);
        formatter.format(gdata, 3);

        var options = {
            legend: { position: "bottom", maxLines: 3, textStyle: { fontName: font_name, fontSize: font_size } },
            tooltip: { textStyle: { fontName: font_name, fontSize: font_size } },
            colors: [color_green, color_yellow, color_red],
            chartArea: { 'width': '70%', 'height': '90%' },
            bar: { groupWidth: "90%" },
            vAxis: { textStyle: { color: 'black', fontName: font_name, fontSize: font_size, bold: false, italic: true } },
            hAxis: {
                textStyle: { color: 'black', fontName: font_name, fontSize: font_size, bold: false, italic: true },
                title: 'Area with hotspot (%)',
                titleTextStyle: { color: 'black', fontName: font_name, fontSize: font_size, bold: false, italic: true }
            },
            isStacked: 'true'
        };

        var chart = new google.visualization.BarChart(document.getElementById('bar_uses_div'));
        chart.draw(gdata, options);
    }</script>