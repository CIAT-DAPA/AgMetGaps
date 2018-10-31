var month = ["none", "DJF", "JFM", "FMA", "MAM", "AMJ", "MJJ", "JJA", "JAS", "ASO", "SON", "OND", "NDJ"];

$("input[type='radio']").click(function () {
    var status = $(this).data("status");
    var name = $(this).prop("name");

    // clear the other checked options of the radio group
    $("#" + name + "-menu input[type='radio']").each(function (index) {
        $(this).data("status", "unchecked");;
    });

    // change of status checked/unchecked
    if (status == undefined || status == "unchecked") {
        $(this).prop("checked", true);
        $(this).data("status", "checked");
    } else {
        $(this).prop("checked", false);
        $(this).data("status", "unchecked");
    }

    updateMap();

    gtag('event', $(this).data("status") + "", {
        'event_category': "AgMetGaps",
        'event_label': name + " " + $(this).val() + ""
    });
});

$("input[type='checkbox']").click(function () {
    var id = this.id;
    var name = id.split("-")[0];
    $("#" + name + "-layers").toggle({ direction: "top" }, 1000);
    gtag('event', "toggle" + "", {
        'event_category': "AgMetGaps",
        'event_label': name + " " + id + ""
    });

});

$("#regions-checkbox").click(function () {
    updateMap();
});

$("#about-checkbox").click(function () {
    $("#about-box").slideToggle("fast", "linear");
});



var handle = $("#custom-handle");
$("#time-slider").slider({
    min: 1,
    max: 12,
    value: 1,
    create: function () {
        handle.text(month[$(this).slider("value")]);
    },
    slide: function (event, ui) {
        handle.text(month[ui.value]);
        updateMapBySlider(month[ui.value]);
        gtag('event', "slide", {
            'event_category': "AgMetGaps",
            'event_label': "slider " + month[ui.value] + ""
        });
    }

});

var globalOverlay;
var precipitationOverlay;
var temperatureOverlay;
var cropOverlay;
var regionsOverlay;
var mapZoom = 3;
var mapCenter = { lat: 0, lng: 0 };
var mapTypeSelected = 'lightStyledMapType';
var mapImageBounds = {
    north: 90,
    south: -90,
    east: 180,
    west: -180
};
var map




function initMap() {

    map = new google.maps.Map(document.getElementById('map'), {
        zoom: mapZoom,
        center: mapCenter,
        streetViewControl: false,
        zoomControl: true,
        zoomControlOptions: {
            position: google.maps.ControlPosition.TOP_LEFT
        },
        fullscreenControl: true,
        fullscreenControlOptions: {
            position: google.maps.ControlPosition.RIGHT_BOTTOM
        },
        mapTypeControlOptions: {
            mapTypeIds: ['roadmap', 'satellite', 'hybrid', 'terrain', 'lightStyledMapType']
        }
    });

    var lightStyledMapType = new google.maps.StyledMapType(lightStyle, { name: 'Light' });

    //Associate the styled map with the MapTypeId and set it to display.
    map.mapTypes.set('lightStyledMapType', lightStyledMapType);
    map.setMapTypeId(mapTypeSelected);

    //Set local storage variables.
    mapCenter = map.getCenter();
    mapZoom = map.getZoom();

    google.maps.event.addListener(map, "center_changed", function () {
        //Set local storage variables.
        mapCenter = map.getCenter();
        mapZoom = map.getZoom();
    });

    google.maps.event.addListener(map, "zoom_changed", function () {
        //Set local storage variables.
        mapCenter = map.getCenter();
        mapZoom = map.getZoom();
    });

    google.maps.event.addListener(map, "maptypeid_changed", function () {
        //Set local storage variables.
        mapTypeSelected = map.getMapTypeId();
    });

    refreshMap("climate", "precipitation_hotspots", "temperature_hotspots", "maize_cv", true, month[1]); // initialize all
    globalOverlay.setMap(null);
    precipitationOverlay.setMap(null);
    temperatureOverlay.setMap(null);
    cropOverlay.setMap(null);
    regionsOverlay.setMap(null);
    $("#global-layers").toggle({ direction: "top" }, 1000);
    $("#precipitation-layers").toggle({ direction: "top" }, 1000);
    $("#temperature-layers").toggle({ direction: "top" }, 1000);
    $("#crop-layers").toggle({ direction: "top" }, 1000);
    $("#about-box").slideUp();
    $("#time-slider").toggle();

    refreshMap("climate", undefined, undefined, undefined, true, month[1]);
    $("#hotspots-menu input[type='radio']:checked").data("status", "checked");
}

function updateMap() {
    globalOverlay.setMap(null);
    precipitationOverlay.setMap(null);
    temperatureOverlay.setMap(null);
    cropOverlay.setMap(null);
    regionsOverlay.setMap(null);
    var global = $("#global-menu input[type='radio']:checked").val();
    var precipitationVar = $("#precipitation-menu input[type='radio']:checked").val();
    var temperatureVar = $("#temperature-menu input[type='radio']:checked").val();
    var crop = $("#crop-menu input[type='radio']:checked").val();
    var regions = $("#regions-checkbox").is(":checked");
    var season = month[$("#time-slider").slider("value")];
    refreshMap(global, precipitationVar, temperatureVar, crop, regions, season);
}

function updateMapBySlider(season) {
    globalOverlay.setMap(null);
    precipitationOverlay.setMap(null);
    temperatureOverlay.setMap(null);
    cropOverlay.setMap(null);
    regionsOverlay.setMap(null);
    var global = $("#global-menu input[type='radio']:checked").val();
    var precipitationVar = $("#precipitation-menu input[type='radio']:checked").val();
    var temperatureVar = $("#temperature-menu input[type='radio']:checked").val();
    var crop = $("#crop-menu input[type='radio']:checked").val();
    var regions = $("#regions-checkbox").is(":checked");
    refreshMap(global, precipitationVar, temperatureVar, crop, regions, season);
}

function refreshMap(global, precipitationVar, temperatureVar, crop, regions, season) {
    var showSlider = false;
    var opacity = 1.0;

    // Create the legend and display on the map
    var legend = document.createElement('div');
    legend.id = 'ts-map-legend';


    //assemble html and place in var
    var content = [];

    if (global != undefined) {
        var layerRelativePath = 'global/' + global
        globalOverlay = new google.maps.GroundOverlay(
            IMG_URL + layerRelativePath + '.png',
            mapImageBounds);
        globalOverlay.setOpacity(opacity);
        globalOverlay.setMap(map);

        switch (global) {
            case 'climate':
                content.push('<h3 class="ts-map-legend-headline">' + ' Climate Hotspots</h3>');
                break;
            case 'crops':
                content.push('<h3 class="ts-map-legend-headline">' + ' Crop Hotspots</h3>');
                break;
        }
        content.push('<p><div class="ts-map-legend-gradient ts-map-legend-gradient-hotspots"></div>' + '</p></br>');
        content.push('<p>' + ' min. 0, max. 1' + '</p><p> <i class="fas fa-download"> </i> <a href="' + FTP_URL + layerRelativePath + '.zip' + '">download</a> </p>');
    }

    if (precipitationVar != undefined) {
        var layerRelativePath = "";

        if (content.length != 0) {
            content.push('<h3></h3>');
        }

        switch (precipitationVar) {
            case 'precipitation_cv':
                layerRelativePath = 'precipitation/CV/' + season + "_" + precipitationVar;
                precipitationOverlay = new google.maps.GroundOverlay(IMG_URL + layerRelativePath + '.png', mapImageBounds);
                showSlider = true;
                content.push('<h3 class="ts-map-legend-headline">' + season + ' Precipitation C.V.</h3>');
                break;
            case 'precipitation_groc':
                layerRelativePath = 'precipitation/GROC/' + season + "_" + precipitationVar;
                precipitationOverlay = new google.maps.GroundOverlay(IMG_URL + layerRelativePath + '.png', mapImageBounds);
                showSlider = true;
                content.push('<h3 class="ts-map-legend-headline">' + season + ' Precipitation GROC</h3>');
                break;
            case 'precipitation_hotspots':
                layerRelativePath = 'precipitation/hotspots/' + precipitationVar;
                precipitationOverlay = new google.maps.GroundOverlay(IMG_URL + layerRelativePath + '.png', mapImageBounds);
                content.push('<h3 class="ts-map-legend-headline">' + ' Precipitation hotspots</h3>');
                break;
        }
        precipitationOverlay.setOpacity(opacity);
        precipitationOverlay.setMap(map);


        content.push('<p><div class="ts-map-legend-gradient ts-map-legend-gradient-precipitation"></div>' + '</p></br>');
        content.push('<p>' + ' min. 0, max. 1' + '</p><p> <i class="fas fa-download"> </i> <a href="' + FTP_URL + layerRelativePath + '.zip' + '">download</a> </p>');
    }


    if (temperatureVar != undefined) {

        if (content.length != 0) {
            content.push('<h3></h3>');
        }

        switch (temperatureVar) {
            case 'temperature_sd':
                layerRelativePath = 'temperature/SD/' + season + "_" + temperatureVar;
                temperatureOverlay = new google.maps.GroundOverlay(IMG_URL + layerRelativePath + '.png', mapImageBounds);
                showSlider = true;
                content.push('<h3 class="ts-map-legend-headline">' + season + ' Temperature S.D.</h3>');
                break;
            case 'temperature_groc':
                layerRelativePath = 'temperature/GROC/' + season + "_" + temperatureVar;
                temperatureOverlay = new google.maps.GroundOverlay(IMG_URL + layerRelativePath + '.png', mapImageBounds);
                showSlider = true;
                content.push('<h3 class="ts-map-legend-headline">' + season + ' Temperature GROC</h3>');
                break;
            case 'temperature_hotspots':
                layerRelativePath = 'temperature/hotspots/' + temperatureVar;
                temperatureOverlay = new google.maps.GroundOverlay(IMG_URL + layerRelativePath + '.png', mapImageBounds);
                content.push('<h3 class="ts-map-legend-headline">' + ' Temperature hotspots</h3>');
                break;
        }
        temperatureOverlay.setOpacity(opacity);
        temperatureOverlay.setMap(map);


        content.push('<p><div class="ts-map-legend-gradient ts-map-legend-gradient-temperature"></div>' + '</p></br>');
        content.push('<p>' + ' min. 0, max. 1' + '</p><p> <i class="fas fa-download"> </i> <a href="' + FTP_URL + layerRelativePath + '.zip' + '">download</a> </p>');
    }

    if (crop != undefined) {

        var name = crop.split("_")[1];
        var layerRelativePath = "";

        if (content.length != 0) {
            content.push('<h3></h3>');
        }
        switch (name) {
            case 'cv':
                layerRelativePath = 'crops/CV/' + crop;
                cropOverlay = new google.maps.GroundOverlay(IMG_URL + layerRelativePath + '.png', mapImageBounds)
                showSlider = true;
                content.push('<h3 class="ts-map-legend-headline">Yield C.V.</h3>');
                break;
            case 'hotspots':
                layerRelativePath = 'crops/hotspots/' + crop;
                cropOverlay = new google.maps.GroundOverlay(IMG_URL + layerRelativePath + '.png', mapImageBounds)
                showSlider = true;
                content.push('<h3 class="ts-map-legend-headline">Hotspots</h3>');
                break;

        }

        cropOverlay.setOpacity(opacity);
        cropOverlay.setMap(map);
        content.push('<p><div class="ts-map-legend-gradient ts-map-legend-gradient-crop"></div>' + '</p></br>');
        content.push('<p>' + ' min. 0, max. 1' + '</p><p> <i class="fas fa-download"> </i> <a href="' + FTP_URL + layerRelativePath + '.zip' + '">download</a> </p>');
    }

    if (regions) {

        regionsOverlay = new google.maps.GroundOverlay(
            IMG_URL + 'LobelRegions' + '.png',
            mapImageBounds);
        regionsOverlay.setOpacity(1.0);
        regionsOverlay.setMap(map);

        if (content.length != 0) {
            content.push('<h3></h3>');
        }
        content.push('<h3 class="ts-map-legend-headline">Regions</h3>');
        content.push('<p>' + 'Central America' + '</p>');
        content.push('<p>' + 'Western Africa' + '</p>');
        content.push('<p>' + 'Eastern Africa' + '</p>');
        content.push('<p>' + 'Southern Asia' + '</p>');
        content.push('<p>' + 'Eastern Asia' + '</p>');
    }
    legend.innerHTML = content.join('');
    legend.index = 1;
    map.controls[google.maps.ControlPosition.TOP_RIGHT].clear();
    map.controls[google.maps.ControlPosition.TOP_RIGHT].push(legend);

    if (showSlider) {
        $("#time-slider").show();
    } else {
        $("#time-slider").hide();
    }


}
