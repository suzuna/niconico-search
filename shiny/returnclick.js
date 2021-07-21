$(document).keyup(function(event) {
    if ($("#q").is(":focus") && (event.keyCode == 13)) {
        $("#submit").click();
    }
});
$(document).keyup(function(event) {
    if ($(".form-control").is(":focus") && (event.keyCode == 13)) {
        $("#submit").click();
    }
});