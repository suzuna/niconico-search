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

/*
page-prev-button page-next-button
*/

$("span.page-button-group-numbers.btn-group>button.btn.btn-default.page-num-button").click(function(){
    $("html, body").animate({scrollTop: 0}, "slow");
});